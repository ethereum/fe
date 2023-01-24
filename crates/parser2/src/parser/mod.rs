use std::collections::VecDeque;

pub(crate) use item::ItemListScope;

use fxhash::FxHashSet;

use crate::{syntax_node::SyntaxNode, ParseError, SyntaxKind, TextRange};

use self::token_stream::{BackTrackableTokenStream, SyntaxToken, TokenStream};

pub mod token_stream;

pub use pat::parse_pat;

pub mod attr;
pub mod expr;
pub mod func;
pub mod item;
pub mod param;
pub mod pat;
pub mod path;
pub mod stmt;
pub mod struct_;
pub mod type_;

mod expr_atom;

type Checkpoint = rowan::Checkpoint;

/// Parser to build a rowan syntax tree.
pub struct Parser<S: TokenStream> {
    /// Token stream to parse.
    stream: BackTrackableTokenStream<S>,

    builder: rowan::GreenNodeBuilder<'static>,
    /// The second element holds `is_newline_trivia` of the parent.
    parents: Vec<(Box<dyn ParsingScope>, bool)>,
    errors: Vec<ParseError>,

    next_trivias: VecDeque<S::Token>,
    /// if `is_newline_trivia` is `true`, `Newline` is also regarded as a trivia
    /// token.
    is_newline_trivia: bool,

    current_pos: rowan::TextSize,
    /// The dry run states which holds the each state of the parser when it
    /// enters dry run mode.
    dry_run_states: Vec<DryRunState<S>>,

    auxiliary_recovery_set: FxHashSet<SyntaxKind>,
}

impl<S: TokenStream> Parser<S> {
    /// Create a parser with the given token stream.
    pub fn new(stream: S) -> Self {
        Self {
            stream: BackTrackableTokenStream::new(stream),
            builder: rowan::GreenNodeBuilder::new(),
            parents: Vec::new(),
            errors: Vec::new(),
            current_pos: rowan::TextSize::from(0),
            is_newline_trivia: true,
            next_trivias: VecDeque::new(),
            dry_run_states: Vec::new(),
            auxiliary_recovery_set: FxHashSet::default(),
        }
    }

    /// Returns the current token of the parser.
    pub fn current_token(&mut self) -> Option<S::Token> {
        self.peek_non_trivia()
        // if !self.next_trivias.is_empty() {
        //     Some(&self.next_trivias[0])
        // } else {
        //     self.stream.peek()
        // }
    }

    /// Returns the current non-trivia token kind of the parser.
    pub fn current_kind(&mut self) -> Option<SyntaxKind> {
        self.current_token().map(|tok| tok.syntax_kind())
    }

    /// Sets the newline kind as trivia if `is_trivia` is `true`. Otherwise, the
    /// newline kind is not regarded as a trivia.
    ///
    /// Returns previous value.
    pub fn set_newline_as_trivia(&mut self, is_trivia: bool) -> bool {
        std::mem::replace(&mut self.is_newline_trivia, is_trivia)
    }

    /// Finish the parsing and return the syntax tree.
    pub fn finish(self) -> (SyntaxNode, Vec<ParseError>) {
        debug_assert!(self.parents.is_empty());
        debug_assert!(!self.is_dry_run());

        (SyntaxNode::new_root(self.builder.finish()), self.errors)
    }

    /// Invoke the scope to parse. The scope is wrapped up by the node specified
    /// by the scope.
    ///
    /// # Arguments
    /// * If the `checkpoint` is `Some`, the marked branch is wrapped up by the
    ///   node.
    /// * If the `checkpoint` is `None`, the current branch is wrapped up by the
    ///   node.
    ///
    /// # Returns
    /// * If the parsing succeeds, the first element of the return value is
    ///   `true`. otherwise, the first element is `false`.
    /// * The second element of the return value is the checkpoint of the start
    ///   of the node.
    pub fn parse<T>(&mut self, mut scope: T, checkpoint: Option<Checkpoint>) -> (bool, Checkpoint)
    where
        T: Parse + 'static,
    {
        let checkpoint = self.enter(scope.clone(), checkpoint);
        let error_len = self.errors.len();
        let start_checkpoint = self.checkpoint();
        scope.parse(self);
        self.leave(checkpoint);
        (error_len == self.errors.len(), start_checkpoint)
    }

    #[doc(hidden)]
    /// Enter the scope and return the checkpoint. The checkpoint branch will be
    /// wrapped up by the scope's node when [`leave`] is called.
    // NOTE: This method is limited to testing and internal usage.
    pub fn enter<T>(&mut self, scope: T, checkpoint: Option<Checkpoint>) -> Checkpoint
    where
        T: ParsingScope + 'static,
    {
        // Ensure the leading trivias are added to the parent node.
        if !self.parents.is_empty() {
            self.bump_trivias();
        }
        self.parents.push((Box::new(scope), self.is_newline_trivia));
        // `is_newline_trivia` is always `true` when entering a scope.
        self.is_newline_trivia = true;
        checkpoint.unwrap_or_else(|| self.checkpoint())
    }

    pub fn add_recovery_token(&mut self, token: SyntaxKind) {
        self.auxiliary_recovery_set.insert(token);
    }

    pub fn remove_recovery_token(&mut self, token: SyntaxKind) {
        self.auxiliary_recovery_set.remove(&token);
    }

    #[doc(hidden)]
    /// Leave the scope and wrap up the checkpoint by the scope's node.
    // NOTE: This method is limited to testing and internal usage.
    pub fn leave(&mut self, checkpoint: Checkpoint) {
        let (scope, is_newline_trivia) = self.parents.pop().unwrap();
        self.is_newline_trivia = is_newline_trivia;

        // Ensure the trailing trivias are added to the current node if the current
        // scope is the root.
        if self.parents.is_empty() {
            self.bump_trivias()
        }

        self.auxiliary_recovery_set.clear();
        if !self.is_dry_run() {
            self.builder
                .start_node_at(checkpoint, scope.syntax_kind().into());
            self.builder.finish_node();
        }
    }

    /// Add `msg` as an error to the error list, then bumps consecutive tokens
    /// until a token in the recovery set is found.
    ///
    /// * If checkpoint is `Some`, the marked branch is wrapped up by an error
    ///   node.
    /// * If checkpoint is `None`, the current branch is wrapped up by an error
    ///   node.
    pub fn error_and_recover(&mut self, msg: &str, checkpoint: Option<Checkpoint>) {
        let err_scope = self.error(msg);
        let checkpoint = self.enter(err_scope, checkpoint);
        self.recover();
        self.leave(checkpoint);
    }

    /// Add `msg` as an error to the error list, then bumps consecutive tokens
    /// until a `tok` is found or the end of the file is reached.
    ///
    /// * If checkpoint is `Some`, the marked branch is wrapped up by an error
    ///   node.
    /// * If checkpoint is `None`, the current branch is wrapped up by an error
    ///   node.
    pub fn error_and_bump_until(
        &mut self,
        msg: &str,
        checkpoint: Option<Checkpoint>,
        kind: SyntaxKind,
    ) {
        let err_scope = self.error(msg);
        let checkpoint = self.enter(err_scope, checkpoint);
        loop {
            if self.current_kind() == Some(kind) || self.current_kind().is_none() {
                break;
            }
            self.bump()
        }
        self.leave(checkpoint);
    }

    /// Starts the dry run mode.
    /// When the parser is in the dry run mode, the parser does not build the
    /// syntax tree.
    ///
    /// When the [`end_dry_run`] is called, all errors occurred in the dry
    /// run mode are discarded, and all tokens which are consumed in the
    /// dry run mode are backtracked.
    pub fn start_dry_run(&mut self) {
        self.stream.set_bt_point();
        self.dry_run_states.push(DryRunState {
            pos: self.current_pos,
            err_num: self.errors.len(),
            next_trivias: self.next_trivias.clone(),
            auxiliary_recovery_set: self.auxiliary_recovery_set.clone(),
        });
    }

    /// Ends the dry run mode.
    /// See `[start_dry_run]` for more details.
    pub fn end_dry_run(&mut self) {
        self.stream.backtrack();
        let state = self.dry_run_states.pop().unwrap();
        self.errors.truncate(state.err_num);
        self.current_pos = state.pos;
        self.next_trivias = state.next_trivias;
        self.auxiliary_recovery_set = state.auxiliary_recovery_set;
    }

    /// Bumps the current token and its leading trivias.
    pub fn bump(&mut self) {
        // Bump leading trivias.
        self.bump_trivias();

        self.bump_raw();
    }

    /// Bumps the current token if the current token is the `expected` kind.
    ///
    /// # Panics
    /// Panics If the current token is not the `expected` kind.
    pub fn bump_expected(&mut self, expected: SyntaxKind) {
        assert_eq!(self.current_kind(), Some(expected));
        self.bump();
    }

    /// Bumps the current token if the current token is the `expected` kind.
    /// Return `true` if the current token is the `expected` kind.
    pub fn bump_if(&mut self, expected: SyntaxKind) -> bool {
        if self.current_kind() == Some(expected) {
            self.bump();
            true
        } else {
            false
        }
    }

    /// Proceeds the parser to the recovery token of the current scope.
    pub fn recover(&mut self) {
        let mut recovery_set: FxHashSet<SyntaxKind> = fxhash::FxHashSet::default();
        let mut scope_index = self.parents.len() - 1;
        loop {
            match self
                .parents
                .get(scope_index)
                .map(|scope| scope.0.recovery_method())
            {
                Some(RecoveryMethod::Inheritance(set)) => {
                    recovery_set.extend(set.iter());
                    scope_index -= 1;
                }
                Some(RecoveryMethod::Override(set)) => {
                    recovery_set.extend(set.iter());
                    break;
                }

                None => break,
            }
        }

        while let Some(kind) = self.current_kind() {
            if recovery_set.contains(&kind) | self.auxiliary_recovery_set.contains(&kind) {
                break;
            } else {
                self.bump();
            }
        }
    }

    fn checkpoint(&mut self) -> Checkpoint {
        self.builder.checkpoint()
    }

    /// Bumps the current token and
    /// current branch.
    fn bump_raw(&mut self) {
        let tok = match self.next_trivias.pop_front() {
            Some(tok) => tok,
            None => self.stream.next().unwrap(),
        };

        self.current_pos += rowan::TextSize::of(tok.text());
        if !self.is_dry_run() {
            self.builder.token(tok.syntax_kind().into(), tok.text());
        }
    }

    fn bump_trivias(&mut self) {
        // Bump trivias.
        loop {
            match self.peek_raw() {
                Some(tok) if self.is_trivia(tok.syntax_kind()) => self.bump_raw(),
                _ => break,
            }
        }
    }

    /// Peek the next non-trivia token.
    fn peek_non_trivia(&mut self) -> Option<S::Token> {
        if !self.is_newline_trivia {
            for tok in &self.next_trivias {
                if tok.syntax_kind() == SyntaxKind::Newline {
                    return Some(tok.clone());
                }
            }
        }

        while let Some(next) = self.stream.peek().map(|tok| tok.syntax_kind()) {
            if self.is_trivia(next) {
                let next = self.stream.next().unwrap();
                self.next_trivias.push_back(next);
                continue;
            } else {
                return self.stream.peek().cloned();
            }
        }

        None
    }

    fn peek_raw(&mut self) -> Option<S::Token> {
        if let Some(tok) = self.next_trivias.front() {
            Some(tok.clone())
        } else {
            self.stream.peek().cloned()
        }
    }

    /// Add the `msg` to the error list.
    fn error(&mut self, msg: &str) -> ErrorScope {
        let start = self.current_pos;
        let end = if let Some(current_token) = self.current_token() {
            start + current_token.text_size()
        } else {
            start
        };
        let range = TextRange::new(start, end);

        self.errors.push(ParseError {
            range,
            msg: msg.to_string(),
        });
        ErrorScope::default()
    }

    /// Returns `true` if the parser is in the dry run mode.
    fn is_dry_run(&self) -> bool {
        !self.dry_run_states.is_empty()
    }

    fn is_trivia(&self, kind: SyntaxKind) -> bool {
        kind.is_trivia() || (self.is_newline_trivia && kind == SyntaxKind::Newline)
    }
}

pub trait ParsingScope {
    /// Returns the recovery method of the current scope.
    fn recovery_method(&self) -> &RecoveryMethod;

    fn syntax_kind(&self) -> SyntaxKind;
}

pub trait Parse: ParsingScope + Clone {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>);
}

struct DryRunState<S: TokenStream> {
    /// The text position is the position when the dry run started.
    pos: rowan::TextSize,
    /// The number of errors when the dry run started.
    err_num: usize,
    /// The stored trivias when the dry run started.
    next_trivias: VecDeque<S::Token>,
    auxiliary_recovery_set: FxHashSet<SyntaxKind>,
}

/// Represents the recovery method of the current scope.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecoveryMethod {
    /// Uses the recovery method of the parent scope and its own recovery set.
    Inheritance(FxHashSet<SyntaxKind>),

    /// The scope has its own recovery set and don't use parent scope's recovery
    /// set.
    Override(FxHashSet<SyntaxKind>),
}

impl RecoveryMethod {
    fn inheritance(tokens: &[SyntaxKind]) -> Self {
        Self::Inheritance(tokens.iter().copied().collect())
    }

    fn override_(tokens: &[SyntaxKind]) -> Self {
        Self::Override(tokens.iter().copied().collect())
    }
}

trait TextSize {
    fn text_size(&self) -> rowan::TextSize;
}

impl<T> TextSize for T
where
    T: SyntaxToken,
{
    fn text_size(&self) -> rowan::TextSize {
        rowan::TextSize::of(self.text())
    }
}

define_scope! {
    ErrorScope,
    Error,
    Inheritance
}

define_scope! {
    pub RootScope,
    Root,
    Override()
}

macro_rules! define_scope {
    (
        $(#[$attrs: expr])*
        $visibility: vis $scope_name: ident $({ $($field: ident: $ty: ty),* })?,
        $kind: path,
        Inheritance $(($($recoveries: path), *))?
    ) => {
        crate::parser::define_scope_struct! {$visibility $scope_name {$($($field: $ty), *)?}, $kind}
        impl crate::parser::ParsingScope for $scope_name {
            fn recovery_method(&self) -> &crate::parser::RecoveryMethod {
                lazy_static::lazy_static! {
                    pub(super) static ref RECOVERY_METHOD: crate::parser::RecoveryMethod = {
                        #[allow(unused)]
                        use crate::SyntaxKind::*;
                        crate::parser::RecoveryMethod::inheritance(&[$($($recoveries), *)?])
                    };
                }

                &RECOVERY_METHOD
            }

            fn syntax_kind(&self) -> crate::SyntaxKind {
                self.__inner.get()
            }
        }
    };

    (
        $(#[$attrs: expr])*
        $visibility: vis $scope_name: ident $({ $($field: ident: $ty: ty),* })?,
        $kind: path,
        Override($($recoveries: path), *)
    ) => {
        crate::parser::define_scope_struct! {$visibility $scope_name {$($($field: $ty), *)?}, $kind}

        impl crate::parser::ParsingScope for $scope_name {
            fn recovery_method(&self) -> &crate::parser::RecoveryMethod {
                lazy_static::lazy_static! {
                    pub(super) static ref RECOVERY_METHOD: crate::parser::RecoveryMethod = {
                        #[allow(unused)]
                        use crate::SyntaxKind::*;
                        crate::parser::RecoveryMethod::override_(&[$($recoveries), *])
                    };
                }

                &RECOVERY_METHOD
            }

            fn syntax_kind(&self) -> crate::SyntaxKind {
                self.__inner.get()
            }
        }
    };
}

macro_rules! define_scope_struct {
    (
        $(#[$attrs: expr])*
        $visibility: vis $scope_name: ident { $($field: ident: $ty: ty),* },
        $kind: path
    ) => {
        $(#[$attrs])*
        #[derive(Debug, Clone)]
        $visibility struct $scope_name {
            __inner: std::rc::Rc<std::cell::Cell<crate::SyntaxKind>>,
            $($field: $ty),*
        }
        impl $scope_name {
            #[allow(unused)]
            fn set_kind(&mut self, kind: crate::SyntaxKind) {
                self.__inner.set(kind);
            }
        }
        impl Default for $scope_name {
            fn default() -> Self {
                use crate::SyntaxKind::*;
                Self {
                    __inner: std::cell::Cell::new($kind).into(),
                    $($field: Default::default()),*
                }
            }
        }
    };
}

use define_scope;
#[doc(hidden)]
use define_scope_struct;
