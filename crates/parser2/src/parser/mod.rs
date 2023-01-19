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

type Checkpoint = rowan::Checkpoint;

/// Parser to build a rowan syntax tree.
pub struct Parser<S: TokenStream> {
    /// Token stream to parse.
    stream: BackTrackableTokenStream<S>,

    builder: rowan::GreenNodeBuilder<'static>,
    scopes: Vec<Box<dyn ParsingScope>>,
    errors: Vec<ParseError>,

    current_pos: rowan::TextSize,
    next_trivias: VecDeque<S::Token>,
    /// The dry run states which holds the each state of the parser when it
    /// enters dry run mode.
    dry_run_states: Vec<DryRunState>,
}

impl<S: TokenStream> Parser<S> {
    /// Create a parser with the given token stream.
    pub fn new(stream: S) -> Self {
        Self {
            stream: BackTrackableTokenStream::new(stream),
            builder: rowan::GreenNodeBuilder::new(),
            scopes: Vec::new(),
            errors: Vec::new(),
            current_pos: rowan::TextSize::from(0),
            next_trivias: VecDeque::new(),
            dry_run_states: Vec::new(),
        }
    }

    /// Returns the current token of the parser.
    pub fn current_token(&mut self) -> Option<&S::Token> {
        if !self.next_trivias.is_empty() {
            Some(&self.next_trivias[0])
        } else {
            self.stream.peek()
        }
    }

    /// Returns the current token kind of the parser.
    pub fn current_kind(&mut self) -> Option<SyntaxKind> {
        self.current_token().map(|token| token.syntax_kind())
    }

    /// Finish the parsing and return the syntax tree.
    pub fn finish(self) -> (SyntaxNode, Vec<ParseError>) {
        debug_assert!(self.scopes.is_empty());
        debug_assert!(!self.is_dry_run());

        (SyntaxNode::new_root(self.builder.finish()), self.errors)
    }

    /// Invoke the scope to parse. The scope is wrapped up by the node specified
    /// by the scope.
    /// Returns `true` if parse succeeded, otherwise `false`.
    ///
    /// * If the checkpoint is `Some`, the marked branch is wrapped up by the
    ///   node.
    /// * If the checkpoint is `None`, the current branch is wrapped up by the
    ///   node.
    pub fn parse<T>(&mut self, mut scope: T, checkpoint: Option<Checkpoint>) -> bool
    where
        T: Parse + 'static,
    {
        let checkpoint = self.enter(scope.clone(), checkpoint);
        let error_len = self.errors.len();
        scope.parse(self);
        self.leave(checkpoint);
        error_len == self.errors.len()
    }

    #[doc(hidden)]
    /// Enter the scope and return the checkpoint. The checkpoint branch will be
    /// wrapped up by the scope's node when [`leave`] is called.
    // NOTE: This method is limited to testing and internal usage.
    pub fn enter<T>(&mut self, scope: T, checkpoint: Option<Checkpoint>) -> Checkpoint
    where
        T: ParsingScope + 'static,
    {
        self.scopes.push(Box::new(scope));
        checkpoint.unwrap_or_else(|| self.checkpoint())
    }

    #[doc(hidden)]
    /// Leave the scope and wrap up the checkpoint by the scope's node.
    // NOTE: This method is limited to testing and internal usage.
    pub fn leave(&mut self, checkpoint: Checkpoint) {
        let scope = self.scopes.pop().unwrap();
        if !self.is_dry_run() {
            self.builder
                .start_node_at(checkpoint, scope.syntax_kind().into());
            self.builder.finish_node();
        }
    }

    /// Marks the current branch as a checkpoint.
    /// The checked branch is wrapped up later when [`parse]` is
    /// called with the `checkpoint`.
    pub fn checkpoint(&mut self) -> Checkpoint {
        self.builder.checkpoint()
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

    /// Add the `msg` to the error list and bumps n token in the error branch.
    pub fn error_and_bump(&mut self, msg: &str, bump_n: usize) {
        let error = self.error(msg);
        let checkpoint = self.enter(error, None);
        for _ in 0..bump_n {
            self.bump();
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
        });
    }

    /// Ends the dry run mode.
    /// See `[start_dry_run]` for more details.
    pub fn end_dry_run(&mut self) {
        self.stream.backtrack();
        let state = self.dry_run_states.pop().unwrap();
        self.errors.truncate(state.err_num);
        self.current_pos = state.pos;
    }

    /// Bumps the current token and
    /// current branch.
    pub fn bump(&mut self) {
        let tok = match self.next_trivias.pop_front() {
            Some(tok) => tok,
            None => self.stream.next().unwrap(),
        };

        self.current_pos += rowan::TextSize::of(tok.text());
        if !self.is_dry_run() {
            self.builder.token(tok.syntax_kind().into(), tok.text());
        }
    }

    /// Peek the next non-trivia token.
    /// If `skip_newlines` is `true`, newlines are also treated as trivia.
    pub fn peek_non_trivia(&mut self, skip_newlines: bool) -> Option<SyntaxKind> {
        if !skip_newlines {
            for tok in &self.next_trivias {
                if tok.syntax_kind() == SyntaxKind::Newline {
                    return Some(SyntaxKind::Newline);
                }
            }
        }

        while let Some(next) = self.stream.peek() {
            let kind = next.syntax_kind();
            if kind.is_trivia() || (skip_newlines && kind == SyntaxKind::Newline) {
                self.next_trivias.push_back(self.stream.next().unwrap());
                continue;
            } else {
                return Some(kind);
            }
        }

        None
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

    /// Bumps consecutive trivia tokens.
    /// If `bump_newlines` is true, newlines are also bumped.
    pub fn bump_trivias(&mut self, bump_newlines: bool) {
        while let Some(tok) = self.current_token() {
            let kind = tok.syntax_kind();
            if kind.is_trivia() || (bump_newlines && kind == SyntaxKind::Newline) {
                self.bump();
            } else {
                break;
            }
        }
    }

    /// Bump consecutive newlines.
    pub fn bump_newlines(&mut self) {
        while let Some(tok) = self.current_token() {
            if tok.syntax_kind() == SyntaxKind::Newline {
                self.bump();
            } else {
                break;
            }
        }
    }

    /// Proceeds the parser to the recovery token of the current scope.
    pub fn recover(&mut self) {
        let mut recovery_set: FxHashSet<SyntaxKind> = fxhash::FxHashSet::default();
        let mut scope_index = self.scopes.len() - 1;
        loop {
            match self
                .scopes
                .get(scope_index)
                .map(|scope| scope.recovery_method())
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

        while let Some(tok) = self.stream.peek() {
            let syntax_kind = tok.syntax_kind();
            if recovery_set.contains(&syntax_kind) {
                break;
            } else {
                self.bump();
            }
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
}

/// The current scope of parsing.
pub trait ParsingScope {
    /// Returns the recovery method of the current scope.
    fn recovery_method(&self) -> &RecoveryMethod;

    fn syntax_kind(&self) -> SyntaxKind;
}

pub trait Parse: ParsingScope + Clone {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>);
}

define_scope! {
    ErrorScope,
    Error,
    Inheritance
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
    fn inheritance_empty() -> Self {
        Self::Inheritance(fxhash::FxHashSet::default())
    }

    fn inheritance(tokens: &[SyntaxKind]) -> Self {
        Self::Inheritance(tokens.iter().copied().collect())
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
    RootScope,
    Root,
    Override()
}

macro_rules! define_scope {
    ($scope_name: ident, $kind: path, Inheritance) => {
        #[derive(Default, Debug, Clone, Copy)]
        pub struct $scope_name {}

        impl crate::parser::ParsingScope for $scope_name {
            fn recovery_method(&self) -> &crate::parser::RecoveryMethod {
                lazy_static::lazy_static! {
                    pub(super) static ref RECOVERY_METHOD: crate::parser::RecoveryMethod = {
                        crate::parser::RecoveryMethod::inheritance_empty()
                    };
                }

                &RECOVERY_METHOD
            }

            fn syntax_kind(&self) -> crate::SyntaxKind {
                use crate::SyntaxKind::*;
                $kind
            }
        }
    };

    ($scope_name: ident, $kind: path, Inheritance($($recoveries: path), *)) => {
        #[derive(Default, Debug, Clone, Copy)]
        pub struct $scope_name {}

        impl crate::parser::ParsingScope for $scope_name {
            fn recovery_method(&self) -> &crate::parser::RecoveryMethod {
                lazy_static::lazy_static! {
                    pub(super) static ref RECOVERY_METHOD: crate::parser::RecoveryMethod = {
                        #[allow(unused)]
                        use crate::SyntaxKind::*;
                        let set: fxhash::FxHashSet<crate::SyntaxKind> = vec![
                            $($recoveries), *
                        ].into_iter().map(|kind: SyntaxKind| kind.into()).collect();

                        crate::parser::RecoveryMethod::Inheritance(set)
                    };
                }

                &RECOVERY_METHOD
            }

            fn syntax_kind(&self) -> crate::SyntaxKind {
                use crate::SyntaxKind::*;
                $kind
            }
        }
    };

    ($scope_name: ident, $kind: path, Override($($recoveries: path), *)) => {
        #[derive(Default, Debug, Clone, Copy)]
        pub struct $scope_name {}

        impl crate::parser::ParsingScope for $scope_name {
            fn recovery_method(&self) -> &crate::parser::RecoveryMethod {
                lazy_static::lazy_static! {
                    pub(super) static ref RECOVERY_METHOD: crate::parser::RecoveryMethod = {
                        #[allow(unused)]
                        use crate::SyntaxKind::*;
                        let set: fxhash::FxHashSet<crate::SyntaxKind> = vec![
                            $($recoveries), *
                        ].into_iter().map(|kind: $crate::SyntaxKind| kind.into()).collect();

                        crate::parser::RecoveryMethod::Override(set)
                    };
                }

                &RECOVERY_METHOD
            }

            fn syntax_kind(&self) -> crate::SyntaxKind {
                use crate::SyntaxKind::*;
                $kind
            }
        }
    };
}

struct DryRunState {
    /// The text position is the position when the dry run started.
    pos: rowan::TextSize,
    /// The number of errors when the dry run started.
    err_num: usize,
}

use define_scope;
