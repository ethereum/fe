use fxhash::FxHashSet;

use crate::{ParseError, SyntaxKind, TextRange};

use self::token_stream::{BackTrackableTokenStream, SyntaxToken, TokenStream};

pub mod token_stream;

/// Parser to build a rowan syntax tree.
pub struct Parser<S: TokenStream> {
    /// Token stream to parse.
    stream: BackTrackableTokenStream<S>,

    builder: rowan::GreenNodeBuilder<'static>,
    scopes: Vec<Box<dyn ParsingScope>>,
    errors: Vec<ParseError>,

    /// The checkpoint where the scope/branch is wrapped up later by another
    /// scope/branch when the `wrap_scope_with` method.
    check_point: Option<(rowan::Checkpoint, usize)>,

    current_pos: rowan::TextSize,
}

impl<S: TokenStream> Parser<S> {
    /// Returns the current token of the parser.
    pub fn current_token(&mut self) -> Option<&S::Token> {
        self.stream.peek()
    }

    /// Enters the scope and set the `scope` to the current scope.
    /// If `is_checkpoint` is true, the current scope/branch is wrapped up by
    /// another scope/branch later when the [`wrap_scope_with`] method is
    /// called.
    pub fn enter(&mut self, scope: Box<dyn ParsingScope>, is_checkpoint: bool) {
        if is_checkpoint {
            self.check_point = Some((self.builder.checkpoint(), self.scopes.len()));
        }

        self.builder.start_node(scope.syntax_kind().into());
        self.scopes.push(scope);
    }

    /// Enters the errors scope and add the `msg` to the error list.
    pub fn enter_with_error(&mut self, msg: &str) {
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
        self.scopes.push(Box::new(ErrorScope()));
    }

    /// Leaves the current scope/branch.
    pub fn leave(&mut self) {
        self.scopes.pop();
        self.builder.finish_node();
    }

    /// Wrap up the marked scope/branch with another scope/branch and set the
    /// `scope` to the current scope.
    pub fn wrap_scope_with(&mut self, scope: Box<dyn ParsingScope>) {
        debug_assert!(self.check_point.is_some(), "No checkpoint");
        let check_point = self.check_point.take().unwrap();
        let syntax_kind = scope.syntax_kind();

        self.scopes.truncate(check_point.1);
        self.scopes.push(scope);

        self.builder
            .start_node_at(check_point.0, syntax_kind.into());
    }

    /// Bumps the current token and adds it to the current branch.
    pub fn bump(&mut self) {
        let tok = self.stream.next().unwrap();
        self.current_pos += rowan::TextSize::of(tok.text());
        self.builder.token(tok.syntax_kind().into(), tok.text());
    }

    /// Bumps consecutive trivia tokens.
    pub fn bump_trivias(&mut self) {
        while let Some(tok) = self.current_token() {
            let kind = tok.syntax_kind();
            if kind.is_trivia() {
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

    /// Proceeds the parser to the recovery token of the current scope. Then
    /// leave the current branch/scope.
    pub fn recovery(&mut self) {
        let mut scope_index = self.scopes.len() - 1;
        // Finds the nearest scope that has its own recovery set.
        loop {
            if self.scopes[scope_index].recovery_method() != &RecoveryMethod::Inheritance
                || scope_index == 0
            {
                break;
            } else {
                scope_index -= 1;
            }
        }

        while let Some(tok) = self.stream.peek() {
            let syntax_kind = tok.syntax_kind();
            if self.scopes[scope_index]
                .recovery_method()
                .contains(syntax_kind)
            {
                break;
            } else {
                self.bump();
            }
        }

        self.leave();
    }
}

/// The current scope of parsing.
pub trait ParsingScope {
    /// Returns the recovery method of the current scope.
    fn recovery_method(&self) -> &RecoveryMethod;

    fn syntax_kind(&self) -> SyntaxKind;
}

pub struct ErrorScope();

impl ParsingScope for ErrorScope {
    fn recovery_method(&self) -> &RecoveryMethod {
        &RecoveryMethod::Inheritance
    }

    fn syntax_kind(&self) -> SyntaxKind {
        SyntaxKind::Error
    }
}

/// Represents the recovery method of the current scope.
#[derive(PartialEq, Eq)]
pub enum RecoveryMethod {
    /// Uses the recovery method of the parent scope.
    Inheritance,

    /// The scope has its own recovery set.
    RecoverySet(FxHashSet<SyntaxKind>),
}

impl RecoveryMethod {
    /// Returns `true` if the recovery set contains the given syntax kind.
    fn contains(&self, syntax_kind: SyntaxKind) -> bool {
        match self {
            RecoveryMethod::Inheritance => false,
            RecoveryMethod::RecoverySet(set) => set.contains(&syntax_kind),
        }
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
