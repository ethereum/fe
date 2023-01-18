use crate::SyntaxKind;

/// This trait works as an abstraction layer to encapsulate the differences
/// between input sources. There are mainly two types of input sources,
/// 1. text in source file
/// 2. tokens stream produced by procedural macros.
pub trait TokenStream {
    type Token: SyntaxToken;

    /// Returns the next token in the stream.
    fn next(&mut self) -> Option<Self::Token>;

    /// Returns the next token in the stream without consuming it.
    fn peek(&mut self) -> Option<&Self::Token>;
}

/// This trait represents a single token in the token stream.
pub trait SyntaxToken: Clone {
    /// Returns `SyntaxKind` of the token.
    fn syntax_kind(&self) -> SyntaxKind;

    /// Returns raw text of the token.
    fn text(&self) -> &str;
}

/// This struct is a thin wrapper around `TokenStream` which allows the parser
/// to backtrack.
pub struct BackTrackableTokenStream<T: TokenStream> {
    stream: T,
    /// Backtrack buffer which stores tokens that have been already consumed.
    bt_buffer: Vec<T::Token>,
    bt_points: Vec<usize>,
    /// Points to the current position of the backtrack buffer.
    bt_cursor: Option<usize>,
}

impl<T: TokenStream> BackTrackableTokenStream<T> {
    /// Creates a new `BackTrackableTokenStream` from the given `TokenStream`.
    pub fn new(stream: T) -> Self {
        Self {
            stream,
            bt_buffer: Vec::new(),
            bt_points: Vec::new(),
            bt_cursor: None,
        }
    }

    /// Returns the next token in the stream.
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<T::Token> {
        if let Some(cursor) = self.bt_cursor {
            if cursor < self.bt_buffer.len() {
                let token = self.bt_buffer.get(cursor).cloned();
                self.bt_cursor = Some(cursor + 1);
                return token;
            }
        }

        let token = self.stream.next()?;
        if self.has_bt_point() {
            self.bt_buffer.push(token.clone());
        }
        if let Some(cursor) = self.bt_cursor {
            self.bt_cursor = Some(cursor + 1);
        }
        Some(token)
    }

    /// Returns the next token in the stream without consuming it.
    pub fn peek(&mut self) -> Option<&T::Token> {
        if let Some(cursor) = self.bt_cursor {
            if cursor < self.bt_buffer.len() {
                return self.bt_buffer.get(cursor);
            }
        }

        self.stream.peek()
    }

    /// Set a backtrack point which allows the parser to backtrack to this
    /// point.
    pub fn set_bt_point(&mut self) {
        println!("{}", self.bt_buffer.len());
        self.bt_points.push(self.bt_buffer.len());
    }

    /// Remove the last backtrack point.
    pub fn complete(&mut self) {
        self.bt_cursor = None;
        if !self.has_bt_point() {
            self.bt_buffer.clear();
        }
    }

    /// Backtracks to the last backtrack point.
    ///
    /// # Panics
    /// Panics if the `set_bt_point` method has not been called before.
    pub fn backtrack(&mut self) {
        debug_assert!(self.has_bt_point(), "backtrack without `bt_point`");
        self.bt_cursor = Some(self.bt_points.pop().unwrap());
    }

    /// Returns `true` if the stream has a backtrack point.
    pub fn has_bt_point(&mut self) -> bool {
        !self.bt_points.is_empty()
    }
}
