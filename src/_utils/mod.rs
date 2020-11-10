/// Formats any kind of structured text that uses curly braces blocks
pub fn pretty_curly_print(text: &str, indent: usize) -> String {
    let mut formatted = String::new();
    let mut level = 0;
    let mut previous_char: Option<char> = None;

    const CURLY_OPEN: char = '{';
    const CURLY_CLOSE: char = '}';
    const WHITESPACE: char = ' ';
    const NEWLINE: &str = "\n";

    for character in text.chars() {
        match character {
            CURLY_OPEN => {
                level += 1;
                formatted.push(character);
                formatted.push_str(NEWLINE);
                formatted.push_str(&WHITESPACE.to_string().repeat(indent * level));
            }
            CURLY_CLOSE => {
                level -= 1;
                formatted.push_str(NEWLINE);
                formatted.push_str(&WHITESPACE.to_string().repeat(indent * level));
                formatted.push(character);
                formatted.push_str(NEWLINE);
                formatted.push_str(&WHITESPACE.to_string().repeat(indent * level));
            }
            WHITESPACE => {
                if !matches!(previous_char, Some(CURLY_CLOSE)) {
                    formatted.push(character)
                }
            }
            _ => formatted.push(character),
        }
        previous_char = Some(character);
    }

    formatted
}
