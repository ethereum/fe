/// A trait to derive plural or singular representations from
pub trait Pluralizable {
    fn to_plural(&self) -> String;

    fn to_singular(&self) -> String;
}

impl Pluralizable for &str {
    fn to_plural(&self) -> String {
        if self.ends_with('s') {
            self.to_string()
        } else {
            format!("{self}s")
        }
    }

    fn to_singular(&self) -> String {
        if self.ends_with('s') {
            self[0..self.len() - 1].to_string()
        } else {
            self.to_string()
        }
    }
}

// Impl Pluralizable for (singular, plural)
impl Pluralizable for (&str, &str) {
    fn to_plural(&self) -> String {
        self.1.to_string()
    }

    fn to_singular(&self) -> String {
        self.0.to_string()
    }
}

// Pluralize the given pluralizable if the `count` is greater than one.
pub fn pluralize_conditionally(pluralizable: impl Pluralizable, count: usize) -> String {
    if count == 1 {
        pluralizable.to_singular()
    } else {
        pluralizable.to_plural()
    }
}
