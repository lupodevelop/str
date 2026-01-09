import str/internal/translit_impl as impl

// Delegating public surface for internal transliteration helpers.
pub fn replacements() -> List(#(String, String)) {
  impl.replacements()
}

pub fn remove_combining_marks(s: String) -> String {
  impl.remove_combining_marks(s)
}
/// - Type safety: exactly 2 elements guaranteed
