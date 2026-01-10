import str/internal/translit_impl as impl
import str/config
import str/internal/translit_pure as pure

// Delegating public surface for internal transliteration helpers.
pub fn replacements() -> List(#(String, String)) {
  impl.replacements()
}

pub fn remove_combining_marks(s: String) -> String {
  impl.remove_combining_marks(s)
}

pub fn transliterate_pure(s: String) -> String {
  // Expose pure transliteration directly for internal use in ascii folding.
  // Directly call the pure implementation to ensure single-pass behavior.
  // Once native transliteration is available, consider exposing a delegating
  // transliterate function instead.
  case config.native_translit_enabled() {
    True -> pure.transliterate_pure(s)
    False -> pure.transliterate_pure(s)
  }
}

/// - Type safety: exactly 2 elements guaranteed
