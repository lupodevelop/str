import str/config
import str/internal/translit_pure as pure
import str/internal/translit_native as native

// Delegating implementation layer for `translit`.
// At runtime this may prefer a native implementation if available.
pub fn replacements() -> List(#(String, String)) {
  case config.native_translit_enabled() {
    True -> native.replacements_native()
    False -> pure.replacements_pure()
  }
}

pub fn remove_combining_marks(s: String) -> String {
  case config.native_combining_marks_enabled() {
    True -> native.remove_combining_marks_native(s)
    False -> pure.remove_combining_marks_pure(s)
  }
}
