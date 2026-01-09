import str/config
import str/internal/translit_pure as pure
import str/internal/translit_native as native

// Delegating implementation layer for `translit`.
// At runtime this may prefer a native implementation if available.
pub fn replacements() -> List(#(String, String)) {
  case config.native_translit_enabled() {
    True -> {
      // Native not yet implemented — keep pure fallback for safety.
      // Once native exists, call native.replacements_native()
      pure.replacements_pure()
    }
    False -> pure.replacements_pure()
  }
}

pub fn remove_combining_marks(s: String) -> String {
  case config.native_combining_marks_enabled() {
    True -> {
      // Native not yet implemented — keep pure fallback for safety.
      // Once native exists, call native.remove_combining_marks_native(s)
      pure.remove_combining_marks_pure(s)
    }
    False -> pure.remove_combining_marks_pure(s)
  }
}
