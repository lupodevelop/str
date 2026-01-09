import str/config
import str/internal/decompose_pure as pure
import str/internal/decompose_native as native

// Delegating implementation layer for `decompose`.
// At runtime this may prefer a native implementation if available.
pub fn decompose_latin(s: String) -> String {
  case config.native_decompose_enabled() {
    True -> {
      // Native not yet implemented — keep pure fallback for safety.
      // Once native exists, replace with: native.decompose_latin_native(s)
      pure.decompose_latin_pure(s)
    }
    False -> pure.decompose_latin_pure(s)
  }
}
