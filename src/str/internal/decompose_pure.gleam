/// Pure-Gleam fallback implementation for Latin decomposition.
import str/internal/decompose_pure_impl

pub fn decompose_latin_pure(s: String) -> String {
  decompose_pure_impl.decompose_impl(s)
}
