import gleam/list
import gleam/string

/// Pure-Gleam fallback implementation for Latin decomposition.
import str/internal/generated_decompose_pages as pages_gen

pub fn decompose_latin_pure(s: String) -> String {
  // Use the page-based lookup to perform grapheme-wise decomposition (single-pass).
  let gs = string.to_graphemes(s)
  let parts = list.fold(gs, [], fn(acc, g) {
    case pages_gen.decompose_pages_lookup_by_grapheme(g) {
      Ok(v) -> [v, ..acc]
      Error(_) -> [g, ..acc]
    }
  })
  |> list.reverse
  |> string.concat

  parts
}
