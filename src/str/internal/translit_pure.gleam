import gleam/list
import gleam/string

/// Pure-Gleam fallback for transliteration tables and helpers.
import str/internal/generated_translit_pairs as gen
import str/internal/generated_translit_pages as pages_gen

pub fn replacements_pure() -> List(#(String, String)) {
  gen.replacements_generated()
}

// Fast grapheme-wise transliteration using generated pages lookup.
pub fn transliterate_pure(s: String) -> String {
  let gs = string.to_graphemes(s)
  let parts = list.fold(gs, [], fn(acc, g) {
    case pages_gen.translit_pages_lookup_by_grapheme(g) {
      Ok(v) -> [v, ..acc]
      Error(_) -> [g, ..acc]
    }
  })
  |> list.reverse
  |> string.concat

  parts
}

import str/internal/generated_combining_bits as bits

pub fn remove_combining_marks_pure(s: String) -> String {
  let cps = string.to_utf_codepoints(s)
  let filtered =
    list.filter(cps, fn(cp) {
      case string.utf_codepoint_to_int(cp) {
        i -> case bits.is_combining_mark(i) {
          True -> False
          False -> True
        }
      }
    })

  string.from_utf_codepoints(filtered)
}
