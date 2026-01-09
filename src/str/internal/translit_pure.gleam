import gleam/list
import gleam/string

/// Pure-Gleam fallback for transliteration tables and helpers.
import str/internal/generated_translit_pairs as gen

pub fn replacements_pure() -> List(#(String, String)) {
  gen.replacements_generated()
}

pub fn remove_combining_marks_pure(s: String) -> String {
  let cps = string.to_utf_codepoints(s)
  let filtered =
    list.filter(cps, fn(cp) {
      case string.utf_codepoint_to_int(cp) {
        i ->
          case
            i >= 0x0300
            && i <= 0x036F
            || i >= 0x1AB0
            && i <= 0x1AFF
            || i >= 0x1DC0
            && i <= 0x1DFF
            || i >= 0x20D0
            && i <= 0x20FF
            || i >= 0xFE20
            && i <= 0xFE2F
          {
            True -> False
            False -> True
          }
      }
    })

  string.from_utf_codepoints(filtered)
}
