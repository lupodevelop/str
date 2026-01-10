import gleam/list
import gleam/string

/// Fallback transliteration based on folding the replacements table.
import str/internal/generated_translit_pairs as gen

pub fn replacements_pure() -> List(#(String, String)) {
  gen.replacements_generated()
}

pub fn remove_combining_marks_pure(s: String) -> String {
  // Simple fallback: no-op
  s
}

pub fn transliterate_pure(s: String) -> String {
  let reps = gen.replacements_generated()
  list.fold(reps, s, fn(acc, pair) {
    let #(from, to) = pair
    string.replace(acc, from, to)
  })
}
