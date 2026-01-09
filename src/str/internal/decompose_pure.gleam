import gleam/list
import gleam/string

/// Pure-Gleam fallback implementation for Latin decomposition.
import str/internal/generated_decompose_pairs as gen

pub fn decompose_latin_pure(s: String) -> String {
  let table = gen.decompose_pairs_generated()

  list.fold(table, s, fn(acc, pair) {
    let #(from, to) = pair
    string.replace(acc, from, to)
  })
}
