import gleam/list
import str

pub fn length_edge_cases_test() {
  // Precomposed vs decomposed: lengths should match (same grapheme count)
  assert str.length("é") == str.length("e\u{0301}")

  // Multiple combining marks on same base should be a single grapheme
  let multi = "a\u{0301}\u{0323}"
  // a + acute + dot below
  assert str.length(multi) == 1

  // ZWJ sequences (family/complex emoji) count as single grapheme
  assert str.length("👩‍❤️‍👨") == 1

  // Long string sanity: 500 'x' characters
  let long = list.fold(list.range(1, 500), "", fn(acc, _) { acc <> "x" })
  assert str.length(long) == 500
}
