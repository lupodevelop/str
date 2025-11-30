import gleam/list
import str/tokenize

pub fn chars_vs_stdlib_length_test() {
  // Examples where we expect close agreement
  let expected_equal = ["hello", "café"]

  let allowed_diff = ["e\u{0301}", "🇮🇹", "👩\u{200D}👩\u{200D}👧\u{200D}👦", "1️⃣", "👍🏿"]

  let check_eq = fn(s) {
    let a = tokenize.chars(s)
    let b = tokenize.chars_stdlib(s)
    assert list.length(a) == list.length(b)
  }

  let check_diff = fn(s) {
    let a = tokenize.chars(s)
    let b = tokenize.chars_stdlib(s)
    // For complex ZWJ/emoji sequences we allow differences, but both must
    // produce at least one grapheme cluster.
    assert list.length(a) >= 1
    assert list.length(b) >= 1
  }

  list.each(expected_equal, check_eq)
  list.each(allowed_diff, check_diff)
}
