/// Unit tests for automatic search strategy selection.
///
/// This file tests the "auto" variants of search functions that
/// automatically choose between KMP and Sliding strategies based on
/// heuristics (pattern length, text length, border size).
///
/// Functions tested:
/// - index_of_auto: Should match index_of results
/// - count_auto: Should match count results (both overlapping modes)
///
/// Purpose:
/// Ensure the automatic strategy selector produces identical results
/// to the default strategy, validating that optimization doesn't break
/// correctness.
import gleam/list
import str

pub fn index_of_auto_matches_legacy_test() {
  let cases = [
    #("hello world", "world"),
    #("aaaa", "aa"),
    #("", ""),
    #("abc", "d"),
    #("👨‍👩‍👧‍👦 family", "family"),
  ]

  list.each(cases, fn(pair) {
    let #(text, pat) = pair
    assert str.index_of_auto(text, pat) == str.index_of(text, pat)
  })
}

pub fn count_auto_matches_legacy_test() {
  let cases = [
    #("aaaa", "aa"),
    #("ababab", "ab"),
    #("hello", "lo"),
    #("👨‍👩‍👧‍👦 family", " "),
  ]

  // overlapping True
  list.each(cases, fn(pair) {
    let #(text, pat) = pair
    assert str.count_auto(text, pat, True) == str.count(text, pat, True)
  })

  // non-overlapping False
  list.each(cases, fn(pair) {
    let #(text, pat) = pair
    assert str.count_auto(text, pat, False) == str.count(text, pat, False)
  })
}
// helper removed: previously unused `repeat` function

