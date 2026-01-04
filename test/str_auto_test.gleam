import gleam/list
import str/core

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
    assert core.index_of_auto(text, pat) == core.index_of(text, pat)
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
    assert core.count_auto(text, pat, True) == core.count(text, pat, True)
  })

  // non-overlapping False
  list.each(cases, fn(pair) {
    let #(text, pat) = pair
    assert core.count_auto(text, pat, False) == core.count(text, pat, False)
  })
}

// helper removed: previously unused `repeat` function
