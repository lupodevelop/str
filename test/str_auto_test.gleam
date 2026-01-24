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
