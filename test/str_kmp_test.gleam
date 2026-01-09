import str

pub fn build_prefix_table_basic_test() {
  let p = "ababaca"
  assert str.build_prefix_table(p) == [0, 0, 1, 2, 3, 0, 1]
}

pub fn kmp_simple_test() {
  let text = "ababa"
  let pat = "aba"
  assert str.kmp_search_all(text, pat) == [0, 2]
}

pub fn kmp_overlapping_test() {
  let text = "aaaa"
  let pat = "aa"
  assert str.kmp_search_all(text, pat) == [0, 1, 2]
}

pub fn kmp_emoji_test() {
  // emoji sequence as single grapheme cluster
  let e = "👨‍👩‍👧‍👦"
  let text = e <> "x" <> e
  assert str.kmp_search_all(text, e) == [0, 2]
}

pub fn kmp_empty_pattern_test() {
  let text = "hello"
  let pat = ""
  assert str.kmp_search_all(text, pat) == []
}
