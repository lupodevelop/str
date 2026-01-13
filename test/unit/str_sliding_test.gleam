import str

pub fn sliding_simple_test() {
  let text = "ababa"
  let pat = "aba"
  assert str.sliding_search_all(text, pat) == [0, 2]
}

pub fn sliding_overlapping_test() {
  let text = "aaaa"
  let pat = "aa"
  assert str.sliding_search_all(text, pat) == [0, 1, 2]
}

pub fn sliding_emoji_test() {
  let e = "👨‍👩‍👧‍👦"
  let text = e <> "x" <> e
  assert str.sliding_search_all(text, e) == [0, 2]
}

pub fn sliding_empty_pattern_test() {
  let text = "hello"
  let pat = ""
  assert str.sliding_search_all(text, pat) == []
}
