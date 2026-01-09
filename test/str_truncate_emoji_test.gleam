import str

pub fn truncate_preserve_emoji_test() {
  let s = "Hello 👨‍👩‍👧‍👦 World"
  assert str.truncate_preserve(s, 10, "...") == "Hello 👨‍👩‍👧‍👦..."
}

pub fn truncate_strict_splits_emoji_test() {
  let s = "Hi 👩‍👩‍👧‍👦"
  assert str.truncate_strict(s, 3, "…") == "Hi…"
}