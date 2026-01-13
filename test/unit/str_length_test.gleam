import str

pub fn length_basic_test() {
  // Basic ASCII
  assert str.length("hello") == 5
  // Empty string
  assert str.length("") == 0
  // Combining characters (café: c a f e with accent)
  assert str.length("café") == 4
  // Emoji family (ZWJ sequence) should be counted as 1 grapheme
  assert str.length("👨‍👩‍👧‍👦") == 1
  // Complex emoji with skin tone modifier
  assert str.length("👍🏽") == 1
  // Multilingual example
  assert str.length("naïve") == 5
}
