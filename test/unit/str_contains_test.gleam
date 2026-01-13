import str

pub fn contains_test() {
  assert str.contains("hello", "ell") == True
  assert str.contains("abc", "d") == False
  // Grapheme-aware: emoji sequence should be found as grapheme
  assert str.contains("👨‍👩‍👧‍👦 family", "👨‍👩‍👧‍👦") == True
  // Combining marks: searching for precomposed or decomposed should behave sensibly
  assert str.contains("café", "é") == True
}

