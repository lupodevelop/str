import str

pub fn starts_ends_test() {
  assert str.starts_with("hello", "he") == True
  assert str.starts_with("hello", "ello") == False
  assert str.ends_with("hello", "lo") == True
  assert str.ends_with("hello", "he") == False

  // Emoji boundaries
  assert str.starts_with("👨‍👩‍👧‍👦abc", "👨‍👩‍👧‍👦") == True
  assert str.ends_with("abc👨‍👩‍👧‍👦", "👨‍👩‍👧‍👦") == True

  // Combining marks
  assert str.starts_with("éclair", "é") == True
  assert str.ends_with("café", "é") == True
}
