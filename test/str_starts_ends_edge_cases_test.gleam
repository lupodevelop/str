import str

pub fn starts_ends_edge_cases_test() {
  // Empty prefix/suffix
  assert str.starts_with("", "") == True
  assert str.ends_with("", "") == True
  assert str.starts_with("a", "") == True
  assert str.ends_with("a", "") == True

  // Prefix/suffix longer than text
  assert str.starts_with("a", "ab") == False
  assert str.ends_with("a", "ba") == False

  // Emoji and combining boundaries
  assert str.starts_with("👨‍👩‍👧‍👦abc", "👨‍👩‍👧‍👦") == True
  assert str.ends_with("abc👨‍👩‍👧‍👦", "👨‍👩‍👧‍👦") == True
  assert str.starts_with("éclair", "é") == True
}
