import str

pub fn contains_edge_cases_test() {
  // Empty needle returns False (index_of treats empty needle as Error)
  assert str.contains("", "") == False

  // Needle longer than haystack
  assert str.contains("a", "aa") == False

  // Simple ascii checks
  assert str.contains("hello world", "world") == True
  assert str.contains("hello world", "planet") == False

  // contains_any / contains_all behavior for lists (ascii)
  assert str.contains_any("abc def", ["x", "def"]) == True
  assert str.contains_all("abc def", ["abc", "def"]) == True
}
