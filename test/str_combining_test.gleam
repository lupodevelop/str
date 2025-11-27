import str/extra

// string module not required here

pub fn combining_ordering_test() {
  // Multiple combining marks on a base letter: acute + dot below
  let a = "a\u{0301}\u{0323}"
  // a + acute + dot below
  // Without decomposition, ascii_fold should remove combining marks
  assert extra.ascii_fold(a) == "a"

  // Decomposed precombined letter with multiple marks (simulate)
  let composed = "Å\u{0323}"
  // Å plus dot below
  let dec = extra.ascii_fold(composed)
  assert dec == "A"
}

pub fn long_combining_sequence_test() {
  // A base letter with several combining marks (tilde, acute, dot)
  let s = "o\u{0303}\u{0301}\u{0323}"
  assert extra.ascii_fold(s) == "o"
}
