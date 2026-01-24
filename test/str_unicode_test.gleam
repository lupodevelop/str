import str

// NFC input should fold to ASCII base
pub fn ascii_fold_nfc_test() {
  let r = str.ascii_fold("á")
  assert r == "a"
}

// NFD (decomposed) input with combining mark should also fold to base
pub fn ascii_fold_nfd_test() {
  // 'a' + combining acute accent
  let decomposed = "a\u{0301}"
  let r = str.ascii_fold(decomposed)
  assert r == "a"
}

// slugify should handle accented input the same way
pub fn slugify_accents_test() {
  let s = str.slugify_opts("Crème Brûlée", 0, "-", False)
  assert s == "creme-brulee"
}

// Truncate preserve should not split a ZWJ family emoji when asking for 1 cluster
pub fn zwj_preserve_test() {
  let s = "👩\u{200D}👩\u{200D}👧\u{200D}👦 family"
  let t = str.truncate_preserve(s, 1, "")
  // The preserved cluster should be the family emoji
  assert t == "👩\u{200D}👩\u{200D}👧\u{200D}👦"
}
