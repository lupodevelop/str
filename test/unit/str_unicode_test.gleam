import str

// NFC input should fold to ASCII base
pub fn ascii_fold_nfc_test() {
  let r = str.ascii_fold("á")
  assert r == "a"
}

// NFD (decomposed) input with combining mark should also fold to base
pub fn ascii_fold_nfd_test() {
  let decomposed = "a\u{0301}"
  let r = str.ascii_fold(decomposed)
  assert r == "a"
}

// slugify should handle accented input the same way
pub fn slugify_accents_test() {
  let opts = str.slugify_options() |> str.with_max_tokens(0) |> str.with_separator("-") |> str.with_preserve_unicode(False)
  let s = str.slugify_with_options("Crème Brûlée", opts)
  assert s == "creme-brulee"
}

// Truncate preserve should not split a ZWJ family emoji when asking for 1 cluster
pub fn zwj_preserve_test() {
  let s = "👩\u{200D}👩\u{200D}👧\u{200D}👦 family"
  let t = str.truncate_preserve(s, 1, "")
  assert t == "👩\u{200D}👩\u{200D}👧\u{200D}👦"
}