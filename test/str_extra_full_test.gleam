import str
import str/internal/decompose

// ASCII fold basic mappings (precomposed)
pub fn ascii_fold_basic_test() {
  assert str.ascii_fold("Å") == "A"
  assert str.ascii_fold("æ") == "ae"
  assert str.ascii_fold("œ") == "oe"
  assert str.ascii_fold("ß") == "ss"
  assert str.ascii_fold("Ł") == "L"
}

// Decomposed input handling: decomposer expands and ascii_fold removes marks
pub fn ascii_fold_decomposed_test() {
  let dec = decompose.decompose_latin("Å")
  // decomposed should contain combining ring
  assert dec != "Å"
  // ascii_fold should normalize decomposed to base
  assert str.ascii_fold(dec) == "A"
  // ascii_fold_no_decompose should leave decomposed sequence intact
  assert str.ascii_fold_no_decompose(dec) == dec
}

// slugify behavior with preserve_unicode flag
pub fn slugify_preserve_flag_test() {
  let s1 = str.slugify_opts("Crème Brûlée", 0, "-", False)
  assert s1 == "creme-brulee"

  let s2 = str.slugify_opts("mañana niño", 0, "-", True)
  assert s2 == "mañana-niño"

  let s3 = str.slugify_opts("hello   world!!", 0, "-", False)
  assert s3 == "hello-world"

  let s4 = str.slugify_opts("one two three four", 2, "-", False)
  assert s4 == "one-two"
}

// Naming helpers
pub fn naming_helpers_test() {
  assert str.to_snake_case("Hello World") == "hello_world"
  assert str.to_kebab_case("Hello World") == "hello-world"
  assert str.to_camel_case("hello world") == "helloWorld"
}

// Truncation edge cases and suffix logic
pub fn truncation_suffix_test() {
  let t = str.truncate("hello", 3, "..")
  // max_len 3, suffix length 2 -> take 1 char then suffix
  assert t == "h.."

  // ensure preserve doesn't split grapheme sequences (ZWJ family)
  let family = "👩\u{200D}👩\u{200D}👧\u{200D}👦 family"
  let p = str.truncate_preserve(family, 1, "")
  assert p == "👩\u{200D}👩\u{200D}👧\u{200D}👦"
}
