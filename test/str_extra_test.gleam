import gleam/list
import gleam/string
import str/extra
import str/internal_decompose
import str/internal_translit

pub fn ascii_fold_internal_helpers_test() {
  // remove combining marks should strip acute accent
  let with_comb = "e\u{0301}"
  let removed = internal_translit.remove_combining_marks(with_comb)
  assert removed == "e"

  // decompose Latin should expand common precomposed characters
  let dec = internal_decompose.decompose_latin("é")
  // decomposer may produce e + combining acute; ensure combining exists
  assert string.contains(dec, "\u{0301}")
}

pub fn ascii_fold_ligature_test() {
  // ligature æ should transliterate to ae
  assert extra.ascii_fold("æ") == "ae" || extra.ascii_fold("Æ") == "AE"
}

pub fn slugify_normalizer_behavior_test() {
  // If preserve_unicode is True the normalizer must NOT be applied.
  let s = "Café"
  let noisy = fn(_) { "X" }
  let res = extra.slugify_opts_with_normalizer(s, -1, "-", True, noisy)
  assert res == "café"
}

pub fn slugify_with_normalizer_token_limit_test() {
  let s = "uno due tre quattro"
  let fake = fn(x) { x }
  // identity normalizer
  let slug = extra.slugify_opts_with_normalizer(s, 2, "-", False, fake)
  assert slug == "uno-due"
}

pub fn camel_and_snake_tests() {
  let s = "  multiple   separators__and--caps "
  let camel = extra.to_camel_case(s)
  assert string.length(camel) > 0
  assert string.slice(camel, 0, 1) != "-"
  let snake = extra.to_snake_case("Hello World")
  assert snake == "hello_world"
}

pub fn slugify_emoji_and_numbers_test() {
  let s = "I ❤️ Gleam 2025"
  assert extra.slugify(s) == "i-gleam-2025"
}

pub fn ascii_fold_no_decompose_identity_test() {
  // ascii_fold_no_decompose with identity normalizer should preserve precomposed handling
  let s = "Ångström"
  let res = extra.ascii_fold_no_decompose_with_normalizer(s, fn(x) { x })
  assert string.contains(res, "A") || string.contains(res, "Ang")
}

pub fn ascii_fold_with_normalizer_effect_test() {
  // fake normalizer that turns ö into o + diaeresis combining
  let fake = fn(x) { string.replace(x, "ö", "o\u{0308}") }
  let res = extra.ascii_fold_with_normalizer("schröder", fake)
  assert string.contains(res, "o")
}

pub fn to_kebab_equals_slugify_test() {
  assert extra.to_kebab_case("Hello World") == extra.slugify("Hello World")
}

pub fn ascii_fold_basic_test() {
  assert extra.ascii_fold("ÀÁÂÃÄÅ") == "AAAAAA"
  assert extra.ascii_fold("àáâãäå") == "aaaaaa"
}

pub fn ascii_fold_specials_test() {
  assert extra.ascii_fold("Çç") == "Cc"
  assert extra.ascii_fold("Ææß") == "AEaess"
}

pub fn slugify_basic_test() {
  let s = "Hello, World!"
  assert extra.slugify(s) == "hello-world"
}

pub fn slugify_accent_test() {
  let s = "Café déjà vu"
  assert extra.slugify(s) == "cafe-deja-vu"
}

pub fn slugify_emoji_removed_test() {
  let s = "I ❤️ Gleam"
  assert extra.slugify(s) == "i-gleam"
}

pub fn slugify_multiple_separators_test() {
  let s = "a--b__c"
  assert extra.slugify(s) == "a-b-c"
}

pub fn slugify_numbers_test() {
  let s = "2025 Year!"
  assert extra.slugify(s) == "2025-year"
}

pub fn to_snake_case_test() {
  assert extra.to_snake_case("Hello World") == "hello_world"
}

pub fn to_camel_case_test() {
  let s = "Hello Fancy World"
  assert extra.to_camel_case(s) == "helloFancyWorld"
}

pub fn camel_case_edge_test() {
  let s = "  multiple   separators__and--caps "
  let c = extra.to_camel_case(s)
  assert string.length(c) > 0
  assert string.slice(c, 0, 1) != "-"
}

pub fn slugify_trim_test() {
  let s = "---Hello---"
  assert extra.slugify(s) == "hello"
}

pub fn slugify_preserves_digits_test() {
  let s = "X1 Y2 Z3"
  assert extra.slugify(s) == "x1-y2-z3"
}

pub fn slugify_opts_max_len_test() {
  let s = "a b c d e"
  assert extra.slugify_opts(s, 3, "-", False) == "a-b-c"
}

pub fn slugify_opts_sep_test() {
  let s = "Hello World"
  assert extra.slugify_opts(s, -1, "_", False) == "hello_world"
}

pub fn slugify_opts_preserve_unicode_true_test() {
  let s = "Café ❤️ Gleam"
  assert extra.slugify_opts(s, -1, "-", True) == "café-❤️-gleam"
}

pub fn slugify_opts_preserve_unicode_false_test() {
  let s = "Café ❤️ Gleam"
  assert extra.slugify_opts(s, -1, "-", False) == "cafe-gleam"
}

pub fn ascii_fold_german_test() {
  assert extra.ascii_fold("Müller") == "Muller"
  assert extra.ascii_fold("Größe") == "Grosse"
}

pub fn ascii_fold_french_test() {
  assert extra.ascii_fold("français") == "francais"
  assert extra.ascii_fold("œuvre") == "oeuvre"
}

pub fn ascii_fold_spanish_test() {
  assert extra.ascii_fold("niño") == "nino"
  assert extra.ascii_fold("Aragón") == "Aragon"
}

pub fn ascii_fold_scandinavian_test() {
  assert extra.ascii_fold("Åse") == "Ase"
  assert extra.ascii_fold("Øystein") == "Oystein"
}

pub fn slugify_long_text_test() {
  let s = "This is a very long title that should be truncated"
  let slug = extra.slugify_opts(s, 5, "-", False)
  let parts = string.split(slug, "-")
  assert list.length(parts) == 5
}

pub fn to_kebab_case_multiword_test() {
  assert extra.to_kebab_case("get User By Id") == "get-user-by-id"
}

pub fn to_snake_case_caps_test() {
  assert extra.to_snake_case("getUserById") == "getuserbyid"
}

pub fn ascii_fold_no_decompose_precomposed_test() {
  let result = extra.ascii_fold_no_decompose("café")
  assert result == "cafe"
}

pub fn slugify_whitespace_normalization_test() {
  let s = "Hello\t\n\rWorld"
  assert extra.slugify(s) == "hello-world"
}

pub fn slugify_punctuation_removal_test() {
  let s = "Hello!@#$%World"
  assert extra.slugify(s) == "hello-world"
}

pub fn to_pascal_case_basic_test() {
  assert extra.to_pascal_case("hello world") == "HelloWorld"
}

pub fn to_pascal_case_multi_word_test() {
  assert extra.to_pascal_case("get user by id") == "GetUserById"
}

pub fn to_pascal_case_with_accents_test() {
  assert extra.to_pascal_case("café brûlée") == "CafeBrulee"
}

pub fn to_pascal_case_with_separators_test() {
  assert extra.to_pascal_case("hello-world_test") == "HelloWorldTest"
}

pub fn to_title_case_basic_test() {
  assert extra.to_title_case("hello world") == "Hello World"
}

pub fn to_title_case_multi_word_test() {
  assert extra.to_title_case("get user by id") == "Get User By Id"
}

pub fn to_title_case_with_accents_test() {
  assert extra.to_title_case("café brûlée") == "Cafe Brulee"
}

pub fn to_title_case_with_separators_test() {
  assert extra.to_title_case("hello-world_test") == "Hello World Test"
}
