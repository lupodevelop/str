/// Unit tests for extra string transformation functions.
///
/// This file tests advanced text processing features:
/// - ASCII folding (ascii_fold, ascii_fold_no_decompose, ascii_fold_with_normalizer)
/// - Slugification (slugify, slugify_with_options, slugify_with_options_and_normalizer)
/// - Case conversion (to_snake_case, to_camel_case, to_kebab_case, to_pascal_case, to_title_case)
/// - Internal helpers (remove_combining_marks, decompose_latin)
///
/// Key test areas:
/// - Diacritic removal (café → cafe, Müller → Muller)
/// - Ligature expansion (æ → ae, ß → ss)
/// - Emoji handling (preserved or removed based on options)
/// - Custom normalizer integration
/// - Options builder pattern (slugify_options)
///
/// Multilingual coverage:
/// - German (ö, ü, ß)
/// - French (é, è, œ)
/// - Spanish (ñ, á)
/// - Scandinavian (å, ø, æ)
import gleam/list
import gleam/string
import str
import str/internal/decompose
import str/internal/translit

pub fn ascii_fold_internal_helpers_test() {
  // remove combining marks should strip acute accent
  let with_comb = "e\u{0301}"
  let removed = translit.remove_combining_marks(with_comb)
  assert removed == "e"
}


// decompose Latin should expand common precomposed characters
pub fn ascii_fold_decompose_test() {
  let dec = decompose.decompose_latin("é")
  // decomposer may produce e + combining acute; ensure combining exists
  assert string.contains(dec, "\u{0301}")
}

pub fn ascii_fold_ligature_test() {
  // ligature æ should transliterate to ae
  assert str.ascii_fold("æ") == "ae" || str.ascii_fold("Æ") == "AE"
}

pub fn slugify_normalizer_behavior_test() {
  // If preserve_unicode is True the normalizer must NOT be applied.
  let s = "Café"
  let noisy = fn(_) { "X" }
  let opts = str.slugify_options() |> str.with_preserve_unicode(True)
  let res = str.slugify_with_options_and_normalizer(s, opts, noisy)
  assert res == "café"
}

pub fn slugify_with_normalizer_token_limit_test() {
  let s = "uno due tre quattro"
  let fake = fn(x) { x }
  // identity normalizer
  let opts = str.slugify_options() |> str.with_max_tokens(2) |> str.with_separator("-") |> str.with_preserve_unicode(False)
  let slug = str.slugify_with_options_and_normalizer(s, opts, fake)
  assert slug == "uno-due"
}

pub fn camel_and_snake_tests() {
  let s = "  multiple   separators__and--caps "
  let camel = str.to_camel_case(s)
  assert string.length(camel) > 0
  assert string.slice(camel, 0, 1) != "-"
  let snake = str.to_snake_case("Hello World")
  assert snake == "hello_world"
}

pub fn slugify_emoji_and_numbers_test() {
  let s = "I ❤️ Gleam 2025"
  assert str.slugify(s) == "i-gleam-2025"
}

pub fn ascii_fold_no_decompose_identity_test() {
  // ascii_fold_no_decompose with identity normalizer should preserve precomposed handling
  let s = "Ångström"
  let res = str.ascii_fold_no_decompose_with_normalizer(s, fn(x) { x })
  assert string.contains(res, "A") || string.contains(res, "Ang")
}

pub fn ascii_fold_with_normalizer_effect_test() {
  // fake normalizer that turns ö into o + diaeresis combining
  let fake = fn(x) { string.replace(x, "ö", "o\u{0308}") }
  let res = str.ascii_fold_with_normalizer("schröder", fake)
  assert string.contains(res, "o")
}

pub fn to_kebab_equals_slugify_test() {
  assert str.to_kebab_case("Hello World") == str.slugify("Hello World")
}

pub fn ascii_fold_basic_test() {
  assert str.ascii_fold("ÀÁÂÃÄÅ") == "AAAAAA"
  assert str.ascii_fold("àáâãäå") == "aaaaaa"
}

pub fn ascii_fold_specials_test() {
  assert str.ascii_fold("Çç") == "Cc"
  assert str.ascii_fold("Ææß") == "AEaess"
}

pub fn slugify_basic_test() {
  let s = "Hello, World!"
  assert str.slugify(s) == "hello-world"
}

pub fn slugify_accent_test() {
  let s = "Café déjà vu"
  assert str.slugify(s) == "cafe-deja-vu"
}

pub fn slugify_emoji_removed_test() {
  let s = "I ❤️ Gleam"
  assert str.slugify(s) == "i-gleam"
}

pub fn slugify_multiple_separators_test() {
  let s = "a--b__c"
  assert str.slugify(s) == "a-b-c"
}

pub fn slugify_numbers_test() {
  let s = "2025 Year!"
  assert str.slugify(s) == "2025-year"
}

pub fn to_snake_case_test() {
  assert str.to_snake_case("Hello World") == "hello_world"
}

pub fn to_camel_case_test() {
  let s = "Hello Fancy World"
  assert str.to_camel_case(s) == "helloFancyWorld"
}

pub fn camel_case_edge_test() {
  let s = "  multiple   separators__and--caps "
  let c = str.to_camel_case(s)
  assert string.length(c) > 0
  assert string.slice(c, 0, 1) != "-"
}

pub fn slugify_trim_test() {
  let s = "---Hello---"
  assert str.slugify(s) == "hello"
}

pub fn slugify_preserves_digits_test() {
  let s = "X1 Y2 Z3"
  assert str.slugify(s) == "x1-y2-z3"
}

pub fn slugify_with_options_max_len_test() {
  let s = "a b c d e"
  let opts = str.slugify_options() |> str.with_max_tokens(3) |> str.with_separator("-") |> str.with_preserve_unicode(False)
  assert str.slugify_with_options(s, opts) == "a-b-c"
}

pub fn slugify_with_options_sep_test() {
  let s = "Hello World"
  let opts = str.slugify_options() |> str.with_max_tokens(-1) |> str.with_separator("_") |> str.with_preserve_unicode(False)
  assert str.slugify_with_options(s, opts) == "hello_world"
}

pub fn slugify_with_options_preserve_unicode_true_test() {
  let s = "Café ❤️ Gleam"
  let opts = str.slugify_options() |> str.with_max_tokens(-1) |> str.with_separator("-") |> str.with_preserve_unicode(True)
  assert str.slugify_with_options(s, opts) == "café-❤️-gleam"
}

pub fn slugify_with_options_preserve_unicode_false_test() {
  let s = "Café ❤️ Gleam"
  let opts = str.slugify_options() |> str.with_max_tokens(-1) |> str.with_separator("-") |> str.with_preserve_unicode(False)
  assert str.slugify_with_options(s, opts) == "cafe-gleam"
}

pub fn ascii_fold_german_test() {
  assert str.ascii_fold("Müller") == "Muller"
  assert str.ascii_fold("Größe") == "Grosse"
}

pub fn ascii_fold_french_test() {
  assert str.ascii_fold("français") == "francais"
  assert str.ascii_fold("œuvre") == "oeuvre"
}

pub fn ascii_fold_spanish_test() {
  assert str.ascii_fold("niño") == "nino"
  assert str.ascii_fold("Aragón") == "Aragon"
}

pub fn ascii_fold_scandinavian_test() {
  assert str.ascii_fold("Åse") == "Ase"
  assert str.ascii_fold("Øystein") == "Oystein"
}

pub fn slugify_long_text_test() {
  let s = "This is a very long title that should be truncated"
  let opts = str.slugify_options() |> str.with_max_tokens(5) |> str.with_separator("-") |> str.with_preserve_unicode(False)
  let slug = str.slugify_with_options(s, opts)
  let parts = string.split(slug, "-")
  assert list.length(parts) == 5
}

pub fn to_kebab_case_multiword_test() {
  assert str.to_kebab_case("get User By Id") == "get-user-by-id"
}

pub fn to_snake_case_caps_test() {
  assert str.to_snake_case("getUserById") == "getuserbyid"
}

pub fn ascii_fold_no_decompose_precomposed_test() {
  let result = str.ascii_fold_no_decompose("café")
  assert result == "cafe"
}

pub fn slugify_whitespace_normalization_test() {
  let s = "Hello\t\n\rWorld"
  assert str.slugify(s) == "hello-world"
}

pub fn slugify_punctuation_removal_test() {
  let s = "Hello!@#$%World"
  assert str.slugify(s) == "hello-world"
}

pub fn to_pascal_case_basic_test() {
  assert str.to_pascal_case("hello world") == "HelloWorld"
}

pub fn to_pascal_case_multi_word_test() {
  assert str.to_pascal_case("get user by id") == "GetUserById"
}

pub fn to_pascal_case_with_accents_test() {
  assert str.to_pascal_case("café brûlée") == "CafeBrulee"
}

pub fn to_pascal_case_with_separators_test() {
  assert str.to_pascal_case("hello-world_test") == "HelloWorldTest"
}

pub fn to_title_case_basic_test() {
  assert str.to_title_case("hello world") == "Hello World"
}

pub fn to_title_case_multi_word_test() {
  assert str.to_title_case("get user by id") == "Get User By Id"
}

pub fn to_title_case_with_accents_test() {
  assert str.to_title_case("café brûlée") == "Cafe Brulee"
}

pub fn to_title_case_with_separators_test() {
  assert str.to_title_case("hello-world_test") == "Hello World Test"
}
