import gleam/list
import gleam/string
import str

pub fn words_to_slug_chain_test() {
  let s = "  Café — 2025  "
  let folded = str.ascii_fold(s)
  let words = str.words(folded)
  let opts = str.slugify_options() |> str.with_max_tokens(-1) |> str.with_separator("-") |> str.with_preserve_unicode(False)
  let slug = str.slugify_with_options_and_normalizer(folded, opts, fn(x) { x })
  assert list.length(words) >= 2
    && string.contains(slug, "cafe")
    && string.contains(slug, "2025")
}

pub fn truncate_then_slugify_chain_test() {
  let s = "Hello, World! This is a longer sentence."
  let t = str.truncate_preserve(s, 12, "…")
  let slug = str.slugify(t)
  assert string.length(slug) > 0
}

pub fn ascii_fold_then_camel_then_slug_chain_test() {
  let s = "  schröder & co "
  let folded = str.ascii_fold(s)
  let camel = str.to_camel_case(folded)
  let slug = str.slugify(camel)
  assert string.contains(slug, "schroder")
    || string.contains(slug, "schroder-co")
}

pub fn pad_truncate_chain_test() {
  let s = "hi"
  let padded = str.pad_left(s, 10, "*")
  let truncated = str.truncate_default(padded, 5)
  assert string.length(truncated) <= 8
}

pub fn reverse_pad_reverse_chain_test() {
  let s = "test"
  let reversed = str.reverse(s)
  let padded = str.pad_right(reversed, 8, " ")
  let final = str.reverse(padded)
  assert string.contains(final, "test")
}

pub fn words_count_chain_test() {
  let s = "hello world hello"
  let words = str.words(s)
  let joined = list.fold(words, "", fn(acc, w) { acc <> w })
  let count = str.count(joined, "hello", False)
  assert count == 2
}

pub fn slugify_truncate_chain_test() {
  let s = "Very Long Title With Many Words Here"
  let opts = str.slugify_options() |> str.with_max_tokens(4) |> str.with_separator("-") |> str.with_preserve_unicode(False)
  let slug = str.slugify_with_options(s, opts)
  let truncated = str.truncate_default(slug, 15)
  assert string.length(truncated) > 0
}

pub fn ascii_fold_surround_unwrap_chain_test() {
  let s = "Café"
  let folded = str.ascii_fold(s)
  let surrounded = str.surround(folded, "[", "]")
  let unwrapped = str.unwrap(surrounded, "[", "]")
  assert unwrapped == "Cafe"
}

pub fn naming_conventions_chain_test() {
  let s = "get user by id"
  let camel = str.to_camel_case(s)
  let snake = str.to_snake_case(camel)
  let kebab = str.to_kebab_case(snake)
  assert string.length(kebab) > 0
}

pub fn center_reverse_center_chain_test() {
  let s = "test"
  let centered = str.center(s, 10, " ")
  let reversed = str.reverse(centered)
  let recentered = str.center(reversed, 12, "-")
  assert string.length(recentered) >= 12
}

pub fn words_slugify_compare_chain_test() {
  let s = "Hello Beautiful World"
  let words = str.words(s)
  let slug = str.slugify(s)
  assert list.length(words) == 3
  assert string.contains(slug, "beautiful")
}

pub fn multiple_ascii_fold_chain_test() {
  let s1 = "Crème"
  let s2 = "Brûlée"
  let f1 = str.ascii_fold(s1)
  let f2 = str.ascii_fold(s2)
  let combined = f1 <> " " <> f2
  let slug = str.slugify(combined)
  assert slug == "creme-brulee"
}
