import gleam/list
import gleam/string
import str/core
import str/extra

pub fn words_to_slug_chain_test() {
  let s = "  Café — 2025  "
  let folded = extra.ascii_fold(s)
  let words = core.words(folded)
  let slug =
    extra.slugify_opts_with_normalizer(folded, -1, "-", False, fn(x) { x })
  assert list.length(words) >= 2
    && string.contains(slug, "cafe")
    && string.contains(slug, "2025")
}

pub fn truncate_then_slugify_chain_test() {
  let s = "Hello, World! This is a longer sentence."
  let t = core.truncate_preserve(s, 12, "…")
  let slug = extra.slugify(t)
  assert string.length(slug) > 0
}

pub fn ascii_fold_then_camel_then_slug_chain_test() {
  let s = "  schröder & co "
  let folded = extra.ascii_fold(s)
  let camel = extra.to_camel_case(folded)
  let slug = extra.slugify(camel)
  assert string.contains(slug, "schroder")
    || string.contains(slug, "schroder-co")
}

pub fn pad_truncate_chain_test() {
  let s = "hi"
  let padded = core.pad_left(s, 10, "*")
  let truncated = core.truncate_default(padded, 5)
  assert string.length(truncated) <= 8
}

pub fn reverse_pad_reverse_chain_test() {
  let s = "test"
  let reversed = core.reverse(s)
  let padded = core.pad_right(reversed, 8, " ")
  let final = core.reverse(padded)
  assert string.contains(final, "test")
}

pub fn words_count_chain_test() {
  let s = "hello world hello"
  let words = core.words(s)
  let joined = list.fold(words, "", fn(acc, w) { acc <> w })
  let count = core.count(joined, "hello", False)
  assert count == 2
}

pub fn slugify_truncate_chain_test() {
  let s = "Very Long Title With Many Words Here"
  let slug = extra.slugify_opts(s, 4, "-", False)
  let truncated = core.truncate_default(slug, 15)
  assert string.length(truncated) > 0
}

pub fn ascii_fold_surround_unwrap_chain_test() {
  let s = "Café"
  let folded = extra.ascii_fold(s)
  let surrounded = core.surround(folded, "[", "]")
  let unwrapped = core.unwrap(surrounded, "[", "]")
  assert unwrapped == "Cafe"
}

pub fn naming_conventions_chain_test() {
  let s = "get user by id"
  let camel = extra.to_camel_case(s)
  let snake = extra.to_snake_case(camel)
  let kebab = extra.to_kebab_case(snake)
  assert string.length(kebab) > 0
}

pub fn center_reverse_center_chain_test() {
  let s = "test"
  let centered = core.center(s, 10, " ")
  let reversed = core.reverse(centered)
  let recentered = core.center(reversed, 12, "-")
  assert string.length(recentered) >= 12
}

pub fn words_slugify_compare_chain_test() {
  let s = "Hello Beautiful World"
  let words = core.words(s)
  let slug = extra.slugify(s)
  assert list.length(words) == 3
  assert string.contains(slug, "beautiful")
}

pub fn multiple_ascii_fold_chain_test() {
  let s1 = "Crème"
  let s2 = "Brûlée"
  let f1 = extra.ascii_fold(s1)
  let f2 = extra.ascii_fold(s2)
  let combined = f1 <> " " <> f2
  let slug = extra.slugify(combined)
  assert slug == "creme-brulee"
}
