import gleam/string
import str

pub fn ascii_fold_with_normalizer_identity_test() {
  // Passing the identity function should produce the same result as the
  // existing `ascii_fold` path.
  let s = "Crème"
  let res1 = str.ascii_fold_with_normalizer(s, fn(x) { x })
  let res2 = str.ascii_fold(s)
  assert res1 == res2
}

pub fn slugify_with_options_and_normalizer_fake_nfd_test() {
  // Simulate an NFD normalizer for the specific character `é` by
  // replacing it with `e` + combining acute (U+0301). The library
  // will then remove combining marks and produce the expected slug.
  let fake_nfd = fn(x) { string.replace(x, "é", "e\u{0301}") }

  let s = "Café ❤️"
  let opts = str.slugify_options() |> str.with_max_tokens(0) |> str.with_separator("-") |> str.with_preserve_unicode(False)
  let slug = str.slugify_with_options_and_normalizer(s, opts, fake_nfd)
  assert slug == "cafe"
}
