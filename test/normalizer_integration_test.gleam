import gleam/string
import str/extra

pub fn ascii_fold_with_normalizer_identity_test() {
  // Passing the identity function should produce the same result as the
  // existing `ascii_fold` path.
  let s = "Crème"
  let res1 = extra.ascii_fold_with_normalizer(s, fn(x) { x })
  let res2 = extra.ascii_fold(s)
  assert res1 == res2
}

pub fn slugify_opts_with_normalizer_fake_nfd_test() {
  // Simulate an NFD normalizer for the specific character `é` by
  // replacing it with `e` + combining acute (U+0301). The library
  // will then remove combining marks and produce the expected slug.
  let fake_nfd = fn(x) { string.replace(x, "é", "e\u{0301}") }

  let s = "Café ❤️"
  let slug = extra.slugify_opts_with_normalizer(s, 0, "-", False, fake_nfd)
  assert slug == "cafe"
}
