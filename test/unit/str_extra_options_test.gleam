import str

pub fn slugify_options_defaults_and_setters_test() {
  let opts = str.slugify_options()
    |> str.with_max_tokens(2)
    |> str.with_separator("_")
    |> str.with_preserve_unicode(True)

  assert str.slugify_with_options("One Two Three", opts) == "one_two"
}

pub fn slugify_with_options_and_normalizer_test() {
  let opts = str.slugify_options() |> str.with_preserve_unicode(False)
  let normalizer = fn(s) { str.decompose_latin(s) }
  let out = str.slugify_with_options_and_normalizer("Crème Brûlée", opts, normalizer)
  assert out == "creme-brulee"
}