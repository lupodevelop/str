# Migration Guide: slugify_opts → SlugifyOptions builder

This guide explains how to migrate from the old positional `slugify_opts`/`slugify_opts_with_normalizer` API to the new `SlugifyOptions` builder and `slugify_with_options` / `slugify_with_options_and_normalizer` functions.

Why migrate
- Positional arguments were fragile (magic numbers, ordering) and hard to extend.
- The builder pattern is self-documenting, extensible, and easier to test.

Examples

Old (positional):

```gleam
str.slugify_opts("Hello World", 2, "-", False)
```

New (builder):

```gleam
let opts = str.slugify_options()
  |> str.with_max_tokens(2)
  |> str.with_separator("-")
  |> str.with_preserve_unicode(False)
str.slugify_with_options("Hello World", opts)
```

With custom normalizer (old):

```gleam
str.slugify_opts_with_normalizer(title, 0, "-", False, my_nfd)
```

New:

```gleam
let opts = str.slugify_options()
  |> str.with_max_tokens(0)
  |> str.with_separator("-")
  |> str.with_preserve_unicode(False)
str.slugify_with_options_and_normalizer(title, opts, my_nfd)
```

Notes
- `with_max_tokens(-1)` or `with_max_tokens(0)` follows previous tests semantics: `-1` is unlimited; you can use 0 to match some existing usage (tests previously used 0 as an explicit token limit). The builder keeps the numeric value and the implementation respects `max_tokens >= 0`.
- `slugify` and `slugify_with_normalizer` convenience functions delegate to the new builder with default options; you can use them as before for common cases.

Compatibility policy
- The old functions have been removed in the current branch (breaking change). If you need to support both versions temporarily, update your codebase to use the builder; we recommend updating CI checks to surface failures early.

If you find migration friction or discover an unhandled case, open an issue with examples and we'll adjust the builder or provide a compatibility helper where appropriate.
