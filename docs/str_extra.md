# str/extra — ASCII Transliteration and Slug Generation

## Overview

The `str/extra` module provides practical utilities for:

- Converting Unicode text to ASCII equivalents
- Generating URL-friendly slugs
- Common naming convention transformations (camelCase, snake_case, kebab-case)

## Design Philosophy

**Pragmatic over Perfect**: This module prioritizes practical, deterministic results for common use cases.

- **Scope**: Optimized for Latin-based scripts, common ligatures, and frequently-used symbols
- **Approach**: Uses curated replacement tables rather than algorithmic transformation
- **Extensibility**: For comprehensive transliteration (Cyrillic, Arabic, CJK), integrate an external library

## ASCII Transliteration

### `ascii_fold(text: String) -> String`

Converts Unicode text to ASCII by applying character replacements and removing combining marks.

**Process**:

1. Apply replacement table (é → e, ß → ss, æ → ae, etc.)
2. Decompose common Latin characters
3. Remove combining marks (diacritics)
4. Reapply replacements for any decomposed forms

**Examples**:

```gleam
ascii_fold("café")           // "cafe"
ascii_fold("Münchner")       // "Munchner"
ascii_fold("naïve")          // "naive"
ascii_fold("Crème Brûlée")   // "Creme Brulee"
```

**Ligatures**:

```gleam
ascii_fold("æon")   // "aeon"
ascii_fold("Æsir")  // "AEsir"
ascii_fold("straße") // "strasse"
```

### `ascii_fold_no_decompose(text: String) -> String`

Applies only the replacement table without decomposition.

**Use case**: When you want to preserve combining marks for further processing.

**Example**:

```gleam
ascii_fold_no_decompose("café")  // "cafe"
// But decomposed input like "cafe\u{0301}" is preserved as-is
```

### With Normalizer (OTP Integration)

#### `ascii_fold_with_normalizer(text: String, normalizer: fn(String) -> String) -> String`

Accepts a custom normalizer function for production Unicode handling.

**Example**:

```gleam
import unicode_helpers  // Your OTP wrapper module

pub fn fold_production(text: String) -> String {
  ascii_fold_with_normalizer(text, unicode_helpers.nfd)
}
```

#### `ascii_fold_no_decompose_with_normalizer(text: String, normalizer: fn(String) -> String) -> String`

Variant without internal decomposition, relying solely on the provided normalizer.

## Slug Generation

### `slugify(text: String) -> String`

Creates a URL-friendly slug with default settings:

- Separator: `-`
- No token limit
- ASCII output only

**Examples**:

```gleam
slugify("Hello, World!")        // "hello-world"
slugify("Café & Bar")           // "cafe-bar"
slugify("2025 — New Year!")     // "2025-new-year"
```

### `slugify_with_options(text: String, opts: SlugifyOptions) -> String`

Use the `SlugifyOptions` builder to construct options; see `docs/MIGRATION.md` for migration examples.

Configurable slug generation.

**Parameters**:

- `max_len`: Maximum number of tokens (0 = unlimited)
- `sep`: Token separator (typically `-` or `_`)
- `preserve_unicode`: If `True`, keeps Unicode characters; if `False`, applies ASCII folding

**Process**:

1. Trim and normalize whitespace
2. Apply ASCII folding (if `preserve_unicode` is `False`)
3. Convert to lowercase
4. Replace non-alphanumeric sequences with separator
5. Collapse consecutive separators
6. Trim separators from ends
7. Limit to `max_len` tokens (if specified)

**Examples**:

```gleam
// Token limit
let opts = str.slugify_options() |> str.with_max_tokens(2) |> str.with_separator("-") |> str.with_preserve_unicode(False)
str.slugify_with_options("one two three four", opts)
// "one-two"

// Custom separator
let opts2 = str.slugify_options() |> str.with_max_tokens(0) |> str.with_separator("_") |> str.with_preserve_unicode(False)
str.slugify_with_options("Hello World", opts2)
// "hello_world"

// Preserve Unicode
let opts3 = str.slugify_options() |> str.with_max_tokens(0) |> str.with_separator("-") |> str.with_preserve_unicode(True)
str.slugify_with_options("Café ❤️ Gleam", opts3)
// "café-❤️-gleam"

// ASCII only
let opts4 = str.slugify_options() |> str.with_max_tokens(0) |> str.with_separator("-") |> str.with_preserve_unicode(False)
str.slugify_with_options("Café ❤️ Gleam", opts4)
// "cafe-gleam"
```

### With Normalizer

#### `slugify_with_normalizer(text: String, normalizer: fn(String) -> String) -> String`

Convenience alias for `slugify_with_options_and_normalizer` using defaults.

#### `slugify_with_options_and_normalizer(text: String, opts: SlugifyOptions, normalizer: fn(String) -> String) -> String`

Full control over slugification with custom normalization.

**Example**:

```gleam
import unicode_helpers

pub fn create_slug(title: String, max_words: Int) -> String {
  let opts = str.slugify_options() |> str.with_max_tokens(max_words) |> str.with_separator("-") |> str.with_preserve_unicode(False)
  str.slugify_with_options_and_normalizer(title, opts, unicode_helpers.nfd)
}
```

## Naming Conventions

### `to_kebab_case(text: String) -> String`

Converts text to kebab-case (lowercase with hyphens).

**Example**:

```gleam
to_kebab_case("Hello World")     // "hello-world"
to_kebab_case("getUserById")     // "getuserbyid"
```

### `to_snake_case(text: String) -> String`

Converts text to snake_case (lowercase with underscores).

**Example**:

```gleam
to_snake_case("Hello World")     // "hello_world"
to_snake_case("getUserById")     // "getuserbyid"
```

### `to_camel_case(text: String) -> String`

Converts text to camelCase (lowercase first word, capitalize subsequent words).

**Example**:

```gleam
to_camel_case("hello world")     // "helloWorld"
to_camel_case("get user by id")  // "getUserById"
```

### `to_pascal_case(text: String) -> String`

Converts text to PascalCase (capitalize all words, no separator).

**Example**:

```gleam
to_pascal_case("hello world")     // "HelloWorld"
to_pascal_case("get user by id")  // "GetUserById"
```

### `to_title_case(text: String) -> String`

Converts text to Title Case (capitalize all words, separated by spaces).

**Example**:

```gleam
to_title_case("hello world")     // "Hello World"
to_title_case("get user by id")  // "Get User By Id"
```

## Character Coverage

### Supported Replacements

The internal replacement table covers:

- **Latin accents**: À, Á, Â, Ã, Ä, Å, È, É, Ê, Ë, etc.
- **Ligatures**: æ, Æ, œ, Œ, ß
- **Special Latin**: Ð, Þ, Ø, ð, þ, ø
- **Common symbols**: ©, ®, ™, €, £, ¥

### Decomposition

The internal Latin decomposer handles:

- Common precomposed characters (é, ñ, ü, etc.)
- Base + combining mark sequences (e + ´, n + ˜, etc.)

**Note**: Coverage is optimized for Western European languages. For comprehensive Unicode support, use an external transliteration library.

## Production Usage

### Basic Slug Generation

```gleam
import str

pub fn create_post_slug(title: String) -> String {
  str.slugify(title)
}
```

### With OTP Normalization

```gleam
import str
import unicode_helpers

pub fn create_url_slug(title: String, max_words: Int) -> String {
  let opts =
    str.slugify_options()
    |> str.with_max_tokens(max_words)
    |> str.with_separator("-")
    |> str.with_preserve_unicode(False)

  str.slugify_with_options_and_normalizer(title, opts, unicode_helpers.nfd)
}
```

### File Name Sanitization

```gleam
import str

pub fn sanitize_filename(name: String) -> String {
  let opts =
    str.slugify_options()
    |> str.with_max_tokens(0)
    |> str.with_separator("_")
    |> str.with_preserve_unicode(False)

  name
  |> str.ascii_fold()
  |> str.slugify_with_options(opts)
}
```

### Identifier Generation

```gleam
import str

pub fn to_variable_name(text: String) -> String {
  extra.to_snake_case(text)
}

pub fn to_function_name(text: String) -> String {
  extra.to_camel_case(text)
}
```

## Limitations

1. **Script Coverage**: Optimized for Latin scripts; limited support for Cyrillic, Greek, Arabic, CJK
2. **Semantic Preservation**: Transliteration is lossy; "café" and "cafe" become indistinguishable
3. **Bidirectional Text**: No special handling for RTL scripts
4. **Contextual Rules**: Simple character-by-character replacement without linguistic context

## Extending the Library

To add custom replacements:

1. Edit `src/str/internal_translit.gleam`
2. Add entries to the `replacements()` table
3. Regenerate documentation: `python3 scripts/generate_character_tables.py`
4. Test with real-world examples

## See Also

- [str/core](str_core.md) — Grapheme-aware core utilities
- [OTP Integration Guide](../examples/with_otp.md) — Unicode normalization setup
- [Examples](../EXAMPLES.md) — Integration patterns
- [Character Tables](character_tables.json) — Machine-readable replacement data
