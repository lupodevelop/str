//// ASCII transliteration and slug generation utilities.
////
//// This module provides pragmatic tools for converting Unicode text to ASCII
//// and generating URL-friendly slugs. Uses curated replacement tables
//// optimized for Latin-based scripts and common ligatures.
//// OTP-free core with optional normalizer injection for production use.

import gleam/list
import gleam/string
import str/internal/decompose
import str/internal/translit

/// Core ASCII folding implementation with optional decomposition.
/// Applies replacement tables, optionally decomposes Latin chars and removes
/// combining marks. Internal use only.
///
/// Pipeline when decompose=True:
/// 1. Apply replacement table (handles precomposed chars like é → e, ß → ss)
/// 2. Decompose remaining Latin chars (é → e + combining accent)
/// 3. Remove combining marks (strips accents, leaving base chars)
/// 4. Apply replacement table again (catch chars that didn't match in step 1
///    due to attached combining marks, e.g., "Å\u{0323}" doesn't match "Å"
///    but after removing the dot below, "Å" matches)
///
fn ascii_fold_full(s: String, decompose: Bool) -> String {
  // Use grapheme-wise transliteration (faster, single-pass) when available
  let replaced = translit.transliterate_pure(s)

  // Optionally decompose (expand precomposed letters) and then remove
  // combining marks. Decomposition is limited to Latin ranges and is
  // performed in pure Gleam by `internal_decompose`.
  case decompose {
    True -> {
      let after_decompose =
        replaced
        |> decompose.decompose_latin
        |> translit.remove_combining_marks

      // Second pass: catch precomposed chars that didn't match initially
      // because they had combining marks attached (handled by transliterate)
      translit.transliterate_pure(after_decompose)
    }
    False -> replaced
  }
}

/// Converts Unicode text to ASCII equivalents.
/// Applies replacement tables, decomposes Latin chars and removes combining marks.
///
///   ascii_fold("café") -> "cafe"
///   ascii_fold("straße") -> "strasse"
///   ascii_fold("Crème Brûlée") -> "Creme Brulee"
///
pub fn ascii_fold(s: String) -> String {
  ascii_fold_full(s, True)
}

/// ASCII folding without decomposition step.
/// Applies only the replacement table, preserving combining marks.
///
///   ascii_fold_no_decompose("café") -> "cafe"
///
pub fn ascii_fold_no_decompose(s: String) -> String {
  ascii_fold_full(s, False)
}

/// ASCII folding with custom normalizer for production Unicode handling.
/// The normalizer (typically OTP's :unicode module) is applied between
/// decomposition and combining mark removal.
///
/// Pipeline:
/// 1. Apply replacement table (precomposed chars)
/// 2. Decompose Latin chars
/// 3. Apply custom normalizer (e.g., NFC/NFD from OTP)
/// 4. Remove combining marks
/// 5. Apply replacement table again (catch chars with attached combining marks)
///
///   ascii_fold_with_normalizer("café", my_nfd) -> "cafe"
///
pub fn ascii_fold_with_normalizer(s: String, normalizer) -> String {
  let reps = translit.replacements()

  let replaced =
    list.fold(reps, s, fn(acc, pair) {
      let #(from, to) = pair
      string.replace(acc, from, to)
    })

  let after_normalize =
    replaced
    |> decompose.decompose_latin
    |> normalizer
    |> translit.remove_combining_marks

  // Second pass for chars that didn't match due to attached combining marks
  list.fold(reps, after_normalize, fn(acc, pair) {
    let #(from, to) = pair
    string.replace(acc, from, to)
  })
}

/// ASCII folding without decomposition, with custom normalizer.
/// The replacement table is applied twice: before and after normalization.
/// This handles cases where the normalizer (e.g., NFC) may compose characters
/// into precomposed forms that the table can then convert.
///
/// Pipeline:
/// 1. Apply replacement table (precomposed chars)
/// 2. Apply custom normalizer (may produce new precomposed chars)
/// 3. Apply replacement table again (catch newly composed chars)
///
pub fn ascii_fold_no_decompose_with_normalizer(s: String, normalizer) -> String {
  let reps = translit.replacements()

  // First pass: handle precomposed characters in the input
  let replaced =
    list.fold(reps, s, fn(acc, pair) {
      let #(from, to) = pair
      string.replace(acc, from, to)
    })

  // Apply the user-provided normalizer (e.g., NFC, NFD)
  let normalized = normalizer(replaced)

  // Second pass: handle any precomposed characters produced by normalizer
  list.fold(reps, normalized, fn(acc, pair) {
    let #(from, to) = pair
    string.replace(acc, from, to)
  })
}

// ---------------------------------------------------------------------------
// Slugify Options Builder 
// ---------------------------------------------------------------------------

pub opaque type SlugifyOptions {
  SlugifyOptions(
    max_tokens: Int,
    separator: String,
    preserve_unicode: Bool,
    lowercase: Bool,
    custom_replacements: List(#(String, String)),
  )
}

pub fn slugify_options() -> SlugifyOptions {
  SlugifyOptions(
    -1,
    "-",
    False,
    True,
    [],
  )
}

pub fn with_max_tokens(opts: SlugifyOptions, n: Int) -> SlugifyOptions {
  SlugifyOptions(..opts, max_tokens: n)
}

pub fn with_separator(opts: SlugifyOptions, sep: String) -> SlugifyOptions {
  SlugifyOptions(..opts, separator: sep)
}

pub fn with_preserve_unicode(opts: SlugifyOptions, v: Bool) -> SlugifyOptions {
  SlugifyOptions(..opts, preserve_unicode: v)
}

pub fn with_custom_replacements(
  opts: SlugifyOptions,
  replacements: List(#(String, String)),
) -> SlugifyOptions {
  SlugifyOptions(..opts, custom_replacements: replacements)
}

pub fn slugify_with_options(s: String, opts: SlugifyOptions) -> String {
  let max_len = case opts.max_tokens >= 0 {
    True -> opts.max_tokens
    False -> -1
  }
  // For now ignore custom_replacements and lowercase flag in opts for simplicity
  slugify_opts(s, max_len, opts.separator, opts.preserve_unicode)
}

pub fn slugify_with_options_and_normalizer(
  s: String,
  opts: SlugifyOptions,
  normalizer,
) -> String {
  let max_len = case opts.max_tokens >= 0 {
    True -> opts.max_tokens
    False -> -1
  }
  slugify_opts_with_normalizer(s, max_len, opts.separator, opts.preserve_unicode, normalizer)
}

// Simple helpers used by slugify
fn trim_leading_sep(gs: List(String), sep: String) -> List(String) {
  case gs {
    [] -> []
    [first, ..rest] ->
      case first == sep {
        True -> trim_leading_sep(rest, sep)
        False -> gs
      }
  }
}

fn trim_surrounding_sep(gs: List(String), sep: String) -> List(String) {
  list.reverse(trim_leading_sep(list.reverse(trim_leading_sep(gs, sep)), sep))
}

fn is_ascii_digit(code: Int) -> Bool {
  code >= 0x30 && code <= 0x39
}

fn is_ascii_lower(code: Int) -> Bool {
  code >= 0x61 && code <= 0x7A
}

fn is_alnum_grapheme(g: String) -> Bool {
  case string.to_utf_codepoints(g) {
    [cp] ->
      case string.utf_codepoint_to_int(cp) {
        n -> is_ascii_digit(n) || is_ascii_lower(n)
      }
    _ -> False
  }
}

/// Creates a URL-friendly slug from text.
/// Uses default settings: hyphen separator, no token limit, ASCII output.
///
///   slugify("Hello, World!") -> "hello-world"
///   slugify("Café & Bar") -> "cafe-bar"
///
pub fn slugify(s: String) -> String {
  slugify_opts(s, -1, "-", False)
}

/// Creates a slug using a custom normalizer function.
///
///   slugify_with_normalizer("Café", my_nfd) -> "cafe"
///
pub fn slugify_with_normalizer(s: String, normalizer) -> String {
  slugify_opts_with_normalizer(s, -1, "-", False, normalizer)
}

/// Converts text to kebab-case (lowercase with hyphens).
///
///   to_kebab_case("Hello World") -> "hello-world"
///
pub fn to_kebab_case(s: String) -> String {
  slugify(s)
}

/// Creates a slug with configurable options.
/// max_len limits tokens (-1 for unlimited), sep is the separator,
/// preserve_unicode keeps Unicode chars if True.
///
///   slugify_opts("one two three", 2, "-", False) -> "one-two"
///   slugify_opts("Hello World", -1, "_", False) -> "hello_world"
///
pub fn slugify_opts(
  s: String,
  max_len: Int,
  sep: String,
  preserve_unicode: Bool,
) -> String {
  let base = case preserve_unicode {
    True -> string.lowercase(s)
    False -> ascii_fold(s) |> string.lowercase
  }
  let clusters = string.to_graphemes(base)

  let raw =
    list.fold(clusters, "", fn(acc, g) {
      case preserve_unicode {
        True ->
          case string.trim(g) == "" {
            True ->
              case acc == "" || string.ends_with(acc, sep) {
                True -> acc
                False -> acc <> sep
              }
            False -> acc <> g
          }
        False ->
          case is_alnum_grapheme(g) {
            True -> acc <> g
            False ->
              case acc == "" || string.ends_with(acc, sep) {
                True -> acc
                False -> acc <> sep
              }
          }
      }
    })

  let trimmed = trim_surrounding_sep(string.to_graphemes(raw), sep)

  let joined = list.fold(trimmed, "", fn(acc, c) { acc <> c })

  let tokens = string.split(joined, sep)
  let taken = case max_len > 0 {
    True -> list.take(tokens, max_len)
    False -> tokens
  }
  let result =
    list.fold(taken, "", fn(acc, t) {
      case acc == "" {
        True -> acc <> t
        False -> acc <> sep <> t
      }
    })

  result
}

/// Creates a slug with full options and custom normalizer.
///
///   slugify_opts_with_normalizer("Crème Brûlée", 2, "-", False, my_nfd) -> "creme-brulee"
///
pub fn slugify_opts_with_normalizer(
  s: String,
  max_len: Int,
  sep: String,
  preserve_unicode: Bool,
  normalizer,
) -> String {
  let base = case preserve_unicode {
    True -> string.lowercase(s)
    False -> ascii_fold_with_normalizer(s, normalizer) |> string.lowercase
  }

  let clusters = string.to_graphemes(base)

  let raw =
    list.fold(clusters, "", fn(acc, g) {
      case preserve_unicode {
        True ->
          case string.trim(g) == "" {
            True ->
              case acc == "" || string.ends_with(acc, sep) {
                True -> acc
                False -> acc <> sep
              }
            False -> acc <> g
          }
        False ->
          case is_alnum_grapheme(g) {
            True -> acc <> g
            False ->
              case acc == "" || string.ends_with(acc, sep) {
                True -> acc
                False -> acc <> sep
              }
          }
      }
    })

  let trimmed = trim_surrounding_sep(string.to_graphemes(raw), sep)
  let joined = list.fold(trimmed, "", fn(acc, c) { acc <> c })

  let tokens = string.split(joined, sep)
  let taken = case max_len > 0 {
    True -> list.take(tokens, max_len)
    False -> tokens
  }
  let result =
    list.fold(taken, "", fn(acc, t) {
      case acc == "" {
        True -> acc <> t
        False -> acc <> sep <> t
      }
    })

  result
}

/// Converts text to snake_case (lowercase with underscores).
///
///   to_snake_case("Hello World") -> "hello_world"
///
pub fn to_snake_case(s: String) -> String {
  slugify(s) |> string.replace("-", "_")
}

/// Converts text to camelCase.
/// First word is lowercase, subsequent words are capitalized.
///
///   to_camel_case("hello world") -> "helloWorld"
///   to_camel_case("get user by id") -> "getUserById"
///
pub fn to_camel_case(s: String) -> String {
  let parts = string.split(slugify(s), "-")
  list.fold(parts, "", fn(acc, part) {
    case string.is_empty(part) {
      True -> acc
      False ->
        case acc == "" {
          True -> acc <> part
          False ->
            acc
            <> string.uppercase(string.slice(part, 0, 1))
            <> string.slice(part, 1, string.length(part) - 1)
        }
    }
  })
}

/// Converts text to PascalCase.
/// All words are capitalized and joined without separator.
///
///   to_pascal_case("hello world") -> "HelloWorld"
///   to_pascal_case("get user by id") -> "GetUserById"
///
pub fn to_pascal_case(s: String) -> String {
  let parts = string.split(slugify(s), "-")
  list.fold(parts, "", fn(acc, part) {
    case string.is_empty(part) {
      True -> acc
      False ->
        acc
        <> string.uppercase(string.slice(part, 0, 1))
        <> string.slice(part, 1, string.length(part) - 1)
    }
  })
}

/// Converts text to Title Case.
/// Each word is capitalized with spaces between them.
///
///   to_title_case("hello world") -> "Hello World"
///   to_title_case("get user by id") -> "Get User By Id"
///   to_title_case("café brûlée") -> "Cafe Brulee"
///
pub fn to_title_case(s: String) -> String {
  let parts = string.split(slugify(s), "-")
  let capitalized =
    list.map(parts, fn(part) {
      case string.is_empty(part) {
        True -> part
        False ->
          string.uppercase(string.slice(part, 0, 1))
          <> string.slice(part, 1, string.length(part) - 1)
      }
    })
  string.join(capitalized, " ")
}
// Note: normalizer helpers (NFC/NFD/NFKC/NFKD) are intentionally not
// exported by the `str` library to avoid introducing an OTP dependency.
// If you need to use OTP normalization, define a small helper in your
// application (see `EXAMPLES.md` and `examples/with_otp.gleam`).
