# OTP Integration Guide for `str`

## Overview

This guide explains how to integrate OTP-based Unicode normalization with the `str` library while maintaining the library's zero-dependency design.

## Core Principle

**Keep OTP integration in your application code, not in the library.**

This approach ensures:
- The `str` library remains dependency-free
- Applications can opt into OTP features as needed
- The library works in any Gleam runtime environment

## Why Separate OTP Integration?

Unicode normalization (NFC, NFD, NFKC, NFKD) requires substantial Unicode data and algorithms. Erlang/OTP provides robust built-in support via the `:unicode` module, but including it as a hard dependency would:

- Force all users to accept the OTP dependency
- Limit portability to JavaScript or other non-BEAM targets
- Increase compilation and deployment overhead for simple use cases

By accepting normalizer functions as parameters, `str` allows applications to choose their normalization strategy.

## Implementation Guide

### Step 1: Create Normalizer Helpers

Create a module in your application (e.g., `src/unicode_helpers.gleam`):

```gleam
// src/unicode_helpers.gleam
import gleam/dynamic

/// Normalize to NFD (canonical decomposition)
pub fn nfd(text: String) -> String {
  // Use Erlang FFI to call :unicode.characters_to_nfd_binary/1
  do_normalize(text, "nfd")
}

/// Normalize to NFC (canonical composition)
pub fn nfc(text: String) -> String {
  do_normalize(text, "nfc")
}

@external(erlang, "unicode", "characters_to_nfd_binary")
fn do_normalize(text: String, mode: String) -> String
```

### Step 2: Use with `str` Functions

Pass your normalizer to the `*_with_normalizer` variants:

```gleam
import str/extra
import unicode_helpers

pub fn process_title(title: String) -> String {
  // Use NFC normalization before ASCII folding
  extra.ascii_fold_with_normalizer(title, unicode_helpers.nfc)
}

pub fn create_slug(text: String) -> String {
  // Use NFD for decomposition-based transliteration
  extra.slugify_with_normalizer(text, unicode_helpers.nfd)
}
```

### Step 3: Configure Slugify Options

For more control, use `slugify_opts_with_normalizer`:

```gleam
pub fn create_url_slug(text: String, max_words: Int) -> String {
  extra.slugify_opts_with_normalizer(
    text,
    max_words,  // Token limit
    "-",        // Separator
    False,      // Convert to ASCII
    unicode_helpers.nfd
  )
}
```

## Testing Without OTP

For testing or development without OTP, create mock normalizers:

```gleam
// test/helpers.gleam
pub fn mock_nfd(text: String) -> String {
  text
  |> string.replace("é", "e\u{0301}")
  |> string.replace("ñ", "n\u{0303}")
  // Add more decompositions as needed
}
```

## Example: Complete Integration

```gleam
// src/slug.gleam
import str/extra
import unicode_helpers

/// Create a URL-friendly slug from arbitrary text
pub fn from_text(text: String) -> String {
  extra.slugify_opts_with_normalizer(
    text,
    0,      // No word limit
    "-",    // Hyphen separator
    False,  // ASCII output only
    unicode_helpers.nfd
  )
}

/// Create a slug preserving Unicode characters
pub fn from_text_unicode(text: String) -> String {
  extra.slugify_opts_with_normalizer(
    text,
    0,
    "-",
    True,   // Preserve Unicode
    unicode_helpers.nfc
  )
}

// Usage:
// slug.from_text("Café Münchner") -> "cafe-munchner"
// slug.from_text_unicode("Café Münchner") -> "café-münchner"
```

## API Reference

### Functions Accepting Normalizers

- `ascii_fold_with_normalizer(text, normalizer)`
- `ascii_fold_no_decompose_with_normalizer(text, normalizer)`
- `slugify_with_normalizer(text, normalizer)` — Convenience alias
- `slugify_opts_with_normalizer(text, max_len, sep, preserve_unicode, normalizer)`

### Normalizer Signature

```gleam
pub type Normalizer = fn(String) -> String
```

All normalizers must accept a string and return a normalized string.

## Best Practices

1. **Centralize Normalizers**: Keep all OTP normalization logic in one module for easy testing and maintenance

2. **Choose Normalization Form**:
   - Use **NFD** (decomposition) for transliteration and ASCII folding
   - Use **NFC** (composition) for display and storage
   - Consider **NFKD/NFKC** for compatibility normalization

3. **Test with Real Data**: Include test cases with actual Unicode edge cases (emoji, combining marks, ligatures)

4. **Document Choices**: Explain which normalization form you're using and why

## Further Reading

- [Unicode Normalization Forms](https://unicode.org/reports/tr15/)
- [Erlang :unicode Module](https://erlang.org/doc/man/unicode.html)
- [str Library Examples](../EXAMPLES.md)
