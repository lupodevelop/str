# Examples — Integration snippets for `str`

This file collects short, copy-pasteable examples showing how to integrate
and extend the `str` library from an application. Keep in mind that the
`str` package itself intentionally does not depend on Erlang/OTP; any OTP
interop should live in the *integrating application* (not in `src/str/*`).

## Core Function Examples

### Grapheme-Aware Indexing and Search

```gleam
import str/core

pub fn search_examples() {
  // Find first occurrence (grapheme-aware!)
  let idx = core.index_of("Hello 👨‍👩‍👧‍👦 World", "World")
  // Ok(8) - the emoji is ONE grapheme cluster!
  
  // Find last occurrence
  let last = core.last_index_of("hello hello hello", "hello")
  // Ok(12)
  
  // Check for multiple needles
  let has_any = core.contains_any("hello world", ["foo", "world"])
  // True
  
  let has_all = core.contains_all("hello world", ["hello", "world"])
  // True
}
```

### Experimental Search Strategies & Caching (1.2.2)

```gleam
import str/core

pub fn search_strategy_examples() {
  // 1) Use the automatic heuristic (experimental)
  // The heuristic chooses between a sliding matcher and KMP based on
  // pattern/text characteristics. It is opt-in and may choose a
  // non-optimal strategy in some cases.
  let auto = core.index_of_auto("some long text...", "pat")

  // 2) Force a specific strategy: use this when performance is critical
  // and you know which algorithm is better for your input shape.
  let forced_kmp = core.index_of_strategy("long text...", "pattern", core.Kmp)
  let forced_sliding = core.index_of_strategy("short text", "pat", core.Sliding)

  // 3) Caching KMP maps: precompute pattern maps once and reuse them
  // across multiple searches to avoid rebuilding prefix tables.
  let pattern = "abababab..."
  let maps = core.build_kmp_maps(pattern)
  let pmap = maps.0
  let pimap = maps.1

  // Reuse maps across many texts
  let idx1 = core.kmp_index_of_with_maps("first long text...", pattern, pmap, pimap)
  let occurrences = core.kmp_search_all_with_maps("another text...", pattern, pmap, pimap)

  // Guidance: prefer explicit strategy or caching in hot loops; use
  // `index_of_auto` for convenience and exploratory testing.
}
```

> Note: `index_of_auto` is experimental and its behavior depends on tunable
> thresholds in `src/str/config.gleam`. For production-critical paths,
> prefer `index_of_strategy` or precomputing maps via `build_kmp_maps`.

### Grapheme-Aware Length and String Checks (NEW in 1.1.0)

```gleam
import str/core

pub fn length_examples() {
  // Grapheme-aware length (NEW in 1.1.0)
  // Unlike standard string length, counts grapheme clusters correctly
  let len = core.length("Hello")
  // 5
  
  // Family emoji is a SINGLE grapheme cluster
  let emoji_len = core.length("👨‍👩‍👧‍👦")
  // 1
  
  // Flag is also a single grapheme
  let flag_len = core.length("🇮🇹")
  // 1
  
  // Combining characters stay attached
  let cafe_len = core.length("café")
  // 4 (even with combining accent)
}

pub fn contains_examples() {
  // Grapheme-aware contains (NEW in 1.1.0)
  let found = core.contains("hello world", "world")
  // True
  
  let not_found = core.contains("hello", "x")
  // False
  
  // Works correctly with emoji
  let emoji_found = core.contains("👨‍👩‍👧‍👦 family", "👨‍👩‍👧‍👦")
  // True
}

pub fn prefix_suffix_examples() {
  // Grapheme-aware starts_with (NEW in 1.1.0)
  let starts = core.starts_with("hello", "he")
  // True
  
  // Empty prefix always matches
  let empty_prefix = core.starts_with("hello", "")
  // True
  
  // Works with emoji on grapheme boundaries
  let emoji_starts = core.starts_with("👨‍👩‍👧‍👦abc", "👨‍👩‍👧‍👦")
  // True
  
  // Grapheme-aware ends_with (NEW in 1.1.0)
  let ends = core.ends_with("hello.txt", ".txt")
  // True
  
  let emoji_ends = core.ends_with("abc👨‍👩‍👧‍👦", "👨‍👩‍👧‍👦")
  // True
}

pub fn empty_check_examples() {
  // is_empty check (NEW in 1.1.0)
  let empty = core.is_empty("")
  // True
  
  let not_empty = core.is_empty(" ")
  // False (whitespace is not empty)
  
  // Combine with is_blank for whitespace check
  let blank = core.is_blank("   ")
  // True
}
```

### Replace First/Last Occurrence

```gleam
import str/core

pub fn replace_examples() {
  // Replace only first occurrence (stdlib only has replace all)
  let text = "hello hello hello"
  let first = core.replace_first(text, "hello", "hi")
  // "hi hello hello"
  
  let last = core.replace_last(text, "hello", "bye")
  // "hello hello bye"
}
```

### HTML Escaping for Web Applications

```gleam
import str/core

pub fn html_examples() {
  // Escape user input before rendering
  let user_input = "<script>alert('xss')</script>"
  let safe = core.escape_html(user_input)
  // "&lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;"
  
  // Unescape for display
  let escaped = "&lt;div&gt;Hello&lt;/div&gt;"
  let original = core.unescape_html(escaped)
  // "<div>Hello</div>"
}
```

### String Validation

```gleam
import str/core

pub fn validation_examples() {
  // Case validation (ignores non-letter characters)
  assert core.is_uppercase("HELLO123") == True
  assert core.is_lowercase("hello_world") == True
  
  // Title Case validation (NEW in 1.0.0)
  assert core.is_title_case("Hello World") == True
  assert core.is_title_case("hello World") == False
  assert core.is_title_case("Hello 123 World") == True  // numbers ignored
  
  // ASCII validation
  assert core.is_ascii("hello!@#") == True
  assert core.is_ascii("café") == False
  
  // Hex validation (useful for color codes, UUIDs, etc.)
  assert core.is_hex("DEADBEEF") == True
  assert core.is_hex("ff00ff") == True
  
  // Printable check (no control characters)
  assert core.is_printable("hello") == True
  assert core.is_printable("hello\n") == False
}
```

### String Similarity and Distance

```gleam
import str/core

pub fn similarity_examples() {
  // Levenshtein distance (edit operations needed)
  let dist = core.distance("kitten", "sitting")
  // 3
  
  // Similarity as percentage (0.0 to 1.0)
  let sim = core.similarity("hello", "hallo")
  // 0.8 (80% similar)
  
  // Hamming distance (same length strings only)
  let ham = core.hamming_distance("karolin", "kathrin")
  // Ok(3)
}
```

### Take/Drop from Right

```gleam
import str/core

pub fn take_drop_examples() {
  // Get last N graphemes
  let last3 = core.take_right("hello world", 3)
  // "rld"
  
  // Drop last N graphemes
  let without_ext = core.drop_right("file.txt", 4)
  // "file"
  
  // Works with emoji too!
  let emoji_end = core.take_right("Hello 👋🏽", 1)
  // "👋🏽" (single grapheme cluster with skin tone)
}
```

### Capitalize and Case Manipulation (NEW in 1.0.0)

```gleam
import str/core

pub fn capitalize_examples() {
  // Capitalize: first letter uppercase, rest lowercase
  let text = core.capitalize("hELLO wORLD")
  // "Hello world"
  
  // Swap case
  let swapped = core.swapcase("Hello World")
  // "hELLO wORLD"
}
```

### Partition and Split (NEW in 1.0.0)

```gleam
import str/core

pub fn partition_examples() {
  // Partition from first occurrence
  let #(before, sep, after) = core.partition("a-b-c", "-")
  // #("a", "-", "b-c")
  
  // Partition from LAST occurrence (rpartition - NEW!)
  // Note: if not found, returns #("", "", text) like Python
  let #(before2, sep2, after2) = core.rpartition("a-b-c", "-")
  // #("a-b", "-", "c")
  
  // Split with max parts limit (splitn - NEW!)
  let parts = core.splitn("one-two-three-four", "-", 2)
  // ["one", "two-three-four"]
  
  let parts3 = core.splitn("a:b:c:d", ":", 3)
  // ["a", "b", "c:d"]
}
```

### Padding and Filling (NEW in 1.0.0)

```gleam
import str/core

pub fn padding_examples() {
  // Standard padding
  let padded = core.pad_left("42", 5, "0")
  // "00042"
  
  // Flexible fill with position type (NEW!)
  let left_fill = core.fill("x", 5, "-", core.Left)
  // "----x"
  
  let right_fill = core.fill("x", 5, "-", core.Right)
  // "x----"
  
  let center_fill = core.fill("x", 5, "-", core.Both)
  // "--x--"
}
```

### Chunking Strings (NEW in 1.0.0)

```gleam
import str/core

pub fn chunk_examples() {
  // Split into fixed-size chunks
  let chunks = core.chunk("abcdefg", 3)
  // ["abc", "def", "g"]
  
  let pairs = core.chunk("abcdef", 2)
  // ["ab", "cd", "ef"]
  
  // Works with emoji (grapheme-aware!)
  let emoji_chunks = core.chunk("👨‍👩‍👧‍👦ab", 2)
  // ["👨‍👩‍👧‍👦a", "b"]
}
```

### Prefix/Suffix Checking (NEW in 1.0.0)

```gleam
import str/core

pub fn prefix_suffix_examples() {
  // Check multiple prefixes at once (starts_with_any - NEW!)
  let is_greeting = core.starts_with_any("hello world", ["hi", "hello", "hey"])
  // True
  
  // Check multiple suffixes at once (ends_with_any - NEW!)
  let is_image = core.ends_with_any("photo.png", [".jpg", ".png", ".gif"])
  // True
  
  let is_code = core.ends_with_any("main.gleam", [".gleam", ".erl", ".ex"])
  // True
}
```

### Whitespace Normalization (NEW in 1.0.0)

```gleam
import str/core

pub fn whitespace_examples() {
  // Collapse all whitespace to single spaces
  let normalized = core.normalize_whitespace("  hello   world  \n\t test  ")
  // "hello world test"
  
  // Great for cleaning user input
  let clean = core.normalize_whitespace("   John    Doe   ")
  // "John Doe"
}
```

### Text Utilities

```gleam
import str/core

pub fn utility_examples() {
  // Reverse word order
  let reversed = core.reverse_words("hello beautiful world")
  // "world beautiful hello"
  
  // Extract initials
  let init = core.initials("John Fitzgerald Kennedy")
  // "JFK"
  
  // Regex escaping for pattern matching
  let pattern = core.escape_regex("hello.world[test]")
  // "hello\\.world\\[test\\]"
}
```

## OTP-based Unicode Normalization

### Implementation Location

Define Unicode normalization helpers in your application code (not in the `str` library). These helpers should implement the `String -> String` signature and can be passed to any `str` function that accepts a normalizer parameter.

### Example Implementation

```gleam
// file: src/normalize.gleam (in your app, not in `str`)
pub fn otp_nfd(s: String) -> String {
  // Call OTP from your app via Erlang interop. Example (conceptual):
  // :unicode.characters_to_nfd_binary(s)
  s
}

// Use it when calling into `str`:
let folded = str::extra::ascii_fold_with_normalizer("Crème Brûlée", otp_nfd)
let slug = str::extra::slugify_opts_with_normalizer("Crème Brûlée", 0, "-", False, otp_nfd)
```

Notes:
- Put the code above in your application so the `str` package remains
  free of OTP as a hard dependency.
- The exact Erlang interop call depends on your project setup and
  runtime; the example above is conceptual.

## 2) Convenience alias `slugify_with_normalizer`

A short wrapper is available for convenience. Example usage:

```gleam
// short alias: uses default separator `-` and no token limit
let s = "Café ❤️ Gleam"
let slug = str::extra::slugify_with_normalizer(s, otp_nfd)
```

## 3) No-decompose variants

If you prefer not to run the library's limited Latin decomposer you can
call the `_no_decompose_` variants and still pass a normalizer:

```gleam
let folded = str::extra::ascii_fold_no_decompose_with_normalizer(s, otp_nfd)
```

This gives you full control over decomposition/normalization order.

## 4) Testing locally (use `gleam test`)

The project uses Gleam's test runner. Example commands:

```sh
# run all tests (325 tests)
gleam test

# run a single test file (shell navigation)
cd /path/to/project && gleam test
```

## 5) Regenerating character tables (docs)

If you extend `src/str/internal_translit.gleam` or
`src/str/internal_decompose.gleam`, regenerate the JSON used by the
docs:

```sh
python3 scripts/generate_character_tables.py
```

## 6) Example of a small fake normalizer (useful for tests)

In tests it's handy to simulate NFD/NFC without OTP. Example:

```gleam
let fake_nfd = fn(x) { string.replace(x, "é", "e\u{0301}") }
let slug = str::extra::slugify_opts_with_normalizer("Café", 0, "-", False, fake_nfd)
assert slug == "cafe"
```

## 8) Where to put NFC/NFD helpers (application-side)

If you want to provide explicit `nfc`/`nfd` helpers that call OTP, put
them in your application (not in the `str` library). Example (commented):

```gleam
// file: src/normalize.gleam (in your app)
// pub fn nfd(s: String) -> String {
//   // :unicode.characters_to_nfd_binary(s)
// }
//
// pub fn nfc(s: String) -> String {
//   // :unicode.characters_to_nfc_binary(s)
// }
```

## 7) Tokenization reference

If you need a pure-Gleam tokenizer for special processing, see
`src/str/tokenize.gleam` which provides a pedagogic reference implementation.
