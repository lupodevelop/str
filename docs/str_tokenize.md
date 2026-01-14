**Tokenizer**

> **Note (v2.0.0):** The legacy module `str/tokenize` is deprecated; prefer the re-exported tokenizer functions on the main module (`str.chars` and `str.chars_stdlib`) for new code.

- **Description:** The `str` library exposes two tokenizer functions for extracting grapheme clusters from text:
  - `tokenize.chars/1`: an experimental pure-Gleam implementation that approximates grapheme segmentation.
  - `tokenize.chars_stdlib/1`: a thin wrapper over the BEAM stdlib `string.to_graphemes/1` and the recommended choice for production.

- **When to use which:**
  - **`chars_stdlib/1` (recommended):** Use in production code. It uses the BEAM runtime's grapheme segmentation, is more accurate for edge cases (UAX #29) and typically faster.
  - **`chars/1` (experimental):** Useful when you want a self-contained, pure-Gleam implementation (for debugging, learning, or portability guarantees within Gleam code). It approximates common grapheme rules (combining marks, variation selectors, skin tones, ZWJ sequences) but may differ on rare or exotic sequences.

- **Examples:**
  - `tokenize.chars("café")` -> `["c", "a", "f", "é"]`
  - `tokenize.chars_stdlib("👩\u{200D}👩")` -> `["👩\u{200D}👩"]`

- **Notes:**
  - Both functions return a `List(String)` of grapheme clusters.
  - If you are writing performance-sensitive code that repeatedly scans long strings, prefer `chars_stdlib/1` and avoid repeated full-tokenization where possible.

**Guidance for library maintainers**

- Keep `chars/1` as an experimental reference implementation. If a user-reported bug shows a clear mismatch between `chars/1` and the BEAM stdlib for a case that matters, prefer fixing docs or recommending `chars_stdlib/1` rather than changing the experimental algorithm in-place unless necessary.
# str/tokenize — Pure-Gleam tokenizer (reference)

This module contains a tokenizer implemented entirely in Gleam as a pedagogical reference. It is not intended to replace standard library APIs, but to show how to iterate grapheme clusters in pure Gleam without NIFs or native dependencies.

Key functions

- `chars(text: String) -> List(String)`
  : Returns the list of grapheme clusters for the input string.

- `words(text: String) -> List(String)`
  : Simple whitespace-normalized word split.

- `scan_with_state(text, init_state, fun)`
  : Generic scanner that calls `fun` for each grapheme with a state value.

Example

```gleam
let chars = str_tokenize::chars("café")
// -> ["c", "a", "f", "é"]
```

When to use this module

- Use when you need a pure-Gleam tokenizer for study, debugging, or environments that cannot rely on native libraries.
