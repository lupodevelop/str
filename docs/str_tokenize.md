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
