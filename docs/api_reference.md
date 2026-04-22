# str — API Reference

Complete function reference for `import str`. For module internals see
[str_core.md](str_core.md) and [str_extra.md](str_extra.md).

---

## Case & Capitalization

| Function | Example | Result |
|----------|---------|--------|
| `capitalize(text)` | `"hELLO wORLD"` | `"Hello world"` |
| `swapcase(text)` | `"Hello World"` | `"hELLO wORLD"` |
| `is_uppercase(text)` | `"HELLO123"` | `True` |
| `is_lowercase(text)` | `"hello_world"` | `True` |
| `is_title_case(text)` | `"Hello World"` | `True` |
| `is_mixed_case(text)` | `"helloWorld"` | `True` |

### Case Conversions

```gleam
str.to_snake_case("Hello World")    // → "hello_world"
str.to_camel_case("hello world")    // → "helloWorld"
str.to_pascal_case("hello world")   // → "HelloWorld"
str.to_kebab_case("Hello World")    // → "hello-world"
str.to_title_case("hello world")    // → "Hello World"
str.camel_to_snake("camelCase")     // → "camel_case"
str.pascal_to_snake("PascalCase")   // → "pascal_case"  // alias for camel_to_snake
str.snake_to_camel("snake_case")    // → "snakeCase"
str.snake_to_pascal("snake_case")   // → "SnakeCase"
```

---

## Grapheme Extraction

| Function | Example | Result |
|----------|---------|--------|
| `take(text, n)` | `take("👨‍👩‍👧‍👦abc", 2)` | `"👨‍👩‍👧‍👦a"` |
| `drop(text, n)` | `drop("hello", 2)` | `"llo"` |
| `take_right(text, n)` | `take_right("hello", 3)` | `"llo"` |
| `drop_right(text, n)` | `drop_right("hello", 2)` | `"hel"` |
| `at(text, index)` | `at("hello", 1)` | `Ok("e")` |
| `chunk(text, size)` | `chunk("abcdef", 2)` | `["ab", "cd", "ef"]` |
| `length(text)` | `length("👨‍👩‍👧‍👦")` | `1` |
| `reverse(text)` | `reverse("Hello 👋")` | `"👋 olleH"` |

---

## Search & Replace

| Function | Example | Result |
|----------|---------|--------|
| `index_of(text, needle)` | `"hello world", "world"` | `Ok(6)` |
| `last_index_of(text, needle)` | `"hello hello", "hello"` | `Ok(6)` |
| `contains(text, needle)` | `"hello world", "world"` | `True` |
| `contains_any(text, needles)` | `"hello", ["x", "e"]` | `True` |
| `contains_all(text, needles)` | `"hello", ["h", "e"]` | `True` |
| `replace_first(text, old, new)` | `"aaa", "a", "b"` | `"baa"` |
| `replace_last(text, old, new)` | `"aaa", "a", "b"` | `"aab"` |

### Search Strategies (experimental)

Automatic heuristic selection between KMP (long/repetitive patterns) and
sliding window (short patterns, zero allocations). Configure thresholds in
`str/config`.

| Function | Description |
|----------|-------------|
| `index_of_auto(text, pattern)` | Heuristic algorithm selection |
| `index_of_strategy(text, pattern, Kmp\|Sliding)` | Explicit algorithm |
| `count_auto(text, pattern, overlapping)` | Heuristic for counting |
| `count_strategy(text, pattern, overlapping, Kmp\|Sliding)` | Explicit count |

For pre-built KMP maps (hot loops), use `import str/advanced`. See
[str_core.md](str_core.md) for details.

---

## Splitting & Partitioning

| Function | Example | Result |
|----------|---------|--------|
| `partition(text, sep)` | `"a-b-c", "-"` | `#("a", "-", "b-c")` |
| `rpartition(text, sep)` | `"a-b-c", "-"` | `#("a-b", "-", "c")` |
| `splitn(text, sep, n)` | `"a-b-c-d", "-", 2` | `["a", "b-c-d"]` |
| `words(text)` | `"hello  world"` | `["hello", "world"]` |
| `lines(text)` | `"a\nb\nc"` | `["a", "b", "c"]` |

---

## Padding & Filling

| Function | Example | Result |
|----------|---------|--------|
| `pad_left(text, width, pad)` | `"42", 5, "0"` | `"00042"` |
| `pad_right(text, width, pad)` | `"hi", 5, "*"` | `"hi***"` |
| `center(text, width, pad)` | `"hi", 6, "-"` | `"--hi--"` |
| `fill(text, width, pad, pos)` | `"x", 5, "-", Both` | `"--x--"` |

`FillPosition` values: `Left`, `Right`, `Both`.

---

## Prefix & Suffix

| Function | Example | Result |
|----------|---------|--------|
| `starts_with(text, prefix)` | `"hello", "he"` | `True` |
| `ends_with(text, suffix)` | `"file.txt", ".txt"` | `True` |
| `starts_with_any(text, list)` | `"hello", ["hi", "he"]` | `True` |
| `ends_with_any(text, list)` | `"file.txt", [".txt"]` | `True` |
| `remove_prefix(text, prefix)` | `"hello world", "hello "` | `"world"` |
| `remove_suffix(text, suffix)` | `"file.txt", ".txt"` | `"file"` |
| `ensure_prefix(text, prefix)` | `"world", "hello "` | `"hello world"` |
| `ensure_suffix(text, suffix)` | `"file", ".txt"` | `"file.txt"` |
| `common_prefix(strings)` | `["abc", "abd"]` | `"ab"` |
| `common_suffix(strings)` | `["abc", "xbc"]` | `"bc"` |

---

## Validation

| Function | Description |
|----------|-------------|
| `is_empty(text)` | Empty string |
| `is_blank(text)` | Whitespace only |
| `is_numeric(text)` | Digits only (0–9) |
| `is_alpha(text)` | Letters only |
| `is_alphanumeric(text)` | Letters and digits |
| `is_ascii(text)` | ASCII only (0x00–0x7F) |
| `is_printable(text)` | Printable ASCII (0x20–0x7E) |
| `is_hex(text)` | Hexadecimal (0–9, a–f, A–F) |
| `is_uppercase(text)` | All cased chars uppercase |
| `is_lowercase(text)` | All cased chars lowercase |
| `is_title_case(text)` | Title Case format |
| `is_mixed_case(text)` | Both cases present |

---

## Escaping

| Function | Example | Result |
|----------|---------|--------|
| `escape_html(text)` | `"<div>"` | `"&lt;div&gt;"` |
| `unescape_html(text)` | `"&lt;div&gt;"` | `"<div>"` |
| `escape_regex(text)` | `"a.b*c"` | `"a\\.b\\*c"` |

---

## Similarity & Distance

| Function | Example | Result |
|----------|---------|--------|
| `distance(a, b)` | `"kitten", "sitting"` | `3` |
| `similarity(a, b)` | `"hello", "hallo"` | `0.8` |
| `hamming_distance(a, b)` | `"karolin", "kathrin"` | `Ok(3)` |

`similarity` uses normalized Levenshtein: `1.0 - distance(a,b) / max(len(a), len(b))`.

---

## Text Manipulation

| Function | Description |
|----------|-------------|
| `truncate(text, len, suffix)` | Truncate grapheme-aware |
| `truncate_strict(text, len, suffix)` | Truncate — may split emoji |
| `truncate_preserve(text, len, suffix)` | Truncate — keeps emoji whole |
| `ellipsis(text, len)` | Truncate with `…` |
| `normalize_whitespace(text)` | Collapse to single spaces |
| `strip(text, chars)` | Remove char set from ends |
| `squeeze(text, char)` | Collapse consecutive chars |
| `chomp(text)` | Remove trailing newline |
| `surround(text, pre, suf)` | Add prefix and suffix |
| `unwrap(text, pre, suf)` | Remove prefix and suffix |
| `strip_affixes(text, pre, suf)` | Alias for `unwrap` (preferred) |
| `reverse_words(text)` | Reverse word order |
| `initials(text)` | Extract initials |

`strip(text, chars)` — `chars` is a **set of graphemes**, not a literal substring.

---

## Line Operations

| Function | Description |
|----------|-------------|
| `lines(text)` | Split into lines |
| `dedent(text)` | Remove common indentation |
| `indent(text, spaces)` | Add indentation |
| `wrap_at(text, width)` | Word wrap |

---

## ASCII Folding

```gleam
str.ascii_fold("Crème Brûlée")  // → "Creme Brulee"
str.ascii_fold("straße")        // → "strasse"
str.ascii_fold("æon")           // → "aeon"
```

`ascii_fold_no_decompose` applies only the precomposed replacement table,
skipping NFD decomposition. Use it when the input is already NFC-normalized
and you want to avoid re-decomposing.

For OTP-based NFC/NFD normalization, see [otp_integration.md](otp_integration.md).

---

## Slug Generation

```gleam
str.slugify("Hello, World!")                     // → "hello-world"
str.slugify_opts("one two three", 2, "-", False) // → "one-two"
str.slugify_opts("Hello World", -1, "_", False)  // → "hello_world"
```

`slugify_opts` parameters:
- `max_len` — max tokens. `-1` = no limit.
- `sep` — separator string.
- `preserve_unicode` — `True` keeps non-ASCII chars instead of folding.
