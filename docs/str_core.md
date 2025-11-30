# str/core — Grapheme-Aware Core Utilities

## Overview

The `str/core` module provides fundamental string operations that correctly handle Unicode grapheme clusters, including:

- Complex emoji sequences (ZWJ, skin tones, flags)
- Combining character sequences (diacritics, accents)
- Multi-codepoint grapheme clusters
- CRLF line endings (treated as single grapheme)

All functions in this module operate at the grapheme boundary level, ensuring Unicode correctness.

## API Reference

### Truncation

#### `truncate(text: String, max_len: Int, suffix: String) -> String`

Truncates text to a maximum number of grapheme clusters, appending a suffix.

**Example**:
```gleam
truncate("Hello 👨‍👩‍👧‍👦 World", 8, "...")  // "Hello 👨‍👩‍👧‍👦..."
```

#### `truncate_preserve(text: String, max_len: Int, suffix: String) -> String`

Variant that prioritizes preserving complete emoji sequences.

#### `truncate_strict(text: String, max_len: Int, suffix: String) -> String`

Strict truncation that may split complex sequences if necessary.

#### `truncate_default(text: String, max_len: Int) -> String`

Convenience function using "..." as the default suffix.

#### `ellipsis(text: String, max_len: Int) -> String`

Truncates text with ellipsis (…) suffix.

**Example**:
```gleam
ellipsis("Hello World", 8)  // "Hello W…"
```

### String Reversal

#### `reverse(text: String) -> String`

Reverses text at grapheme cluster boundaries.

**Example**:
```gleam
reverse("café")       // "éfac"
reverse("👨‍👩‍👧‍👦")  // "👨‍👩‍👧‍👦" (single cluster, unchanged)
```

### Grapheme Extraction

#### `length(text: String) -> Int`

Returns the number of grapheme clusters in text. This is a grapheme-aware length function that correctly counts complex emoji, combining sequences, flags, and other multi-codepoint graphemes.

**Example**:
```gleam
length("hello")       // 5
length("👨‍👩‍👧‍👦")        // 1 (single family emoji cluster)
length("café")        // 4 (with combining accent)
length("🇮🇹")          // 1 (flag is a single grapheme)
length("")            // 0
```

#### `take(text: String, n: Int) -> String`

Returns the first N grapheme clusters from text.

**Example**:
```gleam
take("hello", 3)       // "hel"
take("👨‍👩‍👧‍👦abc", 2)  // "👨‍👩‍👧‍👦a"
```

#### `drop(text: String, n: Int) -> String`

Drops the first N grapheme clusters from text.

**Example**:
```gleam
drop("hello", 2)       // "llo"
drop("👨‍👩‍👧‍👦abc", 1)  // "abc"
```

#### `at(text: String, index: Int) -> Result(String, Nil)`

Returns the grapheme cluster at the given index (0-based).

**Example**:
```gleam
at("hello", 1)       // Ok("e")
at("👨‍👩‍👧‍👦abc", 0)  // Ok("👨‍👩‍👧‍👦")
at("hi", 10)         // Error(Nil)
```

#### `chunk(text: String, size: Int) -> List(String)`

Splits text into chunks of N graphemes. Like Rust's chunks() or Lodash's chunk(). The last chunk may be smaller.

**Example**:
```gleam
chunk("abcdef", 2)   // ["ab", "cd", "ef"]
chunk("abcdefg", 3)  // ["abc", "def", "g"]
chunk("hello", 10)   // ["hello"]
chunk("👨‍👩‍👧‍👦ab", 2)   // ["👨‍👩‍👧‍👦a", "b"]
```

### Padding

#### `pad_left(text: String, width: Int, pad: String) -> String`

Pads text on the left to reach the specified width.

**Example**:
```gleam
pad_left("hi", 5, " ")    // "   hi"
pad_left("x", 3, "->")    // "->->x"
```

#### `pad_right(text: String, width: Int, pad: String) -> String`

Pads text on the right.

#### `center(text: String, width: Int, pad: String) -> String`

Centers text within the specified width (left-biased when uneven).

**Example**:
```gleam
center("hi", 6, " ")  // "  hi  "
```

#### `fill(text: String, width: Int, pad: String, position: FillPosition) -> String`

Flexible padding function. Position is a type: `Left`, `Right`, or `Both` (center).

**Example**:
```gleam
fill("x", 5, "-", Left)   // "----x"
fill("x", 5, "-", Right)  // "x----"
fill("x", 5, "-", Both)   // "--x--"
fill("42", 5, "0", Left)  // "00042"
```

### Counting

#### `count(haystack: String, needle: String, overlapping: Bool) -> Int`

Counts occurrences of a substring (grapheme-aware).

**Example**:
```gleam
count("aaaa", "aa", True)   // 3 (overlapping)
count("aaaa", "aa", False)  // 2 (non-overlapping)
count("👩👩👩", "👩", True)   // 3
```

### Blank Detection

#### `is_blank(text: String) -> Bool`

Checks if a string contains only whitespace characters.

**Example**:
```gleam
is_blank("")           // True
is_blank("   ")        // True
is_blank("  hello  ")  // False
```

### Word and Line Operations

#### `words(text: String) -> List(String)`

Splits text into words by whitespace.

**Example**:
```gleam
words("Hello  world\n\ttest")  // ["Hello", "world", "test"]
```

#### `lines(text: String) -> List(String)`

Splits text into lines. Handles \n, \r\n, and \r.

**Example**:
```gleam
lines("a\nb\nc")    // ["a", "b", "c"]
lines("a\r\nb")     // ["a", "b"]
```

#### `splitn(text: String, sep: String, n: Int) -> List(String)`

Splits text on separator with a maximum number of parts. Like Python's str.split(sep, n).

**Example**:
```gleam
splitn("a-b-c-d", "-", 2)  // ["a", "b-c-d"]
splitn("a-b-c-d", "-", 3)  // ["a", "b", "c-d"]
splitn("a-b", "-", 10)     // ["a", "b"]
splitn("hello", "-", 2)    // ["hello"]
```

#### `dedent(text: String) -> String`

Removes common leading whitespace from all lines.

**Example**:
```gleam
dedent("  a\n  b\n  c")  // "a\nb\nc"
```

#### `indent(text: String, spaces: Int) -> String`

Adds indentation to each line.

**Example**:
```gleam
indent("hello\nworld", 2)  // "  hello\n  world"
```

#### `wrap_at(text: String, width: Int) -> String`

Wraps text at the specified width, breaking on word boundaries.

**Example**:
```gleam
wrap_at("hello world foo bar", 11)  // "hello world\nfoo bar"
```

#### `chomp(text: String) -> String`

Removes trailing newline if present (handles \n, \r\n, \r as graphemes).

**Example**:
```gleam
chomp("hello\n")    // "hello"
chomp("hello\r\n")  // "hello"
```

### String Wrapping

#### `surround(text: String, prefix: String, suffix: String) -> String`

Wraps text with prefix and suffix.

**Example**:
```gleam
surround("world", "Hello ", "!")  // "Hello world!"
```

#### `unwrap(text: String, prefix: String, suffix: String) -> String`

Removes prefix and suffix if both are present.

### Character Stripping

#### `strip(text: String, chars: String) -> String`

Removes specified characters from both ends of text.

**Example**:
```gleam
strip("..hello..", ".")  // "hello"
```

#### `squeeze(text: String, char: String) -> String`

Collapses consecutive occurrences of a character to a single instance.

**Example**:
```gleam
squeeze("heeello", "e")              // "hello"
squeeze("   hello   world   ", " ")  // " hello world "
```

### Partitioning

#### `partition(text: String, sep: String) -> #(String, String, String)`

Splits text into three parts: before, separator, and after.

**Example**:
```gleam
partition("a-b-c", "-")  // #("a", "-", "b-c")
partition("hello", "-")  // #("hello", "", "")
```

#### `rpartition(text: String, sep: String) -> #(String, String, String)`

Splits text from the last occurrence of separator. Like Python's str.rpartition().
If separator not found, returns #("", "", text).

**Example**:
```gleam
rpartition("a-b-c", "-")    // #("a-b", "-", "c")
rpartition("hello", "-")    // #("", "", "hello")
rpartition("a--b--c", "--") // #("a--b", "--", "c")
```

#### `common_prefix(strings: List(String)) -> String`

Finds the longest common prefix among a list of strings.

**Example**:
```gleam
common_prefix(["abc", "abd", "abe"])  // "ab"
```

#### `common_suffix(strings: List(String)) -> String`

Finds the longest common suffix among a list of strings.

**Example**:
```gleam
common_suffix(["abc", "xbc", "zbc"])  // "bc"
```

### Character Type Checks

#### `is_numeric(text: String) -> Bool`

Checks if text contains only ASCII digits (0-9).

**Example**:
```gleam
is_numeric("12345")   // True
is_numeric("123.45")  // False
```

#### `is_alpha(text: String) -> Bool`

Checks if text contains only ASCII letters (a-z, A-Z).

**Example**:
```gleam
is_alpha("hello")     // True
is_alpha("hello123")  // False
```

#### `is_alphanumeric(text: String) -> Bool`

Checks if text contains only ASCII letters and digits.

**Example**:
```gleam
is_alphanumeric("hello123")    // True
is_alphanumeric("hello-world") // False
```

### Prefix/Suffix Manipulation

#### `remove_prefix(text: String, prefix: String) -> String`

Removes prefix from text if present.

**Example**:
```gleam
remove_prefix("hello world", "hello ")  // "world"
remove_prefix("hello", "bye")           // "hello"
```

#### `remove_suffix(text: String, suffix: String) -> String`

Removes suffix from text if present.

#### `ensure_prefix(text: String, prefix: String) -> String`

Adds prefix if not already present.

**Example**:
```gleam
ensure_prefix("world", "hello ")        // "hello world"
ensure_prefix("hello world", "hello ")  // "hello world"
```

#### `ensure_suffix(text: String, suffix: String) -> String`

Adds suffix if not already present.

#### `starts_with_any(text: String, prefixes: List(String)) -> Bool`

Checks if text starts with any of the given prefixes. Like Lodash's startsWith with multiple options.

**Example**:
```gleam
starts_with_any("hello", ["hi", "he", "ha"])  // True
starts_with_any("hello", ["x", "y", "z"])     // False
starts_with_any("", ["a"])                     // False
starts_with_any("hello", [])                   // False
```

#### `ends_with_any(text: String, suffixes: List(String)) -> Bool`

Checks if text ends with any of the given suffixes.

**Example**:
```gleam
ends_with_any("file.txt", [".txt", ".md"])   // True
ends_with_any("file.rs", [".txt", ".md"])    // False
ends_with_any("hello", ["lo", "llo", "o"])   // True
```

### Case Manipulation

#### `swapcase(text: String) -> String`

Swaps case of all ASCII letters.

**Example**:
```gleam
swapcase("Hello World")  // "hELLO wORLD"
```

#### `capitalize(text: String) -> String`

Capitalizes first grapheme and lowercases the rest. Like Python's str.capitalize().

**Example**:
```gleam
capitalize("hello world")  // "Hello world"
capitalize("hELLO wORLD")  // "Hello world"
capitalize("HELLO")        // "Hello"
capitalize("123abc")       // "123abc"
```

### String Distance

#### `distance(a: String, b: String) -> Int`

Calculates Levenshtein distance between two strings.

**Example**:
```gleam
distance("kitten", "sitting")  // 3
distance("hello", "hello")     // 0
```

### Search and Index

#### `index_of(text: String, needle: String) -> Result(Int, Nil)`

Finds the index of the first occurrence of needle in text (grapheme-aware).

**Example**:
```gleam
index_of("hello world", "world")  // Ok(6)
index_of("👨‍👩‍👧‍👦 family", "family")   // Ok(2)
index_of("hello", "x")            // Error(Nil)
```

#### `last_index_of(text: String, needle: String) -> Result(Int, Nil)`

Finds the index of the last occurrence of needle in text.

**Example**:
```gleam
last_index_of("hello hello", "hello")  // Ok(6)
last_index_of("a-b-c", "-")            // Ok(3)
```

#### `contains(text: String, needle: String) -> Bool`

Returns `True` if `needle` is found in `text`. This is grapheme-aware and correctly handles complex Unicode sequences.

**Example**:
```gleam
contains("hello world", "world")  // True
contains("hello", "x")            // False
contains("👨‍👩‍👧‍👦 family", "👨‍👩‍👧‍👦")    // True
contains("", "")                  // False
```

#### `starts_with(text: String, prefix: String) -> Bool`

Returns `True` if `text` starts with `prefix` on grapheme boundaries.

**Example**:
```gleam
starts_with("hello", "he")         // True
starts_with("hello", "")           // True
starts_with("hi", "hello")         // False
starts_with("👨‍👩‍👧‍👦abc", "👨‍👩‍👧‍👦")      // True
```

#### `ends_with(text: String, suffix: String) -> Bool`

Returns `True` if `text` ends with `suffix` on grapheme boundaries.

**Example**:
```gleam
ends_with("hello.txt", ".txt")     // True
ends_with("hello", "")             // True
ends_with("hi", "hello")           // False
ends_with("abc👨‍👩‍👧‍👦", "👨‍👩‍👧‍👦")        // True
```

#### `contains_any(text: String, needles: List(String)) -> Bool`

Checks if text contains any of the given needles.

**Example**:
```gleam
contains_any("hello world", ["foo", "world"])  // True
contains_any("hello", ["x", "y", "z"])         // False
```

#### `contains_all(text: String, needles: List(String)) -> Bool`

Checks if text contains all of the given needles.

**Example**:
```gleam
contains_all("hello world", ["hello", "world"])  // True
contains_all("hello", ["hello", "x"])            // False
```

### Replacement Variants

#### `replace_first(text: String, old: String, new: String) -> String`

Replaces only the first occurrence of old with new.

**Example**:
```gleam
replace_first("hello hello", "hello", "hi")  // "hi hello"
replace_first("aaa", "a", "b")               // "baa"
```

#### `replace_last(text: String, old: String, new: String) -> String`

Replaces only the last occurrence of old with new.

**Example**:
```gleam
replace_last("hello hello", "hello", "hi")  // "hello hi"
replace_last("aaa", "a", "b")               // "aab"
```

### Validation Functions

#### `is_uppercase(text: String) -> Bool`

Checks if all cased characters are uppercase. Non-cased characters are ignored.

**Example**:
```gleam
is_uppercase("HELLO")     // True
is_uppercase("Hello")     // False
is_uppercase("HELLO123")  // True (numbers ignored)
is_uppercase("123")       // False (no cased chars)
```

#### `is_lowercase(text: String) -> Bool`

Checks if all cased characters are lowercase.

**Example**:
```gleam
is_lowercase("hello")     // True
is_lowercase("Hello")     // False
is_lowercase("hello123")  // True
```

#### `is_title_case(text: String) -> Bool`

Checks if text is in Title Case format: each word starts with uppercase and continues with lowercase. Words that don't start with a letter (numbers, emoji, punctuation) are ignored.

**Example**:
```gleam
is_title_case("Hello World")        // True
is_title_case("Hello world")        // False
is_title_case("Hello 123 World")    // True (numbers ignored)
is_title_case("Hello 🎉 World")     // True (emoji ignored)
is_title_case("")                   // False
```

#### `is_empty(text: String) -> Bool`

Returns `True` if `text` is an empty string.

**Example**:
```gleam
is_empty("")   // True
is_empty(" ")  // False
is_empty("a")  // False
```

#### `is_ascii(text: String) -> Bool`

Checks if text contains only ASCII characters (0x00-0x7F).

**Example**:
```gleam
is_ascii("hello!@#")  // True
is_ascii("café")      // False
is_ascii("👋")        // False
```

#### `is_printable(text: String) -> Bool`

Checks if text contains only printable ASCII characters (0x20-0x7E).

**Example**:
```gleam
is_printable("hello")    // True
is_printable("hello\n")  // False
is_printable("hello\t")  // False
```

#### `is_hex(text: String) -> Bool`

Checks if text contains only hexadecimal characters (0-9, a-f, A-F).

**Example**:
```gleam
is_hex("abc123")   // True
is_hex("DEADBEEF") // True
is_hex("xyz")      // False
```

### HTML Escaping

#### `escape_html(text: String) -> String`

Escapes HTML special characters to their entity equivalents.

**Example**:
```gleam
escape_html("<div>Hello</div>")  // "&lt;div&gt;Hello&lt;/div&gt;"
escape_html("Tom & Jerry")       // "Tom &amp; Jerry"
escape_html("Say \"hello\"")     // "Say &quot;hello&quot;"
```

#### `unescape_html(text: String) -> String`

Unescapes HTML entities to their character equivalents.

**Example**:
```gleam
unescape_html("&lt;div&gt;")     // "<div>"
unescape_html("Tom &amp; Jerry") // "Tom & Jerry"
```

#### `escape_regex(text: String) -> String`

Escapes regex metacharacters for use as a literal pattern.

**Example**:
```gleam
escape_regex("hello.world")  // "hello\\.world"
escape_regex("[test]")       // "\\[test\\]"
escape_regex("a+b*c?")       // "a\\+b\\*c\\?"
```

### Similarity

#### `similarity(a: String, b: String) -> Float`

Calculates similarity as a percentage (0.0 to 1.0) based on Levenshtein distance.

**Example**:
```gleam
similarity("hello", "hello")  // 1.0
similarity("hello", "hallo")  // 0.8
similarity("abc", "xyz")      // 0.0
```

#### `hamming_distance(a: String, b: String) -> Result(Int, Nil)`

Calculates Hamming distance between two strings of equal length.

**Example**:
```gleam
hamming_distance("karolin", "kathrin")  // Ok(3)
hamming_distance("hello", "hallo")      // Ok(1)
hamming_distance("abc", "ab")           // Error(Nil)
```

### Additional Transformations

#### `take_right(text: String, n: Int) -> String`

Returns the last N grapheme clusters from text.

**Example**:
```gleam
take_right("hello", 3)       // "llo"
take_right("👨‍👩‍👧‍👦abc", 2)  // "bc"
```

#### `drop_right(text: String, n: Int) -> String`

Drops the last N grapheme clusters from text.

**Example**:
```gleam
drop_right("hello", 2)       // "hel"
drop_right("👨‍👩‍👧‍👦abc", 2)  // "👨‍👩‍👧‍👦a"
```

#### `reverse_words(text: String) -> String`

Reverses the order of words in text.

**Example**:
```gleam
reverse_words("hello world")    // "world hello"
reverse_words("one two three")  // "three two one"
```

#### `initials(text: String) -> String`

Extracts initials from text (first letter of each word, uppercase).

**Example**:
```gleam
initials("John Doe")            // "JD"
initials("visual studio code")  // "VSC"
```

#### `normalize_whitespace(text: String) -> String`

Collapses all consecutive whitespace (spaces, tabs, newlines) into single spaces and trims. Like JavaScript's equivalent.

**Example**:
```gleam
normalize_whitespace("  hello   world  ")     // "hello world"
normalize_whitespace("hello\n\tworld")        // "hello world"
normalize_whitespace("  a  b  c  ")           // "a b c"
normalize_whitespace("")                      // ""
```

## Implementation Notes

### Grapheme Cluster Detection

The module uses `string.to_graphemes/1` from the Gleam standard library for grapheme segmentation, which provides Unicode-compliant grapheme cluster boundaries (UAX #29).

Key behaviors:
- `\r\n` is treated as a single grapheme (CRLF cluster)
- Emoji ZWJ sequences are single graphemes
- Combining marks stay attached to their base character

### Performance Considerations

All functions operate in linear time with respect to the number of grapheme clusters. For very large strings (>100KB), consider pre-processing or chunking.

## See Also

- [str/extra](str_extra.md) — ASCII folding and slug generation
- [str/tokenize](str_tokenize.md) — Pure-Gleam tokenizer reference
- [Examples](../EXAMPLES.md) — Integration examples
