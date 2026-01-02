//// Core grapheme-aware string utilities for Unicode-correct operations.
////
//// This module provides fundamental string operations that respect Unicode
//// grapheme cluster boundaries, ensuring correct handling of:
//// - Complex emoji (ZWJ sequences, skin tones, flags)
//// - Combining character sequences (diacritics, accents)
//// - Regional indicators and variation selectors
////
//// All functions operate at the grapheme level rather than codepoint or byte
//// level, preventing visual corruption of composed characters.

import gleam/int
import gleam/list
import gleam/string

/// Detects if a grapheme cluster likely contains emoji components.
///
/// This is a heuristic detection covering common emoji patterns:
/// - Zero-Width Joiner (U+200D) for family/profession sequences
/// - Variation Selector (U+FE0F) for emoji presentation
/// - Keycap Sequence (U+20E3) for number emojis
/// - Skin Tone Modifiers (U+1F3FB-U+1F3FF)
/// - Regional Indicators (U+1F1E6-U+1F1FF) for flags
/// - Tag Characters (U+E0020-U+E007F) for flag variants
/// - Pictographic blocks (U+1F000-U+1FAFF)
///
/// Note: This is not a complete UAX #29 implementation but handles
/// common cases efficiently.
fn cluster_has_emoji(cluster: String) -> Bool {
  // Heuristic detection for emoji-like clusters. Not full Unicode UAX29,
  // but covers common cases: ZWJ sequences, variation selectors, skin tones,
  // regional indicators (flags), keycap sequences, tag characters and a wide
  // pictographic range.
  let cps = string.to_utf_codepoints(cluster)
  list.any(cps, fn(cp) {
    let code = string.utf_codepoint_to_int(cp)
    // Quick checks for single codepoints that indicate emoji composition
    case code == 0x200D || code == 0xFE0F || code == 0x20E3 {
      True -> True
      False ->
        case code >= 0x1F3FB && code <= 0x1F3FF {
          True -> True
          False ->
            case code >= 0x1F1E6 && code <= 0x1F1FF {
              True -> True
              False ->
                case code >= 0xE0020 && code <= 0xE007F {
                  // Tag characters (emoji tag sequences)
                  True -> True
                  False ->
                    // Broad pictographic/emoji blocks (approximation)
                    case code >= 0x1F000 && code <= 0x1FAFF {
                      True -> True
                      False -> False
                    }
                }
            }
        }
    }
  })
}

/// Truncates text while attempting to preserve emoji sequences.
///
/// This function tries to include complete emoji clusters when truncating,
/// extending the take window if an emoji would otherwise be split.
/// Used internally by `truncate_preserve`.
fn truncate_with_emoji_inclusion(
  clusters: List(String),
  take: Int,
  suffix: String,
) -> String {
  // Take the initial clusters.
  let initial = list.take(clusters, take)

  // If there's an emoji cluster within the first `max_len` clusters of the
  // original text but it's not included in `initial`, extend the kept
  // clusters to include up to that emoji to avoid splitting it.
  let window = list.take(clusters, take + grapheme_len(suffix))

  let keep = case find_emoji_index(window, 0) {
    Ok(idx) ->
      // If emoji is beyond current `take`, include up to the emoji.
      case idx + 1 > take {
        True -> list.take(clusters, idx + 1)
        False -> initial
      }
    Error(_) -> initial
  }

  list.fold(keep, "", fn(acc, s) { acc <> s }) <> suffix
}

/// Finds the index of the first emoji-containing cluster in a list.
///
/// Returns Ok(index) if found, Error(Nil) if no emoji detected.
fn find_emoji_index(clusters: List(String), start: Int) -> Result(Int, Nil) {
  find_emoji_index_loop(clusters, start, 0)
}

fn find_emoji_index_loop(
  clusters: List(String),
  idx: Int,
  offset: Int,
) -> Result(Int, Nil) {
  case clusters {
    [] -> Error(Nil)
    [first, ..rest] ->
      case cluster_has_emoji(first) {
        True -> Ok(idx + offset)
        False -> find_emoji_index_loop(rest, idx, offset + 1)
      }
  }
}

/// Checks if a single grapheme is an uppercase ASCII letter.
fn is_grapheme_uppercase(g: String) -> Bool {
  case string.to_utf_codepoints(g) {
    [cp] -> {
      let code = string.utf_codepoint_to_int(cp)
      code >= 0x41 && code <= 0x5A
    }
    _ -> False
  }
}

/// Checks if a single grapheme is a lowercase ASCII letter.
fn is_grapheme_lowercase(g: String) -> Bool {
  case string.to_utf_codepoints(g) {
    [cp] -> {
      let code = string.utf_codepoint_to_int(cp)
      code >= 0x61 && code <= 0x7A
    }
    _ -> False
  }
}

/// Checks if a single grapheme is a cased ASCII letter (upper or lower).
fn is_grapheme_cased(g: String) -> Bool {
  is_grapheme_uppercase(g) || is_grapheme_lowercase(g)
}

/// Splits text into words by whitespace.
/// Normalizes tabs, newlines and multiple spaces, then filters empty strings.
///
///   words("Hello  world\n\ttest") -> ["Hello", "world", "test"]
///   words("   ") -> []
///
pub fn words(text: String) -> List(String) {
  // Normalize common whitespace characters to a single space, then split
  let normalized =
    text
    |> string.replace("\r\n", " ")
    |> string.replace("\n", " ")
    |> string.replace("\r", " ")
    |> string.replace("\t", " ")

  normalized
  |> string.split(" ")
  |> list.filter(fn(s) { s != "" })
}

/// Checks if a string contains only whitespace characters.
/// Returns True for empty strings or strings with only spaces, tabs, newlines.
///
///   is_blank("") -> True
///   is_blank("   ") -> True
///   is_blank("\t\n") -> True
///   is_blank("  hello  ") -> False
///
pub fn is_blank(text: String) -> Bool {
  string.trim(text) == ""
}

/// Repeats a string n times.
///
/// Internal helper for padding operations. Returns empty string if n <= 0.
fn repeat_str(s: String, n: Int) -> String {
  case n <= 0 {
    True -> ""
    False -> list.fold(list.range(1, n), "", fn(acc, _) { acc <> s })
  }
}

// Internal helper returning the number of grapheme clusters in a string.
fn grapheme_len(s: String) -> Int {
  string.to_graphemes(s) |> list.length
}

/// Internal loop for counting substring occurrences.
///
/// Implements both overlapping and non-overlapping count logic using
/// grapheme cluster comparison.
fn count_loop(
  hs: List(String),
  nd: List(String),
  nd_len: Int,
  overlapping: Bool,
  acc: Int,
) -> Int {
  count_loop_with_len(hs, list.length(hs), nd, nd_len, overlapping, acc)
}

fn count_loop_with_len(
  hs: List(String),
  hs_len: Int,
  nd: List(String),
  nd_len: Int,
  overlapping: Bool,
  acc: Int,
) -> Int {
  case hs_len < nd_len {
    True -> acc
    False ->
      case list.take(hs, nd_len) == nd {
        True ->
          case overlapping {
            True ->
              count_loop_with_len(
                list.drop(hs, 1),
                hs_len - 1,
                nd,
                nd_len,
                overlapping,
                acc + 1,
              )
            False ->
              count_loop_with_len(
                list.drop(hs, nd_len),
                hs_len - nd_len,
                nd,
                nd_len,
                overlapping,
                acc + 1,
              )
          }
        False ->
          count_loop_with_len(
            list.drop(hs, 1),
            hs_len - 1,
            nd,
            nd_len,
            overlapping,
            acc,
          )
      }
  }
}

/// Pads text on the left to reach the specified width (in grapheme clusters).
/// If the text is already equal to or longer than the width, returns unchanged.
///
///   pad_left("hi", 5, " ") -> "   hi"
///   pad_left("hello", 3, "*") -> "hello"
///
pub fn pad_left(text: String, width: Int, pad: String) -> String {
  let clusters = string.to_graphemes(text)
  let len = list.length(clusters)
  let pad_count = width - len
  case pad_count <= 0 {
    True -> text
    False -> repeat_str(pad, pad_count) <> text
  }
}

/// Pads text on the right to reach the specified width (in grapheme clusters).
/// If the text is already equal to or longer than the width, returns unchanged.
///
///   pad_right("hi", 5, " ") -> "hi   "
///   pad_right("hello", 3, "*") -> "hello"
///
pub fn pad_right(text: String, width: Int, pad: String) -> String {
  let clusters = string.to_graphemes(text)
  let len = list.length(clusters)
  let pad_count = width - len
  case pad_count <= 0 {
    True -> text
    False -> text <> repeat_str(pad, pad_count)
  }
}

/// Centers text within the specified width using the given padding.
/// When padding is uneven, the left side receives more (left-biased).
///
///   center("hi", 6, " ") -> "  hi  "
///   center("hi", 5, " ") -> " hi  "
///
pub fn center(text: String, width: Int, pad: String) -> String {
  let clusters = string.to_graphemes(text)
  let len = list.length(clusters)
  let total_pad = width - len
  let left = total_pad / 2
  let right = total_pad - left
  case total_pad <= 0 {
    True -> text
    False -> repeat_str(pad, left) <> text <> repeat_str(pad, right)
  }
}

/// Counts occurrences of needle in haystack (grapheme-aware).
/// If overlapping is True, counts overlapping matches.
///
///   count("aaaa", "aa", True) -> 3
///   count("aaaa", "aa", False) -> 2
///   count("hello", "", False) -> 0
///
pub fn count(haystack: String, needle: String, overlapping: Bool) -> Int {
  let hs = string.to_graphemes(haystack)
  let nd = string.to_graphemes(needle)
  let nd_len = list.length(nd)

  // Edge cases: empty needle -> 0
  case nd_len == 0 {
    True -> 0
    False -> count_loop(hs, nd, nd_len, overlapping, 0)
  }
}

/// Wraps text with a prefix and suffix.
///
///   surround("world", "Hello ", "!") -> "Hello world!"
///
pub fn surround(text: String, prefix: String, suffix: String) -> String {
  prefix <> text <> suffix
}

/// Removes prefix and suffix from text if both are present.
/// Operates on grapheme cluster boundaries.
///
///   unwrap("Hello world!", "Hello ", "!") -> "world"
///   unwrap("test", "<<", ">>") -> "test"
///
pub fn unwrap(text: String, prefix: String, suffix: String) -> String {
  // Grapheme-aware unwrap: only remove prefix/suffix when they align on
  // grapheme cluster boundaries.
  let t_clusters = string.to_graphemes(text)
  let p_clusters = string.to_graphemes(prefix)
  let s_clusters = string.to_graphemes(suffix)
  let t_len = list.length(t_clusters)
  let p_len = list.length(p_clusters)
  let s_len = list.length(s_clusters)

  case p_len + s_len > t_len {
    True -> text
    False ->
      case
        list.take(t_clusters, p_len) == p_clusters
        && list.take(list.reverse(t_clusters), s_len)
        == list.reverse(s_clusters)
      {
        True ->
          list.fold(
            list.take(list.drop(t_clusters, p_len), t_len - p_len - s_len),
            "",
            fn(acc, s) { acc <> s },
          )
        False -> text
      }
  }
}

/// Truncates text to max_len grapheme clusters with configurable options.
/// If keep_whole_emoji is True, extends to include complete emoji sequences.
///
///   truncate_with_flag("Hello World", 8, "...", True) -> "Hello..."
///
pub fn truncate_with_flag(
  text: String,
  max_len: Int,
  suffix: String,
  keep_whole_emoji: Bool,
) -> String {
  // Use stdlib grapheme segmentation for correctness.
  let clusters = string.to_graphemes(text)
  let total = list.length(clusters)
  let suffix_clusters = string.to_graphemes(suffix)
  let suffix_len = list.length(suffix_clusters)
  let take = max_len - suffix_len

  case max_len <= 0 {
    True -> ""
    False ->
      // If the text already fits and appending the suffix wouldn't exceed max_len,
      // return the original text.
      case total <= max_len && total + suffix_len <= max_len {
        True -> text
        False ->
          case take <= 0 {
            // Not enough room for content; return truncated suffix clusters joined
            True ->
              suffix_clusters
              |> list.take(max_len)
              |> list.fold("", fn(acc, s) { acc <> s })
            False ->
              case keep_whole_emoji {
                True -> truncate_with_emoji_inclusion(clusters, take, suffix)
                False ->
                  list.fold(list.take(clusters, take), "", fn(acc, s) {
                    acc <> s
                  })
                  <> suffix
              }
          }
      }
  }
}

/// Truncates text to max_len grapheme clusters, preserving emoji sequences.
///
///   truncate("Hello 👨‍👩‍👧‍👦 World", 10, "...") -> "Hello 👨‍👩‍👧‍👦..."
///
pub fn truncate(text: String, max_len: Int, suffix: String) -> String {
  truncate_with_flag(text, max_len, suffix, True)
}

/// Strictly truncates text to exact length without emoji preservation.
///
///   truncate_strict("Hi 👩‍👩‍👧‍👦", 3, "…") -> "Hi…"
///
pub fn truncate_strict(text: String, max_len: Int, suffix: String) -> String {
  truncate_with_flag(text, max_len, suffix, False)
}

/// Truncates text while prioritizing complete emoji sequences.
/// Explicit alias for the default truncate behavior.
pub fn truncate_preserve(text: String, max_len: Int, suffix: String) -> String {
  truncate_with_flag(text, max_len, suffix, True)
}

/// Truncates text using "..." as the default suffix.
///
///   truncate_default("Hello World", 8) -> "Hello..."
///
pub fn truncate_default(text: String, max_len: Int) -> String {
  truncate(text, max_len, "...")
}

/// Reverses text at grapheme cluster boundaries.
/// Preserves combining marks and keeps emoji sequences intact.
/// Involutive: reverse(reverse(x)) == x
///
///   reverse("café") -> "éfac"
///   reverse("👨‍👩‍👧‍👦") -> "👨‍👩‍👧‍👦"
///
pub fn reverse(text: String) -> String {
  text
  |> string.to_graphemes
  |> list.reverse
  |> string.concat
}

// ============================================================================
// GRAPHEME EXTRACTION
// ============================================================================

/// Returns the first N grapheme clusters from text.
///
///   take("hello", 3) -> "hel"
///   take("👨‍👩‍👧‍👦abc", 2) -> "👨‍👩‍👧‍👦a"
///   take("hi", 10) -> "hi"
///
pub fn take(text: String, n: Int) -> String {
  case n <= 0 {
    True -> ""
    False ->
      text
      |> string.to_graphemes
      |> list.take(n)
      |> string.concat
  }
}

/// Returns the number of grapheme clusters in `text`.
pub fn length(text: String) -> Int {
  grapheme_len(text)
}

/// Drops the first N grapheme clusters from text.
///
///   drop("hello", 2) -> "llo"
///   drop("👨‍👩‍👧‍👦abc", 1) -> "abc"
///   drop("hi", 10) -> ""
///
pub fn drop(text: String, n: Int) -> String {
  case n <= 0 {
    True -> text
    False ->
      text
      |> string.to_graphemes
      |> list.drop(n)
      |> string.concat
  }
}

/// Returns the grapheme cluster at the given index (0-based).
///
///   at("hello", 1) -> Ok("e")
///   at("👨‍👩‍👧‍👦abc", 0) -> Ok("👨‍👩‍👧‍👦")
///   at("hi", 10) -> Error(Nil)
///
pub fn at(text: String, index: Int) -> Result(String, Nil) {
  case index < 0 {
    True -> Error(Nil)
    False -> {
      let clusters = string.to_graphemes(text)
      case list.drop(clusters, index) {
        [first, ..] -> Ok(first)
        [] -> Error(Nil)
      }
    }
  }
}

/// Returns the last N grapheme clusters from text.
///
///   take_right("hello", 3) -> "llo"
///   take_right("👨‍👩‍👧‍👦abc", 2) -> "bc"
///   take_right("hi", 10) -> "hi"
///
pub fn take_right(text: String, n: Int) -> String {
  case n <= 0 {
    True -> ""
    False -> {
      let chars = string.to_graphemes(text)
      let len = list.length(chars)
      case n >= len {
        True -> text
        False ->
          chars
          |> list.drop(len - n)
          |> string.concat
      }
    }
  }
}

/// Drops the last N grapheme clusters from text.
///
///   drop_right("hello", 2) -> "hel"
///   drop_right("👨‍👩‍👧‍👦abc", 2) -> "👨‍👩‍👧‍👦a"
///   drop_right("hi", 10) -> ""
///
pub fn drop_right(text: String, n: Int) -> String {
  case n <= 0 {
    True -> text
    False -> {
      let chars = string.to_graphemes(text)
      let len = list.length(chars)
      case n >= len {
        True -> ""
        False ->
          chars
          |> list.take(len - n)
          |> string.concat
      }
    }
  }
}

/// Splits text into chunks of n grapheme clusters each.
/// The last chunk may be smaller if text doesn't divide evenly.
///
///   chunk("abcdefg", 2) -> ["ab", "cd", "ef", "g"]
///   chunk("hello", 3) -> ["hel", "lo"]
///   chunk("👨‍👩‍👧‍👦abc", 2) -> ["👨‍👩‍👧‍👦a", "bc"]
///   chunk("hi", 10) -> ["hi"]
///
pub fn chunk(text: String, size: Int) -> List(String) {
  case size <= 0 {
    True -> []
    False -> {
      let chars = string.to_graphemes(text)
      chunk_loop(chars, size, [])
    }
  }
}

fn chunk_loop(chars: List(String), size: Int, acc: List(String)) -> List(String) {
  case chars {
    [] -> list.reverse(acc)
    _ -> {
      let chunk_chars = list.take(chars, size)
      let rest = list.drop(chars, size)
      let chunk_str = string.concat(chunk_chars)
      chunk_loop(rest, size, [chunk_str, ..acc])
    }
  }
}

// ============================================================================
// LINE OPERATIONS
// ============================================================================

/// Splits text into lines.
/// Handles \n, \r\n, and \r line endings.
///
///   lines("a\nb\nc") -> ["a", "b", "c"]
///   lines("hello") -> ["hello"]
///   lines("a\r\nb") -> ["a", "b"]
///
pub fn lines(text: String) -> List(String) {
  text
  |> string.replace("\r\n", "\n")
  |> string.replace("\r", "\n")
  |> string.split("\n")
}

/// Removes common leading whitespace from all lines.
/// Useful for multiline string literals.
///
///   dedent("  a\n  b\n  c") -> "a\nb\nc"
///   dedent("    hello\n    world") -> "hello\nworld"
///
pub fn dedent(text: String) -> String {
  let text_lines = lines(text)

  // Find minimum indentation (ignoring empty lines)
  let min_indent =
    list.fold(text_lines, -1, fn(acc, line) {
      case string.trim(line) == "" {
        True -> acc
        False -> {
          let indent = count_leading_spaces(line)
          case acc == -1 || indent < acc {
            True -> indent
            False -> acc
          }
        }
      }
    })

  case min_indent <= 0 {
    True -> text
    False ->
      text_lines
      |> list.map(fn(line) {
        case string.trim(line) == "" {
          True -> ""
          False -> drop(line, min_indent)
        }
      })
      |> string.join("\n")
  }
}

/// Counts leading space characters in a string.
fn count_leading_spaces(text: String) -> Int {
  count_leading_spaces_loop(string.to_graphemes(text), 0)
}

fn count_leading_spaces_loop(chars: List(String), acc: Int) -> Int {
  case chars {
    [] -> acc
    [" ", ..rest] -> count_leading_spaces_loop(rest, acc + 1)
    ["\t", ..rest] -> count_leading_spaces_loop(rest, acc + 1)
    _ -> acc
  }
}

/// Adds indentation to each line.
///
///   indent("hello\nworld", 2) -> "  hello\n  world"
///   indent("hi", 4) -> "    hi"
///
pub fn indent(text: String, spaces: Int) -> String {
  let prefix = repeat_str(" ", spaces)
  text
  |> lines
  |> list.map(fn(line) { prefix <> line })
  |> string.join("\n")
}

// ============================================================================
// TEXT WRAPPING
// ============================================================================

/// Wraps text at the specified width, breaking on word boundaries.
///
///   wrap_at("hello world foo bar", 10) -> "hello\nworld foo\nbar"
///
pub fn wrap_at(text: String, width: Int) -> String {
  case width <= 0 {
    True -> text
    False -> {
      let text_words = words(text)
      wrap_words(text_words, width, "", 0)
    }
  }
}

fn wrap_words(
  remaining: List(String),
  width: Int,
  acc: String,
  line_len: Int,
) -> String {
  case remaining {
    [] -> acc
    [word, ..rest] -> {
      let word_len = grapheme_len(word)
      case line_len == 0 {
        True ->
          // First word on line
          wrap_words(rest, width, acc <> word, word_len)
        False ->
          case line_len + 1 + word_len <= width {
            True ->
              // Fits on current line
              wrap_words(
                rest,
                width,
                acc <> " " <> word,
                line_len + 1 + word_len,
              )
            False ->
              // Start new line
              wrap_words(rest, width, acc <> "\n" <> word, word_len)
          }
      }
    }
  }
}

/// Truncates text with ellipsis (…).
/// Convenience wrapper for truncate with single-character suffix.
///
///   ellipsis("Hello World", 8) -> "Hello W…"
///
pub fn ellipsis(text: String, max_len: Int) -> String {
  truncate(text, max_len, "…")
}

// ============================================================================
// CHARACTER STRIPPING
// ============================================================================

/// Removes specified characters from both ends of text.
///
///   strip("..hello..", ".") -> "hello"
///   strip("xxhelloxx", "x") -> "hello"
///
pub fn strip(text: String, chars: String) -> String {
  let char_set = string.to_graphemes(chars)
  text
  |> string.to_graphemes
  |> strip_leading(char_set)
  |> strip_trailing(char_set)
  |> string.concat
}

fn strip_leading(chars: List(String), to_remove: List(String)) -> List(String) {
  case chars {
    [] -> []
    [first, ..rest] ->
      case list.contains(to_remove, first) {
        True -> strip_leading(rest, to_remove)
        False -> chars
      }
  }
}

fn strip_trailing(chars: List(String), to_remove: List(String)) -> List(String) {
  chars
  |> list.reverse
  |> strip_leading(to_remove)
  |> list.reverse
}

/// Collapses consecutive occurrences of a character to a single instance.
///
///   squeeze("heeello", "e") -> "helo"
///   squeeze("mississippi", "s") -> "misisippi"
///   squeeze("   hello   world   ", " ") -> " hello world "
///
pub fn squeeze(text: String, char: String) -> String {
  let chars = string.to_graphemes(text)
  squeeze_loop(chars, char, False, [])
  |> list.reverse
  |> string.concat
}

fn squeeze_loop(
  chars: List(String),
  target: String,
  prev_was_target: Bool,
  acc: List(String),
) -> List(String) {
  case chars {
    [] -> acc
    [first, ..rest] ->
      case first == target {
        True ->
          case prev_was_target {
            True -> squeeze_loop(rest, target, True, acc)
            False -> squeeze_loop(rest, target, True, [first, ..acc])
          }
        False -> squeeze_loop(rest, target, False, [first, ..acc])
      }
  }
}

/// Removes trailing newline if present.
///
///   chomp("hello\n") -> "hello"
///   chomp("hello\r\n") -> "hello"
///   chomp("hello") -> "hello"
///
pub fn chomp(text: String) -> String {
  let chars = string.to_graphemes(text)
  let len = list.length(chars)

  case len == 0 {
    True -> text
    False -> {
      let last = list.last(chars)
      case last {
        Ok("\n") -> chars |> list.take(len - 1) |> string.concat
        Ok("\r") -> chars |> list.take(len - 1) |> string.concat
        Ok(g) -> {
          // Check if last grapheme is \r\n combined
          let cps = string.to_utf_codepoints(g)
          case cps {
            [cp1, cp2] -> {
              let c1 = string.utf_codepoint_to_int(cp1)
              let c2 = string.utf_codepoint_to_int(cp2)
              case c1 == 0x0D && c2 == 0x0A {
                True -> chars |> list.take(len - 1) |> string.concat
                False -> text
              }
            }
            _ -> text
          }
        }
        Error(_) -> text
      }
    }
  }
}

/// Normalizes whitespace: collapses multiple whitespace characters into single spaces.
/// Also trims leading and trailing whitespace.
///
///   normalize_whitespace("hello   world") -> "hello world"
///   normalize_whitespace("  foo  bar  baz  ") -> "foo bar baz"
///   normalize_whitespace("a\t\nb") -> "a b"
///
pub fn normalize_whitespace(text: String) -> String {
  text
  |> words
  |> string.join(" ")
}

// ============================================================================
// STRING PARTITIONING
// ============================================================================

/// Splits text into three parts: before, separator, and after.
///
///   partition("a-b-c", "-") -> #("a", "-", "b-c")
///   partition("hello", "-") -> #("hello", "", "")
///
pub fn partition(text: String, sep: String) -> #(String, String, String) {
  case string.split_once(text, sep) {
    Ok(#(before, after)) -> #(before, sep, after)
    Error(_) -> #(text, "", "")
  }
}

/// Splits text into three parts from the last occurrence of separator.
/// Returns #(before, separator, after). If separator not found, returns #("", "", text).
/// This mirrors Python's str.rpartition() behavior.
///
///   rpartition("a-b-c", "-") -> #("a-b", "-", "c")
///   rpartition("hello", "-") -> #("", "", "hello")
///   rpartition("one::two::three", "::") -> #("one::two", "::", "three")
///
pub fn rpartition(text: String, sep: String) -> #(String, String, String) {
  case last_index_of(text, sep) {
    Error(_) -> #("", "", text)
    Ok(idx) -> {
      let sep_len = grapheme_len(sep)
      let before = take(text, idx)
      let after = drop(text, idx + sep_len)
      #(before, sep, after)
    }
  }
}

/// Splits text into at most n parts.
/// The last part contains the remainder of the string.
///
///   splitn("a-b-c-d", "-", 2) -> ["a", "b-c-d"]
///   splitn("a-b-c-d", "-", 3) -> ["a", "b", "c-d"]
///   splitn("hello", "-", 5) -> ["hello"]
///   splitn("a-b-c", "-", 0) -> []
///
pub fn splitn(text: String, sep: String, n: Int) -> List(String) {
  case n <= 0 {
    True -> []
    False -> splitn_loop(text, sep, n, [])
  }
}

fn splitn_loop(
  text: String,
  sep: String,
  remaining: Int,
  acc: List(String),
) -> List(String) {
  case remaining <= 1 {
    True -> list.reverse([text, ..acc])
    False ->
      case string.split_once(text, sep) {
        Error(_) -> list.reverse([text, ..acc])
        Ok(#(before, after)) ->
          splitn_loop(after, sep, remaining - 1, [before, ..acc])
      }
  }
}

/// Finds the longest common prefix among a list of strings.
///
///   common_prefix(["abc", "abd", "abe"]) -> "ab"
///   common_prefix(["hello", "world"]) -> ""
///   common_prefix([]) -> ""
///
pub fn common_prefix(strings: List(String)) -> String {
  case strings {
    [] -> ""
    [single] -> single
    [first, ..rest] -> {
      let first_chars = string.to_graphemes(first)
      let others_chars = list.map(rest, string.to_graphemes)
      find_common_prefix(first_chars, others_chars, [])
      |> list.reverse
      |> string.concat
    }
  }
}

fn find_common_prefix(
  reference: List(String),
  others: List(List(String)),
  acc: List(String),
) -> List(String) {
  case reference {
    [] -> acc
    [char, ..rest_ref] -> {
      let all_have_char =
        list.all(others, fn(graphemes) {
          case graphemes {
            [] -> False
            [first, ..] -> first == char
          }
        })
      case all_have_char {
        False -> acc
        True -> {
          let new_others =
            list.map(others, fn(graphemes) { list.drop(graphemes, 1) })
          find_common_prefix(rest_ref, new_others, [char, ..acc])
        }
      }
    }
  }
}

/// Finds the longest common suffix among a list of strings.
///
///   common_suffix(["abc", "xbc", "zbc"]) -> "bc"
///   common_suffix(["hello", "world"]) -> ""
///
pub fn common_suffix(strings: List(String)) -> String {
  case strings {
    [] -> ""
    [single] -> single
    [first, ..rest] -> {
      let first_chars = string.to_graphemes(first) |> list.reverse
      let others_chars =
        list.map(rest, fn(s) { string.to_graphemes(s) |> list.reverse })
      find_common_prefix(first_chars, others_chars, [])
      |> string.concat
    }
  }
}

// ============================================================================
// CHARACTER TYPE CHECKS
// ============================================================================

/// Checks if text contains only ASCII digits (0-9).
///
///   is_numeric("12345") -> True
///   is_numeric("123.45") -> False
///   is_numeric("") -> False
///
pub fn is_numeric(text: String) -> Bool {
  case string.is_empty(text) {
    True -> False
    False ->
      text
      |> string.to_graphemes
      |> list.all(fn(g) {
        case string.to_utf_codepoints(g) {
          [cp] -> {
            let code = string.utf_codepoint_to_int(cp)
            code >= 0x30 && code <= 0x39
          }
          _ -> False
        }
      })
  }
}

/// Checks if text contains only ASCII letters (a-z, A-Z).
///
///   is_alpha("hello") -> True
///   is_alpha("Hello") -> True
///   is_alpha("hello123") -> False
///   is_alpha("") -> False
///
pub fn is_alpha(text: String) -> Bool {
  case string.is_empty(text) {
    True -> False
    False ->
      text
      |> string.to_graphemes
      |> list.all(fn(g) {
        case string.to_utf_codepoints(g) {
          [cp] -> {
            let code = string.utf_codepoint_to_int(cp)
            { code >= 0x41 && code <= 0x5A } || { code >= 0x61 && code <= 0x7A }
          }
          _ -> False
        }
      })
  }
}

/// Checks if text contains only ASCII letters and digits.
///
///   is_alphanumeric("hello123") -> True
///   is_alphanumeric("hello-world") -> False
///   is_alphanumeric("") -> False
///
pub fn is_alphanumeric(text: String) -> Bool {
  case string.is_empty(text) {
    True -> False
    False ->
      text
      |> string.to_graphemes
      |> list.all(fn(g) {
        case string.to_utf_codepoints(g) {
          [cp] -> {
            let code = string.utf_codepoint_to_int(cp)
            { code >= 0x30 && code <= 0x39 }
            || { code >= 0x41 && code <= 0x5A }
            || { code >= 0x61 && code <= 0x7A }
          }
          _ -> False
        }
      })
  }
}

// ============================================================================
// PREFIX/SUFFIX MANIPULATION
// ============================================================================

/// Removes prefix from text if present.
///
///   remove_prefix("hello world", "hello ") -> "world"
///   remove_prefix("hello", "bye") -> "hello"
///
pub fn remove_prefix(text: String, prefix: String) -> String {
  case string.starts_with(text, prefix) {
    True -> string.drop_start(text, string.length(prefix))
    False -> text
  }
}

/// Removes suffix from text if present.
///
///   remove_suffix("hello world", " world") -> "hello"
///   remove_suffix("hello", "bye") -> "hello"
///
pub fn remove_suffix(text: String, suffix: String) -> String {
  case string.ends_with(text, suffix) {
    True -> string.drop_end(text, string.length(suffix))
    False -> text
  }
}

/// Adds prefix if not already present.
///
///   ensure_prefix("world", "hello ") -> "hello world"
///   ensure_prefix("hello world", "hello ") -> "hello world"
///
pub fn ensure_prefix(text: String, prefix: String) -> String {
  case string.starts_with(text, prefix) {
    True -> text
    False -> prefix <> text
  }
}

/// Adds suffix if not already present.
///
///   ensure_suffix("hello", " world") -> "hello world"
///   ensure_suffix("hello world", " world") -> "hello world"
///
pub fn ensure_suffix(text: String, suffix: String) -> String {
  case string.ends_with(text, suffix) {
    True -> text
    False -> text <> suffix
  }
}

/// Checks if text starts with any of the given prefixes.
///
///   starts_with_any("hello", ["hi", "he", "ho"]) -> True
///   starts_with_any("hello", ["bye", "world"]) -> False
///   starts_with_any("test", []) -> False
///
pub fn starts_with_any(text: String, prefixes: List(String)) -> Bool {
  list.any(prefixes, fn(prefix) { string.starts_with(text, prefix) })
}

/// Checks if text ends with any of the given suffixes.
///
///   ends_with_any("hello.txt", [".txt", ".md", ".gleam"]) -> True
///   ends_with_any("hello", ["bye", "world"]) -> False
///   ends_with_any("test", []) -> False
///
pub fn ends_with_any(text: String, suffixes: List(String)) -> Bool {
  list.any(suffixes, fn(suffix) { string.ends_with(text, suffix) })
}

// ============================================================================
// CASE MANIPULATION
// ============================================================================

/// Swaps case of all ASCII letters.
///
///   swapcase("Hello World") -> "hELLO wORLD"
///   swapcase("ABC") -> "abc"
///
pub fn swapcase(text: String) -> String {
  text
  |> string.to_graphemes
  |> list.map(fn(g) {
    case string.to_utf_codepoints(g) {
      [cp] -> {
        let code = string.utf_codepoint_to_int(cp)
        case code >= 0x41 && code <= 0x5A {
          True -> string.lowercase(g)
          False ->
            case code >= 0x61 && code <= 0x7A {
              True -> string.uppercase(g)
              False -> g
            }
        }
      }
      _ -> g
    }
  })
  |> string.concat
}

/// Capitalizes text: first letter uppercase, rest lowercase.
///
///   capitalize("hello") -> "Hello"
///   capitalize("hELLO wORLD") -> "Hello world"
///   capitalize("") -> ""
///   capitalize("👋 hello") -> "👋 hello"
///
pub fn capitalize(text: String) -> String {
  case string.to_graphemes(text) {
    [] -> ""
    [first, ..rest] ->
      string.uppercase(first) <> string.concat(list.map(rest, string.lowercase))
  }
}

/// Reverses the order of words in text.
///
///   reverse_words("hello world") -> "world hello"
///   reverse_words("one two three") -> "three two one"
///   reverse_words("single") -> "single"
///
pub fn reverse_words(text: String) -> String {
  text
  |> words
  |> list.reverse
  |> string.join(" ")
}

/// Extracts initials from text (first letter of each word, uppercase).
///
///   initials("John Doe") -> "JD"
///   initials("visual studio code") -> "VSC"
///   initials("hello") -> "H"
///   initials("") -> ""
///
pub fn initials(text: String) -> String {
  text
  |> words
  |> list.filter_map(fn(word) {
    case string.to_graphemes(word) {
      [first, ..] -> Ok(string.uppercase(first))
      [] -> Error(Nil)
    }
  })
  |> string.concat
}

// ============================================================================
// STRING DISTANCE
// ============================================================================

/// Calculates Levenshtein distance between two strings.
/// Returns the minimum number of single-character edits (insertions,
/// deletions, substitutions) needed to transform one string into another.
///
///   distance("kitten", "sitting") -> 3
///   distance("hello", "hello") -> 0
///   distance("", "abc") -> 3
///
pub fn distance(a: String, b: String) -> Int {
  let a_chars = string.to_graphemes(a)
  let b_chars = string.to_graphemes(b)
  let a_len = list.length(a_chars)
  let b_len = list.length(b_chars)

  case a_len == 0 {
    True -> b_len
    False ->
      case b_len == 0 {
        True -> a_len
        False -> levenshtein(a_chars, b_chars, a_len, b_len)
      }
  }
}

/// Levenshtein distance using dynamic programming.
/// Uses a single row for space efficiency.
fn levenshtein(a: List(String), b: List(String), _a_len: Int, b_len: Int) -> Int {
  // Initialize first row: [0, 1, 2, ..., b_len]
  let initial_row = list.range(0, b_len)

  // Process each character of a
  let final_row =
    list.index_fold(a, initial_row, fn(prev_row, a_char, i) {
      compute_row(b, a_char, i + 1, prev_row)
    })

  // Result is the last element
  case list.last(final_row) {
    Ok(result) -> result
    Error(_) -> 0
  }
}

fn compute_row(
  b: List(String),
  a_char: String,
  i: Int,
  prev_row: List(Int),
) -> List(Int) {
  // First element of new row is i (distance from empty string)
  let initial = #([i], i, prev_row)

  let result =
    list.index_fold(b, initial, fn(state, b_char, _j) {
      let #(new_row, prev_val, prev_row_remaining) = state
      let above = case prev_row_remaining {
        [first, ..rest] -> #(first, rest)
        [] -> #(0, [])
      }
      let #(diag, rest) = above
      let left = case rest {
        [first, ..] -> first
        [] -> 0
      }

      let cost = case a_char == b_char {
        True -> 0
        False -> 1
      }

      let min_val = min3(left + 1, prev_val + 1, diag + cost)
      #([min_val, ..new_row], min_val, rest)
    })

  let #(new_row, _, _) = result
  list.reverse(new_row)
}

fn min3(a: Int, b: Int, c: Int) -> Int {
  case a <= b && a <= c {
    True -> a
    False ->
      case b <= c {
        True -> b
        False -> c
      }
  }
}

// ============================================================================
// SEARCH AND INDEX
// ============================================================================

/// Finds the index of the first occurrence of needle in text (grapheme-aware).
/// Returns Ok(index) if found, Error(Nil) if not found.
///
///   index_of("hello world", "world") -> Ok(6)
///   index_of("hello", "x") -> Error(Nil)
///   index_of("👨‍👩‍👧‍👦 family", "family") -> Ok(2)
///
pub fn index_of(text: String, needle: String) -> Result(Int, Nil) {
  let text_chars = string.to_graphemes(text)
  let needle_chars = string.to_graphemes(needle)
  let text_len = list.length(text_chars)
  let needle_len = list.length(needle_chars)

  case needle_len == 0 {
    True -> Error(Nil)
    False -> index_of_loop(text_chars, needle_chars, text_len, needle_len, 0)
  }
}

fn index_of_loop(
  text: List(String),
  needle: List(String),
  text_len: Int,
  needle_len: Int,
  index: Int,
) -> Result(Int, Nil) {
  case text_len < needle_len {
    True -> Error(Nil)
    False ->
      case list.take(text, needle_len) == needle {
        True -> Ok(index)
        False ->
          index_of_loop(
            list.drop(text, 1),
            needle,
            text_len - 1,
            needle_len,
            index + 1,
          )
      }
  }
}

/// Finds the index of the last occurrence of needle in text (grapheme-aware).
/// Returns Ok(index) if found, Error(Nil) if not found.
///
///   last_index_of("hello hello", "hello") -> Ok(6)
///   last_index_of("hello", "x") -> Error(Nil)
///   last_index_of("a-b-c", "-") -> Ok(3)
///
pub fn last_index_of(text: String, needle: String) -> Result(Int, Nil) {
  let text_chars = string.to_graphemes(text)
  let needle_chars = string.to_graphemes(needle)
  let text_len = list.length(text_chars)
  let needle_len = list.length(needle_chars)

  case needle_len == 0 {
    True -> Error(Nil)
    False ->
      last_index_of_loop(
        text_chars,
        needle_chars,
        text_len,
        needle_len,
        0,
        Error(Nil),
      )
  }
}

fn last_index_of_loop(
  text: List(String),
  needle: List(String),
  text_len: Int,
  needle_len: Int,
  index: Int,
  last_found: Result(Int, Nil),
) -> Result(Int, Nil) {
  case text_len < needle_len {
    True -> last_found
    False -> {
      let new_found = case list.take(text, needle_len) == needle {
        True -> Ok(index)
        False -> last_found
      }
      last_index_of_loop(
        list.drop(text, 1),
        needle,
        text_len - 1,
        needle_len,
        index + 1,
        new_found,
      )
    }
  }
}

/// Checks if text contains any of the given needles (grapheme-aware).
/// Uses grapheme-boundary matching for correct Unicode handling.
///
///   contains_any("hello world", ["foo", "world"]) -> True
///   contains_any("hello", ["x", "y", "z"]) -> False
///   contains_any("test", []) -> False
///   contains_any("👨‍👩‍👧‍👦 family", ["👨‍👩‍👧‍👦", "test"]) -> True
///
pub fn contains_any(text: String, needles: List(String)) -> Bool {
  list.any(needles, fn(needle) { contains(text, needle) })
}

/// Checks if text contains all of the given needles (grapheme-aware).
/// Uses grapheme-boundary matching for correct Unicode handling.
///
///   contains_all("hello world", ["hello", "world"]) -> True
///   contains_all("hello", ["hello", "x"]) -> False
///   contains_all("test", []) -> True
///   contains_all("👨‍👩‍👧‍👦 family", ["👨‍👩‍👧‍👦", "family"]) -> True
///
pub fn contains_all(text: String, needles: List(String)) -> Bool {
  list.all(needles, fn(needle) { contains(text, needle) })
}

/// Returns True if `needle` is found in `text` (grapheme-aware).
///
///   contains("hello world", "world") -> True
///   contains("hello", "x") -> False
///   contains("", "") -> False
pub fn contains(text: String, needle: String) -> Bool {
  case index_of(text, needle) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Returns True if `text` starts with `prefix` on grapheme boundaries.
///
///   starts_with("hello", "he") -> True
///   starts_with("hello", "") -> True
///   starts_with("hi", "hello") -> False
pub fn starts_with(text: String, prefix: String) -> Bool {
  let t = string.to_graphemes(text)
  let p = string.to_graphemes(prefix)
  let p_len = list.length(p)
  case p_len == 0 {
    True -> True
    False ->
      case list.length(t) < p_len {
        True -> False
        False -> list.take(t, p_len) == p
      }
  }
}

/// Returns True if `text` ends with `suffix` on grapheme boundaries.
///
///   ends_with("hello.txt", ".txt") -> True
///   ends_with("hello", "") -> True
///   ends_with("hi", "hello") -> False
pub fn ends_with(text: String, suffix: String) -> Bool {
  let t = string.to_graphemes(text)
  let s = string.to_graphemes(suffix)
  let s_len = list.length(s)
  let t_len = list.length(t)
  case s_len == 0 {
    True -> True
    False ->
      case s_len > t_len {
        True -> False
        False -> list.take(list.drop(t, t_len - s_len), s_len) == s
      }
  }
}

/// Returns True if `text` is an empty string.
///
///   is_empty("") -> True
///   is_empty(" ") -> False
pub fn is_empty(text: String) -> Bool {
  text == ""
}

// ============================================================================
// REPLACEMENT VARIANTS
// ============================================================================

/// Replaces only the first occurrence of old with new.
///
///   replace_first("hello hello", "hello", "hi") -> "hi hello"
///   replace_first("aaa", "a", "b") -> "baa"
///   replace_first("test", "x", "y") -> "test"
///
pub fn replace_first(text: String, old: String, new: String) -> String {
  case string.split_once(text, old) {
    Ok(#(before, after)) -> before <> new <> after
    Error(_) -> text
  }
}

/// Replaces only the last occurrence of old with new.
///
///   replace_last("hello hello", "hello", "hi") -> "hello hi"
///   replace_last("aaa", "a", "b") -> "aab"
///   replace_last("test", "x", "y") -> "test"
///
pub fn replace_last(text: String, old: String, new: String) -> String {
  case last_index_of(text, old) {
    Error(_) -> text
    Ok(idx) -> {
      let old_len = grapheme_len(old)
      take(text, idx) <> new <> drop(text, idx + old_len)
    }
  }
}

// ============================================================================
// VALIDATION FUNCTIONS
// ============================================================================

/// Checks if all cased characters in text are uppercase.
/// Non-cased characters (numbers, symbols) are ignored.
/// Returns False for empty strings or strings with no cased characters.
///
///   is_uppercase("HELLO") -> True
///   is_uppercase("Hello") -> False
///   is_uppercase("HELLO123") -> True
///   is_uppercase("123") -> False
///   is_uppercase("") -> False
///
pub fn is_uppercase(text: String) -> Bool {
  let chars = string.to_graphemes(text)
  let cased_chars = list.filter(chars, is_grapheme_cased)
  case list.is_empty(cased_chars) {
    True -> False
    False -> list.all(cased_chars, is_grapheme_uppercase)
  }
}

/// Checks if all cased characters in text are lowercase.
/// Non-cased characters (numbers, symbols) are ignored.
/// Returns False for empty strings or strings with no cased characters.
///
///   is_lowercase("hello") -> True
///   is_lowercase("Hello") -> False
///   is_lowercase("hello123") -> True
///   is_lowercase("123") -> False
///   is_lowercase("") -> False
///
pub fn is_lowercase(text: String) -> Bool {
  let chars = string.to_graphemes(text)
  let cased_chars = list.filter(chars, is_grapheme_cased)
  case list.is_empty(cased_chars) {
    True -> False
    False -> list.all(cased_chars, is_grapheme_lowercase)
  }
}

/// Checks if text is in Title Case (first letter of each word is uppercase).
/// Non-alphabetic characters are ignored. Empty strings return False.
///
///   is_title_case("Hello World") -> True
///   is_title_case("Hello world") -> False
///   is_title_case("HELLO WORLD") -> False
///   is_title_case("Hello") -> True
///   is_title_case("") -> False
///
pub fn is_title_case(text: String) -> Bool {
  let text_words = words(text)
  let cased_words =
    list.filter(text_words, fn(word) {
      case string.to_graphemes(word) {
        [first, ..] -> is_grapheme_cased(first)
        [] -> False
      }
    })
  case list.is_empty(cased_words) {
    True -> False
    False ->
      list.all(cased_words, fn(word) {
        case string.to_graphemes(word) {
          [] -> True
          [first, ..rest] -> {
            let first_is_upper = is_grapheme_uppercase(first)
            let rest_are_lower =
              list.all(rest, fn(g) {
                case is_grapheme_cased(g) {
                  False -> True
                  True -> is_grapheme_lowercase(g)
                }
              })
            first_is_upper && rest_are_lower
          }
        }
      })
  }
}

/// Checks if text contains only ASCII characters (0x00-0x7F).
///
///   is_ascii("hello") -> True
///   is_ascii("hello!@#") -> True
///   is_ascii("café") -> False
///   is_ascii("👋") -> False
///   is_ascii("") -> True
///
pub fn is_ascii(text: String) -> Bool {
  text
  |> string.to_graphemes
  |> list.all(fn(g) {
    case string.to_utf_codepoints(g) {
      [cp] -> {
        let code = string.utf_codepoint_to_int(cp)
        code >= 0x00 && code <= 0x7F
      }
      _ -> False
    }
  })
}

/// Checks if text contains only printable ASCII characters (0x20-0x7E).
///
///   is_printable("hello") -> True
///   is_printable("hello\n") -> False
///   is_printable("hello\t") -> False
///   is_printable("") -> True
///
pub fn is_printable(text: String) -> Bool {
  text
  |> string.to_graphemes
  |> list.all(fn(g) {
    case string.to_utf_codepoints(g) {
      [cp] -> {
        let code = string.utf_codepoint_to_int(cp)
        code >= 0x20 && code <= 0x7E
      }
      _ -> False
    }
  })
}

/// Checks if text contains only hexadecimal characters (0-9, a-f, A-F).
///
///   is_hex("abc123") -> True
///   is_hex("DEADBEEF") -> True
///   is_hex("xyz") -> False
///   is_hex("") -> False
///
pub fn is_hex(text: String) -> Bool {
  case string.is_empty(text) {
    True -> False
    False ->
      text
      |> string.to_graphemes
      |> list.all(fn(g) {
        case string.to_utf_codepoints(g) {
          [cp] -> {
            let code = string.utf_codepoint_to_int(cp)
            { code >= 0x30 && code <= 0x39 }
            || { code >= 0x41 && code <= 0x46 }
            || { code >= 0x61 && code <= 0x66 }
          }
          _ -> False
        }
      })
  }
}

// ============================================================================
// HTML ESCAPING
// ============================================================================

/// Escapes HTML special characters to their entity equivalents.
/// Escapes: & < > " '
///
///   escape_html("<div>Hello</div>") -> "&lt;div&gt;Hello&lt;/div&gt;"
///   escape_html("Tom & Jerry") -> "Tom &amp; Jerry"
///   escape_html("Say \"hello\"") -> "Say &quot;hello&quot;"
///
pub fn escape_html(text: String) -> String {
  text
  |> string.replace("&", "&amp;")
  |> string.replace("<", "&lt;")
  |> string.replace(">", "&gt;")
  |> string.replace("\"", "&quot;")
  |> string.replace("'", "&#39;")
}

/// Unescapes HTML entities to their character equivalents.
/// Handles: &amp; &lt; &gt; &quot; &#39;
///
///   unescape_html("&lt;div&gt;") -> "<div>"
///   unescape_html("Tom &amp; Jerry") -> "Tom & Jerry"
///
pub fn unescape_html(text: String) -> String {
  text
  |> string.replace("&#39;", "'")
  |> string.replace("&quot;", "\"")
  |> string.replace("&gt;", ">")
  |> string.replace("&lt;", "<")
  |> string.replace("&amp;", "&")
}

/// Escapes regex metacharacters so the string can be used as a literal pattern.
/// Escapes: \ ^ $ . | ? * + ( ) [ ] { }
///
///   escape_regex("hello.world") -> "hello\\.world"
///   escape_regex("[test]") -> "\\[test\\]"
///   escape_regex("a+b*c?") -> "a\\+b\\*c\\?"
///
pub fn escape_regex(text: String) -> String {
  text
  |> string.replace("\\", "\\\\")
  |> string.replace("^", "\\^")
  |> string.replace("$", "\\$")
  |> string.replace(".", "\\.")
  |> string.replace("|", "\\|")
  |> string.replace("?", "\\?")
  |> string.replace("*", "\\*")
  |> string.replace("+", "\\+")
  |> string.replace("(", "\\(")
  |> string.replace(")", "\\)")
  |> string.replace("[", "\\[")
  |> string.replace("]", "\\]")
  |> string.replace("{", "\\{")
  |> string.replace("}", "\\}")
}

// ============================================================================
// SIMILARITY
// ============================================================================

/// Calculates similarity as a percentage (0.0 to 1.0) based on Levenshtein distance.
/// Returns 1.0 for identical strings, 0.0 for completely different strings.
///
///   similarity("hello", "hello") -> 1.0
///   similarity("hello", "hallo") -> 0.8
///   similarity("abc", "xyz") -> 0.0
///   similarity("", "") -> 1.0
///
pub fn similarity(a: String, b: String) -> Float {
  let a_len = grapheme_len(a)
  let b_len = grapheme_len(b)
  let max_len = case a_len > b_len {
    True -> a_len
    False -> b_len
  }
  case max_len == 0 {
    True -> 1.0
    False -> {
      let dist = distance(a, b)
      let diff = int.to_float(max_len - dist)
      let max_f = int.to_float(max_len)
      diff /. max_f
    }
  }
}

/// Calculates Hamming distance between two strings of equal length.
/// Returns the number of positions where the corresponding graphemes differ.
/// Returns Error(Nil) if strings have different lengths.
///
///   hamming_distance("karolin", "kathrin") -> Ok(3)
///   hamming_distance("hello", "hallo") -> Ok(1)
///   hamming_distance("abc", "ab") -> Error(Nil)
///
pub fn hamming_distance(a: String, b: String) -> Result(Int, Nil) {
  let a_chars = string.to_graphemes(a)
  let b_chars = string.to_graphemes(b)
  case list.length(a_chars) == list.length(b_chars) {
    False -> Error(Nil)
    True -> Ok(hamming_loop(a_chars, b_chars, 0))
  }
}

fn hamming_loop(a: List(String), b: List(String), acc: Int) -> Int {
  case a, b {
    [], [] -> acc
    [a_head, ..a_rest], [b_head, ..b_rest] ->
      case a_head == b_head {
        True -> hamming_loop(a_rest, b_rest, acc)
        False -> hamming_loop(a_rest, b_rest, acc + 1)
      }
    _, _ -> acc
  }
}

// ============================================================================
// PADDING OPERATIONS
// ============================================================================

/// Position for the fill function.
pub type FillPosition {
  Left
  Right
  Both
}

/// Fills text to reach width by adding pad characters.
/// Position specifies where to add padding: Left, Right, or Both (center).
///
///   fill("42", 5, "0", Left) -> "00042"
///   fill("hi", 6, "*", Right) -> "hi****"
///   fill("x", 5, "-", Both) -> "--x--"
///
pub fn fill(
  text: String,
  width: Int,
  pad: String,
  position: FillPosition,
) -> String {
  case position {
    Left -> pad_left(text, width, pad)
    Right -> pad_right(text, width, pad)
    Both -> center(text, width, pad)
  }
}
