//// Unicode-aware string utilities for Gleam.
////
//// This module re-exports the most commonly used functions from `str/core`
//// and `str/extra` for convenient access. For the full API, import the
//// submodules directly.
////
//// ## Quick Start
////
////     import str
////
////     str.truncate("Hello 👨‍👩‍👧‍👦 World", 10, "...")  // "Hello 👨‍👩‍👧‍👦..."
////     str.slugify("Crème Brûlée")                 // "creme-brulee"
////     str.similarity("hello", "hallo")           // 0.8
////

// Re-export core functions
import str/core
import str/extra

// Re-export types
pub type FillPosition =
  core.FillPosition

// ============================================================================
// GRAPHEME OPERATIONS
// ============================================================================

/// Returns the first N grapheme clusters from text.
pub fn take(text: String, n: Int) -> String {
  core.take(text, n)
}

/// Drops the first N grapheme clusters from text.
pub fn drop(text: String, n: Int) -> String {
  core.drop(text, n)
}

/// Returns the grapheme cluster at the given index (0-based).
pub fn at(text: String, index: Int) -> Result(String, Nil) {
  core.at(text, index)
}

/// Reverses text at grapheme cluster boundaries.
pub fn reverse(text: String) -> String {
  core.reverse(text)
}

// ============================================================================
// TRUNCATION
// ============================================================================

/// Truncates text to max_len graphemes, preserving emoji sequences.
pub fn truncate(text: String, max_len: Int, suffix: String) -> String {
  core.truncate(text, max_len, suffix)
}

/// Truncates text with ellipsis (…).
pub fn ellipsis(text: String, max_len: Int) -> String {
  core.ellipsis(text, max_len)
}

// ============================================================================
// PADDING
// ============================================================================

/// Pads text on the left to reach the specified width.
pub fn pad_left(text: String, width: Int, pad: String) -> String {
  core.pad_left(text, width, pad)
}

/// Pads text on the right to reach the specified width.
pub fn pad_right(text: String, width: Int, pad: String) -> String {
  core.pad_right(text, width, pad)
}

/// Centers text within the specified width.
pub fn center(text: String, width: Int, pad: String) -> String {
  core.center(text, width, pad)
}

// ============================================================================
// SEARCH & REPLACE
// ============================================================================

/// Finds the index of the first occurrence of needle (grapheme-aware).
pub fn index_of(text: String, needle: String) -> Result(Int, Nil) {
  core.index_of(text, needle)
}

/// Finds the index of the last occurrence of needle.
pub fn last_index_of(text: String, needle: String) -> Result(Int, Nil) {
  core.last_index_of(text, needle)
}

/// Replaces only the first occurrence of old with new.
pub fn replace_first(text: String, old: String, new: String) -> String {
  core.replace_first(text, old, new)
}

/// Replaces only the last occurrence of old with new.
pub fn replace_last(text: String, old: String, new: String) -> String {
  core.replace_last(text, old, new)
}

// ============================================================================
// VALIDATION
// ============================================================================

/// Checks if a string contains only whitespace.
pub fn is_blank(text: String) -> Bool {
  core.is_blank(text)
}

/// Checks if all cased characters are uppercase.
pub fn is_uppercase(text: String) -> Bool {
  core.is_uppercase(text)
}

/// Checks if all cased characters are lowercase.
pub fn is_lowercase(text: String) -> Bool {
  core.is_lowercase(text)
}

/// Checks if text is in Title Case format.
pub fn is_title_case(text: String) -> Bool {
  core.is_title_case(text)
}

/// Checks if text contains only ASCII characters.
pub fn is_ascii(text: String) -> Bool {
  core.is_ascii(text)
}

// ============================================================================
// SIMILARITY
// ============================================================================

/// Calculates Levenshtein distance between two strings.
pub fn distance(a: String, b: String) -> Int {
  core.distance(a, b)
}

/// Calculates similarity as a percentage (0.0 to 1.0).
pub fn similarity(a: String, b: String) -> Float {
  core.similarity(a, b)
}

// ============================================================================
// TEXT MANIPULATION
// ============================================================================

/// Splits text into words by whitespace.
pub fn words(text: String) -> List(String) {
  core.words(text)
}

/// Splits text into lines.
pub fn lines(text: String) -> List(String) {
  core.lines(text)
}

/// Capitalizes text: first letter uppercase, rest lowercase.
pub fn capitalize(text: String) -> String {
  core.capitalize(text)
}

/// Normalizes whitespace: collapses to single spaces and trims.
pub fn normalize_whitespace(text: String) -> String {
  core.normalize_whitespace(text)
}

// ============================================================================
// HTML ESCAPING
// ============================================================================

/// Escapes HTML special characters.
pub fn escape_html(text: String) -> String {
  core.escape_html(text)
}

/// Unescapes HTML entities.
pub fn unescape_html(text: String) -> String {
  core.unescape_html(text)
}

// ============================================================================
// SLUG & CASE CONVERSION (from str/extra)
// ============================================================================

/// Creates a URL-friendly slug from text.
pub fn slugify(text: String) -> String {
  extra.slugify(text)
}

/// Converts text to ASCII equivalents.
pub fn ascii_fold(text: String) -> String {
  extra.ascii_fold(text)
}

/// Converts text to snake_case.
pub fn to_snake_case(text: String) -> String {
  extra.to_snake_case(text)
}

/// Converts text to camelCase.
pub fn to_camel_case(text: String) -> String {
  extra.to_camel_case(text)
}

/// Converts text to PascalCase.
pub fn to_pascal_case(text: String) -> String {
  extra.to_pascal_case(text)
}

/// Converts text to kebab-case.
pub fn to_kebab_case(text: String) -> String {
  extra.to_kebab_case(text)
}

/// Converts text to Title Case.
pub fn to_title_case(text: String) -> String {
  extra.to_title_case(text)
}
