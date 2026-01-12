//// Unicode-aware string utilities for Gleam.
////
//// This is the main public API for the `str` library. All functionality is
//// re-exported from internal modules for your convenience.
////
//// ## Quick Start
////
////     import str
////
////     str.truncate("Hello 👨‍👩‍👧‍👦 World", 10, "...")  // "Hello 👨‍👩‍👧‍👦..."
////     str.slugify("Crème Brûlée")                 // "creme-brulee"
////     str.similarity("hello", "hallo")           // 0.8
////
//// ## Core Functions
////
//// - **Grapheme operations**: `take`, `drop`, `at`, `reverse`, `length`
//// - **Truncation**: `truncate`, `ellipsis`, `truncate_strict`, `truncate_preserve`
//// - **Padding**: `pad_left`, `pad_right`, `center`
//// - **Search**: `index_of`, `last_index_of`, `contains`, `starts_with`, `ends_with`
//// - **Validation**: `is_blank`, `is_empty`, `is_ascii`, `is_uppercase`, etc.
//// - **Similarity**: `distance`, `similarity`, `hamming_distance`
//// - **Text manipulation**: `words`, `lines`, `capitalize`, `normalize_whitespace`
//// - **HTML**: `escape_html`, `unescape_html`
////
//// ## Extra Functions
////
//// - **Slugification**: `slugify`, `slugify_with_options`
//// - **ASCII folding**: `ascii_fold`, `ascii_fold_no_decompose`
//// - **Case conversion**: `to_snake_case`, `to_camel_case`, `to_pascal_case`, 
////   `to_kebab_case`, `to_title_case`
////
//// ## Advanced Features
////
//// - **Search strategies**: KMP and sliding window algorithms with automatic selection
//// - **Grapheme tokenization**: Pure Gleam Unicode segmentation
//// - **Configuration**: Customizable search heuristics via `str/config`
////
//// ## Note on Internal Modules
////
//// The `str/internal/*` modules are implementation details and should not be
//// imported directly. They may change without notice between minor versions.
//// Always use `import str` for stable public API.

import gleam/dict
import str/config
import str/internal/core
import str/internal/extra
import str/internal/tokenize
import str/internal/decompose as internal_decompose

// ============================================================================
// RE-EXPORT TYPES
// ============================================================================

/// Position for fill operations.
pub type FillPosition {
  Left
  Right
  Both
}

/// Search strategy selection (automatic, KMP, or sliding window).
pub type SearchStrategy {
  Sliding
  Kmp
}

// ============================================================================
// GRAPHEME OPERATIONS
// ============================================================================

/// Returns the first N grapheme clusters from text.
///
/// ## Examples
///
/// ```gleam
/// take("Hello 👨‍👩‍👧‍👦", 6)
/// // -> "Hello "
/// ```
pub fn take(text: String, n: Int) -> String {
  core.take(text, n)
}

/// Drops the first N grapheme clusters from text.
///
/// ## Examples
///
/// ```gleam
/// drop("Hello World", 6)
/// // -> "World"
/// ```
pub fn drop(text: String, n: Int) -> String {
  core.drop(text, n)
}

/// Returns the grapheme cluster at the given index (0-based).
///
/// ## Examples
///
/// ```gleam
/// at("Hello", 1)
/// // -> Ok("e")
/// ```
pub fn at(text: String, index: Int) -> Result(String, Nil) {
  core.at(text, index)
}

/// Reverses text at grapheme cluster boundaries.
///
/// ## Examples
///
/// ```gleam
/// reverse("Hello 👋")
/// // -> "👋 olleH"
/// ```
pub fn reverse(text: String) -> String {
  core.reverse(text)
}

/// Returns the number of grapheme clusters in text.
///
/// ## Examples
///
/// ```gleam
/// length("Hello 👨‍👩‍👧‍👦")
/// // -> 7
/// ```
pub fn length(text: String) -> Int {
  core.length(text)
}

/// Returns the last N grapheme clusters from text.
pub fn take_right(text: String, n: Int) -> String {
  core.take_right(text, n)
}

/// Drops the last N grapheme clusters from text.
pub fn drop_right(text: String, n: Int) -> String {
  core.drop_right(text, n)
}

/// Splits text into chunks of the specified size.
pub fn chunk(text: String, size: Int) -> List(String) {
  core.chunk(text, size)
}

// ============================================================================
// TRUNCATION
// ============================================================================

/// Truncates text to max_len graphemes, adding suffix if truncated.
///
/// ## Examples
///
/// ```gleam
/// truncate("Hello World", 8, "...")
/// // -> "Hello..."
/// ```
pub fn truncate(text: String, max_len: Int, suffix: String) -> String {
  core.truncate(text, max_len, suffix)
}

/// Truncates text with ellipsis (…).
pub fn ellipsis(text: String, max_len: Int) -> String {
  core.ellipsis(text, max_len)
}

/// Truncates strictly at max_len, even if it breaks emoji sequences.
pub fn truncate_strict(text: String, max_len: Int, suffix: String) -> String {
  core.truncate_strict(text, max_len, suffix)
}

/// Truncates preserving emoji sequences, may exceed max_len slightly.
pub fn truncate_preserve(text: String, max_len: Int, suffix: String) -> String {
  core.truncate_preserve(text, max_len, suffix)
}

/// Truncates to max_len with empty suffix.
pub fn truncate_default(text: String, max_len: Int) -> String {
  core.truncate_default(text, max_len)
}

/// Truncates with emoji handling control.
pub fn truncate_with_flag(
  text: String,
  max_len: Int,
  suffix: String,
  keep_whole_emoji: Bool,
) -> String {
  core.truncate_with_flag(text, max_len, suffix, keep_whole_emoji)
}

// ============================================================================
// PADDING & ALIGNMENT
// ============================================================================

/// Pads text on the left to reach the specified width.
///
/// ## Examples
///
/// ```gleam
/// pad_left("Hi", 5, " ")
/// // -> "   Hi"
/// ```
pub fn pad_left(text: String, width: Int, pad: String) -> String {
  core.pad_left(text, width, pad)
}

/// Pads text on the right to reach the specified width.
pub fn pad_right(text: String, width: Int, pad: String) -> String {
  core.pad_right(text, width, pad)
}

/// Centers text within the specified width.
///
/// ## Examples
///
/// ```gleam
/// center("Hi", 6, " ")
/// // -> "  Hi  "
/// ```
pub fn center(text: String, width: Int, pad: String) -> String {
  core.center(text, width, pad)
}

/// Fills text to specified width with padding at position.
pub fn fill(
  text: String,
  width: Int,
  pad: String,
  position: FillPosition,
) -> String {
  let pos = case position {
    Left -> core.Left
    Right -> core.Right
    Both -> core.Both
  }
  core.fill(text, width, pad, pos)
}

// ============================================================================
// SEARCH & MATCHING
// ============================================================================

/// Finds the index of the first occurrence of needle (grapheme-aware).
///
/// ## Examples
///
/// ```gleam
/// index_of("Hello World", "World")
/// // -> Ok(6)
/// ```
pub fn index_of(text: String, needle: String) -> Result(Int, Nil) {
  core.index_of(text, needle)
}

/// Simple (direct) index algorithm — stable, straightforward implementation.
/// Use `index_of_auto` for heuristic/optimized selection.
pub fn index_of_simple(text: String, needle: String) -> Result(Int, Nil) {
  core.index_of(text, needle)
}

/// Finds the index of the last occurrence of needle.
pub fn last_index_of(text: String, needle: String) -> Result(Int, Nil) {
  core.last_index_of(text, needle)
}

/// Returns True if needle is found in text (grapheme-aware).
pub fn contains(text: String, needle: String) -> Bool {
  core.contains(text, needle)
}

/// Returns True if text starts with prefix on grapheme boundaries.
pub fn starts_with(text: String, prefix: String) -> Bool {
  core.starts_with(text, prefix)
}

/// Returns True if text ends with suffix on grapheme boundaries.
pub fn ends_with(text: String, suffix: String) -> Bool {
  core.ends_with(text, suffix)
}

/// Returns True if text is an empty string.
pub fn is_empty(text: String) -> Bool {
  core.is_empty(text)
}

/// Returns True if any of the needles appear in text.
pub fn contains_any(text: String, needles: List(String)) -> Bool {
  core.contains_any(text, needles)
}

/// Returns True if all of the needles appear in text.
pub fn contains_all(text: String, needles: List(String)) -> Bool {
  core.contains_all(text, needles)
}

/// Returns True if text starts with any of the prefixes.
pub fn starts_with_any(text: String, prefixes: List(String)) -> Bool {
  core.starts_with_any(text, prefixes)
}

/// Returns True if text ends with any of the suffixes.
pub fn ends_with_any(text: String, suffixes: List(String)) -> Bool {
  core.ends_with_any(text, suffixes)
}

/// Counts occurrences of needle in haystack.
pub fn count(haystack: String, needle: String, overlapping: Bool) -> Int {
  core.count(haystack, needle, overlapping)
}

/// Simple (direct) count algorithm — stable, straightforward implementation.
/// Use `count_auto` for heuristic/optimized selection.
pub fn count_simple(haystack: String, needle: String, overlapping: Bool) -> Int {
  core.count(haystack, needle, overlapping)
}

/// Automatic search strategy selection for index_of.
pub fn index_of_auto(text: String, needle: String) -> Result(Int, Nil) {
  core.index_of_auto(text, needle)
}

/// Automatic search strategy selection for count.
pub fn count_auto(haystack: String, needle: String, overlapping: Bool) -> Int {
  core.count_auto(haystack, needle, overlapping)
}

/// Search with explicit strategy selection.
pub fn index_of_strategy(
  text: String,
  needle: String,
  strategy: SearchStrategy,
) -> Result(Int, Nil) {
  let core_strategy = case strategy {
    Sliding -> core.Sliding
    Kmp -> core.Kmp
  }
  core.index_of_strategy(text, needle, core_strategy)
}

/// Count with explicit strategy selection.
pub fn count_strategy(
  haystack: String,
  needle: String,
  overlapping: Bool,
  strategy: SearchStrategy,
) -> Int {
  let core_strategy = case strategy {
    Sliding -> core.Sliding
    Kmp -> core.Kmp
  }
  core.count_strategy(haystack, needle, overlapping, core_strategy)
}

// ============================================================================
// STRING MANIPULATION
// ============================================================================

/// Replaces only the first occurrence of old with new.
pub fn replace_first(text: String, old: String, new: String) -> String {
  core.replace_first(text, old, new)
}

/// Replaces only the last occurrence of old with new.
pub fn replace_last(text: String, old: String, new: String) -> String {
  core.replace_last(text, old, new)
}

/// Adds prefix and suffix to text.
pub fn surround(text: String, prefix: String, suffix: String) -> String {
  core.surround(text, prefix, suffix)
}

/// Removes prefix and suffix from text if present.
pub fn unwrap(text: String, prefix: String, suffix: String) -> String {
  core.unwrap(text, prefix, suffix)
}

/// Removes prefix from text if present.
pub fn remove_prefix(text: String, prefix: String) -> String {
  core.remove_prefix(text, prefix)
}

/// Removes suffix from text if present.
pub fn remove_suffix(text: String, suffix: String) -> String {
  core.remove_suffix(text, suffix)
}

/// Ensures text starts with prefix.
pub fn ensure_prefix(text: String, prefix: String) -> String {
  core.ensure_prefix(text, prefix)
}

/// Ensures text ends with suffix.
pub fn ensure_suffix(text: String, suffix: String) -> String {
  core.ensure_suffix(text, suffix)
}

/// Strips specified characters from both ends.
pub fn strip(text: String, chars: String) -> String {
  core.strip(text, chars)
}

/// Reduces consecutive occurrences of char to a single occurrence.
pub fn squeeze(text: String, char: String) -> String {
  core.squeeze(text, char)
}

/// Removes trailing newline from text.
pub fn chomp(text: String) -> String {
  core.chomp(text)
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

/// Checks if text contains only numeric characters.
pub fn is_numeric(text: String) -> Bool {
  core.is_numeric(text)
}

/// Checks if text contains only alphabetic characters.
pub fn is_alpha(text: String) -> Bool {
  core.is_alpha(text)
}

/// Checks if text contains only alphanumeric characters.
pub fn is_alphanumeric(text: String) -> Bool {
  core.is_alphanumeric(text)
}

/// Checks if text contains only printable characters.
pub fn is_printable(text: String) -> Bool {
  core.is_printable(text)
}

/// Checks if text is a valid hexadecimal string.
pub fn is_hex(text: String) -> Bool {
  core.is_hex(text)
}

// ============================================================================
// SIMILARITY & DISTANCE
// ============================================================================

/// Calculates Levenshtein distance between two strings.
///
/// ## Examples
///
/// ```gleam
/// distance("kitten", "sitting")
/// // -> 3
/// ```
pub fn distance(a: String, b: String) -> Int {
  core.distance(a, b)
}

/// Calculates similarity as a percentage (0.0 to 1.0).
///
/// ## Examples
///
/// ```gleam
/// similarity("hello", "hallo")
/// // -> 0.8
/// ```
pub fn similarity(a: String, b: String) -> Float {
  core.similarity(a, b)
}

/// Calculates Hamming distance between strings of equal length.
pub fn hamming_distance(a: String, b: String) -> Result(Int, Nil) {
  core.hamming_distance(a, b)
}

// ============================================================================
// TEXT MANIPULATION & FORMATTING
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
///
/// ## Examples
///
/// ```gleam
/// capitalize("hello WORLD")
/// // -> "Hello world"
/// ```
pub fn capitalize(text: String) -> String {
  core.capitalize(text)
}

/// Swaps case of all characters.
pub fn swapcase(text: String) -> String {
  core.swapcase(text)
}

/// Normalizes whitespace: collapses to single spaces and trims.
pub fn normalize_whitespace(text: String) -> String {
  core.normalize_whitespace(text)
}

/// Removes common leading whitespace from all lines.
pub fn dedent(text: String) -> String {
  core.dedent(text)
}

/// Adds indentation to each line.
pub fn indent(text: String, spaces: Int) -> String {
  core.indent(text, spaces)
}

/// Wraps text at specified width.
pub fn wrap_at(text: String, width: Int) -> String {
  core.wrap_at(text, width)
}

/// Reverses the order of words in text.
pub fn reverse_words(text: String) -> String {
  core.reverse_words(text)
}

/// Extracts initials from text.
pub fn initials(text: String) -> String {
  core.initials(text)
}

/// Splits text at separator, returning before, separator, and after.
pub fn partition(text: String, sep: String) -> #(String, String, String) {
  core.partition(text, sep)
}

/// Splits text at last occurrence of separator.
pub fn rpartition(text: String, sep: String) -> #(String, String, String) {
  core.rpartition(text, sep)
}

/// Splits text into at most n parts.
pub fn splitn(text: String, sep: String, n: Int) -> List(String) {
  core.splitn(text, sep, n)
}

/// Finds common prefix of all strings.
pub fn common_prefix(strings: List(String)) -> String {
  core.common_prefix(strings)
}

/// Finds common suffix of all strings.
pub fn common_suffix(strings: List(String)) -> String {
  core.common_suffix(strings)
}

// ============================================================================
// HTML & ESCAPING
// ============================================================================

/// Escapes HTML special characters.
///
/// ## Examples
///
/// ```gleam
/// escape_html("<div>Hello & goodbye</div>")
/// // -> "&lt;div&gt;Hello &amp; goodbye&lt;/div&gt;"
/// ```
pub fn escape_html(text: String) -> String {
  core.escape_html(text)
}

/// Unescapes HTML entities.
pub fn unescape_html(text: String) -> String {
  core.unescape_html(text)
}

/// Escapes special regex characters.
pub fn escape_regex(text: String) -> String {
  core.escape_regex(text)
}

// ============================================================================
// ADVANCED SEARCH ALGORITHMS
// ============================================================================

/// Builds KMP prefix table for pattern.
pub fn build_prefix_table(pattern: String) -> List(Int) {
  core.build_prefix_table(pattern)
}

/// Finds all occurrences using KMP algorithm.
pub fn kmp_search_all(text: String, pattern: String) -> List(Int) {
  core.kmp_search_all(text, pattern)
}

/// Builds optimized KMP lookup maps.
pub fn build_kmp_maps(
  pattern: String,
) -> #(dict.Dict(Int, String), dict.Dict(Int, Int)) {
  core.build_kmp_maps(pattern)
}

/// KMP search with pre-built maps for better performance.
pub fn kmp_search_all_with_maps(
  text: String,
  pmap: dict.Dict(Int, String),
  pimap: dict.Dict(Int, Int),
) -> List(Int) {
  core.kmp_search_all_with_maps(text, pmap, pimap)
}

/// Finds all occurrences using sliding window algorithm.
pub fn sliding_search_all(text: String, pattern: String) -> List(Int) {
  core.sliding_search_all(text, pattern)
}

/// Sliding window search for first occurrence.
pub fn sliding_index_of(text: String, pattern: String) -> Result(Int, Nil) {
  core.sliding_index_of(text, pattern)
}

/// KMP search for first occurrence.
pub fn kmp_index_of(text: String, pattern: String) -> Result(Int, Nil) {
  core.kmp_index_of(text, pattern)
}

/// KMP search with pre-built maps.
pub fn kmp_index_of_with_maps(
  text: String,
  pattern: String,
  pmap: dict.Dict(Int, String),
  pimap: dict.Dict(Int, Int),
) -> Result(Int, Nil) {
  core.kmp_index_of_with_maps(text, pattern, pmap, pimap)
}

/// Chooses optimal search strategy based on input.
pub fn choose_search_strategy(text: String, pattern: String) -> SearchStrategy {
  case core.choose_search_strategy(text, pattern) {
    core.Sliding -> Sliding
    core.Kmp -> Kmp
  }
}

// ============================================================================
// SLUG GENERATION & ASCII FOLDING (from str/extra)
// ============================================================================

/// Creates a URL-friendly slug from text.
///
/// ## Examples
///
/// ```gleam
/// slugify("Crème Brûlée")
/// // -> "creme-brulee"
/// ```
pub fn slugify(text: String) -> String {
  extra.slugify(text)
}

/// Creates slug with custom normalizer.
pub fn slugify_with_normalizer(text: String, normalizer) -> String {
  extra.slugify_with_normalizer(text, normalizer)
}

/// Decompose helper (forwarded from internal)
pub fn decompose_latin(s: String) -> String {
  internal_decompose.decompose_latin(s)
}

/// Creates slug using `SlugifyOptions` builder.
pub fn slugify_with_options(text: String, opts: extra.SlugifyOptions) -> String {
  extra.slugify_with_options(text, opts)
}

pub fn slugify_with_options_and_normalizer(
  text: String,
  opts: extra.SlugifyOptions,
  normalizer,
) -> String {
  extra.slugify_with_options_and_normalizer(text, opts, normalizer)
}

// Slugify options builder wrappers (expose via `import str`)
pub fn slugify_options() -> extra.SlugifyOptions {
  extra.slugify_options()
}

pub fn with_max_tokens(opts: extra.SlugifyOptions, n: Int) -> extra.SlugifyOptions {
  extra.with_max_tokens(opts, n)
}

pub fn with_separator(opts: extra.SlugifyOptions, sep: String) -> extra.SlugifyOptions {
  extra.with_separator(opts, sep)
}

pub fn with_preserve_unicode(opts: extra.SlugifyOptions, v: Bool) -> extra.SlugifyOptions {
  extra.with_preserve_unicode(opts, v)
}

pub fn with_custom_replacements(
  opts: extra.SlugifyOptions,
  replacements: List(#(String, String)),
) -> extra.SlugifyOptions {
  extra.with_custom_replacements(opts, replacements)
}

/// Creates slug with detailed options (backwards-compatible API).
// `slugify_opts` has been removed; use `SlugifyOptions` builder and `slugify_with_options` instead.

// `slugify_opts_with_normalizer` removed; use `SlugifyOptions` builder and `slugify_with_options_and_normalizer` instead.
// Example:
// let opts = str.slugify_options() |> str.with_max_tokens(0) |> str.with_separator("-") |> str.with_preserve_unicode(False)
// str.slugify_with_options_and_normalizer(text, opts, normalizer)

/// Converts text to ASCII equivalents.
///
/// ## Examples
///
/// ```gleam
/// ascii_fold("Café")
/// // -> "Cafe"
/// ```
pub fn ascii_fold(text: String) -> String {
  extra.ascii_fold(text)
}

/// ASCII folding without Unicode decomposition.
pub fn ascii_fold_no_decompose(text: String) -> String {
  extra.ascii_fold_no_decompose(text)
}

/// ASCII folding with custom normalizer.
pub fn ascii_fold_with_normalizer(text: String, normalizer) -> String {
  extra.ascii_fold_with_normalizer(text, normalizer)
}

/// ASCII folding without decomposition, with custom normalizer.
pub fn ascii_fold_no_decompose_with_normalizer(
  text: String,
  normalizer,
) -> String {
  extra.ascii_fold_no_decompose_with_normalizer(text, normalizer)
}

// ============================================================================
// CASE CONVERSION (from str/extra)
// ============================================================================

/// Converts text to snake_case.
///
/// ## Examples
///
/// ```gleam
/// to_snake_case("HelloWorld")
/// // -> "hello_world"
/// ```
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

// ============================================================================
// GRAPHEME TOKENIZATION (from str/tokenize)
// ============================================================================

/// Pure Gleam grapheme tokenizer (approximates Unicode segmentation).
///
/// This is an experimental pure-Gleam implementation that approximates
/// Unicode grapheme cluster segmentation without external dependencies.
pub fn chars(text: String) -> List(String) {
  tokenize.chars(text)
}

/// BEAM stdlib grapheme tokenizer (uses platform's Unicode support).
///
/// This wraps the standard library's grapheme segmentation for comparison
/// and compatibility.
pub fn chars_stdlib(text: String) -> List(String) {
  tokenize.chars_stdlib(text)
}

// ============================================================================
// CONFIGURATION (re-export from str/config)
// ============================================================================

/// Returns True when smart search is enabled.
pub fn smart_search_enabled() -> Bool {
  config.smart_search_enabled()
}

/// Minimum pattern length to consider KMP algorithm.
pub fn kmp_min_pattern_len() -> Int {
  config.kmp_min_pattern_len()
}

/// Threshold for "large" text lengths where KMP may be preferred.
pub fn kmp_large_text_threshold() -> Int {
  config.kmp_large_text_threshold()
}

/// Minimum pattern length to consider KMP on large texts.
pub fn kmp_large_text_min_pat() -> Int {
  config.kmp_large_text_min_pat()
}

/// Multiplier applied to max border to decide repetitiveness.
pub fn kmp_border_multiplier() -> Int {
  config.kmp_border_multiplier()
}
