/// Unit tests for string search algorithms.
///
/// This file tests all search-related functionality:
/// - KMP (Knuth-Morris-Pratt) algorithm
/// - Sliding window search
/// - Automatic strategy selection (choose_search_strategy)
/// - Search with cached/pre-built maps
///
/// Test coverage:
/// - Basic pattern matching
/// - Overlapping occurrences
/// - Empty patterns and edge cases
/// - Emoji and Unicode grapheme clusters
/// - Strategy performance characteristics
/// - Index finding (index_of_strategy)
/// - Occurrence counting (count_strategy)
/// - Cache reuse (build_kmp_maps, *_with_maps functions)
import gleam/list
import str
import str/internal/core as core

// KMP & Sliding tests consolidated
pub fn build_prefix_table_basic_test() {
  let p = "ababaca"
  assert str.build_prefix_table(p) == [0, 0, 1, 2, 3, 0, 1]
}

pub fn kmp_simple_test() {
  let text = "ababa"
  let pat = "aba"
  assert str.kmp_search_all(text, pat) == [0, 2]
}

pub fn kmp_overlapping_test() {
  let text = "aaaa"
  let pat = "aa"
  assert str.kmp_search_all(text, pat) == [0, 1, 2]
}

pub fn kmp_emoji_test() {
  let e = "👨‍👩‍👧‍👦"
  let text = e <> "x" <> e
  assert str.kmp_search_all(text, e) == [0, 2]
}

pub fn kmp_empty_pattern_test() {
  let text = "hello"
  let pat = ""
  assert str.kmp_search_all(text, pat) == []
}

pub fn sliding_simple_test() {
  let text = "ababa"
  let pat = "aba"
  assert str.sliding_search_all(text, pat) == [0, 2]
}

pub fn sliding_overlapping_test() {
  let text = "aaaa"
  let pat = "aa"
  assert str.sliding_search_all(text, pat) == [0, 1, 2]
}

pub fn sliding_emoji_test() {
  let e = "👨‍👩‍👧‍👦"
  let text = e <> "x" <> e
  assert str.sliding_search_all(text, e) == [0, 2]
}

pub fn sliding_empty_pattern_test() {
  let text = "hello"
  let pat = ""
  assert str.sliding_search_all(text, pat) == []
}

// Strategy chooser tests
pub fn choose_strategy_small_random_test() {
  let text = "abcdefghij"
  let pat = "cd"
  assert str.choose_search_strategy(text, pat) == str.Sliding
}

pub fn choose_strategy_large_pat_test() {
  let text = repeat("a", 100)
  let pat = repeat("a", 100)
  assert str.choose_search_strategy(text, pat) == str.Kmp
}

// Large string test - only runs on Erlang (would OOM on JS)
@target(erlang)
pub fn choose_strategy_long_text_small_pat_test() {
  let text = repeat("a", 200_000)
  let pat = "abcdabcd"
  assert str.choose_search_strategy(text, pat) == str.Kmp
}

pub fn choose_strategy_repetitive_border_test() {
  let pat = repeat("ab", 50)
  let text = repeat("ab", 1000)
  assert str.choose_search_strategy(text, pat) == str.Kmp
}

pub fn choose_strategy_empty_pattern_test() {
  assert str.choose_search_strategy("hello", "") == str.Sliding
}

// Explicit strategy / count tests
pub fn index_of_strategy_sliding_test() {
  let text = "hello world"
  let pat = "world"
  assert str.index_of_strategy(text, pat, str.Sliding)
    == str.sliding_index_of(text, pat)
}

pub fn index_of_strategy_kmp_test() {
  let text = repeat("ab", 100)
  let pat = repeat("ab", 50)
  assert str.index_of_strategy(text, pat, str.Kmp)
    == str.kmp_index_of(text, pat)
}

pub fn count_strategy_sliding_test() {
  let text = "aaaa"
  let pat = "aa"
  assert str.count_strategy(text, pat, True, str.Sliding)
    == list.length(core.sliding_search_all(text, pat))
}

pub fn count_strategy_kmp_test() {
  let text = repeat("ab", 100)
  let pat = repeat("ab", 5)
  assert str.count_strategy(text, pat, True, str.Kmp)
    == list.length(core.kmp_search_all(text, pat))
}

// KMP map reuse tests
pub fn kmp_maps_reuse_index_test() {
  let pat = repeat("ab", 50)
  let maps = str.build_kmp_maps(pat)
  let pmap = maps.0
  let pimap = maps.1

  let text1 = repeat("ab", 200)
  let text2 = "xxxx" <> repeat("ab", 100) <> "yyyy"

  assert str.kmp_index_of_with_maps(text1, pat, pmap, pimap)
    == str.kmp_index_of(text1, pat)
  assert str.kmp_index_of_with_maps(text2, pat, pmap, pimap)
    == str.kmp_index_of(text2, pat)
}

pub fn kmp_maps_reuse_search_all_test() {
  let pat = repeat("aba", 30)
  let maps = str.build_kmp_maps(pat)
  let pmap = maps.0
  let pimap = maps.1

  let text = repeat("aba", 200)
  assert str.kmp_search_all_with_maps(text, pmap, pimap)
    == str.kmp_search_all(text, pat)
}

fn repeat(s: String, n: Int) -> String {
  list.fold(list.range(1, n), "", fn(acc, _) { acc <> s })
}