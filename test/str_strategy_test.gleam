import gleam/list
import str/core as core

pub fn choose_strategy_small_random_test() {
  let text = "abcdefghij"
  let pat = "cd"
  assert core.choose_search_strategy(text, pat) == core.Sliding
}

pub fn choose_strategy_large_pat_test() {
  let text = repeat("a", 100)
  let pat = repeat("a", 100)
  assert core.choose_search_strategy(text, pat) == core.Kmp
}

pub fn choose_strategy_long_text_small_pat_test() {
  let text = repeat("a", 200000)
  let pat = "abcdabcd"
  assert core.choose_search_strategy(text, pat) == core.Kmp
}

pub fn choose_strategy_repetitive_border_test() {
  // pattern with large border: 'abababab...'
  let pat = repeat("ab", 50)
  let text = repeat("ab", 1000)
  assert core.choose_search_strategy(text, pat) == core.Kmp
}

pub fn choose_strategy_empty_pattern_test() {
  assert core.choose_search_strategy("hello", "") == core.Sliding
}

fn repeat(s: String, n: Int) -> String {
  list.fold(list.range(1, n), "", fn(acc, _) { acc <> s })
}
