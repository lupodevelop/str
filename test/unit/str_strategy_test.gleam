import gleam/list
import str

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
  // pattern with large border: 'abababab...'
  let pat = repeat("ab", 50)
  let text = repeat("ab", 1000)
  assert str.choose_search_strategy(text, pat) == str.Kmp
}

pub fn choose_strategy_empty_pattern_test() {
  assert str.choose_search_strategy("hello", "") == str.Sliding
}

fn repeat(s: String, n: Int) -> String {
  list.fold(list.range(1, n), "", fn(acc, _) { acc <> s })
}
