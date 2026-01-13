import str
import gleam/list

pub fn smart_search_default_test() {
  assert str.smart_search_enabled() == False
}

fn make_repeat(s: String, n: Int) -> String {
  list.fold(list.range(1, n), "", fn(acc, _) { acc <> s })
}

pub fn choose_strategy_min_pattern_test() {
  let min = str.kmp_min_pattern_len()
  // pattern of length `min` should prefer KMP
  let pat = make_repeat("a", min)
  let strategy = str.choose_search_strategy("some text", pat)
  assert strategy == str.Kmp
}

pub fn choose_strategy_small_pattern_test() {
  let pat = "a"
  let strategy = str.choose_search_strategy("short", pat)
  assert strategy == str.Sliding
}
