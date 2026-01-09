import gleam/list
import str
import str/internal/core

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

fn repeat(s: String, n: Int) -> String {
  list.fold(list.range(1, n), "", fn(acc, _) { acc <> s })
}
