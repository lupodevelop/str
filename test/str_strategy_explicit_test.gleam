import gleam/list
import str/core

pub fn index_of_strategy_sliding_test() {
  let text = "hello world"
  let pat = "world"
  assert core.index_of_strategy(text, pat, core.Sliding)
    == core.sliding_index_of(text, pat)
}

pub fn index_of_strategy_kmp_test() {
  let text = repeat("ab", 100)
  let pat = repeat("ab", 50)
  assert core.index_of_strategy(text, pat, core.Kmp)
    == core.kmp_index_of(text, pat)
}

pub fn count_strategy_sliding_test() {
  let text = "aaaa"
  let pat = "aa"
  assert core.count_strategy(text, pat, True, core.Sliding)
    == list.length(core.sliding_search_all(text, pat))
}

pub fn count_strategy_kmp_test() {
  let text = repeat("ab", 100)
  let pat = repeat("ab", 5)
  assert core.count_strategy(text, pat, True, core.Kmp)
    == list.length(core.kmp_search_all(text, pat))
}

fn repeat(s: String, n: Int) -> String {
  list.fold(list.range(1, n), "", fn(acc, _) { acc <> s })
}
