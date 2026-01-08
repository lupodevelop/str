import gleam/list
import str/core

pub fn kmp_maps_reuse_index_test() {
  let pat = repeat("ab", 50)
  let maps = core.build_kmp_maps(pat)
  let pmap = maps.0
  let pimap = maps.1

  let text1 = repeat("ab", 200)
  let text2 = "xxxx" <> repeat("ab", 100) <> "yyyy"

  assert core.kmp_index_of_with_maps(text1, pat, pmap, pimap)
    == core.kmp_index_of(text1, pat)
  assert core.kmp_index_of_with_maps(text2, pat, pmap, pimap)
    == core.kmp_index_of(text2, pat)
}

pub fn kmp_maps_reuse_search_all_test() {
  let pat = repeat("aba", 30)
  let maps = core.build_kmp_maps(pat)
  let pmap = maps.0
  let pimap = maps.1

  let text = repeat("aba", 200)
  assert core.kmp_search_all_with_maps(text, pmap, pimap)
    == core.kmp_search_all(text, pat)
}

fn repeat(s: String, n: Int) -> String {
  list.fold(list.range(1, n), "", fn(acc, _) { acc <> s })
}
