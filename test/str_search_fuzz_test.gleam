import gleam/list
import gleam/string
import gleeunit
import str

pub fn main() -> Nil {
  gleeunit.main()
}

// Deterministic token pool including multi-codepoint graphemes
fn gen_token_pool() -> List(String) {
  [
    "a",
    "b",
    "c",
    "1",
    "2",
    " ",
    "é",
    "ß",
    "ø",
    "漢",
    "字",
    "👩‍👩‍👧‍👦",
    "👨‍👩‍👧",
    "✈️",
    "🏳️‍🌈",
    "\u{0301}",
    "aa",
    "ab",
    "ba",
  ]
}

// Deterministic pseudo-random index using seed and i
fn idx_for(seed: Int, i: Int, len: Int) -> Int {
  let v = seed * 1_103_515_245 + 12_345 + i
  let v_pos = case v < 0 { True -> -v False -> v }
  v_pos % len
}

fn gen_string(seed: Int, tokens: List(String), n: Int) -> String {
  let len = list.length(tokens)
  let seq = list.range(0, n - 1)
  seq
  |> list.map(fn(i) {
    let j = idx_for(seed, i, len)
    case list.drop(tokens, j) {
      [first, ..] -> first
      [] -> ""
    }
  })
  |> list.fold("", fn(acc, s) { acc <> s })
}

// Naive grapheme-aware search: returns all match start indices
fn naive_search_all(text: String, pat: String) -> List(Int) {
  let tgs = string.to_graphemes(text)
  let pgs = string.to_graphemes(pat)
  let tp = list.length(tgs)
  let pp = list.length(pgs)

  case pp == 0 {
    True -> []
    False -> {
      case pp > tp {
        True -> []
        False -> {
          let last = tp - pp
          let r = list.range(0, last)
          r
          |> list.filter(fn(i) {
            let slice = list.take(list.drop(tgs, i), pp)
            slice == pgs
          })
        }
      }
    }
  }
}

fn first_or_neg(xs: List(Int)) -> Int {
  case xs {
    [] -> -1
    [h, ..] -> h
  }
}

fn run_cfg(seed: Int, tlen: Int, plen: Int) -> Bool {
  let tokens = gen_token_pool()
  let text = gen_string(seed, tokens, tlen)
  let pat = gen_string(seed + 7, tokens, plen)

  let na = naive_search_all(text, pat)
  let k = str.kmp_search_all(text, pat)
  let s = str.sliding_search_all(text, pat)

  // parity of all-match lists
  assert na == k
  assert k == s

  // first-index parity (Result vs list)
  let kidx = case str.kmp_index_of(text, pat) { Ok(v) -> v Error(_) -> -1 }
  let sidx = case str.sliding_index_of(text, pat) { Ok(v) -> v Error(_) -> -1 }
  let tfirst = first_or_neg(na)
  assert kidx == tfirst
  assert sidx == tfirst

  // count parity (non-overlapping and overlapping)
  // overlapping: count should match length of matches
  let count_overlapping = str.count(text, pat, True)
  let count_non_overlapping = str.count(text, pat, False)
  assert count_overlapping == list.length(na)

  // Non-overlapping naive count (greedy left-to-right)
  let non_overlap_count = fn_count_non_overlap(text, pat)
  assert count_non_overlapping == non_overlap_count

  True
}

fn fn_count_non_overlap(text: String, pat: String) -> Int {
  // Mirror internal `count` semantics: walk the haystack left-to-right and
  // advance by the needle length when a match is found (greedy, non-overlapping).
  let hs = string.to_graphemes(text)
  let nd = string.to_graphemes(pat)
  let nd_len = list.length(nd)

  case nd_len == 0 {
    True -> 0
    False -> fn_count_non_overlap_walk(hs, list.length(hs), nd, nd_len, 0)
  }
}

fn fn_count_non_overlap_walk(hs: List(String), hs_len: Int, nd: List(String), nd_len: Int, acc: Int) -> Int {
  case hs_len < nd_len {
    True -> acc
    False ->
      case list.take(hs, nd_len) == nd {
        True -> fn_count_non_overlap_walk(list.drop(hs, nd_len), hs_len - nd_len, nd, nd_len, acc + 1)
        False -> fn_count_non_overlap_walk(list.drop(hs, 1), hs_len - 1, nd, nd_len, acc)
      }
  }
}

pub fn fuzz_search_parity_test() {
  run_cfg(1, 50, 1)
  run_cfg(1, 50, 2)
  run_cfg(42, 100, 3)
  run_cfg(123, 200, 5)
  run_cfg(999, 300, 7)

  True
}