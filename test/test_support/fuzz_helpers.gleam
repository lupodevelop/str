import gleam/list

// Deterministic token pools and generators used by fuzz tests.
pub fn gen_token_pool_basic() -> List(String) {
  [
    "a",
    "b",
    "c",
    "1",
    "2",
    "3",
    " ",
    "\n",
    "<",
    ">",
    "&",
    "\"",
    "'",
    "&amp;",
    "&lt;",
    "&gt;",
    "&quot;",
    "&#39;",
    "&#x27;",
    "&#x22;",
    "&notanentity;",
    "&",
    "&amp",
    "&#",
    "&#x",
    "\u{00A0}",
    // NBSP
    "Café",
    "naïve",
    "ø",
    "漢",
    "字",
    "👩‍👩‍👧‍👦",
    "👨‍👩‍👧",
    "️",
    "✈️",
    "🏳️‍🌈",
    "\u{0301}",
    // combining acute
    "&alpha;",
    "&beta;",
    "&gamma;",
  ]
}

pub fn gen_token_pool_search() -> List(String) {
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

pub fn idx_for(seed: Int, i: Int, len: Int) -> Int {
  let v = seed * 1_103_515_245 + 12_345 + i
  let v_pos = case v < 0 { True -> -v False -> v }
  v_pos % len
}

pub fn gen_string(seed: Int, tokens: List(String), n: Int) -> String {
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