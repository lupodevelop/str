import gleam/int
import gleam/list
import gleam/string
import gleeunit
import str

pub fn main() -> Nil {
  gleeunit.main()
}

// Deterministic, simple generator over a token pool.
fn gen_token_pool() -> List(String) {
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

// Deterministic pseudo-random index using seed and i
fn idx_for(seed: Int, i: Int, len: Int) -> Int {
  // simple LCG-ish formula; keep small to avoid large-int overhead
  let v = seed * 1_103_515_245 + 12_345 + i
  let v_pos = case v < 0 {
    True -> -v
    False -> v
  }
  v_pos % len
}

fn gen_string(seed: Int, tokens: List(String), n: Int) -> String {
  let len = list.length(tokens)
  int.range(from: 0, to: n, with: "", run: fn(acc, i) {
    let j = idx_for(seed, i, len)
    let token = case list.drop(tokens, j) {
      [first, ..] -> first
      [] -> ""
    }
    acc <> token
  })
}

fn run_cfg(seed: Int, n: Int, tokens: List(String)) -> Bool {
  let s = gen_string(seed, tokens, n)
  // Roundtrip: unescape(escape(s)) == s
  let escaped = str.escape_html(s)
  let unescaped = str.unescape_html(escaped)
  assert unescaped == s

  // Escaped string must not contain raw angle brackets or quotes
  assert string.contains(escaped, "<") == False
  assert string.contains(escaped, ">") == False
  assert string.contains(escaped, "\"") == False
  assert string.contains(escaped, "'") == False

  True
}

pub fn fuzz_roundtrip_test() {
  let tokens = gen_token_pool()

  run_cfg(1, 20, tokens)
  run_cfg(42, 50, tokens)
  run_cfg(123, 200, tokens)

  True
}
