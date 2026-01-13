import gleam/string
import str

fn gen_token_pool() -> List(String) {
  fh.gen_token_pool_basic()
}

import test_support/fuzz_helpers as fh

fn gen_string(seed: Int, tokens: List(String), n: Int) -> String {
  fh.gen_string(seed, tokens, n)
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
