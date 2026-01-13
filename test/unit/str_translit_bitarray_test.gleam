// This test only runs on Erlang (64-bit page tables)
@target(erlang)
import str/internal/generated_translit_pages as gen
import str/internal/translit as translit
import gleam/string
@target(erlang)
import gleam/list

@target(erlang)
pub fn generated_pages_lookup_parity_test() {
  // sample a few single-codepoint graphemes from generated data
  let samples = ["À", "á", "ß", "Æ", "œ", "Ğ", "ł", "Ů", "ŕ", "Œ"]
  let ok = list.fold(samples, True, fn(acc, s) {
    case gen.translit_pages_lookup_by_grapheme(s) {
      Ok(v) -> acc && v != ""
      Error(_) -> False
    }
  })
  assert ok
}

pub fn transliterate_sample_test() {
  let s = "Café — Ångström, Æther, Straße"
  let new = translit.transliterate_pure(s)
  assert string.contains(new, "Cafe") || string.contains(new, "Cafe")
}