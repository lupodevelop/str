// This test only runs on Erlang (64-bit page tables)
@target(erlang)
import str/internal/generated_translit_pairs as gen
@target(erlang)
import str/internal/generated_translit_pages as pages
@target(erlang)
import str/internal/translit_pure as pure
@target(erlang)
import gleam/list
@target(erlang)
import gleam/string

@target(erlang)
pub fn translit_pages_parity_test() {
  let pairs = gen.replacements_generated()

  // Single-codepoint pairs should be present in the pages lookup
  list.fold(pairs, Nil, fn(_, pair) {
    let #(from, to) = pair
    case string.to_graphemes(from) {
      [g] -> {
        case pages.translit_pages_lookup_by_grapheme(g) {
          Ok(v) -> { assert v == to }
          Error(_) -> { assert string.length(from) == 0 }
        }
      }
      _ -> Nil
    }
  })

  // Spot-check: a composed string should match the fold-based replacement
  let sample = "Crème Brûlée"

  let folded = list.fold(pairs, sample, fn(acc, pair) {
    let #(f, t) = pair
    string.replace(acc, f, t)
  })

  let translit = pure.transliterate_pure(sample)
  assert folded == translit
}