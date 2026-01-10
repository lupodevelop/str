import gleam/expect
import str/internal/generated_translit_pairs as gen
import str/internal/generated_translit_pages as pages
import str/internal/translit_pure as pure

pub fn main() {
  let pairs = gen.replacements_generated()

  // Single-codepoint pairs should be present in the pages lookup
  list.fold(pairs, Nil, fn(_, pair) {
    let #(from, to) = pair
    case string.to_graphemes(from) {
      [g] -> {
        case pages.translit_pages_lookup_by_grapheme(g) {
          Ok(v) -> expect.equal(v, to)
          Error(_) -> expect.fail("Missing pages entry for translit pair")
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
  expect.equal(folded, translit)
}