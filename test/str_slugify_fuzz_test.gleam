import gleam/expect
import gleam/list
import str
import str/internal/generated_translit_pairs as gen

// Deterministic pseudo-random generator
fn next(seed: Int) -> Int {
  (1103515245 * seed + 12345) % 2147483648
}

pub fn main() {
  let pairs = gen.replacements_generated()
  let mut seed = 42

  let rec loop(i, seed) {
    case i == 0 {
      True -> True
      False -> {
        let seed2 = next(seed)
        // Build a string by concatenating a few random "from" graphemes
        let s = list.fold(1..=5, "", fn(acc, _) {
          let seed2 = next(seed2)
          let idx = seed2 % list.length(pairs)
          case list.drop(pairs, idx) {
            [#(f, _), ..] -> acc ++ f
            _ -> acc ++ "a"
          }
        })

        let slug = str.slugify(s)

        // basic assertions: slug is lowercase and contains no space
        expect.equal(slug, string.lowercase(slug))
        expect.equal(string.contains(slug, " "), False)

        loop(i - 1, seed2)
      }
    }
  }
  loop(200, seed)
}
