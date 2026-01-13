import gleam/list
import gleam/string
import str
import str/internal/generated_translit_pairs as gen

// Deterministic pseudo-random generator
fn next(seed: Int) -> Int {
  1103515245 * seed + 12345 % 2147483648
}

fn slugify_fuzz_loop(i: Int, seed: Int, pairs: List(#(String, String))) {
  case i == 0 {
    True -> True
    False -> {
      let seed2 = next(seed)
      // Build a string by concatenating a few random "from" graphemes
      let s = list.fold([1, 2, 3, 4, 5], "", fn(acc, _) {
        let seed2 = next(seed2)
        let idx = seed2 % list.length(pairs)
        case list.drop(pairs, idx) {
          [#(f, _), ..] -> string.concat([acc, f])
          _ -> string.concat([acc, "a"])
        }
      })

      let slug = str.slugify(s)

      // basic assertions: slug is lowercase and contains no space
        assert slug == string.lowercase(slug)
        assert string.contains(slug, " ") == False
      slugify_fuzz_loop(i - 1, seed2, pairs)
    }
  }
}

pub fn slugify_fuzz_test() {
  let pairs = gen.replacements_generated()
  let seed = 42
  slugify_fuzz_loop(200, seed, pairs)
}
