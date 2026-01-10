import gleam/list

// Shared helpers for packed bit masks used by generated page modules.

pub fn get_at(xs: List(a), idx: Int) -> Result(a, Nil) {
  case list.drop(xs, idx) {
    [h, ..] -> Ok(h)
    _ -> Error(Nil)
  }
}

pub fn popcount(x: Int) -> Int {
  case x == 0 {
    True -> 0
    False -> popcount(x / 2) + (x % 2)
  }
}

pub fn bit_in_word(w: Int, i: Int) -> Int {
  case i == 0 {
    True -> w % 2
    False -> bit_in_word(w / 2, i - 1)
  }
}

pub fn count_lower_bits(w: Int, n: Int) -> Int {
  case n == 0 {
    True -> 0
    False -> count_lower_bits(w, n - 1) + bit_in_word(w, n - 1)
  }
}

pub fn rank_in_masks(words: List(Int), idx: Int, word_bits: Int) -> Int {
  let word_idx = idx / word_bits
  let bit_idx = idx % word_bits

  let before = list.take(words, word_idx)
  let s = list.fold(before, 0, fn(acc, w) { acc + popcount(w) })

  case get_at(words, word_idx) {
    Ok(w) -> s + count_lower_bits(w, bit_idx)
    Error(_) -> s
  }
}
