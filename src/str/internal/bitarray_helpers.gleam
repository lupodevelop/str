import gleam/list

// Shared helpers for packed bit masks used by generated page modules.

pub fn get_at(xs: List(a), idx: Int) -> Result(a, Nil) {
  case list.drop(xs, idx) {
    [h, ..] -> Ok(h)
    _ -> Error(Nil)
  }
}

fn popcount_loop(xi: Int, acc: Int) -> Int {
  case xi == 0 {
    True -> acc
    False -> { let b = xi % 2
      popcount_loop(xi / 2, acc + b) }
  }
}

pub fn popcount(x: Int) -> Int {
  popcount_loop(x, 0)
}

fn bit_in_word_loop(wi: Int, ii: Int) -> Int {
  case ii == 0 {
    True -> wi % 2
    False -> bit_in_word_loop(wi / 2, ii - 1)
  }
}

pub fn bit_in_word(w: Int, i: Int) -> Int {
  bit_in_word_loop(w, i)
}

fn count_lower_bits_loop(wi: Int, ni: Int, acc: Int) -> Int {
  case ni == 0 {
    True -> acc
    False -> { let b = wi % 2
      count_lower_bits_loop(wi / 2, ni - 1, acc + b) }
  }
}

pub fn count_lower_bits(w: Int, n: Int) -> Int {
  count_lower_bits_loop(w, n, 0)
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
