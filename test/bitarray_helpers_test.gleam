import gleam/expect
import str/internal/bitarray_helpers as bits

pub fn main() {
  expect.equal(bits.popcount(0), 0)
  expect.equal(bits.popcount(13), 3) // 13 = 0b1101

  expect.equal(bits.bit_in_word(5, 0), 1) // 5 = 0b101
  expect.equal(bits.bit_in_word(5, 1), 0)
  expect.equal(bits.bit_in_word(5, 2), 1)

  let words = [5]
  // rank before index 2 counts bits at positions 0..1 -> 1
  expect.equal(bits.rank_in_masks(words, 2, 64), 1)
}