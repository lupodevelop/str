import gleam/list
import str

pub fn length_more_test() {
  // Basic cases
  assert str.length("") == 0
  assert str.length("a") == 1
  assert str.length("hello") == 5

  // Precomposed accented characters
  assert str.length("café") == 4
  assert str.length("Crème") == 5

  // Combining sequence (e + combining acute) should count as a single grapheme
  // The literal below is an 'e' + U+0301 combining acute accent
  assert str.length("é") == 1

  // Multiple characters with combining mark
  assert str.length("áb") == 2

  // Emoji sequences
  assert str.length("👍") == 1
  assert str.length("👍🏽") == 1
  assert str.length("👨‍👩‍👧‍👦") == 1

  // Flags (two regional indicators) count as one grapheme
  assert str.length("🇮🇹") == 1

  // Astral plane character (surrogate pair / multi-byte) counts as one
  assert str.length("𐐷") == 1

  // Long ASCII string
  let long = list.fold(list.range(1, 100), "", fn(acc, _) { acc <> "x" })
  assert str.length(long) == 100
}
