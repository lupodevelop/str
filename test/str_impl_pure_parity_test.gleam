import gleam/expect
import str/internal/translit_impl as impl_translit
import str/internal/translit_pure as pure_translit
import str/internal/decompose_impl as impl_decompose
import str/internal/decompose_pure as pure_decompose

pub fn main() {
  // Transliteration mappings parity
  expect.equal(impl_translit.replacements(), pure_translit.replacements_pure())

  // Combining marks removal parity (sample)
  let s = "café"
  expect.equal(impl_translit.remove_combining_marks(s), pure_translit.remove_combining_marks_pure(s))

  // Decompose parity
  let d = "Å"
  expect.equal(impl_decompose.decompose_latin(d), pure_decompose.decompose_latin_pure(d))
}