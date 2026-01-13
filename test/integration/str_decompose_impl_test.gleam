import str/internal/decompose as decomp
import str/internal/decompose_pure as pure

pub fn decompose_impl_matches_pure_test() {
  let s = "Café — Ångström"
  assert decomp.decompose_latin(s) == pure.decompose_latin_pure(s)
}

