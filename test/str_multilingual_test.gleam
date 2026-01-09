import gleam/list
import gleam/string
import str
import str/internal/decompose

// French examples
pub fn french_fold_and_slug_test() {
  let s = "Crème Brûlée — déjà vu"
  assert str.ascii_fold(s) == "Creme Brulee — deja vu"
  let slug = str.slugify_opts(s, 0, "-", False)
  assert slug == "creme-brulee-deja-vu"
}

// Polish examples
pub fn polish_fold_and_decomposed_test() {
  let s = "Gdańsk Łódź Żółć"
  assert str.ascii_fold(s) == "Gdansk Lodz Zolc"

  let dec = decompose.decompose_latin("Łódź")
  assert str.ascii_fold(dec) == "Lodz"
}

// Scandinavian (Norwegian / Swedish / Danish)
pub fn scandi_test() {
  let s = "Smörgåsbord Ærø Ångström Øresund"
  assert str.ascii_fold(s) == "Smorgasbord AEro Angstrom Oresund"
  let slug = str.slugify_opts(s, 0, "-", False)
  assert slug == "smorgasbord-aero-angstrom-oresund"
}

// Romanian and Turkish
pub fn rom_turk_test() {
  let r = "Țări Șosea"
  assert str.ascii_fold(r) == "Tari Sosea"

  let t = "Şişli İzmir"
  assert str.ascii_fold(t) == "Sisli Izmir"
}

// Icelandic
pub fn icelandic_test() {
  let s = "Þingvellirmaður ð"
  // Current transliteration maps "Þ" -> "TH"
  assert str.ascii_fold(s) == "THingvellirmadur d"
}

// Complex combined case: mixing emoji and diacritics and decomposed input
pub fn complex_mixed_test() {
  let mixed = "👩\u{200D}👩\u{200D}👧 café — Ångström"
  // preserve unicode for emoji, fold accents
  let folded = str.ascii_fold(mixed)
  let gs = string.to_graphemes(folded)
  let firsts = list.take(gs, 1)
  let ok = case firsts {
    [first] -> first == "👩\u{200D}👩\u{200D}👧"
    _ -> False
  }
  assert ok
  assert string.contains(folded, "Angstrom")

  // decomposed sequence for Å
  let dec = decompose.decompose_latin("Ångström")
  assert str.ascii_fold(dec) == "Angstrom"
}
