import str

// internal_decompose not needed directly here; folding covers decomposed inputs

pub fn corpus_french_test() {
  assert str.ascii_fold("Élévation déjà") == "Elevation deja"
}

pub fn corpus_polish_test() {
  assert str.ascii_fold("Żywiec Łódź Gdańsk") == "Zywiec Lodz Gdansk"
}

pub fn corpus_czech_test() {
  assert str.ascii_fold("Příliš žluťoučký kůň") == "Prilis zlutoucky kun"
}

pub fn corpus_slovak_test() {
  // note: our folding is pragmatic; test expected approximations
  assert str.ascii_fold("Žltý kôň Ťažký") == "Zlty kon Tazky"
}

pub fn corpus_lithuanian_test() {
  assert str.ascii_fold("Žemėlapis ėė ąč") == "Zemelapis ee ac"
}

pub fn corpus_latvian_test() {
  assert str.ascii_fold("Ķekava Ēriks Ūdens") == "Kekava Eriks Udens"
}
