import str

pub fn is_empty_test() {
  assert str.is_empty("") == True
  assert str.is_empty(" ") == False
  assert str.is_empty("a") == False
}

