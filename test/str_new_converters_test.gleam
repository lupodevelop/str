import gleeunit/should
import str

pub fn is_mixed_case_test() {
  str.is_mixed_case("Hello") |> should.be_true
  str.is_mixed_case("hello") |> should.be_false
  str.is_mixed_case("HELLO") |> should.be_false
  str.is_mixed_case("Hello123") |> should.be_true
  str.is_mixed_case("123") |> should.be_false
  str.is_mixed_case("") |> should.be_false
}

pub fn camel_to_snake_test() {
  str.camel_to_snake("camelCase") |> should.equal("camel_case")
  str.camel_to_snake("XMLHttpRequest") |> should.equal("xml_http_request")
  str.camel_to_snake("simple") |> should.equal("simple")
  str.camel_to_snake("Already_Snake") |> should.equal("already_snake")
}

pub fn pascal_to_snake_test() {
  str.pascal_to_snake("PascalCase") |> should.equal("pascal_case")
  str.pascal_to_snake("XMLHttpRequest") |> should.equal("xml_http_request")
}

pub fn snake_to_camel_test() {
  str.snake_to_camel("snake_case_name") |> should.equal("snakeCaseName")
  str.snake_to_camel("simple") |> should.equal("simple")
  // Testing numbers and acroynms
  str.snake_to_camel("xml_http_request") |> should.equal("xmlHttpRequest")
}

pub fn snake_to_pascal_test() {
  str.snake_to_pascal("snake_case_name") |> should.equal("SnakeCaseName")
  str.snake_to_pascal("simple") |> should.equal("Simple")
}
