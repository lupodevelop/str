import gleam/list
import gleeunit
import str

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn roundtrip_basic_entities_test() {
  let cases = [
    "<div>Hello</div>",
    "Tom & Jerry",
    "Say \"hello\"",
    "It's me",
    "5 < 10 && 10 > 5",
    "Ampersand: &",
  ]

  list.fold(cases, True, fn(_, s) {
    let escaped = str.escape_html(s)
    let unescaped = str.unescape_html(escaped)
    assert unescaped == s
    True
  })
}

pub fn numeric_and_named_entities_test() {
  assert str.unescape_html("&lt;&gt;&amp;&#39;&#x27;&#34;") == "<>&''\""
  assert str.unescape_html("&quot; and &#34; and &#x22;") == "\" and \" and \""
  assert str.unescape_html("I like &#39;quotes&#39;") == "I like 'quotes'"
  assert str.unescape_html("Hex: &#x27;") == "Hex: '"
}

pub fn malformed_and_unknown_entity_test() {
  // Missing semicolon should remain unchanged
  assert str.unescape_html("This &amp is broken") == "This &amp is broken"

  // Unknown entity should remain unchanged
  assert str.unescape_html("This &notanentity; remains")
    == "This &notanentity; remains"
}

pub fn combined_and_adjacent_entities_test() {
  assert str.unescape_html("&lt;&lt; &gt;&gt;") == "<< >>"
  assert str.unescape_html("&amp;&amp;&amp;") == "&&&"
}

pub fn unicode_and_emoji_roundtrip_test() {
  let s = "Café — ️👩‍👩‍👧‍👦 \u{00A0}"
  let escaped = str.escape_html(s)
  // Expect unescape to restore the original (escape may not change emoji/nbspace)
  assert str.unescape_html(escaped) == s
}

pub fn idempotence_and_double_escape_test() {
  let s = "&"
  let once = str.escape_html(s)
  let twice = str.escape_html(once)
  assert once == "&amp;"
  assert twice == "&amp;amp;"
  // unescape decodes one level: "&amp;amp;" -> "&amp;"; double unescape restores original
  assert str.unescape_html(twice) == "&amp;"
  assert str.unescape_html(str.unescape_html(twice)) == s
}
