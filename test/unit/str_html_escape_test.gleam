import str

pub fn escape_basic_test() {
  assert str.escape_html("<div>Hello</div>") == "&lt;div&gt;Hello&lt;/div&gt;"
  assert str.escape_html("Tom & Jerry") == "Tom &amp; Jerry"
  assert str.escape_html("Say \"hello\"") == "Say &quot;hello&quot;"
}

pub fn unescape_basic_test() {
  assert str.unescape_html("&lt;div&gt;") == "<div>"
  assert str.unescape_html("Tom &amp; Jerry") == "Tom & Jerry"
  assert str.unescape_html("Say &quot;hello&quot;") == "Say \"hello\""
  assert str.unescape_html("It&#39;s me") == "It's me"
}

pub fn roundtrip_test() {
  let s = "Hello & < > \""
  let escaped = str.escape_html(s)
  assert str.unescape_html(escaped) == s
}

pub fn numeric_entities_test() {
  // Decimal numeric entity
  assert str.unescape_html("I like &#39;quotes&#39;") == "I like 'quotes'"

  // Hex numeric entity
  assert str.unescape_html("Hex: &#x27;") == "Hex: '"

  // Double quote numeric and hex
  assert str.unescape_html("&quot; and &#34; and &#x22;") == "\" and \" and \""
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
