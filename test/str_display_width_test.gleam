import gleeunit
import gleam/string
import str/display_width

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn ascii_width_test() {
  assert display_width.display_width("hello", 1) == 5
}

pub fn cjk_width_test() {
  // Each CJK ideograph typically counts as width 2
  assert display_width.display_width("漢字", 1) == 4
}

pub fn combining_width_test() {
  let s = "a\u{0301}" // a + combining acute
  // base 'a' (1) + combining (0) = 1
  assert display_width.display_width(s, 1) == 1
}

pub fn emoji_width_test() {
  let s = "👩‍👩‍👧‍👦" // family emoji (ZWJ sequence)
  assert display_width.display_width(s, 1) == 2
}

pub fn truncate_preserve_emoji_test() {
  let s = "Hello 👩‍👩‍👧‍👦 World"
  // truncate to width 10 and append "..."
  let t = display_width.truncate_display(s, 10, "...", 1)
  // the truncation should not split the family emoji; verify suffix present
  assert string.contains(t, "...")
}

pub fn pad_display_test() {
  let s = "a"
  let p = display_width.pad_display(s, 4, "right", " ", 1)
  // a + three spaces = 4 columns
  assert display_width.display_width(p, 1) == 4
}

pub fn pad_left_test() {
  let s = "hi"
  let p = display_width.pad_display(s, 5, "left", " ", 1)
  assert display_width.display_width(p, 1) == 5
  assert string.starts_with(p, "   hi")
}

pub fn pad_center_test() {
  let s = "a"
  let p = display_width.pad_display(s, 5, "center", "-", 1)
  assert display_width.display_width(p, 1) == 5
  assert string.contains(p, "a")
}

pub fn truncate_zero_width_test() {
  assert display_width.truncate_display("hello", 0, "...", 1) == ""
}

pub fn truncate_exact_fit_test() {
  assert display_width.truncate_display("hello", 5, "...", 1) == "hello"
}

pub fn truncate_with_cjk_test() {
  let s = "你好世界" // 4 CJK chars = 8 width
  let t = display_width.truncate_display(s, 5, ".", 1)
  // Should fit 2 CJK chars (width 4) + suffix (width 1) = 5
  assert display_width.display_width(t, 1) <= 5
  assert string.contains(t, ".")
}

pub fn ambiguous_width_as_1_test() {
  // U+00B1 (±) is ambiguous - should be 1 when ambiguous_as=1
  assert display_width.display_width("±", 1) == 1
}

pub fn ambiguous_width_as_2_test() {
  // U+00B1 (±) is ambiguous - should be 2 when ambiguous_as=2
  assert display_width.display_width("±", 2) == 2
}

pub fn mixed_content_test() {
  let s = "Hi世界👍" // ASCII (2) + CJK (4) + emoji (2) = 8
  assert display_width.display_width(s, 1) == 8
}

pub fn zero_width_combining_test() {
  let s = "e\u{0301}\u{0302}" // e + acute + circumflex
  // base 'e' (1) + two combinings (0 each) = 1
  assert display_width.display_width(s, 1) == 1
}

