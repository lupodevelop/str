import gleam/list
import gleam/string
import str/core
import str/tokenize

pub fn pad_and_center_tests() {
  assert core.pad_left("x", 3, "*") == "**x"
  assert core.pad_right("x", 3, "*") == "x**"
  assert core.center("ab", 5, "-") == "--ab-"
  // left bias
}

pub fn surround_and_unwrap_test() {
  let t = core.surround("hello", "<b>", "</b>")
  assert t == "<b>hello</b>"
  assert core.unwrap(t, "<b>", "</b>") == "hello"
}

pub fn count_overlapping_tests() {
  let s = "aaaa"
  // overlapping: 'aa' occurs at positions 0,1,2 -> 3
  assert core.count(s, "aa", True) == 3
  // non-overlapping: positions 0 and 2 -> 2
  assert core.count(s, "aa", False) == 2
}

pub fn reverse_and_tokenize_tests() {
  let s = "a👩\u{200D}👩b"
  let r = core.reverse(s)
  // reverse twice returns original
  assert core.reverse(r) == s
  // tokenizer returns grapheme clusters
  let t1 = tokenize.chars(s)
  let t2 = tokenize.chars_stdlib(s)
  assert list.length(t1) == list.length(t2)
}

pub fn truncate_preserve_emoji_test() {
  let family = "👩\u{200D}👩\u{200D}👧 family"
  // keep whole emoji cluster when truncating
  let out = core.truncate_preserve(family, 4, "…")
  // ensure we didn't slice into an emoji cluster: output should contain the emoji or be shorter
  assert string.contains(out, "👩") || string.length(out) <= 4
}

pub fn pad_noop_and_words_tests() {
  // pad noop when width <= length
  assert core.pad_left("abcd", 2, "*") == "abcd"

  // words splits various whitespace
  let w = core.words("a\n b\t c")
  assert list.length(w) == 3
}

pub fn truncate_strict_suffix_only_test() {
  // Not enough room for content: should return truncated suffix
  assert core.truncate_strict("hello", 1, "...") == "."
}

pub fn count_empty_needle_test() {
  assert core.count("abc", "", True) == 0
}

pub fn surround_no_unwrap_test() {
  // if prefix+suffix longer than text, unwrap should be no-op
  let s = "x"
  let t = core.surround(s, "<<<", ">>>")
  // Unwrap removes matching prefix and suffix, so expect original inner text
  assert core.unwrap(t, "<<<", ">>>") == s
}

pub fn truncate_simple_test() {
  let input = "Hello, world!"
  assert core.truncate_default(input, 5) == "He..."
}

pub fn truncate_noop_test() {
  let input = "Hi"
  assert core.truncate_default(input, 10) == "Hi"
}

pub fn truncate_unicode_grapheme_test() {
  let input = "Hello 👩🏽‍⚕️ World"
  let out = core.truncate_default(input, 8)
  assert string.contains(out, "👩🏽‍⚕️")
}

pub fn reverse_basic_test() {
  assert core.reverse("abcde") == "edcba"
}

pub fn reverse_combining_test() {
  let e_accent = "e\u{0301}"
  let input = "a" <> e_accent <> "b"
  let rev = core.reverse(input)
  assert rev == "b" <> e_accent <> "a"
}

pub fn reverse_involutive_test() {
  let text = "👨‍👩‍👧‍👦abc"
  assert core.reverse(core.reverse(text)) == text
}

pub fn pad_left_ascii_test() {
  assert core.pad_left("a", 3, " ") == "  a"
}

pub fn pad_right_ascii_test() {
  assert core.pad_right("a", 3, " ") == "a  "
}

pub fn center_ascii_test() {
  assert core.center("a", 3, " ") == " a "
}

pub fn center_left_bias_test() {
  assert core.center("hi", 5, " ") == " hi  "
}

pub fn pad_emoji_test() {
  let result = core.pad_left("x", 3, "😊")
  assert string.contains(result, "😊")
}

pub fn count_non_overlapping_test() {
  assert core.count("aaaa", "aa", False) == 2
}

pub fn count_emoji_test() {
  let t = "👩👩👩"
  assert core.count(t, "👩", True) == 3
}

pub fn words_simple_test() {
  let input = "  Hello   world \nThis\tis  test  "
  let expect = ["Hello", "world", "This", "is", "test"]
  assert core.words(input) == expect
}

pub fn words_unicode_test() {
  let input = "ciao    mondo  😊"
  let expect = ["ciao", "mondo", "😊"]
  assert core.words(input) == expect
}

pub fn words_empty_test() {
  let input = "   "
  let expect: List(String) = []
  assert core.words(input) == expect
}

pub fn surround_basic_test() {
  let s = "world"
  let w = core.surround(s, "Hello ", "!")
  assert w == "Hello world!"
}

pub fn unwrap_basic_test() {
  assert core.unwrap("Hello world!", "Hello ", "!") == "world"
}

pub fn unwrap_emoji_test() {
  let wrapped = core.surround("mid", "👩🏽‍⚕️ ", " 😊")
  assert core.unwrap(wrapped, "👩🏽‍⚕️ ", " 😊") == "mid"
}

pub fn unwrap_missing_prefix_test() {
  assert core.unwrap("hello", "<", ">") == "hello"
}

pub fn truncate_preserve_vs_strict_emoji_test() {
  let input = "Hello 👩🏽‍⚕️ World"
  let preserve = core.truncate_preserve(input, 8, "...")
  assert string.contains(preserve, "👩🏽‍⚕️")

  let strict = core.truncate_strict(input, 8, "...")
  assert !string.contains(strict, "👩🏽‍⚕️")
}

pub fn truncate_preserve_flag_test() {
  let input = "Hello 🇮🇹 World"
  let out = core.truncate_preserve(input, 8, "...")
  assert string.contains(out, "🇮🇹")
}

pub fn truncate_strict_flag_test() {
  let input = "Hello 🇮🇹 World"
  let out = core.truncate_strict(input, 8, "...")
  assert !string.contains(out, "🇮🇹")
}

pub fn truncate_keycap_test() {
  let input = "Num 1️⃣ test"
  let out = core.truncate_preserve(input, 6, "...")
  assert string.contains(out, "1️⃣")
}

pub fn count_keycap_test() {
  let k = "1️⃣1️⃣1️⃣"
  assert core.count(k, "1️⃣", True) == 3
}

pub fn pad_multigrapheme_test() {
  let pad = "🙂👍"
  let out = core.pad_left("x", 3, pad)
  assert out == pad <> pad <> "x"
}

pub fn center_multigrapheme_test() {
  let centered = core.center("x", 5, "ab")
  assert centered == "ababxabab"
}

pub fn tokenize_simple_emoji_test() {
  let s1 = "😊"
  let c1 = tokenize.chars(s1)
  assert list.length(c1) == 1
}

pub fn tokenize_family_emoji_test() {
  let family = "👨‍👩‍👧‍👦"
  let cf = tokenize.chars(family)
  assert list.length(cf) == 1
}

pub fn tokenize_skin_tone_test() {
  let thumbs = "👍🏿"
  let ct = tokenize.chars(thumbs)
  assert list.length(ct) == 1
}

pub fn truncate_suffix_longer_than_max_test() {
  let res = core.truncate("abcd", 2, "!!!")
  assert res == "!!"
}

pub fn reverse_zwj_test() {
  let family = "👨‍👩‍👧‍👦"
  assert core.reverse(family) == family
}

pub fn combined_pad_count_test() {
  let base = "aba"
  let padded = core.pad_left(base, 5, "-")
  assert list.length(tokenize.chars(padded)) == 5

  let doubled = base <> base
  assert core.count(doubled, "a", True) == 4
}

pub fn pad_right_noop_test() {
  assert core.pad_right("hello", 3, "*") == "hello"
}

pub fn center_even_width_test() {
  assert core.center("ab", 6, " ") == "  ab  "
}

pub fn is_blank_empty_test() {
  assert core.is_blank("") == True
}

pub fn is_blank_spaces_test() {
  assert core.is_blank("   ") == True
}

pub fn is_blank_tabs_newlines_test() {
  assert core.is_blank("\t\n\r") == True
}

pub fn is_blank_mixed_whitespace_test() {
  assert core.is_blank("  \t\n  ") == True
}

pub fn is_blank_with_content_test() {
  assert core.is_blank("  hello  ") == False
}

pub fn is_blank_unicode_spaces_test() {
  // Non-breaking space is NOT treated as whitespace by Gleam's string.trim
  // This is consistent with Erlang/BEAM behavior
  assert core.is_blank("\u{00A0}") == False
}

pub fn is_blank_single_char_test() {
  assert core.is_blank("x") == False
}

// ============================================================================
// NEW FUNCTION TESTS
// ============================================================================

// --- take/drop/at tests ---

pub fn take_basic_test() {
  assert core.take("hello", 3) == "hel"
}

pub fn take_emoji_test() {
  assert core.take("👨‍👩‍👧‍👦abc", 2) == "👨‍👩‍👧‍👦a"
}

pub fn take_exceeds_length_test() {
  assert core.take("hi", 10) == "hi"
}

pub fn take_zero_test() {
  assert core.take("hello", 0) == ""
}

pub fn drop_basic_test() {
  assert core.drop("hello", 2) == "llo"
}

pub fn drop_emoji_test() {
  assert core.drop("👨‍👩‍👧‍👦abc", 1) == "abc"
}

pub fn drop_exceeds_length_test() {
  assert core.drop("hi", 10) == ""
}

pub fn drop_zero_test() {
  assert core.drop("hello", 0) == "hello"
}

pub fn at_basic_test() {
  assert core.at("hello", 1) == Ok("e")
}

pub fn at_emoji_test() {
  assert core.at("👨‍👩‍👧‍👦abc", 0) == Ok("👨‍👩‍👧‍👦")
}

pub fn at_out_of_bounds_test() {
  assert core.at("hi", 10) == Error(Nil)
}

pub fn at_negative_test() {
  assert core.at("hello", -1) == Error(Nil)
}

// --- lines/dedent/indent tests ---

pub fn lines_basic_test() {
  assert core.lines("a\nb\nc") == ["a", "b", "c"]
}

pub fn lines_crlf_test() {
  assert core.lines("a\r\nb\r\nc") == ["a", "b", "c"]
}

pub fn lines_single_test() {
  assert core.lines("hello") == ["hello"]
}

pub fn dedent_basic_test() {
  assert core.dedent("  a\n  b\n  c") == "a\nb\nc"
}

pub fn dedent_mixed_indent_test() {
  assert core.dedent("    hello\n    world") == "hello\nworld"
}

pub fn dedent_no_indent_test() {
  assert core.dedent("hello\nworld") == "hello\nworld"
}

pub fn indent_basic_test() {
  assert core.indent("hello\nworld", 2) == "  hello\n  world"
}

pub fn indent_single_line_test() {
  assert core.indent("hi", 4) == "    hi"
}

// --- wrap_at/ellipsis tests ---

pub fn wrap_at_basic_test() {
  let result = core.wrap_at("hello world foo bar", 11)
  assert string.contains(result, "\n")
}

pub fn wrap_at_no_wrap_needed_test() {
  assert core.wrap_at("hello", 100) == "hello"
}

pub fn wrap_at_zero_width_test() {
  assert core.wrap_at("hello", 0) == "hello"
}

pub fn ellipsis_basic_test() {
  let result = core.ellipsis("Hello World", 8)
  assert string.ends_with(result, "…")
}

// --- strip/squeeze/chomp tests ---

pub fn strip_basic_test() {
  assert core.strip("..hello..", ".") == "hello"
}
