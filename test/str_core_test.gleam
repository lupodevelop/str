import gleam/list
import gleam/string
import str

pub fn pad_and_center_tests() {
  assert str.pad_left("x", 3, "*") == "**x"
  assert str.pad_right("x", 3, "*") == "x**"
  assert str.center("ab", 5, "-") == "--ab-"
  // left bias
}

pub fn surround_and_unwrap_test() {
  let t = str.surround("hello", "<b>", "</b>")
  assert t == "<b>hello</b>"
  assert str.unwrap(t, "<b>", "</b>") == "hello"
}

pub fn count_overlapping_tests() {
  let s = "aaaa"
  // overlapping: 'aa' occurs at positions 0,1,2 -> 3
  assert str.count(s, "aa", True) == 3
  // non-overlapping: positions 0 and 2 -> 2
  assert str.count(s, "aa", False) == 2
}

pub fn reverse_and_tokenize_tests() {
  let s = "a👩\u{200D}👩b"
  let r = str.reverse(s)
  // reverse twice returns original
  assert str.reverse(r) == s
  // tokenizer returns grapheme clusters
  let t1 = str.chars(s)
  let t2 = str.chars_stdlib(s)
  assert list.length(t1) == list.length(t2)
}

pub fn truncate_preserve_emoji_test() {
  let family = "👩\u{200D}👩\u{200D}👧 family"
  // keep whole emoji cluster when truncating
  let out = str.truncate_preserve(family, 4, "…")
  // ensure we didn't slice into an emoji cluster: output should contain the emoji or be shorter
  assert string.contains(out, "👩") || string.length(out) <= 4
}

pub fn pad_noop_and_words_tests() {
  // pad noop when width <= length
  assert str.pad_left("abcd", 2, "*") == "abcd"

  // words splits various whitespace
  let w = str.words("a\n b\t c")
  assert list.length(w) == 3
}

pub fn truncate_strict_suffix_only_test() {
  // Not enough room for content: should return truncated suffix
  assert str.truncate_strict("hello", 1, "...") == "."
}

pub fn count_empty_needle_test() {
  assert str.count("abc", "", True) == 0
}

pub fn surround_no_unwrap_test() {
  // if prefix+suffix longer than text, unwrap should be no-op
  let s = "x"
  let t = str.surround(s, "<<", ">>")
  assert str.unwrap(t, "<<<", ">>>") == t
}

pub fn truncate_simple_test() {
  let input = "Hello, world!"
  assert str.truncate_default(input, 5) == "He..."
}

pub fn truncate_noop_test() {
  let input = "Hi"
  assert str.truncate_default(input, 10) == "Hi"
}

pub fn truncate_unicode_grapheme_test() {
  let input = "Hello 👩🏽‍⚕️ World"
  let out = str.truncate_default(input, 8)
  assert string.contains(out, "👩🏽‍⚕️")
}

pub fn reverse_basic_test() {
  assert str.reverse("abcde") == "edcba"
}

pub fn reverse_combining_test() {
  let e_accent = "e\u{0301}"
  let input = "a" <> e_accent <> "b"
  let rev = str.reverse(input)
  assert rev == "b" <> e_accent <> "a"
}

pub fn reverse_involutive_test() {
  let text = "👨‍👩‍👧‍👦abc"
  assert str.reverse(str.reverse(text)) == text
}

pub fn pad_left_ascii_test() {
  assert str.pad_left("a", 3, " ") == "  a"
}

pub fn pad_right_ascii_test() {
  assert str.pad_right("a", 3, " ") == "a  "
}

pub fn center_ascii_test() {
  assert str.center("a", 3, " ") == " a "
}

pub fn center_left_bias_test() {
  assert str.center("hi", 5, " ") == " hi  "
}

pub fn pad_emoji_test() {
  let result = str.pad_left("x", 3, "😊")
  assert string.contains(result, "😊")
}

pub fn count_non_overlapping_test() {
  assert str.count("aaaa", "aa", False) == 2
}

pub fn count_emoji_test() {
  let t = "👩👩👩"
  assert str.count(t, "👩", True) == 3
}

pub fn words_simple_test() {
  let input = "  Hello   world \nThis\tis  test  "
  let expect = ["Hello", "world", "This", "is", "test"]
  assert str.words(input) == expect
}

pub fn words_unicode_test() {
  let input = "ciao    mondo  😊"
  let expect = ["ciao", "mondo", "😊"]
  assert str.words(input) == expect
}

pub fn words_empty_test() {
  let input = "   "
  let expect: List(String) = []
  assert str.words(input) == expect
}

pub fn surround_basic_test() {
  let s = "world"
  let w = str.surround(s, "Hello ", "!")
  assert w == "Hello world!"
}

pub fn unwrap_basic_test() {
  assert str.unwrap("Hello world!", "Hello ", "!") == "world"
}

pub fn unwrap_emoji_test() {
  let wrapped = str.surround("mid", "👩🏽‍⚕️ ", " 😊")
  assert str.unwrap(wrapped, "👩🏽‍⚕️ ", " 😊") == "mid"
}

pub fn unwrap_missing_prefix_test() {
  assert str.unwrap("hello", "<", ">") == "hello"
}

pub fn truncate_preserve_vs_strict_emoji_test() {
  let input = "Hello 👩🏽‍⚕️ World"
  let preserve = str.truncate_preserve(input, 8, "...")
  assert string.contains(preserve, "👩🏽‍⚕️")

  let strict = str.truncate_strict(input, 8, "...")
  assert !string.contains(strict, "👩🏽‍⚕️")
}

pub fn truncate_preserve_flag_test() {
  let input = "Hello 🇮🇹 World"
  let out = str.truncate_preserve(input, 8, "...")
  assert string.contains(out, "🇮🇹")
}

pub fn truncate_strict_flag_test() {
  let input = "Hello 🇮🇹 World"
  let out = str.truncate_strict(input, 8, "...")
  assert !string.contains(out, "🇮🇹")
}

pub fn truncate_keycap_test() {
  let input = "Num 1️⃣ test"
  let out = str.truncate_preserve(input, 6, "...")
  assert string.contains(out, "1️⃣")
}

pub fn count_keycap_test() {
  let k = "1️⃣1️⃣1️⃣"
  assert str.count(k, "1️⃣", True) == 3
}

pub fn pad_multigrapheme_test() {
  let pad = "🙂👍"
  let out = str.pad_left("x", 3, pad)
  assert out == pad <> pad <> "x"
}

pub fn center_multigrapheme_test() {
  let centered = str.center("x", 5, "ab")
  assert centered == "ababxabab"
}

pub fn tokenize_simple_emoji_test() {
  let s1 = "😊"
  let c1 = str.chars(s1)
  assert list.length(c1) == 1
}

pub fn tokenize_family_emoji_test() {
  let family = "👨‍👩‍👧‍👦"
  let cf = str.chars(family)
  assert list.length(cf) == 1
}

pub fn tokenize_skin_tone_test() {
  let thumbs = "👍🏿"
  let ct = str.chars(thumbs)
  assert list.length(ct) == 1
}

pub fn truncate_suffix_longer_than_max_test() {
  let res = str.truncate("abcd", 2, "!!!")
  assert res == "!!"
}

pub fn reverse_zwj_test() {
  let family = "👨‍👩‍👧‍👦"
  assert str.reverse(family) == family
}

pub fn combined_pad_count_test() {
  let base = "aba"
  let padded = str.pad_left(base, 5, "-")
  assert list.length(str.chars(padded)) == 5

  let doubled = base <> base
  assert str.count(doubled, "a", True) == 4
}

pub fn pad_right_noop_test() {
  assert str.pad_right("hello", 3, "*") == "hello"
}

pub fn center_even_width_test() {
  assert str.center("ab", 6, " ") == "  ab  "
}

pub fn is_blank_empty_test() {
  assert str.is_blank("") == True
}

pub fn is_blank_spaces_test() {
  assert str.is_blank("   ") == True
}

pub fn is_blank_tabs_newlines_test() {
  assert str.is_blank("\t\n\r") == True
}

pub fn is_blank_mixed_whitespace_test() {
  assert str.is_blank("  \t\n  ") == True
}

pub fn is_blank_with_content_test() {
  assert str.is_blank("  hello  ") == False
}

pub fn is_blank_unicode_spaces_test() {
  // Non-breaking space is NOT treated as whitespace by Gleam's string.trim
  // This is consistent with Erlang/BEAM behavior
  assert str.is_blank("\u{00A0}") == False
}

pub fn is_blank_single_char_test() {
  assert str.is_blank("x") == False
}

// ============================================================================
// NEW FUNCTION TESTS
// ============================================================================

// --- take/drop/at tests ---

pub fn take_basic_test() {
  assert str.take("hello", 3) == "hel"
}

pub fn take_emoji_test() {
  assert str.take("👨‍👩‍👧‍👦abc", 2) == "👨‍👩‍👧‍👦a"
}

pub fn take_exceeds_length_test() {
  assert str.take("hi", 10) == "hi"
}

pub fn take_zero_test() {
  assert str.take("hello", 0) == ""
}

pub fn drop_basic_test() {
  assert str.drop("hello", 2) == "llo"
}

pub fn drop_emoji_test() {
  assert str.drop("👨‍👩‍👧‍👦abc", 1) == "abc"
}

pub fn drop_exceeds_length_test() {
  assert str.drop("hi", 10) == ""
}

pub fn drop_zero_test() {
  assert str.drop("hello", 0) == "hello"
}

pub fn at_basic_test() {
  assert str.at("hello", 1) == Ok("e")
}

pub fn at_emoji_test() {
  assert str.at("👨‍👩‍👧‍👦abc", 0) == Ok("👨‍👩‍👧‍👦")
}

pub fn at_out_of_bounds_test() {
  assert str.at("hi", 10) == Error(Nil)
}

pub fn at_negative_test() {
  assert str.at("hello", -1) == Error(Nil)
}

// --- lines/dedent/indent tests ---

pub fn lines_basic_test() {
  assert str.lines("a\nb\nc") == ["a", "b", "c"]
}

pub fn lines_crlf_test() {
  assert str.lines("a\r\nb\r\nc") == ["a", "b", "c"]
}

pub fn lines_single_test() {
  assert str.lines("hello") == ["hello"]
}

pub fn dedent_basic_test() {
  assert str.dedent("  a\n  b\n  c") == "a\nb\nc"
}

pub fn dedent_mixed_indent_test() {
  assert str.dedent("    hello\n    world") == "hello\nworld"
}

pub fn dedent_no_indent_test() {
  assert str.dedent("hello\nworld") == "hello\nworld"
}

pub fn indent_basic_test() {
  assert str.indent("hello\nworld", 2) == "  hello\n  world"
}

pub fn indent_single_line_test() {
  assert str.indent("hi", 4) == "    hi"
}

// --- wrap_at/ellipsis tests ---

pub fn wrap_at_basic_test() {
  let result = str.wrap_at("hello world foo bar", 11)
  assert string.contains(result, "\n")
}

pub fn wrap_at_no_wrap_needed_test() {
  assert str.wrap_at("hello", 100) == "hello"
}

pub fn wrap_at_zero_width_test() {
  assert str.wrap_at("hello", 0) == "hello"
}

pub fn wrap_at_emoji_grapheme_test() {
  // Ensure grapheme-aware wrapping treats emoji as single units
  let s = "a 👨‍👩‍👧‍👦 b"
  // Width 2 should force a newline between "a" and the emoji
  assert string.contains(str.wrap_at(s, 2), "\n")
}

pub fn grapple_len_behavior_test() {
  // Verify grapheme-aware counting on representative cases
  assert str.length("") == 0
  assert str.length("abc") == 3
  // a + combining acute accent should be one grapheme
  assert str.length("a\u{0301}") == 1
  // Regional indicator flag (two codepoints) is a single grapheme
  assert str.length("🇮🇹") == 1
  // Family ZWJ sequence should be one grapheme cluster
  assert str.length("👨‍👩‍👧‍👦") == 1

  // Stress: long ASCII string should return its length
  let long = list.fold(list.range(1, 1000), "", fn(acc, _) { acc <> "x" })
  assert str.length(long) == 1000
}

pub fn ellipsis_basic_test() {
  let result = str.ellipsis("Hello World", 8)
  assert string.ends_with(result, "…")
}

// --- strip/squeeze/chomp tests ---

pub fn strip_basic_test() {
  assert str.strip("..hello..", ".") == "hello"
}

pub fn strip_multiple_chars_test() {
  assert str.strip("xxhelloxx", "x") == "hello"
}

pub fn strip_no_match_test() {
  assert str.strip("hello", "x") == "hello"
}

pub fn squeeze_basic_test() {
  assert str.squeeze("heeello", "e") == "hello"
}

pub fn squeeze_spaces_test() {
  assert str.squeeze("   hello   world   ", " ") == " hello world "
}

pub fn squeeze_no_consecutive_test() {
  assert str.squeeze("hello", "l") == "helo"
}

pub fn chomp_newline_test() {
  assert str.chomp("hello\n") == "hello"
}

pub fn chomp_crlf_test() {
  assert str.chomp("hello\r\n") == "hello"
  assert str.chomp("hi\r\n") == "hi"
}

pub fn chomp_no_newline_test() {
  assert str.chomp("hello") == "hello"
}

// --- partition tests ---

pub fn partition_basic_test() {
  assert str.partition("a-b-c", "-") == #("a", "-", "b-c")
}

pub fn partition_no_match_test() {
  assert str.partition("hello", "-") == #("hello", "", "")
}

// --- common_prefix/suffix tests ---

pub fn common_prefix_basic_test() {
  assert str.common_prefix(["abc", "abd", "abe"]) == "ab"
}

pub fn common_prefix_no_common_test() {
  assert str.common_prefix(["hello", "world"]) == ""
}

pub fn common_prefix_empty_list_test() {
  assert str.common_prefix([]) == ""
}

pub fn common_prefix_single_test() {
  assert str.common_prefix(["hello"]) == "hello"
}

pub fn common_suffix_basic_test() {
  assert str.common_suffix(["abc", "xbc", "zbc"]) == "bc"
}

pub fn common_suffix_no_common_test() {
  assert str.common_suffix(["hello", "world"]) == ""
}

// --- is_numeric/alpha/alphanumeric tests ---

pub fn is_numeric_true_test() {
  assert str.is_numeric("12345") == True
}

pub fn is_numeric_false_test() {
  assert str.is_numeric("123.45") == False
}

pub fn is_numeric_empty_test() {
  assert str.is_numeric("") == False
}

pub fn is_alpha_true_test() {
  assert str.is_alpha("hello") == True
}

pub fn is_alpha_mixed_case_test() {
  assert str.is_alpha("HeLLo") == True
}

pub fn is_alpha_with_numbers_test() {
  assert str.is_alpha("hello123") == False
}

pub fn is_alpha_empty_test() {
  assert str.is_alpha("") == False
}

pub fn is_alphanumeric_true_test() {
  assert str.is_alphanumeric("hello123") == True
}

pub fn is_alphanumeric_with_special_test() {
  assert str.is_alphanumeric("hello-world") == False
}

pub fn is_alphanumeric_empty_test() {
  assert str.is_alphanumeric("") == False
}

// --- remove/ensure prefix/suffix tests ---

pub fn remove_prefix_present_test() {
  assert str.remove_prefix("hello world", "hello ") == "world"
}

pub fn remove_prefix_absent_test() {
  assert str.remove_prefix("hello", "bye") == "hello"
}

pub fn remove_suffix_present_test() {
  assert str.remove_suffix("hello world", " world") == "hello"
}

pub fn remove_suffix_absent_test() {
  assert str.remove_suffix("hello", "bye") == "hello"
}

pub fn ensure_prefix_absent_test() {
  assert str.ensure_prefix("world", "hello ") == "hello world"
}

pub fn ensure_prefix_present_test() {
  assert str.ensure_prefix("hello world", "hello ") == "hello world"
}

pub fn remove_prefix_emoji_test() {
  assert str.remove_prefix("👨‍👩‍👧‍👦 family", "👨‍👩‍👧‍👦") == " family"
}

pub fn remove_suffix_emoji_test() {
  assert str.remove_suffix("family 👨‍👩‍👧‍👦", "👨‍👩‍👧‍👦") == "family "
}

pub fn ensure_prefix_emoji_test() {
  assert str.ensure_prefix("family", "👨‍👩‍👧‍👦 ") == "👨‍👩‍👧‍👦 family"
  assert str.ensure_prefix("👨‍👩‍👧‍👦 family", "👨‍👩‍👧‍👦 ") == "👨‍👩‍👧‍👦 family"
}

pub fn ensure_suffix_emoji_test() {
  assert str.ensure_suffix("family", " 👨‍👩‍👧‍👦") == "family 👨‍👩‍👧‍👦"
  assert str.ensure_suffix("family 👨‍👩‍👧‍👦", " 👨‍👩‍👧‍👦") == "family 👨‍👩‍👧‍👦"
}

pub fn ensure_suffix_absent_test() {
  assert str.ensure_suffix("hello", " world") == "hello world"
}

pub fn ensure_suffix_present_test() {
  assert str.ensure_suffix("hello world", " world") == "hello world"
}

// --- swapcase tests ---

pub fn swapcase_basic_test() {
  assert str.swapcase("Hello World") == "hELLO wORLD"
}

pub fn swapcase_all_upper_test() {
  assert str.swapcase("ABC") == "abc"
}

pub fn swapcase_all_lower_test() {
  assert str.swapcase("abc") == "ABC"
}

// --- distance tests ---

pub fn distance_same_test() {
  assert str.distance("hello", "hello") == 0
}

pub fn distance_empty_test() {
  assert str.distance("", "abc") == 3
}

pub fn distance_kitten_sitting_test() {
  assert str.distance("kitten", "sitting") == 3
}

pub fn distance_single_char_test() {
  assert str.distance("a", "b") == 1
}

// ============================================================================
// NEW FUNCTIONS TESTS (v2.1.0)
// ============================================================================

// --- index_of tests ---

pub fn index_of_basic_test() {
  assert str.index_of("hello world", "world") == Ok(6)
}

pub fn index_of_not_found_test() {
  assert str.index_of("hello", "x") == Error(Nil)
}

pub fn index_of_emoji_test() {
  assert str.index_of("👨‍👩‍👧‍👦 family", "family") == Ok(2)
}

pub fn index_of_start_test() {
  assert str.index_of("hello", "hello") == Ok(0)
}

pub fn index_of_empty_needle_test() {
  assert str.index_of("hello", "") == Error(Nil)
}

// --- last_index_of tests ---

pub fn last_index_of_basic_test() {
  assert str.last_index_of("hello hello", "hello") == Ok(6)
}

pub fn last_index_of_not_found_test() {
  assert str.last_index_of("hello", "x") == Error(Nil)
}

pub fn last_index_of_single_test() {
  assert str.last_index_of("hello", "hello") == Ok(0)
}

pub fn last_index_of_separator_test() {
  assert str.last_index_of("a-b-c", "-") == Ok(3)
}

// --- contains_any tests ---

pub fn contains_any_found_test() {
  assert str.contains_any("hello world", ["foo", "world"]) == True
}

pub fn contains_any_not_found_test() {
  assert str.contains_any("hello", ["x", "y", "z"]) == False
}

pub fn contains_any_empty_list_test() {
  assert str.contains_any("test", []) == False
}

// --- contains_all tests ---

pub fn contains_all_true_test() {
  assert str.contains_all("hello world", ["hello", "world"]) == True
}

pub fn contains_all_false_test() {
  assert str.contains_all("hello", ["hello", "x"]) == False
}

pub fn contains_all_empty_list_test() {
  assert str.contains_all("test", []) == True
}

// --- replace_first tests ---

pub fn replace_first_basic_test() {
  assert str.replace_first("hello hello", "hello", "hi") == "hi hello"
}

pub fn replace_first_single_test() {
  assert str.replace_first("aaa", "a", "b") == "baa"
}

pub fn replace_first_not_found_test() {
  assert str.replace_first("test", "x", "y") == "test"
}

// --- replace_last tests ---

pub fn replace_last_basic_test() {
  assert str.replace_last("hello hello", "hello", "hi") == "hello hi"
}

pub fn replace_last_single_test() {
  assert str.replace_last("aaa", "a", "b") == "aab"
}

pub fn replace_last_not_found_test() {
  assert str.replace_last("test", "x", "y") == "test"
}

// --- is_uppercase tests ---

pub fn is_uppercase_true_test() {
  assert str.is_uppercase("HELLO") == True
}

pub fn is_uppercase_false_test() {
  assert str.is_uppercase("Hello") == False
}

pub fn is_uppercase_with_numbers_test() {
  assert str.is_uppercase("HELLO123") == True
}

pub fn is_uppercase_only_numbers_test() {
  assert str.is_uppercase("123") == False
}

pub fn is_uppercase_empty_test() {
  assert str.is_uppercase("") == False
}

// --- is_lowercase tests ---

pub fn is_lowercase_true_test() {
  assert str.is_lowercase("hello") == True
}

pub fn is_lowercase_false_test() {
  assert str.is_lowercase("Hello") == False
}

pub fn is_lowercase_with_numbers_test() {
  assert str.is_lowercase("hello123") == True
}

pub fn is_lowercase_only_numbers_test() {
  assert str.is_lowercase("123") == False
}

pub fn is_lowercase_empty_test() {
  assert str.is_lowercase("") == False
}

// --- is_ascii tests ---

pub fn is_ascii_true_test() {
  assert str.is_ascii("hello") == True
}

pub fn is_ascii_with_symbols_test() {
  assert str.is_ascii("hello!@#") == True
}

pub fn is_ascii_false_test() {
  assert str.is_ascii("café") == False
}

pub fn is_ascii_emoji_test() {
  assert str.is_ascii("👋") == False
}

pub fn is_ascii_empty_test() {
  assert str.is_ascii("") == True
}

// --- is_printable tests ---

pub fn is_printable_true_test() {
  assert str.is_printable("hello") == True
}

pub fn is_printable_newline_test() {
  assert str.is_printable("hello\n") == False
}

pub fn is_printable_tab_test() {
  assert str.is_printable("hello\t") == False
}

pub fn is_printable_empty_test() {
  assert str.is_printable("") == True
}

// --- is_hex tests ---

pub fn is_hex_lowercase_test() {
  assert str.is_hex("abc123") == True
}

pub fn is_hex_uppercase_test() {
  assert str.is_hex("DEADBEEF") == True
}

pub fn is_hex_invalid_test() {
  assert str.is_hex("xyz") == False
}

pub fn is_hex_empty_test() {
  assert str.is_hex("") == False
}

// --- escape_html tests ---

pub fn escape_html_tags_test() {
  assert str.escape_html("<div>Hello</div>") == "&lt;div&gt;Hello&lt;/div&gt;"
}

pub fn escape_html_ampersand_test() {
  assert str.escape_html("Tom & Jerry") == "Tom &amp; Jerry"
}

pub fn escape_html_quotes_test() {
  assert str.escape_html("Say \"hello\"") == "Say &quot;hello&quot;"
}

pub fn escape_html_single_quote_test() {
  assert str.escape_html("It's") == "It&#39;s"
}

// --- unescape_html tests ---

pub fn unescape_html_tags_test() {
  assert str.unescape_html("&lt;div&gt;") == "<div>"
}

pub fn unescape_html_ampersand_test() {
  assert str.unescape_html("Tom &amp; Jerry") == "Tom & Jerry"
}

pub fn unescape_html_quotes_test() {
  assert str.unescape_html("Say &quot;hello&quot;") == "Say \"hello\""
}

// --- escape_regex tests ---

pub fn escape_regex_dot_test() {
  assert str.escape_regex("hello.world") == "hello\\.world"
}

pub fn escape_regex_brackets_test() {
  assert str.escape_regex("[test]") == "\\[test\\]"
}

pub fn escape_regex_quantifiers_test() {
  assert str.escape_regex("a+b*c?") == "a\\+b\\*c\\?"
}

pub fn escape_regex_anchors_test() {
  assert str.escape_regex("^start$end") == "\\^start\\$end"
}

// --- similarity tests ---

pub fn similarity_identical_test() {
  let result = str.similarity("hello", "hello")
  assert result == 1.0
}

pub fn similarity_one_diff_test() {
  let result = str.similarity("hello", "hallo")
  assert result == 0.8
}

pub fn similarity_totally_diff_test() {
  let result = str.similarity("abc", "xyz")
  assert result == 0.0
}

pub fn similarity_empty_test() {
  let result = str.similarity("", "")
  assert result == 1.0
}

// --- hamming_distance tests ---

pub fn hamming_distance_basic_test() {
  assert str.hamming_distance("karolin", "kathrin") == Ok(3)
}

pub fn hamming_distance_one_diff_test() {
  assert str.hamming_distance("hello", "hallo") == Ok(1)
}

pub fn hamming_distance_diff_length_test() {
  assert str.hamming_distance("abc", "ab") == Error(Nil)
}

pub fn hamming_distance_same_test() {
  assert str.hamming_distance("abc", "abc") == Ok(0)
}

// --- take_right tests ---

pub fn take_right_basic_test() {
  assert str.take_right("hello", 3) == "llo"
}

pub fn take_right_emoji_test() {
  assert str.take_right("👨‍👩‍👧‍👦abc", 2) == "bc"
}

pub fn take_right_exceeds_test() {
  assert str.take_right("hi", 10) == "hi"
}

pub fn take_right_zero_test() {
  assert str.take_right("hello", 0) == ""
}

// --- drop_right tests ---

pub fn drop_right_basic_test() {
  assert str.drop_right("hello", 2) == "hel"
}

pub fn drop_right_emoji_test() {
  assert str.drop_right("👨‍👩‍👧‍👦abc", 2) == "👨‍👩‍👧‍👦a"
}

pub fn drop_right_exceeds_test() {
  assert str.drop_right("hi", 10) == ""
}

pub fn drop_right_zero_test() {
  assert str.drop_right("hello", 0) == "hello"
}

// --- reverse_words tests ---

pub fn reverse_words_basic_test() {
  assert str.reverse_words("hello world") == "world hello"
}

pub fn reverse_words_three_test() {
  assert str.reverse_words("one two three") == "three two one"
}

pub fn reverse_words_single_test() {
  assert str.reverse_words("single") == "single"
}

pub fn reverse_words_empty_test() {
  assert str.reverse_words("") == ""
}

// --- initials tests ---

pub fn initials_basic_test() {
  assert str.initials("John Doe") == "JD"
}

pub fn initials_lowercase_test() {
  assert str.initials("visual studio code") == "VSC"
}

pub fn initials_single_word_test() {
  assert str.initials("hello") == "H"
}

pub fn initials_empty_test() {
  assert str.initials("") == ""
}

// ============================================================================
// capitalize tests
// ============================================================================

pub fn capitalize_basic_test() {
  assert str.capitalize("hello") == "Hello"
}

pub fn capitalize_mixed_case_test() {
  assert str.capitalize("hELLO wORLD") == "Hello world"
}

pub fn capitalize_empty_test() {
  assert str.capitalize("") == ""
}

pub fn capitalize_single_char_test() {
  assert str.capitalize("a") == "A"
}

pub fn capitalize_already_capitalized_test() {
  assert str.capitalize("Hello") == "Hello"
}

pub fn capitalize_emoji_prefix_test() {
  // Emoji at start: should uppercase emoji (no-op) and lowercase rest
  assert str.capitalize("👋 HELLO") == "👋 hello"
}

// ============================================================================
// rpartition tests
// ============================================================================

pub fn rpartition_basic_test() {
  assert str.rpartition("a-b-c", "-") == #("a-b", "-", "c")
}

pub fn rpartition_not_found_test() {
  assert str.rpartition("hello", "-") == #("", "", "hello")
}

pub fn rpartition_multi_char_sep_test() {
  assert str.rpartition("one::two::three", "::") == #("one::two", "::", "three")
}

pub fn rpartition_single_occurrence_test() {
  assert str.rpartition("hello-world", "-") == #("hello", "-", "world")
}

pub fn rpartition_at_start_test() {
  assert str.rpartition("-hello", "-") == #("", "-", "hello")
}

pub fn rpartition_at_end_test() {
  assert str.rpartition("hello-", "-") == #("hello", "-", "")
}

// ============================================================================
// splitn tests
// ============================================================================

pub fn splitn_basic_test() {
  assert str.splitn("a-b-c-d", "-", 2) == ["a", "b-c-d"]
}

pub fn splitn_three_test() {
  assert str.splitn("a-b-c-d", "-", 3) == ["a", "b", "c-d"]
}

pub fn splitn_no_sep_test() {
  assert str.splitn("hello", "-", 5) == ["hello"]
}

pub fn splitn_zero_test() {
  assert str.splitn("a-b-c", "-", 0) == []
}

pub fn splitn_one_test() {
  assert str.splitn("a-b-c", "-", 1) == ["a-b-c"]
}

pub fn splitn_exceeds_parts_test() {
  assert str.splitn("a-b", "-", 10) == ["a", "b"]
}

// ============================================================================
// is_title_case tests
// ============================================================================

pub fn is_title_case_true_test() {
  assert str.is_title_case("Hello World") == True
}

pub fn is_title_case_false_lowercase_second_test() {
  assert str.is_title_case("Hello world") == False
}

pub fn is_title_case_all_caps_test() {
  assert str.is_title_case("HELLO WORLD") == False
}

pub fn is_title_case_single_word_test() {
  assert str.is_title_case("Hello") == True
}

pub fn is_title_case_empty_test() {
  assert str.is_title_case("") == False
}

pub fn is_title_case_with_numbers_test() {
  assert str.is_title_case("Hello 123 World") == True
}

pub fn is_title_case_with_emoji_test() {
  assert str.is_title_case("Hello 👋 World") == True
  assert str.is_title_case("👋 Hello World") == True
  assert str.is_title_case("Hello World 🎉") == True
  // Only emoji - no cased words, should be False
  assert str.is_title_case("🎉 👋 🌍") == False
}

// ============================================================================
// fill tests
// ============================================================================

pub fn fill_left_test() {
  assert str.fill("42", 5, "0", str.Left) == "00042"
}

pub fn fill_right_test() {
  assert str.fill("hi", 6, "*", str.Right) == "hi****"
}

pub fn fill_both_test() {
  assert str.fill("x", 5, "-", str.Both) == "--x--"
}

// ============================================================================
// chunk tests
// ============================================================================

pub fn chunk_basic_test() {
  assert str.chunk("abcdefg", 2) == ["ab", "cd", "ef", "g"]
}

pub fn chunk_three_test() {
  assert str.chunk("hello", 3) == ["hel", "lo"]
}

pub fn chunk_emoji_test() {
  assert str.chunk("👨‍👩‍👧‍👦abc", 2) == ["👨‍👩‍👧‍👦a", "bc"]
}

pub fn chunk_larger_than_text_test() {
  assert str.chunk("hi", 10) == ["hi"]
}

pub fn chunk_zero_size_test() {
  assert str.chunk("hello", 0) == []
}

pub fn chunk_single_test() {
  assert str.chunk("abc", 1) == ["a", "b", "c"]
}

// ============================================================================
// starts_with_any tests
// ============================================================================

pub fn starts_with_any_true_test() {
  assert str.starts_with_any("hello", ["hi", "he", "ho"]) == True
}

pub fn starts_with_any_false_test() {
  assert str.starts_with_any("hello", ["bye", "world"]) == False
}

pub fn starts_with_any_empty_list_test() {
  assert str.starts_with_any("test", []) == False
}

pub fn starts_with_any_exact_match_test() {
  assert str.starts_with_any("hello", ["hello"]) == True
}

// ============================================================================
// ends_with_any tests
// ============================================================================

pub fn ends_with_any_true_test() {
  assert str.ends_with_any("hello.txt", [".txt", ".md", ".gleam"]) == True
}

pub fn ends_with_any_false_test() {
  assert str.ends_with_any("hello", ["bye", "world"]) == False
}

pub fn ends_with_any_empty_list_test() {
  assert str.ends_with_any("test", []) == False
}

pub fn ends_with_any_exact_match_test() {
  assert str.ends_with_any("hello", ["hello"]) == True
}

// ============================================================================
// normalize_whitespace tests
// ============================================================================

pub fn normalize_whitespace_basic_test() {
  assert str.normalize_whitespace("hello   world") == "hello world"
}

pub fn normalize_whitespace_tabs_newlines_test() {
  assert str.normalize_whitespace("a\t\nb") == "a b"
}

pub fn normalize_whitespace_leading_trailing_test() {
  assert str.normalize_whitespace("  foo  bar  baz  ") == "foo bar baz"
}

pub fn normalize_whitespace_single_word_test() {
  assert str.normalize_whitespace("hello") == "hello"
}

pub fn normalize_whitespace_empty_test() {
  assert str.normalize_whitespace("") == ""
}

pub fn normalize_whitespace_only_spaces_test() {
  assert str.normalize_whitespace("     ") == ""
}
