/// FFI Parity Tests
///
/// Verifies that native FFI implementations produce identical results to pure Gleam
/// implementations across various Unicode inputs and edge cases.
///
/// These tests call the pure and native implementations directly to ensure parity
/// regardless of the config flag state.
///
/// NOTE: decompose_latin differs between pure and native:
/// - Pure: limited decomposition table (pragmatic subset for common Latin chars)
/// - Native: full NFD decomposition via OTP unicode module
/// Therefore decompose tests verify pure ⊆ native (not strict equality).

import gleam/list
import str/internal/translit_pure
import str/internal/translit_native

// ============================================================================
// Transliteration Table Parity Tests
// ============================================================================

pub fn translit_replacements_parity_test() {
  let pure_table = translit_pure.replacements_pure()
  let native_table = translit_native.replacements_native()
  
  // Tables should have the same length
  assert list.length(pure_table) == list.length(native_table)
  
  // Every entry in pure table should exist in native table
  list.each(pure_table, fn(pure_entry) {
    let #(from, to) = pure_entry
    assert list.contains(native_table, #(from, to))
  })
}

pub fn translit_replacements_common_chars_test() {
  let pure_table = translit_pure.replacements_pure()
  let native_table = translit_native.replacements_native()
  
  // Test common transliterations are present in both
  let common = [
    #("é", "e"),
    #("ü", "u"),
    #("ñ", "n"),
    #("ø", "o"),
    #("ł", "l"),
    #("ß", "ss"),
    #("æ", "ae"),
    #("œ", "oe"),
  ]
  
  list.each(common, fn(entry) {
    assert list.contains(pure_table, entry)
    assert list.contains(native_table, entry)
  })
}

pub fn remove_combining_marks_basic_test() {
  let inputs = [
    "e\u{0301}",
    // e + acute accent
    "n\u{0303}",
    // n + tilde
    "u\u{0308}",
    // u + diaeresis
    "a\u{0300}",
    // a + grave
  ]
  
  list.each(inputs, fn(s) {
    let pure_result = translit_pure.remove_combining_marks_pure(s)
    let native_result = translit_native.remove_combining_marks_native(s)
    assert pure_result == native_result
  })
}

pub fn remove_combining_marks_multiple_test() {
  let inputs = [
    "e\u{0301}\u{0302}",
    // e + acute + circumflex
    "a\u{0300}\u{0308}",
    // a + grave + diaeresis
  ]
  
  list.each(inputs, fn(s) {
    let pure_result = translit_pure.remove_combining_marks_pure(s)
    let native_result = translit_native.remove_combining_marks_native(s)
    assert pure_result == native_result
  })
}

pub fn remove_combining_marks_mixed_text_test() {
  let inputs = [
    "Hello e\u{0301}",
    "Cafe\u{0301} au lait",
    "Test \u{0301} combining",
  ]
  
  list.each(inputs, fn(s) {
    let pure_result = translit_pure.remove_combining_marks_pure(s)
    let native_result = translit_native.remove_combining_marks_native(s)
    assert pure_result == native_result
  })
}

pub fn remove_combining_marks_no_marks_test() {
  let inputs = ["Hello", "World", "Café", "Test123"]
  
  list.each(inputs, fn(s) {
    let pure_result = translit_pure.remove_combining_marks_pure(s)
    let native_result = translit_native.remove_combining_marks_native(s)
    assert pure_result == native_result
    assert pure_result == s
  })
}

pub fn remove_combining_marks_empty_test() {
  let pure_result = translit_pure.remove_combining_marks_pure("")
  let native_result = translit_native.remove_combining_marks_native("")
  assert pure_result == native_result
  assert pure_result == ""
}

pub fn remove_combining_marks_ranges_test() {
  // Test different Unicode combining mark ranges
  let inputs = [
    "a\u{0300}",
    // U+0300-036F: Combining Diacritical Marks
    "b\u{1AB0}",
    // U+1AB0-1AFF: Combining Diacritical Marks Extended
    "c\u{1DC0}",
    // U+1DC0-1DFF: Combining Diacritical Marks Supplement
    "d\u{20D0}",
    // U+20D0-20FF: Combining Diacritical Marks for Symbols
    "e\u{FE20}",
    // U+FE20-FE2F: Combining Half Marks
  ]
  
  list.each(inputs, fn(s) {
    let pure_result = translit_pure.remove_combining_marks_pure(s)
    let native_result = translit_native.remove_combining_marks_native(s)
    assert pure_result == native_result
  })
}
