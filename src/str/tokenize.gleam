//// Pure-Gleam tokenizer for grapheme cluster segmentation.
////
//// Handles base characters followed by combining marks, variation selectors,
//// skin-tone modifiers and simple ZWJ sequences. This is an experimental
//// pure-Gleam implementation that approximates grapheme segmentation.
//// For production use the BEAM-provided `string.to_graphemes` via
//// `chars_stdlib/1`, which follows the platform's grapheme segmentation.

import gleam/list
import gleam/string

fn from_cp_list(cps) -> String {
  string.from_utf_codepoints(cps)
}

fn is_combining(cp_int: Int) -> Bool {
  case cp_int >= 0x0300 && cp_int <= 0x036F {
    True -> True
    False ->
      case cp_int >= 0x1DC0 && cp_int <= 0x1DFF {
        True -> True
        False ->
          case cp_int >= 0x1AB0 && cp_int <= 0x1AFF {
            True -> True
            False ->
              case cp_int >= 0x20D0 && cp_int <= 0x20FF {
                True -> True
                False ->
                  case cp_int >= 0xFE20 && cp_int <= 0xFE2F {
                    True -> True
                    False -> False
                  }
              }
          }
      }
  }
}

fn is_skin_tone(cp_int: Int) -> Bool {
  cp_int >= 0x1F3FB && cp_int <= 0x1F3FF
}

fn is_variation_selector(cp_int: Int) -> Bool {
  cp_int >= 0xFE00 && cp_int <= 0xFE0F
}

fn handle_nonempty(hd, tl, clusters, current_rev) -> List(String) {
  let hd_int = string.utf_codepoint_to_int(hd)

  case
    is_combining(hd_int)
    || is_skin_tone(hd_int)
    || is_variation_selector(hd_int)
  {
    True -> rec_build(tl, clusters, [hd, ..current_rev], False)
    False ->
      case hd_int == 0x200D {
        True -> rec_build(tl, clusters, [hd, ..current_rev], True)
        False ->
          rec_build(
            tl,
            [from_cp_list(list.reverse(current_rev)), ..clusters],
            [hd],
            False,
          )
      }
  }
}

fn rec_build(cps, clusters, current_rev, pending) -> List(String) {
  case cps, pending {
    [], _ ->
      case list.is_empty(current_rev) {
        True -> list.reverse(clusters)
        False ->
          list.reverse([from_cp_list(list.reverse(current_rev)), ..clusters])
      }
    [hd, ..tl], True -> rec_build(tl, clusters, [hd, ..current_rev], False)

    [hd, ..tl], False ->
      case list.is_empty(current_rev) {
        True -> rec_build(tl, clusters, [hd], False)
        False -> handle_nonempty(hd, tl, clusters, current_rev)
      }
  }
}

/// Returns a list of grapheme clusters for the input string.
/// This `chars/1` function is an experimental, pure-Gleam tokenizer that
/// approximates grapheme cluster segmentation. It is useful when you need a
/// tokenizer implemented purely in Gleam (e.g. for understanding or
/// environments where you prefer not to depend on the BEAM helper), but it
/// may differ in edge cases from the BEAM stdlib implementation. Prefer
/// `chars_stdlib/1` for production code where accuracy and performance are
/// important.
///
/// Example:
///   chars("café") -> ["c", "a", "f", "é"]
///
@deprecated("Will be removed in str 2.0; prefer the unified `str` module when available")
pub fn chars(text: String) -> List(String) {
  let cps = string.to_utf_codepoints(text)

  rec_build(cps, [], [], False)
}

/// Uses the BEAM stdlib grapheme segmentation (more accurate).
///
///   chars_stdlib("café") -> ["c", "a", "f", "é"]
///
@deprecated("Will be removed in str 2.0; prefer the unified `str` module when available")
pub fn chars_stdlib(text: String) -> List(String) {
  string.to_graphemes(text)
}
