//// Internal transliteration tables and helpers.
//// Not part of the public API - may change without notice.

import gleam/list
import gleam/string

/// Returns the character replacement table as a list of (from, to) tuples.
/// Each tuple maps a Unicode character to its ASCII equivalent.
///
/// Using tuples instead of lists provides:
/// - Type safety: exactly 2 elements guaranteed
/// - Better pattern matching: no need for fallback case
/// - Clearer intent: represents a mapping relationship
///
pub fn replacements() -> List(#(String, String)) {
  [
    #("À", "A"),
    #("Á", "A"),
    #("Â", "A"),
    #("Ã", "A"),
    #("Ä", "A"),
    #("Å", "A"),
    #("à", "a"),
    #("á", "a"),
    #("â", "a"),
    #("ã", "a"),
    #("ä", "a"),
    #("å", "a"),
    #("Ç", "C"),
    #("ç", "c"),
    #("È", "E"),
    #("É", "E"),
    #("Ê", "E"),
    #("Ë", "E"),
    #("è", "e"),
    #("é", "e"),
    #("ê", "e"),
    #("ë", "e"),
    #("Ì", "I"),
    #("Í", "I"),
    #("Î", "I"),
    #("Ï", "I"),
    #("ì", "i"),
    #("í", "i"),
    #("î", "i"),
    #("ï", "i"),
    #("Ò", "O"),
    #("Ó", "O"),
    #("Ô", "O"),
    #("Õ", "O"),
    #("Ö", "O"),
    #("Ø", "O"),
    #("ò", "o"),
    #("ó", "o"),
    #("ô", "o"),
    #("õ", "o"),
    #("ö", "o"),
    #("ø", "o"),
    #("Ù", "U"),
    #("Ú", "U"),
    #("Û", "U"),
    #("Ü", "U"),
    #("ù", "u"),
    #("ú", "u"),
    #("û", "u"),
    #("ü", "u"),
    #("Ñ", "N"),
    #("ñ", "n"),
    #("Ý", "Y"),
    #("ý", "y"),
    #("ÿ", "y"),
    #("Æ", "AE"),
    #("æ", "ae"),
    #("ß", "ss"),
    #("Ā", "A"),
    #("ā", "a"),
    #("Ă", "A"),
    #("ă", "a"),
    #("Ą", "A"),
    #("ą", "a"),
    // Baltic: Latvian, Lithuanian, Estonian common letters
    #("Ė", "E"),
    #("ė", "e"),
    #("Ļ", "L"),
    #("ļ", "l"),
    #("Ŗ", "R"),
    #("ŗ", "r"),
    #("Ū", "U"),
    #("ū", "u"),
    #("Ļ", "L"),
    #("ļ", "l"),
    #("Ķ", "K"),
    #("ķ", "k"),
    #("Ģ", "G"),
    #("ģ", "g"),
    #("Ē", "E"),
    #("ē", "e"),
    #("Į", "I"),
    #("į", "i"),
    #("Ų", "U"),
    #("ų", "u"),
    #("Ą", "A"),
    #("ą", "a"),
    #("Ć", "C"),
    #("ć", "c"),
    #("Č", "C"),
    #("č", "c"),
    #("Ď", "D"),
    #("ď", "d"),
    #("Đ", "D"),
    #("đ", "d"),
    #("Ē", "E"),
    #("ē", "e"),
    #("Ę", "E"),
    #("ę", "e"),
    #("Ě", "E"),
    #("ě", "e"),
    #("Ğ", "G"),
    #("ğ", "g"),
    #("İ", "I"),
    #("ı", "i"),
    #("Ł", "L"),
    #("ł", "l"),
    #("Ń", "N"),
    #("ń", "n"),
    #("Ň", "N"),
    #("ň", "n"),
    #("Ő", "O"),
    #("ő", "o"),
    #("Ř", "R"),
    #("ř", "r"),
    #("Ś", "S"),
    #("ś", "s"),
    #("Š", "S"),
    #("š", "s"),
    #("Ţ", "T"),
    #("ţ", "t"),
    #("Ť", "T"),
    #("ť", "t"),
    #("Ź", "Z"),
    #("ź", "z"),
    #("Ż", "Z"),
    #("ż", "z"),
    #("Œ", "OE"),
    #("œ", "oe"),
    #("Þ", "TH"),
    #("þ", "th"),
    // Romanian
    #("Ț", "T"),
    #("ț", "t"),
    #("Ș", "S"),
    #("ș", "s"),
    // Turkish
    #("Ş", "S"),
    #("ş", "s"),
    // Icelandic / Old Norse
    #("Ð", "D"),
    #("ð", "d"),
    // Additional Latin Extended-A
    #("Į", "I"),
    #("į", "i"),
    #("Ů", "U"),
    #("ů", "u"),
    #("Ą", "A"),
    #("ą", "a"),
    // Czech/Slovak extras
    #("ě", "e"),
    #("Ě", "E"),
    #("Ť", "T"),
    #("ť", "t"),
    #("Ŕ", "R"),
    #("ŕ", "r"),
    // Lithuanian dot-above e
    #("ė", "e"),
    #("Ė", "E"),
  ]
}

/// Removes Unicode combining marks (diacritics) from a string.
pub fn remove_combining_marks(s: String) -> String {
  let cps = string.to_utf_codepoints(s)
  let filtered =
    list.filter(cps, fn(cp) {
      case string.utf_codepoint_to_int(cp) {
        i ->
          case
            i >= 0x0300
            && i <= 0x036F
            || i >= 0x1AB0
            && i <= 0x1AFF
            || i >= 0x1DC0
            && i <= 0x1DFF
            || i >= 0x20D0
            && i <= 0x20FF
            || i >= 0xFE20
            && i <= 0xFE2F
          {
            True -> False
            False -> True
          }
      }
    })

  string.from_utf_codepoints(filtered)
}
