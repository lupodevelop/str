// Generated combining marks helper
// Centralized detection for Unicode combining mark ranges used by `str`.

pub fn is_combining_mark(cp: Int) -> Bool {
  case
    cp >= 0x0300 && cp <= 0x036F
    || cp >= 0x1AB0 && cp <= 0x1AFF
    || cp >= 0x1DC0 && cp <= 0x1DFF
    || cp >= 0x20D0 && cp <= 0x20FF
    || cp >= 0xFE20 && cp <= 0xFE2F
  {
    True -> True
    False -> False
  }
}