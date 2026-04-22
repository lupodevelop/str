# OTP Integration

`str` is OTP-free by design. Its internal decomposer covers common Latin
scripts; for production-grade Unicode normalization (NFC/NFD) pass an OTP
normalizer function from your application.

---

## Normalizer signature

Any function with type `fn(String) -> String` works.

```gleam
// In your application (not in the str library)
pub fn otp_nfd(s: String) -> String {
  // Call Erlang's :unicode module via FFI
  // :unicode.characters_to_nfd_binary(s)
  s // replace with actual FFI call
}
```

---

## Usage with ascii_fold

```gleam
import str

// Full pipeline: decompose → normalize via OTP → remove combining marks
str.ascii_fold_with_normalizer("Crème Brûlée", otp_nfd)
// → "Creme Brulee"

// Skip decomposition, apply normalizer only
str.ascii_fold_no_decompose_with_normalizer("Café", otp_nfd)
```

---

## Usage with slugify

```gleam
// Default separator, no token limit
str.slugify_with_normalizer("Crème Brûlée", otp_nfd)
// → "creme-brulee"

// Full options + normalizer
str.slugify_opts_with_normalizer("Crème Brûlée", 2, "-", False, otp_nfd)
// → "creme-brulee"
```

---

## Testing without OTP

A fake normalizer is useful in tests to simulate NFD without Erlang interop:

```gleam
import gleam/string

let fake_nfd = fn(s) { string.replace(s, "é", "e\u{0301}") }
let slug = str.slugify_opts_with_normalizer("Café", -1, "-", False, fake_nfd)
// → "cafe"
```

---

## Placement note

Keep normalizer helpers in your application, not in `str` itself.
`str` has no OTP dependency; adding one would break JavaScript targets and
pure-Gleam environments.
