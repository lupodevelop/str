# str_otp_helpers (example)

This folder shows an *optional* helper module that an application can
provide to expose OTP-backed Unicode normalization helpers (NFD/NFC).

Important:
- This is an example project and is intentionally kept outside of the
  `str` core library to avoid introducing OTP as a dependency of
  `str`.
- If you want to use these helpers in your application, put this code
  in your app or in a separate package and depend on it from your app.

Usage (conceptual)

1. Copy `src/str_otp_helpers/otp_helpers.gleam` to your application or
   create a small package and add it as a dependency.
2. Implement the OTP interop calls as appropriate for your project.
3. Pass the helper(s) to the `str` API that accept a normalizer, e.g.:

```gleam
let opts = str.slugify_options() |> str.with_max_tokens(0) |> str.with_separator("-") |> str.with_preserve_unicode(False)
let slug = str.slugify_with_options_and_normalizer("Crème Brûlée", opts, str_otp_helpers::otp_helpers::nfd)
```

Notes on Erlang interop

The exact way you call OTP from Gleam depends on your interop setup.
The examples in `otp_helpers.gleam` show commented pseudocode; adapt it
for your environment.
