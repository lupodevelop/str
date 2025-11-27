// Example optional module `str_otp_helpers`.
//
// Purpose: show how an app can provide OTP-backed normalization helpers
// (NFD/NFC) in a separate package or in the application's source tree.
// This file is intentionally an example — keep OTP interop in the
// integrating project so the `str` library stays dependency-free.

// NOTE: The implementations below are intentionally identity functions
// so this example compiles without OTP present. Uncomment and adapt
// the pseudocode to call into OTP from your application.

pub fn nfd(s: String) -> String {
  // Example pseudocode calling OTP (DO NOT uncomment in the core
  // library; put this in your app or a separate package):
  //
  // // Call Erlang/OTP unicode NFD:
  // let res = erlang.call(:unicode, "characters_to_nfd_binary", [s])
  // res

  s
}

pub fn nfc(s: String) -> String {
  // Example pseudocode calling OTP:
  // let res = erlang.call(:unicode, "characters_to_nfc_binary", [s])
  // res

  s
}
