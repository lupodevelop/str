// Placeholder declarations for native FFI implementations (Erlang/JS)
@external(erlang, "str_ffi", "decompose_nfd")
pub fn decompose_latin_native(s: String) -> String

@external(erlang, "str_ffi", "remove_combining_marks")
pub fn remove_combining_marks_native(s: String) -> String
