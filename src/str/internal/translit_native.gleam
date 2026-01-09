// Placeholder declarations for native FFI implementations (Erlang/JS)
@external(erlang, "str_ffi", "translit_replacements")
pub fn replacements_native() -> List(#(String, String))

@external(erlang, "str_ffi", "remove_combining_marks")
pub fn remove_combining_marks_native(s: String) -> String
