// Native FFI implementations for transliteration (Erlang/JS)
@external(erlang, "str_ffi", "translit_replacements")
@external(javascript, "../../str_ffi.mjs", "translit_replacements")
pub fn replacements_native() -> List(#(String, String))

@external(erlang, "str_ffi", "remove_combining_marks")
@external(javascript, "../../str_ffi.mjs", "remove_combining_marks")
pub fn remove_combining_marks_native(s: String) -> String
