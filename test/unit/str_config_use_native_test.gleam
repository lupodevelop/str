import str/config

pub fn use_native_default_test() {
  assert config.use_native_ffi() == False
}

pub fn native_flags_follow_default_test() {
  assert config.native_decompose_enabled() == False
  assert config.native_translit_enabled() == False
  assert config.native_combining_marks_enabled() == False
}