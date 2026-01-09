import str/internal/decompose_impl as impl

// Delegating public surface for internal decomposition helpers.
pub fn decompose_latin(s: String) -> String {
  impl.decompose_latin(s)
}

