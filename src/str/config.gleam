//// Configuration helpers for opt-in experimental features.
////
//// This module provides a single place to control compile-time or
//// build-time toggles related to experimental string search helpers.
//// By default features are disabled; projects may replace this module
//// at build-time if they want a different default (e.g. enable in
//// staging).

/// Returns True when the smart search (heuristic KMP/sliding selection)
/// should be enabled by default. This is intentionally `False` so that
/// experimental APIs remain opt-in unless consumers explicitly enable
/// them in their build.
pub fn smart_search_enabled() -> Bool {
  False
}

/// Minimum pattern length (in grapheme clusters) at which KMP is preferred
pub fn kmp_min_pattern_len() -> Int {
  24
}

/// Threshold for "large" text lengths where KMP may be preferred
pub fn kmp_large_text_threshold() -> Int {
  50_000
}

/// Minimum pattern length to consider KMP on large texts
pub fn kmp_large_text_min_pat() -> Int {
  8
}

/// Multiplier applied to max border to decide repetitiveness (default 2)
pub fn kmp_border_multiplier() -> Int {
  1
}

/// Controlla se usare implementazioni FFI native quando disponibili.
/// Default: False (usa sempre pure Gleam per sicurezza/portabilità).
/// Impostare a True per abilitare FFI Erlang/JS ottimizzate.
pub fn use_native_ffi() -> Bool {
  False
}

/// Controllo granulari: utile per abilitare gradualmente funzionalità native.
pub fn native_decompose_enabled() -> Bool {
  use_native_ffi()
}

pub fn native_translit_enabled() -> Bool {
  use_native_ffi()
}

pub fn native_combining_marks_enabled() -> Bool {
  use_native_ffi()
}
