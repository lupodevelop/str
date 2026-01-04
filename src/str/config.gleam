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
