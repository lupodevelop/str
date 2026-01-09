//// Advanced, low-level algorithms and helpers for power users.
////
//// This module exposes stable, well-documented building blocks intended
//// for callers who need explicit control over search algorithms, caching
//// of KMP maps, or want to benchmark/compare algorithms.

import gleam/dict
import str/internal/core as core

/// Builds KMP prefix/lookup maps for a pattern.
pub fn build_kmp_maps(
  pattern: String,
) -> #(dict.Dict(Int, String), dict.Dict(Int, Int)) {
  core.build_kmp_maps(pattern)
}

/// KMP search with pre-built maps for better performance.
pub fn kmp_search_all_with_maps(
  text: String,
  pmap: dict.Dict(Int, String),
  pimap: dict.Dict(Int, Int),
) -> List(Int) {
  core.kmp_search_all_with_maps(text, pmap, pimap)
}

/// KMP index of with pre-built maps.
pub fn kmp_index_of_with_maps(
  text: String,
  pattern: String,
  pmap: dict.Dict(Int, String),
  pimap: dict.Dict(Int, Int),
) -> Result(Int, Nil) {
  core.kmp_index_of_with_maps(text, pattern, pmap, pimap)
}

/// KMP search (first occurrence) wrapper.
pub fn kmp_index_of(text: String, pattern: String) -> Result(Int, Nil) {
  core.kmp_index_of(text, pattern)
}

/// Find all occurrences using the KMP algorithm.
pub fn kmp_search_all(text: String, pattern: String) -> List(Int) {
  core.kmp_search_all(text, pattern)
}

/// Sliding-window search: find all occurrences.
pub fn sliding_search_all(text: String, pattern: String) -> List(Int) {
  core.sliding_search_all(text, pattern)
}

/// Sliding-window search: first occurrence.
pub fn sliding_index_of(text: String, pattern: String) -> Result(Int, Nil) {
  core.sliding_index_of(text, pattern)
}

/// Chooses optimal search strategy using the library's heuristics.
pub fn choose_search_strategy(text: String, pattern: String) -> core.SearchStrategy {
  core.choose_search_strategy(text, pattern)
}

/// Helper types for users building caches.
pub type KmpMaps = #(dict.Dict(Int, String), dict.Dict(Int, Int))

/// NOTE: This module is "advanced" — functions here give fine-grained
/// control and are intended for performance-sensitive code paths. They
/// may change across minor releases; prefer `import str` for stable APIs.
