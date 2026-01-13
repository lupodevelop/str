// Target-specific decomposition implementation
// Erlang: uses 64-bit page tables for optimal performance
// JavaScript: uses 32-bit page tables for JS Number compatibility

import gleam/list
import gleam/string

@target(erlang)
import str/internal/generated_decompose_pages as pages_gen

@target(javascript)
import str/internal/generated_decompose_pages_js as pages_gen_js

/// Erlang: use 64-bit optimized page tables
@target(erlang)
pub fn decompose_impl(s: String) -> String {
  let gs = string.to_graphemes(s)
  list.fold(gs, [], fn(acc, g) {
    case pages_gen.decompose_pages_lookup_by_grapheme(g) {
      Ok(v) -> [v, ..acc]
      Error(_) -> [g, ..acc]
    }
  })
  |> list.reverse
  |> string.concat
}

/// JavaScript: use 32-bit page tables (JS Number safe)
@target(javascript)
pub fn decompose_impl(s: String) -> String {
  let gs = string.to_graphemes(s)
  list.fold(gs, [], fn(acc, g) {
    case pages_gen_js.decompose_pages_js_lookup_by_grapheme(g) {
      Ok(v) -> [v, ..acc]
      Error(_) -> [g, ..acc]
    }
  })
  |> list.reverse
  |> string.concat
}
