# FFI Test Plan

This document outlines the planned test structure for native (FFI) implementations.

## Future Directory Structure

```
test/
└── ffi/                         # FFI/native implementation tests (planned)
    ├── README.md               # This file
    ├── erlang/                 # Erlang-specific native tests
    │   ├── native_decompose_test.gleam
    │   ├── native_translit_test.gleam
    │   └── native_combining_test.gleam
    └── javascript/             # JavaScript-specific native tests
        ├── js_decompose_test.gleam
        ├── js_translit_test.gleam
        └── js_combining_test.gleam
```

## Test Strategy

### 1. Parity Tests
Ensure native implementations produce identical results to pure Gleam implementations:

```gleam
pub fn native_decompose_matches_pure_test() {
  let inputs = [
    "café", "Ångström", "naïve", "Zürich", ...
  ]
  
  list.each(inputs, fn(s) {
    assert decompose_native(s) == decompose_pure(s)
  })
}
```

### 2. Performance Benchmarks
Validate that native implementations are actually faster:

```gleam
pub fn native_performance_test() {
  let large_text = string.repeat("café ", 10_000)
  
  // Both should produce same result
  let pure_result = decompose_pure(large_text)
  let native_result = decompose_native(large_text)
  
  assert pure_result == native_result
  
  // Native implementation should be significantly faster (measured externally)
}
```

### 3. Edge Case Coverage
Test FFI implementations with challenging inputs:

- Empty strings
- Very long strings (> 1MB)
- Invalid UTF-8 (if applicable)
- Boundary conditions
- Special Unicode ranges

### 4. Target-Specific Tests
Some tests only run on specific targets:

```gleam
@target(erlang)
pub fn erlang_specific_nif_test() {
  // Only runs on Erlang target
}

@target(javascript)
pub fn javascript_specific_test() {
  // Only runs on JS target
}
```

## Implementation Checklist

When adding FFI implementations:

- [ ] Add pure Gleam implementation first
- [ ] Add comprehensive unit tests for pure implementation
- [ ] Add FFI implementation (Erlang NIF or JS)
- [ ] Add parity tests comparing FFI vs pure
- [ ] Add performance benchmarks
- [ ] Add edge case tests specific to FFI
- [ ] Document performance characteristics
- [ ] Add fallback logic if FFI unavailable

## Target Configuration

FFI tests should respect the `config.use_native_ffi()` setting:

```gleam
import str/config

pub fn test_uses_appropriate_impl() {
  case config.use_native_ffi() {
    True -> assert uses_nif_decompose()
    False -> assert uses_pure_decompose()
  }
}
```

## Performance Expectations

Document expected performance improvements:

| Operation | Pure (Gleam) | NIF (Erlang) | JS (Native) |
|-----------|--------------|--------------|-------------|
| decompose_latin (1KB) | ~500μs | ~50μs | ~100μs |
| transliterate (1KB) | ~800μs | ~80μs | ~150μs |
| remove_combining (1KB) | ~300μs | ~30μs | ~60μs |

*(Numbers are illustrative - measure actual performance)*

## Resources

- Erlang NIF documentation: https://www.erlang.org/doc/tutorial/nif.html
- Gleam FFI guide: https://gleam.run/book/tour/external-functions
- JavaScript FFI examples: https://gleam.run/book/tour/javascript

---

For current test organization, see [test/README.md](../README.md).
