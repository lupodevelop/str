# Test Suite Organization

This directory contains the complete test suite for the `str` library, organized by test type and scope.

## Directory Structure

```
test/
├── README.md                    # This file
├── CONVENTIONS.md              # Testing conventions and best practices
├── ffi_plan.md                 # Future FFI (native Erlang/JS) test plan
├── str_test.gleam              # Main test entry point (gleeunit.main())
├── str_auto_test.gleam         # Integration tests (moved from root)
├── str_integration_test.gleam  # Integration tests (moved from root)
├── str_strategy_explicit_test.gleam  # Integration tests (moved from root)
│
├── unit/                        # Unit tests for individual functions (25 files)
│   ├── bitarray_helpers_test.gleam         # BitArray helper functions (popcount, rank, etc.)
│   ├── str_auto_test.gleam                 # Auto-strategy tests (index_of_auto, count_auto)
│   ├── str_combining_test.gleam            # Unicode combining marks handling
│   ├── str_config_test.gleam               # Configuration functions
│   ├── str_config_use_native_test.gleam    # Native FFI configuration
│   ├── str_contains_test.gleam             # String contains functions
│   ├── str_contains_edge_cases_test.gleam  # Edge cases for contains
│   ├── str_core_test.gleam                 # Core string functions (pad, center, reverse, etc.)
│   ├── str_extra_test.gleam                # Extra functions (slugify, ascii_fold, case conversion)
│   ├── str_extra_options_test.gleam        # Slugify options builder
│   ├── str_html_escape_test.gleam          # HTML escape/unescape
│   ├── str_is_empty_test.gleam             # Empty string checks
│   ├── str_kmp_test.gleam                  # KMP search algorithm
│   ├── str_kmp_cache_test.gleam            # KMP cache/map reuse
│   ├── str_length_test.gleam               # Length function basics
│   ├── str_length_edge_cases_test.gleam    # Length edge cases (emoji, combining)
│   ├── str_length_more_test.gleam          # Additional length tests
│   ├── str_multilingual_test.gleam         # Multilingual support (French, Polish, etc.)
│   ├── str_search_test.gleam               # Search functions (KMP, Sliding, strategy)
│   ├── str_sliding_test.gleam              # Sliding window search
│   ├── str_strategy_test.gleam             # Search strategy selection
│   ├── str_tokenize_test.gleam             # Tokenization functions
│   ├── str_translit_bitarray_test.gleam    # BitArray-based transliteration
│   ├── str_truncate_emoji_test.gleam       # Truncate with emoji preservation
│   └── str_unicode_test.gleam              # Unicode handling (NFC/NFD)
│
├── integration/                 # Integration and parity tests (8 files)
│   ├── generate_pages_dedup_test.gleam         # Pages generation validation
│   ├── str_corpus_test.gleam                   # Large corpus tests (multiple languages)
│   ├── str_decompose_impl_test.gleam           # Decompose implementation tests
│   ├── str_decompose_pages_parity_test.gleam   # Decompose pages vs pure parity
│   ├── str_impl_pure_parity_test.gleam         # Implementation parity checks
│   ├── str_integration_test.gleam              # End-to-end integration tests
│   ├── str_normalizer_integration_test.gleam   # Custom normalizer integration
│   └── str_translit_pages_parity_test.gleam    # Transliteration pages vs pure parity
│
├── fuzz/                        # Deterministic fuzz tests (3 files)
│   ├── str_html_escape_fuzz_test.gleam   # HTML escape/unescape fuzzing
│   ├── str_search_fuzz_test.gleam        # Search algorithm parity fuzzing
│   └── str_slugify_fuzz_test.gleam       # Slugify fuzzing
│
└── support/                     # Test support utilities
    └── README.md               # Documentation for future test helpers

```

## Test Categories

### Unit Tests (`test/unit/`)
Focused tests for individual functions and features. Each file tests a specific module or functionality area.

**Key areas:**
- **Core string operations**: padding, centering, reversing, wrapping
- **Search algorithms**: KMP, sliding window, auto-strategy selection
- **Unicode handling**: grapheme clusters, combining marks, NFC/NFD normalization
- **Text transformation**: slugify, ascii_fold, case conversion
- **Configuration**: runtime settings, FFI flags

### Integration Tests (`test/integration/`)
Tests that validate interactions between multiple components and ensure parity between different implementations.

**Key areas:**
- **Parity tests**: Verify BitArray-based implementations match pure implementations
- **Corpus tests**: Validate behavior across multiple languages and character sets
- **End-to-end scenarios**: Complex chains of transformations
- **Custom normalizers**: User-provided normalization functions

### Fuzz Tests (`test/fuzz/`)
Deterministic property-based tests that verify invariants across generated inputs.

**Key areas:**
- **Search parity**: KMP vs Sliding vs Auto strategies produce identical results
- **HTML round-trips**: escape(unescape(x)) == x
- **Slugify properties**: output is lowercase, no spaces, valid separators

## Test Statistics

- **Total test files**: 40
- **Total test functions**: 401
- **Unit tests**: 25 files (~330 test functions)
- **Integration tests**: 8 files (~60 test functions)  
- **Fuzz tests**: 3 files (~10 test functions)
- **Documentation files**: 3 (README.md, CONVENTIONS.md, ffi_plan.md)

All tests pass consistently across Erlang and JavaScript targets.

### Test Count History

The test count increased from 390 to 401 (+11 tests) during reorganization:
- **+5 tests**: Converted `pub fn main()` to proper `pub fn *_test()` functions in:
  - `bitarray_helpers_test.gleam`
  - `str_impl_pure_parity_test.gleam`
  - `str_translit_pages_parity_test.gleam`
  - `generate_pages_dedup_test.gleam`
  - `str_decompose_pages_parity_test.gleam`
  - `str_slugify_fuzz_test.gleam`
- **+6 tests**: Added proper test coverage during cleanup (corpus tests, extra options, config tests)
- **-0 duplicates**: Removed duplicate `str_auto_test.gleam` from root (was in both root and unit/)

All increases represent legitimate new test coverage or proper test function conversions.

## Running Tests

```bash
# Run all tests
gleam test

# Run specific target
gleam test --target erlang
gleam test --target javascript

# Run with coverage (if configured)
gleam test --coverage
```

## Test Conventions

1. **Naming**: All test files end in `_test.gleam`
2. **Functions**: All test functions are public and end in `_test`
3. **Entry point**: Only `test/str_test.gleam` contains `pub fn main()`
4. **Imports**: No need to import `gleeunit` in individual test files
5. **Documentation**: Each test file includes a comment header describing its purpose

## Adding New Tests

### For unit tests:
1. Create or update file in `test/unit/`
2. Add public functions ending in `_test`
3. Group related tests in the same file
4. Add documentation comments

### For integration tests:
1. Create file in `test/integration/`
2. Name clearly to indicate what's being integrated
3. Add parity checks where applicable

### For fuzz tests:
1. Create file in `test/fuzz/`
2. Use deterministic seed for reproducibility
3. Verify invariants, not specific outputs
4. Document the property being tested

## Future Test Areas

- **FFI tests** (`test/ffi/`): Native Erlang/JS implementation tests
- **Performance tests** (`test/perf/`): Microbenchmarks and regression tests
- **Benchmark validation**: Ensure optimizations don't break correctness

## Test Support Modules

Helper modules for test utilities are located in `src/test_support/`:
- `fuzz_helpers.gleam`: Deterministic random generators for fuzz tests

---

For more information, see the main [README.md](../README.md) and [CONTRIBUTING.md](../CONTRIBUTING.md).
