# Testing Conventions & Best Practices

This document outlines the conventions and best practices for writing tests in the `str` library.

## General Conventions

### File Naming
- All test files MUST end with `_test.gleam`
- Use descriptive names: `str_<feature>_test.gleam`
- Group related tests in the same file

### Function Naming
- All test functions MUST be `pub` (public)
- All test functions MUST end with `_test`
- Use descriptive names: `<feature>_<scenario>_test`

Examples:
```gleam
pub fn slugify_basic_test() { ... }
pub fn slugify_with_emoji_test() { ... }
pub fn slugify_preserves_unicode_test() { ... }
```

### Test Organization

```
test/
├── str_test.gleam          # ONLY file with pub fn main()
├── unit/                   # Unit tests (single function/module)
├── integration/            # Integration tests (multiple components)
├── fuzz/                   # Property-based/fuzz tests
└── support/                # Shared test utilities (README only)
```

**Important**: 
- Only `test/str_test.gleam` should contain `pub fn main() { gleeunit.main() }`
- All other files only need `pub fn *_test()` functions
- gleeunit automatically discovers all test files in subdirectories

## Test Structure

### Basic Test Template

```gleam
/// Brief description of what this file tests.
///
/// More details about test coverage, edge cases, etc.
import gleam/list
import str

pub fn feature_basic_test() {
  let input = "test input"
  let expected = "expected output"
  
  assert str.some_function(input) == expected
}

pub fn feature_edge_case_test() {
  // Test empty string
  assert str.some_function("") == ""
  
  // Test unicode
  assert str.some_function("café") == "cafe"
}
```

### Property-Based Test Template

```gleam
/// Fuzz tests for <feature> invariants.
///
/// Uses deterministic seed for reproducibility.
import test_support/fuzz_helpers as fuzz

pub fn feature_property_test() {
  let seed = 42
  let pool = fuzz.gen_token_pool()
  
  // Generate 100 test cases
  list.range(0, 99)
  |> list.each(fn(i) {
    let input = fuzz.gen_string(pool, seed + i, 20)
    
    // Test invariant holds
    assert some_invariant(input)
  })
}
```

## Testing Best Practices

### 1. Test One Thing
Each test should verify one specific behavior:

```gleam
// ✅ Good - tests one specific case
pub fn slugify_removes_special_chars_test() {
  assert str.slugify("hello@world") == "hello-world"
}

// ❌ Bad - tests multiple unrelated things
pub fn slugify_test() {
  assert str.slugify("hello@world") == "hello-world"
  assert str.slugify("CAPS") == "caps"
  assert str.slugify("emoji 😀") == "emoji"
}
```

### 2. Use Descriptive Names
Test names should explain what they test:

```gleam
// ✅ Good
pub fn length_counts_emoji_as_one_grapheme_test()

// ❌ Bad
pub fn test1()
pub fn length_test()
```

### 3. Test Edge Cases
Always test boundary conditions:

```gleam
pub fn length_edge_cases_test() {
  assert str.length("") == 0                    // Empty
  assert str.length("a") == 1                   // Single char
  assert str.length("👨‍👩‍👧‍👦") == 1            // Complex emoji
  assert str.length("e\u{0301}") == 1           // Combining marks
}
```

### 4. Group Related Tests
Keep related tests in the same file:

```gleam
// str_slugify_test.gleam
pub fn slugify_basic_test() { ... }
pub fn slugify_with_accents_test() { ... }
pub fn slugify_with_emoji_test() { ... }
pub fn slugify_custom_separator_test() { ... }
```

### 5. Document Complex Tests
Add comments for non-obvious test logic:

```gleam
pub fn kmp_overlapping_count_test() {
  let text = "aaaa"
  let pat = "aa"
  
  // Pattern "aa" appears at positions 0, 1, 2 when overlapping
  // But only at positions 0, 2 when non-overlapping
  assert str.count(text, pat, True) == 3   // overlapping
  assert str.count(text, pat, False) == 2  // non-overlapping
}
```

### 6. Use Helper Functions
Extract common test setup:

```gleam
fn make_large_text() -> String {
  list.fold(list.range(1, 1000), "", fn(acc, _) { acc <> "test " })
}

pub fn truncate_large_text_test() {
  let text = make_large_text()
  assert str.length(str.truncate(text, 10, "...")) <= 13
}
```

### 7. Test Both Success and Failure
Test what should work AND what should fail:

```gleam
pub fn contains_found_test() {
  assert str.contains("hello world", "world") == True
}

pub fn contains_not_found_test() {
  assert str.contains("hello world", "xyz") == False
}
```

## Coverage Goals

- **Unit tests**: Cover all public functions
- **Edge cases**: Empty, single char, very long, Unicode
- **Integration**: Test function combinations
- **Fuzz**: Verify invariants across many inputs

## Running Tests

```bash
# Run all tests
gleam test

# Run with specific target
gleam test --target erlang
gleam test --target javascript

# Run tests multiple times (for fuzz validation)
for i in {1..10}; do gleam test || break; done
```

## Adding New Tests

1. **Decide on category**: unit, integration, or fuzz?
2. **Choose appropriate directory**: `test/unit/`, `test/integration/`, or `test/fuzz/`
3. **Create or update file**: `test/unit/str_<feature>_test.gleam`
4. **Add public test functions**: `pub fn <feature>_<case>_test() { ... }`
5. **Run tests**: `gleam test`
6. **Add documentation**: Comment explaining what's tested

### Testing internals vs public API

- **Prefer public API tests**: default to testing behaviour through `import str` when possible. This ensures stability and reduces coupling to internal implementation details.

- **When to test internals**: create tests that import `str/internal/...` only in these cases:
  - **Parity tests**: verify optimized or generated implementations match the pure reference implementations (e.g., pages vs fold-based tables).
  - **Performance-critical helpers**: validate correctness of helpers used in tight loops (bitarray helpers, popcount/rank etc.).
  - **Generated code**: test correctness of generated modules where the public API cannot fully exercise internal invariants.

**FFI / native testing note:** When testing native/FFI implementations, use target annotations and config checks to avoid false positives:

```gleam
@target(erlang)
pub fn erlang_native_decompose_test() {
  case str/config.native_decompose_enabled() {
    True -> assert decompose_native("café") == decompose_pure("café")
    False -> skip_test() // or assert using pure implementation
  }
}
```

Also prefer `@target(javascript)` for JS-specific native tests and ensure tests respect `str/config.use_native_ffi()`.

- **How to label internal tests**: add a short header comment beginning with `// INTERNAL` or `// PARITY` describing why the internal test exists and when it may be promotable to a public test.

- **Keep internal tests small and focused**: test the invariant or equivalence you need, and prefer to add public API tests if the behaviour is exposed or relied upon by callers.

## Anti-Patterns to Avoid

❌ Don't add `pub fn main()` to test files (except `str_test.gleam`)
❌ Don't import `gleeunit` in individual test files
❌ Don't use random generators without fixed seeds
❌ Don't test implementation details (test behavior, not internals)
❌ Don't write tests that depend on execution order
❌ Don't ignore warnings (fix them immediately)

## Resources

- gleeunit documentation: https://hexdocs.pm/gleeunit/
- Gleam testing guide: https://gleam.run/book/tour/testing
- Property-based testing: https://hexdocs.pm/gleeunit/gleeunit/should.html

---

For test organization, see [test/README.md](README.md).
