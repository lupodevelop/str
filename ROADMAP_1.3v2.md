# Roadmap to str 1.3.0 (v2)

> **Pure Gleam optimizations** — Inspired by `string_width` patterns, no FFI

---

## 🎯 Goals

1. **Compact lookup tables** — BitArray trie instead of giant `case` statements
2. **Options builder pattern** — Cleaner API for complex functions
3. **Improved test coverage** — Property-based / fuzz tests for edge cases
4. **Prepare for 2.0 deprecations** — Mark old APIs as deprecated

---

## 📋 Tasks

### 1. BitArray Trie for Internal Tables

**Problem:** `internal_decompose.gleam` and `internal_translit.gleam` use large `case` statements that bloat binary size and slow compilation.

**Solution:** Replace with BitArray-based lookup (pure Gleam, no FFI).

```gleam
// Compact O(1) lookup using BitArray
fn table_lookup(table: BitArray, codepoint: Int) -> Bool {
  let hi = int.bitwise_shift_right(codepoint, 8)
  let md = int.bitwise_shift_right(int.bitwise_and(codepoint, 0xff), 3)
  let lo = int.bitwise_and(codepoint, 7)
  
  // 3-level trie lookup
  case bit_array.slice(table, hi, 1) {
    Ok(<<lvl1:int>>) -> {
      let offset = lvl1 * 32 + md
      case bit_array.slice(table, offset, 1) {
        Ok(<<lvl2:int>>) -> {
          int.bitwise_and(int.bitwise_shift_right(lvl2, lo), 1) == 1
        }
        Error(_) -> False
      }
    }
    Error(_) -> False
  }
}
```

**Files to update:**
- [ ] `scripts/generate_bitarray_tables.py` — New generator
- [ ] `src/str/internal_decompose.gleam` — Use BitArray
- [ ] `src/str/internal_translit.gleam` — Use BitArray

**Expected impact:**
- ~80% reduction in generated code size
- Faster compilation
- Slightly faster runtime lookup

---

### 2. Options Builder for `slugify`

**Problem:** `slugify_opts` has many parameters, hard to extend.

**Current:**
```gleam
slugify_opts(text, max_len: 50, separator: "-", lowercase: True)
```

**Proposed:**
```gleam
pub opaque type SlugifyOptions {
  SlugifyOptions(
    max_length: Option(Int),
    separator: String,
    lowercase: Bool,
    preserve_unicode: Bool,
    transliterate: Bool,
  )
}

pub fn slugify_options() -> SlugifyOptions
pub fn with_max_length(opts, n) -> SlugifyOptions
pub fn with_separator(opts, sep) -> SlugifyOptions
pub fn with_lowercase(opts, flag) -> SlugifyOptions
pub fn slugify_with(text, opts) -> String
```

**Files to update:**
- [ ] `src/str/extra.gleam` — Add options type and builder functions
- [ ] `src/str.gleam` — Re-export new API
- [ ] `test/str_extra_test.gleam` — Add tests for builder pattern

---

### 3. Fuzz / Property-Based Tests

**Problem:** Edge cases in Unicode handling may not be covered.

**Solution:** Add deterministic fuzz tests (already started for HTML escape).

**Files to add/update:**
- [x] `test/str_html_escape_fuzz_test.gleam` — Done
- [ ] `test/str_slugify_fuzz_test.gleam` — Roundtrip, edge cases
- [ ] `test/str_search_fuzz_test.gleam` — KMP/Sliding consistency

---

### 4. Deprecation Annotations for 2.0

**Problem:** Users need warning before 2.0 breaking changes.

**Solution:** Add `@deprecated` to functions that will change/move in 2.0.

```gleam
// In str/core.gleam
@deprecated("Use str.index_of in 2.0")
pub fn index_of(text: String, needle: String) -> Result(Int, Nil) {
  // ...
}
```

**Modules to annotate:**
- [ ] `src/str/core.gleam` — All `pub fn`
- [ ] `src/str/extra.gleam` — All `pub fn`
- [ ] `src/str/tokenize.gleam` — All `pub fn` (will move to internal)
- [ ] `src/str/config.gleam` — All `pub` items

**Note:** The main `src/str.gleam` re-exports will NOT be deprecated — that's the future-proof API.

---

## 🚫 Out of Scope

- ~~Display width~~ — Use `string_width` package instead
- ~~FFI optimizations~~ — Stay pure Gleam
- ~~New grapheme algorithms~~ — Defer to 2.0

---

## 📅 Timeline

| Phase | Tasks | Status |
|-------|-------|--------|
| **Phase 1** | Deprecation annotations | 🔲 Not started |
| **Phase 2** | Options builder for slugify | 🔲 Not started |
| **Phase 3** | BitArray trie prototype | 🔲 Not started |
| **Phase 4** | Fuzz tests | 🟡 Partial (HTML done) |

---

## ✅ Definition of Done

- [ ] All public APIs in submodules have `@deprecated` annotation
- [ ] `slugify_with` + options builder implemented and tested
- [ ] BitArray lookup working for at least one internal table
- [ ] Additional fuzz tests for slugify and search
- [ ] All tests pass: `gleam test --target erlang`
- [ ] README updated with deprecation notice
- [ ] CHANGELOG updated

---

## 💡 Inspiration

Patterns borrowed from [`string_width`](https://hexdocs.pm/string_width/):
- BitArray trie lookup (adapted for pure Gleam without FFI)
- Options builder with opaque type
- Fold-based iteration patterns (for future consideration)

---

_Created: 2026-01-09_
