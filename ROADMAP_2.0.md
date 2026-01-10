# Roadmap to str 2.0.0

> **Simplified, Unified API** — Moving towards a single-module design with clearer patterns

---

## 🎯 Vision

**str 2.0** will provide a unified, intuitive API through a single entry point while maintaining all current functionality. The goal is to reduce cognitive load for users and eliminate confusion about which module to import.

### Design Principles

1. **Single import for 95% of use cases** → `import str`
2. **Consistent naming patterns** → Clear, predictable function names
3. **No breaking existing patterns** → Migration path for all functions
4. **Advanced users still supported** → Submodules available but optional

---

## 🔄 Major Changes

### 1. Unified Module Structure

**Current (1.x):**
```gleam
import str/core
import str/extra

core.truncate("text", 10, "...")
extra.slugify("Hello World")
```

**New (2.0):**
```gleam
import str

str.truncate("text", 10, "...")
str.slugify("Hello World")
str.index_of("hello", "llo")        // Grapheme-aware by default
str.ascii_fold("Crème")              // Clear name, no module confusion
```

### 2. Simplified Search API

**Current (1.x):**
```gleam
import str/core

// Multiple experimental APIs
core.index_of(text, pattern)           // Legacy
core.index_of_auto(text, pattern)      // Experimental heuristic
core.index_of_strategy(text, pattern, core.Kmp)  // Explicit
```

**New (2.0):**
```gleam
import str

// Single, smart API
str.index_of(text, pattern)            // Auto-optimized (KMP/Sliding)
str.index_of(text, pattern, strategy: str.Kmp)  // Optional explicit control
```

### 3. Internal Modules Hidden

**Current (1.x):**
- `str/core` — public
- `str/extra` — public
- `str/tokenize` — public
- `str/internal_*` — technically public

**New (2.0):**
- `str` — **main public API**
- `str/internal/*` — all implementation details hidden
- `str/advanced` — optional, for power users who need fine control

---

## 📋 Breaking Changes

### Module Reorganization

| 1.x Import | 2.0 Import | Notes |
|------------|------------|-------|
| `str/core.truncate` | `str.truncate` | Re-exported from main module |
| `str/extra.slugify` | `str.slugify` | Re-exported from main module |
| `str/tokenize.chars` | `str/internal/tokenize.chars` | Moved to internal (reference impl) |
| `str/core.index_of_auto` | `str.index_of` | Becomes default, `_auto` suffix removed |
| `str/core.index_of_strategy` | `str.index_of(strategy: X)` | Named parameter instead |

### API Consolidation

**Removed experimental suffixes:**
- `index_of_auto` → `index_of` (smart by default)
- `count_auto` → `count` (smart by default)

**Removed redundant variants:**
- `truncate_preserve` → merged into `truncate(preserve: True)` (named param)
- `truncate_strict` → merged into `truncate(preserve: False)` (named param)

### Type Exports

```gleam
// 1.x - scattered
import str/core.{type SearchStrategy, Kmp, Sliding}
import str/core.{type FillPosition, Left, Right, Both}

// 2.0 - unified
import str.{type SearchStrategy, Kmp, Sliding}
import str.{type FillPosition, Left, Right, Both}
```

---

## 🚀 Migration Guide

### Automated Migration

We'll provide a migration script:

```sh
# Automated import rewriter
gleam run -m str/migrate -- src/

# Rewrites:
# import str/core -> import str
# import str/extra -> import str
# core.function() -> str.function()
# extra.function() -> str.function()
```

### Manual Migration Steps

**Step 1: Update imports**
```gleam
// Before
import str/core
import str/extra

// After
import str
```

**Step 2: Update function calls**
```gleam
// Before
core.truncate("text", 10, "...")
extra.slugify("Hello")
core.index_of_auto(text, pattern)

// After
str.truncate("text", 10, "...")
str.slugify("Hello")
str.index_of(text, pattern)  // Now smart by default
```

**Step 3: Update explicit strategies**
```gleam
// Before
import str/core.{type SearchStrategy, Kmp}
core.index_of_strategy(text, pattern, Kmp)

// After
import str.{type SearchStrategy, Kmp}
str.index_of(text, pattern, strategy: Kmp)
```

---

## 📦 New Main Module API

### Complete re-export from `str`

```gleam
// Grapheme operations
pub fn take(text: String, n: Int) -> String
pub fn drop(text: String, n: Int) -> String
pub fn at(text: String, index: Int) -> Result(String, Nil)
pub fn length(text: String) -> Int
pub fn reverse(text: String) -> String

// Search (smart, auto-optimized)
pub fn index_of(text: String, needle: String, strategy: Option(SearchStrategy)) -> Result(Int, Nil)
pub fn last_index_of(text: String, needle: String) -> Result(Int, Nil)
pub fn contains(text: String, needle: String) -> Bool
pub fn count(text: String, needle: String, overlapping: Bool, strategy: Option(SearchStrategy)) -> Int

// Truncation
pub fn truncate(text: String, max_len: Int, suffix: String, preserve_emoji: Bool) -> String
pub fn ellipsis(text: String, max_len: Int) -> String

// Case & normalization
pub fn capitalize(text: String) -> String
pub fn swapcase(text: String) -> String
pub fn normalize_whitespace(text: String) -> String

// Slugs & ASCII
pub fn slugify(text: String) -> String
pub fn slugify_opts(text: String, max_len: Int, sep: String, preserve_unicode: Bool) -> String
pub fn ascii_fold(text: String) -> String

// Case conversions
pub fn to_snake_case(text: String) -> String
pub fn to_camel_case(text: String) -> String
pub fn to_pascal_case(text: String) -> String
pub fn to_kebab_case(text: String) -> String
pub fn to_title_case(text: String) -> String

// Validation
pub fn is_empty(text: String) -> Bool
pub fn is_blank(text: String) -> Bool
pub fn is_uppercase(text: String) -> Bool
pub fn is_lowercase(text: String) -> Bool
pub fn is_numeric(text: String) -> Bool
pub fn is_alpha(text: String) -> Bool

// ... (all other core and extra functions)
```

---

## 🔧 Advanced Module (Optional)

For users who need fine-grained control:

```gleam
import str/advanced

// Explicit KMP with cached maps
let maps = advanced.build_kmp_maps(pattern)
let result = advanced.kmp_search_with_maps(text, maps)

// Direct access to algorithms
advanced.kmp_index_of(text, pattern)
advanced.sliding_index_of(text, pattern)

// Custom normalizers
advanced.ascii_fold_with_normalizer(text, my_nfd)
```

---

## 📅 Timeline

### Phase 1: Design & Feedback (Q1 2026)
- ✅ Publish this roadmap
- 📢 Gather community feedback
- 🔍 Review use cases and pain points

### Phase 2: Implementation (Q2 2026)
- 🔨 Implement unified `str` module
- 🧪 Create migration tooling
- 📝 Update all documentation

### Phase 3: Beta Release (Q2 2026)
- 🚀 Release `str@2.0.0-beta.1`
- 🐛 Collect feedback and fix issues
- 📊 Performance benchmarking

### Phase 4: Stable Release (Q3 2026)
- ✨ Release `str@2.0.0`
- 📚 Migration guides for all major use cases
- 🎉 Celebrate simplified API!

---

## 💬 Feedback

We'd love your input! Please comment on:

- **GitHub Issue**: [str#XX - Roadmap to 2.0](https://github.com/lupodevelop/str/issues/XX)
- **Discord**: Join the Gleam Discord #str channel

### Questions to Consider

1. **Is the single-module approach right for your use case?**
2. **Are there any functions you use frequently that should be in the main `str` module?**
3. **Do you need the `str/advanced` module, or is the main API sufficient?**
4. **Any concerns about the migration path?**

---

## 🎁 Benefits of 2.0

✅ **Simpler imports** → One import for everything  
✅ **Less confusion** → No more "which module?" questions  
✅ **Smarter defaults** → Auto-optimized search with opt-out  
✅ **Backward compatible imports** → `str/core` and `str/extra` still work during transition  
✅ **Cleaner API** → Consistent patterns, fewer experimental suffixes  
✅ **Migration tooling** → Automated import rewriter  

---

## 📌 Principles We Won't Change

- ✅ **Grapheme-aware by default** → Unicode correctness is non-negotiable
- ✅ **Zero OTP dependencies** → Pure Gleam, portable across targets
- ✅ **Performance conscious** → Smart algorithms, benchmarked
- ✅ **Well-tested** → Comprehensive test suite (357+ tests)
- ✅ **Production ready** → Battle-tested patterns

---

**Let's build str 2.0 together!** 🚀

_Last updated: 2026-01-08_
