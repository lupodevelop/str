# Roadmap

## v2.1.1 — Documentation fixes ✅

Patch release. No API changes.

- Documented `similarity` formula (normalized Levenshtein)
- Clarified `strip` treats `chars` as a set, not a literal
- Clarified `ascii_fold_no_decompose` vs `ascii_fold` with examples
- Documented `slugify_opts` `max_len: -1` = no limit
- Promoted `chars` experimental warning
- Fixed README `fill` example (was showing string `"both"` instead of `Both`)

---

## v2.2.0 — New API surface + deprecations

Additive release. All existing code continues to work.

**New API:**
- `TruncateMode` type (`Strict | Preserve`) replacing `truncate_strict` /
  `truncate_preserve` / `truncate_with_flag` — collapses 4 functions into 1
- `SlugifyOpts` record + `slug_with` replacing the positional `slugify_opts`
  variants — eliminates opaque positional args and combinatorial `_with_normalizer` variants
- `strip_affixes` as the canonical name for `unwrap` — `unwrap` conflicts
  with established FP semantics (extract from container or panic)

**Deprecations** (removed in 3.0):
- `truncate_strict`, `truncate_preserve`, `truncate_default`, `truncate_with_flag`
- `slugify_opts`, `slugify_with_normalizer`, `slugify_opts_with_normalizer`
- `ascii_fold_with_normalizer`, `ascii_fold_no_decompose_with_normalizer`
- `unwrap`, `index_of_simple`, `count_simple`
- KMP/sliding re-exports in `str` main module → use `str/advanced` directly
- Config constant re-exports in `str` main module → use `str/config` directly

**Also:**
- `MIGRATION.md` with full old → new mapping
- `chars` moved to `str/tokenize` or renamed `approximate_chars`;
  `chars_stdlib` becomes the canonical `chars`

---

## v3.0.0 — Breaking cleanup

Removes everything deprecated in v2.2.0. Users who followed deprecation
notices have zero changes to make.

**Removed:**
- All deprecated truncate variants → only `truncate` (3-arg, default behaviour)
  and `truncate_mode` (4-arg with `TruncateMode`) remain
- All deprecated slugify/ascii_fold variants → `slugify`, `slug_with`, `ascii_fold`,
  `ascii_fold_no_decompose` remain
- `unwrap` → `strip_affixes`
- `index_of_simple`, `count_simple`
- KMP/sliding/config re-exports from `str` main module
- Legacy modules `str/core`, `str/extra`, `str/tokenize` (deprecated since 2.0)

**API surface goal:** `str.gleam` exposes only user-facing operations.
Algorithm plumbing lives exclusively in `str/advanced`. Configuration in
`str/config`. No internals leaking through the main module.
