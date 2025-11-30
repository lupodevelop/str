# Changelog

All notable changes to this project are documented in this file.

## [1.1.0] - 2025-11-30
### Added
- `length(text)` — grapheme-aware length function returning the number of grapheme clusters. Correctly counts emoji, combining sequences, flags, and other multi-codepoint graphemes.
- `contains(text, needle)` — grapheme-aware substring search returning `True` if `needle` is found in `text`.
- `starts_with(text, prefix)` — returns `True` if `text` starts with `prefix` on grapheme boundaries.
- `ends_with(text, suffix)` — returns `True` if `text` ends with `suffix` on grapheme boundaries.
- `is_empty(text)` — returns `True` if `text` is an empty string.

### Tests
- Comprehensive tests for all new functions covering ASCII, combining marks, ZWJ emoji sequences, skin-tone modifiers, regional indicator flags, edge cases (empty strings, long needles, etc.).

Contributed by: Daniele (`lupodevelop`)

## [1.0.1] - 2025-11-28
- Improvement: Optimized the `index_of` and `last_index_of` functions to avoid repeated calls to `list.length` while matching grapheme clusters. This reduces overhead on long strings and improves search performance.

Contributed by: Miao (`lemorage`) — commit `63a2b0c` (merged in PR #2)

Note: The test suite has been run and all tests pass.

## [1.0.0] - Initial release
- Initial release of the package.

