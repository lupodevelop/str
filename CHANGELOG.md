# Changelog

All notable changes to this project are documented in this file.

## [1.2.2] - 2026-01-02
### Added
- Added internal helper `grapheme_len/1` (internal) to centralize grapheme cluster length computation and avoid repetitive `string.to_graphemes |> list.length` patterns.

### Style
- Replaced direct grapheme-length patterns with `grapheme_len/1` where appropriate to improve readability and maintainability.

Contributed by: Daniele (`lupodevelop`)

## [1.2.1] - 2026-01-02
### Fixed
- Made `repeat_str/2` iterative to avoid deep recursion and improve performance on large repetition counts.
- Fixed `wrap_at/2` to measure word lengths using grapheme clusters to avoid splitting emoji and other multi-codepoint graphemes.
- Replaced several `string.length(part) == 0` checks with `string.is_empty(part)` for clarity.

### Performance
- Optimized `find_common_prefix` to accept pre-segmented grapheme lists (`List(List(String))`) to avoid repeated `string.to_graphemes` conversions and expensive string concatenation/drop operations in recursive calls, reducing allocations and improving performance on lists of long strings (contributed by Miao `lemorage`).


### Tests
- Added `wrap_at_emoji_grapheme_test` to verify grapheme-aware wrapping behavior.

### Style
- Ran `gleam format` to normalize formatting.

Contributed by: Daniele (`lupodevelop`), Miao (`lemorage`)

## [1.2.0] - 2025-12-20
### Changed
- `reverse/1` now uses the BEAM stdlib grapheme segmentation (`string.to_graphemes`) for better Unicode correctness and consistency across the library.

### Performance
- Optimized `count/3` internals to avoid repeated `list.length` calls inside recursive loops, improving performance on long strings.

### CI
- Pinned Gleam version in CI and fixed build cache path/key for more reproducible and faster runs.

Contributed by: Daniele (`lupodevelop`)


## [1.1.1] - 2025-11-30
### Fixed
- Robustness fixes for grapheme-aware utilities; resolved parity issues in `ends_with/2` for complex ZWJ sequences.

### Performance
- Minor optimizations and safe fast-paths for ASCII suffix handling.

### Documentation
- Minor clarifications in the tokenizer documentation.

### Tests
- Updated tests covering emoji/ZWJ and tokenization edge cases.

Contributed by: Daniele (`lupodevelop`)


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

