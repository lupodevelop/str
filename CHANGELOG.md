# Changelog

All notable changes to this project are documented in this file.

## [1.1.0] - 2025-11-29
- Added: `length(text)` — a grapheme-aware `length/1` function that returns the number of grapheme clusters in a string. Correctly counts emoji, combining sequences, flags and other multi-codepoint graphemes.
- Tests: Added comprehensive tests for `length/1` covering ASCII, combining marks, ZWJ emoji sequences, skin-tone modifiers, regional indicator flags, astral characters, and long ASCII strings.

Contributed by: Daniele (`lupodevelop`) — added public wrapper and tests.

## [1.0.1] - 2025-11-28
- Improvement: Optimized the `index_of` and `last_index_of` functions to avoid repeated calls to `list.length` while matching grapheme clusters. This reduces overhead on long strings and improves search performance.

Contributed by: Miao (`lemorage`) — commit `63a2b0c` (merged in PR #2)

Note: The test suite has been run and all tests pass.

## [1.0.0] - Initial release
- Initial release of the package.

