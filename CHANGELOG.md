# Changelog

All notable changes to this project are documented in this file.

## [1.0.1] - 2025-11-28
- Improvement: Optimized the `index_of` and `last_index_of` functions to avoid repeated calls to `list.length` while matching grapheme clusters. This reduces overhead on long strings and improves search performance.

Contributed by: Glenn Miao (`one.lemorage`) — commit `63a2b0c` (merged in PR #2)

Note: The test suite has been run and all tests pass.

## [1.0.0] - Initial release
- Initial release of the package.
