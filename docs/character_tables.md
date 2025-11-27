# Character tables — authoritative reference

This document is the authoritative reference for the pragmatic transliteration and limited decomposition mappings used by this library.

Scope and guarantees

- These tables are intended to support common European Latin-script characters encountered in user-facing text (Western, Central and some Baltic languages).
- The mappings are pragmatic: they prioritise readability and predictability (e.g. `ß -> ss`, `Æ -> AE`) rather than linguistically perfect transliteration for every language.
- This is NOT a comprehensive Unicode transliteration. Scripts such as Cyrillic, Greek, Arabic, Hebrew, Devanagari, East Asian CJK characters are out of scope for these tables and should be handled with dedicated libraries if needed.

Modules

- `src/str/internal_translit.gleam`: a replacement table (list of `[source, replacement]`) used by `ascii_fold`.
- `src/str/internal_decompose.gleam`: a limited NFD-style decomposer (list of `[source, decomposed_sequence]`) used when decomposition is enabled.

Design notes

- The ascii-folding pipeline (when decomposition is enabled) is:

1. Apply `internal_translit.replacements()` to the input string to handle common precomposed cases.
2. Run `internal_decompose.decompose_latin/1` to expand additional precomposed characters into base + combining marks (limited set).
3. Remove combining marks (`internal_translit.remove_combining_marks/1`).
4. Re-apply `internal_translit.replacements()` to catch any sequences introduced by decomposition.

Machine-readable data

- The full mapping is available in `docs/character_tables.json` (machine-readable). Use that file to generate language-specific tables or tests.

## Highlights (human-readable summary)

- Ligatures: `Æ -> AE`, `æ -> ae`, `Œ -> OE`, `œ -> oe`.
- German sharp s: `ß -> ss`.
- Scandinavian letters: `Å/å -> A`, `Ø/ø -> O`, `Ä/ä -> A`, `Ö/ö -> O`.
- Common diacritics are folded to their base letters: e.g. `É/È/Ê/Ë -> E`, `Ç/ç -> C`.
- Central European letters mapped to ASCII equivalents: `Ł/ł -> L`, `Ń/ń -> N`, `Š/š -> S`, `Ž/ž -> Z`, etc.
- Romanian and Turkish special letters handled: `Ț/ț -> T`, `Ș/ș -> S`, `Ş/ş -> S`, `İ/ı -> I`.

For the exhaustive list and exact sequences produced by decomposition, consult [docs/character_tables.json](./character_tables.json).

## Maintenance guidance

- To add or change a mapping, edit `src/str/internal_translit.gleam` (for direct replacement mappings) or `src/str/internal_decompose.gleam` (for decomposition rules), then regenerate the machine-readable JSON (I can add a small script to automate that).
- Keep replacements idempotent where possible: prefer mapping to ASCII sequences (e.g. `Æ -> AE`) rather than language-specific transliteration that could vary.

## OTP and normalization

- The module `:unicode` is part of Erlang/OTP. It provides full Unicode normalization (NFC/NFD/NFKC/NFKD) which is more correct and complete than a manual table-based approach.
- This library intentionally provides a pure-Gleam fallback to avoid forcing an OTP-specific dependency. If you run on the standard BEAM with OTP available you may choose to enable OTP normalization for maximum correctness. I can add an opt-in path that calls `:unicode` and falls back to the current pipeline if unavailable.

----

If you want, I will now also add `docs/character_tables.json` with the exact mappings currently present in the code so you can review or consume them programmatically.
