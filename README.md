<p align="center">
  <img src="https://raw.githubusercontent.com/lupodevelop/str/c190b21/assets/img/logo-str.png" alt="str logo" width="280">
</p>

<h1 align="center">str</h1>

<p align="center">
  <strong>Unicode-aware string utilities for Gleam</strong>
</p>

<p align="center">
  <a href="https://hex.pm/packages/str"><img src="https://img.shields.io/hexpm/v/str" alt="Package Version"></a>
  <a href="https://hexdocs.pm/str/"><img src="https://img.shields.io/badge/hex-docs-ffaff3" alt="Hex Docs"></a>
  <a href="https://github.com/lupodevelop/str/actions"><img src="https://img.shields.io/github/actions/workflow/status/lupodevelop/str/ci.yml?branch=main&label=ci&logo=github" alt="CI"></a>
  <a href="https://opensource.org/licenses/MIT"><img src="https://img.shields.io/badge/License-MIT-yellow.svg" alt="License: MIT"></a>
</p>

> Production-ready Gleam library for Unicode-aware string operations. All
> operations work at grapheme cluster boundaries, correct behaviour for
> emoji, ZWJ sequences, combining marks, and flags.

---

## Features

| Category | Highlights |
|----------|------------|
| **Grapheme-Aware** | `take`, `drop`, `length`, `reverse`, `chunk` — all grapheme-correct |
| **Case Conversions** | `snake_case`, `camelCase`, `kebab-case`, `PascalCase`, `Title Case` |
| **Slug Generation** | `slugify` with token limits, custom separators, Unicode preservation |
| **Search & Replace** | `index_of`, `last_index_of`, `replace_first/last`, `contains_any/all` |
| **Validation** | `is_uppercase/lowercase/title_case`, `is_ascii/hex/numeric/alpha` |
| **Escaping** | `escape_html`, `unescape_html`, `escape_regex` |
| **Similarity** | Levenshtein `distance`, normalized `similarity`, `hamming_distance` |
| **Splitting** | `splitn`, `partition`, `rpartition`, `chunk`, `lines`, `words` |
| **Padding** | `pad_left`, `pad_right`, `center`, `fill` |
| **Minimal deps** | No OTP requirement — works on Erlang and JavaScript targets |

---

## Installation

```sh
gleam add str
```

---

## Quick Start

```gleam
import str

// Grapheme-safe truncation preserves emoji sequences
str.truncate("Hello 👩‍👩‍👧‍👦 World", 10, "...")
// → "Hello 👩‍👩‍👧‍👦..."

// ASCII transliteration and URL-friendly slugs
str.slugify("Crème Brûlée — Recipe 2025!")
// → "creme-brulee-recipe-2025"

// Case conversions
str.to_camel_case("hello world")  // → "helloWorld"
str.to_snake_case("Hello World")  // → "hello_world"

// Grapheme-aware search — counts clusters, not bytes
str.index_of("👨‍👩‍👧‍👦 family test", "family")
// → Ok(2)

// Normalized Levenshtein similarity
str.similarity("hello", "hallo")
// → 0.8
```

---

## Documentation

| Document | Description |
|----------|-------------|
| [API Reference](docs/api_reference.md) | Complete function reference with examples |
| [Examples](docs/examples.md) | Integration snippets and patterns |

| [OTP Integration](docs/otp_integration.md) | NFC/NFD normalization via Erlang |
| [Core internals](docs/str_core.md) | Grapheme-aware core operations |
| [Extra internals](docs/str_extra.md) | ASCII folding and slug generation |
| [Tokenizer](docs/str_tokenize.md) | Pure-Gleam grapheme tokenizer reference |

---

## Module Guide

| Module | Use when | Import |
|--------|----------|--------|
| `str` | Everything — recommended entry point | `import str` |
| `str/advanced` | Explicit KMP algorithm control, map caching | `import str/advanced` |
| `str/config` | Tune search heuristic thresholds | `import str/config` |

```text
str/
├── str.gleam       # Main module — complete public API
├── advanced.gleam  # Low-level search algorithms
├── config.gleam    # Search heuristic configuration
└── internal/       # Implementation details (not part of public API)
```

---

## Development

```sh
gleam test                               # run test suite (Erlang target)
gleam test --target javascript           # run on JavaScript target
python3 scripts/generate_character_tables.py  # regenerate transliteration tables
```

---

## Contributing

Contributions welcome. See [CONTRIBUTING.md](CONTRIBUTING.md).
Run `gleam test` before submitting PRs.

---

## License

MIT — see [LICENSE](LICENSE).

---

## Links

- [Gleam Language](https://gleam.run/)
- [Unicode Grapheme Clusters — UAX #29](https://unicode.org/reports/tr29/)
- [Hex Package](https://hex.pm/packages/str)
- [Hex Docs](https://hexdocs.pm/str/)
