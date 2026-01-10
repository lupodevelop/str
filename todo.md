# str — Piano di Implementazione Ottimizzazioni

Questo file traccia il piano di implementazione basato su [OPTIMIZATION_ANALYSIS.md](docs/OPTIMIZATION_ANALYSIS.md).

---

## Stato Attuale

- **Branch**: `feature/ffi-layer`
- **Versione target**: 1.3.0 → 2.0.0
- **Test**: 371 passing

### Lavoro Completato
- [x] Struttura delegante `*_impl` / `*_pure` per decompose e translit
- [x] Modulo `src/str/advanced.gleam` per API low-level
- [x] Re-export funzioni KMP/sliding in str.gleam
- [x] Alias `index_of_simple` / `count_simple`
- [x] Documento `OPTIMIZATION_ANALYSIS.md`

---

## Fase 1: Configurazione FFI (Priorità Alta)

| Task | Status | File |
|------|--------|------|
| Aggiungere `use_native_ffi()` a config | ⬜ | `src/str/config.gleam` |
| Aggiungere flag granulari (`native_decompose_enabled`, etc.) | ⬜ | `src/str/config.gleam` |

---

## Fase 2: BitArray Tables (Priorità Alta)

| Task | Status | File |
|------|--------|------|
| Creare script generatore Python | ⬜ | `scripts/generate_bitarray_tables.py` |
| Applicare BitArray a translit | ⬜ | `src/str/internal/translit_pure.gleam` |
| Applicare BitArray a decompose | ⬜ | `src/str/internal/decompose_pure.gleam` |
| Benchmark BitArray vs list.fold | ⬜ | `scripts/bench_bitarray.py` |

---

## Fase 3: Options Builder (Priorità Media)

| Task | Status | File |
|------|--------|------|
| Creare `SlugifyOptions` opaque type | ⬜ | `src/str/extra.gleam` |
| Implementare builder functions (`with_*`) | ⬜ | `src/str/extra.gleam` |
| Implementare `slugify_with_options` | ⬜ | `src/str/extra.gleam` |
| Re-export in `str.gleam` | ⬜ | `src/str.gleam` |
| Deprecare `slugify_opts` | ⬜ | `src/str/extra.gleam` |
| Aggiungere test per Options builder | ⬜ | `test/str_extra_test.gleam` |

---

## Fase 4: FFI Erlang (Priorità Alta dopo Fase 2)

| Task | Status | File |
|------|--------|------|
| Creare modulo FFI Erlang | ⬜ | `src/str_ffi.erl` |
| Creare `decompose_native.gleam` | ⬜ | `src/str/internal/decompose_native.gleam` |
| Creare `translit_native.gleam` | ⬜ | `src/str/internal/translit_native.gleam` |
| Aggiornare `decompose_impl.gleam` | ⬜ | `src/str/internal/decompose_impl.gleam` |
| Aggiornare `translit_impl.gleam` | ⬜ | `src/str/internal/translit_impl.gleam` |
| Test parità pure vs native | ⬜ | `test/str_ffi_parity_test.gleam` |
| Benchmark pure vs native | ⬜ | `scripts/bench_ffi.py` |

---

## Fase 5: Ottimizzazioni Interne (Priorità Media)

| Task | Status | File |
|------|--------|------|
| Refactor `cluster_has_emoji` (flat) | ⬜ | `src/str/internal/core.gleam` |
| Refactor string concat → list accumulate | ⬜ | `src/str/internal/extra.gleam` |
| BitArray per combining marks | ⬜ | `src/str/internal/translit_pure.gleam` |

---

## Fase 6: Testing & Quality (Priorità Media)

| Task | Status | File |
|------|--------|------|
| Fuzz tests per slugify | ⬜ | `test/str_slugify_fuzz_test.gleam` |
| Fuzz tests per search | ⬜ | `test/str_search_fuzz_test.gleam` |
| CI job benchmark KMP vs Sliding | ⬜ | `.github/workflows/bench.yml` |

---

## Fase 7: FFI JavaScript (Priorità Futura)

| Task | Status | File |
|------|--------|------|
| Creare `decompose_native.mjs` | ⬜ | `src/str/internal/decompose_native.mjs` |
| Creare `translit_native.mjs` | ⬜ | `src/str/internal/translit_native.mjs` |
| Aggiungere `@external(javascript, ...)` | ⬜ | `src/str/internal/*_native.gleam` |
| Test su target JavaScript | ⬜ | CI workflow |

---

## Fase 8: Documentazione & Release (Priorità Finale)

| Task | Status | File |
|------|--------|------|
| Aggiornare README | ⬜ | `README.md` |
| Aggiornare CHANGELOG | ⬜ | `CHANGELOG.md` |
| Scrivere migration guide | ⬜ | `docs/MIGRATION.md` |
| Preparare 2.0.0-beta | ⬜ | Release |
| Raccogliere feedback | ⬜ | Issues |
| Tagliare 2.0.0 finale | ⬜ | Release |

---

## Comandi Utili

```sh
# Run tests
gleam test --target erlang

# Check for warnings
gleam check

# Format code
gleam format

# Build docs
gleam docs build
```

---

## Metriche Target

| Metrica | Target |
|---------|--------|
| Tempo `translit` 1000 char (pure) | -70% |
| Tempo `translit` 1000 char (native) | -95% |
| Dimensione binario | -20% |
| Copertura test | >95% |
| API breaking changes | 0 (deprecation path) |

---

## Note

- **Default FFI**: `use_native_ffi() -> False` (sicurezza prima)
- **Override**: I consumatori possono sostituire `config.gleam` a build-time
- **Rollback**: Un flag per disabilitare tutto FFI istantaneamente
- **JavaScript**: Priorità bassa, dopo stabilizzazione Erlang

---

*Ultimo aggiornamento: 9 gennaio 2026*
