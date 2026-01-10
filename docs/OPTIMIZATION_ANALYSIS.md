# Analisi Ottimizzazioni per str

Documento che analizza pattern, algoritmi e scelte architetturali estratti dal repository `string-width` e altri best practices, applicabili alle funzionalità esistenti della libreria `str`.

> **Nota**: Questo documento NON propone l'implementazione di funzionalità width/display-width. Si concentra esclusivamente su pattern algoritmici e architetturali che possono migliorare le prestazioni e la solidità delle funzionalità già presenti in `str`.

---

## 1. Indice

1. [Stato Attuale di str](#2-stato-attuale-di-str)
2. [Pattern BitArray per Lookup Tables](#3-pattern-bitarray-per-lookup-tables)
3. [Ottimizzazione Classificazione Codepoint](#4-ottimizzazione-classificazione-codepoint)
4. [Options Builder Pattern](#5-options-builder-pattern)
5. [Ottimizzazione Iteration con Fold](#6-ottimizzazione-iteration-con-fold)
6. [FFI Native Layer](#7-ffi-native-layer)
7. [Controllo FFI via config.gleam](#8-controllo-ffi-via-configgleam)
8. [Roadmap Implementazione](#9-roadmap-implementazione)

---

## 2. Stato Attuale di str

### 2.1 Funzionalità Principali

| Modulo | Funzionalità | Pattern Attuale |
|--------|--------------|-----------------|
| `core.gleam` | Ricerca substring (KMP/Sliding) | Dict-based prefix table |
| `core.gleam` | Emoji detection | Nested case con range checks |
| `core.gleam` | Grapheme counting | `string.to_graphemes` + `list.length` |
| `extra.gleam` | `ascii_fold` / `translit` | `list.fold` su List of tuples (~130 entries) |
| `extra.gleam` | `slugify` | Parametri espliciti, non configurabile |
| `decompose_pure.gleam` | Latin decomposition | `list.fold` su List of tuples (~100 entries) |
| `translit_pure.gleam` | Transliteration | `list.fold` su List of tuples (~130 entries) |
| `translit_pure.gleam` | `remove_combining_marks` | Range checks nested |

### 2.2 Problemi Identificati

1. **Tabelle come List**: `decompose` e `translit` usano `List(#(String, String))` con `list.fold` → O(n) per ogni carattere, iterazione completa della lista per ogni lookup.

2. **Range Checks Nested**: `cluster_has_emoji` e `remove_combining_marks` usano case statements annidati → difficili da manutenere, non estensibili.

3. **Nessun Builder Pattern**: `slugify_opts` richiede 4 parametri posizionali → API fragile, breaking changes frequenti.

4. **Dict per KMP**: La prefix table KMP usa `Dict(Int, String)` → overhead allocazione, hashing ad ogni lookup.

---

## 3. Pattern BitArray per Lookup Tables

### 3.1 Concetto

Il pattern estratto da `string-width/tables.gleam` usa BitArray literals per creare tabelle di lookup O(1) ultra-compatte. Invece di iterare una lista di ~130 tuple, si usa un BitArray binario con indici calcolati.

### 3.2 Applicabilità a str

#### 3.2.1 Transliteration Lookup

**Attuale** (`translit_pure.gleam`):
```gleam
pub fn replacements_pure() -> List(#(String, String)) {
  [
    #("À", "A"),
    #("Á", "A"),
    // ... ~130 entries
  ]
}

// Uso: O(n) per ogni carattere
list.fold(table, s, fn(acc, pair) {
  string.replace(acc, pair.0, pair.1)
})
```

**Proposto** - Schema a due livelli:
```gleam
// Generato da script Python
const translit_index: BitArray = <<...>>  // Indice sparse per codepoint → offset
const translit_data: BitArray = <<...>>   // Dati replacement compressi

pub fn translit_lookup(codepoint: Int) -> Result(String, Nil) {
  // 1. Calcola bucket index dal codepoint
  // 2. Leggi offset da translit_index
  // 3. Estrai replacement da translit_data
  // Complessità: O(1)
}
```

**Benefici**:
- Tempo lookup: O(n) → O(1)
- Dimensione binario: riduzione ~60-80% (no tuple overhead)
- Compile time: riduzione significativa (meno AST nodes)

#### 3.2.2 Decompose Lookup

**Attuale** (`decompose_pure.gleam`):
```gleam
let table = [
  #("Á", "A\u{0301}"),
  #("À", "A\u{0300}"),
  // ... ~100 entries
]
list.fold(table, s, fn(acc, pair) {
  string.replace(acc, pair.0, pair.1)
})
```

**Proposto**: Stesso pattern BitArray con indice sparse.

#### 3.2.3 Combining Marks Detection

**Attuale** (`translit_pure.gleam`):
```gleam
case i >= 0x0300 && i <= 0x036F
  || i >= 0x1AB0 && i <= 0x1AFF
  || i >= 0x1DC0 && i <= 0x1DFF
  || i >= 0x20D0 && i <= 0x20FF
  || i >= 0xFE20 && i <= 0xFE2F
```

**Proposto**: BitArray con bit-per-codepoint per range densi:
```gleam
// 1 bit per codepoint nel range 0x0300-0x036F (112 bits = 14 bytes)
const combining_marks_0300: BitArray = <<0b11111111, 0b11111111, ...>>

pub fn is_combining_mark(cp: Int) -> Bool {
  case cp >= 0x0300 && cp <= 0x036F {
    True -> bit_array.get_bit(combining_marks_0300, cp - 0x0300)
    False -> // check altri range
  }
}
```

### 3.3 Script Generatore

Creare `scripts/generate_bitarray_tables.py`:
```python
def generate_translit_bitarray():
    """Genera BitArray Gleam per transliteration lookup."""
    # Input: lista di (codepoint, replacement)
    # Output: Gleam const declarations per index + data
```

---

## 4. Ottimizzazione Classificazione Codepoint

### 4.1 Emoji Detection

**Attuale** (`core.gleam:cluster_has_emoji`):
```gleam
fn cluster_has_emoji(cluster: String) -> Bool {
  let cps = string.to_utf_codepoints(cluster)
  list.any(cps, fn(cp) {
    let code = string.utf_codepoint_to_int(cp)
    case code == 0x200D || code == 0xFE0F || code == 0x20E3 {
      True -> True
      False ->
        case code >= 0x1F3FB && code <= 0x1F3FF {
          True -> True
          False ->
            case code >= 0x1F1E6 && code <= 0x1F1FF {
              // ... 4 livelli di nesting
            }
        }
    }
  })
}
```

**Problemi**:
1. Nesting profondo → difficile manutenzione
2. Non copre tutti i casi Unicode (UAX #29 incompleto)
3. Ogni check è sequenziale

**Proposto** - Approccio ispirato a `string-width`:
```gleam
// BitArray sparso per emoji indicators
const emoji_indicators: BitArray = <<...>>

fn is_emoji_indicator(code: Int) -> Bool {
  // Single-codepoint quick checks
  case code {
    0x200D | 0xFE0F | 0x20E3 -> True
    _ -> bitarray_contains(emoji_indicators, code)
  }
}

// Range check ottimizzato con early exit
fn is_in_emoji_range(code: Int) -> Bool {
  // Ordine ottimizzato: range più comuni prima
  code >= 0x1F000 && code <= 0x1FAFF  // Pictographic (più comune)
  || code >= 0x1F3FB && code <= 0x1F3FF  // Skin tones
  || code >= 0x1F1E6 && code <= 0x1F1FF  // Regional
  || code >= 0xE0020 && code <= 0xE007F  // Tags
}
```

### 4.2 Vantaggi

- **Flat structure**: Niente nesting, logica chiara
- **Estensibile**: Aggiungere range = aggiungere riga
- **Testabile**: Ogni funzione helper testabile in isolamento
- **Performance**: Early exit sui casi comuni

---

## 5. Options Builder Pattern

### 5.1 Problema Attuale

`slugify_opts` ha signature fragile:
```gleam
pub fn slugify_opts(
  s: String,
  max_len: Int,      // -1 = unlimited (magic number)
  sep: String,
  preserve_unicode: Bool,
) -> String
```

**Problemi**:
- Magic number `-1` per "unlimited"
- Ordine parametri da ricordare
- Aggiungere opzioni = breaking change
- Nessuna validazione

### 5.2 Pattern Proposto

Ispirato a `string-width` Options:

```gleam
// Tipo opaco per opzioni
pub opaque type SlugifyOptions {
  SlugifyOptions(
    max_tokens: Option(Int),
    separator: String,
    preserve_unicode: Bool,
    lowercase: Bool,
    custom_replacements: List(#(String, String)),
  )
}

// Default builder
pub fn slugify_options() -> SlugifyOptions {
  SlugifyOptions(
    max_tokens: None,
    separator: "-",
    preserve_unicode: False,
    lowercase: True,
    custom_replacements: [],
  )
}

// Fluent setters
pub fn with_max_tokens(opts: SlugifyOptions, n: Int) -> SlugifyOptions {
  SlugifyOptions(..opts, max_tokens: Some(n))
}

pub fn with_separator(opts: SlugifyOptions, sep: String) -> SlugifyOptions {
  SlugifyOptions(..opts, separator: sep)
}

pub fn with_preserve_unicode(opts: SlugifyOptions, v: Bool) -> SlugifyOptions {
  SlugifyOptions(..opts, preserve_unicode: v)
}

pub fn with_custom_replacements(
  opts: SlugifyOptions,
  replacements: List(#(String, String)),
) -> SlugifyOptions {
  SlugifyOptions(..opts, custom_replacements: replacements)
}

// Uso
pub fn slugify_with_options(s: String, opts: SlugifyOptions) -> String {
  // Implementazione
}
```

### 5.3 Benefici

| Aspetto | Prima | Dopo |
|---------|-------|------|
| Aggiungere opzioni | Breaking change | Non-breaking |
| Default values | Magic numbers | Espliciti nel builder |
| Validazione | Nessuna | Nel builder/setter |
| Discoverability | Docs o signature | Autocompletamento |
| Composizione | Impossibile | `opts \|> with_x \|> with_y` |

---

## 6. Ottimizzazione Iteration con Fold

### 6.1 Pattern Attuale in str

Molte funzioni in `str` iterano carattere per carattere:

```gleam
// slugify
let raw = list.fold(clusters, "", fn(acc, g) {
  case is_alnum_grapheme(g) {
    True -> acc <> g
    False -> ...
  }
})
```

**Problema**: Concatenazione stringa in loop → O(n²) per stringhe lunghe.

### 6.2 Pattern Ottimizzato

Usare `StringBuilder` pattern o accumulare lista e join finale:

```gleam
// Opzione 1: Accumula lista, join alla fine
let parts = list.fold(clusters, [], fn(acc, g) {
  case is_alnum_grapheme(g) {
    True -> [g, ..acc]
    False -> ...
  }
})
|> list.reverse
|> string.concat

// Opzione 2: Usa string.join per costruzione efficiente
let kept = list.filter_map(clusters, fn(g) {
  case is_alnum_grapheme(g) {
    True -> Some(g)
    False -> None
  }
})
string.concat(kept)
```

### 6.3 Applicabilità

Funzioni da ottimizzare con questo pattern:
- `slugify_opts` (concatenazione in loop)
- `ascii_fold_full` (string.replace multipli)
- `remove_combining_marks` (filter + concat)

---

## 7. FFI Native Layer

### 7.1 Architettura Delegante

La libreria usa un pattern a tre livelli per supportare FFI native con fallback pure Gleam:

```
┌─────────────────────┐
│  module.gleam       │  ← API pubblica stabile (mai modificata)
└────────┬────────────┘
         │ delega a
         ▼
┌─────────────────────┐
│  module_impl.gleam  │  ← Layer decisionale (sceglie native vs pure)
└────────┬────────────┘
         │
    ┌────┴────┐
    ▼         ▼
┌────────┐ ┌────────────┐
│ native │ │ _pure.gleam│  ← Fallback garantito
└────────┘ └────────────┘
```

### 7.2 FFI Erlang - Priorità Alta

#### 7.2.1 Funzioni Candidate

| Funzione | Modulo Erlang | Beneficio |
|----------|---------------|-----------|
| `decompose_latin` | `unicode:characters_to_nfd_binary/1` | NFD nativo, completo |
| `remove_combining_marks` | Pattern matching binario | O(1) per byte |
| `translit` | `unicode` + regex | Più completo |
| `to_graphemes` | `unicode:characters_to_list/1` | Già usato via gleam_stdlib |

#### 7.2.2 Implementazione Proposta

**File: `src/str/internal/decompose_native.gleam`**
```gleam
// FFI Erlang per decomposizione Unicode
@external(erlang, "str_ffi", "decompose_nfd")
pub fn decompose_latin_native(s: String) -> String

// Modulo Erlang: src/str_ffi.erl
// decompose_nfd(S) -> unicode:characters_to_nfd_binary(S).
```

**File: `src/str/internal/decompose_impl.gleam`** (aggiornato)
```gleam
import str/config
import str/internal/decompose_native as native
import str/internal/decompose_pure as pure

pub fn decompose_latin(s: String) -> String {
  case config.use_native_ffi() {
    True -> native.decompose_latin_native(s)
    False -> pure.decompose_latin_pure(s)
  }
}
```

#### 7.2.3 Modulo FFI Erlang

**File: `src/str_ffi.erl`**
```erlang
-module(str_ffi).
-export([decompose_nfd/1, remove_combining_marks/1]).

%% Decomposizione NFD usando unicode:characters_to_nfd_binary/1
decompose_nfd(S) when is_binary(S) ->
    unicode:characters_to_nfd_binary(S).

%% Rimozione combining marks (U+0300-U+036F, etc.)
remove_combining_marks(S) when is_binary(S) ->
    filter_combining(S, <<>>).

filter_combining(<<>>, Acc) -> Acc;
filter_combining(<<C/utf8, Rest/binary>>, Acc) ->
    case is_combining_mark(C) of
        true -> filter_combining(Rest, Acc);
        false -> filter_combining(Rest, <<Acc/binary, C/utf8>>)
    end.

is_combining_mark(C) when C >= 16#0300, C =< 16#036F -> true;
is_combining_mark(C) when C >= 16#1AB0, C =< 16#1AFF -> true;
is_combining_mark(C) when C >= 16#1DC0, C =< 16#1DFF -> true;
is_combining_mark(C) when C >= 16#20D0, C =< 16#20FF -> true;
is_combining_mark(C) when C >= 16#FE20, C =< 16#FE2F -> true;
is_combining_mark(_) -> false.
```

### 7.3 FFI JavaScript - Priorità Bassa

#### 7.3.1 Funzioni Candidate

| Funzione | API JS | Note |
|----------|--------|------|
| `decompose_latin` | `String.normalize('NFD')` | Nativo ES6 |
| `slugify` | `Intl.Segmenter` | Grapheme-aware |
| `remove_combining_marks` | Regex `/[\u0300-\u036f]/g` | Veloce |

#### 7.3.2 Implementazione Proposta

**File: `src/str/internal/decompose_native.mjs`**
```javascript
export function decompose_nfd(s) {
  return s.normalize('NFD');
}

export function remove_combining_marks(s) {
  return s.normalize('NFD').replace(/[\u0300-\u036f\u1ab0-\u1aff\u1dc0-\u1dff\u20d0-\u20ff\ufe20-\ufe2f]/g, '');
}
```

**File: `src/str/internal/decompose_native.gleam`** (target JS)
```gleam
@external(javascript, "./decompose_native.mjs", "decompose_nfd")
pub fn decompose_latin_native(s: String) -> String

@external(javascript, "./decompose_native.mjs", "remove_combining_marks")
pub fn remove_combining_marks_native(s: String) -> String
```

### 7.4 Struttura File FFI

```
src/
├── str_ffi.erl                          # FFI Erlang
├── str/
│   └── internal/
│       ├── decompose.gleam              # API pubblica (delega a impl)
│       ├── decompose_impl.gleam         # Scelta native vs pure
│       ├── decompose_pure.gleam         # Fallback pure Gleam
│       ├── decompose_native.gleam       # Dichiarazioni @external
│       ├── decompose_native.mjs         # Implementazione JS (opzionale)
│       ├── translit.gleam
│       ├── translit_impl.gleam
│       ├── translit_pure.gleam
│       ├── translit_native.gleam
│       └── translit_native.mjs
```

---

## 8. Controllo FFI via config.gleam

### 8.1 Flag di Controllo

Aggiungere a `src/str/config.gleam`:

```gleam
/// Controlla se usare implementazioni FFI native quando disponibili.
/// Default: False (usa sempre pure Gleam per sicurezza/portabilità).
/// Impostare a True per abilitare FFI Erlang/JS ottimizzate.
pub fn use_native_ffi() -> Bool {
  False
}

/// Controlla se preferire FFI solo per operazioni specifiche.
/// Utile per abilitare gradualmente funzioni native.
pub fn native_decompose_enabled() -> Bool {
  use_native_ffi()
}

pub fn native_translit_enabled() -> Bool {
  use_native_ffi()
}

pub fn native_combining_marks_enabled() -> Bool {
  use_native_ffi()
}
```

### 8.2 Uso nei Moduli _impl

**`decompose_impl.gleam`**:
```gleam
import str/config

pub fn decompose_latin(s: String) -> String {
  case config.native_decompose_enabled() {
    True -> {
      // Prova native, fallback su errore
      // Per ora: sempre pure (native non implementato)
      pure.decompose_latin_pure(s)
    }
    False -> pure.decompose_latin_pure(s)
  }
}
```

### 8.3 Override a Build-Time

I consumatori della libreria possono sostituire `config.gleam` a build-time:

```gleam
// Nel progetto consumatore: src/str/config.gleam (override)
pub fn use_native_ffi() -> Bool {
  True  // Abilita FFI native
}

pub fn smart_search_enabled() -> Bool {
  True  // Abilita anche smart search
}
// ... altri override
```

### 8.4 Vantaggi del Pattern

| Aspetto | Beneficio |
|---------|-----------|
| **Sicurezza** | Default pure = nessun rischio FFI |
| **Opt-in** | Utenti abilitano esplicitamente |
| **Granularità** | Controllo per-funzione |
| **Testing** | Facile testare pure vs native |
| **Rollback** | Un flag per disabilitare tutto |
| **Build-time** | Nessun overhead runtime per decisione |

### 8.5 Piano di Rollout

1. **Fase 1** (Attuale): `use_native_ffi() -> False`, tutto pure Gleam
2. **Fase 2**: Implementare FFI Erlang, testare con `True`
3. **Fase 3**: Documentare come abilitare, raccogliere feedback
4. **Fase 4**: Considerare `True` come default (major version)
5. **Fase 5** (Futuro): Aggiungere FFI JavaScript

---

## 9. Roadmap Implementazione

### Fase 1: BitArray Tables (Alta Priorità)

| Task | Impatto | Effort |
|------|---------|--------|
| Creare `scripts/generate_bitarray_tables.py` | Fondamentale | Medio |
| Implementare `translit` con BitArray | Alto | Medio |
| Implementare `decompose` con BitArray | Alto | Medio |
| Benchmark comparativi | Validazione | Basso |

**Risultato atteso**: Riduzione 60-80% tempo lookup caratteri, riduzione dimensione binario.

### Fase 2: Options Builder (Media Priorità)

| Task | Impatto | Effort |
|------|---------|--------|
| Creare `SlugifyOptions` type | API quality | Basso |
| Refactor `slugify_opts` → `slugify_with_options` | Non-breaking | Medio |
| Deprecare vecchia API | Migration path | Basso |
| Documentazione ed esempi | DX | Basso |

**Risultato atteso**: API stabile, extensible, developer-friendly.

### Fase 3: Codepoint Classification (Media Priorità)

| Task | Impatto | Effort |
|------|---------|--------|
| BitArray per combining marks | Performance | Basso |
| Refactor `cluster_has_emoji` | Manutenibilità | Medio |
| Aggiornare range emoji a Unicode 15 | Correttezza | Medio |

**Risultato atteso**: Emoji detection più accurato e manutenibile.

### Fase 4: Iteration Optimization (Bassa Priorità)

| Task | Impatto | Effort |
|------|---------|--------|
| Audit funzioni con string concat in loop | Identificazione | Basso |
| Refactor a pattern list-accumulate | Performance | Medio |
| Benchmark stringhe lunghe | Validazione | Basso |

**Risultato atteso**: Performance lineare per stringhe lunghe.

### Fase 5: FFI Erlang (Alta Priorità dopo Fase 1)

| Task | Impatto | Effort |
|------|---------|--------|
| Aggiungere `use_native_ffi()` a config.gleam | Fondamentale | Basso |
| Creare `src/str_ffi.erl` | Fondamentale | Medio |
| Implementare `decompose_native.gleam` | Alto | Basso |
| Implementare `translit_native.gleam` | Alto | Basso |
| Aggiornare `*_impl.gleam` per usare config | Integrazione | Basso |
| Test parità pure vs native | Validazione | Medio |
| Benchmark pure vs native | Validazione | Basso |

**Risultato atteso**: 10-100x speedup per decompose/translit con FFI Erlang.

### Fase 6: FFI JavaScript (Priorità Futura)

| Task | Impatto | Effort |
|------|---------|--------|
| Creare `decompose_native.mjs` | Medio | Basso |
| Creare `translit_native.mjs` | Medio | Basso |
| Aggiungere `@external(javascript, ...)` | Integrazione | Basso |
| Test su target JavaScript | Validazione | Medio |

**Risultato atteso**: Supporto multi-target ottimizzato.

---

## 10. Metriche di Successo

| Metrica | Target |
|---------|--------|
| Tempo `translit` su 1000 char (pure) | -70% |
| Tempo `translit` su 1000 char (native) | -95% |
| Tempo `decompose` su 1000 char (pure) | -70% |
| Tempo `decompose` su 1000 char (native) | -95% |
| Dimensione binario compilato | -20% |
| Copertura test | >95% |
| API breaking changes | 0 (deprecation path) |
| FFI fallback garantito | 100% casi |

---

## 11. Riferimenti

- **string-width**: Pattern BitArray lookup tables
- **UAX #29**: Unicode Text Segmentation
- **Gleam BitArray docs**: `bit_array` module
- **Erlang unicode module**: NFD/NFC normalization native
- **ROADMAP_1.3v2.md**: Pure Gleam optimizations planned

---

*Documento generato per la pianificazione ottimizzazioni str v1.3+*
