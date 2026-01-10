# Roadmap 2.0 — Confronto implementazione vs. obiettivi

Questo documento confronta gli obiettivi definiti in `ROADMAP_2.0.md` con lo stato corrente della codebase e propone azioni raccomandate.

**File creato:** [ROADMAP_2.0_COMPARISON.md](ROADMAP_2.0_COMPARISON.md)


**Tabella delle differenze**

| Area | Roadmap (obiettivo) | Stato attuale | Raccomandazione / Azione |
|---|---|---|---|
| Entry point | Unico import pubblico `import str` | Implementato: `src/str.gleam` re-esporta funzioni; test verdi | Mantieni: già conforme. Aggiornare README con esempi unificati. |
| Search API | `str.index_of(text, pattern)` default simple algorithm; `index_of_auto` heuristic; `index_of_strategy` explicit strategy | Esistono `index_of`, `index_of_auto`, `index_of_strategy`, `count`, `count_auto`, `count_strategy` | Decisione: mantenere tre API distinte — `index_of` come algoritmo semplice/predefinito, `index_of_auto` per l'euristica/auto-optimizer e `index_of_strategy` per scelta esplicita. Aggiungere alias opzionali `index_of_simple`/`count_simple` e aggiornare `README` con esempi e linee guida di migrazione. |
| Truncate API | Singola firma `truncate(..., preserve_emoji: Bool)` | Multiple varianti: `truncate`, `truncate_preserve`, `truncate_strict`, `truncate_with_flag` | Nota: queste sono funzioni semanticamente diverse e vanno mantenute. Raccomandazione: non unificare forzatamente; documentare chiaramente le differenze (simple/strict/preserve) e fornire un wrapper esplicito opzionale `truncate(text, max_len, suffix, preserve_emoji: Bool)` che invoca `truncate_with_flag` per chi preferisce un'unica firma. Aggiungere esempi nel `README`. |
| Advanced module | `str/advanced` opzionale per funzioni low-level (KMP maps, ecc.) | Funzioni avanzate (es. `build_kmp_maps`, `kmp_search_all_with_maps`) esposte nel main | Creare `src/str/advanced.gleam` e spostare (o re-export) le API avanzate; mantenere alias nel main per transizione. |
| KMP maps & caching | Fornire API per costruire e riusare mappe (documentate) | `build_kmp_maps` presente nel main; bench script esistente | Spostare sotto `str/advanced`; documentare pattern di caching + esempi; aggiungere benchmark CI. |
| Tipi pubblici | `SearchStrategy` e `FillPosition` re-esportati, costruttori accessibili | Tipi re-esportati correttamente; mapping fatto in `src/str.gleam` | Aggiungere esempi nel README su come usare i costruttori (es. `str.Kmp` ecc.). |
| Tokenizer | Reference implementation pure-Gleam come interno | Tokenizer esposto internamente in `str/internal/tokenize`; main non espone low-level | Lasciare come internal; documentare che è riferimento e valutare implementazione nativa futura opzionale. |
| Configuration | Modulo `str/config` per toggle opt-in e soglie di ricerca | `src/str/config.gleam` esiste con default (es. `smart_search_enabled()` = False, soglie KMP) | Mantenerlo come modulo puro e separato; documentare come sovrascriverlo a build-time; aggiungere test per i due casi (True/False) e valutare un wrapper `choose_search_strategy_with_config` in `str/advanced` per chi desidera passare config runtime. |
| Tests & compatibilità | Test estesi e green | 368 test passati; import aggiornati | Ottimo. Aggiungere test che verificano wrapper deprecati mappino alle nuove API. |
| Migrazione tooling | Script `str/migrate` per riscrivere import e chiamate | Non presente come file eseguibile; roadmap lo suggeriva | Implementare script migrator (Gleam script o piccolo tool Python) e includere esempi d'uso. |
| Documentazione | README + CHANGELOG aggiornati con note di migrazione | ROADMAP_2.0.md presente; README ha riferimenti ma va aggiornato | Aggiornare `README.md` e `CHANGELOG.md` con snippet di migrazione e policy di deprecazione. |



**Raccomandazioni prioritarie (breve)**

- Consolidamento API (alta priorità):
  - Mantenere `str.index_of` e `str.count` come implementazioni semplici (default).
  - Mantenere `*_auto` per l'euristica/auto-optimizer e `*_strategy` per scelta esplicita.
  - Aggiungere alias opzionali `index_of_simple` / `count_simple` per chiarezza e compatibilità.

- Truncate (alta priorità):
  - Le varianti (`truncate`, `truncate_preserve`, `truncate_strict`, `truncate_with_flag`) hanno comportamenti distinti e vanno mantenute.
  - Fornire un wrapper opzionale `truncate(text, max_len, suffix, preserve_emoji: Bool)` che chiama `truncate_with_flag` per chi preferisce un'unica firma; mantenere le funzioni esistenti per retrocompatibilità.

- Modulo `str/advanced` (medium):
  - Creare `src/str/advanced.gleam` che re-esporta / contenga API low-level: `build_kmp_maps`, `kmp_search_all_with_maps`, `kmp_index_of_with_maps`, `kmp_search_all`, `kmp_index_of`, `sliding_*` e helper di caching.
  - Lasciare alias nel main per 1–2 release (transizione).

- Tooling & docs (high):
  - Implementare il migrator automatico (`scripts/migrate_imports.py` o script Gleam) e inserire esempi nel `README.md`.
  - Aggiungere una sezione “Advanced” nel `README` con pattern di caching KMP e linee guida su quando usare le API avanzate.

- CI & benchmark (medium):
  - Aggiungere job CI che esegue benchmark sui path critici (KMP vs sliding) e segnala regressioni di performance.

**Proposta di passi immediati**

1. Creare `src/str/advanced.gleam` con re-export delle funzioni avanzate dal `core`.
2. Aggiungere alias deprecati nel `src/str.gleam` che chiamano le nuove API centralizzate.
3. Aggiornare `README.md` con esempi di migration e uso di `str.Kmp` / `str.Sliding`.
4. Implementare piccolo script `scripts/migrate_imports.py` (o `gleam` script) che riscrive import e prefissi di funzione.
5. Aggiungere test che confermano la compatibilità dei wrapper deprecati.


**Link utili**

- Roadmap: [ROADMAP_2.0.md](ROADMAP_2.0.md)
- Main module: [src/str.gleam](src/str.gleam)


---

Vuoi che proceda subito con la creazione del modulo `src/str/advanced.gleam` e la creazione degli alias deprecati nel main (opzione suggerita)?