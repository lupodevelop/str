# Contributing to str

Thanks for helping! Short, practical guide.

## Quick start
- Fork, create a branch: `git switch -c feat/your-change`.
- Run `gleam format` and `gleam test` locally.
- Open a PR against `main` with a short description and tests.

## Setup
- Requirements: Gleam (see `gleam.toml`)

Commands:
```bash
gleam format
gleam test
```

## Commits
Use brief prefixes: `feat:`, `fix:`, `chore:`, `test:`, `perf:`.
Example: `feat(display): add truncate_display`
No strict enforcement, use these prefixes as a guideline, not a hard rule.

## PR checklist
- [ ] Tests added/updated
- [ ] `gleam format` & `gleam test` pass
- [ ] Update `CHANGELOG.md` if behaviour changes
- [ ] Document noteworthy changes in `README.md` , docs/ or examples/

## Deprecations
- Report breaking changes in an issue and add migration notes in PRs. See `DEPRECATIONS.md` if present.

## Testing
- Add unit tests for edge cases (ZWJ, skin tones, combining marks, CJK, ambiguous widths).
