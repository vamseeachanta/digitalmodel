# Engineering Writing Register

Canonical source: `workspace-hub/config/agents/SHARED_SOUL.md`, section "Engineering register".

These rules apply to documents, wiki pages, commit messages, chat, and email
produced by this repository's contributors and agents.

## Rules

1. **Subject is the analysis or component, not a person.**
   Write `the pipeline satisfies burst criteria` instead of `we verified the pipeline.`

2. **Every conclusion is bound to its criterion and comparator.**
   State the standard clause, limit value, and computed value together.

3. **Unestablished matters are stated as such with the missing evidence named.**
   Write `fatigue life is not established; S-N test data for this alloy are unavailable`
   instead of `fatigue life could not be determined.`

4. **`should`/`is recommended` for advice; `shall`/`must` for requirements only.**
   Do not use `shall` in a recommendation or `should` in a mandatory clause.

5. **No `acceptable`/`conservative`/`safe` without the governing criterion.**
   The criterion may appear in the same sentence or the immediately following clause.

6. **Captions below tables.**

7. **Three decimal places on thickness values (mm or in).**

## Exclusions

- Verbatim third-party transcriptions (standard text, vendor data sheets, quoted
  correspondence) are exempt. A linter that flags transcribed sources trains people
  to ignore it.

## Enforcement levels

- **Level 2:** `scripts/enforcement/check-engineering-register.py` runs
  manually or via `make check-register`. The self-test validates the detector fixtures.
- **Level 3:** Pre-commit hook and a dedicated CI step block violations in changed Markdown/reStructuredText files. The CI comparison uses `origin/main` for pull requests and the prior event SHA for pushes; existing findings in a modified file are also reported.

Register detector regressions shall cover quoted examples, code blocks, unrelated
dimensional quantities, failed Git comparisons, disposable repositories and
platform-independent in-memory text checks.
Diff scope selects changed files; each selected file is scanned in full.
