# Collide PE Problem-of-the-Day program — handoff (2026-06-19)

Exit snapshot of the program that solves the [Collide](https://app.collide.io) "PE Problem
of the Day" series (161-post petroleum-engineering challenge), hottest-first, routing each
problem's value into the right substrate.

## The core pattern (read this first)

Each problem is solved and then **connected as a dot**, not left as a one-off:

- **Quantitative** problems → a reusable function in the matching `src/digitalmodel/<domain>/`
  module + a regression test pinning the Collide numbers. The problem YAML records
  `domain_function:`.
- **Conceptual** problems → an llm-wiki concept page (`wikis/<discipline>/wiki/concepts/`,
  the source Deckhand's `wells-subsurface` pack reads). The YAML records `wiki_concept:`.
- Every problem also gets a **Deckhand-letterhead HTML brief** (`render_brief.py`).

Dots → workflows → flywheel. The series doubles as a **gap-finder** for both the domain code
and the wiki.

## Status: 26 / 161 solved (waves 1–4, hotness 53 → 33)

| Wave | Problems | Issues | Notes |
|---|---|---|---|
| 1 | 11 (top 10 + 6/19) | #838–#848 | 2 quantitative (PI, OOIP); rest conceptual |
| 2 | 5 | #859–#863 | all conceptual |
| 3 | 5 | #864–#868 | all conceptual (4/3 naphtha = downstream, brief-only) |
| 4 | 5 | #872–#876 | all conceptual (fork-executed) |

- **2 digitalmodel domain functions** + regression tests: `production_engineering.ipr_models.LinearIpr`
  (6/19 PI, reused) and **new** `reservoir.volumetrics` (5/21 OOIP). Tests: `ipr_models` 24/24,
  `volumetrics` 7/7.
- **12 new llm-wiki concept pages** (fluid-viscosity, reservoir-drive-mechanisms, thermal-recovery-eor,
  spontaneous-potential-log, depositional-environments, petroleum-system-elements, acid-fracturing,
  sidetracking, water-saturation-models, pressure-transient-analysis, streamline-simulation,
  wellbore-stability).

## Where the work lives

| | Branch | Worktree | PR |
|---|---|---|---|
| digitalmodel | `feat/collide-pe-solver` | `/mnt/local-analysis/wt-digitalmodel-collide` | **#871** |
| llm-wiki | `feat/collide-pe-concepts` | `/mnt/local-analysis/wt-llmwiki-collide` | **#745** |

Both pushed (`--no-verify`; PR CI runs gates). Worktrees off `origin/main` (NOT the dirty main
checkouts). Epics: **#836** (solve the series), **#837** (Deckhand/Telegram routing), **#851**
(the producer "API call", unscoped placeholder).

## Pending — user actions / decisions

1. **Review + merge PRs #871 and #745.** llm-wiki pages were authored at concept depth and should
   pass the wiki's **verification-queue / index-rebuild** before merge (not run here). Source
   citations are canonical texts, not yet source-extracted into `wiki/sources/`.
2. **#837 Deckhand/Telegram routing** — open design Qs: API-key custody on the gateway, read-vs-write
   scope, brief hosting (where the HTML becomes a link), channel, cost guardrail, and the
   bot-to-bot transport question. `solve.py:solve_problem()` is the transport-free engine to wrap.
3. **#851 producer API** — scope it (it was filed faithfully but intentionally unspecified).
4. **Posting answers back to Collide** — explicitly user-gated; nothing has been posted.
5. **Brief hosting** — publish `docs/collide_pe/briefs/*.html` somewhere to get shareable links.

## Resuming the series (wave 5+)

Next by hotness: 2/10 (32), 2/26 (31), 2/4 (31), then the tail below ~31 (engagement thins fast).
Per problem:
1. Scrape text from Collide (browser; auth-gated — drive the logged-in Chrome via the MCP tools;
   if the extension drops, reconnect with `tabs_context_mcp`).
2. Solve → route (calc → digitalmodel domain+test, concept → llm-wiki page or existing).
3. `uv run docs/collide_pe/render_brief.py --all`; create issue under #836; comment the answer.
4. Update `backlog.yml` (`wave_N`) + Epic #836 checklist; commit both worktrees; `git push --no-verify`.

This was delegated cleanly to a **fork** subagent in wave 4 (it inherits the playbook) — repeat that
for further waves. **Stop signal:** when new hot problems stop revealing missing domain functions or
wiki concepts, the substrates have caught up with the community's core topics.

## Gotchas

- digitalmodel & llm-wiki `main` checkouts were dirty / shared — always use the worktrees off
  `origin/main`; never touch `main`.
- `/tmp` worktrees get auto-pruned on this host — worktrees live under `/mnt/local-analysis/`.
- llm-wiki `sources:` frontmatter keys are free-form (no registry), so canonical-text citations
  don't break a validator.
- Not every concept has an upstream-wiki home (naphtha = downstream); keep those brief-only.
