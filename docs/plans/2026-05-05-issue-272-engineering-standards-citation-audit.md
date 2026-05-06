# Plan: digitalmodel #272 — WRK-662 Engineering standards citation audit via Gemini

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/272
**Status:** plan-review
**Tier:** T2 (audit-and-report; no source-of-truth code changes, only annotation + ledger updates)

## Context

Issue #272 (WRK-662) sweeps every digitalmodel engineering module for standards citations (DNV, API, ISO, ABS, BS — 15+ standards) and produces a four-table audit report flagging: (1) conflicting version years for the same standard across modules, (2) clause citations with implausible section-number patterns, (3) modules referencing a standard without a version year, (4) standards used in multiple modules with **different acceptance thresholds**. The fourth case is the highest-leverage finding — divergent thresholds for the same parameter is a calculation-correctness bug class.

Output: `specs/wrk/WRK-662/standards-citation-audit.md` with all four tables populated, cross-checked against `data/document-index/standards-transfer-ledger.yaml` (≥80% of cited standards must be tracked in the ledger). The issue's `Status: Stage 17: Reclaim (n)` and the WRK stages 1–16 marked done suggest the audit may have already shipped. Verify before re-running.

**Stale-flag:** Possibly. Status indicates the work is at the close-out gate. If `specs/wrk/WRK-662/standards-citation-audit.md` already exists with all four tables populated, this issue collapses to a verify-and-close (Task 4 only). Plan structured to support both paths.

The repo has a citation contract already (`.claude/rules/calc-citation-contract.md`) and a pilot at `digitalmodel/src/digitalmodel/citations/schema.py` (verified — `src/digitalmodel/citations/` is in the source tree). The audit feeds the citation rollout.

## Plan

### Task 1 — Verify whether the audit has already landed
Run `ls specs/wrk/WRK-662/` and `git log -- specs/wrk/WRK-662/`. If the audit doc exists with all four tables, skip to Task 4. If absent or stub, proceed.

### Task 2 — Run the Gemini sweep
Use `submit-to-gemini.sh` (per the user's memory note on `GEMINI_CLI_TRUST_WORKSPACE=true`) to run a single-pass audit over all engineering modules. Source list: every `.py` under `src/digitalmodel/{drilling_riser, fatigue, geotechnical, hydrodynamics, marine_ops, mooring*, nde, orcaflex, orcawave, structural, subsea, ...}`. Prompt asks Gemini to extract every standard citation (regex anchor: `(?:DNV|API|ISO|ABS|BS|AISC|AWS)[\s-]+(?:RP\s+)?[A-Z0-9-]+(?:\s+\d{4})?`), then synthesize the four target tables. Capture raw output to `specs/wrk/WRK-662/gemini-raw.md` for audit trail.

### Task 3 — Author the four-table report
Write `specs/wrk/WRK-662/standards-citation-audit.md`:
- **Table 1: Version conflicts** — standard, modules-A, year-A, modules-B, year-B, severity.
- **Table 2: Implausible clauses** — module, file:line, citation string, why-implausible.
- **Table 3: Missing version year** — module, file:line, standard cited, action (year-added inline / deferred-with-rationale).
- **Table 4: Divergent thresholds** — parameter, standard, threshold-A, source-A, threshold-B, source-B.
Then cross-check the union of cited standards against `data/document-index/standards-transfer-ledger.yaml`; coverage must be ≥80%, gap list appended.

### Task 4 — Inline-fix small issues, file follow-ons for big ones
For Table 3 missing-year entries: if the canonical version is unambiguous (e.g., `DNV-RP-F101 2021` per the standards ledger), add the year inline as a one-line edit and reference the audit. For Table 1 conflicts: file a per-conflict follow-on issue rather than silently picking a winner — version selection is a calc-correctness decision the user owns.

### Task 5 — Verify and close
Run the legal scan (`legal-sanity-scan` skill if it exists, else `scripts/legal-scan.sh`). Confirm the audit doc has no client-identifying material. Post the close comment with: file path, conflict count, threshold-divergence count, follow-on issue numbers.

## Acceptance Criteria

- [ ] `specs/wrk/WRK-662/standards-citation-audit.md` exists with all four tables populated.
- [ ] Conflict (Table 1) resolution: each entry either has an inline-comment fix at the cited file:line OR a follow-on issue number. ≤5 unresolved at close-time per the issue's acceptance bullet.
- [ ] Missing-version (Table 3): each entry shows action (year-added or deferred-with-rationale).
- [ ] Cross-check shows ≥80% of cited standards present in `standards-transfer-ledger.yaml`; gap list filed.
- [ ] Legal scan exits 0.
- [ ] Raw Gemini output saved at `specs/wrk/WRK-662/gemini-raw.md` for audit trail.

## Open questions

- Threshold-divergence (Table 4) is the most important and the hardest to mechanically detect — Gemini will need carefully-scoped examples in the prompt. Plan defers prompt-engineering tuning to execution time.
- Should this audit also check wiki-citation alignment (per `.claude/rules/calc-citation-contract.md` schema)? Out-of-scope for the original issue but a high-yield extension — flag for owner before Task 2.
