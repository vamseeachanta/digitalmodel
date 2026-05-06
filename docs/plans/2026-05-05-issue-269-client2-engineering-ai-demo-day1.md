# Plan: digitalmodel #269 — WRK-629 Client 2 AI demo Day 1 (system prompts + knowledge base)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/269
**Status:** plan-review
**Tier:** T2 (foundation prompt-engineering + 7 knowledge markdowns; no source code)

## Context

Issue #269 (WRK-629) is Day 1 of the Client 2 engineering AI demo and the foundation that #270 (Days 2–5, plan filed in parallel) builds on. Client 2 disciplines: (1) **Diffraction analysis** (AQWA/WAMIT/OrcaFlex panel-method, RAOs, QTFs), (2) **FFS for plates** (API 579-1 Part 4/5/8/9 — note plate methodology differs from pipe: net-section collapse vs. burst-pressure RSF), (3) **GoA analysis** (API RP 2EQ seismic, API RP 2MET extreme metocean, API RP 2N Arctic ice, NEPA/ESA permitting), (4) **Maritime legal cases consulting** (MAIB/NTSB/USCG casualty investigation, ISM Code, COLREGs, Jones Act, expert-witness support).

Deliverables: 2 system prompts (`structural_offshore_assistant.md`, `maritime_legal_consultant.md`), 7 knowledge-base markdowns (`api_579_plate_ffs.md`, `diffraction_analysis_reference.md`, `goa_design_criteria.md`, `api_rp_2a_structural.md`, `maritime_casualty_investigation.md`, `admiralty_law_reference.md`, `expert_witness_framework.md`), folder scaffold under `ai-initiatives-client2/` (`prompts/`, `knowledge/`, `demo/`, `pilot/`), and 5 test questions validated per focus area.

The deliverables live in **a TBD client-2 repo**, not digitalmodel — verified by the issue body's `Repo: ['TBD-client2-repo']`. Status `Stage 17: Reclaim (n)` with stages 1–16 done suggests the foundation may have already shipped. Verify before re-doing.

**Stale-flag (mis-filed + possibly landed):** Mis-filed in digitalmodel; target repo TBD. Status indicates close-out gate. Plan supports both verify-and-close and fresh-write paths. #269 is a prerequisite for #270 — sequencing matters.

## Plan

### Task 1 — Verify whether the foundation has already landed
Run `find . -path '*ai-initiatives-client2/prompts/*' -o -path '*ai-initiatives-client2/knowledge/*'`. If both system prompts and all 7 knowledge files exist with non-stub content, collapse to verify-and-close (Task 5 only).

### Task 2 — System prompts
Author the two system prompts:
- `ai-initiatives-client2/prompts/structural_offshore_assistant.md` — covers diffraction (AQWA/WAMIT/OrcaFlex conventions, RAO/QTF interpretation, wave heading matrices), FFS for plates (Part 4/5/8/9 with the explicit plate-vs-pipe methodology distinction), GoA analysis (API RP 2EQ, API RP 2MET, API RP 2N, environmental permitting). Primary codes section: API RP 2EQ, API RP 2A-WSD/LRFD, ISO 19902, API 579-1, BS 7910, API RP 2MET, API RP 2N, AISC 360, AWS D1.1.
- `ai-initiatives-client2/prompts/maritime_legal_consultant.md` — covers casualty investigation (MAIB / NTSB Marine / USCG MISLE), ISM Code non-conformity, expert witness support (Daubert/Civil Evidence Act framing), admiralty law (COLREGs, Jones Act, Hague-Visby, Limitation Act), incident-data analysis via `worldenergydata.MAIBLoader` + `NTSBMarineLoader` (WRK-320 done).

### Task 3 — Knowledge base files
Author the 7 markdowns:
- `knowledge/api_579_plate_ffs.md` — Part 4 (general metal loss), Part 5 (LTA), Part 8 (shell distortions), Part 9 (cracks via FAD); explicit "plate uses limit-load, not burst-pressure" distinction with worked example.
- `knowledge/diffraction_analysis_reference.md` — panel method theory, AQWA/WAMIT model setup conventions, RAO/QTF interpretation, damping models.
- `knowledge/goa_design_criteria.md` — 100-yr design parameters, seismic zones, ice drift/pressure, BSEE GoA lease/permit context, ADEC environmental.
- `knowledge/api_rp_2a_structural.md` — tubular joint checks, member utilisation, fatigue SCFs, in-service inspection intervals.
- `knowledge/maritime_casualty_investigation.md` — MAIB/NTSB/USCG investigation process, ISM Code audit framework, root-cause taxonomy table.
- `knowledge/admiralty_law_reference.md` — COLREGs rules summary, Jones Act key provisions, limitation thresholds, P&I Club claim stages.
- `knowledge/expert_witness_framework.md` — report structure, Daubert standard (US) / Civil Evidence Act (UK), engineering-opinion-as-standard-of-care framing, deposition prep.

### Task 4 — Folder scaffold + test questions
Create the four-directory scaffold (`prompts/`, `knowledge/`, `demo/`, `pilot/`). For each focus area (diffraction, plate FFS, GoA, maritime legal), author 5 test questions and run them against the system prompt (Claude API or interactive) — capture pass/fail and refine the prompt until at least 4 of 5 produce engineering-coherent answers per area. Save the validated Q&A under `pilot/test-questions/<area>.md`.

### Task 5 — Verify, scan, close
Run the legal-scan skill across all new files (no client-proprietary references; only public codes/standards allowed). Confirm `target_repos` is updated from TBD to the actual client repo when provisioned. Close with the verification artifact list.

## Acceptance Criteria

- [ ] `ai-initiatives-client2/prompts/structural_offshore_assistant.md` present with all four discipline sections.
- [ ] `ai-initiatives-client2/prompts/maritime_legal_consultant.md` present with casualty / expert-witness / admiralty / regulatory sections.
- [ ] All 7 knowledge-base markdowns present with the required content.
- [ ] Plate-vs-pipe FFS distinction explicit in the system prompt (single-line easy-to-find statement).
- [ ] 5 test questions validated per focus area; ≥4/5 produce coherent answers.
- [ ] Folder scaffold (`prompts/`, `knowledge/`, `demo/`, `pilot/`) in place.
- [ ] `target_repos` updated from TBD to actual repo (or gap documented).
- [ ] Legal scan exits 0; no client-proprietary content.

## Open questions

- **Repo scope:** confirm the canonical client-2 repo path. If still TBD, deliverables may stage in `ai-initiatives-client2/` (workspace-hub-adjacent) and migrate later — flag the migration as a follow-on issue.
- **Live MAIB/NTSB validation:** the loaders exist; should the maritime statistical-context test question be wired to a real query at validation time, or stub-only at Day 1? Recommend live to catch loader regressions early.
- **Citation contract:** any worked example citing a standards-derived numeric must comply with `.claude/rules/calc-citation-contract.md`. Knowledge files often quote thresholds — flag if Task 3 produces examples the contract would reject.
