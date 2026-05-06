# Plan: digitalmodel #270 — WRK-630 Client 2 AI demo Days 2–5 (calc templates + workflows + demo package)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/270
**Status:** plan-review
**Tier:** T3 (large prompt-template + workflow library deliverable; ~15 markdown artifacts plus demo script)

## Context

Issue #270 (WRK-630) is the Days 2–5 deliverable set for the Client 2 engineering AI demo, building on WRK-629 (#269 — Day 1: system prompts + knowledge base). Acceptance criteria require ~15+ markdown artifacts: 3 diffraction calc templates (panel model, RAO interpretation, QTF), 3 plate FFS templates (API 579-1 Part 4 / 8 / 9), 2 GoA-specific calc templates (seismic + extreme metocean), a diffraction-to-structural-load workflow, a GoA FFS decision workflow, BSEE GoA data query prompts, 4 maritime legal templates (casualty analysis, statistical context, expert witness report, liability framing), a 7-step maritime legal decision workflow, MAIB/NTSB statistical-context prompt validated against `worldenergydata.MAIBLoader` + `NTSBMarineLoader` (WRK-320), and a 15–20 minute demo script.

The deliverables live in **a TBD client-2 repo**, not digitalmodel — verified by the issue body's `Repo: ['TBD-client2-repo']`. Status `Stage 17: Reclaim (n)` and stages 1–16 done suggest the artifacts may have already shipped under `ai-initiatives-client2/`. Verify before re-doing.

**Stale-flag (mis-filed + possibly landed):** Mis-filed in digitalmodel (target repo TBD). Status indicates close-out gate. The plan below assumes execution in whatever client-2 repo gets provisioned (or `ai-initiatives-client2/` if that is now canonical) and supports both verify-and-close and fresh-write paths.

## Plan

### Task 1 — Verify whether the artifacts have already landed
Run `find . -path '*ai-initiatives-client2/demo/*'` in workspace-hub-scoped trees (and any provisioned client-2 repo). If the 15-artifact set exists with non-stub content, this collapses to verify-and-close (Task 5 only).

### Task 2 — Calculation templates (Day 2)
Author the 8 calc-template markdown files under `demo/`:
- `calc_diffraction_panel_model_setup.md` — AQWA/WAMIT mesh density (≥8 panels per wavelength), symmetry planes, free-surface treatment, waterplane closure, RAO output format.
- `calc_rao_interpretation.md` — peak-period extraction, heading-matrix coverage, cancellation frequencies, heave/pitch coupling.
- `calc_qtf_second_order.md` — difference-frequency loads for mooring low-frequency response, sum-frequency for TLP/spar springing, Newman approximation fallback.
- `calc_plate_ffs_part4_metal_loss.md` — API 579-1 Part 4 RSF_a screening, Level 1 vs Level 2 distinction, MFFL.
- `calc_plate_ffs_part8_distortion.md` — peaking/angular misalignment, out-of-roundness, equivalent stress + buckling interaction.
- `calc_plate_ffs_part9_cracks.md` — FAD K_r vs L_r, BS 7910 Level 2B, Paris-law fatigue projection.
- `calc_goa_seismic_check.md` — API RP 2EQ spectral analysis, OLE/CLE/ALE limit states.
- `calc_goa_extreme_metocean.md` — 100-yr GoA wave/current/wind, API RP 2N ice loading, GoA-vs-GoM contrast.

Each template: prompt block + sample I/O + applicable code citations. Cite issue #2481 contract for any standards-derived constants used in worked examples.

### Task 3 — Multi-step workflows (Day 4)
Author 3 workflow markdowns:
- `workflow_diffraction_to_structural.md` — 4-step chain (panel adequacy → RAO → combined load → utilisation flag).
- `workflow_goa_ffs_decision.md` — 4-step chain (env severity → FFS Level 1 → remaining life → re-inspection interval).
- `workflow_maritime_legal_analysis.md` — 7-step chain (regulatory framework → ISM screen → precedent search via `worldenergydata.MAIBLoader` + `NTSBMarineLoader` → standard-of-care assessment → damages framework → output memo).

### Task 4 — Maritime legal templates + BSEE GoA queries
Author `maritime_casualty_analysis.md`, `maritime_statistical_context.md`, `expert_witness_report_template.md`, `liability_framing_prompt.md`, `goa_bsee_data_queries.md`. Validate the statistical-context prompt by actually running it against `worldenergydata.MAIBLoader` + `NTSBMarineLoader` and capturing one real precedent table in the template's "example output" section.

### Task 5 — Demo script + close
Author `demo/demo_script_client2.md` as a 15–20 minute flow: diffraction overview → plate FFS walkthrough → GoA live data query → maritime casualty analysis. Three "wow" moments: QTF second-order loads, FAD diagram for plate cracks, GoA-vs-GoM seismic comparison. Include FAQ section. Run the legal-scan skill across all new files. Close the issue with the verification artifact list.

## Acceptance Criteria

- [ ] All 8 calc templates present under `demo/` with prompt + sample I/O + code citations.
- [ ] Both technical workflows (diffraction-to-structural, GoA-FFS-decision) documented end-to-end.
- [ ] BSEE GoA data query prompts tested against the actual BSEE data loader (or gap explicitly flagged).
- [ ] 4 maritime legal templates present.
- [ ] 7-step maritime legal workflow documented with sample I/O.
- [ ] MAIB/NTSB statistical-context prompt validated live against `worldenergydata` loaders; one real precedent table embedded.
- [ ] Demo script (15–20 min) walks the full flow and includes a maritime legal moment.
- [ ] Legal scan exits 0; no client-proprietary text in any template.

## Open questions

- **Repo scope:** confirm the canonical client-2 repo path. If still TBD, the deliverables may be staged in `ai-initiatives-client2/` (workspace-hub-adjacent) and migrated later.
- **Live BSEE / MAIB validation:** the data loaders exist (WRK-320 marked done) but the demo prompts may need throttling/auth — verify before Task 4.
- **Standards-citation contract:** `.claude/rules/calc-citation-contract.md` requires Citation emission for standards-derived constants. Calc templates that ship worked examples must comply — flag if any example uses a numeric the contract would reject.
