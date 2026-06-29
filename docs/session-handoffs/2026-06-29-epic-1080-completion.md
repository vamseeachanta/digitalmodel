# Session Handoff — EPIC #1080 completion + FFS/SCF/hull-girder hardening (2026-06-29)

## What shipped (all merged to `main`)

EPIC #1080 (tubular products + ship structural elements + code provenance) is **complete, 14/14 children**, plus an extensive verified hardening and extension pass.

**Tubular products** — material grade matrix (API 5L/IACS/EN, 33 grades, single source of truth) · API 5L line-pipe catalog (NPS×schedule×grade) · API 5CT casing (API 5C3 4-regime collapse, burst, body-yield) · API 11B sucker rod (modified Goodman) · routed 8 scattered grade dicts through the matrix (preserve migration, zero value change + convergence-delta report).

**Ship structural strength** — hull-girder longitudinal strength (IACS UR S11 wave BM + simplified ultimate) and a **full Smith incremental-iterative** ultimate · class-rule scantlings · girder & web-frame buckling · grillage (orthogonally-stiffened) · corrugated bulkhead — all on the validated DNV-RP-C201 plate/panel solvers.

**Fitness-for-service** — published-example validation of B31G/RSTRENG/DNV-RP-F101 (no discrepancy found) · DNV-RP-F101 interacting-defect **area-averaging fix** (length-weighted combined depth per Sec 3.8.2, replacing max-depth) + full Part-A **PSF tables** (Table 3-2 γ_m, γ_d/ε_d) + validity layer · circumferential (API 579 Part 5 net-section) and **2D river-bottom** RSTRENG extensions.

**Fatigue / SCF** — Efthymiou T/Y SCFs corrected to the published equation set (all four terms) + the short-chord **F1** factor (α<12).

**Provenance** — every strength/FFS result carries a `code_reference`; one `codes.py` register + a kept-current CI test; consolidated `docs/domains/codes-register.md`.

**Showcase (live)** — `docs/api/capabilities/index.html`, served at
`https://vamseeachanta.github.io/digitalmodel/capabilities/` — capability cards + a published-validated-vs-derivation-anchored golden-values table.

## How it was built (reusable process)

- **Dependency-ordered layering**: panel buckling → metal-loss FFS → simplified hull-girder → full Smith; grade matrix → line pipe → casing/rod → migration. Each tier reused the validated tier below; new code was mostly orchestration, the hard nonlinear physics already golden-tested.
- **Parallel build agents + a dynamic verification workflow run concurrently, reconciled before merge.** The independent derivation (against the actual standard, computing golden values standalone) caught what passing tests cannot: a pre-existing 7%-high Efthymiou SCF, a grillage degeneracy, a wrong DNV interacting-depth denominator, and out-of-standard γ_m constants — several of them conservative-direction and test-green, i.e. invisible the usual ways.
- **Honesty guardrails**: where a formula could only be found as a scanned image (the DNV-RP-F101 Sec 3.7.4 H1 combined-loading factor), it was shipped as a documented `NotImplementedError` stub rather than fabricated.
- Validation records for every method live under `docs/domains/`.

## Open follow-ups (filed as issues)

- **#1146** — implement the DNV-RP-F101 Sec 3.7.4 combined-loading **H1** factor (needs a non-image source of the equation; worked-example anchor recorded in the validation doc).
- **#1147** — **grade convergence**: adopt the canonical ISO values across consumers (a sanctioned-value change). Worklist = `docs/domains/grade-table-migration-2026-06-28.md`.
- **#1148** — Efthymiou short-chord **F2** (pinned-fixity axial saddle) and **F3** (out-of-plane-bending saddle).

## Notes for the next session

- One judgment call to revisit: `dnv_f101_psf` defaults to `safety_class="very high"` to preserve the legacy γ_m=0.77 — the most conservative class. Callers should set their pipeline's actual safety class; flip the default to "medium" if a conventional default is preferred (one-line + a test update).
- `efthymiou_ty_axial` defaults to a short chord (α=2L/D=5), so default saddle SCFs are F1-reduced ~23%; long-chord users pass α≥12 (or larger L).
- Method APIs live under `src/digitalmodel/{materials, well/tubulars, naval_architecture, structural, asset_integrity, fatigue}/`.
