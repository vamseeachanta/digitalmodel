# Buckling & Panel Buckling — Code Review and Parametric Study

**Date:** 2026-06-26
**Scope:** Review of buckling / panel-buckling analysis in `digitalmodel`, test & config coverage,
and a parametric utility-curve study on standard ship-hull plate.

---

## 1. What exists today

There are **three overlapping plate-buckling implementations** plus pipeline/rod/FE buckling. Knowing
which is which avoids future duplication.

| # | Module | Main class(es) | Standard | State | Tests |
|---|--------|----------------|----------|-------|-------|
| 1 | `structural/structural_analysis/buckling.py` | `PlateBucklingAnalyzer`, `ColumnBucklingAnalyzer` | DNV-RP-C201 + Eurocode 3 + Johnson-Ostenfeld | **Cleanest.** Tidy dataclass I/O (`BucklingResult.utilization`), predefined steels. | `tests/structural/structural_analysis/test_buckling_capacity.py` (28) |
| 2 | `infrastructure/base_solvers/structural/plate_buckling.py` | `PlateBuckling` | DNV-RP-C201 | Engine-wired (`basename: plate_buckling`). Verbose dict-passing, ft conversions. | indirect |
| 3 | `infrastructure/calculations/plate_buckling.py` | `ElasticBucklingCalculator`, `SlendernessCalculator`, `UltimateStrengthCalculator` | DNV-RP-C201, API | Unit-aware (pint / `TrackedQuantity`). | `tests/test_plate_buckling_units.py`, `tests/test_plate_capacity.py` |

Other buckling families (not plate): `structural/fe/elastic_buckling_workflow.py` (eigenvalue/FE),
`subsea/pipeline/{lateral,upheaval,thermal}_buckling.py`, `marine_ops/.../dynacard/rod_buckling.py`.

### Recommendation
For new structural work, build on **#1** (`structural_analysis`). It has the cleanest object model,
returns a `utilization` directly, and already defines material grades.

---

## 2. Gaps found

- **Stiffened-panel ("panel") buckling has no working module.** It existed only as incomplete
  2018 spreadsheet-derived scripts under `structural/plate_capacity/StiffnerBuckling_Cal/`, where the
  actual buckling-resistance checks are **commented out**. No tests, no config.
- **`plate_buckling.yml` had an empty `groups: []`** — no runnable worked example.
- **No parametric config** wiring plate buckling into the ecosystem `parametric_run` sweep engine.

---

## 3. What this work added

1. **Marine hull-steel grades** in `structural_analysis/models.py` — `STEEL_GRADE_A` (235),
   `STEEL_AH36` (355), `STEEL_EH40` (390), with classification-society naming and `MARINE_GRADES`
   lookup. These are the "standard sheet materials used for ships". E = 206 GPa (class-society value).
2. **`StiffenedPanelBucklingAnalyzer`** (`structural_analysis/panel_buckling.py`) + tests — a clean,
   **PRELIMINARY** stiffened-panel solver (plate-induced + column modes). Flagged `PRELIMINARY` and
   **not yet validated** against a DNV worked example; kept out of the management headline chart.
3. **Parametric sweep + lookup** (`structural/buckling_parametric.py`) — full-factorial sweep over
   thickness × width × load × grade following the `parametric_run` convention, emitting `cases.csv`
   and an **indexed `results.json`** for O(1) point lookups (downstream Deckhand API workflows).
4. **Configs** — populated `plate_buckling.yml` with an AH36 worked example, and a
   `parametric_run`-style `ship_plate_buckling_parametric.yml` sweep config.
5. **Management report** — interactive Plotly HTML (`examples/demos/structural/`) with buckling
   utilisation-vs-thickness utility curves per grade, critical-stress curves, slenderness curves, and
   a combined-load interaction view, built with the ecosystem `GTMReportBuilder`.

---

## 4. Engineering basis (plate buckling, module #1)

- Elastic critical stress: σ_e = k · π²E / [12(1−ν²)] · (t/b)², with k=4 (long simply-supported
  plate, uniform compression) and shear coefficient k_τ = 5.34 + 4(b/a)².
- **Johnson-Ostenfeld** inelastic correction above 0.5·f_y: σ_cr = f_y·(1 − f_y/(4σ_e)).
- Combined check: util = σ_x/(σ_cr/γ_m) + [τ/(τ_cr/γ_m)]², γ_m = 1.15 (DNV usage-factor convention).
- Plate slenderness for curves: β = (b/t)·√(f_y/E).

---

## 5. Standard ship-plate materials used

| Grade | Min yield (MPa) | Min tensile (MPa) | E (MPa) | Notes |
|-------|-----------------|-------------------|---------|-------|
| Grade A | 235 | 400 | 206 000 | Normal-strength hull steel |
| AH36 | 355 | 490 | 206 000 | Higher-strength hull steel (most common) |
| EH40 | 390 | 510 | 206 000 | Higher-strength, −40 °C notch toughness |

The leading letter (A/D/E/F) denotes Charpy test temperature (toughness), not strength, so it does
not change the buckling properties used here.
