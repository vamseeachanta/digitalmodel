# Plan for digitalmodel#616: polar plot with vessel silhouette and on-body force vectors

> **Status:** plan-review (Claude r1 MAJOR closed → r2 MINOR; Codex + Gemini pending)
> **Complexity:** T3 (new module + dependent refactor + downstream consumer hook + TDD across 4+ test surfaces)
> **Date:** 2026-05-20
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/616
> **Review artifacts (workspace-hub-tracked):** workspace-hub `scripts/review/results/2026-05-20-plan-draft-digitalmodel-ocimf-polar-vessel-force-overlay-claude-r{1,2}.md`
> **Authorization scope:** digitalmodel-side writes ONLY after user approves this plan + applies `status:plan-approved` + creates `.planning/plan-approved/616.md`. No code may land before all three signals exist.

---

## Resource Intelligence Summary

### Existing repo code (verified 2026-05-20T19:50:00Z)

- **EXISTS** `/mnt/local-analysis/digitalmodel/scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` (770 LOC) — current polar overlay function at lines 396-457: `make_polar_overlay(df, figure_key, coef, group_col)` uses `Scatterpolar` with `r = |C|`, `direction="clockwise"`, `rotation=90`, ticks at bow/stbd/stern/port. No silhouette, no force-arrows.
- **EXISTS** `/mnt/local-analysis/digitalmodel/src/digitalmodel/marine_ops/marine_analysis/reporting/ocimf_interactive_report.py` (660 LOC) — `OCIMFInteractiveReport` class with its own polar diagrams, 3D surfaces, heatmaps, force-comparison charts. Header comment claims `HTML_REPORTING_STANDARDS.md ✅`. **Potentially overlapping** with build_coefficient_explorer.py but predates it — decision needed (extend / deprecate / leave alone).
- **EXISTS** `/mnt/local-analysis/digitalmodel/src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` (826 LOC) — `OCIMFDatabase` class; the source-of-truth for coefficient lookups.
- **EXISTS** `/mnt/local-analysis/digitalmodel/src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` (825 LOC) — duplicate twin (umbrella'd at workspace-hub#2768).
- **EXISTS** `/mnt/local-analysis/digitalmodel/src/digitalmodel/visualization/` — top-level viz tree (has `agent_dashboard.py`, `reporting/`, `design_tools/`, `orcaflex_dashboard/`).
- **EXISTS** `/mnt/local-analysis/digitalmodel/src/digitalmodel/marine_ops/marine_analysis/visualization/` — marine-specific viz tree (subdirectory of the duplicate-twin path).
- **EXISTS** `/mnt/local-analysis/digitalmodel/src/digitalmodel/hydrodynamics/diffraction/polars_exporter.py` — unrelated; for orcawave RAO polars.

**Existing vessel/hull dataclasses and models (added in r1 revision per M2):**

- **EXISTS** `/mnt/local-analysis/digitalmodel/src/digitalmodel/hydrodynamics/hull_library/profile_schema.py` — `HullType` (Enum), `HullStation` (Pydantic BaseModel), `HullProfile` (Pydantic BaseModel: "Complete hull profile definition"). Primary candidate for reuse instead of introducing a new dataclass.
- **EXISTS** `/mnt/local-analysis/digitalmodel/src/digitalmodel/hydrodynamics/models.py` — header comment: "Data models for hydrodynamic analysis including coefficient matrices, vessel properties, wave parameters, and environmental conditions." Uses both `@dataclass` and `Enum` patterns. Likely defines vessel-property dataclasses adjacent to this module's needs.
- **EXISTS** `/mnt/local-analysis/digitalmodel/src/digitalmodel/orcawave/vessel_database.py` — vessel database; likely has its own Vessel/Hull type adjacent to OrcaWave workflows.
- **EXISTS** `/mnt/local-analysis/digitalmodel/src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` — diffraction input schemas; likely includes vessel-geometry input shapes for OrcaWave/diffraction analyses.
- **EXISTS** `/mnt/local-analysis/digitalmodel/src/digitalmodel/hydrodynamics/hull_library/{catalog,lookup,mesh_generator}.py` — hull-library helpers (catalog browse, lookup, mesh generation) that compose with `HullProfile`.

**Decision (r1 M2 closure):** This plan adopts **Option A — reuse `HullProfile`** as the foundation, and introduces a thin sibling `VesselSilhouetteSpec` dataclass that *references* a `HullProfile` and adds only the silhouette-rendering-specific fields (`silhouette_kind`, optional `custom_path`). Rationale: `HullProfile` already carries LOA, beam, draft, and station data; reinventing them in a competing `VesselGeometry` repeats the duplicate-file anti-pattern workspace-hub#2768 is umbrella'd to fix. The two-class split (reuse `HullProfile` + new `VesselSilhouetteSpec`) keeps geometry separate from rendering-time concerns. Implementation must verify `HullProfile`'s exact field set during plan-review and amend this decision if it lacks a required field — see §Open Questions.

- **MISSING** (this plan creates): `src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py`
- **MISSING** (this plan creates): `src/digitalmodel/marine_ops/marine_engineering/visualization/__init__.py` (new directory)
- **MISSING** (this plan creates): `src/digitalmodel/marine_ops/marine_engineering/visualization/vessel_silhouettes.py` (silhouette polygon registry; consumes `HullProfile` via composition)
- **MISSING** (this plan creates): `src/digitalmodel/marine_ops/marine_engineering/visualization/types.py` (`VesselSilhouetteSpec` dataclass; references `HullProfile`)

### Standards

| Standard | Status | Source |
|---|---|---|
| OCIMF MEG3/MEG4 sign convention | applies to default frame enum `INCIDENCE_HEADING_BODY_FIXED` | `digitalmodel/docs/data/OCIMF_CORPUS_README.md` (committed `9796effa`); `.claude/rules/calc-citation-contract.md` |
| Coordinate-frame and sign-convention rigor | required by `engineering-parametric-chart-plan-review` reference under issue-planning-mode | `.claude/skills/coordination/issue-planning-mode/references/engineering-parametric-chart-plan-review.md` |
| Digital naval-architecture body-frame convention (+X=bow, +Y=starboard, +Z=down) | applies to vessel_geometry dataclass | digitalmodel convention (verify in OrcaFlex/OrcaWave wrappers) |

### Documents consulted

- workspace-hub#2768 epic plan: `docs/plans/2026-05-20-issue-2768-epic-ocimf-meg3-meg4-closeout.md` — defines the larger OCIMF closeout context.
- workspace-hub#2760 (status:plan-review): labels include `domain:visualization` + `domain:naval-architecture`; force-by-force review of X/Y/Z/K/M/N at rudder ±5°; acceptance criteria require "Updated Markdown/HTML/PDF deliverables are regenerated". Direct consumer of this module.
- `.claude/skills/coordination/issue-planning-mode/references/engineering-parametric-chart-plan-review.md` — checklist for parametric chart plans: freeze local vs reported coordinate frames before formulas; require tests for frame transforms/sign conventions.
- `.claude/skills/coordination/issue-planning-mode/references/layered-architecture-issue-planning.md` — applies because this plan declares a canonical home that competes with two pre-existing duplicate paths.
- Existing `digitalmodel/docs/plans/2026-05-05-issue-{556,557,561,564}-*.md` — pre-existing OCIMF child plans; this plan must declare it does NOT conflict (different module).

### Gaps identified

1. No reusable polar-with-vessel-and-force-vector function exists in digitalmodel.
2. No vessel-silhouette polygon registry exists.
3. No coordinate-frame enum exists that distinguishes "incidence heading body-fixed" from "force direction inertial".
4. No tests pin the force-arrow-direction correctness claim against an externally citable convention (OCIMF MEG3/MEG4 Annex A). The build script's own internal docstring is **not** an independent source — it must be cross-checked against the standard before any direction value is pinned in a test.
5. workspace-hub#2768 has not yet resolved `marine_engineering` vs `marine_analysis` duplication — this plan declares `marine_engineering` as canonical home but cannot consolidate the twin file (out of scope; #2768 territory).
6. No documented decision exists in digitalmodel for whether new vessel-rendering modules should reuse `HullProfile` or introduce module-local geometry types. This plan declares the reuse decision (see Existing repo code, Decision).
7. No documented citation-emission policy exists for the case of a visualization module that *renders* (rather than computes) standards-sourced numeric data. This plan addresses it under §Risks/Open (m5).

### Evidence (embedded verification)

**Issue statuses** (verified 2026-05-20T19:50:00Z via `gh issue view`):

- `workspace-hub#2760` — OPEN — `status:plan-review` — "revise(naval-arch): B1528 SIROCCO force calculation review updates" — confirms downstream-consumer status.
- `workspace-hub#2768` — OPEN — `status:plan-review` — OCIMF closeout umbrella; non-blocking for this plan but constrains the canonical-home decision.
- `digitalmodel#556`, `#557`, `#561`, `#563`, `#564` — all OPEN — independent OCIMF defects; no overlap with the new visualization module.

**File existence** (`ls -la` 2026-05-20T19:50:00Z):

- EXISTS: `digitalmodel/scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` (770 LOC)
- EXISTS: `digitalmodel/src/digitalmodel/marine_ops/marine_analysis/reporting/ocimf_interactive_report.py` (660 LOC)
- EXISTS: `digitalmodel/src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` (826 LOC)
- EXISTS: `digitalmodel/src/digitalmodel/visualization/__init__.py`
- EXISTS: `digitalmodel/tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py`
- EXISTS: `digitalmodel/tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py`
- MISSING (this plan creates): all paths listed in §Files to Change > Create rows.

**Line excerpts** (`sed -n 396,406p` on `build_coefficient_explorer.py`):

```python
def make_polar_overlay(df, figure_key, coef, group_col):
    """Polar plot preserving sign of the coefficient.

    Convention: radial axis = |C| (magnitude), angular axis = wind/current
    incidence heading (0deg=bow, 90deg=starboard beam, 180deg=stern). Sign of
    the coefficient is encoded by line style and marker shape:
      - Solid line + circle marker  = C >= 0 (force in +Y vessel-fixed dir)
      - Dashed line + x marker      = C <  0 (force in -Y vessel-fixed dir)
```

**Reproduction proofs** (Step 1.5): **N/A — visualization-feature plan, not a runtime-bug plan.** No alleged failure to reproduce. However, the §TDD list below includes a "before-snapshot" assertion: the existing rendered HTML at `digitalmodel/docs/domains/charts/phase2/ocimf/ocimf_coefficient_explorer.html` is captured before any change so reviewers can compare against the after-snapshot.

Distinct sources consulted: 7 (workspace-hub#2760 + workspace-hub#2768 + 4 file paths + 1 skill reference). Minimum 3 required — exceeded.

---

## Artifact Map

| Artifact | Path |
|---|---|
| This plan (final location) | `digitalmodel/docs/plans/2026-05-20-issue-616-ocimf-polar-vessel-force-overlay.md` |
| This plan (draft location) | `workspace-hub/docs/governance/2026-05-20-digitalmodel-plan-draft-ocimf-polar-vessel-force-overlay.md` |
| New module (canonical home) | `digitalmodel/src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py` |
| New module __init__ | `digitalmodel/src/digitalmodel/marine_ops/marine_engineering/visualization/__init__.py` |
| Silhouette polygon registry | `digitalmodel/src/digitalmodel/marine_ops/marine_engineering/visualization/vessel_silhouettes.py` |
| New tests | `digitalmodel/tests/marine_ops/marine_engineering/visualization/test_polar_force_overlay.py` |
| New tests (silhouettes) | `digitalmodel/tests/marine_ops/marine_engineering/visualization/test_vessel_silhouettes.py` |
| New smoke test (cross-consumer) | `digitalmodel/tests/marine_ops/marine_engineering/visualization/test_polar_force_overlay_smoke_sirocco.py` |
| Refactored consumer #1 | `digitalmodel/scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` (delegate `make_polar_overlay` to new module) |
| Regenerated HTML output | `digitalmodel/docs/domains/charts/phase2/ocimf/ocimf_coefficient_explorer.html` |
| Plan review — Claude | `digitalmodel/scripts/review/results/2026-05-20-plan-616-claude.md` |
| Plan review — Codex | `digitalmodel/scripts/review/results/2026-05-20-plan-616-codex.md` |
| Plan review — Gemini | `digitalmodel/scripts/review/results/2026-05-20-plan-616-gemini.md` |

---

## Deliverable

A reusable `polar_force_overlay()` function in `digitalmodel.marine_ops.marine_engineering.visualization` that produces a Plotly polar Figure with (a) a transparent vessel silhouette centered at the radial origin, bow up, and (b) on-body force-vector arrows at sampled headings whose direction unambiguously shows the physical force action on the vessel — consumable by the OCIMF coefficient explorer (refactored to call it) and by future studies including the workspace-hub#2760 SIROCCO force review.

---

## Pseudocode

```python
# src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py

from dataclasses import dataclass
from enum import Enum
import plotly.graph_objects as go
import pandas as pd
import numpy as np

class FrameConvention(Enum):
    INCIDENCE_HEADING_BODY_FIXED = "incidence_heading_body_fixed"  # OCIMF default
    FORCE_DIRECTION_INERTIAL = "force_direction_inertial"

class ForceArrowKind(Enum):
    LATERAL_ONLY = "lateral_only"      # Y-component only
    LONGITUDINAL_ONLY = "longitudinal_only"  # X-component only
    RESULTANT_2D = "resultant_2d"      # vector sum in XY
    NONE = "none"

class RadialAxisMode(Enum):
    MAGNITUDE = "magnitude"  # r = |C|, sign by line style (today's behavior)
    SIGNED = "signed"        # r = C directly, with color split for sign

# PER r1 M2 DECISION: reuse digitalmodel.hydrodynamics.hull_library.HullProfile as the
# vessel-geometry source-of-truth. Introduce only a thin silhouette-rendering wrapper.
from digitalmodel.hydrodynamics.hull_library.profile_schema import HullProfile

@dataclass(frozen=True)
class VesselSilhouetteSpec:
    """Rendering-specific silhouette spec that composes a HullProfile (geometry)
    with silhouette-style choices. Does NOT duplicate LOA/beam/draft fields."""
    hull_profile: HullProfile             # reused — carries LOA, beam, draft, stations
    silhouette_kind: str = "tanker"       # tanker | gas_carrier | generic | custom_path
    custom_path: list[tuple[float,float]] | None = None  # if silhouette_kind=='custom_path', polygon in body coords
    opacity: float = 0.20                 # transparent overlay; default per acceptance criterion

def polar_force_overlay(
    data: pd.DataFrame,                   # cols: (theta_deg, fx, fy, fz, mx, my, mz) OR (theta_deg, value, component)
    silhouette: VesselSilhouetteSpec,
    frame_convention: FrameConvention = FrameConvention.INCIDENCE_HEADING_BODY_FIXED,
    force_arrow_kind: ForceArrowKind = ForceArrowKind.LATERAL_ONLY,
    radial_axis_mode: RadialAxisMode = RadialAxisMode.MAGNITUDE,
    title: str = "",
    group_col: str | None = None,         # if set, one trace per group; consumer pattern from make_polar_overlay
    arrow_sample_step_deg: float = 30.0,  # arrow density on the polar — every N degrees
) -> go.Figure:
    """Produce a polar Plotly Figure with vessel silhouette + force-arrow overlay."""
    1. Validate input data: required schema; theta_deg in [0,360); finite values.
    2. Validate vessel_geometry: positive loa_m and beam_m; if silhouette_kind=='custom_path', verify path is non-empty.
    3. Compute silhouette polygon points in polar coordinates (radial scale = max-data-r * 0.25 by default).
    4. Add silhouette trace as a closed Scatterpolar with fill='toself', opacity=0.20, color='#888', hoverinfo='skip'.
    5. For each group (or single trace if group_col is None):
        a. Sort by theta_deg.
        b. Build the data trace as today's make_polar_overlay does (positive half + negative half if applicable).
        c. If force_arrow_kind != NONE: sample at every arrow_sample_step_deg and add arrow annotations.
           For LATERAL_ONLY: arrow origin = (silhouette_edge_radius, theta_arrow_start),
                              arrow tip = origin + (|Cy| * sign(Cy)) in the +Y vessel-fixed direction,
                              where Y direction in polar coords is theta=90° (or 270° if -Y).
           For RESULTANT_2D: compute fx, fy components and add a 2D arrow.
           Arrow color: positive=#1f77b4 / negative=#d62728 (configurable).
    6. Apply layout (angularaxis ticks identical to today's, radialaxis title, transparent paper).
    7. Return fig.

def _resolve_arrow_direction_in_body_frame(theta_incidence_deg, component_sign, frame_convention):
    """Pure function. CRITICAL TDD TARGET — frame transform correctness.

    IMPLEMENTATION GATE (r1 M1): the numeric mapping below is provisional. Before
    pinning the specific direction values in tests, the implementing agent must
    cite OCIMF MEG3 (2008) Annex A and/or MEG4 (2018) Annex A section/page that
    defines whether a positive Cy at starboard-incidence (θ=90°) means:
      (a) force vector in vessel-fixed +Y axis (the build_coefficient_explorer.py
          docstring at lines 399-405 asserts this), OR
      (b) drag coefficient magnitude with sign-handling defined caller-side, OR
      (c) something else specific to the standard's frame definition.
    The plan must NOT be approved while the convention citation is missing.
    """
    if frame_convention == FrameConvention.INCIDENCE_HEADING_BODY_FIXED:
        # Provisional mapping (verify against OCIMF MEG3/MEG4 Annex A before pinning):
        # positive Cy -> +Y body-fixed (theta_body=90), negative Cy -> -Y (theta_body=270)
        return 90.0 if component_sign >= 0 else 270.0
    elif frame_convention == FrameConvention.FORCE_DIRECTION_INERTIAL:
        # input theta is already the force-direction angle
        return theta_incidence_deg
    raise ValueError(f"unsupported frame: {frame_convention}")
```

---

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `src/digitalmodel/marine_ops/marine_engineering/visualization/__init__.py` | New directory; re-exports `polar_force_overlay`, `VesselSilhouetteSpec`, `FrameConvention`, `ForceArrowKind` |
| Create | `src/digitalmodel/marine_ops/marine_engineering/visualization/types.py` | **r1 M2**: `VesselSilhouetteSpec` dataclass + enums (`FrameConvention`, `ForceArrowKind`, `RadialAxisMode`); references `HullProfile` from `hydrodynamics.hull_library.profile_schema` |
| Create | `src/digitalmodel/marine_ops/marine_engineering/visualization/_convention.py` | **r2 n1**: `OCIMF_CONVENTION_AUTHORITY` singleton constant. Module-private (underscore prefix) — re-exported only via `__init__.py` if downstream consumers need access. Required public surface: `positive_cy_arrow_at_starboard_incidence_deg() -> float`, `negative_cy_arrow_at_starboard_incidence_deg() -> float`, `citation_text() -> str` (returns OCIMF MEG3 §X.Y / MEG4 §A.B page reference string). Authority values are pinned at implementation time AFTER the implementing agent has read the cited Annex A sections directly — NOT derived from `build_coefficient_explorer.py`'s internal docstring. |
| Create | `src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py` | Main module per §Pseudocode |
| Create | `src/digitalmodel/marine_ops/marine_engineering/visualization/vessel_silhouettes.py` | Silhouette polygon registry (tanker / gas_carrier / generic); consumes `HullProfile` via composition |
| Create | `tests/marine_ops/marine_engineering/visualization/__init__.py` | Test package init |
| Create | `tests/marine_ops/marine_engineering/visualization/test_polar_force_overlay.py` | TDD per §TDD Test List rows 1-8, 12, 16, 17 |
| Create | `tests/marine_ops/marine_engineering/visualization/test_vessel_silhouettes.py` | TDD per §TDD Test List rows 9-11 |
| Create | `tests/marine_ops/marine_engineering/visualization/test_polar_force_overlay_smoke_sirocco.py` | Cross-consumer smoke test #14 (strengthened per r1 m2) |
| Create | `tests/marine_ops/marine_engineering/visualization/test_no_regression_traces.py` | TDD #13; reads from the fixture below |
| Create | `tests/marine_ops/marine_engineering/visualization/fixtures/ocimf_explorer_pre_refactor_trace_signature.json` | **r1 m1**: per-figure `(trace_count, sum_theta_len, sum_r_len)` triples captured from `build_coefficient_explorer.py` at the pre-refactor commit SHA. Capture script: run `python -c "from build_coefficient_explorer import build_all; dump_trace_signature(build_all(), '<path>')"` at the SHA recorded below. |
| Create | `tests/marine_ops/marine_engineering/visualization/fixtures/ocimf_explorer_baseline.html` | **r1 m4**: byte-frozen copy of the pre-refactor `ocimf_coefficient_explorer.html` (from `digitalmodel:9796effa` or later HEAD before this plan's branch) so reviewers can do a visual diff against the post-refactor output. |
| Modify | `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` | Refactor `make_polar_overlay()` (lines 396-457) to delegate to new module; preserve all 15 figure traces |
| Regenerate | `docs/domains/charts/phase2/ocimf/ocimf_coefficient_explorer.html` | Output of refactored build script |

**Capture sequencing (added r1 m1, m4; tightened r2 n2):** the two fixture-capture rows above MUST land in a commit BEFORE any refactor of `build_coefficient_explorer.py` lands. Sequence: (1) capture-commit lands the trace-signature JSON + the byte-frozen baseline HTML; (2) refactor commit modifies `make_polar_overlay`; (3) test commit lands the regression test that reads the fixture. The implementing agent records the pre-refactor commit SHA in the fixture JSON under a `source_commit_sha` field.

**Enforceable verification of capture sequencing (r2 n2):** the regression test #13 itself asserts the ordering by running:

```python
import subprocess, json, pathlib
fixture = json.loads(pathlib.Path('tests/.../fixtures/ocimf_explorer_pre_refactor_trace_signature.json').read_text())
captured_sha = fixture['source_commit_sha']
refactor_commits = subprocess.check_output(
    ['git', 'log', '--format=%H', '--', 'scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py'],
    text=True
).strip().splitlines()
# captured_sha must appear in the history AND be older than the most-recent refactor-target commit
assert captured_sha in refactor_commits, f"captured SHA {captured_sha} not in build_coefficient_explorer.py history"
captured_idx = refactor_commits.index(captured_sha)
# in git log, index 0 is newest; captured must be at a higher index (older) than any refactor commit
# implementing agent encodes the refactor commit subject pattern (e.g., "refactor(ocimf):") and asserts position
```

This converts the prose ordering instruction into a test-time assertion the implementing agent cannot accidentally bypass.

**Explicitly not modified:**

| Would-be action | Path | Why blocked |
|---|---|---|
| Modify | `src/digitalmodel/marine_ops/marine_analysis/reporting/ocimf_interactive_report.py` | Pre-existing 660-LOC report; refactoring it is a separate decision tracked under workspace-hub#2768. This plan leaves it untouched and flags "extend vs deprecate" as an open question. |
| Modify | `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | OCIMFDatabase changes belong to digitalmodel#556/#563 child plans, not this one. |
| Delete | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` | Twin-file consolidation is workspace-hub#2768 scope. |

---

## TDD Test List

| # | Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|---|
| 1 | `test_input_schema_long_format` | accepts `(theta_deg, value, component)` long-format DataFrame | df with rows (0, 0.85, 'X'), (90, 1.0, 'Y'), … | returns `go.Figure` without raising |
| 2 | `test_input_schema_wide_format` | accepts `(theta_deg, fx, fy, fz, mx, my, mz)` wide-format | wide-format df | returns `go.Figure` |
| 3 | `test_input_schema_invalid_raises` | rejects missing required columns | df without `theta_deg` | raises `ValueError("theta_deg required")` |
| 4 | `test_input_theta_range_clamp` | rejects theta outside [0, 360) | theta=370° | raises `ValueError("theta_deg must be in [0,360)")` |
| 5 | `test_arrow_direction_ocimf_positive_cy_property` | **CRITICAL frame test (r1 M1 revised)**: under OCIMF `INCIDENCE_HEADING_BODY_FIXED` convention, positive Cy at θ_incidence=90° produces an arrow whose terminal direction is consistent with the convention cited in the implementation (citation must be present in code + docstring). Property assertion: the returned direction is *bound to one of {90.0, 270.0}* and *agrees with the convention citation*; the specific value is pinned ONLY after the implementing agent has cited OCIMF MEG3/MEG4 Annex A section/page directly. Test scaffold: `expected_direction = OCIMF_CONVENTION_AUTHORITY.positive_cy_arrow_at_starboard_incidence()` where the authority constant is itself bound by citation. | parameterized: `_resolve_arrow_direction_in_body_frame(90, +1, INCIDENCE_HEADING_BODY_FIXED)` with the convention authority bound by the standards citation | returns the value that matches the cited OCIMF Annex A convention (not pre-committed in this plan) |
| 6 | `test_arrow_direction_ocimf_negative_cy_property` | mirror of #5 for negative Cy. Asserts that flipping the sign flips the direction by 180° (mod 360°). Value-pinning gated on the same standards citation. | parameterized as #5 with `component_sign=-1` | returns the 180°-rotated direction relative to #5 |
| 7 | `test_frame_inertial_passthrough` | INERTIAL frame returns input theta unchanged | call with frame=FORCE_DIRECTION_INERTIAL, theta=135 | returns 135.0 |
| 8 | `test_figure_contains_silhouette_trace` | rendered fig has a `Scatterpolar` trace with `fill='toself'` and `opacity ≤ 0.30` | call `polar_force_overlay(...)` | `fig.data[0].fill == 'toself'` and opacity in (0, 0.31) |
| 9 | `test_silhouette_tanker_polygon_bow_up` | tanker silhouette has its forward-most point at angle 0° (bow up convention) | `vessel_silhouettes.get_polygon('tanker', loa=300, beam=50)` | max-x point (forward) is at vessel-fixed +X i.e. polar theta=0° |
| 10 | `test_silhouette_scales_with_loa` | doubling LOA doubles silhouette extent | call with loa=300 vs loa=600 | radial-extent ratio ≈ 2.0 ± 0.01 |
| 11 | `test_silhouette_custom_path_accepted` | custom_path option renders the provided polygon | path=[(0,0),(1,0),(1,1)] | fig contains a trace with those vertices |
| 12 | `test_arrow_density_respects_sample_step` | `arrow_sample_step_deg=30` produces 12 arrows over 0-360 (or fewer if data sparser) | step=30, dense data | arrow-annotation count ≤ 12 |
| 13 | `test_no_regression_existing_data_traces_preserved` | refactored `make_polar_overlay` produces the same data-trace signature as the pre-refactor version. **r1 m1 fix**: compares against a fixture captured BEFORE the refactor lands. Fixture is `tests/marine_ops/marine_engineering/visualization/fixtures/ocimf_explorer_pre_refactor_trace_signature.json` containing per-figure `(trace_count, sum_of_theta_lengths, sum_of_r_lengths)` triples from the live `build_coefficient_explorer.py` at the pre-refactor commit SHA (capture step listed in §Files-to-Change). | refactored function called on real OCIMF figure data | per-figure trace-signature dict equals fixture dict (bitwise for counts, exact for shape tuples) |
| 14 | `test_smoke_sirocco_consumer_readiness` | **r1 m2 strengthened**: synthetic SIROCCO-style force DataFrame (X/Y/Z/K/M/N at rudder ±5°) produces a fully-readable figure: (a) returns `go.Figure`; (b) figure has exactly 6 distinct legend entries (one per component); (c) for at least 3 of the 6 components, the resolved arrow directions are all *distinct* (no collapsed/coincident arrows); (d) no exceptions, no warnings; (e) figure layout has non-empty title and angularaxis ticks. If an anonymized SIROCCO fixture becomes available, swap the synthetic shape for it. | wide-format df with `(theta_deg, fx, fy, fz, mx, my, mz)` at 11 rudder-angle headings | all five sub-conditions above pass |
| 15 | `test_no_client_identifiers_in_module_source` | **r1 m3 strengthened**: invokes the workspace-hub legal-sanity-scan script (or, if cross-repo invocation is blocked, reads `.legal-deny-list.yaml` patterns programmatically and applies them via grep) over the new module source + tests. Hardcoding three patterns is rejected per the legal-baseline policy in `.claude/rules/`. | run `scripts/legal/legal-sanity-scan.sh src/digitalmodel/marine_ops/marine_engineering/visualization/` (or the equivalent pattern-driven grep) | returns clean / exit 0 |
| 16 | `test_arrow_direction_180_degree_invariant_under_sign_flip` | **r1 M1 cross-check**: independent property assertion that flipping the sign of any Cy component flips the arrow direction by exactly 180° (mod 360°). Catches errors where the sign-handling logic uses an asymmetric mapping. | call `_resolve_arrow_direction_in_body_frame(θ, +1, ...)` and `_resolve_arrow_direction_in_body_frame(θ, -1, ...)` for θ ∈ {0, 45, 90, 135, 180} | `abs((dir_plus - dir_minus) mod 360) == 180` for every θ |
| 17 | `test_silhouette_consumes_hull_profile_not_competing_dataclass` | **r1 M2 + r2 n3 enforcement (widened)**: the visualization module declares ONLY `VesselSilhouetteSpec` as a dataclass and no other. Catches both naive-rename attempts (`length_overall_m` instead of `loa_m`) and semantic overlap via a stricter discriminator: count `@dataclass`-decorated classes in `polar_force_overlay.py`, `types.py`, `vessel_silhouettes.py`, and `_convention.py`. The expected count is **exactly 1** (`VesselSilhouetteSpec` in `types.py`). | walk AST of all module files; count `@dataclass` decorations | dataclass count == 1; the sole dataclass is `VesselSilhouetteSpec` in `types.py` |

---

## Acceptance Criteria

- [ ] New module + tests + refactored build script land via one PR (or one coherent commit series) referencing the digitalmodel issue, **after** the capture-commit (per §Files-to-Change capture-sequencing) has landed the pre-refactor fixtures.
- [ ] All tests pass: `cd digitalmodel && uv run pytest tests/marine_ops/marine_engineering/visualization/ -v` (17 cases listed above).
- [ ] No regression: `cd digitalmodel && uv run pytest tests/marine_ops/ -v` passes.
- [ ] Regenerated `ocimf_coefficient_explorer.html` opens in a browser, shows ship silhouette at center of every polar diagram, and shows on-body force arrows at sampled headings.
- [ ] Visual diff confirmed between pre-refactor and post-refactor HTML using the byte-frozen baseline at `tests/.../fixtures/ocimf_explorer_baseline.html` (r1 m4) — all 15 figure traces preserved.
- [ ] Module is importable from a synthetic workspace-hub#2760-side caller AND satisfies all five sub-conditions of smoke test #14 (r1 m2): returns Figure, has 6 distinct legend entries, ≥3 components produce distinct arrow directions, no warnings, layout non-empty.
- [ ] Legal-sanity scan passes via the workspace-hub script or its pattern-driven equivalent (per r1 m3) on the new module + tests.
- [ ] **r1 M1 enforcement:** OCIMF MEG3/MEG4 Annex A convention citation is present in `polar_force_overlay.py` docstring and in the `OCIMF_CONVENTION_AUTHORITY` constant; tests #5/#6 derive their expected value from that authority, not from inline literals.
- [ ] **r1 M2 enforcement:** the module imports `HullProfile` from `hydrodynamics.hull_library.profile_schema` and the static-analysis test #17 passes (no competing geometry dataclass).
- [ ] **r1 m6 + r2 n4 enforcement:** Plotly arrow technique pre-spike artifact is committed under `digitalmodel/docs/spikes/2026-05-20-plotly-polar-arrow-technique/` with at minimum a prototype script, rendered HTML, and one-paragraph README. The digitalmodel issue comment links the committed path with the spike-commit SHA.
- [ ] **r1 m5 enforcement:** the rendered Figure's `layout.meta` includes the three provenance fields specified under §Risks Open, OR §Risks Open is updated with an alternate reviewer-approved decision.
- [ ] Adversarial review wave complete (Claude r1 + r2 inline + Codex + Gemini external) and the latest verdict from each provider is MINOR or APPROVE before this plan is surfaced for user approval. Any MAJOR triggers re-revision.
- [ ] User has applied `status:plan-approved` label AND created `digitalmodel/.planning/plan-approved/616.md` marker (or workspace-hub equivalent if digitalmodel doesn't track markers locally).

---

## Adversarial Review Summary

| Provider | Verdict | Key findings |
|---|---|---|
| Claude r1 | **MAJOR** | M1 (TDD #5/#6 pre-commits direction without OCIMF citation); M2 (RIS missed `hydrodynamics/hull_library/HullProfile` + 3 other vessel types); 6 MINOR. Artifact: `scripts/review/results/2026-05-20-plan-draft-digitalmodel-ocimf-polar-vessel-force-overlay-claude-r1.md`. |
| Claude r2 | **MINOR (approval-eligible)** | r1 MAJORs closed structurally; 4 new MINOR findings (n1-n4) addressed in this revision wave. Artifact: `scripts/review/results/2026-05-20-plan-draft-digitalmodel-ocimf-polar-vessel-force-overlay-claude-r2.md`. |
| Revision (r2→post-r2) | applied 2026-05-20 | n1: added `_convention.py` row with `OCIMF_CONVENTION_AUTHORITY` API surface; n2: added enforceable git-history assertion in test #13; n3: widened TDD #17 to dataclass-count-exactly-1; n4: moved Plotly spike artifact from GH-comment to `digitalmodel/docs/spikes/`. |
| Codex | _pending_ | Not yet run; requires issue + plan to be posted to digitalmodel first per `feedback_codex_needs_pushed_artifact`. |
| Gemini | _pending_ | Not yet run; same prerequisite. |

**Overall result:** _r1 MAJOR — revised in this commit; r2 verdict will determine whether revisions are sufficient to surface for user approval._ Per `feedback_adversarial_review_stance` and `feedback_r3_inline_loop_break_pattern`, two inline Claude rounds (r1+r2) are the cap before requiring external provider dispatch; if r2 returns MAJOR, the plan is handed back to user with the MAJOR-loop signal rather than self-cycling to r3.

**Revisions applied between r1 and r2:**

1. **M1:** §Pseudocode `_resolve_arrow_direction_in_body_frame` docstring now declares a citation gate; §TDD #5/#6 rewritten as property assertions deriving expected value from a citation-bound authority constant; new TDD #16 (180° invariant under sign flip) added as independent cross-check; §Acceptance now requires the OCIMF citation in code + docstring.
2. **M2:** §RIS Existing repo code expanded with 4-5 hull/vessel-related entries; explicit reuse decision recorded (`HullProfile` + new `VesselSilhouetteSpec` wrapper, not a competing `VesselGeometry`); §Pseudocode dataclass updated; new TDD #17 (static-analysis check forbidding competing dataclass); §Files-to-Change adds `types.py` for `VesselSilhouetteSpec`.
3. **m1:** §Files-to-Change adds `fixtures/ocimf_explorer_pre_refactor_trace_signature.json` row with capture sequencing.
4. **m2:** §TDD #14 strengthened to five sub-conditions; §Acceptance updated accordingly.
5. **m3:** §TDD #15 rewritten to use the legal-sanity-scan script or `.legal-deny-list.yaml` patterns.
6. **m4:** §Files-to-Change adds `fixtures/ocimf_explorer_baseline.html` byte-frozen baseline row.
7. **m5:** §Risks Open adds citation-contract applicability decision (rendering module emits no Citation; Figure `layout.meta` carries provenance for downstream report consumers).
8. **m6:** §Risks Plotly-arrow-rendering risk now requires an implementation-phase pre-spike artifact attached to the digitalmodel issue before plan-approval.

---

## Risks and Open Questions

- **Risk (canonical home contested):** placing the new module under `marine_engineering/visualization/` declares that subtree as canonical, while a 660-LOC report at `marine_analysis/reporting/ocimf_interactive_report.py` and the `marine_analysis/visualization/` subdir exist. If workspace-hub#2768's consolidation later decides `marine_analysis` is canonical, the module moves. Mitigation: name it `polar_force_overlay` (concern-specific, not OCIMF-specific) so the move is mechanical.
- **Risk (silhouette accuracy):** generic tanker/gas-carrier silhouettes may visually mismatch the specific vessel a study models (e.g., B1528 SIROCCO). Mitigation: support `silhouette_kind='custom_path'` with caller-supplied polygon (test #11).
- **Risk (frame-convention bug):** the OCIMF convention "positive Cyc at θ=90° starboard-incidence means force +Y starboard" is unintuitive (wind from starboard physically pushes toward port). Test #5 + #6 pin the correctness claim. If the test is written wrong, the bug propagates downstream silently. Mitigation: cite the docstring at `build_coefficient_explorer.py:399-405` AND the file comment at line 752 as the convention source; require reviewer to verify against OCIMF MEG3/MEG4 directly.
- **Risk (Plotly polar arrow rendering):** Plotly's `Scatterpolar` does not natively support arrowheads. Three candidate techniques: (a) `layout.annotations` with explicit Cartesian conversion + `xref='x'`/`yref='y'`, (b) line+marker pairs with `symbol='arrow'` (Cartesian projection only — would require breaking out of polar), (c) custom SVG injection via `fig.add_shape` with `'path'` type. **r1 m6 fix (tightened r2 n4):** the implementing agent MUST pre-spike the chosen technique during plan-review (≤30 LOC throwaway prototype rendering a single arrow on a polar axis at θ=90°, r=0.5, pointing to vessel-fixed +Y). The spike artifact MUST be committed to the repo at `digitalmodel/docs/spikes/2026-05-20-plotly-polar-arrow-technique/` (containing at minimum: the prototype script, the rendered HTML output, and a one-paragraph README naming the chosen technique and why). The digitalmodel issue comment links to the committed spike artifact rather than carrying the spike inline. This survives GH-comment edits/deletes and lets reviewers verify by commit SHA. Tests assert *direction property* (per r1-revised #5/#6) not *arrowhead pixel exactness*.
- **Risk (overlap with existing 660-LOC report):** `ocimf_interactive_report.py` may already render similar polars. Not consolidating it now leaves duplicated logic; consolidating it expands this plan's scope significantly. Decision: leave it alone; flag for workspace-hub#2768 follow-on.
- **Open:** should the module accept a Plotly `Figure` to mutate-in-place (composability with other reports) instead of always creating a new one? Suggested API extension: `polar_force_overlay(..., fig: go.Figure | None = None)` where `None` creates new. Defer to plan-review.
- **Open:** Force arrow rendering for moments (K, M, N): are curved-arrow glyphs acceptable, or should moments be rendered separately as a secondary subplot? Defer to plan-review (with a default of "curved-arrow glyph in v1, secondary subplot deferred").
- **Open:** for SIROCCO consumer (#2760), should the arrows reflect the *rudder-angle-induced* component split (separate arrows per rudder angle) or only the total resultant? This is a #2760-side decision; this module's API supports either via `group_col`.
- **Open (r1 m5 — citation contract applicability):** `.claude/rules/calc-citation-contract.md` applies "When a calc module uses a standards-derived constant or formula." This module *renders* (does not compute) coefficient values. **Decision proposed:** (a) the module itself emits no Citation — it is a rendering layer, not a calc module; (b) the rendered Plotly Figure carries a `fig.layout.meta` dict with provenance fields `{'data_source_kind': 'OCIMF-MEG3|OCIMF-MEG4|caller-supplied', 'data_source_code_id': '<resolver-code-id>', 'data_source_revision': '<rev>'}` populated by the caller, so downstream report consumers can emit the correct `Citation` sidecar at report-emission time without re-deriving provenance; (c) silhouette polygons are classified "conventional, not standards-derived" — they are stylized hull shapes for visual reference only, not engineering geometry, and emit no Citation. Reviewer to confirm or override this decision before plan-approval.
- **Open (r1 M2 — HullProfile field-set confirmation):** the §RIS Decision adopts `HullProfile` reuse, but `HullProfile`'s exact field set was not enumerated in this plan (only its existence and high-level docstring). The implementing agent must verify during plan-review that `HullProfile` carries LOA, beam, and (optional) draft, OR document the field-mapping shim required. If a critical field is missing, revisit the decision and consider whether to add the field to `HullProfile` (which broadens this plan's scope into `hydrodynamics/hull_library/`) or fall back to a local minimal dataclass with explicit "extends HullProfile" framing.

---

## Complexity: T3

**T3** — new module + new directory + 3 new test files + refactor of consumer #1 + downstream-consumer hook + 15 TDD cases including a CRITICAL frame-correctness test (#5/#6). Layered-architecture-issue-planning checks apply because the plan declares a canonical home that competes with two pre-existing duplicate paths. Falls under `cat:engineering-calculations`, so the `engineering-issue-workflow` skill applies during implementation.
