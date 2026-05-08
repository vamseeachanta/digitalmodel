# Issue #598 Plan — SIROCCO current-heading/rudder force component chart set

Status: `status:plan-review` (awaiting user approval before implementation)

- Issue: https://github.com/vamseeachanta/digitalmodel/issues/598
- Repository: `vamseeachanta/digitalmodel`
- Date: 2026-05-08
- Complexity: Medium engineering-calculation/reporting feature
- Workflow gate: engineering-critical; no implementation until user approval.
- Adversarial review: first pass returned `MAJOR`; this revision resolves the findings by defining a ship-fixed force transform, hardening the default-speed contract, and adding template evidence/artifact sections.

## 1. Objective

Create a B1528 SIROCCO/Sorocco force-component sweep for a moored/current-driven rudder case with:

- Current speeds: `1, 1.5, 2.0, 2.5, 3, 3.5, 4, 4.5 kn`
- Heading offsets: `-10..+10 deg`, step `1 deg`
- Rudder angles: `-10..+10 deg`, step `1 deg`
- Requested chart dropdown default: `4.56 kn`

The report shall let the reviewer select a current speed from a dropdown, default to the requested `4.56 kn`, and then inspect the force-component response over heading offset and rudder angle.

## 2. Resource Intelligence Summary

### Existing repo code consulted

| File | Evidence/use |
|---|---|
| `src/digitalmodel/naval_architecture/b1528_sirocco_moored_current_report.py` | Existing fixed B1528 moored-current report. It assumes moored vessel, centerline current, `3.5 kn`, rudder angles `1..5 deg`, and neutral `Cr=1.0`. Existing formulas define `F`, `Fn`, local `X`, local `Y`, and yaw `N`. |
| `src/digitalmodel/naval_architecture/data/b1528_sirocco_moored_current.yml` | Stores B1528 design inputs: `LBP=225.5 m`, `yaw_lever_m=135.3 m`, `rudder_area_m2=44.93956319369854`, `current_speed_kn=3.5`, `Cr=1.0`, and force convention. |
| `tests/naval_architecture/test_b1528_sirocco_moored_current.py` | Locks fixed-case YAML loading, symmetry, sample formulas, artifacts, report sections, and traceability. Use as TDD pattern for the new sweep. |
| `src/digitalmodel/naval_architecture/b1528_sirocco_yaw_report.py` | Provides `KNOT_TO_M_PER_S`, `NON_ROTATING_PROPELLER_CR`, and propeller-rotation factor notes. |
| `src/digitalmodel/naval_architecture/yaw_moment.py` | Demonstrates stricter sign convention metadata and validation. Mirror this style in the new workflow. |
| `src/digitalmodel/naval_architecture/maneuverability.py` | Contains a general `rudder_normal_force` helper. Useful reference, but this plan intentionally retains the B1528/Barrass workbook-family formula for continuity with the fixed report. |
| `src/digitalmodel/naval_architecture/rudder_stock_torque.py` | Useful pattern for separating signed physical quantities from equal/opposite reaction quantities. |

### Existing docs consulted

| Document | Evidence/use |
|---|---|
| `docs/domains/marine-engineering/b1528-sirocco-moored-current-report.md` | Fixed-case durable report contract. Confirms scope exclusions and existing force convention. |
| `/mnt/local-analysis/workspace-hub/docs/plans/_template-issue-plan.md` | Planning template source. This plan includes evidence, artifact map, files-to-change table, risks, and adversarial review summary. |
| `knowledge/wikis/acma-projects/wiki/concepts/b1528-sirocco-rudder-yaw-moment-inputs.md` | B1528/SIROCCO source-pack concept page. |
| `knowledge/wikis/acma-projects/wiki/entities/b1528-sirocco-breakaway.md` | B1528/SIROCCO entity context. |

### GitHub issue context consulted

- Existing open issue #597: `chore(repo-structure): classify and relocate B1528 generated evidence`.
- Related open issues #143/#144/#163 cover propeller-rudder/rudder survey work but do not cover this specific current-heading/rudder interactive chart request.
- New issue created: #598.

## 3. Embedded Evidence

```text
Existing fixed case formula from b1528_sirocco_moored_current_report.py:
F = beta * rudder_area_m2 * current_speed_m_s**2 * Cr
normal_force_N = F * sin(delta)
force_x_surge_downstream_N = abs(F * sin(delta)**2)
force_y_sway_port_N = F * sin(delta) * cos(delta)
moment_n_yaw_bow_port_Nm = force_y_sway_port_N * yaw_lever_m

Existing fixed report scope:
- centerline/aligned current only
- rudder-induced current loads only
- no hull current force, bank effect, tug load, mooring-line stiffness,
  current-profile variation, propeller race, or class/IMO conclusion

New requested grid:
- 8 requested speed planes
- 21 heading offsets
- 21 rudder angles
- 3528 requested engineering rows
- plus 441 extra rows if 4.56 kn is included as exact chart default plane
```

## 4. Gaps Identified

1. Existing fixed report only handles a `3.5 kn` aligned-current case and rudder magnitudes `1..5 deg`.
2. There is no speed × heading offset × rudder angle sweep.
3. Existing fixed-report `X/Y` components are valid only when the current-aligned local frame and ship-fixed frame coincide.
4. Requested dropdown default `4.56 kn` is outside the stated discrete speed list ending at `4.5 kn`.
5. Existing HTML charts do not provide a current-speed dropdown.
6. A two-chart presentation must still expose all 21 rudder angles, not just representative traces.

## 5. Proposed Calculation Contract

### 5.1 Coordinate and sign conventions

Use a ship-fixed COG output frame for all reported force/moment components:

- Ship axes: `+X_ship` forward/downstream reference for zero heading, `+Y_ship` port, `+Z` up.
- Positive heading offset `psi`: local downstream current-force axis rotated toward port from `+X_ship`.
- Positive rudder angle `delta`: rudder to port.
- Positive yaw moment `+N`: bow-to-port.
- `K` and `M`: zero in this planar rudder-only model.
- Mooring reactions are equal/opposite context quantities only, not mooring-line tensions.

### 5.2 Local current/rudder model

Retain the B1528/Barrass workbook-family formula in the **local current frame**:

```text
V = current_speed_kn * 0.51444
F = beta * A_R * V^2 * Cr
alpha = radians(delta - psi)
Fn_local = F * sin(alpha)
X_local_downstream = F * sin(alpha)^2
Y_local_port_of_current = F * sin(alpha) * cos(alpha)
```

Interpretation:

- `alpha = delta - psi` is a first-cut relative rudder inflow angle.
- `X_local_downstream` is nonnegative drag along the local current downstream axis.
- `Y_local_port_of_current` is signed transverse force in the local current frame.
- When `psi=0`, local current frame equals ship-fixed frame and this reduces to the existing fixed-report formula.
- This remains a first-cut visualization assumption, not a validated MMG/hull-current model.

### 5.3 Transform local force to ship-fixed COG components

For nonzero heading offset, rotate local current-frame components into ship-fixed axes before reporting charts/tables:

```text
psi_rad = radians(heading_offset_deg)
e_x_local = [cos(psi), sin(psi)]
e_y_local = [-sin(psi), cos(psi)]

X_ship = X_local_downstream * cos(psi) - Y_local_port_of_current * sin(psi)
Y_ship = X_local_downstream * sin(psi) + Y_local_port_of_current * cos(psi)
N_ship = Y_ship * yaw_lever_m
```

This resolves the major review finding that the original draft mixed local/current-aligned and ship-fixed components.

### 5.4 Worked sign examples

At selected speed `4.56 kn`:

- `psi=0`, `delta=+10`: `alpha=+10`, `Y_local>0`, `Y_ship>0`, `N_ship>0`.
- `psi=0`, `delta=-10`: `alpha=-10`, `Y_local<0`, `Y_ship<0`, `N_ship<0`; `X_local` remains positive.
- `psi=+5`, `delta=+10`: `alpha=+5`; local rudder-induced force magnitude matches the `psi=0`, `delta=+5` local case, but ship-fixed `X_ship/Y_ship` differ because the local force vector is rotated by `+5 deg`.
- `psi=+5`, `delta=+5`: `alpha=0`; local rudder-induced `X_local/Y_local≈0`, and therefore ship-fixed rudder-induced `X_ship/Y_ship/N_ship≈0`. This does **not** mean total current load is zero; it means this simplified rudder-induced component is zero.

### 5.5 Speed grid and default handling

The engineering sweep and UI default are deliberately separated:

- Engineering sweep speeds: `[1, 1.5, 2.0, 2.5, 3, 3.5, 4, 4.5]`.
- Chart default speed: `4.56 kn`.
- Implementation shall include `4.56 kn` as an extra computed chart-default plane, flagged with `is_chart_default_extra_speed=true`.
- Provenance and labels shall state: `4.56 kn is an extra chart-default case outside the requested engineering sweep list`.

This avoids a dropdown default that points to a missing dataset. If the user rejects the extra-plane approach, switch before implementation to nearest-neighbor default `4.5 kn` or interpolation.

### 5.6 Grid size

- Requested engineering grid: `8 speeds * 21 headings * 21 rudders = 3528 rows`.
- With exact default extra plane: `9 speeds * 21 headings * 21 rudders = 3969 rows`.
- Extra-plane rows: `441`, all with `is_chart_default_extra_speed=true`.

## 6. Artifact Map

| Artifact | Path | Purpose | Commit timing |
|---|---|---|---|
| Plan | `docs/plans/2026-05-08-issue-598-sirocco-current-heading-rudder-force-components.md` | Review/approval artifact | Commit before implementation if user wants persistent plan; otherwise attach/link via issue comment. |
| Input YAML | `src/digitalmodel/naval_architecture/data/b1528_sirocco_current_heading_rudder.yml` | Packaged input contract | Implementation after approval. |
| Report module | `src/digitalmodel/naval_architecture/b1528_sirocco_current_heading_rudder_report.py` | Calculation and report writer | Implementation after approval. |
| Package exports | `src/digitalmodel/naval_architecture/__init__.py` | Public API exports | Implementation after approval. |
| Tests | `tests/naval_architecture/test_b1528_sirocco_current_heading_rudder.py` | TDD coverage | First implementation artifact after approval. |
| Durable doc | `docs/domains/marine-engineering/b1528-sirocco-current-heading-rudder-report.md` | Human-readable contract | Implementation after approval. |
| Generated outputs | `outputs/b1528_sirocco/current_heading_rudder/*` | CSV/JSON/HTML report artifacts | Implementation after approval; coordinate with #597 before committing generated evidence. |

## 7. Files-to-Change Table

| File | Action | Notes |
|---|---|---|
| `src/digitalmodel/naval_architecture/data/b1528_sirocco_current_heading_rudder.yml` | Add | New packaged input. Include requested sweep, default speed, sign convention, scope exclusions, and chart metadata. |
| `src/digitalmodel/naval_architecture/b1528_sirocco_current_heading_rudder_report.py` | Add | New workflow. Avoid destabilizing fixed 3.5 kn report. |
| `src/digitalmodel/naval_architecture/__init__.py` | Update | Export load/run/write helpers. |
| `tests/naval_architecture/test_b1528_sirocco_current_heading_rudder.py` | Add | TDD first. |
| `docs/domains/marine-engineering/b1528-sirocco-current-heading-rudder-report.md` | Add | Durable report contract and generation command. |
| `outputs/b1528_sirocco/current_heading_rudder/*` | Add/update after approval | Generated artifacts; verify placement against #597. |

## 8. Chart Concept

The HTML shall use two primary chart panels, each controlled by a current-speed dropdown that defaults to `4.56 kn`.

### Chart 1 — Ship-fixed force components over heading, with rudder selector

Purpose: show how ship-fixed `X_ship`, `Y_ship`, and resultant horizontal force vary with heading at a selected current speed and selected rudder angle(s).

Required controls:

- Current speed dropdown: includes engineering speeds and flagged extra `4.56 kn` default.
- Rudder angle selector/dropdown/slider: all `-10..+10 deg` values must be accessible in the HTML.

Default display:

- Select `4.56 kn`.
- Select representative rudder `+10 deg` or show five initial traces (`-10`, `-5`, `0`, `+5`, `+10`) with selector access to all 21.

Axes/data:

- X axis: `heading_offset_deg`.
- Traces: `X_ship`, `Y_ship`, `resultant_horizontal_force`, in kN for readability.
- Include hover fields for `alpha`, `X_local`, `Y_local`, `N_ship`.

Placeholder concept:

```text
[Current speed: 4.56 kn v] [Rudder angle(s): all available v]

Chart panel 1: Ship-fixed force components
┌────────────────────────────────────────────────────────────┐
│ X_ship, Y_ship, and resultant vs heading offset             │
│ full rudder-angle access through selector/slider            │
└────────────────────────────────────────────────────────────┘
```

### Chart 2 — Ship-fixed yaw moment heatmap over heading × rudder

Purpose: show combined heading/rudder sensitivity at selected current speed.

Required controls:

- Current speed dropdown: defaults to `4.56 kn`.

Axes/data:

- X axis: `heading_offset_deg`.
- Y axis: `rudder_angle_deg`.
- Color: `N_ship yaw moment (kN-m)`, signed with diverging color scale centered at zero.
- Hover fields: `alpha`, `X_ship`, `Y_ship`, resultant.

Placeholder concept:

```text
[Current speed: 4.56 kn v]

Chart panel 2: Yaw moment response surface
┌────────────────────────────────────────────────────────────┐
│ Heatmap: rudder angle (y) × heading offset (x), color=Nship │
│ Diverging scale: starboard-yaw / zero / port-yaw            │
└────────────────────────────────────────────────────────────┘
```

## 9. Pseudocode

```python
@dataclass(frozen=True)
class B1528CurrentHeadingRudderConfig:
    case_id: str
    current_speeds_kn: tuple[float, ...]
    chart_default_current_speed_kn: float
    heading_offsets_deg: tuple[float, ...]
    rudder_angles_deg: tuple[float, ...]
    lbp_m: float
    yaw_lever_m: float
    rudder_area_m2: float
    beta: float
    prop_rotation_factor: float
    force_convention: dict[str, str]


def validate_config(payload):
    require top-level sections
    assert current_speeds_kn == (1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5)
    assert heading_offsets_deg == tuple(range(-10, 11))
    assert rudder_angles_deg == tuple(range(-10, 11))
    assert chart_default_current_speed_kn == 4.56
    assert prop_rotation_factor == NON_ROTATING_PROPELLER_CR
    return B1528CurrentHeadingRudderConfig(...)


def speed_planes_for_calculation(cfg):
    speeds = list(cfg.current_speeds_kn)
    if cfg.chart_default_current_speed_kn not in speeds:
        speeds.append(cfg.chart_default_current_speed_kn)
    return sorted(speeds)


def row(cfg, current_speed_kn, heading_offset_deg, rudder_angle_deg):
    V = current_speed_kn * KNOT_TO_M_PER_S
    base_force_N = cfg.beta * cfg.rudder_area_m2 * V**2 * cfg.prop_rotation_factor

    psi = radians(heading_offset_deg)
    alpha_deg = rudder_angle_deg - heading_offset_deg
    alpha = radians(alpha_deg)

    normal_force_N = base_force_N * sin(alpha)
    x_local_N = base_force_N * sin(alpha)**2
    y_local_N = base_force_N * sin(alpha) * cos(alpha)

    x_ship_N = x_local_N * cos(psi) - y_local_N * sin(psi)
    y_ship_N = x_local_N * sin(psi) + y_local_N * cos(psi)
    n_ship_Nm = y_ship_N * cfg.yaw_lever_m

    is_extra_default = (
        current_speed_kn == cfg.chart_default_current_speed_kn
        and current_speed_kn not in cfg.current_speeds_kn
    )

    return {
        "case_id": cfg.case_id,
        "current_speed_kn": current_speed_kn,
        "is_chart_default_extra_speed": is_extra_default,
        "heading_offset_deg": heading_offset_deg,
        "rudder_angle_deg": rudder_angle_deg,
        "effective_rudder_inflow_angle_deg": alpha_deg,
        "base_force_N": base_force_N,
        "normal_force_N": normal_force_N,
        "force_x_local_downstream_N": x_local_N,
        "force_y_local_port_of_current_N": y_local_N,
        "force_x_ship_N": x_ship_N,
        "force_y_ship_port_N": y_ship_N,
        "force_z_heave_N": 0.0,
        "moment_k_roll_Nm": 0.0,
        "moment_m_pitch_Nm": 0.0,
        "moment_n_yaw_bow_port_Nm": n_ship_Nm,
        "moment_n_yaw_bow_port_kN_m": n_ship_Nm / 1000.0,
        "resultant_horizontal_force_N": hypot(x_ship_N, y_ship_N),
        "mooring_reaction_x_N": -x_ship_N,
        "mooring_reaction_y_N": -y_ship_N,
        "mooring_reaction_n_Nm": -n_ship_Nm,
    }


def run_report(cfg):
    rows = []
    for speed in speed_planes_for_calculation(cfg):
        for heading in cfg.heading_offsets_deg:
            for rudder in cfg.rudder_angles_deg:
                rows.append(row(cfg, speed, heading, rudder))
    return {"metadata": metadata(cfg), "rows": rows, "summary": summary(rows)}


def html_report(result):
    embed rows JSON
    build speed dropdown from unique current_speed_kn
    set default selected speed to exact 4.56
    build rudder selector for all 21 rudder angles for Chart 1
    on speed/rudder change:
        filter rows for speed and selected rudder(s)
        update Chart 1 line traces
        filter rows for speed and all headings/rudders
        update Chart 2 yaw heatmap
```

## 10. TDD Plan

Add `tests/naval_architecture/test_b1528_sirocco_current_heading_rudder.py` before implementation.

Required tests:

1. YAML loads and validates exact sweep lists:
   - 8 requested speeds.
   - 21 headings.
   - 21 rudder angles.
   - default speed `4.56` present as chart default.
2. Full run row count:
   - `3969` rows if default extra speed plane is included.
   - `3528` rows where `is_chart_default_extra_speed == False`.
   - `441` rows where `is_chart_default_extra_speed == True`.
3. Zero effective rudder-induced angle identity:
   - If `rudder_angle_deg == heading_offset_deg`, then local and ship-fixed rudder-induced `X/Y/N≈0`.
   - Report wording must clarify this is not total hull current load.
4. Centerline regression identity:
   - At `heading=0`, formulas match existing fixed-report formulas for the same speed/rudder.
5. Sign case:
   - At `heading=0`, `rudder=+10`: `Y_ship>0`, `N_ship>0`.
   - At `heading=0`, `rudder=-10`: `Y_ship<0`, `N_ship<0`.
   - Local `X` equal and nonnegative for ±10.
6. Heading/rudder interaction:
   - `heading=+5`, `rudder=+10` has same local force as `heading=0`, `rudder=+5`, but different ship-fixed `X/Y` due to rotation.
7. Ship-fixed transform test:
   - Independently compute `X_ship = X_local*cos(psi)-Y_local*sin(psi)` and `Y_ship = X_local*sin(psi)+Y_local*cos(psi)` for a nonzero heading case.
8. Speed-squared scaling:
   - Compare `2 kn` vs `4 kn`; same heading/rudder gives local force and ship-fixed force/moment ratio `4.0`.
9. Formula sample for default speed:
   - At `4.56 kn`, `heading=0`, `rudder=+1`, independently compute `F`, `X`, `Y`, `N` and compare.
10. Artifact test:
   - `write_*_report()` creates CSV/JSON/provenance/Markdown/HTML/manifest.
   - HTML contains dropdown, exact `4.56`, `is_chart_default_extra_speed`, `Plotly.newPlot`, two chart-panel containers, rudder selector for all 21 angles, and embedded units.
11. Provenance test:
   - Captures heading/rudder effective-angle convention.
   - Captures local-to-ship transform.
   - Captures `4.56 kn` as an extra chart-default plane.
   - Captures scope exclusions.
12. Package export/import smoke:
   - `digitalmodel.naval_architecture.load_packaged_b1528_current_heading_rudder_config`
   - `run_b1528_current_heading_rudder_report`
   - `write_b1528_current_heading_rudder_report`
13. Package-data smoke if new YAML is packaged.

## 11. Verification Commands

Run focused tests first:

```bash
PYTHONPATH=src uv run python -m pytest tests/naval_architecture/test_b1528_sirocco_current_heading_rudder.py -q
```

Then regression tests touching reused helpers/patterns:

```bash
PYTHONPATH=src uv run python -m pytest \
  tests/naval_architecture/test_b1528_sirocco_moored_current.py \
  tests/naval_architecture/test_b1528_sirocco_yaw_moment.py \
  tests/naval_architecture/test_rudder_stock_torque_sweep.py \
  -q
```

If package exports/data change:

```bash
PYTHONPATH=src uv run python -m pytest tests/naval_architecture -q
```

## 12. Review Artifact Paths

- Draft plan: `docs/plans/2026-05-08-issue-598-sirocco-current-heading-rudder-force-components.md`
- Adversarial review summary: embedded in Section 14.
- Issue comment/update target: https://github.com/vamseeachanta/digitalmodel/issues/598

## 13. Risks and Open Questions

1. **Heading sign convention** — This revision freezes a convention (`psi>0` rotates local downstream current-force axis toward port; `alpha=delta-psi`). User/engineering reviewer can still change it before approval.
2. **Formula validity under oblique current** — This remains a first-cut rudder-only visualization using the B1528/Barrass workbook-family formula. It is not a validated oblique-current hull/rudder interaction model.
3. **Default `4.56 kn` vs speed list ending at `4.5 kn`** — Plan proposes `4.56 kn` as an extra chart-default plane outside the engineering sweep. User can reject this and choose nearest-neighbor or interpolation before approval.
4. **Chart density** — Chart 1 must expose all rudder angles through a selector/slider. Chart 2 heatmap carries the full 21×21 response surface.
5. **Artifact placement** — Existing issue #597 may relocate B1528 generated evidence. Generated outputs should follow the latest repo-structure decision before committing.

## 14. Adversarial Review Summary and Resolution

### First adversarial review verdict: `MAJOR`

Major findings received:

1. Heading-offset physics was underdefined and mixed local/current-aligned and ship-fixed components.
2. Formula validity under oblique current was not sufficiently bounded.
3. Sign convention was ambiguous.
4. `4.56 kn` default handling was operationally sensible but not crisply labeled.
5. Chart 1 risked hiding full rudder-angle coverage by showing only representative traces.
6. Tests did not guard frame-definition problems.
7. Plan lacked template sections: embedded evidence, artifact map, files-to-change table, review artifact paths.

### Resolution in this revision

- Added ship-fixed output frame and explicit local-to-ship force transform.
- Framed the oblique-current extension as first-cut visualization, not a validated full hydrodynamic/MMG model.
- Froze heading/rudder sign convention and added worked sign examples.
- Labeled `4.56 kn` as `is_chart_default_extra_speed=true` outside the requested engineering sweep.
- Required all 21 rudder angles to be accessible in the HTML.
- Added transform-specific tests and chart-data tests.
- Added evidence, artifact map, files-to-change table, and review artifact paths.

### Re-review verdict: `APPROVE`

The re-review found no remaining plan-level blocker. Remaining approval choices for the user are:

- accept or modify the proposed `4.56 kn` extra-plane approach;
- accept or modify the frozen heading/sign convention.

## 15. Acceptance Criteria

- [ ] No implementation code/output artifacts are committed before user approval.
- [ ] Issue #598 links to this plan.
- [ ] User approves or modifies the sign/default-speed contract.
- [ ] Issue #598 is moved to `status:plan-approved` before implementation.
- [ ] Tests are written before implementation.
- [ ] Calculation output includes the requested engineering sweep: 8 speeds × 21 headings × 21 rudders.
- [ ] Calculation output separately flags the extra `4.56 kn` chart-default plane, if approved.
- [ ] Dropdown defaults exactly to `4.56 kn` or the user-approved alternative.
- [ ] Two interpretation-focused charts are generated and documented.
- [ ] Chart 1 exposes all 21 rudder angles via selector/slider.
- [ ] Chart 2 shows full heading × rudder yaw-moment heatmap.
- [ ] Provenance documents formula, sign convention, local-to-ship transform, default-speed handling, and exclusions.
- [ ] Existing B1528 fixed-case report tests continue to pass.
