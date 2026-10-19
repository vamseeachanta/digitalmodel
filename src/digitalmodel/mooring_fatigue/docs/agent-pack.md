# Mooring fatigue knowledge pack

| Header | Value |
|---|---|
| Module | `mooring_fatigue` |
| Repository | `digitalmodel` |
| Package path | `src/digitalmodel/mooring_fatigue` |
| Pack revision | 0 |
| Code revision | `29a87f1aba14aa22067794846aa5c2ec8bf407c1` |
| Analysis family | Engineering calculations (`EC`), AQG r0 Python calculations |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |

Citation ranges refer to the code revision above; section 9 explicitly identifies the separate implementation-map revision.

## 1. Purpose and scope

The router screens metallic mooring lines by converting stress or tension bins or histories to cumulative damage, life and design-fatigue-factor margin, then writes detail and summary CSV files. [src/digitalmodel/mooring_fatigue/workflow.py:L23-L58] [src/digitalmodel/mooring_fatigue/workflow.py:L61-L108]

Supported material tags are `CHAIN`, `STEEL-WIRE` and `STEEL_WIRE`; synthetic-rope tags and unsupported tags raise `ValueError`. [src/digitalmodel/mooring_fatigue/workflow.py:L19-L20] [src/digitalmodel/mooring_fatigue/workflow.py:L175-L181]

The routing registry records development maturity; the engine dispatches basename `mooring_fatigue` to this router. [docs/registry/module-routing.yaml:L576-L581] [src/digitalmodel/engine.py:L322-L325]

The package performs scalar fatigue screening on supplied loading; its computation route has no geometry, water-depth, heading, coordinate-origin or phase input, and material labels do not select different fatigue curves. [src/digitalmodel/mooring_fatigue/workflow.py:L23-L85] [src/digitalmodel/mooring_fatigue/workflow.py:L88-L149] [src/digitalmodel/mooring_fatigue/workflow.py:L175-L186]

UNVERIFIED: suitability of the selected detail curve for a particular chain or wire construction, and a numerical geometry or water-depth applicability envelope; issue pending (2).

## 2. Inputs and outputs

### Inputs

Ranges below describe implemented checks, not a qualified design envelope. [src/digitalmodel/mooring_fatigue/workflow.py:L189-L227]

| Input | Unit | Implemented range / precedence | Default | Source and evidence |
|---|---|---|---|---|
| `design_life_years` | years | Required, converted to float, rejects values ≤ 0; no finiteness guard | None | Settings; [src/digitalmodel/mooring_fatigue/workflow.py:L24-L26] [src/digitalmodel/mooring_fatigue/workflow.py:L210-L217] |
| `dff` | dimensionless | Same positivity check | None | Settings; [src/digitalmodel/mooring_fatigue/workflow.py:L24-L26] [src/digitalmodel/mooring_fatigue/workflow.py:L210-L217] |
| `sn_curve.curve`, `.environment` | identifiers | Upstream curve/environment validation | `D`, `seawater_cp` | Settings; [src/digitalmodel/mooring_fatigue/workflow.py:L184-L186] [src/digitalmodel/fatigue/sn_curves.py:L96-L124] |
| `lines` | records | Nonempty list | None | Settings; [src/digitalmodel/mooring_fatigue/workflow.py:L189-L193] |
| `id` / `name`, `material` | identifiers | ID then name then fallback; material normalisation and allowlist | `line`, `CHAIN` | Line; [src/digitalmodel/mooring_fatigue/workflow.py:L67-L69] [src/digitalmodel/mooring_fatigue/workflow.py:L175-L181] |
| `stress_range_bins` | MPa, cycles | First route; aliases `stress_range_MPa`, `stress_range_mpa`, `stress_range`, `range`; positive float ranges/counts | None | Line; [src/digitalmodel/mooring_fatigue/workflow.py:L88-L91] [src/digitalmodel/mooring_fatigue/workflow.py:L124-L134] [src/digitalmodel/mooring_fatigue/workflow.py:L200-L227] |
| `tension_range_bins`, `area_mm2` | kN, mm², cycles | Second route; tension aliases `tension_range_kN`, `range_kN`, `range`, `tension_range`; area alias `cross_section_area_mm2` | None | Line; [src/digitalmodel/mooring_fatigue/workflow.py:L92-L98] [src/digitalmodel/mooring_fatigue/workflow.py:L111-L121] [src/digitalmodel/mooring_fatigue/workflow.py:L196-L201] |
| `stress_history_MPa` / `tension_history_kN` | MPa / kN samples | Third / fourth route; tension requires area; history conversion does not use positive-bin validation | None | Line; [src/digitalmodel/mooring_fatigue/workflow.py:L99-L107] [src/digitalmodel/mooring_fatigue/workflow.py:L137-L149] |
| `n_bins` | bins | Converted with `int`; no explicit positive-integer guard | 20 | Line; [src/digitalmodel/mooring_fatigue/workflow.py:L137-L139] |
| `n_repeats` / `n_histories` | multiplier | First key wins; `float` conversion without positive/finiteness guard | 1.0 | Line; [src/digitalmodel/mooring_fatigue/workflow.py:L140-L148] |
| Bin `n_cycles` / `cycles` / `count` | cycles | First non-null alias wins; rejects ≤ 0 | None | Bin; [src/digitalmodel/mooring_fatigue/workflow.py:L200-L227] |
| `output_dir`, config metadata, basename | filesystem identifiers | Relative output anchored to config directory, file parent or working directory; file stem preferred | `results`, `mooring_fatigue` | Settings/config; [src/digitalmodel/mooring_fatigue/workflow.py:L242-L268] |

Scalar histories are counted without a time array, and supplied bin counts are passed directly to Miner damage without annualisation; therefore a life calculation requires an explicitly documented exposure basis for those counts. [src/digitalmodel/mooring_fatigue/workflow.py:L69-L74] [src/digitalmodel/mooring_fatigue/workflow.py:L137-L148] [src/digitalmodel/fatigue/rainflow.py:L22-L38]

### Outputs

| Output | Unit / convention | Contents and evidence |
|---|---|---|
| Updated `cfg.mooring_fatigue` | damage and margin dimensionless; life in years | Replaces settings with design life, factor, curve, governing line, damage, life, margin, status, summaries and CSV paths; status passes at margin ≥ 1.0. [src/digitalmodel/mooring_fatigue/workflow.py:L37-L58] |
| Detail CSV | kN, MPa, cycles; damage dimensionless | Line/material IDs, zero-based bin index, tension range, stress range, count, allowable count and bin damage; direct stress/history inputs use NaN for unavailable tension. [src/digitalmodel/mooring_fatigue/workflow.py:L124-L149] [src/digitalmodel/mooring_fatigue/workflow.py:L152-L172] |
| Summary CSV | years, dimensionless damage/margin | One row per supplied line; required life, calculated life and `passes_dff`. [src/digitalmodel/mooring_fatigue/workflow.py:L76-L85] |
| File naming / encoding | CSV header row; platform text encoding | `<stem>_mooring_fatigue.csv` and `<stem>_mooring_fatigue_summary.csv`; writes replace existing contents, with encoding unspecified in `open`. [src/digitalmodel/mooring_fatigue/workflow.py:L242-L247] [src/digitalmodel/mooring_fatigue/workflow.py:L271-L278] |

## 3. Method and governing standards

| Method | Implementation and basis | Provenance boundary |
|---|---|---|
| Tension-to-stress | Stress range in MPa = tension range in kN × 1000 / area in mm²; direct stress is used unchanged. [src/digitalmodel/mooring_fatigue/workflow.py:L99-L134] | Unit conversion, not a structural model; no mean-stress correction on the bin route. [src/digitalmodel/mooring_fatigue/workflow.py:L111-L134] |
| History processing | `_history_bins` calls `rainflow_count`, `stress_histogram` and multiplies resulting counts by repeat count. [src/digitalmodel/mooring_fatigue/workflow.py:L137-L149] | Upstream docstring names ASTM E1049-85, *Standard Practices for Cycle Counting in Fatigue Analysis*, and pyLife rainflow documentation; no clause is supplied; clause text not checked against the licensed copy. [src/digitalmodel/fatigue/rainflow.py:L1-L10] |
| Allowable cycles | Router selects `get_sn_curve`; that function constructs the pyLife curve from the fatigue package's data. [src/digitalmodel/mooring_fatigue/workflow.py:L27-L28] [src/digitalmodel/fatigue/sn_curves.py:L96-L134] | DNV-RP-C203, *Fatigue Design of Offshore Steel Structures*, 2021 is the code label, while the data verification label is 2011; equality across editions is explicitly not established; clause text not checked against the licensed copy. [src/digitalmodel/fatigue/sn_library.py:L34-L35] [src/digitalmodel/fatigue/c203_editions.py:L19-L37] |
| Miner accumulation | `_evaluate_line` delegates to `miner_damage`: bin damage = applied cycles / allowable cycles, total = sum of bin damage. [src/digitalmodel/mooring_fatigue/workflow.py:L69-L71] [src/digitalmodel/fatigue/damage.py:L33-L39] | Shared implementation; the parametric response uses the same curve and damage functions and is not an independent reference. [src/digitalmodel/parametric/generate.py:L28-L45] |
| Life / margin | Life = design life / damage, required life = design life × DFF, margin = life / required life; nonpositive damage returns infinity; governing line maximises damage. [src/digitalmodel/mooring_fatigue/workflow.py:L37-L53] [src/digitalmodel/mooring_fatigue/workflow.py:L70-L83] [src/digitalmodel/mooring_fatigue/workflow.py:L230-L239] | Positive-damage bin calculations and history calculations follow the same life route. [src/digitalmodel/mooring_fatigue/workflow.py:L69-L74] |

The router's returned mapping contains curve/environment identifiers but no `Citation` sidecar or standard-edition field; the atlas separately stamps a standard-edition label. [src/digitalmodel/mooring_fatigue/workflow.py:L43-L57] [src/digitalmodel/parametric/refresh.py:L32-L35]

UNVERIFIED: equivalence of the implemented and data-verified standard editions, licensed clause compliance, and construction-specific fatigue qualification; issue pending (2).

## 4. Validated worked examples

Validated examples: 0; the listed regression cases compare internal identities, positivity, ordering or interpolation against the same fatigue implementation, rather than an independent physical reference. [tests/workflows/test_durable_workflows.py:L137-L168] [tests/parametric/test_atlas.py:L37-L59] [src/digitalmodel/parametric/generate.py:L28-L45]

| Case / kind | Input and expected quantity | Tolerance and basis | Existing test node ID | Offline result at the code revision |
|---|---|---|---|---|
| Metallic bins v1 / regression | `examples/workflows/mooring-fatigue/input.yml`; governing ID `wire-01`, 6 detail rows, 2 line IDs, positive stress in MPa and dimensionless damage; life = design life / damage. [tests/workflows/test_durable_workflows.py:L137-L168] | Counts/IDs exact; identity uses `pytest.approx` default relative 1e-6 and absolute 1e-12, a numerical regression tolerance. [tests/workflows/test_durable_workflows.py:L152-L159] | `tests/workflows/test_durable_workflows.py::test_workflow_registry[mooring-fatigue0]` | PASS before and after; registry v1 is offline. [docs/registry/workflows.yaml:L84-L96] |
| Metallic bins v2 / regression | `examples/workflows/mooring-fatigue-v2/input.yml`; same count/identity assertions with free-corrosion environment. [examples/workflows/mooring-fatigue-v2/input.yml:L9-L36] [tests/workflows/test_durable_workflows.py:L137-L168] | Same numerical tolerance as v1; no independent allowable-cycle reference. [tests/workflows/test_durable_workflows.py:L152-L159] | `tests/workflows/test_durable_workflows.py::test_workflow_registry[mooring-fatigue1]` | PASS before and after; registry v2 is experimental and offline. [docs/registry/workflows.yaml:L120-L131] |
| Atlas query / regression | `examples/workflows/mooring-fatigue-atlas-query/input.yml`; positive life query in years and an in-range flag, not a physical benchmark. [tests/workflows/test_durable_workflows.py:L227-L240] | Positive life in years and exact boolean checks; no published tolerance. [tests/workflows/test_durable_workflows.py:L227-L240] | `tests/workflows/test_durable_workflows.py::test_workflow_registry[mooring-fatigue-atlas-query]` | PASS before and after; offline registry route. [docs/registry/workflows.yaml:L132-L139] |

Execution record: `PYTHONPATH=src uv run --no-sources python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>`; the three nodes above plus `tests/parametric/test_atlas.py`, `tests/parametric/test_refresh.py` and `tests/parametric/test_catalog.py` run in a temporary copy because workflow tests write their registered outputs. [tests/workflows/test_durable_workflows.py:L55-L96]

Both BASE and AFTER recorded 58 run / 58 passed / 0 failed / 0 skipped / 0 NOT_RUN at the header code revision; source bytes are unchanged. Contract and routing checks separately recorded 497 run / 497 passed / 0 failed / 0 skipped / 0 NOT_RUN before and after the registry edit. All commands ran on the uncommitted tree; no solver was started.

## 5. QA gates

Gate selection follows AQG r0, section 10, Python calculations; G5 follows section 9 of that standard.

| Gate | Applicability | Module-specific checks |
|---|---|---|
| G0 | Applicable: inputs, units and citations | C01, C02, C03 |
| G1 | Applicable: Python import and tests; the router writes CSV, so no external-program load route is required. [src/digitalmodel/mooring_fatigue/workflow.py:L23-L58] | C04 |
| G2 | Applicable: independent arithmetic and same-engine regressions distinguished | C05, C06 |
| G3 | NOT_APPLICABLE: no dynamic solver launch in the router. [src/digitalmodel/mooring_fatigue/workflow.py:L23-L58] | None |
| G4 | NOT_APPLICABLE: no diffraction model or mesh in the evaluation path. [src/digitalmodel/mooring_fatigue/workflow.py:L61-L85] | None |
| G5 | Applicable: evidence and output traceability | C07, C08 |

These are executable acceptance criteria; a failed check documents a module limitation and does not certify the module for design.

| Check | Gate | Executable check and numeric pass criterion | Evidence / BASE and AFTER results |
|---|---|---|---|
| C01 | G0 | For each of `_positive_float` and `_first_float`, supply NaN and positive infinity; pass = 0 nonfinite scalar values accepted (count). [src/digitalmodel/mooring_fatigue/workflow.py:L210-L227] | Scratch audit before / after: FAIL, 4 accepted; issue pending (1). |
| C02 | G0 | `_stress_bins` with history `[0,20,0,20,0]` MPa and `n_repeats=0`, and a line containing both stress and tension bins: pass = 0 invalid repeat values / conflicting routes accepted (count); histories require integer `n_bins` ≥ 1 bin and finite repeats > 0 dimensionless. [src/digitalmodel/mooring_fatigue/workflow.py:L88-L107] [src/digitalmodel/mooring_fatigue/workflow.py:L137-L149] | Scratch audit before / after: FAIL, zero repeat and simultaneous routes accepted; issue pending (1). |
| C03 | G0 | Call `router` on two otherwise valid unnamed lines: pass = 0 duplicate `line_id` values (count); explicitly record selected curve, material, exposure and unit aliases before evaluation. [src/digitalmodel/mooring_fatigue/workflow.py:L67-L85] [src/digitalmodel/mooring_fatigue/workflow.py:L175-L186] | Scratch audit before / after: FAIL, 1 duplicate; issue pending (3); exposure contract issue pending (2). |
| C04 | G1 | Import workflow via the section-4 test command; pass = 58 passed tests, 0 failed tests and 0 collection errors (counts). [tests/workflows/test_durable_workflows.py:L55-L96] [tests/parametric/test_atlas.py:L37-L59] | BASE / AFTER PASS: 58 passed each. |
| C05 | G2 | `_evaluate_line` with a test curve returning 1000 allowable cycles, tension 2.0 kN, area 100.0 mm², count 5.0, life 25.0 years, DFF 3.0: independently calculate stress 20.0 MPa, damage 5/1000, life 5000.0 years and required life 75.0 years; pass = stress error 0 MPa, damage/margin error ≤ 1e-12 dimensionless, life error ≤ 1e-9 years. [src/digitalmodel/mooring_fatigue/workflow.py:L61-L85] [src/digitalmodel/mooring_fatigue/workflow.py:L111-L121] | Scratch arithmetic audit PASS before and after; arbitrary test curve, no standard-derived coefficients and no physical qualification. |
| C06 | G2 | Run `tests/parametric/test_atlas.py::test_grid_point_is_exact` and `tests/parametric/test_atlas.py::test_n_cycles_is_exactly_linear`: pass = dimensionless damage relative error ≤ 1e-9 at a knot and ≤ 1e-6 for off-grid cycle counts, respectively. [tests/parametric/test_atlas.py:L43-L59] | BASE / AFTER PASS; shared damage implementation, regression evidence only. |
| C07 | G5 | Read workflow detail/summary CSVs: pass = 6 detail rows, 2 unique line IDs, 0 nonpositive stresses (MPa) and 0 nonpositive damage values (dimensionless); life identity relative error ≤ 1e-6. [tests/workflows/test_durable_workflows.py:L137-L168] | BASE / AFTER PASS; file hashes and logs retained with the execution record. |
| C08 | G5 | Evaluate `_display_path` outside the repository and inspect router output: pass = 0 absolute metadata paths (count) and 1 revision-bound citation sidecar per calculation (count); retain input/output SHA-256 records with 0 digest mismatches (count). [src/digitalmodel/mooring_fatigue/workflow.py:L43-L57] [src/digitalmodel/mooring_fatigue/workflow.py:L281-L286] | Scratch audit before / after: FAIL, 1 absolute path and 0 citation sidecars; issue pending (2), issue pending (3). |

For reproducibility, C01–C03, C05 and C08 can be executed with the following solver-free Python recipe; put its outputs under a temporary directory.

```python
from pathlib import Path
from tempfile import TemporaryDirectory
import copy
import math
import numpy as np
from digitalmodel.mooring_fatigue import workflow as w

for fn in (w._positive_float, lambda d, k: w._first_float(d, (k,), k)):
    for value in (float("nan"), float("inf")):
        try:
            fn({"x": value}, "x")
            print("C01 FAIL: nonfinite value accepted")
        except ValueError:
            print("C01 PASS")
print("C02 repeat cycles", sum(b["cycles"] for b in w._stress_bins({
    "stress_history_MPa": [0., 20., 0., 20., 0.], "n_repeats": 0.})))
print("C02 conflicting routes", w._stress_bins({
    "stress_range_bins": [{"range": 1., "cycles": 1.}],
    "tension_range_bins": [{"range": 2., "cycles": 1.}]}))
line = {"area_mm2": 100., "tension_range_bins": [{"range_kN": 2., "n_cycles": 5.}]}
class TestCurve:
    def cycles(self, stress):
        assert np.all(stress == 20.)
        return np.full(len(stress), 1000.)
_, summary = w._evaluate_line(line, TestCurve(), 25., 3.)
assert abs(summary["total_damage"] - 5./1000.) <= 1e-12
assert abs(summary["fatigue_life_years"] - 5000.) <= 1e-9
assert abs(summary["dff_margin"] - 5000./75.) <= 1e-12
with TemporaryDirectory() as tmp:
    result = w.router({"mooring_fatigue": {
        "design_life_years": 25., "dff": 3., "output_dir": tmp,
        "lines": [line, copy.deepcopy(line)]}})["mooring_fatigue"]
    print("C03 duplicate IDs", len(result["lines"]) - len({r["line_id"] for r in result["lines"]}))
    print("C08 absolute path", Path(w._display_path(Path(tmp)/"result.csv")).is_absolute())
    print("C08 citation sidecars", int("citations" in result))
```

## 6. Known failure modes and pitfalls

sweep: 32 found

The sweep covers both package Python files, including conversions and normal defaults as potential pitfalls; rows distinguish unchecked behaviour from checked or internal conversions. [src/digitalmodel/mooring_fatigue/__init__.py:L1-L1] [src/digitalmodel/mooring_fatigue/workflow.py:L1-L286]

Exact searches used, followed by reading every matching definition and both complete files:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|\b0(\.0)?\b|1970|0\.05|5\s*%' src/digitalmodel/mooring_fatigue --glob '*.py'
rg -n 'int\(|float\(|dtype=float|\.get\(|1000|MPa|mm2|kN|years|open\(|write|cache' src/digitalmodel/mooring_fatigue --glob '*.py'
```

All rows were recorded on 2026-10-18 at the code revision in the header.

| ID | Site / symptom and cause | Detection or coverage boundary | Tracking |
|---|---|---|---|
| S01 | Missing settings becomes an empty dictionary; required life/factor checks then raise. [src/digitalmodel/mooring_fatigue/workflow.py:L24-L26] | C01 covers scalar guard only | issue pending (1) |
| S02 | Missing IDs fall back to the same `line` identifier, potentially hiding distinct lines. [src/digitalmodel/mooring_fatigue/workflow.py:L67-L68] | C03 | issue pending (3) |
| S03 | Multiple loading representations silently select stress bins, tension bins, stress history, then tension history. [src/digitalmodel/mooring_fatigue/workflow.py:L88-L108] | C02 | issue pending (1) |
| S04 | Tension-history float conversion assumes kN and mm²; locale separators raise and finite infinity has no local guard. [src/digitalmodel/mooring_fatigue/workflow.py:L101-L107] | C01 covers bin scalars only; no detection check exists for history conversion | issue pending (1) |
| S05 | Tension-bin aliases assume kN; multiplication/division assumes area in mm² and yields MPa. [src/digitalmodel/mooring_fatigue/workflow.py:L111-L121] | C05 checks explicit units; no detection check exists for ambiguous aliases | issue pending (2) |
| S06 | Stress aliases including bare `range` assume MPa; the first present alias wins. [src/digitalmodel/mooring_fatigue/workflow.py:L124-L132] | no detection check exists for contradictory aliases | issue pending (1) |
| S07 | Direct stress bins write an intentional NaN tension sentinel to the detail CSV. [src/digitalmodel/mooring_fatigue/workflow.py:L130-L134] | no detection check exists for sentinel documentation | issue pending (3) |
| S08 | History conversion uses `np.asarray(dtype=float)`; NaN handling is delegated, not validated here. [src/digitalmodel/mooring_fatigue/workflow.py:L137-L138] | no detection check exists for direct module histories | issue pending (1) |
| S09 | Histogram count defaults to 20 bins; `int` truncates a fractional request without a local integer/range check. [src/digitalmodel/mooring_fatigue/workflow.py:L139-L139] | no detection check exists for fractional or nonpositive bin counts | issue pending (1) |
| S10 | Repeat multiplier defaults to 1.0, with `n_repeats` preceding `n_histories`; negative, zero and nonfinite repeats are unchecked. [src/digitalmodel/mooring_fatigue/workflow.py:L140-L141] | C02 tests zero only; no detection check exists for negative/nonfinite repeats | issue pending (1) |
| S11 | Histogram stress values are converted from internal numeric rows to float. [src/digitalmodel/mooring_fatigue/workflow.py:L144-L144] | no detection check exists for this history output conversion | issue pending (1) |
| S12 | Histogram counts are converted from internal rows to float after repeat scaling. [src/digitalmodel/mooring_fatigue/workflow.py:L145-L145] | no detection check exists for this history output conversion | issue pending (1) |
| S13 | History bins carry intentional NaN tension sentinels. [src/digitalmodel/mooring_fatigue/workflow.py:L146-L146] | no detection check exists for history sentinel output | issue pending (3) |
| S14 | Internal damage-frame index is coerced with int and used to index the supplied bins. [src/digitalmodel/mooring_fatigue/workflow.py:L164-L164] | C07 checks row count on the supplied regression cases | issue pending (1) |
| S15 | Internal damage-frame stress is coerced to float, assuming MPa. [src/digitalmodel/mooring_fatigue/workflow.py:L166-L166] | C07 checks positive stress on the supplied regression cases | issue pending (1) |
| S16 | Internal damage-frame applied cycles are coerced to float. [src/digitalmodel/mooring_fatigue/workflow.py:L167-L167] | C07 checks supplied regression outputs; no detection check exists for conversion edge cases | issue pending (1) |
| S17 | Internal allowable cycles are coerced to float. [src/digitalmodel/mooring_fatigue/workflow.py:L168-L168] | C07 checks ordering on the supplied regression cases | issue pending (1) |
| S18 | Internal damage is coerced to float without an additional finite check. [src/digitalmodel/mooring_fatigue/workflow.py:L169-L169] | C07 checks positivity only; no detection check exists for nonfinite bin damage | issue pending (1) |
| S19 | Material defaults to CHAIN; an omitted material remains a valid label. [src/digitalmodel/mooring_fatigue/workflow.py:L175-L181] | C03 records the default; no detection check exists for missing material rejection | issue pending (2) |
| S20 | Missing or false curve settings becomes an empty dictionary. [src/digitalmodel/mooring_fatigue/workflow.py:L184-L185] | no detection check exists for empty curve settings | issue pending (2) |
| S21 | Missing curve identifier silently selects D. [src/digitalmodel/mooring_fatigue/workflow.py:L186-L186] | C03 records the default; no detection check exists for explicit-curve enforcement | issue pending (2) |
| S22 | Missing environment silently selects seawater_cp. [src/digitalmodel/mooring_fatigue/workflow.py:L186-L186] | C03 records the default; no detection check exists for explicit-environment enforcement | issue pending (2) |
| S23 | External scalar float conversion rejects ≤ 0 but accepts NaN/infinity; formatted thousands/decimal separators are not normalised. [src/digitalmodel/mooring_fatigue/workflow.py:L210-L217] | C01 | issue pending (1) |
| S24 | External bin/area/count float conversion chooses the first non-null alias and accepts NaN/infinity; separators are not normalised. [src/digitalmodel/mooring_fatigue/workflow.py:L220-L227] | C01 covers nonfinite values; no detection check exists for separator/alias conflicts | issue pending (1) |
| S25 | Nonpositive damage maps to infinite life, including zero/negative history-repeat damage. [src/digitalmodel/mooring_fatigue/workflow.py:L230-L233] | C02 exposes zero repeat; no detection check exists for negative damage | issue pending (1) |
| S26 | Any infinite numerator becomes positive infinity in margin calculation. [src/digitalmodel/mooring_fatigue/workflow.py:L236-L239] | C05 checks ordinary arithmetic only; no detection check exists for infinity sign | issue pending (1) |
| S27 | Output directory defaults to results; absolute directories are accepted and repeated stems reuse files. [src/digitalmodel/mooring_fatigue/workflow.py:L250-L254] | C08 checks metadata paths; no detection check exists for overwrite policy | issue pending (3) |
| S28 | Config directory falls through from config metadata to current working directory. [src/digitalmodel/mooring_fatigue/workflow.py:L257-L262] | no detection check exists for missing config metadata | issue pending (3) |
| S29 | Output stem falls back from input filename to basename, then mooring_fatigue. [src/digitalmodel/mooring_fatigue/workflow.py:L265-L268] | no detection check exists for filename collisions | issue pending (3) |
| S30 | Output writes replace files and use unspecified text encoding; empty rows raise rather than caching empty results. [src/digitalmodel/mooring_fatigue/workflow.py:L271-L278] | C07 checks normal CSV output; no detection check exists for interrupted writes/encoding | issue pending (3) |
| S31 | The sole caught exception suppresses relative-path failure and emits an absolute metadata path. [src/digitalmodel/mooring_fatigue/workflow.py:L281-L286] | C08 | issue pending (3) |
| S32 | Supplied counts are treated as the damage exposure for `design_life_years` without an exposure-duration input or annualisation; life is reported in years. [src/digitalmodel/mooring_fatigue/workflow.py:L69-L83] | C05 checks the implemented identity; no detection check exists for inconsistent exposure basis | issue pending (2) |

No `continue`, explicit `return None`, error-path empty-list/dictionary return, epoch default or empty-result cache write occurs in the package; `_write_csv` rejects empty rows. [src/digitalmodel/mooring_fatigue/workflow.py:L1-L286]

Historical incident #2165 concerns shared fatigue-curve parameters and edition traceability; `test_sn_curve_atlases_track_the_c203_sources` now checks that both curve and table sources participate in drift detection, but it is not an independent numerical benchmark of this module. [tests/parametric/test_refresh.py:L108-L116] [src/digitalmodel/fatigue/sn_curves.py:L1-L20] [src/digitalmodel/fatigue/c203_editions.py:L19-L37]

No detection check exists here for cross-edition equivalence or construction-specific qualification; issue pending (2). [src/digitalmodel/fatigue/c203_editions.py:L19-L26] [src/digitalmodel/mooring_fatigue/workflow.py:L61-L85]

## 7. Lessons log

| Date | Record | Lesson / evidence | Pack change | Shared-log entry |
|---|---|---|---|---|
| 2026-10-18 | Initial pack revision 0 | Atlas and direct workflow share `get_sn_curve` and `miner_damage`; agreement is regression evidence, not an independent benchmark. [src/digitalmodel/parametric/generate.py:L28-L45] [src/digitalmodel/mooring_fatigue/workflow.py:L69-L71] | Sections 3–5 separate arithmetic checks, regressions and physical qualification | Pending with issue pending (2) |
| 2026-10-18 | Initial pack revision 0 | Nonpositive history damage maps to infinite life, while repeat controls are unchecked. [src/digitalmodel/mooring_fatigue/workflow.py:L137-L149] [src/digitalmodel/mooring_fatigue/workflow.py:L230-L239] | Sections 5–6 add C02 and the complete package sweep | Pending with issue pending (1) |

## 8. Ownership

Review assignment for this draft: engineering calculations quality manager; deputy: engineering calculations deputy quality manager; producing discipline: engineering calculations; escalation within the family goes to its quality manager, and cross-family scope goes to the maintainers (AQG r0, section 11).

Clearance remains pending; the draft status in the header is not independent approval.

## 9. Dependencies and implementation inventory

The package imports NumPy, pandas, the shared fatigue damage/counting/curve functions, and standard-library CSV, math and path utilities. [src/digitalmodel/mooring_fatigue/workflow.py:L5-L15]

| Direction | Dependency / boundary | Evidence |
|---|---|---|
| Upstream | `fatigue.sn_curves.get_sn_curve`, which constructs a pyLife curve from shared fatigue data | [src/digitalmodel/fatigue/sn_curves.py:L23-L28] [src/digitalmodel/fatigue/sn_curves.py:L96-L134] |
| Upstream | `fatigue.rainflow.rainflow_count` / `stress_histogram`, pyLife four-point counting and histogram bins | [src/digitalmodel/fatigue/rainflow.py:L46-L75] [src/digitalmodel/fatigue/rainflow.py:L103-L127] |
| Upstream | `fatigue.damage.miner_damage`, applied cycles divided by allowable cycles | [src/digitalmodel/fatigue/damage.py:L33-L39] |
| Downstream | Engine basename dispatch | [src/digitalmodel/engine.py:L322-L325] |
| Related caller | Parametric response computes the same per-bin damage through the shared fatigue functions | [src/digitalmodel/parametric/generate.py:L28-L45] |
| Data / workflow | Public metallic-bin inputs, v1 stable and v2 experimental, plus an offline atlas query | [docs/registry/workflows.yaml:L84-L139] |

Solver version constraints are not part of this package's imports or evaluation route; no solver was launched for this pack. [src/digitalmodel/mooring_fatigue/workflow.py:L5-L15] [src/digitalmodel/mooring_fatigue/workflow.py:L23-L85]

### Map revision and counted family

Implementation-map source revision: `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` (`origin/codex/implmap-work`); declared data revision: `270c8759c374b67a7f032953e7fcdb14e0224461`; citations to `docs/registry/implementation-map.yaml` resolve at the source revision, all other repository citations at the header code revision.

The module's only map family is `sn-curves-and-miner-damage`: 4 in-package rows, ranked first; all 392 family rows, including those outside the package and under `scripts/`, are inventoried below. [docs/registry/implementation-map.yaml:L16807-L16812]

The exact case-sensitive Python `re` search signals from this map revision are reproduced below; no signal matches the package path `mooring_fatigue` case-insensitively, so none is excluded.

```python
search_signals = [
    'rainflow|miner|cycles_to_failure|log_a|log10.*(?:cycles|stress)|(?:stress|stress_range|delta_sigma).*\\*\\*.*\\bm\\b|damage_sum|thickness_correction|endurance_limit|sn_curve|SNcurve|sn_data|stress_life',
    'S.N.curve|fatigue.damage|C203|BS.?7608|RP.?2A|SNcurve|SN.?Curve',
    'thickness_correction|endurance_limit|n_cycles|sn_curve|stress_ranges',
]
# Use re.search(signal, source_line), without flags.
# Suppress a signal only if re.search(signal, "mooring_fatigue", re.I).
```

Signal source: [docs/registry/implementation-map.yaml:L16810-L16812]

| Definition hit at code pin | Classification | Evidence |
|---|---|---|
| `<module>` | Imports only; helper | [src/digitalmodel/mooring_fatigue/workflow.py:L13-L15] |
| `router` | Listed caller | [src/digitalmodel/mooring_fatigue/workflow.py:L23-L58] |
| `_evaluate_line` | Listed implementation row; delegates damage | [src/digitalmodel/mooring_fatigue/workflow.py:L61-L85] |
| `_history_bins` | Listed implementation row; delegates counting | [src/digitalmodel/mooring_fatigue/workflow.py:L137-L149] |
| `_curve_settings` | Listed settings helper | [src/digitalmodel/mooring_fatigue/workflow.py:L184-L186] |
| `_cycles` | Unlisted alias accessor; helper, not another implementation | [src/digitalmodel/mooring_fatigue/workflow.py:L200-L201] |

Counted-family result: 0 further in-package implementations; inventory and search meet the A8 threshold of at most 1. The module-level matches are imports and `_cycles` only delegates numeric alias selection. [src/digitalmodel/mooring_fatigue/workflow.py:L13-L15] [src/digitalmodel/mooring_fatigue/workflow.py:L200-L227]

The broader repository grep used the same three signals over `git ls-files '*.py'`, including `scripts/`; 321 files matched, including 30 under `scripts/`. This discovery count is not a second-implementation count. The inventory below preserves every mapped row, including callers, helpers and test fixtures, without promoting them to independent references.

### Complete family inventory

The relation column reproduces the map label; it does not assert independent validation.

| Row | Repository path | Symbol | Map relation | Map evidence |
|---|---|---|---|---|
| 1 | `docs/domains/examples/orcaflex_signal_analysis_example.py` | `example_6_custom_workflow` | independent | [docs/registry/implementation-map.yaml:L16814-L16823] |
| 2 | `docs/domains/examples/signal_analysis_usage_example.py` | `example_1_basic_rainflow_counting` | independent | [docs/registry/implementation-map.yaml:L16824-L16833] |
| 3 | `docs/domains/examples/signal_analysis_usage_example.py` | `example_4_fatigue_analysis` | independent | [docs/registry/implementation-map.yaml:L16834-L16843] |
| 4 | `docs/domains/examples/signal_analysis_usage_example.py` | `example_6_backward_compatibility` | independent | [docs/registry/implementation-map.yaml:L16844-L16853] |
| 5 | `docs/domains/examples/signal_analysis_usage_example.py` | `main` | independent | [docs/registry/implementation-map.yaml:L16854-L16867] |
| 6 | `examples/demos/demo_sn_library.py` | `main` | example | [docs/registry/implementation-map.yaml:L16868-L16879] |
| 7 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_sn_curves` | example | [docs/registry/implementation-map.yaml:L16880-L16889] |
| 8 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_rainflow_counting` | example | [docs/registry/implementation-map.yaml:L16890-L16899] |
| 9 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_damage_accumulation` | example | [docs/registry/implementation-map.yaml:L16900-L16909] |
| 10 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_frequency_domain_analysis` | example | [docs/registry/implementation-map.yaml:L16910-L16919] |
| 11 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_quick_analysis` | example | [docs/registry/implementation-map.yaml:L16920-L16929] |
| 12 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `main` | example | [docs/registry/implementation-map.yaml:L16930-L16945] |
| 13 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_cli.py` | `list_curves` | example | [docs/registry/implementation-map.yaml:L16946-L16955] |
| 14 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_cli.py` | `compare_curves` | example | [docs/registry/implementation-map.yaml:L16956-L16965] |
| 15 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_2_linear_log` | example | [docs/registry/implementation-map.yaml:L16966-L16975] |
| 16 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_3_with_scf` | example | [docs/registry/implementation-map.yaml:L16976-L16985] |
| 17 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_4_without_fatigue_limit` | example | [docs/registry/implementation-map.yaml:L16986-L16995] |
| 18 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_5_dnv_comparison` | example | [docs/registry/implementation-map.yaml:L16996-L17005] |
| 19 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_6_abs_comparison` | example | [docs/registry/implementation-map.yaml:L17006-L17015] |
| 20 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_7_reference_comparison` | example | [docs/registry/implementation-map.yaml:L17016-L17025] |
| 21 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_8_joint_type_comparison` | example | [docs/registry/implementation-map.yaml:L17026-L17035] |
| 22 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_1_basic_sn_curves` | example | [docs/registry/implementation-map.yaml:L17036-L17045] |
| 23 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_2_multislope_curves` | example | [docs/registry/implementation-map.yaml:L17046-L17055] |
| 24 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_3_time_domain_analysis` | example | [docs/registry/implementation-map.yaml:L17056-L17065] |
| 25 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_4_frequency_domain_analysis` | example | [docs/registry/implementation-map.yaml:L17066-L17075] |
| 26 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_5_high_stress_range_analysis` | example | [docs/registry/implementation-map.yaml:L17076-L17085] |
| 27 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_6_shear_data_analysis` | example | [docs/registry/implementation-map.yaml:L17086-L17095] |
| 28 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `run_all_examples` | example | [docs/registry/implementation-map.yaml:L17096-L17112] |
| 29 | `examples/domains/reservoir/reservoir_analysis_examples.py` | `example_well_log_analysis` | example | [docs/registry/implementation-map.yaml:L17113-L17122] |
| 30 | `examples/domains/reservoir/reservoir_analysis_examples.py` | `example_stratigraphic_correlation` | example | [docs/registry/implementation-map.yaml:L17123-L17132] |
| 31 | `examples/domains/reservoir/reservoir_analysis_examples.py` | `main` | example | [docs/registry/implementation-map.yaml:L17133-L17145] |
| 32 | `scripts/capabilities/build_onepagers.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L17146-L17154] |
| 33 | `scripts/python/digitalmodel/legacy/27_-_RainflowHalfCycles.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L17155-L17163] |
| 34 | `scripts/python/digitalmodel/tools/check_process_fatigue_single.py` | `process_single_test_file` | independent | [docs/registry/implementation-map.yaml:L17164-L17173] |
| 35 | `scripts/python/digitalmodel/tools/migrate_yaml_configs.py` | `YAMLConfigMigrator._migrate_analysis` | independent | [docs/registry/implementation-map.yaml:L17174-L17183] |
| 36 | `scripts/python/digitalmodel/tools/migrate_yaml_configs.py` | `YAMLConfigMigrator._migrate_time_series_components` | independent | [docs/registry/implementation-map.yaml:L17184-L17193] |
| 37 | `scripts/python/digitalmodel/tools/process_fatigue_data.py` | `process_fatigue_files` | independent | [docs/registry/implementation-map.yaml:L17194-L17203] |
| 38 | `scripts/python/digitalmodel/tools/process_fatigue_final.py` | `process_single_test_file` | independent | [docs/registry/implementation-map.yaml:L17204-L17213] |
| 39 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `process_fatigue_file` | independent | [docs/registry/implementation-map.yaml:L17214-L17223] |
| 40 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `process_sample_file` | independent | [docs/registry/implementation-map.yaml:L17224-L17235] |
| 41 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `batch_process_all_files` | independent | [docs/registry/implementation-map.yaml:L17236-L17246] |
| 42 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `rainflow_counting` | independent | [docs/registry/implementation-map.yaml:L17247-L17256] |
| 43 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `process_fatigue_file` | independent | [docs/registry/implementation-map.yaml:L17257-L17268] |
| 44 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `batch_process` | independent | [docs/registry/implementation-map.yaml:L17269-L17280] |
| 45 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v2.py` | `process_single_test_file` | independent | [docs/registry/implementation-map.yaml:L17281-L17290] |
| 46 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v3.py` | `process_single_test_file` | independent | [docs/registry/implementation-map.yaml:L17291-L17300] |
| 47 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v4.py` | `process_single_test_file` | independent | [docs/registry/implementation-map.yaml:L17301-L17310] |
| 48 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v5.py` | `process_single_test_file` | independent | [docs/registry/implementation-map.yaml:L17311-L17320] |
| 49 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v6.py` | `process_single_test_file` | independent | [docs/registry/implementation-map.yaml:L17321-L17330] |
| 50 | `scripts/riser_database/build_tables.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L17331-L17339] |
| 51 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `SNcurve.from_dnv` | independent | [docs/registry/implementation-map.yaml:L17340-L17349] |
| 52 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.calculate_cycles_to_failure` | independent | [docs/registry/implementation-map.yaml:L17350-L17359] |
| 53 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.calculate_fatigue_damage` | independent | [docs/registry/implementation-map.yaml:L17360-L17371] |
| 54 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.generate_sn_curve_apdl` | independent | [docs/registry/implementation-map.yaml:L17372-L17381] |
| 55 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.create_damage_summary_df` | independent | [docs/registry/implementation-map.yaml:L17382-L17391] |
| 56 | `src/digitalmodel/benchmarks/inventory.py` | `build_model_inventory` | independent | [docs/registry/implementation-map.yaml:L17392-L17401] |
| 57 | `src/digitalmodel/drilling_riser/global_model/fatigue_channels.py` | `half_cycles` | independent | [docs/registry/implementation-map.yaml:L17402-L17411] |
| 58 | `src/digitalmodel/drilling_riser/global_model/fatigue_channels.py` | `extract` | independent | [docs/registry/implementation-map.yaml:L17412-L17423] |
| 59 | `src/digitalmodel/engine.py` | `engine` | caller | [docs/registry/implementation-map.yaml:L17424-L17441] |
| 60 | `src/digitalmodel/fatg_spectral_fatigue/workflow.py` | `router` | independent | [docs/registry/implementation-map.yaml:L17442-L17454] |
| 61 | `src/digitalmodel/fatg_spectral_fatigue/workflow.py` | `_sn_curve` | independent | [docs/registry/implementation-map.yaml:L17455-L17464] |
| 62 | `src/digitalmodel/fatigue/assessment.py` | `_case_histogram` | independent | [docs/registry/implementation-map.yaml:L17465-L17474] |
| 63 | `src/digitalmodel/fatigue/assessment.py` | `assess_fatigue` | independent | [docs/registry/implementation-map.yaml:L17475-L17488] |
| 64 | `src/digitalmodel/fatigue/counting_contract.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L17489-L17497] |
| 65 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_pylife_4pt` | independent | [docs/registry/implementation-map.yaml:L17498-L17507] |
| 66 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_pypi_rainflow` | independent | [docs/registry/implementation-map.yaml:L17508-L17517] |
| 67 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_sigproc_astm` | independent | [docs/registry/implementation-map.yaml:L17518-L17527] |
| 68 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_calm_buoy` | independent | [docs/registry/implementation-map.yaml:L17528-L17537] |
| 69 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_fapps_counting` | independent | [docs/registry/implementation-map.yaml:L17538-L17547] |
| 70 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_fapps_counter` | independent | [docs/registry/implementation-map.yaml:L17548-L17557] |
| 71 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_struct_fatigue` | independent | [docs/registry/implementation-map.yaml:L17558-L17567] |
| 72 | `src/digitalmodel/fatigue/counting_contract.py` | `_is_counting_name` | independent | [docs/registry/implementation-map.yaml:L17568-L17577] |
| 73 | `src/digitalmodel/fatigue/damage.py` | `miner_damage` | independent | [docs/registry/implementation-map.yaml:L17578-L17587] |
| 74 | `src/digitalmodel/fatigue/damage.py` | `design_life_check` | independent | [docs/registry/implementation-map.yaml:L17588-L17599] |
| 75 | `src/digitalmodel/fatigue/environmental_correction.py` | `environment_correction` | independent | [docs/registry/implementation-map.yaml:L17600-L17609] |
| 76 | `src/digitalmodel/fatigue/environmental_correction.py` | `apply_environment_to_sn` | independent | [docs/registry/implementation-map.yaml:L17610-L17621] |
| 77 | `src/digitalmodel/fatigue/fatg.py` | `fatg_sea_state_damage` | independent | [docs/registry/implementation-map.yaml:L17622-L17631] |
| 78 | `src/digitalmodel/fatigue/fatg.py` | `fatg_annual_damage` | independent | [docs/registry/implementation-map.yaml:L17632-L17643] |
| 79 | `src/digitalmodel/fatigue/fatigue_reporting.py` | `_sec_fatigue_locations` | independent | [docs/registry/implementation-map.yaml:L17644-L17653] |
| 80 | `src/digitalmodel/fatigue/rainflow.py` | `fatigue_life` | independent | [docs/registry/implementation-map.yaml:L17654-L17666] |
| 81 | `src/digitalmodel/fatigue/sn_curves.py` | `_compute_sd` | independent | [docs/registry/implementation-map.yaml:L17667-L17676] |
| 82 | `src/digitalmodel/fatigue/sn_curves.py` | `_build_curve_dict` | caller | [docs/registry/implementation-map.yaml:L17677-L17688] |
| 83 | `src/digitalmodel/fatigue/sn_curves.py` | `get_sn_curve` | caller | [docs/registry/implementation-map.yaml:L17689-L17699] |
| 84 | `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.knee_stress` | wrapper | [docs/registry/implementation-map.yaml:L17700-L17711] |
| 85 | `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.switch_stress` | wrapper | [docs/registry/implementation-map.yaml:L17712-L17723] |
| 86 | `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.cycles` | wrapper | [docs/registry/implementation-map.yaml:L17724-L17735] |
| 87 | `src/digitalmodel/fatigue/sn_library.py` | `_knee_stress` | independent | [docs/registry/implementation-map.yaml:L17736-L17745] |
| 88 | `src/digitalmodel/fatigue/sn_library.py` | `_switch_stress` | caller | [docs/registry/implementation-map.yaml:L17746-L17756] |
| 89 | `src/digitalmodel/fatigue/sn_library.py` | `_bilinear_cycles` | independent | [docs/registry/implementation-map.yaml:L17757-L17766] |
| 90 | `src/digitalmodel/fatigue/sn_library.py` | `_sd` | independent | [docs/registry/implementation-map.yaml:L17767-L17776] |
| 91 | `src/digitalmodel/fatigue/sn_library.py` | `_build_dnv_curves` | independent | [docs/registry/implementation-map.yaml:L17777-L17786] |
| 92 | `src/digitalmodel/fatigue/sn_library.py` | `_build_api_curves` | independent | [docs/registry/implementation-map.yaml:L17787-L17796] |
| 93 | `src/digitalmodel/fatigue/sn_library.py` | `_build_bs7608_curves` | independent | [docs/registry/implementation-map.yaml:L17797-L17808] |
| 94 | `src/digitalmodel/fatigue/sn_library.py` | `_build_iiw_curves` | independent | [docs/registry/implementation-map.yaml:L17809-L17819] |
| 95 | `src/digitalmodel/fatigue/sn_library.py` | `_build_eurocode3_curves` | independent | [docs/registry/implementation-map.yaml:L17820-L17830] |
| 96 | `src/digitalmodel/fatigue/sn_library.py` | `_build_norsok_curves` | caller | [docs/registry/implementation-map.yaml:L17831-L17841] |
| 97 | `src/digitalmodel/fatigue/sn_library.py` | `_build_aws_curves` | independent | [docs/registry/implementation-map.yaml:L17842-L17851] |
| 98 | `src/digitalmodel/fatigue/sn_library.py` | `_build_asme_curves` | independent | [docs/registry/implementation-map.yaml:L17852-L17861] |
| 99 | `src/digitalmodel/fatigue/sn_library.py` | `_build_en13445_curves` | independent | [docs/registry/implementation-map.yaml:L17862-L17872] |
| 100 | `src/digitalmodel/fatigue/sn_library.py` | `_build_pd5500_curves` | caller | [docs/registry/implementation-map.yaml:L17873-L17883] |
| 101 | `src/digitalmodel/fatigue/sn_library.py` | `_build_abs_curves` | independent | [docs/registry/implementation-map.yaml:L17884-L17893] |
| 102 | `src/digitalmodel/fatigue/sn_library.py` | `_build_iso19902_curves` | independent | [docs/registry/implementation-map.yaml:L17894-L17904] |
| 103 | `src/digitalmodel/fatigue/sn_library.py` | `_build_csa_curves` | independent | [docs/registry/implementation-map.yaml:L17905-L17914] |
| 104 | `src/digitalmodel/fatigue/sn_library.py` | `_build_hse_curves` | independent | [docs/registry/implementation-map.yaml:L17915-L17925] |
| 105 | `src/digitalmodel/fatigue/sn_library.py` | `_build_doe_curves` | caller | [docs/registry/implementation-map.yaml:L17926-L17936] |
| 106 | `src/digitalmodel/fatigue/sn_library.py` | `_build_gl_curves` | independent | [docs/registry/implementation-map.yaml:L17937-L17946] |
| 107 | `src/digitalmodel/fatigue/sn_library.py` | `_build_jis_curves` | independent | [docs/registry/implementation-map.yaml:L17947-L17956] |
| 108 | `src/digitalmodel/fatigue/sn_library.py` | `_ensure_loaded` | independent | [docs/registry/implementation-map.yaml:L17957-L17966] |
| 109 | `src/digitalmodel/fatigue/sn_library.py` | `summary_table` | caller | [docs/registry/implementation-map.yaml:L17967-L17978] |
| 110 | `src/digitalmodel/fatigue/sn_library_api.py` | `_record_to_curve_info` | independent | [docs/registry/implementation-map.yaml:L17979-L17988] |
| 111 | `src/digitalmodel/fatigue/sn_library_api.py` | `_record_to_sncurve` | independent | [docs/registry/implementation-map.yaml:L17989-L17998] |
| 112 | `src/digitalmodel/fatigue/sn_library_api.py` | `get_curve` | wrapper | [docs/registry/implementation-map.yaml:L17999-L18010] |
| 113 | `src/digitalmodel/fatigue/sn_library_api.py` | `calculate_endurance` | wrapper | [docs/registry/implementation-map.yaml:L18011-L18023] |
| 114 | `src/digitalmodel/fatigue/spectral_fatigue.py` | `wirsching_light_damage` | independent | [docs/registry/implementation-map.yaml:L18024-L18033] |
| 115 | `src/digitalmodel/fatigue/spectral_fatigue.py` | `benasciutti_tovo_damage` | independent | [docs/registry/implementation-map.yaml:L18034-L18043] |
| 116 | `src/digitalmodel/fatigue/weld_fatigue_quickcheck.py` | `weld_fatigue_quickcheck` | independent | [docs/registry/implementation-map.yaml:L18044-L18054] |
| 117 | `src/digitalmodel/fatigue/weld_fatigue_quickcheck.py` | `main` | independent | [docs/registry/implementation-map.yaml:L18055-L18064] |
| 118 | `src/digitalmodel/fatigue/woehler_fitting.py` | `generate_test_data` | independent | [docs/registry/implementation-map.yaml:L18065-L18074] |
| 119 | `src/digitalmodel/fatigue/woehler_fitting.py` | `_fallback_log_regression` | independent | [docs/registry/implementation-map.yaml:L18075-L18084] |
| 120 | `src/digitalmodel/fatigue/workflow.py` | `router` | caller | [docs/registry/implementation-map.yaml:L18085-L18102] |
| 121 | `src/digitalmodel/fatigue/workflow.py` | `_curve_ids` | independent | [docs/registry/implementation-map.yaml:L18103-L18112] |
| 122 | `src/digitalmodel/fatigue/workflow.py` | `_stress_ranges` | independent | [docs/registry/implementation-map.yaml:L18113-L18122] |
| 123 | `src/digitalmodel/fatigue/workflow.py` | `_curve_summary` | independent | [docs/registry/implementation-map.yaml:L18123-L18132] |
| 124 | `src/digitalmodel/fatigue/workflow.py` | `_curve_csv_path` | independent | [docs/registry/implementation-map.yaml:L18133-L18144] |
| 125 | `src/digitalmodel/fatigue/workflow.py` | `_input_stem` | independent | [docs/registry/implementation-map.yaml:L18145-L18154] |
| 126 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.router` | caller | [docs/registry/implementation-map.yaml:L18155-L18167] |
| 127 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.damage_from_sn_data` | independent | [docs/registry/implementation-map.yaml:L18168-L18177] |
| 128 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.damage_from_rainflow_cycles` | independent | [docs/registry/implementation-map.yaml:L18178-L18187] |
| 129 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.damage_from_timetrace` | independent | [docs/registry/implementation-map.yaml:L18188-L18200] |
| 130 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.get_rainflow_from_timetrace` | independent | [docs/registry/implementation-map.yaml:L18201-L18210] |
| 131 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | independent | [docs/registry/implementation-map.yaml:L18211-L18220] |
| 132 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.router` | wrapper | [docs/registry/implementation-map.yaml:L18221-L18232] |
| 133 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.get_lcf_timetrace_rainflow_and_fatigue_damage` | independent | [docs/registry/implementation-map.yaml:L18233-L18244] |
| 134 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.get_fatigue_damage_output_df_for_timetrace` | wrapper | [docs/registry/implementation-map.yaml:L18245-L18256] |
| 135 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.get_fatigue_df_definition_for_timetrace` | independent | [docs/registry/implementation-map.yaml:L18257-L18266] |
| 136 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | independent | [docs/registry/implementation-map.yaml:L18267-L18276] |
| 137 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `SNcurve.cycles_to_failure` | independent | [docs/registry/implementation-map.yaml:L18277-L18286] |
| 138 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `RainflowFatigue.count_cycles` | independent | [docs/registry/implementation-map.yaml:L18287-L18296] |
| 139 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `MinersRuleDamage.compute` | independent | [docs/registry/implementation-map.yaml:L18297-L18306] |
| 140 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `ScatterDiagramFatigue.compute` | independent | [docs/registry/implementation-map.yaml:L18307-L18316] |
| 141 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `compute_fatigue_life` | independent | [docs/registry/implementation-map.yaml:L18317-L18326] |
| 142 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.add_well_analysis` | independent | [docs/registry/implementation-map.yaml:L18327-L18336] |
| 143 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.set_well_order` | independent | [docs/registry/implementation-map.yaml:L18337-L18346] |
| 144 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.create_flattened_section` | independent | [docs/registry/implementation-map.yaml:L18347-L18356] |
| 145 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `WellCorrelation.add_well_data` | independent | [docs/registry/implementation-map.yaml:L18357-L18366] |
| 146 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `CoreAnalysis.correlate_core_to_logs` | independent | [docs/registry/implementation-map.yaml:L18367-L18376] |
| 147 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `ReservoirAnalysis.add_well_analysis` | independent | [docs/registry/implementation-map.yaml:L18377-L18386] |
| 148 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `ReservoirAnalysis.run_comprehensive_analysis` | independent | [docs/registry/implementation-map.yaml:L18387-L18396] |
| 149 | `src/digitalmodel/mooring_fatigue/workflow.py` | `router` | caller | [docs/registry/implementation-map.yaml:L18397-L18410] |
| 150 | `src/digitalmodel/mooring_fatigue/workflow.py` | `_evaluate_line` | independent | [docs/registry/implementation-map.yaml:L18411-L18421] |
| 151 | `src/digitalmodel/mooring_fatigue/workflow.py` | `_history_bins` | independent | [docs/registry/implementation-map.yaml:L18422-L18431] |
| 152 | `src/digitalmodel/mooring_fatigue/workflow.py` | `_curve_settings` | independent | [docs/registry/implementation-map.yaml:L18432-L18441] |
| 153 | `src/digitalmodel/parametric/generate.py` | `_mooring_fatigue_damage` | independent | [docs/registry/implementation-map.yaml:L18442-L18452] |
| 154 | `src/digitalmodel/parametric/generate.py` | `_free_span_utilisation` | independent | [docs/registry/implementation-map.yaml:L18453-L18462] |
| 155 | `src/digitalmodel/parametric/generate.py` | `_span_rectification_utilisation` | independent | [docs/registry/implementation-map.yaml:L18463-L18472] |
| 156 | `src/digitalmodel/parametric/refresh.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L18473-L18481] |
| 157 | `src/digitalmodel/rao_spectral_fatigue/workflow.py` | `router` | caller | [docs/registry/implementation-map.yaml:L18482-L18493] |
| 158 | `src/digitalmodel/riser_fatigue/touchdown.py` | `assess_touchdown_fatigue` | independent | [docs/registry/implementation-map.yaml:L18494-L18503] |
| 159 | `src/digitalmodel/riser_fatigue/workflow.py` | `router` | caller | [docs/registry/implementation-map.yaml:L18504-L18515] |
| 160 | `src/digitalmodel/riser_fatigue/workflow.py` | `_viv_damage` | independent | [docs/registry/implementation-map.yaml:L18516-L18526] |
| 161 | `src/digitalmodel/riser_fatigue/workflow.py` | `_curve_settings` | independent | [docs/registry/implementation-map.yaml:L18527-L18536] |
| 162 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `TimeSeriesComponentsAdapter.get_rainflow_count_from_time_series` | independent | [docs/registry/implementation-map.yaml:L18537-L18546] |
| 163 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `FatigueAnalysisAdapter.get_rainflow_from_timetrace` | independent | [docs/registry/implementation-map.yaml:L18547-L18556] |
| 164 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `FatigueAnalysisAdapter.damage_from_rainflow_cycles` | independent | [docs/registry/implementation-map.yaml:L18557-L18566] |
| 165 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `OrcaFlexAdapter.RainflowHalfCycles` | independent | [docs/registry/implementation-map.yaml:L18567-L18576] |
| 166 | `src/digitalmodel/signal_processing/signal_analysis/cli.py` | `rainflow_cmd` | independent | [docs/registry/implementation-map.yaml:L18577-L18586] |
| 167 | `src/digitalmodel/signal_processing/signal_analysis/core/rainflow.py` | `RainflowCounter.count_cycles` | independent | [docs/registry/implementation-map.yaml:L18587-L18596] |
| 168 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/curves.py` | `SNCurve._bilinear` | independent | [docs/registry/implementation-map.yaml:L18597-L18606] |
| 169 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/curves.py` | `SNCurve.from_test_data` | independent | [docs/registry/implementation-map.yaml:L18607-L18616] |
| 170 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator.calculate_damage` | independent | [docs/registry/implementation-map.yaml:L18617-L18632] |
| 171 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._miners_rule` | independent | [docs/registry/implementation-map.yaml:L18633-L18644] |
| 172 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._modified_miners_rule` | independent | [docs/registry/implementation-map.yaml:L18645-L18655] |
| 173 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._nonlinear_damage` | independent | [docs/registry/implementation-map.yaml:L18656-L18666] |
| 174 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._calculate_allowable_cycles` | independent | [docs/registry/implementation-map.yaml:L18667-L18676] |
| 175 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._apply_mean_stress_correction` | independent | [docs/registry/implementation-map.yaml:L18677-L18686] |
| 176 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._estimate_cycles_to_failure` | independent | [docs/registry/implementation-map.yaml:L18687-L18696] |
| 177 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator.calculate_equivalent_stress` | independent | [docs/registry/implementation-map.yaml:L18697-L18706] |
| 178 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `create_parser` | independent | [docs/registry/implementation-map.yaml:L18707-L18716] |
| 179 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `build_config_from_args` | independent | [docs/registry/implementation-map.yaml:L18717-L18726] |
| 180 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `main` | independent | [docs/registry/implementation-map.yaml:L18727-L18739] |
| 181 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._analyze_column` | independent | [docs/registry/implementation-map.yaml:L18740-L18749] |
| 182 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._generate_outputs` | independent | [docs/registry/implementation-map.yaml:L18750-L18759] |
| 183 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._generate_summary` | independent | [docs/registry/implementation-map.yaml:L18760-L18769] |
| 184 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/error_handling.py` | `SafeAnalyzer.safe_rainflow_analysis` | independent | [docs/registry/implementation-map.yaml:L18770-L18779] |
| 185 | `src/digitalmodel/signal_processing/time_series/time_series_analysis.py` | `TimeSeriesAnalysis.router` | independent | [docs/registry/implementation-map.yaml:L18780-L18789] |
| 186 | `src/digitalmodel/signal_processing/time_series/time_series_components.py` | `TimeSeriesComponents.count_cycles` | independent | [docs/registry/implementation-map.yaml:L18790-L18799] |
| 187 | `src/digitalmodel/solvers/orcaflex/opp_time_series.py` | `OPPTimeSeries.get_histogram_object_from_orcaflex_run` | independent | [docs/registry/implementation-map.yaml:L18800-L18809] |
| 188 | `src/digitalmodel/solvers/orcaflex/opp_time_series.py` | `OPPTimeSeries.get_detailed_histogram_df_columns` | independent | [docs/registry/implementation-map.yaml:L18810-L18819] |
| 189 | `src/digitalmodel/solvers/orcaflex/opp_time_series_v2.py` | `OPPTimeSeriesV2.process_histograms` | independent | [docs/registry/implementation-map.yaml:L18820-L18829] |
| 190 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.get_histogram_object_from_orcaflex_run` | independent | [docs/registry/implementation-map.yaml:L18830-L18839] |
| 191 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.get_detailed_histogram_df_columns` | copy | [docs/registry/implementation-map.yaml:L18840-L18849] |
| 192 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._perform_fatigue_analysis` | independent | [docs/registry/implementation-map.yaml:L18850-L18861] |
| 193 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._parallel_fatigue_analysis` | independent | [docs/registry/implementation-map.yaml:L18862-L18871] |
| 194 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._analyze_single_trace` | independent | [docs/registry/implementation-map.yaml:L18872-L18881] |
| 195 | `src/digitalmodel/spectral_fatigue/workflow.py` | `router` | caller | [docs/registry/implementation-map.yaml:L18882-L18893] |
| 196 | `src/digitalmodel/spectral_fatigue/workflow.py` | `_sn_curve` | independent | [docs/registry/implementation-map.yaml:L18894-L18903] |
| 197 | `src/digitalmodel/structural/fatigue/__init__.py` | `quick_fatigue_analysis` | caller | [docs/registry/implementation-map.yaml:L18904-L18917] |
| 198 | `src/digitalmodel/structural/fatigue/__init__.py` | `get_module_info` | independent | [docs/registry/implementation-map.yaml:L18918-L18927] |
| 199 | `src/digitalmodel/structural/fatigue/analysis.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L18928-L18936] |
| 200 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._setup_sn_curve` | wrapper | [docs/registry/implementation-map.yaml:L18937-L18947] |
| 201 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._setup_damage_calculator` | independent | [docs/registry/implementation-map.yaml:L18948-L18957] |
| 202 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._setup_rainflow_counter` | independent | [docs/registry/implementation-map.yaml:L18958-L18967] |
| 203 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_time_series` | independent | [docs/registry/implementation-map.yaml:L18968-L18979] |
| 204 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_psd` | independent | [docs/registry/implementation-map.yaml:L18980-L18989] |
| 205 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.calculate_high_stress_range_life` | independent | [docs/registry/implementation-map.yaml:L18990-L18999] |
| 206 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_shear_data` | independent | [docs/registry/implementation-map.yaml:L19000-L19009] |
| 207 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._perform_engineering_validation` | independent | [docs/registry/implementation-map.yaml:L19010-L19019] |
| 208 | `src/digitalmodel/structural/fatigue/analysis.py` | `quick_time_domain_analysis` | independent | [docs/registry/implementation-map.yaml:L19020-L19029] |
| 209 | `src/digitalmodel/structural/fatigue/analysis.py` | `quick_frequency_domain_analysis` | independent | [docs/registry/implementation-map.yaml:L19030-L19039] |
| 210 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L19040-L19048] |
| 211 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `LinearDamageAccumulation.calculate_damage` | independent | [docs/registry/implementation-map.yaml:L19049-L19060] |
| 212 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `LinearDamageAccumulation._apply_mean_stress_correction` | independent | [docs/registry/implementation-map.yaml:L19061-L19070] |
| 213 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `ModifiedMinersRule.calculate_damage` | independent | [docs/registry/implementation-map.yaml:L19071-L19080] |
| 214 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation.calculate_damage` | wrapper | [docs/registry/implementation-map.yaml:L19081-L19094] |
| 215 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation._marco_starkey_damage` | independent | [docs/registry/implementation-map.yaml:L19095-L19104] |
| 216 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation._corten_dolan_damage` | independent | [docs/registry/implementation-map.yaml:L19105-L19114] |
| 217 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation._double_linear_damage` | independent | [docs/registry/implementation-map.yaml:L19115-L19124] |
| 218 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `compare_damage_methods` | independent | [docs/registry/implementation-map.yaml:L19125-L19134] |
| 219 | `src/digitalmodel/structural/fatigue/design_code_report.py` | `_build_curve` | independent | [docs/registry/implementation-map.yaml:L19135-L19144] |
| 220 | `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `failure_probability` | independent | [docs/registry/implementation-map.yaml:L19145-L19154] |
| 221 | `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `sensitivity_analysis` | independent | [docs/registry/implementation-map.yaml:L19155-L19164] |
| 222 | `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `monte_carlo_fatigue` | independent | [docs/registry/implementation-map.yaml:L19165-L19174] |
| 223 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L19175-L19183] |
| 224 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `NarrowBandMethod.calculate_damage_rate` | independent | [docs/registry/implementation-map.yaml:L19184-L19193] |
| 225 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `DirlikMethod.calculate_damage_rate` | independent | [docs/registry/implementation-map.yaml:L19194-L19203] |
| 226 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `TovoBenasciuttiMethod.calculate_damage_rate` | independent | [docs/registry/implementation-map.yaml:L19204-L19213] |
| 227 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `SingleMomentMethod.calculate_damage_rate` | independent | [docs/registry/implementation-map.yaml:L19214-L19223] |
| 228 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `compare_frequency_methods` | independent | [docs/registry/implementation-map.yaml:L19224-L19233] |
| 229 | `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `run_sweep` | independent | [docs/registry/implementation-map.yaml:L19234-L19243] |
| 230 | `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `compute_sensitivity._single_damage` | independent | [docs/registry/implementation-map.yaml:L19244-L19253] |
| 231 | `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `_build_scatter_json` | independent | [docs/registry/implementation-map.yaml:L19254-L19263] |
| 232 | `src/digitalmodel/structural/fatigue/rainflow.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L19264-L19272] |
| 233 | `src/digitalmodel/structural/fatigue/rainflow.py` | `RainflowCounter.count_cycles` | caller | [docs/registry/implementation-map.yaml:L19273-L19284] |
| 234 | `src/digitalmodel/structural/fatigue/rainflow.py` | `RainflowCounter._rainflow_count` | independent | [docs/registry/implementation-map.yaml:L19285-L19294] |
| 235 | `src/digitalmodel/structural/fatigue/rainflow.py` | `RainflowCounter.extract_cycles_with_means` | caller | [docs/registry/implementation-map.yaml:L19295-L19306] |
| 236 | `src/digitalmodel/structural/fatigue/rainflow.py` | `rainflow_count` | independent | [docs/registry/implementation-map.yaml:L19307-L19316] |
| 237 | `src/digitalmodel/structural/fatigue/rainflow.py` | `rainflow_with_means` | independent | [docs/registry/implementation-map.yaml:L19317-L19326] |
| 238 | `src/digitalmodel/structural/fatigue/scatter_fatigue.py` | `scatter_fatigue_damage` | independent | [docs/registry/implementation-map.yaml:L19327-L19336] |
| 239 | `src/digitalmodel/structural/fatigue/skill.py` | `fatigue_assessment` | independent | [docs/registry/implementation-map.yaml:L19337-L19348] |
| 240 | `src/digitalmodel/structural/fatigue/sn_curve_plotter.py` | `main` | independent | [docs/registry/implementation-map.yaml:L19349-L19358] |
| 241 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L19359-L19367] |
| 242 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves._ensure_loaded` | independent | [docs/registry/implementation-map.yaml:L19368-L19377] |
| 243 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.register_custom_curve` | independent | [docs/registry/implementation-map.yaml:L19378-L19387] |
| 244 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.get_dnv_c203_curve` | independent | [docs/registry/implementation-map.yaml:L19388-L19397] |
| 245 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.get_curve` | independent | [docs/registry/implementation-map.yaml:L19398-L19407] |
| 246 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.get_multislope_curve` | independent | [docs/registry/implementation-map.yaml:L19408-L19417] |
| 247 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `ThicknessCorrection.apply_thickness_effect` | independent | [docs/registry/implementation-map.yaml:L19418-L19427] |
| 248 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `SNDataFitting.fit_power_law` | independent | [docs/registry/implementation-map.yaml:L19428-L19437] |
| 249 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `get_dnv_curve` | independent | [docs/registry/implementation-map.yaml:L19438-L19447] |
| 250 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `get_api_curve` | independent | [docs/registry/implementation-map.yaml:L19448-L19457] |
| 251 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `get_bs_curve` | independent | [docs/registry/implementation-map.yaml:L19458-L19467] |
| 252 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `_dnv_curves_from_c203` | independent | [docs/registry/implementation-map.yaml:L19468-L19477] |
| 253 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_dnv_seawater_cp` | independent | [docs/registry/implementation-map.yaml:L19478-L19487] |
| 254 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_dnv_seawater_free` | independent | [docs/registry/implementation-map.yaml:L19488-L19497] |
| 255 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_chain_seawater_cp` | independent | [docs/registry/implementation-map.yaml:L19498-L19507] |
| 256 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `pipeline_girth_weld` | caller | [docs/registry/implementation-map.yaml:L19508-L19520] |
| 257 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `scr_touchdown` | caller | [docs/registry/implementation-map.yaml:L19521-L19533] |
| 258 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `mooring_chain` | caller | [docs/registry/implementation-map.yaml:L19534-L19546] |
| 259 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_compute_example` | independent | [docs/registry/implementation-map.yaml:L19547-L19556] |
| 260 | `src/digitalmodel/structural/fatigue_apps/__main__.py` | `run_analysis` | independent | [docs/registry/implementation-map.yaml:L19557-L19568] |
| 261 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `SNCurve._load_sn_parameters` | independent | [docs/registry/implementation-map.yaml:L19569-L19578] |
| 262 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `SNCurve.calculate_cycles_to_failure` | independent | [docs/registry/implementation-map.yaml:L19579-L19588] |
| 263 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `FatigueDamageCalculator.calculate_damage` | independent | [docs/registry/implementation-map.yaml:L19589-L19598] |
| 264 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `main` | independent | [docs/registry/implementation-map.yaml:L19599-L19608] |
| 265 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L19609-L19617] |
| 266 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `SNCurveParameters.a1` | independent | [docs/registry/implementation-map.yaml:L19618-L19627] |
| 267 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `SNCurveParameters.a2` | independent | [docs/registry/implementation-map.yaml:L19628-L19637] |
| 268 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `FatigueDamageCalculator.cycles_to_failure` | independent | [docs/registry/implementation-map.yaml:L19638-L19647] |
| 269 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `FatigueDamageCalculator.calculate_damage` | independent | [docs/registry/implementation-map.yaml:L19648-L19659] |
| 270 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `FatigueDamageCalculator.export_results` | independent | [docs/registry/implementation-map.yaml:L19660-L19669] |
| 271 | `src/digitalmodel/structural/fatigue_apps/file_namer.py` | `FatigueFileNamer.get_rainflow_filename` | independent | [docs/registry/implementation-map.yaml:L19670-L19679] |
| 272 | `src/digitalmodel/structural/fatigue_apps/file_namer.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L19680-L19688] |
| 273 | `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `IntegratedFatigueProcessor.process_fatigue_condition` | independent | [docs/registry/implementation-map.yaml:L19689-L19698] |
| 274 | `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `main` | independent | [docs/registry/implementation-map.yaml:L19699-L19708] |
| 275 | `src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py` | `IntegratedFatigueProcessorWithNaming._process_condition_base` | independent | [docs/registry/implementation-map.yaml:L19709-L19718] |
| 276 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L19719-L19727] |
| 277 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `RainflowCounter.count_cycles` | caller | [docs/registry/implementation-map.yaml:L19728-L19739] |
| 278 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `RainflowCounter._rainflow_algorithm` | independent | [docs/registry/implementation-map.yaml:L19740-L19749] |
| 279 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `RainflowCounter.export_results` | independent | [docs/registry/implementation-map.yaml:L19750-L19759] |
| 280 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `rainflow_count` | independent | [docs/registry/implementation-map.yaml:L19760-L19769] |
| 281 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.process_time_series` | independent | [docs/registry/implementation-map.yaml:L19770-L19779] |
| 282 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.process_batch` | independent | [docs/registry/implementation-map.yaml:L19780-L19791] |
| 283 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.save_cycle_data` | independent | [docs/registry/implementation-map.yaml:L19792-L19801] |
| 284 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `main` | independent | [docs/registry/implementation-map.yaml:L19802-L19811] |
| 285 | `src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py` | `ReferenceSeaStateProcessor.process_fatigue_condition` | independent | [docs/registry/implementation-map.yaml:L19812-L19821] |
| 286 | `src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py` | `TensionStressConverter.process_batch` | independent | [docs/registry/implementation-map.yaml:L19822-L19831] |
| 287 | `src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py` | `main` | independent | [docs/registry/implementation-map.yaml:L19832-L19841] |
| 288 | `src/digitalmodel/structural/fatigue_apps/visualizer.py` | `generate_all_visualizations` | independent | [docs/registry/implementation-map.yaml:L19842-L19851] |
| 289 | `src/digitalmodel/structural/spectral_fatigue.py` | `narrow_band_damage` | independent | [docs/registry/implementation-map.yaml:L19852-L19861] |
| 290 | `src/digitalmodel/subsea/connectors/fatigue.py` | `assess_jumper_fatigue` | caller | [docs/registry/implementation-map.yaml:L19862-L19873] |
| 291 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `_dnv_params` | independent | [docs/registry/implementation-map.yaml:L19874-L19883] |
| 292 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `get_sn_curve` | independent | [docs/registry/implementation-map.yaml:L19884-L19895] |
| 293 | `src/digitalmodel/subsea/pipeline/free_span/span_fatigue_damage.py` | `SpanFatigueDamage._load_sn_curve` | caller | [docs/registry/implementation-map.yaml:L19896-L19907] |
| 294 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.calculate_damage` | independent | [docs/registry/implementation-map.yaml:L19908-L19917] |
| 295 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.calculate_fatigue_life` | independent | [docs/registry/implementation-map.yaml:L19918-L19927] |
| 296 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.analyze_viv_fatigue` | independent | [docs/registry/implementation-map.yaml:L19928-L19940] |
| 297 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.cumulative_damage` | caller | [docs/registry/implementation-map.yaml:L19941-L19952] |
| 298 | `src/digitalmodel/viv_screening/inputs.py` | `parse_inputs` | independent | [docs/registry/implementation-map.yaml:L19953-L19962] |
| 299 | `src/digitalmodel/viv_screening/workflow.py` | `router` | independent | [docs/registry/implementation-map.yaml:L19963-L19976] |
| 300 | `src/digitalmodel/viv_screening/workflow.py` | `_damage_rate` | independent | [docs/registry/implementation-map.yaml:L19977-L19987] |
| 301 | `src/digitalmodel/viv_screening/workflow.py` | `_provenance` | independent | [docs/registry/implementation-map.yaml:L19988-L19997] |
| 302 | `src/digitalmodel/web/digitaltwinfeed/app.py` | `page` | independent | [docs/registry/implementation-map.yaml:L19998-L20007] |
| 303 | `tests/ansys/padeye_pressure_bundle.py` | `_check_receipt` | test_helper | [docs/registry/implementation-map.yaml:L20008-L20017] |
| 304 | `tests/ansys/padeye_pressure_bundle.py` | `verify_pressure_bundle` | test_helper | [docs/registry/implementation-map.yaml:L20018-L20029] |
| 305 | `tests/ansys/test_fatigue_postprocessor.py` | `TestThicknessCorrection.test_known_correction` | test_helper | [docs/registry/implementation-map.yaml:L20030-L20039] |
| 306 | `tests/ansys/test_padeye_pressure_bundle.py` | `bundle` | test_helper | [docs/registry/implementation-map.yaml:L20040-L20049] |
| 307 | `tests/fatigue/test_assessment.py` | `histogram_input` | test_helper | [docs/registry/implementation-map.yaml:L20050-L20061] |
| 308 | `tests/fatigue/test_c203_consumers.py` | `_span_input` | test_helper | [docs/registry/implementation-map.yaml:L20062-L20071] |
| 309 | `tests/fatigue/test_c203_sn_values.py` | `_non_dnv_bilinear` | test_helper | [docs/registry/implementation-map.yaml:L20072-L20081] |
| 310 | `tests/fatigue/test_c203_sn_values.py` | `test_non_dnv_bilinear_switch_stays_at_endurance_limit` | test_helper | [docs/registry/implementation-map.yaml:L20082-L20095] |
| 311 | `tests/fatigue/test_c203_variable_amplitude.py` | `_signal_e` | test_helper | [docs/registry/implementation-map.yaml:L20096-L20105] |
| 312 | `tests/fatigue/test_c203_variable_amplitude.py` | `_user_curve` | test_helper | [docs/registry/implementation-map.yaml:L20106-L20115] |
| 313 | `tests/fatigue/test_c203_variable_amplitude.py` | `_span_input` | test_helper | [docs/registry/implementation-map.yaml:L20116-L20125] |
| 314 | `tests/fatigue/test_c203_variable_amplitude.py` | `_fresh_standard_curves` | test_helper | [docs/registry/implementation-map.yaml:L20126-L20135] |
| 315 | `tests/fatigue/test_c203_variable_amplitude.py` | `_data_dir` | test_helper | [docs/registry/implementation-map.yaml:L20136-L20145] |
| 316 | `tests/fatigue/test_damage.py` | `f_curve` | test_helper | [docs/registry/implementation-map.yaml:L20146-L20156] |
| 317 | `tests/fatigue/test_damage.py` | `TestThicknessCorrection.test_correction_above_ref` | test_helper | [docs/registry/implementation-map.yaml:L20157-L20166] |
| 318 | `tests/fatigue/test_damage.py` | `TestThicknessCorrection.test_correction_formula` | test_helper | [docs/registry/implementation-map.yaml:L20167-L20176] |
| 319 | `tests/fatigue/test_fatg.py` | `TestAgainstTransliterationSweep.test_all_presets_and_curves` | test_helper | [docs/registry/implementation-map.yaml:L20177-L20188] |
| 320 | `tests/fatigue/test_fatg_workflow.py` | `_cfg` | test_helper | [docs/registry/implementation-map.yaml:L20189-L20198] |
| 321 | `tests/fatigue/test_fatg_workflow.py` | `_library_reference` | test_helper | [docs/registry/implementation-map.yaml:L20199-L20209] |
| 322 | `tests/fatigue/test_fatigue_reporting.py` | `_sample_locations` | test_helper | [docs/registry/implementation-map.yaml:L20210-L20219] |
| 323 | `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls` | test_helper | [docs/registry/implementation-map.yaml:L20220-L20232] |
| 324 | `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls.calm_scatter` | test_helper | [docs/registry/implementation-map.yaml:L20233-L20242] |
| 325 | `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls.fapps_batch` | test_helper | [docs/registry/implementation-map.yaml:L20243-L20252] |
| 326 | `tests/fatigue/test_sn_curves.py` | `TestGetSNCurve.test_f_curve_at_100mpa` | test_helper | [docs/registry/implementation-map.yaml:L20253-L20263] |
| 327 | `tests/fatigue/test_sn_curves.py` | `TestGetSNCurve.test_d_curve_at_100mpa` | test_helper | [docs/registry/implementation-map.yaml:L20264-L20274] |
| 328 | `tests/fatigue/test_weld_fatigue_quickcheck.py` | `_hand_N` | test_helper | [docs/registry/implementation-map.yaml:L20275-L20284] |
| 329 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_cycles_to_failure_chain_known_value` | test_helper | [docs/registry/implementation-map.yaml:L20285-L20294] |
| 330 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_cycles_to_failure_wire_rope_known_value` | test_helper | [docs/registry/implementation-map.yaml:L20295-L20304] |
| 331 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_custom_sn_curve` | test_helper | [docs/registry/implementation-map.yaml:L20305-L20314] |
| 332 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestMinersRuleDamage.test_single_range_known_damage` | test_helper | [docs/registry/implementation-map.yaml:L20315-L20324] |
| 333 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestMinersRuleDamage.test_damage_additivity` | test_helper | [docs/registry/implementation-map.yaml:L20325-L20334] |
| 334 | `tests/parametric/test_atlas.py` | `_small_atlas` | test_helper | [docs/registry/implementation-map.yaml:L20335-L20344] |
| 335 | `tests/signal_processing/signal_analysis/fatigue_analysis_reference/process_fatigue_final.py` | `process_single_test_file` | test_helper | [docs/registry/implementation-map.yaml:L20345-L20354] |
| 336 | `tests/signal_processing/signal_analysis/reference_fatigue_processor.py` | `process_single_test_file` | test_helper | [docs/registry/implementation-map.yaml:L20355-L20364] |
| 337 | `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `<module>` | test_helper | [docs/registry/implementation-map.yaml:L20365-L20373] |
| 338 | `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `analyze_signal` | test_helper | [docs/registry/implementation-map.yaml:L20374-L20383] |
| 339 | `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `main` | test_helper | [docs/registry/implementation-map.yaml:L20384-L20395] |
| 340 | `tests/signal_processing/signal_analysis/test_orcaflex_tension_analysis.py` | `OrcaFlexTensionAnalyzer.analyze_tension` | test_helper | [docs/registry/implementation-map.yaml:L20396-L20405] |
| 341 | `tests/signal_processing/signal_analysis/test_orcaflex_tension_analysis.py` | `OrcaFlexTensionAnalyzer.export_results` | test_helper | [docs/registry/implementation-map.yaml:L20406-L20415] |
| 342 | `tests/signal_processing/signal_analysis/test_rainflow_timeseries1.py` | `run_rainflow_for_timeseries` | test_helper | [docs/registry/implementation-map.yaml:L20416-L20425] |
| 343 | `tests/signal_processing/signal_analysis/test_rainflow_timeseries1.py` | `test_rainflow_for_timeseries` | test_helper | [docs/registry/implementation-map.yaml:L20426-L20437] |
| 344 | `tests/signal_processing/signal_analysis/validate_module.py` | `main` | test_helper | [docs/registry/implementation-map.yaml:L20438-L20452] |
| 345 | `tests/solvers/test_integration_solvers.py` | `mock_solver_factory.MockSolverFactory.create_fatigue_solver` | test_helper | [docs/registry/implementation-map.yaml:L20453-L20462] |
| 346 | `tests/structural/fatigue/conftest.py` | `simple_sn_curve` | test_helper | [docs/registry/implementation-map.yaml:L20463-L20472] |
| 347 | `tests/structural/fatigue/test_damage_accumulation.py` | `dnv_d_curve` | test_helper | [docs/registry/implementation-map.yaml:L20473-L20482] |
| 348 | `tests/structural/fatigue/test_damage_accumulation.py` | `simple_curve` | test_helper | [docs/registry/implementation-map.yaml:L20483-L20492] |
| 349 | `tests/structural/fatigue/test_damage_accumulation.py` | `curve_with_material` | test_helper | [docs/registry/implementation-map.yaml:L20493-L20502] |
| 350 | `tests/structural/fatigue/test_fatigue_migration.py` | `TestSNCurveMigration.test_multislope_curve_implementation` | test_helper | [docs/registry/implementation-map.yaml:L20503-L20512] |
| 351 | `tests/structural/fatigue/test_fatigue_migration.py` | `TestSNCurveMigration.test_curve_continuity` | test_helper | [docs/registry/implementation-map.yaml:L20513-L20522] |
| 352 | `tests/structural/fatigue/test_fatigue_migration.py` | `TestFrequencyDomainMigration.test_basic_frequency_analysis` | test_helper | [docs/registry/implementation-map.yaml:L20523-L20532] |
| 353 | `tests/structural/fatigue/test_fatigue_reliability.py` | `simple_sn` | test_helper | [docs/registry/implementation-map.yaml:L20533-L20542] |
| 354 | `tests/structural/fatigue/test_fatigue_reliability.py` | `mild_sn` | test_helper | [docs/registry/implementation-map.yaml:L20543-L20552] |
| 355 | `tests/structural/fatigue/test_fatigue_skill.py` | `_simple_input` | test_helper | [docs/registry/implementation-map.yaml:L20553-L20562] |
| 356 | `tests/structural/fatigue/test_fatigue_skill.py` | `TestOffshoreScenario.test_high_cycle_low_stress_result` | test_helper | [docs/registry/implementation-map.yaml:L20563-L20574] |
| 357 | `tests/structural/fatigue/test_frequency_domain.py` | `simple_sn_curve` | test_helper | [docs/registry/implementation-map.yaml:L20575-L20584] |
| 358 | `tests/structural/fatigue/test_rainflow.py` | `default_counter` | test_helper | [docs/registry/implementation-map.yaml:L20585-L20594] |
| 359 | `tests/structural/fatigue/test_scatter_fatigue.py` | `simple_sn` | test_helper | [docs/registry/implementation-map.yaml:L20595-L20604] |
| 360 | `tests/structural/fatigue/test_scatter_fatigue.py` | `TestFatigueLife.test_fatigue_life_inverse_of_damage` | test_helper | [docs/registry/implementation-map.yaml:L20605-L20616] |
| 361 | `tests/structural/fatigue/test_sn_curves.py` | `simple_power_law` | test_helper | [docs/registry/implementation-map.yaml:L20617-L20626] |
| 362 | `tests/structural/fatigue/test_sn_curves.py` | `power_law_with_limit` | test_helper | [docs/registry/implementation-map.yaml:L20627-L20636] |
| 363 | `tests/structural/fatigue/test_sn_curves.py` | `bilinear_curve` | test_helper | [docs/registry/implementation-map.yaml:L20637-L20646] |
| 364 | `tests/structural/fatigue/test_sn_curves.py` | `dnv_d_curve` | test_helper | [docs/registry/implementation-map.yaml:L20647-L20656] |
| 365 | `tests/structural/fatigue/test_sn_curves.py` | `TestBS7608RevisedCurveParameters.test_bs7608_revised_d_known_n_at_50mpa` | test_helper | [docs/registry/implementation-map.yaml:L20657-L20666] |
| 366 | `tests/structural/fatigue/test_sn_curves.py` | `TestISO19902CurveParameters.test_iso19902_f_air_known_n_at_100mpa` | test_helper | [docs/registry/implementation-map.yaml:L20667-L20676] |
| 367 | `tests/structural/fatigue/test_sn_curves.py` | `TestISO19902CurveParameters.test_iso19902_f2_sw_cp_known_n_at_50mpa` | test_helper | [docs/registry/implementation-map.yaml:L20677-L20686] |
| 368 | `tests/structural/fatigue/test_sn_curves_yaml.py` | `_all_single_slope_cases` | test_helper | [docs/registry/implementation-map.yaml:L20687-L20696] |
| 369 | `tests/structural/fatigue/test_sn_curves_yaml.py` | `_all_multislope_cases` | test_helper | [docs/registry/implementation-map.yaml:L20697-L20706] |
| 370 | `tests/structural/fatigue_analysis/conftest.py` | `sn_curve_data` | test_helper | [docs/registry/implementation-map.yaml:L20707-L20716] |
| 371 | `tests/structural/fatigue_analysis/test_fatigue_analysis_sn.py` | `run_fatigue_analysis` | test_helper | [docs/registry/implementation-map.yaml:L20717-L20727] |
| 372 | `tests/structural/fatigue_analysis/test_fatigue_analysis_timetrace.py` | `run_fatigue_analysis` | test_helper | [docs/registry/implementation-map.yaml:L20728-L20738] |
| 373 | `tests/subsea/pipeline/test_free_span_f105.py` | `ref_input` | test_helper | [docs/registry/implementation-map.yaml:L20739-L20748] |
| 374 | `tests/subsea/viv_analysis/test_viv_analysis_unit.py` | `TestVIVFatigueCalculator.test_cumulative_damage` | test_helper | [docs/registry/implementation-map.yaml:L20749-L20758] |
| 375 | `tests/test_automation/fixtures/engineering_fixtures.py` | `get_orcaflex_post_process_mock` | test_helper | [docs/registry/implementation-map.yaml:L20759-L20768] |
| 376 | `tests/test_automation/scripts/comprehensive_fix_campaign.py` | `ComprehensiveTestFixCampaign.generate_mock_data` | test_helper | [docs/registry/implementation-map.yaml:L20769-L20778] |
| 377 | `tests/test_complete_fatigue_pipeline.py` | `process_configuration` | test_helper | [docs/registry/implementation-map.yaml:L20779-L20788] |
| 378 | `tests/test_complete_fatigue_pipeline.py` | `main` | test_helper | [docs/registry/implementation-map.yaml:L20789-L20800] |
| 379 | `tests/test_fatigue_basic.py` | `run_all_tests` | test_helper | [docs/registry/implementation-map.yaml:L20801-L20810] |
| 380 | `tests/test_rainflow_with_effective_tension.py` | `process_effective_tension_file` | test_helper | [docs/registry/implementation-map.yaml:L20811-L20820] |
| 381 | `tests/test_rainflow_with_effective_tension.py` | `main` | test_helper | [docs/registry/implementation-map.yaml:L20821-L20831] |
| 382 | `tests/test_reservoir_framework.py` | `run_performance_tests` | test_helper | [docs/registry/implementation-map.yaml:L20832-L20841] |
| 383 | `tests/test_spectral_fatigue_validation.py` | `TestSNCurveDNVReference.test_dnv_d_curve_parameters` | test_helper | [docs/registry/implementation-map.yaml:L20842-L20853] |
| 384 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | test_helper | [docs/registry/implementation-map.yaml:L20854-L20864] |
| 385 | `tests/workflows/test_durable_workflows.py` | `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | test_helper | [docs/registry/implementation-map.yaml:L20865-L20875] |
| 386 | `tests/workflows/test_durable_workflows.py` | `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | test_helper | [docs/registry/implementation-map.yaml:L20876-L20886] |
| 387 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `PowerLawSNCurve.get_allowable_cycles` | independent | [docs/registry/implementation-map.yaml:L20887-L20897] |
| 388 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `PowerLawSNCurve.get_stress_range` | independent | [docs/registry/implementation-map.yaml:L20898-L20907] |
| 389 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `BilinearSNCurve.get_allowable_cycles` | independent | [docs/registry/implementation-map.yaml:L20908-L20917] |
| 390 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `BilinearSNCurve.get_stress_range` | independent | [docs/registry/implementation-map.yaml:L20918-L20928] |
| 391 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `BilinearSNCurve.get_allowable_cycles` | independent | [docs/registry/implementation-map.yaml:L20929-L20938] |
| 392 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `BilinearSNCurve.get_stress_range` | independent | [docs/registry/implementation-map.yaml:L20939-L20948] |

### Map follow-up

Tension-to-stress conversion and life/DFF screening have no separate in-package map family; add or relate them in the map before counting them in a later review, issue pending (4). [src/digitalmodel/mooring_fatigue/workflow.py:L111-L121] [src/digitalmodel/mooring_fatigue/workflow.py:L230-L239]

External implementations are map inventory and discovery context, not package misses; the shared fatigue and parametric paths above remain the directly traced dependency route. [src/digitalmodel/mooring_fatigue/workflow.py:L13-L15] [src/digitalmodel/parametric/generate.py:L28-L45]

## 10. Coverage and open items

| Coverage | Files read / scope evidence | Tracking for remaining work |
|---|---|---|
| Complete package | `__init__.py` and every line of `workflow.py`; no child Python package is present in this scope. [src/digitalmodel/mooring_fatigue/__init__.py:L1-L1] [src/digitalmodel/mooring_fatigue/workflow.py:L1-L286] | Direct import, input-edge and history tests remain to be added; issue pending (1). |
| Workflow and tests | Both metallic example inputs, atlas-query input, registry rows and metallic/query assertions; all three discovered parametric test files were read and run. [examples/workflows/mooring-fatigue/input.yml:L1-L36] [examples/workflows/mooring-fatigue-v2/input.yml:L1-L42] [docs/registry/workflows.yaml:L84-L139] [tests/workflows/test_durable_workflows.py:L137-L168] [tests/parametric/test_atlas.py:L1-L409] [tests/parametric/test_refresh.py:L1-L379] [tests/parametric/test_catalog.py:L1-L40] | Independent module-level physical reference remains absent from these cases; issue pending (2). |
| Documentary discovery | `docs/maps/digitalmodel-operator-map.md`, `docs/registry/module-routing.yaml`, `docs/registry/workflows.yaml`, `docs/capability-map/capabilities-added.yml`, `docs/cad-inventory/ecosystem-and-automation-surface.md`, `docs/plans/2026-09-25-issue-1057-ffs-offering-program.md`, `docs/plans/parametrics-preparedness.md`, `docs/plans/parametric-refresh/adding-an-atlas.md`, `docs/plans/parametric-refresh/atlas-spec.md`, `docs/domains/asset-integrity/ffs-offering-catalog.md`, and both `docs/registry/implementation-map` documents name the module. [docs/maps/digitalmodel-operator-map.md:L91-L91] [docs/capability-map/capabilities-added.yml:L65-L65] [docs/cad-inventory/ecosystem-and-automation-surface.md:L78-L81] [docs/plans/2026-09-25-issue-1057-ffs-offering-program.md:L21-L21] [docs/plans/parametrics-preparedness.md:L52-L60] [docs/plans/parametric-refresh/adding-an-atlas.md:L112-L119] [docs/plans/parametric-refresh/atlas-spec.md:L20-L23] [docs/domains/asset-integrity/ffs-offering-catalog.md:L91-L91] [docs/registry/implementation-map.md:L24-L24] | Documentary claims about environment sweeps and out-of-plane bending are not established by this scalar package; reconcile capability wording, issue pending (4). |
| Source and citation basis | Shared curve, damage and counting functions and edition metadata, engine dispatch and parametric wrapper were read for the paths cited in sections 1–9. [src/digitalmodel/fatigue/c203_editions.py:L19-L37] [src/digitalmodel/fatigue/damage.py:L13-L39] [src/digitalmodel/fatigue/rainflow.py:L22-L127] [src/digitalmodel/parametric/generate.py:L28-L45] | Runtime citation-sidecar and edition basis need completion; issue pending (2). |
| Input failures | Sweep S01–S32 and C01–C03 distinguish defaults, internal conversions and unchecked external input. [src/digitalmodel/mooring_fatigue/workflow.py:L24-L286] | Nonfinite values, repeat/bin controls, aliases and exposure: issue pending (1), issue pending (2). |
| Output failures | Duplicate fallback IDs, replacement writes, text encoding and absolute metadata paths. [src/digitalmodel/mooring_fatigue/workflow.py:L67-L85] [src/digitalmodel/mooring_fatigue/workflow.py:L242-L286] | Portable output handling: issue pending (3). |
| Implementation map | Complete S-N/Miner family inventory and exact in-package signal search in section 9 | Other method-family coverage: issue pending (4). |
| Clearance | Review role assignment is in section 8 | Maintainers to obtain independent review, resolve issue-draft references and create the shared lesson entries; issue pending (4). |

UNVERIFIED claims carried from sections 1 and 3:

1. Construction-specific curve suitability and the geometric/depth applicability envelope: issue pending (2).
2. Implemented/data-verified edition equivalence, licensed clause compliance and construction-specific qualification: issue pending (2).

The calculation-output citation contract was readable for this draft; no private sidecar entry or private input data was needed for the public examples.
