# Subsea structure lowering: offline workflow

This workflow prepares a sourced sea-state case table, optionally overlays it on
an explicitly supplied model, screens imported phase extrema, and writes the
standard HTML analysis with a weather-window screen. It never imports or queries
a licensed solver. Engineering acceptance remains `NOT_EVALUATED`.

## Method and sources

- Case count is the Cartesian product of the supplied Hs, peak-period Tp and
  heading axes. The requested grid has 3 × 4 × 3 = 36 combinations; numerical
  levels are required inputs, not a recommended operational range.
- For a maximum criterion the response passes when response ≤ supplied limit;
  for a minimum criterion it passes when response ≥ supplied limit. Equality
  passes. These are definitions of the selected project bounds, not DNV limits.
- A case requires all supplied criteria and coverage of splash, submerged and
  landing phases. Missing cases, phase metrics, matching units, identifiers or
  a common nonblank solver version prevent an envelope. Extra/duplicate result
  cases and duplicate metrics are rejected. A known failure cannot substitute
  for missing evidence.
- The screened grid limit is the highest Hs in an uninterrupted sequence of
  passing levels starting at the lowest supplied Hs, with every supplied Tp and
  heading passing. All levels must have complete evidence. No interpolation,
  extrapolation or probabilistic limit is calculated.
- Weather persistence reuses
  `digitalmodel.weather_window.workflow.window_lengths_hours`: Hs ≤ grid limit,
  spell duration = sample count × sample interval, workability = passing sample
  count / total sample count × 100 %. This sampled Hs-only exercise assumes
  persistence over each sample interval. It does not check period, heading,
  forecast uncertainty, an operational margin or forecast validity.
- The wave overlay follows [Orcina, OrcaFlex web help, Environment: Data for
  JONSWAP and ISSC spectra, Setting the data and Spectral parameters](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Environment,DataforJONSWAPandISSCspectra.htm)
  (accessed 2026-10-20). Only one partially specified JONSWAP train is supported;
  the supplied gamma precedes Tp. Automatic and fully specified spectra,
  simultaneous Tz/Fm inputs and unresolved external dependencies are rejected.
  No spectral coefficient is supplied by this workflow.

## Preparation and source ledger

Each axis and criterion limit requires `value`, `unit` and a nonblank `source`.
Units are exactly `m`, `s`, `deg`, and the metric unit supplied by the criterion;
there is no implicit conversion. Headings use [0, 360) degrees by the definition
of a full turn. `basis_id` identifies the criteria/model revision;
`model_sha256` identifies the exact parent model bytes. Result records echo both,
the exact sea state, the intended-use `kind` and a common `solver_version`.
These fields are consistency checks, not authenticated native-output evidence.

Before any engineering use, the supplied model ledger must identify the source,
units, revision and intended-use qualification for every physical value,
including buoy mass, displacement, inertia, centres, drag/added-mass/slam data,
rigging, lowering history, seabed contact, motion input, spectral settings,
seeds, durations and logging. No inherited template value is qualified here.

The issue-designated sources were inspected:

| Repository source | Role and limitation |
| --- | --- |
| `docs/domains/orcaflex/library/templates/installation_subsea/spec.yml` | Generic starting specification; free buoy and empty simulation settings do not define a suspended lowering system. |
| `docs/domains/orcaflex/library/model_library/f01_lowered_cone/spec.yml` | Vendor-derived lowering reference; model redistribution is not established. |
| `docs/domains/orcaflex/library/model_library/f02_passive_compensation/spec.yml` | Vendor-derived compensation reference; coefficients are not adopted. |
| `docs/domains/orcaflex/library/model_library/f07_suction_anchor_lowering/spec.yml` | Vendor-derived lowering reference; load history is not adopted. |
| `docs/domains/orcaflex/library/model_library/heave_compensated_winch/spec.yml` | Vendor-derived external-function reference; unresolved dependencies cannot be prepared by this workflow. |
| `examples/workflows/orcaflex-6dbuoy-dnvrph103/input.yml` | Existing simplified deep-zone buoy preparation; splash and landing qualification is not established. |
| `src/digitalmodel/orcaflex/installation_analysis.py` | Existing analytical screens; defaults are not imported as acceptance criteria. |
| `docs/domains/orcaflex/dnv-rp-h103.md` | Shape-property guidance; no numerical acceptance basis or edition is established. |

Table 1: preparation sources and their current limitations. Internal comparator
models are excluded from this workflow and its generated report.

After the physical model is independently prepared and flattened, the optional
`--model` argument accepts that complete native YAML. The parent digest must
match. Only WaveHs, WaveTp and WaveDirection are overlaid. Other parsed model
properties are preserved; formatting and comments are not preserved. The
repository YAML validator checks every generated file. Each case manifest retains
its generated-file digest, source axes and validation warnings. A structurally
valid YAML file does not establish a valid suspended system or solver loading.
No buoy-property derivation or lowering-history design is performed.

```sh
PYTHONPATH=src python3 -m digitalmodel.workflows.subsea_lowering basis.json \
  --model prepared-model.yml --records extrema.json --weather weather.json \
  --output new-assessment
```

All inputs are caller-provided. Output must be a new directory. No model from the
vendor library is copied into this example or redistributed by the package.
Generated files remain local until their source rights permit publication.

## Worked arithmetic example

The following executable example supplies its complete inputs in memory; it
writes no sample or solver files. All numbers are invented test inputs, with the
independent calculation below as their source. They are not measurements,
previous-project data, operating recommendations or a published benchmark.

```python
from digitalmodel.workflows.subsea_lowering import assess_sweep, build_cases

source = "Independent arithmetic exercise in this worked example"
def q(value, unit):
    return {"value": value, "unit": unit, "source": source}

basis = {
    "basis_id": "illustrative-v1", "purpose": "illustrative",
    "model_sha256": "a" * 64,  # Placeholder identity; no real model is qualified.
    "hs": [q(v, "m") for v in [1, 2, 3]],
    "tp": [q(v, "s") for v in [6, 8, 10, 12]],
    "headings": [q(v, "deg") for v in [0, 90, 180]],
    "criteria": [
        {"name": "hook_tension", "phase": phase, "bound": "maximum",
         "limit": q(100, "kN")}
        for phase in ["splash", "submerged", "landing"]
    ],
}
records = []
for case in build_cases(basis):
    records.append({
        "case_id": case["case_id"], "basis_id": basis["basis_id"],
        "model_sha256": basis["model_sha256"], "kind": "illustrative",
        "solver_version": "not-run",
        "sea_state": {k: case[k] for k in ["hs_m", "tp_s", "heading_deg"]},
        "metrics": [
            {"name": "hook_tension", "phase": phase, "unit": "kN",
             "value": {1: 80, 2: 120, 3: 70}[case["hs_m"]]}
            for phase in ["splash", "submerged", "landing"]
        ],
    })
weather = {
    "source": source, "hs_timeseries_m": [0.5, 1, 2, 0.5],
    "time_step_hours": 1, "required_duration_hours": 2,
}
result = assess_sweep(basis, records, weather)
assert len(result["cases"]) == 36
assert result["screened_hs_limit_m"] == 1
assert result["weather_window"]["workability_pct"] == 75
assert result["weather_window"]["num_viable_windows"] == 1
assert result["engineering_status"] == "NOT_EVALUATED"
```

Independent answer: at Hs = 1.0 m, 80.0 kN ≤ 100.0 kN for all twelve
period/heading combinations and all phases. At Hs = 2.0 m, 120.0 kN exceeds
100.0 kN, so the contiguous sampled limit is 1.0 m even though the third
level passes at 70.0 kN. The weather samples below or equal to 1.0 m form spells
of 2.0 h and 1.0 h. Three of four samples pass: 75.0 % workability; one spell
meets the required 2.0 h duration. Native runs attempted/completed: zero;
qualified engineering results: zero.

The JSON CLI writes `assessment.json` and the standard `report.html`, retaining
input byte digests. The report includes summary, revision history, sourced case
and phase comparison tables, weather results, evidence identifiers and the way
forward. Displayed values are rounded to one decimal; JSON preserves computation
precision. Revision 00 is the initial issue; this command creates a new report,
not a revision-management system for reissued documents.

## Remaining engineering gates

The three offline slices cover case/model preparation, complete-grid extrema
screening, and weather/report integration. Full issue acceptance remains open:

1. A public numerical benchmark and qualified buoy, rigging and lowering basis
   must be selected and independently reviewed; DNV-RP-H103 / DNV-OS-H101 titles
   alone do not supply acceptance values.
2. A separately authorized licensed run must establish loading/statics, splash
   through landing, convergence, native readback and a recorded solver version
   across the sea-state grid, retaining native files and per-generated-model
   hashes. No execution or queue submission is implemented here.
3. Imported extrema must be bound to qualified native evidence and approved
   criteria before an operational limiting Hs or weather decision is issued.
   The current `PASS`/`FAIL` labels mean arithmetic comparisons only; the report
   always keeps engineering acceptance `NOT_EVALUATED`.
