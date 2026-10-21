# Spectral fatigue knowledge pack

| Header | Value |
| --- | --- |
| Module | `spectral_fatigue` |
| Repository | `digitalmodel` |
| Package | `src/digitalmodel/spectral_fatigue` |
| Pack revision | 1, 2026-10-19 |
| Code revision | `29a87f1aba14aa22067794846aa5c2ec8bf407c1` |
| Analysis family | Engineering calculations (`EC`); AQG r0, Python calculations |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |
| Implementation-map revision | `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, data revision `270c8759c374b67a7f032953e7fcdb14e0224461` |

## 1. Purpose and scope

`router` evaluates supplied stress PSDs at each location, occurrence-weights annual damage, selects the smallest life margin, writes detail and summary CSVs, and replaces the input settings with result metadata. [src/digitalmodel/spectral_fatigue/workflow.py:L37-L76] [src/digitalmodel/spectral_fatigue/workflow.py:L88-L127]

The engine dispatches the `spectral_fatigue` basename to this router; the registry marks the module `development` and the durable workflow `offline`. [src/digitalmodel/engine.py:L340-L345] [docs/registry/module-routing.yaml:L646-L651] [docs/registry/workflows.yaml:L169-L178]

This package consumes stress PSD arrays directly; wave-spectrum generation, RAO transfer and the fatigue estimators belong to other packages and are outside this pack's implementation scope. [src/digitalmodel/spectral_fatigue/workflow.py:L93-L98] [src/digitalmodel/spectral_fatigue/workflow.py:L160-L171] [src/digitalmodel/fatigue/spectral_fatigue.py:L116-L143]

Use as screening arithmetic pending qualification: the imported library integrates Hz moments but divides its crossing and peak rates by an additional angular conversion; the same convention reaches annual damage through every supported estimator (#2583). [src/digitalmodel/fatigue/spectral_fatigue.py:L64-L71] [src/digitalmodel/fatigue/spectral_fatigue.py:L138-L141] [src/digitalmodel/fatigue/spectral_fatigue.py:L178-L195] [src/digitalmodel/fatigue/spectral_fatigue.py:L234-L249] [src/digitalmodel/fatigue/spectral_fatigue.py:L320-L327] [src/digitalmodel/fatigue/spectral_fatigue.py:L361-L390]

## 2. Inputs and outputs

Input fields below are relative to `cfg["spectral_fatigue"]`; ranges describe checks actually implemented, rather than a qualified engineering envelope. [src/digitalmodel/spectral_fatigue/workflow.py:L38-L49] [src/digitalmodel/spectral_fatigue/workflow.py:L130-L190]

| Input | Unit | Implemented range / default | Source |
| --- | --- | --- | --- |
| `design_life_years` | year | Required, `float(value) > 0`; no finite check | Configuration; [src/digitalmodel/spectral_fatigue/workflow.py:L39-L40] [src/digitalmodel/spectral_fatigue/workflow.py:L182-L190] |
| `dff` | dimensionless | Required, same positive comparison | Configuration; [src/digitalmodel/spectral_fatigue/workflow.py:L39-L40] [src/digitalmodel/spectral_fatigue/workflow.py:L182-L190] |
| `method` | identifier | Defaults to `dirlik`; stripped, lowercased; alternatives `narrow_band`, `wirsching_light`, `benasciutti_tovo` | Configuration; [src/digitalmodel/spectral_fatigue/workflow.py:L29-L34] [src/digitalmodel/spectral_fatigue/workflow.py:L130-L136] |
| `sn_curve.slope`, `sn_curve.log_intercept` | dimensionless exponent; base-10 intercept for MPa stress range | Required positive floats; no named-curve lookup, environmental selection or finite check here | Configuration and delegated convention; [src/digitalmodel/spectral_fatigue/workflow.py:L139-L143] [src/digitalmodel/spectral_fatigue/workflow.py:L182-L190] [src/digitalmodel/fatigue/spectral_fatigue.py:L164-L172] |
| `locations` / `sea_states` | lists | Nonempty lists; individual item mapping types are not checked | Configuration; [src/digitalmodel/spectral_fatigue/workflow.py:L146-L157] [src/digitalmodel/spectral_fatigue/workflow.py:L88-L94] |
| Location `id` / `name` | identifier | `id`, then `name`, then literal `location`; uniqueness is not checked | Configuration; [src/digitalmodel/spectral_fatigue/workflow.py:L88-L89] |
| `frequency_Hz` | Hz | List, length at least 2, equal to PSD length; converted with `float`; ordering and finiteness unchecked | Sea state; [src/digitalmodel/spectral_fatigue/workflow.py:L160-L171] |
| `stress_psd_MPa2_Hz` | MPa²/Hz | Same length constraint; signs and finiteness unchecked | Sea state; [src/digitalmodel/spectral_fatigue/workflow.py:L160-L171] |
| `occurrence_fraction` | dimensionless annual fraction | Default 1.0; positive comparison only; no upper bound or sum-to-one check | Sea state; [src/digitalmodel/spectral_fatigue/workflow.py:L174-L179] |
| `output_dir` | path | Default `results`; relative to configuration directory | Configuration; [src/digitalmodel/spectral_fatigue/workflow.py:L201-L205] |
| `_config_dir_path`, `_config_file_path`, `basename` | path / identifier | Directory hint, then input parent, then current directory; input stem, then basename, then `spectral_fatigue` | Engine metadata; [src/digitalmodel/spectral_fatigue/workflow.py:L208-L219] |

The dependency documents a one-sided PSD in Hz and MPa²/Hz; no spatial coordinates, origin, directional sign or phase fields enter this router. [src/digitalmodel/fatigue/spectral_fatigue.py:L120-L127] [src/digitalmodel/spectral_fatigue/workflow.py:L79-L98] [src/digitalmodel/spectral_fatigue/workflow.py:L160-L171]

| Output | Unit / format | Meaning and bounds | Source |
| --- | --- | --- | --- |
| `<stem>_spectral_fatigue.csv` | CSV | One row per sea state: location, zero-based state index, occurrence, method, RMS MPa, bandwidth, `peak_rate_Hz` (affected by the extra 1/(2π) factor in #2583), full and weighted annual damage (year⁻¹) | [src/digitalmodel/spectral_fatigue/workflow.py:L99-L110] [src/digitalmodel/spectral_fatigue/workflow.py:L193-L194] |
| `<stem>_spectral_fatigue_summary.csv` | CSV | One row per location: annual damage year⁻¹, life and required life in years, margin dimensionless, Boolean pass | [src/digitalmodel/spectral_fatigue/workflow.py:L113-L127] [src/digitalmodel/spectral_fatigue/workflow.py:L197-L198] |
| Replaced `cfg["spectral_fatigue"]` | mapping | Design inputs, method, curve, governing location/margin/life, locations and displayed CSV paths | [src/digitalmodel/spectral_fatigue/workflow.py:L60-L74] |
| `cfg["screening_status"]` | `pass` / `fail` | Pass only if every location passes; location passes when margin ≥ 1.0 | [src/digitalmodel/spectral_fatigue/workflow.py:L68-L75] [src/digitalmodel/spectral_fatigue/workflow.py:L118-L126] |

CSV columns follow first-row dictionary order; existing files are opened for replacement, with platform-default text encoding and CSV newline handling; no encoding parameter is supplied. [src/digitalmodel/spectral_fatigue/workflow.py:L222-L229]

## 3. Method and governing standards

| Stage / implementation | Algorithm and basis |
| --- | --- |
| `_method`, `_sn_curve` | Select one imported estimator and pass through scalar S-N parameters; no standard table lookup occurs in these helpers. [src/digitalmodel/spectral_fatigue/workflow.py:L130-L143] |
| `compute_spectral_moments` | Trapezoidal integration of PSD times frequency powers; RMS and bandwidth are dependency properties. [src/digitalmodel/fatigue/spectral_fatigue.py:L59-L82] [src/digitalmodel/fatigue/spectral_fatigue.py:L133-L143] |
| `narrow_band_damage` | Library docstring attributes the Rayleigh-range closed form to Bendat (1964); that attribution is recorded here as code provenance, without independent source verification. [src/digitalmodel/fatigue/spectral_fatigue.py:L150-L162] |
| `wirsching_light_damage` | Library docstring names DNV-RP-C203, 2021, Frequency domain fatigue analysis, and a subsection locator; empirical coefficients are not reproduced here. [src/digitalmodel/fatigue/spectral_fatigue.py:L207-L221] |
| `dirlik_damage` | Library attributes its exponential/Rayleigh mixture to Dirlik (1985), but substitutes bandwidth ε = √(1−α2²) for irregularity γ = α2 in D1, R, D2 and Q, then clamps parameters and denominators. This default-estimator formula defect is tracked in #2746, item 2; C10 exposes it in the narrow-band limit. [src/digitalmodel/fatigue/spectral_fatigue.py:L261-L271] [src/digitalmodel/fatigue/spectral_fatigue.py:L291-L327] |
| `benasciutti_tovo_damage` | Library attributes the interpolation to Benasciutti and Tovo (2005); its wide-band factor and final maximum remain a qualification defect (#2775). [src/digitalmodel/fatigue/spectral_fatigue.py:L339-L348] [src/digitalmodel/fatigue/spectral_fatigue.py:L364-L390] |
| `_evaluate_location` | `annual_damage = sum(full_annual_damage * occurrence_fraction)`; life = reciprocal annual damage; required life = design life × DFF; margin = life / required life; reject nonpositive accumulated damage. [src/digitalmodel/spectral_fatigue/workflow.py:L91-L98] [src/digitalmodel/spectral_fatigue/workflow.py:L113-L125] |
| `router` | Governing location minimizes margin; writes both CSVs and collapses all location pass flags to the overall verdict. [src/digitalmodel/spectral_fatigue/workflow.py:L54-L75] |

The workflow docstring and example name DNV-RP-C203 without an edition; the dependency names 2021, Frequency domain fatigue analysis; the title is recorded in its S-N library as Fatigue Design of Offshore Steel Structures. Clause text not checked against the licensed copy; no tabulated standard values are reproduced in this pack. [src/digitalmodel/spectral_fatigue/workflow.py:L1-L6] [examples/workflows/spectral-fatigue/input.yml:L6-L9] [src/digitalmodel/fatigue/spectral_fatigue.py:L23-L28] [src/digitalmodel/fatigue/sn_library.py:L32-L34]

The separate structural comparison tests name the 2011 edition for curve assertions, whereas the imported spectral library names 2021; those adjacent tests do not reconcile the router example's unspecified edition (issue pending (2)). [tests/test_spectral_fatigue_validation.py:L54-L63] [src/digitalmodel/fatigue/spectral_fatigue.py:L23-L28]

UNVERIFIED: the example's curve designation and standard edition have not been independently reconciled with the licensed standard; the package emits no `Citation` sidecar, and a matching scalar curve input does not establish standard compliance (issue pending (2)). [examples/workflows/spectral-fatigue/input.yml:L6-L9] [src/digitalmodel/spectral_fatigue/workflow.py:L60-L76] [src/digitalmodel/spectral_fatigue/workflow.py:L139-L143]

## 4. Validated worked examples

Validated workflow examples: **0**; the shipped example supplies a PSD and curve, and the narrow-band test obtains its expected damage by calling the same fatigue library as the router. These are regression examples, irrespective of the test docstring's validation wording. [examples/workflows/spectral-fatigue/input.yml:L1-L26] [tests/workflows/test_durable_workflows.py:L1893-L1905] [tests/workflows/test_durable_workflows.py:L1941-L1950]

| Regression case | Source / input | Expected output and tolerance | Existing test node ID | Last offline result |
| --- | --- | --- | --- | --- |
| R01 durable example | Repository example at the code revision; `examples/workflows/spectral-fatigue/input.yml` | Dirlik; matching overall/configuration verdicts of `fail`; governing location is the splash zone and governing margin is the minimum; the summary contains splash-zone and keel-joint IDs, with pass flags false and true respectively; exactly 2 splash-zone detail rows, all full damage positive; reciprocal life and weighted damage agree at pytest default tolerance. Output files must exist. The keel-joint pass depends on #2583: with Hz-consistent rates, its Dirlik margin is approximately 0.94, below the 1.0 pass criterion. R01 and C04 must be re-baselined with that repair, including the keel-joint pass assertion. These are consistency checks, not an independent fatigue benchmark; total row counts are C09. [tests/workflows/test_durable_workflows.py:L364-L401] | `tests/workflows/test_durable_workflows.py::test_workflow_registry[spectral-fatigue]` | PASS at BASE and AFTER; temporary copy, outputs retained outside the source tree |
| R02 narrow-band wiring | Synthetic PSD written by the test; expected annual damage and reciprocal life from the imported library | Relative error ≤ 1e-9, dimensionless, in damage (year⁻¹) and life (year); shared-library regression, affected by #2583. [tests/workflows/test_durable_workflows.py:L1893-L1950] | `tests/workflows/test_durable_workflows.py::test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | PASS at BASE and AFTER; temporary copy |

Offline execution record: code revision `29a87f1aba14aa22067794846aa5c2ec8bf407c1`; the two nodes above plus `tests/fatigue/test_spectral_fatigue.py`, `tests/test_spectral_fatigue.py` and `tests/test_spectral_fatigue_validation.py`; command form `PYTHONPATH=src uv run --no-sources --with 'pytest>=7.4.3,<9.0.0' python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>`. The tests and source are copied without edits before running; the workflow outputs remain in the temporary copy. Observed results are recorded in section 10; no licensed runtime is needed for these cases. [tests/workflows/test_durable_workflows.py:L56-L59] [docs/registry/workflows.yaml:L169-L178]

## 5. QA gates

Gate applicability follows AQG r0, Python calculations: G0 inputs/units/citations, G1 imports/tests, G2 hand or published checks, G3 and G4 `NOT_APPLICABLE`, G5 evidence. The checks below are acceptance prescriptions; a passing pack self-check does not clear the calculation's unresolved gates.

| Gate | Applicability and route | Checks |
| --- | --- | --- |
| G0 | Applicable: supplied PSD and scalar-curve preflight; standard provenance remains open | C01–C03 |
| G1 | Applicable: Python import and offline pytest route, no model-load route because the router writes CSV rather than a solver deck. [src/digitalmodel/spectral_fatigue/workflow.py:L54-L58] [src/digitalmodel/spectral_fatigue/workflow.py:L222-L229] | C04–C05 |
| G2 | Applicable: check weighted accumulation independently, then qualify crossing rates and spectral estimators separately | C06–C08, C10 |
| G3 | NOT_APPLICABLE: no time integration is performed by the supplied-PSD router. [src/digitalmodel/spectral_fatigue/workflow.py:L92-L98] | None |
| G4 | NOT_APPLICABLE: no diffraction mesh or hydrostatics is read or written by this route. [src/digitalmodel/spectral_fatigue/workflow.py:L160-L171] [src/digitalmodel/spectral_fatigue/workflow.py:L222-L229] | None |
| G5 | Applicable: retain input/output fingerprints, test outcomes and unresolved dispositions; a signed report appendix is not generated by the router. [src/digitalmodel/spectral_fatigue/workflow.py:L60-L76] | C09 |

| Check | Gate | Executable check, quantity, numeric criterion and unit | Evidence / observed status |
| --- | --- | --- | --- |
| C01 | G0 | Exercise `_positive_float`, `_locations`, `_sea_states`, `_method`, `_psd` and `_occurrence` using missing/zero scalars, empty lists, unknown method, one-point arrays, zero occurrence and comma-separated numeric text; require 8 of 8 `ValueError` results, count | PASS, 8/8 guard cases. [src/digitalmodel/spectral_fatigue/workflow.py:L130-L190] |
| C02 | G0 | Before `router`, require finite positive life, DFF and curve values; unique location IDs (duplicates = 0 count); finite PSD ≥ 0 MPa²/Hz; strictly increasing nonnegative frequency in Hz; per-location occurrence sum error ≤ 1e-9 dimensionless | Shipped input PASS; bypass probes show missing enforcement in the router, issue pending (1). [examples/workflows/spectral-fatigue/input.yml:L3-L26] [src/digitalmodel/spectral_fatigue/workflow.py:L146-L190] |
| C03 | G0 | Inspect returned result metadata and sidecars: require at least 1 resolved standard citation record, with edition and clause title matching the approved curve, count | Known gap: 0 records in router payload; no standard compliance asserted, issue pending (2). [src/digitalmodel/spectral_fatigue/workflow.py:L60-L76] |
| C04 | G1 | Execute R01 and R02 by their exact section-4 nodes; require 2 pass, 0 fail, 0 skip, count | Historical PASS, BASE and AFTER; the R01 keel-joint pass depends on #2583 (corrected Dirlik margin approximately 0.94 < 1.0). Re-baseline R01 and C04 with that repair. [tests/workflows/test_durable_workflows.py:L364-L401] [tests/workflows/test_durable_workflows.py:L1893-L1950] |
| C05 | G1 | Run all tests in the three dependency/comparison files named in section 4; require 63 pass, 0 fail, 0 skip, count | PASS, BASE and AFTER; these include sibling implementations rather than direct router tests. [tests/fatigue/test_spectral_fatigue.py:L7-L14] [tests/test_spectral_fatigue.py:L6-L14] [tests/test_spectral_fatigue_validation.py:L24-L42] |
| C06 | G2 | Read detail CSV, independently sum full damage × occurrence by location and compare `annual_damage`; check life × damage = 1 and margin × required life / life = 1; maximum relative error ≤ 1e-9 dimensionless | PASS on shipped example; checks only accumulation and reporting algebra, not the spectral-damage reference. [src/digitalmodel/spectral_fatigue/workflow.py:L97-L125] [tests/workflows/test_durable_workflows.py:L385-L401] |
| C07 | G2 | Integrate the rectangular PSD in the recipe below and compare `moments.nu_0` with a separate trapezoidal Hz-moment calculation; relative discrepancy ≤ 1e-9 dimensionless; report both in Hz | Known dependency defect #2583: observed 0.159 Hz versus 1.000 Hz, ratio 6.3 (rounded); fails qualification. [src/digitalmodel/fatigue/spectral_fatigue.py:L64-L71] [src/digitalmodel/fatigue/spectral_fatigue.py:L133-L143] |
| C08 | G2 | Compute BT / narrow-band annual damage for the synthetic moments in the recipe; require the claimed wide-band interpolation to remain finite and between 0 and 1, dimensionless, pending independent qualification of that bound | Known dependency defect #2775: ratio 2.8, above 1; positivity tests alone do not detect this. [src/digitalmodel/fatigue/spectral_fatigue.py:L345-L348] [src/digitalmodel/fatigue/spectral_fatigue.py:L364-L390] [tests/fatigue/test_spectral_fatigue.py:L122-L140] |
| C09 | G5 | For R01, require detail rows = 3, summary rows = 2, verdict mismatches = 0, count; retain SHA-256 for 1 input and 2 CSV outputs with check results; require 0 missing evidence files | PASS for local evidence capture; signed appendix and provenance remain open, issue pending (2). [tests/workflows/test_durable_workflows.py:L364-L401] [src/digitalmodel/spectral_fatigue/workflow.py:L54-L76] |
| C10 | G2 | For the C07 rectangular PSD, compare Dirlik / narrow-band annual damage at S-N slope 3 and log intercept 12; require finite damage ratio with absolute departure from 1 ≤ 0.02 (2%), dimensionless | Known dependency defect #2746, item 2: ratio 49.8, outside tolerance despite α2 approaching 1; R01 does not expose the substitution because its bandwidth and irregularity are similar. [src/digitalmodel/fatigue/spectral_fatigue.py:L74-L82] [src/digitalmodel/fatigue/spectral_fatigue.py:L291-L327] |

Executable preflight and dependency checks (run against the pinned source; C07, C08 and C10 deliberately expose unchanged defects and must run separately):

```python
import math
from digitalmodel.spectral_fatigue.workflow import (
    _positive_float, _locations, _sea_states, _method, _psd, _occurrence,
)
from digitalmodel.fatigue.spectral_fatigue import (
    compute_spectral_moments, SpectralMoments,
    benasciutti_tovo_damage, narrow_band_damage, dirlik_damage,
)

# C01
cases = [
    (_positive_float, ({}, "dff")),
    (_positive_float, ({"dff": 0}, "dff")),
    (_locations, ({},)),
    (_sea_states, ({}, "detail")),
    (_method, ({"method": "unknown"},)),
    (_psd, ({"frequency_Hz": [1], "stress_psd_MPa2_Hz": [1]}, "detail")),
    (_occurrence, ({"occurrence_fraction": 0}, "detail")),
    (_positive_float, ({"dff": "1,000"}, "dff")),
]
rejected = 0
for fn, args in cases:
    try:
        fn(*args)
    except ValueError:
        rejected += 1
assert rejected == 8

# C02; settings is the input spectral_fatigue mapping before router replaces it.
def audit(settings):
    scalars = [settings["design_life_years"], settings["dff"],
               settings["sn_curve"]["slope"], settings["sn_curve"]["log_intercept"]]
    assert all(math.isfinite(float(x)) and float(x) > 0 for x in scalars)
    ids = [loc["id"] for loc in settings["locations"]]
    assert len(ids) == len(set(ids))
    for loc in settings["locations"]:
        total = 0.0
        for sea in loc["sea_states"]:
            f, psd = _psd(sea, loc["id"])
            assert all(math.isfinite(x) and x >= 0 for x in f + psd)
            assert all(b > a for a, b in zip(f, f[1:]))
            total += _occurrence(sea, loc["id"])
        assert abs(total - 1.0) <= 1e-9

# C07, independent frequency convention; not a reference to damaged library output.
m = compute_spectral_moments([0.99, 1.0, 1.01], [1.0, 1.0, 1.0])
expected_Hz = math.sqrt((0.99**2 + 2.0 + 1.01**2) / 4.0)
assert abs(m.nu_0 / expected_Hz - 1) <= 1e-9  # Known failure, #2583.

# C08, execute separately after C07's known failure.
m = SpectralMoments(m0=100, m1=500, m2=5000, m4=2000000)
ratio = (benasciutti_tovo_damage(m, 3, 12).damage_per_year /
         narrow_band_damage(m, 3, 12).damage_per_year)
assert math.isfinite(ratio) and 0 <= ratio <= 1  # Known failure, #2775.

# C10, execute separately; narrow-band limit cancels the shared rate defect.
m = compute_spectral_moments([0.99, 1.0, 1.01], [1.0, 1.0, 1.0])
ratio = (dirlik_damage(m, 3, 12).damage_per_year /
         narrow_band_damage(m, 3, 12).damage_per_year)
assert math.isfinite(ratio) and abs(ratio - 1) <= 0.02  # Known failure, #2746.
```

## 6. Known failure modes and pitfalls

sweep: 18 found

The count covers distinct reviewed default, conversion, error-path, unit and output behaviors, including guarded cases; it is not a count of exceptions or confirmed defects. The package sweep used the following commands and a line-by-line review of all returned definitions. [src/digitalmodel/spectral_fatigue/workflow.py:L37-L237]

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05' src/digitalmodel/spectral_fatigue --glob '*.py'
rg -n 'float\(|int\(|get\(|or \{\}|Hz|MPa|year|cache|open\(' src/digitalmodel/spectral_fatigue --glob '*.py'
```

| Item | Symptom / cause at code pin | Existing detection / disposition |
| --- | --- | --- |
| S01 | Missing/falsy settings become `{}`; required scalar checks then raise rather than returning an empty success. [src/digitalmodel/spectral_fatigue/workflow.py:L38-L40] | C01; guarded behavior, issue pending (1) for explicit coverage |
| S02 | Missing ID and name collapse locations onto the same literal identifier; no uniqueness check. [src/digitalmodel/spectral_fatigue/workflow.py:L88-L89] | C02 caller audit; issue pending (1) |
| S03 | Empty row/summary containers and annual damage initialized to zero are accumulators; `_sea_states` rejects an empty list before the loop. [src/digitalmodel/spectral_fatigue/workflow.py:L44-L52] [src/digitalmodel/spectral_fatigue/workflow.py:L89-L98] [src/digitalmodel/spectral_fatigue/workflow.py:L153-L157] | C01; guarded behavior, issue pending (1) for negative-case coverage |
| S04 | Nonpositive annual damage raises, but NaN evades the comparison and can reach life/margin selection. [src/digitalmodel/spectral_fatigue/workflow.py:L113-L125] | C02 prevents nonfinite inputs; no detection check exists for nonfinite intermediate outputs; issue pending (1) |
| S05 | Omitted method silently chooses Dirlik; invalid explicit method raises. [src/digitalmodel/spectral_fatigue/workflow.py:L130-L136] | C01 invalid-method guard and R01 explicit selection; no detection check exists for accidental omission; issue pending (1) |
| S06 | Falsy S-N mapping becomes `{}`; both scalar parameters are still required. [src/digitalmodel/spectral_fatigue/workflow.py:L139-L143] [src/digitalmodel/spectral_fatigue/workflow.py:L182-L190] | C01 required-scalar guard; issue pending (1) for curve-specific coverage |
| S07 | PSD lists are checked for matching length and at least 2 entries, but not order, signs or finite values. [src/digitalmodel/spectral_fatigue/workflow.py:L160-L171] | C01 length guard and C02 caller audit; issue pending (1) |
| S08 | Each external PSD/frequency element uses `float`; comma decimal or thousands text raises, while `nan` and `inf` convert. [src/digitalmodel/spectral_fatigue/workflow.py:L171-L171] | C02 finiteness audit; C01 demonstrates scalar comma rejection; no array-locale detection check exists; issue pending (1) |
| S09 | Missing occurrence defaults to 1.0 per state; multiple omitted states can count several full years. [src/digitalmodel/spectral_fatigue/workflow.py:L174-L179] [src/digitalmodel/spectral_fatigue/workflow.py:L97-L98] | C02 sum-to-one audit; issue pending (1) |
| S10 | Occurrence `float` conversion rejects nonpositive values only; values above 1, infinity or NaN are not rejected here. [src/digitalmodel/spectral_fatigue/workflow.py:L174-L179] | C01 zero guard and C02 audit; issue pending (1) |
| S11 | Required positive scalar conversion accepts nonfinite values; text has Python `float` conventions, with no locale normalization. [src/digitalmodel/spectral_fatigue/workflow.py:L182-L190] | C01 missing/zero/comma guards, C02 finite audit; issue pending (1) |
| S12 | Default output directory is `results`, resolved relative to the selected configuration directory. [src/digitalmodel/spectral_fatigue/workflow.py:L201-L205] | R01 exercises explicit `results`; no detection check exists for omitted-directory intent; issue pending (1) |
| S13 | Directory hint takes precedence over file parent; absent hints fall back to current directory, changing output destination with invocation context. [src/digitalmodel/spectral_fatigue/workflow.py:L208-L213] | no detection check exists; issue pending (1) |
| S14 | File stem takes precedence over basename, whose fallback is `spectral_fatigue`; reused stems overwrite the same output names. [src/digitalmodel/spectral_fatigue/workflow.py:L193-L219] | no detection check exists for collisions; issue pending (1) |
| S15 | `_display_path` catches `ValueError` and returns a resolved absolute path for outputs outside repository root; the exception is a path-format fallback, not a swallowed calculation error. [src/digitalmodel/spectral_fatigue/workflow.py:L232-L237] | no detection check exists for path disclosure; issue pending (1) |
| S16 | Empty results raise before CSV writing. [src/digitalmodel/spectral_fatigue/workflow.py:L222-L224] | no dedicated detection check exists; issue pending (1) |
| S17 | CSV files use replacement writes and default encoding, with no atomic pair or rollback between detail and summary. [src/digitalmodel/spectral_fatigue/workflow.py:L54-L58] [src/digitalmodel/spectral_fatigue/workflow.py:L225-L229] | C09 counts successful files only; no detection check exists for partial-write recovery; issue pending (1) |
| S18 | Caller-supplied Hz, MPa²/Hz and annual occurrence units are assumed; estimator is called for unit duration then annualized by the dependency. [src/digitalmodel/spectral_fatigue/workflow.py:L93-L108] [src/digitalmodel/fatigue/spectral_fatigue.py:L171-L195] | C07 detects Hz convention defect #2583; C02 audits occurrence; metadata conversion check remains issue pending (1) |

| Additional confirmed dependency or evidence pitfall | Detection / incident |
| --- | --- |
| Hz moments are paired with angular-rate conversion, propagating an understated damage rate through all methods. [src/digitalmodel/fatigue/spectral_fatigue.py:L64-L71] [src/digitalmodel/fatigue/spectral_fatigue.py:L138-L141] | C07; #2583. The R01 keel-joint pass is defect-dependent: corrected Dirlik margin is approximately 0.94 < 1.0. Re-baseline R01 and C04 with the rate repair. |
| BT factor exceeds the narrow-band bound for the supplied wide-band moments, despite the interpolation description. [src/digitalmodel/fatigue/spectral_fatigue.py:L345-L348] [src/digitalmodel/fatigue/spectral_fatigue.py:L364-L390] | C08; #2775 |
| Default Dirlik uses bandwidth ε instead of irregularity γ = α2 in D1, R, D2 and Q and clamps the resulting mixture; existing tests check positivity, a broad bound and annualization rather than this narrow-band limit. [src/digitalmodel/fatigue/spectral_fatigue.py:L294-L305] [tests/fatigue/test_spectral_fatigue.py:L99-L119] | C10; #2746, item 2; known failure |
| Standard edition and machine-readable citation evidence are absent from the router output; same-library equality is insufficient validation. [src/digitalmodel/spectral_fatigue/workflow.py:L60-L76] [tests/workflows/test_durable_workflows.py:L1941-L1950] | C03; issue pending (2) |

The complete source contains no record-skipping `continue`, `return None`, empty-result error return, epoch fallback, percentage fallback or cache write; its sole caught exception is S15. [src/digitalmodel/spectral_fatigue/workflow.py:L37-L237]

## 7. Lessons log

| Date | Change reference | Lesson / evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-18 | Initial pack revision | Library agreement cannot independently verify the same library's frequency convention. [tests/workflows/test_durable_workflows.py:L1943-L1950] [src/digitalmodel/fatigue/spectral_fatigue.py:L64-L71] | R02 classified as regression; C07 and #2583 recorded | Not assigned; issue pending (2) |

## 8. Ownership

Proposed review responsibilities: engineering calculations quality manager; deputy engineering calculations quality manager from a different provider. Maintainers select the reviewers and own the open items. Unresolved tolerance, input basis or standard-edition questions go to the family quality manager; cross-family questions go to the maintainers. These are review assignments for this draft, not evidence of completed review.

## 9. Dependencies

The router delegates the four estimators and moment calculation to `digitalmodel.fatigue.spectral_fatigue`; its runtime dependencies include NumPy, SciPy and Pydantic in that library, while CSV and path handling use the standard library. [src/digitalmodel/spectral_fatigue/workflow.py:L11-L22] [src/digitalmodel/fatigue/spectral_fatigue.py:L31-L36]

The engine is the upstream configuration router; per-state and per-location CSVs and the replaced configuration mapping are downstream products. [src/digitalmodel/engine.py:L340-L345] [src/digitalmodel/spectral_fatigue/workflow.py:L54-L76] [src/digitalmodel/spectral_fatigue/workflow.py:L99-L127]

The durable registry separately declares a parametric atlas using fixed stress-gain inputs; that generator is outside the supplied-PSD package and is not a second implementation inside this pack's boundary. [docs/registry/workflows.yaml:L179-L189]

### Implementation-map inventory

Map evidence in this section is pinned to `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` on `origin/codex/implmap-work`; its recorded data revision is `270c8759c374b67a7f032953e7fcdb14e0224461`. This map pin is on a side branch and is not an ancestor of main at `6476218e8f5af3fa7d16c3dafc60b6a9084c6e6c`; the map blob there differs. The 534 map citations resolve only at the separate map pin, not the header code pin, and remain fetchable through `refs/pull/2545/head`, which retains the head of merged PR #2545 after side-branch deletion. All other source evidence uses the header's code pin. Row relations below are the map's labels, not independent endorsements of those classifications. [docs/registry/implementation-map.yaml:L1-L3]

Counted families, ranked by in-package rows: `sn-curves-and-miner-damage` (2), then `wave-spectra` (1); both count because fewer than three families have rows in the package. The inventory below retains every row in both families, including scripts, tests and implementations outside the package. [src/digitalmodel/spectral_fatigue/workflow.py:L37-L49] [src/digitalmodel/spectral_fatigue/workflow.py:L93-L98] [src/digitalmodel/spectral_fatigue/workflow.py:L139-L143]

#### wave-spectra — 141 map rows

| Row | Path | Definition | Map relation / evidence |
| --- | --- | --- | --- |
| 1 | `docs/domains/orcaflex/examples/raw/K01/PythonController.py` | `Actuator.output` | independent; [docs/registry/implementation-map.yaml:L13600-L13611] |
| 2 | `docs/domains/orcaflex/examples/raw/K02/BladedControllerWrapper.py` | `Actuator.output` | independent; [docs/registry/implementation-map.yaml:L13612-L13623] |
| 3 | `examples/domains/fpso/fpso_mooring_analysis.py` | `define_environmental_conditions` | example; [docs/registry/implementation-map.yaml:L13624-L13635] |
| 4 | `examples/domains/fpso/fpso_mooring_analysis.py` | `generate_visualizations` | example; [docs/registry/implementation-map.yaml:L13636-L13647] |
| 5 | `examples/domains/fpso/fpso_mooring_analysis.py` | `main` | example; [docs/registry/implementation-map.yaml:L13648-L13662] |
| 6 | `examples/prototypes/orcaflex-agent/generate_test.py` | `main` | example; [docs/registry/implementation-map.yaml:L13663-L13674] |
| 7 | `scripts/_seed_eq_mock.py` | `mock_time_series` | independent; [docs/registry/implementation-map.yaml:L13675-L13686] |
| 8 | `scripts/benchmark/extract_pdf_plots.py` | `None` | independent; [docs/registry/implementation-map.yaml:L13687-L13697] |
| 9 | `scripts/benchmark/validate_owd_vs_spec.py` | `None` | independent; [docs/registry/implementation-map.yaml:L13698-L13708] |
| 10 | `scripts/extract_property_inventory.py` | `build_dormancy_rules` | independent; [docs/registry/implementation-map.yaml:L13709-L13720] |
| 11 | `scripts/generate_env_files.py` | `create_wave_file` | independent; [docs/registry/implementation-map.yaml:L13721-L13732] |
| 12 | `scripts/generate_env_files.py` | `main` | independent; [docs/registry/implementation-map.yaml:L13733-L13746] |
| 13 | `scripts/ingest_orcina_help.py` | `None` | independent; [docs/registry/implementation-map.yaml:L13747-L13757] |
| 14 | `scripts/python/digitalmodel/legacy/19_-_DistanceBetweenTwoVessels.py` | `None` | independent; [docs/registry/implementation-map.yaml:L13758-L13768] |
| 15 | `scripts/python/digitalmodel/legacy/26_-_LinkedStatistics.py` | `None` | independent; [docs/registry/implementation-map.yaml:L13769-L13779] |
| 16 | `scripts/python/digitalmodel/legacy/27_-_RainflowHalfCycles.py` | `None` | independent; [docs/registry/implementation-map.yaml:L13780-L13790] |
| 17 | `scripts/python/digitalmodel/legacy/29_-_ExtremeStatisticsRayleigh.py` | `None` | independent; [docs/registry/implementation-map.yaml:L13791-L13801] |
| 18 | `scripts/python/digitalmodel/legacy/30_-_ExtremeStatisticsLikelihood.py` | `None` | independent; [docs/registry/implementation-map.yaml:L13802-L13812] |
| 19 | `scripts/python/digitalmodel/legacy/31_-_ExtremeStatisticsAggregation.py` | `None` | independent; [docs/registry/implementation-map.yaml:L13813-L13823] |
| 20 | `scripts/python/digitalmodel/legacy/32_-_FrequencyDomainResults.py` | `None` | independent; [docs/registry/implementation-map.yaml:L13824-L13834] |
| 21 | `scripts/python/digitalmodel/legacy/33_-_FrequencyDomainMPM.py` | `None` | independent; [docs/registry/implementation-map.yaml:L13835-L13845] |
| 22 | `scripts/python/digitalmodel/legacy/34_-_FrequencyDomainRangeGraph.py` | `None` | independent; [docs/registry/implementation-map.yaml:L13846-L13856] |
| 23 | `scripts/python/digitalmodel/legacy/35_-_FrequencyDomainSpectralDensitySpectralResponse.py` | `None` | independent; [docs/registry/implementation-map.yaml:L13857-L13867] |
| 24 | `scripts/python/digitalmodel/legacy/36_-_FrequencyDomainTimeHistorySynthesis.py` | `None` | independent; [docs/registry/implementation-map.yaml:L13868-L13878] |
| 25 | `scripts/python/digitalmodel/legacy/analyze_marine_excel.py` | `analyze_excel_file` | independent; [docs/registry/implementation-map.yaml:L13879-L13890] |
| 26 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.profile_wave_spectrum` | independent; [docs/registry/implementation-map.yaml:L13891-L13902] |
| 27 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.profile_wave_spectrum.run_spectrum` | independent; [docs/registry/implementation-map.yaml:L13903-L13914] |
| 28 | `scripts/python/digitalmodel/orcawave/mcp_orcawave.py` | `freq_analyze` | independent; [docs/registry/implementation-map.yaml:L13915-L13926] |
| 29 | `src/digitalmodel/drilling_riser/global_model/build.py` | `_environment_and_simulation` | independent; [docs/registry/implementation-map.yaml:L13927-L13938] |
| 30 | `src/digitalmodel/fatigue/scf_library.py` | `short_chord_factor_f1` | independent; [docs/registry/implementation-map.yaml:L13939-L13950] |
| 31 | `src/digitalmodel/hydrodynamics/cli.py` | `spectrum_cmd` | independent; [docs/registry/implementation-map.yaml:L13951-L13964] |
| 32 | `src/digitalmodel/hydrodynamics/seakeeping.py` | `operability_analysis` | independent; [docs/registry/implementation-map.yaml:L13965-L13976] |
| 33 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra._normalize_to_significant_height` | independent; [docs/registry/implementation-map.yaml:L13977-L13988] |
| 34 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.jonswap` | independent; [docs/registry/implementation-map.yaml:L13989-L14004] |
| 35 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.pierson_moskowitz` | independent; [docs/registry/implementation-map.yaml:L14005-L14017] |
| 36 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.bretschneider` | independent; [docs/registry/implementation-map.yaml:L14018-L14030] |
| 37 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.issc` | independent; [docs/registry/implementation-map.yaml:L14031-L14043] |
| 38 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.generate_spectrum` | wrapper; [docs/registry/implementation-map.yaml:L14044-L14060] |
| 39 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.significant_height_from_spectrum` | independent; [docs/registry/implementation-map.yaml:L14061-L14072] |
| 40 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.zero_crossing_period_from_spectrum` | independent; [docs/registry/implementation-map.yaml:L14073-L14084] |
| 41 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.spectrum_statistics` | independent; [docs/registry/implementation-map.yaml:L14085-L14096] |
| 42 | `src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py` | `router` | independent; [docs/registry/implementation-map.yaml:L14097-L14110] |
| 43 | `src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py` | `_spectrum_name` | independent; [docs/registry/implementation-map.yaml:L14111-L14122] |
| 44 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.wave` | independent; [docs/registry/implementation-map.yaml:L14123-L14134] |
| 45 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.RayleighDampingCoefficients` | independent; [docs/registry/implementation-map.yaml:L14135-L14146] |
| 46 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `_complex_rao` | independent; [docs/registry/implementation-map.yaml:L14147-L14158] |
| 47 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `crane_tip_raos` | independent; [docs/registry/implementation-map.yaml:L14159-L14172] |
| 48 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `crane_tip_significant_motion` | independent; [docs/registry/implementation-map.yaml:L14173-L14186] |
| 49 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `crane_tip_significant_velocity` | independent; [docs/registry/implementation-map.yaml:L14187-L14199] |
| 50 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `_jonswap_spectrum` | independent; [docs/registry/implementation-map.yaml:L14200-L14213] |
| 51 | `src/digitalmodel/marine_ops/marine_analysis/analysis/excel_analyzer.py` | `analyze_excel_file` | copy; [docs/registry/implementation-map.yaml:L14214-L14225] |
| 52 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.profile_wave_spectrum` | independent; [docs/registry/implementation-map.yaml:L14226-L14237] |
| 53 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.profile_wave_spectrum.run_spectrum` | independent; [docs/registry/implementation-map.yaml:L14238-L14249] |
| 54 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._wave_spectrum` | independent; [docs/registry/implementation-map.yaml:L14250-L14261] |
| 55 | `src/digitalmodel/marine_ops/operation_envelope.py` | `operation_envelope` | independent; [docs/registry/implementation-map.yaml:L14262-L14275] |
| 56 | `src/digitalmodel/marine_ops/operation_envelope.py` | `_significant_response` | independent; [docs/registry/implementation-map.yaml:L14276-L14287] |
| 57 | `src/digitalmodel/motion_forecast/wave_forecast.py` | `synthesize_directional_forecast` | independent; [docs/registry/implementation-map.yaml:L14288-L14301] |
| 58 | `src/digitalmodel/motion_forecast/wave_source.py` | `jonswap_spectrum` | independent; [docs/registry/implementation-map.yaml:L14302-L14315] |
| 59 | `src/digitalmodel/motion_forecast/wave_source.py` | `synthesize_forecast` | independent; [docs/registry/implementation-map.yaml:L14316-L14328] |
| 60 | `src/digitalmodel/orcaflex/environment.py` | `WaveSpectrumParams.tz` | independent; [docs/registry/implementation-map.yaml:L14329-L14342] |
| 61 | `src/digitalmodel/orcaflex/environment.py` | `WaveSpectrumParams.spectral_density` | independent; [docs/registry/implementation-map.yaml:L14343-L14356] |
| 62 | `src/digitalmodel/orcaflex/environment.py` | `WaveSpectrumParams.to_orcaflex_dict` | independent; [docs/registry/implementation-map.yaml:L14357-L14368] |
| 63 | `src/digitalmodel/orcawave/motion_statistics.py` | `compute_response_spectrum` | independent; [docs/registry/implementation-map.yaml:L14369-L14380] |
| 64 | `src/digitalmodel/orcawave/wave_spectrum.py` | `pierson_moskowitz` | independent; [docs/registry/implementation-map.yaml:L14381-L14392] |
| 65 | `src/digitalmodel/orcawave/wave_spectrum.py` | `jonswap` | independent; [docs/registry/implementation-map.yaml:L14393-L14408] |
| 66 | `src/digitalmodel/orcawave/wave_spectrum.py` | `bretschneider` | wrapper; [docs/registry/implementation-map.yaml:L14409-L14421] |
| 67 | `src/digitalmodel/orcawave/wave_spectrum.py` | `issc_spectrum` | independent; [docs/registry/implementation-map.yaml:L14422-L14433] |
| 68 | `src/digitalmodel/orcawave/wave_spectrum.py` | `ochi_hubble._component` | independent; [docs/registry/implementation-map.yaml:L14434-L14445] |
| 69 | `src/digitalmodel/orcawave/wave_spectrum.py` | `torsethaugen` | independent; [docs/registry/implementation-map.yaml:L14446-L14459] |
| 70 | `src/digitalmodel/orcawave/wave_spectrum.py` | `generate_spectrum` | independent; [docs/registry/implementation-map.yaml:L14460-L14477] |
| 71 | `src/digitalmodel/parametric/generate.py` | `_spectral_fatigue_annual_damage` | independent; [docs/registry/implementation-map.yaml:L14478-L14489] |
| 72 | `src/digitalmodel/parametric/generate.py` | `_esp_pump_utilisation` | independent; [docs/registry/implementation-map.yaml:L14490-L14501] |
| 73 | `src/digitalmodel/rao_spectral_fatigue/workflow.py` | `_stress_psd_from_rao` | independent; [docs/registry/implementation-map.yaml:L14502-L14513] |
| 74 | `src/digitalmodel/solvers/benchmark/solvers.py` | `run_orcaflex` | independent; [docs/registry/implementation-map.yaml:L14514-L14525] |
| 75 | `src/digitalmodel/solvers/fea_model/environment_components.py` | `Environment.get_wave_waveTrains` | wrapper; [docs/registry/implementation-map.yaml:L14526-L14539] |
| 76 | `src/digitalmodel/solvers/fea_model/environment_components.py` | `Environment.getJONSWAPGamma` | independent; [docs/registry/implementation-map.yaml:L14540-L14551] |
| 77 | `src/digitalmodel/solvers/fea_model/environment_components.py` | `Environment.RayleighDampingCoefficients` | copy; [docs/registry/implementation-map.yaml:L14552-L14563] |
| 78 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py` | `EnvironmentBuilder._build_wave_trains` | independent; [docs/registry/implementation-map.yaml:L14564-L14577] |
| 79 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py` | `EnvironmentBuilder._order_wave_train` | independent; [docs/registry/implementation-map.yaml:L14578-L14591] |
| 80 | `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` | `CampaignSpec.from_legacy_config` | independent; [docs/registry/implementation-map.yaml:L14592-L14605] |
| 81 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.wave` | copy; [docs/registry/implementation-map.yaml:L14606-L14617] |
| 82 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.RayleighDampingCoefficients` | copy; [docs/registry/implementation-map.yaml:L14618-L14629] |
| 83 | `src/digitalmodel/spectral_fatigue/workflow.py` | `_evaluate_location` | independent; [docs/registry/implementation-map.yaml:L14630-L14641] |
| 84 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_psd` | independent; [docs/registry/implementation-map.yaml:L14642-L14655] |
| 85 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._perform_frequency_validation` | independent; [docs/registry/implementation-map.yaml:L14656-L14667] |
| 86 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `NarrowBandMethod.calculate_damage_rate` | independent; [docs/registry/implementation-map.yaml:L14668-L14679] |
| 87 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `DirlikMethod.calculate_damage_rate` | independent; [docs/registry/implementation-map.yaml:L14680-L14691] |
| 88 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `TovoBenasciuttiMethod.calculate_damage_rate` | independent; [docs/registry/implementation-map.yaml:L14692-L14703] |
| 89 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `SingleMomentMethod.calculate_damage_rate` | independent; [docs/registry/implementation-map.yaml:L14704-L14715] |
| 90 | `src/digitalmodel/structural/fatigue/scatter_fatigue.py` | `_resolve_wave_spectrum_func` | independent; [docs/registry/implementation-map.yaml:L14716-L14727] |
| 91 | `src/digitalmodel/structural/fatigue/scatter_fatigue.py` | `_resolve_wave_spectrum_func._jonswap_wrapper` | independent; [docs/registry/implementation-map.yaml:L14728-L14739] |
| 92 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` | `orcaflexModel` | independent; [docs/registry/implementation-map.yaml:L14740-L14751] |
| 93 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` | `wave` | independent; [docs/registry/implementation-map.yaml:L14752-L14763] |
| 94 | `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `jonswap_gamma` | independent; [docs/registry/implementation-map.yaml:L14764-L14775] |
| 95 | `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `jonswap_spectrum` | independent; [docs/registry/implementation-map.yaml:L14776-L14791] |
| 96 | `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `compute_wave_velocity` | independent; [docs/registry/implementation-map.yaml:L14792-L14806] |
| 97 | `src/digitalmodel/vessel_seakeeping/workflow.py` | `_spectrum_type` | independent; [docs/registry/implementation-map.yaml:L14807-L14818] |
| 98 | `src/digitalmodel/workflows/installation_full_report.py` | `_design` | independent; [docs/registry/implementation-map.yaml:L14819-L14830] |
| 99 | `src/digitalmodel/workflows/installation_report_sections.py` | `design_data` | independent; [docs/registry/implementation-map.yaml:L14831-L14842] |
| 100 | `src/digitalmodel/workflows/installation_seastates.py` | `_configure` | independent; [docs/registry/implementation-map.yaml:L14843-L14854] |
| 101 | `src/digitalmodel/workflows/installation_seastates.py` | `_verify` | independent; [docs/registry/implementation-map.yaml:L14855-L14866] |
| 102 | `src/digitalmodel/workflows/installation_seastates.py` | `_change_payload` | independent; [docs/registry/implementation-map.yaml:L14867-L14878] |
| 103 | `src/digitalmodel/workflows/workflow_automation/workflows.py` | `VesselResponseWorkflow.create` | independent; [docs/registry/implementation-map.yaml:L14879-L14892] |
| 104 | `tests/hydrodynamics/rao_analysis/conftest.py` | `wave_spectrum` | test_helper; [docs/registry/implementation-map.yaml:L14893-L14906] |
| 105 | `tests/hydrodynamics/test_seakeeping.py` | `jonswap_spectrum` | test_helper; [docs/registry/implementation-map.yaml:L14907-L14920] |
| 106 | `tests/marine_ops/marine_engineering/integration/integration_example.py` | `main` | test_helper; [docs/registry/implementation-map.yaml:L14921-L14932] |
| 107 | `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_jonswap_generates_valid_spectrum` | test_helper; [docs/registry/implementation-map.yaml:L14933-L14944] |
| 108 | `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_zero_crossing_period_relationship` | test_helper; [docs/registry/implementation-map.yaml:L14945-L14956] |
| 109 | `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_wave_elevation_time_series_generation` | test_helper; [docs/registry/implementation-map.yaml:L14957-L14968] |
| 110 | `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_wave_energy_distribution` | test_helper; [docs/registry/implementation-map.yaml:L14969-L14980] |
| 111 | `tests/marine_ops/marine_engineering/legacy/test_validation.py` | `test_jonswap_hs_recovery` | test_helper; [docs/registry/implementation-map.yaml:L14981-L14992] |
| 112 | `tests/marine_ops/marine_engineering/legacy/test_validation.py` | `test_pierson_moskowitz_hs_recovery` | test_helper; [docs/registry/implementation-map.yaml:L14993-L15004] |
| 113 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_spectral_moments_accuracy` | test_helper; [docs/registry/implementation-map.yaml:L15005-L15016] |
| 114 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_hs_from_m0_relationship` | test_helper; [docs/registry/implementation-map.yaml:L15017-L15028] |
| 115 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_statistics` | test_helper; [docs/registry/implementation-map.yaml:L15029-L15040] |
| 116 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_gaussian_distribution` | test_helper; [docs/registry/implementation-map.yaml:L15041-L15052] |
| 117 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestPublishedBenchmarks.test_jonswap_vs_published_curves` | test_helper; [docs/registry/implementation-map.yaml:L15053-L15064] |
| 118 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_spectral_moments_accuracy` | test_helper; [docs/registry/implementation-map.yaml:L15065-L15076] |
| 119 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_hs_from_m0_relationship` | test_helper; [docs/registry/implementation-map.yaml:L15077-L15088] |
| 120 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_statistics` | test_helper; [docs/registry/implementation-map.yaml:L15089-L15100] |
| 121 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_gaussian_distribution` | test_helper; [docs/registry/implementation-map.yaml:L15101-L15112] |
| 122 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestPublishedBenchmarks.test_jonswap_vs_published_curves` | test_helper; [docs/registry/implementation-map.yaml:L15113-L15124] |
| 123 | `tests/orcaflex/test_model_default_tokens.py` | `wave_order_findings` | test_helper; [docs/registry/implementation-map.yaml:L15125-L15136] |
| 124 | `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` | `_write_floating_campaign` | test_helper; [docs/registry/implementation-map.yaml:L15137-L15148] |
| 125 | `tests/solvers/orcaflex/modular_generator/test_campaign_generator.py` | `_make_campaign_data` | test_helper; [docs/registry/implementation-map.yaml:L15149-L15160] |
| 126 | `tests/structural/fatigue/test_frequency_domain.py` | `TestSpectralMomentsCalculation.test_white_noise_m0_equals_level_times_bandwidth` | test_helper; [docs/registry/implementation-map.yaml:L15161-L15172] |
| 127 | `tests/structural/fatigue/test_scatter_fatigue.py` | `_jonswap_hz` | test_helper; [docs/registry/implementation-map.yaml:L15173-L15184] |
| 128 | `tests/unit/hydrodynamics/test_rao_analysis.py` | `simple_wave_params` | test_helper; [docs/registry/implementation-map.yaml:L15185-L15198] |
| 129 | `tests/unit/hydrodynamics/test_rao_analysis.py` | `TestWaveSpectra.test_jonswap_peak_near_tp` | test_helper; [docs/registry/implementation-map.yaml:L15199-L15210] |
| 130 | `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestPiersonMoskowitzExtended.test_pm_peak_near_tp` | test_helper; [docs/registry/implementation-map.yaml:L15211-L15222] |
| 131 | `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestBretschneiderExtended.test_bretschneider_peak_near_tp` | test_helper; [docs/registry/implementation-map.yaml:L15223-L15234] |
| 132 | `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestZeroCrossingPeriodFromSpectrum.test_tz_pm_approximately_0711_tp` | test_helper; [docs/registry/implementation-map.yaml:L15235-L15246] |
| 133 | `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestPeakFrequencyFromSpectrum.test_peak_frequency_near_2pi_over_tp` | test_helper; [docs/registry/implementation-map.yaml:L15247-L15258] |
| 134 | `tests/workflows/hydrodynamics_assertions.py` | `_assert_wave_spectra` | test_helper; [docs/registry/implementation-map.yaml:L15259-L15270] |
| 135 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | test_helper; [docs/registry/implementation-map.yaml:L15271-L15284] |
| 136 | `tests/workflows/test_durable_workflows.py` | `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | test_helper; [docs/registry/implementation-map.yaml:L15285-L15298] |
| 137 | `tests/workflows/test_durable_workflows.py` | `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | test_helper; [docs/registry/implementation-map.yaml:L15299-L15314] |
| 138 | `tests/workflows/test_durable_workflows.py` | `test_vessel_seakeeping_significant_amplitude_matches_library` | test_helper; [docs/registry/implementation-map.yaml:L15315-L15328] |
| 139 | `tests/workflows/test_durable_workflows.py` | `test_esp_pump_hydraulics_tdh_and_sizing_closed_form` | test_helper; [docs/registry/implementation-map.yaml:L15329-L15342] |
| 140 | `src/digitalmodel/fatigue/fatg.py` | `fatg_significant_stress_range` | caller; [docs/registry/implementation-map.yaml:L15343-L15361] |
| 141 | `src/digitalmodel/fatigue/fatg.py` | `fatg_significant_stress_range.ordinate` | independent; [docs/registry/implementation-map.yaml:L15362-L15377] |

#### sn-curves-and-miner-damage — 392 map rows

| Row | Path | Definition | Map relation / evidence |
| --- | --- | --- | --- |
| 1 | `docs/domains/examples/orcaflex_signal_analysis_example.py` | `example_6_custom_workflow` | independent; [docs/registry/implementation-map.yaml:L16814-L16823] |
| 2 | `docs/domains/examples/signal_analysis_usage_example.py` | `example_1_basic_rainflow_counting` | independent; [docs/registry/implementation-map.yaml:L16824-L16833] |
| 3 | `docs/domains/examples/signal_analysis_usage_example.py` | `example_4_fatigue_analysis` | independent; [docs/registry/implementation-map.yaml:L16834-L16843] |
| 4 | `docs/domains/examples/signal_analysis_usage_example.py` | `example_6_backward_compatibility` | independent; [docs/registry/implementation-map.yaml:L16844-L16853] |
| 5 | `docs/domains/examples/signal_analysis_usage_example.py` | `main` | independent; [docs/registry/implementation-map.yaml:L16854-L16867] |
| 6 | `examples/demos/demo_sn_library.py` | `main` | example; [docs/registry/implementation-map.yaml:L16868-L16879] |
| 7 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_sn_curves` | example; [docs/registry/implementation-map.yaml:L16880-L16889] |
| 8 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_rainflow_counting` | example; [docs/registry/implementation-map.yaml:L16890-L16899] |
| 9 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_damage_accumulation` | example; [docs/registry/implementation-map.yaml:L16900-L16909] |
| 10 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_frequency_domain_analysis` | example; [docs/registry/implementation-map.yaml:L16910-L16919] |
| 11 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_quick_analysis` | example; [docs/registry/implementation-map.yaml:L16920-L16929] |
| 12 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `main` | example; [docs/registry/implementation-map.yaml:L16930-L16945] |
| 13 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_cli.py` | `list_curves` | example; [docs/registry/implementation-map.yaml:L16946-L16955] |
| 14 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_cli.py` | `compare_curves` | example; [docs/registry/implementation-map.yaml:L16956-L16965] |
| 15 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_2_linear_log` | example; [docs/registry/implementation-map.yaml:L16966-L16975] |
| 16 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_3_with_scf` | example; [docs/registry/implementation-map.yaml:L16976-L16985] |
| 17 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_4_without_fatigue_limit` | example; [docs/registry/implementation-map.yaml:L16986-L16995] |
| 18 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_5_dnv_comparison` | example; [docs/registry/implementation-map.yaml:L16996-L17005] |
| 19 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_6_abs_comparison` | example; [docs/registry/implementation-map.yaml:L17006-L17015] |
| 20 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_7_reference_comparison` | example; [docs/registry/implementation-map.yaml:L17016-L17025] |
| 21 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_8_joint_type_comparison` | example; [docs/registry/implementation-map.yaml:L17026-L17035] |
| 22 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_1_basic_sn_curves` | example; [docs/registry/implementation-map.yaml:L17036-L17045] |
| 23 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_2_multislope_curves` | example; [docs/registry/implementation-map.yaml:L17046-L17055] |
| 24 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_3_time_domain_analysis` | example; [docs/registry/implementation-map.yaml:L17056-L17065] |
| 25 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_4_frequency_domain_analysis` | example; [docs/registry/implementation-map.yaml:L17066-L17075] |
| 26 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_5_high_stress_range_analysis` | example; [docs/registry/implementation-map.yaml:L17076-L17085] |
| 27 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_6_shear_data_analysis` | example; [docs/registry/implementation-map.yaml:L17086-L17095] |
| 28 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `run_all_examples` | example; [docs/registry/implementation-map.yaml:L17096-L17112] |
| 29 | `examples/domains/reservoir/reservoir_analysis_examples.py` | `example_well_log_analysis` | example; [docs/registry/implementation-map.yaml:L17113-L17122] |
| 30 | `examples/domains/reservoir/reservoir_analysis_examples.py` | `example_stratigraphic_correlation` | example; [docs/registry/implementation-map.yaml:L17123-L17132] |
| 31 | `examples/domains/reservoir/reservoir_analysis_examples.py` | `main` | example; [docs/registry/implementation-map.yaml:L17133-L17145] |
| 32 | `scripts/capabilities/build_onepagers.py` | `None` | independent; [docs/registry/implementation-map.yaml:L17146-L17154] |
| 33 | `scripts/python/digitalmodel/legacy/27_-_RainflowHalfCycles.py` | `None` | independent; [docs/registry/implementation-map.yaml:L17155-L17163] |
| 34 | `scripts/python/digitalmodel/tools/check_process_fatigue_single.py` | `process_single_test_file` | independent; [docs/registry/implementation-map.yaml:L17164-L17173] |
| 35 | `scripts/python/digitalmodel/tools/migrate_yaml_configs.py` | `YAMLConfigMigrator._migrate_analysis` | independent; [docs/registry/implementation-map.yaml:L17174-L17183] |
| 36 | `scripts/python/digitalmodel/tools/migrate_yaml_configs.py` | `YAMLConfigMigrator._migrate_time_series_components` | independent; [docs/registry/implementation-map.yaml:L17184-L17193] |
| 37 | `scripts/python/digitalmodel/tools/process_fatigue_data.py` | `process_fatigue_files` | independent; [docs/registry/implementation-map.yaml:L17194-L17203] |
| 38 | `scripts/python/digitalmodel/tools/process_fatigue_final.py` | `process_single_test_file` | independent; [docs/registry/implementation-map.yaml:L17204-L17213] |
| 39 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `process_fatigue_file` | independent; [docs/registry/implementation-map.yaml:L17214-L17223] |
| 40 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `process_sample_file` | independent; [docs/registry/implementation-map.yaml:L17224-L17235] |
| 41 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `batch_process_all_files` | independent; [docs/registry/implementation-map.yaml:L17236-L17246] |
| 42 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `rainflow_counting` | independent; [docs/registry/implementation-map.yaml:L17247-L17256] |
| 43 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `process_fatigue_file` | independent; [docs/registry/implementation-map.yaml:L17257-L17268] |
| 44 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `batch_process` | independent; [docs/registry/implementation-map.yaml:L17269-L17280] |
| 45 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v2.py` | `process_single_test_file` | independent; [docs/registry/implementation-map.yaml:L17281-L17290] |
| 46 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v3.py` | `process_single_test_file` | independent; [docs/registry/implementation-map.yaml:L17291-L17300] |
| 47 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v4.py` | `process_single_test_file` | independent; [docs/registry/implementation-map.yaml:L17301-L17310] |
| 48 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v5.py` | `process_single_test_file` | independent; [docs/registry/implementation-map.yaml:L17311-L17320] |
| 49 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v6.py` | `process_single_test_file` | independent; [docs/registry/implementation-map.yaml:L17321-L17330] |
| 50 | `scripts/riser_database/build_tables.py` | `None` | independent; [docs/registry/implementation-map.yaml:L17331-L17339] |
| 51 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `SNcurve.from_dnv` | independent; [docs/registry/implementation-map.yaml:L17340-L17349] |
| 52 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.calculate_cycles_to_failure` | independent; [docs/registry/implementation-map.yaml:L17350-L17359] |
| 53 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.calculate_fatigue_damage` | independent; [docs/registry/implementation-map.yaml:L17360-L17371] |
| 54 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.generate_sn_curve_apdl` | independent; [docs/registry/implementation-map.yaml:L17372-L17381] |
| 55 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.create_damage_summary_df` | independent; [docs/registry/implementation-map.yaml:L17382-L17391] |
| 56 | `src/digitalmodel/benchmarks/inventory.py` | `build_model_inventory` | independent; [docs/registry/implementation-map.yaml:L17392-L17401] |
| 57 | `src/digitalmodel/drilling_riser/global_model/fatigue_channels.py` | `half_cycles` | independent; [docs/registry/implementation-map.yaml:L17402-L17411] |
| 58 | `src/digitalmodel/drilling_riser/global_model/fatigue_channels.py` | `extract` | independent; [docs/registry/implementation-map.yaml:L17412-L17423] |
| 59 | `src/digitalmodel/engine.py` | `engine` | caller; [docs/registry/implementation-map.yaml:L17424-L17441] |
| 60 | `src/digitalmodel/fatg_spectral_fatigue/workflow.py` | `router` | independent; [docs/registry/implementation-map.yaml:L17442-L17454] |
| 61 | `src/digitalmodel/fatg_spectral_fatigue/workflow.py` | `_sn_curve` | independent; [docs/registry/implementation-map.yaml:L17455-L17464] |
| 62 | `src/digitalmodel/fatigue/assessment.py` | `_case_histogram` | independent; [docs/registry/implementation-map.yaml:L17465-L17474] |
| 63 | `src/digitalmodel/fatigue/assessment.py` | `assess_fatigue` | independent; [docs/registry/implementation-map.yaml:L17475-L17488] |
| 64 | `src/digitalmodel/fatigue/counting_contract.py` | `None` | independent; [docs/registry/implementation-map.yaml:L17489-L17497] |
| 65 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_pylife_4pt` | independent; [docs/registry/implementation-map.yaml:L17498-L17507] |
| 66 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_pypi_rainflow` | independent; [docs/registry/implementation-map.yaml:L17508-L17517] |
| 67 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_sigproc_astm` | independent; [docs/registry/implementation-map.yaml:L17518-L17527] |
| 68 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_calm_buoy` | independent; [docs/registry/implementation-map.yaml:L17528-L17537] |
| 69 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_fapps_counting` | independent; [docs/registry/implementation-map.yaml:L17538-L17547] |
| 70 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_fapps_counter` | independent; [docs/registry/implementation-map.yaml:L17548-L17557] |
| 71 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_struct_fatigue` | independent; [docs/registry/implementation-map.yaml:L17558-L17567] |
| 72 | `src/digitalmodel/fatigue/counting_contract.py` | `_is_counting_name` | independent; [docs/registry/implementation-map.yaml:L17568-L17577] |
| 73 | `src/digitalmodel/fatigue/damage.py` | `miner_damage` | independent; [docs/registry/implementation-map.yaml:L17578-L17587] |
| 74 | `src/digitalmodel/fatigue/damage.py` | `design_life_check` | independent; [docs/registry/implementation-map.yaml:L17588-L17599] |
| 75 | `src/digitalmodel/fatigue/environmental_correction.py` | `environment_correction` | independent; [docs/registry/implementation-map.yaml:L17600-L17609] |
| 76 | `src/digitalmodel/fatigue/environmental_correction.py` | `apply_environment_to_sn` | independent; [docs/registry/implementation-map.yaml:L17610-L17621] |
| 77 | `src/digitalmodel/fatigue/fatg.py` | `fatg_sea_state_damage` | independent; [docs/registry/implementation-map.yaml:L17622-L17631] |
| 78 | `src/digitalmodel/fatigue/fatg.py` | `fatg_annual_damage` | independent; [docs/registry/implementation-map.yaml:L17632-L17643] |
| 79 | `src/digitalmodel/fatigue/fatigue_reporting.py` | `_sec_fatigue_locations` | independent; [docs/registry/implementation-map.yaml:L17644-L17653] |
| 80 | `src/digitalmodel/fatigue/rainflow.py` | `fatigue_life` | independent; [docs/registry/implementation-map.yaml:L17654-L17666] |
| 81 | `src/digitalmodel/fatigue/sn_curves.py` | `_compute_sd` | independent; [docs/registry/implementation-map.yaml:L17667-L17676] |
| 82 | `src/digitalmodel/fatigue/sn_curves.py` | `_build_curve_dict` | caller; [docs/registry/implementation-map.yaml:L17677-L17688] |
| 83 | `src/digitalmodel/fatigue/sn_curves.py` | `get_sn_curve` | caller; [docs/registry/implementation-map.yaml:L17689-L17699] |
| 84 | `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.knee_stress` | wrapper; [docs/registry/implementation-map.yaml:L17700-L17711] |
| 85 | `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.switch_stress` | wrapper; [docs/registry/implementation-map.yaml:L17712-L17723] |
| 86 | `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.cycles` | wrapper; [docs/registry/implementation-map.yaml:L17724-L17735] |
| 87 | `src/digitalmodel/fatigue/sn_library.py` | `_knee_stress` | independent; [docs/registry/implementation-map.yaml:L17736-L17745] |
| 88 | `src/digitalmodel/fatigue/sn_library.py` | `_switch_stress` | caller; [docs/registry/implementation-map.yaml:L17746-L17756] |
| 89 | `src/digitalmodel/fatigue/sn_library.py` | `_bilinear_cycles` | independent; [docs/registry/implementation-map.yaml:L17757-L17766] |
| 90 | `src/digitalmodel/fatigue/sn_library.py` | `_sd` | independent; [docs/registry/implementation-map.yaml:L17767-L17776] |
| 91 | `src/digitalmodel/fatigue/sn_library.py` | `_build_dnv_curves` | independent; [docs/registry/implementation-map.yaml:L17777-L17786] |
| 92 | `src/digitalmodel/fatigue/sn_library.py` | `_build_api_curves` | independent; [docs/registry/implementation-map.yaml:L17787-L17796] |
| 93 | `src/digitalmodel/fatigue/sn_library.py` | `_build_bs7608_curves` | independent; [docs/registry/implementation-map.yaml:L17797-L17808] |
| 94 | `src/digitalmodel/fatigue/sn_library.py` | `_build_iiw_curves` | independent; [docs/registry/implementation-map.yaml:L17809-L17819] |
| 95 | `src/digitalmodel/fatigue/sn_library.py` | `_build_eurocode3_curves` | independent; [docs/registry/implementation-map.yaml:L17820-L17830] |
| 96 | `src/digitalmodel/fatigue/sn_library.py` | `_build_norsok_curves` | caller; [docs/registry/implementation-map.yaml:L17831-L17841] |
| 97 | `src/digitalmodel/fatigue/sn_library.py` | `_build_aws_curves` | independent; [docs/registry/implementation-map.yaml:L17842-L17851] |
| 98 | `src/digitalmodel/fatigue/sn_library.py` | `_build_asme_curves` | independent; [docs/registry/implementation-map.yaml:L17852-L17861] |
| 99 | `src/digitalmodel/fatigue/sn_library.py` | `_build_en13445_curves` | independent; [docs/registry/implementation-map.yaml:L17862-L17872] |
| 100 | `src/digitalmodel/fatigue/sn_library.py` | `_build_pd5500_curves` | caller; [docs/registry/implementation-map.yaml:L17873-L17883] |
| 101 | `src/digitalmodel/fatigue/sn_library.py` | `_build_abs_curves` | independent; [docs/registry/implementation-map.yaml:L17884-L17893] |
| 102 | `src/digitalmodel/fatigue/sn_library.py` | `_build_iso19902_curves` | independent; [docs/registry/implementation-map.yaml:L17894-L17904] |
| 103 | `src/digitalmodel/fatigue/sn_library.py` | `_build_csa_curves` | independent; [docs/registry/implementation-map.yaml:L17905-L17914] |
| 104 | `src/digitalmodel/fatigue/sn_library.py` | `_build_hse_curves` | independent; [docs/registry/implementation-map.yaml:L17915-L17925] |
| 105 | `src/digitalmodel/fatigue/sn_library.py` | `_build_doe_curves` | caller; [docs/registry/implementation-map.yaml:L17926-L17936] |
| 106 | `src/digitalmodel/fatigue/sn_library.py` | `_build_gl_curves` | independent; [docs/registry/implementation-map.yaml:L17937-L17946] |
| 107 | `src/digitalmodel/fatigue/sn_library.py` | `_build_jis_curves` | independent; [docs/registry/implementation-map.yaml:L17947-L17956] |
| 108 | `src/digitalmodel/fatigue/sn_library.py` | `_ensure_loaded` | independent; [docs/registry/implementation-map.yaml:L17957-L17966] |
| 109 | `src/digitalmodel/fatigue/sn_library.py` | `summary_table` | caller; [docs/registry/implementation-map.yaml:L17967-L17978] |
| 110 | `src/digitalmodel/fatigue/sn_library_api.py` | `_record_to_curve_info` | independent; [docs/registry/implementation-map.yaml:L17979-L17988] |
| 111 | `src/digitalmodel/fatigue/sn_library_api.py` | `_record_to_sncurve` | independent; [docs/registry/implementation-map.yaml:L17989-L17998] |
| 112 | `src/digitalmodel/fatigue/sn_library_api.py` | `get_curve` | wrapper; [docs/registry/implementation-map.yaml:L17999-L18010] |
| 113 | `src/digitalmodel/fatigue/sn_library_api.py` | `calculate_endurance` | wrapper; [docs/registry/implementation-map.yaml:L18011-L18023] |
| 114 | `src/digitalmodel/fatigue/spectral_fatigue.py` | `wirsching_light_damage` | independent; [docs/registry/implementation-map.yaml:L18024-L18033] |
| 115 | `src/digitalmodel/fatigue/spectral_fatigue.py` | `benasciutti_tovo_damage` | independent; [docs/registry/implementation-map.yaml:L18034-L18043] |
| 116 | `src/digitalmodel/fatigue/weld_fatigue_quickcheck.py` | `weld_fatigue_quickcheck` | independent; [docs/registry/implementation-map.yaml:L18044-L18054] |
| 117 | `src/digitalmodel/fatigue/weld_fatigue_quickcheck.py` | `main` | independent; [docs/registry/implementation-map.yaml:L18055-L18064] |
| 118 | `src/digitalmodel/fatigue/woehler_fitting.py` | `generate_test_data` | independent; [docs/registry/implementation-map.yaml:L18065-L18074] |
| 119 | `src/digitalmodel/fatigue/woehler_fitting.py` | `_fallback_log_regression` | independent; [docs/registry/implementation-map.yaml:L18075-L18084] |
| 120 | `src/digitalmodel/fatigue/workflow.py` | `router` | caller; [docs/registry/implementation-map.yaml:L18085-L18102] |
| 121 | `src/digitalmodel/fatigue/workflow.py` | `_curve_ids` | independent; [docs/registry/implementation-map.yaml:L18103-L18112] |
| 122 | `src/digitalmodel/fatigue/workflow.py` | `_stress_ranges` | independent; [docs/registry/implementation-map.yaml:L18113-L18122] |
| 123 | `src/digitalmodel/fatigue/workflow.py` | `_curve_summary` | independent; [docs/registry/implementation-map.yaml:L18123-L18132] |
| 124 | `src/digitalmodel/fatigue/workflow.py` | `_curve_csv_path` | independent; [docs/registry/implementation-map.yaml:L18133-L18144] |
| 125 | `src/digitalmodel/fatigue/workflow.py` | `_input_stem` | independent; [docs/registry/implementation-map.yaml:L18145-L18154] |
| 126 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.router` | caller; [docs/registry/implementation-map.yaml:L18155-L18167] |
| 127 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.damage_from_sn_data` | independent; [docs/registry/implementation-map.yaml:L18168-L18177] |
| 128 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.damage_from_rainflow_cycles` | independent; [docs/registry/implementation-map.yaml:L18178-L18187] |
| 129 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.damage_from_timetrace` | independent; [docs/registry/implementation-map.yaml:L18188-L18200] |
| 130 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.get_rainflow_from_timetrace` | independent; [docs/registry/implementation-map.yaml:L18201-L18210] |
| 131 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | independent; [docs/registry/implementation-map.yaml:L18211-L18220] |
| 132 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.router` | wrapper; [docs/registry/implementation-map.yaml:L18221-L18232] |
| 133 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.get_lcf_timetrace_rainflow_and_fatigue_damage` | independent; [docs/registry/implementation-map.yaml:L18233-L18244] |
| 134 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.get_fatigue_damage_output_df_for_timetrace` | wrapper; [docs/registry/implementation-map.yaml:L18245-L18256] |
| 135 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.get_fatigue_df_definition_for_timetrace` | independent; [docs/registry/implementation-map.yaml:L18257-L18266] |
| 136 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | independent; [docs/registry/implementation-map.yaml:L18267-L18276] |
| 137 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `SNcurve.cycles_to_failure` | independent; [docs/registry/implementation-map.yaml:L18277-L18286] |
| 138 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `RainflowFatigue.count_cycles` | independent; [docs/registry/implementation-map.yaml:L18287-L18296] |
| 139 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `MinersRuleDamage.compute` | independent; [docs/registry/implementation-map.yaml:L18297-L18306] |
| 140 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `ScatterDiagramFatigue.compute` | independent; [docs/registry/implementation-map.yaml:L18307-L18316] |
| 141 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `compute_fatigue_life` | independent; [docs/registry/implementation-map.yaml:L18317-L18326] |
| 142 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.add_well_analysis` | independent; [docs/registry/implementation-map.yaml:L18327-L18336] |
| 143 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.set_well_order` | independent; [docs/registry/implementation-map.yaml:L18337-L18346] |
| 144 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.create_flattened_section` | independent; [docs/registry/implementation-map.yaml:L18347-L18356] |
| 145 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `WellCorrelation.add_well_data` | independent; [docs/registry/implementation-map.yaml:L18357-L18366] |
| 146 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `CoreAnalysis.correlate_core_to_logs` | independent; [docs/registry/implementation-map.yaml:L18367-L18376] |
| 147 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `ReservoirAnalysis.add_well_analysis` | independent; [docs/registry/implementation-map.yaml:L18377-L18386] |
| 148 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `ReservoirAnalysis.run_comprehensive_analysis` | independent; [docs/registry/implementation-map.yaml:L18387-L18396] |
| 149 | `src/digitalmodel/mooring_fatigue/workflow.py` | `router` | caller; [docs/registry/implementation-map.yaml:L18397-L18410] |
| 150 | `src/digitalmodel/mooring_fatigue/workflow.py` | `_evaluate_line` | independent; [docs/registry/implementation-map.yaml:L18411-L18421] |
| 151 | `src/digitalmodel/mooring_fatigue/workflow.py` | `_history_bins` | independent; [docs/registry/implementation-map.yaml:L18422-L18431] |
| 152 | `src/digitalmodel/mooring_fatigue/workflow.py` | `_curve_settings` | independent; [docs/registry/implementation-map.yaml:L18432-L18441] |
| 153 | `src/digitalmodel/parametric/generate.py` | `_mooring_fatigue_damage` | independent; [docs/registry/implementation-map.yaml:L18442-L18452] |
| 154 | `src/digitalmodel/parametric/generate.py` | `_free_span_utilisation` | independent; [docs/registry/implementation-map.yaml:L18453-L18462] |
| 155 | `src/digitalmodel/parametric/generate.py` | `_span_rectification_utilisation` | independent; [docs/registry/implementation-map.yaml:L18463-L18472] |
| 156 | `src/digitalmodel/parametric/refresh.py` | `None` | independent; [docs/registry/implementation-map.yaml:L18473-L18481] |
| 157 | `src/digitalmodel/rao_spectral_fatigue/workflow.py` | `router` | caller; [docs/registry/implementation-map.yaml:L18482-L18493] |
| 158 | `src/digitalmodel/riser_fatigue/touchdown.py` | `assess_touchdown_fatigue` | independent; [docs/registry/implementation-map.yaml:L18494-L18503] |
| 159 | `src/digitalmodel/riser_fatigue/workflow.py` | `router` | caller; [docs/registry/implementation-map.yaml:L18504-L18515] |
| 160 | `src/digitalmodel/riser_fatigue/workflow.py` | `_viv_damage` | independent; [docs/registry/implementation-map.yaml:L18516-L18526] |
| 161 | `src/digitalmodel/riser_fatigue/workflow.py` | `_curve_settings` | independent; [docs/registry/implementation-map.yaml:L18527-L18536] |
| 162 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `TimeSeriesComponentsAdapter.get_rainflow_count_from_time_series` | independent; [docs/registry/implementation-map.yaml:L18537-L18546] |
| 163 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `FatigueAnalysisAdapter.get_rainflow_from_timetrace` | independent; [docs/registry/implementation-map.yaml:L18547-L18556] |
| 164 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `FatigueAnalysisAdapter.damage_from_rainflow_cycles` | independent; [docs/registry/implementation-map.yaml:L18557-L18566] |
| 165 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `OrcaFlexAdapter.RainflowHalfCycles` | independent; [docs/registry/implementation-map.yaml:L18567-L18576] |
| 166 | `src/digitalmodel/signal_processing/signal_analysis/cli.py` | `rainflow_cmd` | independent; [docs/registry/implementation-map.yaml:L18577-L18586] |
| 167 | `src/digitalmodel/signal_processing/signal_analysis/core/rainflow.py` | `RainflowCounter.count_cycles` | independent; [docs/registry/implementation-map.yaml:L18587-L18596] |
| 168 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/curves.py` | `SNCurve._bilinear` | independent; [docs/registry/implementation-map.yaml:L18597-L18606] |
| 169 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/curves.py` | `SNCurve.from_test_data` | independent; [docs/registry/implementation-map.yaml:L18607-L18616] |
| 170 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator.calculate_damage` | independent; [docs/registry/implementation-map.yaml:L18617-L18632] |
| 171 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._miners_rule` | independent; [docs/registry/implementation-map.yaml:L18633-L18644] |
| 172 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._modified_miners_rule` | independent; [docs/registry/implementation-map.yaml:L18645-L18655] |
| 173 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._nonlinear_damage` | independent; [docs/registry/implementation-map.yaml:L18656-L18666] |
| 174 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._calculate_allowable_cycles` | independent; [docs/registry/implementation-map.yaml:L18667-L18676] |
| 175 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._apply_mean_stress_correction` | independent; [docs/registry/implementation-map.yaml:L18677-L18686] |
| 176 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._estimate_cycles_to_failure` | independent; [docs/registry/implementation-map.yaml:L18687-L18696] |
| 177 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator.calculate_equivalent_stress` | independent; [docs/registry/implementation-map.yaml:L18697-L18706] |
| 178 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `create_parser` | independent; [docs/registry/implementation-map.yaml:L18707-L18716] |
| 179 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `build_config_from_args` | independent; [docs/registry/implementation-map.yaml:L18717-L18726] |
| 180 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `main` | independent; [docs/registry/implementation-map.yaml:L18727-L18739] |
| 181 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._analyze_column` | independent; [docs/registry/implementation-map.yaml:L18740-L18749] |
| 182 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._generate_outputs` | independent; [docs/registry/implementation-map.yaml:L18750-L18759] |
| 183 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._generate_summary` | independent; [docs/registry/implementation-map.yaml:L18760-L18769] |
| 184 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/error_handling.py` | `SafeAnalyzer.safe_rainflow_analysis` | independent; [docs/registry/implementation-map.yaml:L18770-L18779] |
| 185 | `src/digitalmodel/signal_processing/time_series/time_series_analysis.py` | `TimeSeriesAnalysis.router` | independent; [docs/registry/implementation-map.yaml:L18780-L18789] |
| 186 | `src/digitalmodel/signal_processing/time_series/time_series_components.py` | `TimeSeriesComponents.count_cycles` | independent; [docs/registry/implementation-map.yaml:L18790-L18799] |
| 187 | `src/digitalmodel/solvers/orcaflex/opp_time_series.py` | `OPPTimeSeries.get_histogram_object_from_orcaflex_run` | independent; [docs/registry/implementation-map.yaml:L18800-L18809] |
| 188 | `src/digitalmodel/solvers/orcaflex/opp_time_series.py` | `OPPTimeSeries.get_detailed_histogram_df_columns` | independent; [docs/registry/implementation-map.yaml:L18810-L18819] |
| 189 | `src/digitalmodel/solvers/orcaflex/opp_time_series_v2.py` | `OPPTimeSeriesV2.process_histograms` | independent; [docs/registry/implementation-map.yaml:L18820-L18829] |
| 190 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.get_histogram_object_from_orcaflex_run` | independent; [docs/registry/implementation-map.yaml:L18830-L18839] |
| 191 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.get_detailed_histogram_df_columns` | copy; [docs/registry/implementation-map.yaml:L18840-L18849] |
| 192 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._perform_fatigue_analysis` | independent; [docs/registry/implementation-map.yaml:L18850-L18861] |
| 193 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._parallel_fatigue_analysis` | independent; [docs/registry/implementation-map.yaml:L18862-L18871] |
| 194 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._analyze_single_trace` | independent; [docs/registry/implementation-map.yaml:L18872-L18881] |
| 195 | `src/digitalmodel/spectral_fatigue/workflow.py` | `router` | caller; [docs/registry/implementation-map.yaml:L18882-L18893] |
| 196 | `src/digitalmodel/spectral_fatigue/workflow.py` | `_sn_curve` | independent; [docs/registry/implementation-map.yaml:L18894-L18903] |
| 197 | `src/digitalmodel/structural/fatigue/__init__.py` | `quick_fatigue_analysis` | caller; [docs/registry/implementation-map.yaml:L18904-L18917] |
| 198 | `src/digitalmodel/structural/fatigue/__init__.py` | `get_module_info` | independent; [docs/registry/implementation-map.yaml:L18918-L18927] |
| 199 | `src/digitalmodel/structural/fatigue/analysis.py` | `None` | independent; [docs/registry/implementation-map.yaml:L18928-L18936] |
| 200 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._setup_sn_curve` | wrapper; [docs/registry/implementation-map.yaml:L18937-L18947] |
| 201 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._setup_damage_calculator` | independent; [docs/registry/implementation-map.yaml:L18948-L18957] |
| 202 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._setup_rainflow_counter` | independent; [docs/registry/implementation-map.yaml:L18958-L18967] |
| 203 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_time_series` | independent; [docs/registry/implementation-map.yaml:L18968-L18979] |
| 204 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_psd` | independent; [docs/registry/implementation-map.yaml:L18980-L18989] |
| 205 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.calculate_high_stress_range_life` | independent; [docs/registry/implementation-map.yaml:L18990-L18999] |
| 206 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_shear_data` | independent; [docs/registry/implementation-map.yaml:L19000-L19009] |
| 207 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._perform_engineering_validation` | independent; [docs/registry/implementation-map.yaml:L19010-L19019] |
| 208 | `src/digitalmodel/structural/fatigue/analysis.py` | `quick_time_domain_analysis` | independent; [docs/registry/implementation-map.yaml:L19020-L19029] |
| 209 | `src/digitalmodel/structural/fatigue/analysis.py` | `quick_frequency_domain_analysis` | independent; [docs/registry/implementation-map.yaml:L19030-L19039] |
| 210 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `None` | independent; [docs/registry/implementation-map.yaml:L19040-L19048] |
| 211 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `LinearDamageAccumulation.calculate_damage` | independent; [docs/registry/implementation-map.yaml:L19049-L19060] |
| 212 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `LinearDamageAccumulation._apply_mean_stress_correction` | independent; [docs/registry/implementation-map.yaml:L19061-L19070] |
| 213 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `ModifiedMinersRule.calculate_damage` | independent; [docs/registry/implementation-map.yaml:L19071-L19080] |
| 214 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation.calculate_damage` | wrapper; [docs/registry/implementation-map.yaml:L19081-L19094] |
| 215 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation._marco_starkey_damage` | independent; [docs/registry/implementation-map.yaml:L19095-L19104] |
| 216 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation._corten_dolan_damage` | independent; [docs/registry/implementation-map.yaml:L19105-L19114] |
| 217 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation._double_linear_damage` | independent; [docs/registry/implementation-map.yaml:L19115-L19124] |
| 218 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `compare_damage_methods` | independent; [docs/registry/implementation-map.yaml:L19125-L19134] |
| 219 | `src/digitalmodel/structural/fatigue/design_code_report.py` | `_build_curve` | independent; [docs/registry/implementation-map.yaml:L19135-L19144] |
| 220 | `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `failure_probability` | independent; [docs/registry/implementation-map.yaml:L19145-L19154] |
| 221 | `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `sensitivity_analysis` | independent; [docs/registry/implementation-map.yaml:L19155-L19164] |
| 222 | `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `monte_carlo_fatigue` | independent; [docs/registry/implementation-map.yaml:L19165-L19174] |
| 223 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `None` | independent; [docs/registry/implementation-map.yaml:L19175-L19183] |
| 224 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `NarrowBandMethod.calculate_damage_rate` | independent; [docs/registry/implementation-map.yaml:L19184-L19193] |
| 225 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `DirlikMethod.calculate_damage_rate` | independent; [docs/registry/implementation-map.yaml:L19194-L19203] |
| 226 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `TovoBenasciuttiMethod.calculate_damage_rate` | independent; [docs/registry/implementation-map.yaml:L19204-L19213] |
| 227 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `SingleMomentMethod.calculate_damage_rate` | independent; [docs/registry/implementation-map.yaml:L19214-L19223] |
| 228 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `compare_frequency_methods` | independent; [docs/registry/implementation-map.yaml:L19224-L19233] |
| 229 | `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `run_sweep` | independent; [docs/registry/implementation-map.yaml:L19234-L19243] |
| 230 | `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `compute_sensitivity._single_damage` | independent; [docs/registry/implementation-map.yaml:L19244-L19253] |
| 231 | `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `_build_scatter_json` | independent; [docs/registry/implementation-map.yaml:L19254-L19263] |
| 232 | `src/digitalmodel/structural/fatigue/rainflow.py` | `None` | independent; [docs/registry/implementation-map.yaml:L19264-L19272] |
| 233 | `src/digitalmodel/structural/fatigue/rainflow.py` | `RainflowCounter.count_cycles` | caller; [docs/registry/implementation-map.yaml:L19273-L19284] |
| 234 | `src/digitalmodel/structural/fatigue/rainflow.py` | `RainflowCounter._rainflow_count` | independent; [docs/registry/implementation-map.yaml:L19285-L19294] |
| 235 | `src/digitalmodel/structural/fatigue/rainflow.py` | `RainflowCounter.extract_cycles_with_means` | caller; [docs/registry/implementation-map.yaml:L19295-L19306] |
| 236 | `src/digitalmodel/structural/fatigue/rainflow.py` | `rainflow_count` | independent; [docs/registry/implementation-map.yaml:L19307-L19316] |
| 237 | `src/digitalmodel/structural/fatigue/rainflow.py` | `rainflow_with_means` | independent; [docs/registry/implementation-map.yaml:L19317-L19326] |
| 238 | `src/digitalmodel/structural/fatigue/scatter_fatigue.py` | `scatter_fatigue_damage` | independent; [docs/registry/implementation-map.yaml:L19327-L19336] |
| 239 | `src/digitalmodel/structural/fatigue/skill.py` | `fatigue_assessment` | independent; [docs/registry/implementation-map.yaml:L19337-L19348] |
| 240 | `src/digitalmodel/structural/fatigue/sn_curve_plotter.py` | `main` | independent; [docs/registry/implementation-map.yaml:L19349-L19358] |
| 241 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `None` | independent; [docs/registry/implementation-map.yaml:L19359-L19367] |
| 242 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves._ensure_loaded` | independent; [docs/registry/implementation-map.yaml:L19368-L19377] |
| 243 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.register_custom_curve` | independent; [docs/registry/implementation-map.yaml:L19378-L19387] |
| 244 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.get_dnv_c203_curve` | independent; [docs/registry/implementation-map.yaml:L19388-L19397] |
| 245 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.get_curve` | independent; [docs/registry/implementation-map.yaml:L19398-L19407] |
| 246 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.get_multislope_curve` | independent; [docs/registry/implementation-map.yaml:L19408-L19417] |
| 247 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `ThicknessCorrection.apply_thickness_effect` | independent; [docs/registry/implementation-map.yaml:L19418-L19427] |
| 248 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `SNDataFitting.fit_power_law` | independent; [docs/registry/implementation-map.yaml:L19428-L19437] |
| 249 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `get_dnv_curve` | independent; [docs/registry/implementation-map.yaml:L19438-L19447] |
| 250 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `get_api_curve` | independent; [docs/registry/implementation-map.yaml:L19448-L19457] |
| 251 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `get_bs_curve` | independent; [docs/registry/implementation-map.yaml:L19458-L19467] |
| 252 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `_dnv_curves_from_c203` | independent; [docs/registry/implementation-map.yaml:L19468-L19477] |
| 253 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_dnv_seawater_cp` | independent; [docs/registry/implementation-map.yaml:L19478-L19487] |
| 254 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_dnv_seawater_free` | independent; [docs/registry/implementation-map.yaml:L19488-L19497] |
| 255 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_chain_seawater_cp` | independent; [docs/registry/implementation-map.yaml:L19498-L19507] |
| 256 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `pipeline_girth_weld` | caller; [docs/registry/implementation-map.yaml:L19508-L19520] |
| 257 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `scr_touchdown` | caller; [docs/registry/implementation-map.yaml:L19521-L19533] |
| 258 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `mooring_chain` | caller; [docs/registry/implementation-map.yaml:L19534-L19546] |
| 259 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_compute_example` | independent; [docs/registry/implementation-map.yaml:L19547-L19556] |
| 260 | `src/digitalmodel/structural/fatigue_apps/__main__.py` | `run_analysis` | independent; [docs/registry/implementation-map.yaml:L19557-L19568] |
| 261 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `SNCurve._load_sn_parameters` | independent; [docs/registry/implementation-map.yaml:L19569-L19578] |
| 262 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `SNCurve.calculate_cycles_to_failure` | independent; [docs/registry/implementation-map.yaml:L19579-L19588] |
| 263 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `FatigueDamageCalculator.calculate_damage` | independent; [docs/registry/implementation-map.yaml:L19589-L19598] |
| 264 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `main` | independent; [docs/registry/implementation-map.yaml:L19599-L19608] |
| 265 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `None` | independent; [docs/registry/implementation-map.yaml:L19609-L19617] |
| 266 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `SNCurveParameters.a1` | independent; [docs/registry/implementation-map.yaml:L19618-L19627] |
| 267 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `SNCurveParameters.a2` | independent; [docs/registry/implementation-map.yaml:L19628-L19637] |
| 268 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `FatigueDamageCalculator.cycles_to_failure` | independent; [docs/registry/implementation-map.yaml:L19638-L19647] |
| 269 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `FatigueDamageCalculator.calculate_damage` | independent; [docs/registry/implementation-map.yaml:L19648-L19659] |
| 270 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `FatigueDamageCalculator.export_results` | independent; [docs/registry/implementation-map.yaml:L19660-L19669] |
| 271 | `src/digitalmodel/structural/fatigue_apps/file_namer.py` | `FatigueFileNamer.get_rainflow_filename` | independent; [docs/registry/implementation-map.yaml:L19670-L19679] |
| 272 | `src/digitalmodel/structural/fatigue_apps/file_namer.py` | `None` | independent; [docs/registry/implementation-map.yaml:L19680-L19688] |
| 273 | `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `IntegratedFatigueProcessor.process_fatigue_condition` | independent; [docs/registry/implementation-map.yaml:L19689-L19698] |
| 274 | `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `main` | independent; [docs/registry/implementation-map.yaml:L19699-L19708] |
| 275 | `src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py` | `IntegratedFatigueProcessorWithNaming._process_condition_base` | independent; [docs/registry/implementation-map.yaml:L19709-L19718] |
| 276 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `None` | independent; [docs/registry/implementation-map.yaml:L19719-L19727] |
| 277 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `RainflowCounter.count_cycles` | caller; [docs/registry/implementation-map.yaml:L19728-L19739] |
| 278 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `RainflowCounter._rainflow_algorithm` | independent; [docs/registry/implementation-map.yaml:L19740-L19749] |
| 279 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `RainflowCounter.export_results` | independent; [docs/registry/implementation-map.yaml:L19750-L19759] |
| 280 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `rainflow_count` | independent; [docs/registry/implementation-map.yaml:L19760-L19769] |
| 281 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.process_time_series` | independent; [docs/registry/implementation-map.yaml:L19770-L19779] |
| 282 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.process_batch` | independent; [docs/registry/implementation-map.yaml:L19780-L19791] |
| 283 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.save_cycle_data` | independent; [docs/registry/implementation-map.yaml:L19792-L19801] |
| 284 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `main` | independent; [docs/registry/implementation-map.yaml:L19802-L19811] |
| 285 | `src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py` | `ReferenceSeaStateProcessor.process_fatigue_condition` | independent; [docs/registry/implementation-map.yaml:L19812-L19821] |
| 286 | `src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py` | `TensionStressConverter.process_batch` | independent; [docs/registry/implementation-map.yaml:L19822-L19831] |
| 287 | `src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py` | `main` | independent; [docs/registry/implementation-map.yaml:L19832-L19841] |
| 288 | `src/digitalmodel/structural/fatigue_apps/visualizer.py` | `generate_all_visualizations` | independent; [docs/registry/implementation-map.yaml:L19842-L19851] |
| 289 | `src/digitalmodel/structural/spectral_fatigue.py` | `narrow_band_damage` | independent; [docs/registry/implementation-map.yaml:L19852-L19861] |
| 290 | `src/digitalmodel/subsea/connectors/fatigue.py` | `assess_jumper_fatigue` | caller; [docs/registry/implementation-map.yaml:L19862-L19873] |
| 291 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `_dnv_params` | independent; [docs/registry/implementation-map.yaml:L19874-L19883] |
| 292 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `get_sn_curve` | independent; [docs/registry/implementation-map.yaml:L19884-L19895] |
| 293 | `src/digitalmodel/subsea/pipeline/free_span/span_fatigue_damage.py` | `SpanFatigueDamage._load_sn_curve` | caller; [docs/registry/implementation-map.yaml:L19896-L19907] |
| 294 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.calculate_damage` | independent; [docs/registry/implementation-map.yaml:L19908-L19917] |
| 295 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.calculate_fatigue_life` | independent; [docs/registry/implementation-map.yaml:L19918-L19927] |
| 296 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.analyze_viv_fatigue` | independent; [docs/registry/implementation-map.yaml:L19928-L19940] |
| 297 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.cumulative_damage` | caller; [docs/registry/implementation-map.yaml:L19941-L19952] |
| 298 | `src/digitalmodel/viv_screening/inputs.py` | `parse_inputs` | independent; [docs/registry/implementation-map.yaml:L19953-L19962] |
| 299 | `src/digitalmodel/viv_screening/workflow.py` | `router` | independent; [docs/registry/implementation-map.yaml:L19963-L19976] |
| 300 | `src/digitalmodel/viv_screening/workflow.py` | `_damage_rate` | independent; [docs/registry/implementation-map.yaml:L19977-L19987] |
| 301 | `src/digitalmodel/viv_screening/workflow.py` | `_provenance` | independent; [docs/registry/implementation-map.yaml:L19988-L19997] |
| 302 | `src/digitalmodel/web/digitaltwinfeed/app.py` | `page` | independent; [docs/registry/implementation-map.yaml:L19998-L20007] |
| 303 | `tests/ansys/padeye_pressure_bundle.py` | `_check_receipt` | test_helper; [docs/registry/implementation-map.yaml:L20008-L20017] |
| 304 | `tests/ansys/padeye_pressure_bundle.py` | `verify_pressure_bundle` | test_helper; [docs/registry/implementation-map.yaml:L20018-L20029] |
| 305 | `tests/ansys/test_fatigue_postprocessor.py` | `TestThicknessCorrection.test_known_correction` | test_helper; [docs/registry/implementation-map.yaml:L20030-L20039] |
| 306 | `tests/ansys/test_padeye_pressure_bundle.py` | `bundle` | test_helper; [docs/registry/implementation-map.yaml:L20040-L20049] |
| 307 | `tests/fatigue/test_assessment.py` | `histogram_input` | test_helper; [docs/registry/implementation-map.yaml:L20050-L20061] |
| 308 | `tests/fatigue/test_c203_consumers.py` | `_span_input` | test_helper; [docs/registry/implementation-map.yaml:L20062-L20071] |
| 309 | `tests/fatigue/test_c203_sn_values.py` | `_non_dnv_bilinear` | test_helper; [docs/registry/implementation-map.yaml:L20072-L20081] |
| 310 | `tests/fatigue/test_c203_sn_values.py` | `test_non_dnv_bilinear_switch_stays_at_endurance_limit` | test_helper; [docs/registry/implementation-map.yaml:L20082-L20095] |
| 311 | `tests/fatigue/test_c203_variable_amplitude.py` | `_signal_e` | test_helper; [docs/registry/implementation-map.yaml:L20096-L20105] |
| 312 | `tests/fatigue/test_c203_variable_amplitude.py` | `_user_curve` | test_helper; [docs/registry/implementation-map.yaml:L20106-L20115] |
| 313 | `tests/fatigue/test_c203_variable_amplitude.py` | `_span_input` | test_helper; [docs/registry/implementation-map.yaml:L20116-L20125] |
| 314 | `tests/fatigue/test_c203_variable_amplitude.py` | `_fresh_standard_curves` | test_helper; [docs/registry/implementation-map.yaml:L20126-L20135] |
| 315 | `tests/fatigue/test_c203_variable_amplitude.py` | `_data_dir` | test_helper; [docs/registry/implementation-map.yaml:L20136-L20145] |
| 316 | `tests/fatigue/test_damage.py` | `f_curve` | test_helper; [docs/registry/implementation-map.yaml:L20146-L20156] |
| 317 | `tests/fatigue/test_damage.py` | `TestThicknessCorrection.test_correction_above_ref` | test_helper; [docs/registry/implementation-map.yaml:L20157-L20166] |
| 318 | `tests/fatigue/test_damage.py` | `TestThicknessCorrection.test_correction_formula` | test_helper; [docs/registry/implementation-map.yaml:L20167-L20176] |
| 319 | `tests/fatigue/test_fatg.py` | `TestAgainstTransliterationSweep.test_all_presets_and_curves` | test_helper; [docs/registry/implementation-map.yaml:L20177-L20188] |
| 320 | `tests/fatigue/test_fatg_workflow.py` | `_cfg` | test_helper; [docs/registry/implementation-map.yaml:L20189-L20198] |
| 321 | `tests/fatigue/test_fatg_workflow.py` | `_library_reference` | test_helper; [docs/registry/implementation-map.yaml:L20199-L20209] |
| 322 | `tests/fatigue/test_fatigue_reporting.py` | `_sample_locations` | test_helper; [docs/registry/implementation-map.yaml:L20210-L20219] |
| 323 | `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls` | test_helper; [docs/registry/implementation-map.yaml:L20220-L20232] |
| 324 | `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls.calm_scatter` | test_helper; [docs/registry/implementation-map.yaml:L20233-L20242] |
| 325 | `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls.fapps_batch` | test_helper; [docs/registry/implementation-map.yaml:L20243-L20252] |
| 326 | `tests/fatigue/test_sn_curves.py` | `TestGetSNCurve.test_f_curve_at_100mpa` | test_helper; [docs/registry/implementation-map.yaml:L20253-L20263] |
| 327 | `tests/fatigue/test_sn_curves.py` | `TestGetSNCurve.test_d_curve_at_100mpa` | test_helper; [docs/registry/implementation-map.yaml:L20264-L20274] |
| 328 | `tests/fatigue/test_weld_fatigue_quickcheck.py` | `_hand_N` | test_helper; [docs/registry/implementation-map.yaml:L20275-L20284] |
| 329 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_cycles_to_failure_chain_known_value` | test_helper; [docs/registry/implementation-map.yaml:L20285-L20294] |
| 330 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_cycles_to_failure_wire_rope_known_value` | test_helper; [docs/registry/implementation-map.yaml:L20295-L20304] |
| 331 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_custom_sn_curve` | test_helper; [docs/registry/implementation-map.yaml:L20305-L20314] |
| 332 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestMinersRuleDamage.test_single_range_known_damage` | test_helper; [docs/registry/implementation-map.yaml:L20315-L20324] |
| 333 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestMinersRuleDamage.test_damage_additivity` | test_helper; [docs/registry/implementation-map.yaml:L20325-L20334] |
| 334 | `tests/parametric/test_atlas.py` | `_small_atlas` | test_helper; [docs/registry/implementation-map.yaml:L20335-L20344] |
| 335 | `tests/signal_processing/signal_analysis/fatigue_analysis_reference/process_fatigue_final.py` | `process_single_test_file` | test_helper; [docs/registry/implementation-map.yaml:L20345-L20354] |
| 336 | `tests/signal_processing/signal_analysis/reference_fatigue_processor.py` | `process_single_test_file` | test_helper; [docs/registry/implementation-map.yaml:L20355-L20364] |
| 337 | `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `None` | test_helper; [docs/registry/implementation-map.yaml:L20365-L20373] |
| 338 | `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `analyze_signal` | test_helper; [docs/registry/implementation-map.yaml:L20374-L20383] |
| 339 | `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `main` | test_helper; [docs/registry/implementation-map.yaml:L20384-L20395] |
| 340 | `tests/signal_processing/signal_analysis/test_orcaflex_tension_analysis.py` | `OrcaFlexTensionAnalyzer.analyze_tension` | test_helper; [docs/registry/implementation-map.yaml:L20396-L20405] |
| 341 | `tests/signal_processing/signal_analysis/test_orcaflex_tension_analysis.py` | `OrcaFlexTensionAnalyzer.export_results` | test_helper; [docs/registry/implementation-map.yaml:L20406-L20415] |
| 342 | `tests/signal_processing/signal_analysis/test_rainflow_timeseries1.py` | `run_rainflow_for_timeseries` | test_helper; [docs/registry/implementation-map.yaml:L20416-L20425] |
| 343 | `tests/signal_processing/signal_analysis/test_rainflow_timeseries1.py` | `test_rainflow_for_timeseries` | test_helper; [docs/registry/implementation-map.yaml:L20426-L20437] |
| 344 | `tests/signal_processing/signal_analysis/validate_module.py` | `main` | test_helper; [docs/registry/implementation-map.yaml:L20438-L20452] |
| 345 | `tests/solvers/test_integration_solvers.py` | `mock_solver_factory.MockSolverFactory.create_fatigue_solver` | test_helper; [docs/registry/implementation-map.yaml:L20453-L20462] |
| 346 | `tests/structural/fatigue/conftest.py` | `simple_sn_curve` | test_helper; [docs/registry/implementation-map.yaml:L20463-L20472] |
| 347 | `tests/structural/fatigue/test_damage_accumulation.py` | `dnv_d_curve` | test_helper; [docs/registry/implementation-map.yaml:L20473-L20482] |
| 348 | `tests/structural/fatigue/test_damage_accumulation.py` | `simple_curve` | test_helper; [docs/registry/implementation-map.yaml:L20483-L20492] |
| 349 | `tests/structural/fatigue/test_damage_accumulation.py` | `curve_with_material` | test_helper; [docs/registry/implementation-map.yaml:L20493-L20502] |
| 350 | `tests/structural/fatigue/test_fatigue_migration.py` | `TestSNCurveMigration.test_multislope_curve_implementation` | test_helper; [docs/registry/implementation-map.yaml:L20503-L20512] |
| 351 | `tests/structural/fatigue/test_fatigue_migration.py` | `TestSNCurveMigration.test_curve_continuity` | test_helper; [docs/registry/implementation-map.yaml:L20513-L20522] |
| 352 | `tests/structural/fatigue/test_fatigue_migration.py` | `TestFrequencyDomainMigration.test_basic_frequency_analysis` | test_helper; [docs/registry/implementation-map.yaml:L20523-L20532] |
| 353 | `tests/structural/fatigue/test_fatigue_reliability.py` | `simple_sn` | test_helper; [docs/registry/implementation-map.yaml:L20533-L20542] |
| 354 | `tests/structural/fatigue/test_fatigue_reliability.py` | `mild_sn` | test_helper; [docs/registry/implementation-map.yaml:L20543-L20552] |
| 355 | `tests/structural/fatigue/test_fatigue_skill.py` | `_simple_input` | test_helper; [docs/registry/implementation-map.yaml:L20553-L20562] |
| 356 | `tests/structural/fatigue/test_fatigue_skill.py` | `TestOffshoreScenario.test_high_cycle_low_stress_result` | test_helper; [docs/registry/implementation-map.yaml:L20563-L20574] |
| 357 | `tests/structural/fatigue/test_frequency_domain.py` | `simple_sn_curve` | test_helper; [docs/registry/implementation-map.yaml:L20575-L20584] |
| 358 | `tests/structural/fatigue/test_rainflow.py` | `default_counter` | test_helper; [docs/registry/implementation-map.yaml:L20585-L20594] |
| 359 | `tests/structural/fatigue/test_scatter_fatigue.py` | `simple_sn` | test_helper; [docs/registry/implementation-map.yaml:L20595-L20604] |
| 360 | `tests/structural/fatigue/test_scatter_fatigue.py` | `TestFatigueLife.test_fatigue_life_inverse_of_damage` | test_helper; [docs/registry/implementation-map.yaml:L20605-L20616] |
| 361 | `tests/structural/fatigue/test_sn_curves.py` | `simple_power_law` | test_helper; [docs/registry/implementation-map.yaml:L20617-L20626] |
| 362 | `tests/structural/fatigue/test_sn_curves.py` | `power_law_with_limit` | test_helper; [docs/registry/implementation-map.yaml:L20627-L20636] |
| 363 | `tests/structural/fatigue/test_sn_curves.py` | `bilinear_curve` | test_helper; [docs/registry/implementation-map.yaml:L20637-L20646] |
| 364 | `tests/structural/fatigue/test_sn_curves.py` | `dnv_d_curve` | test_helper; [docs/registry/implementation-map.yaml:L20647-L20656] |
| 365 | `tests/structural/fatigue/test_sn_curves.py` | `TestBS7608RevisedCurveParameters.test_bs7608_revised_d_known_n_at_50mpa` | test_helper; [docs/registry/implementation-map.yaml:L20657-L20666] |
| 366 | `tests/structural/fatigue/test_sn_curves.py` | `TestISO19902CurveParameters.test_iso19902_f_air_known_n_at_100mpa` | test_helper; [docs/registry/implementation-map.yaml:L20667-L20676] |
| 367 | `tests/structural/fatigue/test_sn_curves.py` | `TestISO19902CurveParameters.test_iso19902_f2_sw_cp_known_n_at_50mpa` | test_helper; [docs/registry/implementation-map.yaml:L20677-L20686] |
| 368 | `tests/structural/fatigue/test_sn_curves_yaml.py` | `_all_single_slope_cases` | test_helper; [docs/registry/implementation-map.yaml:L20687-L20696] |
| 369 | `tests/structural/fatigue/test_sn_curves_yaml.py` | `_all_multislope_cases` | test_helper; [docs/registry/implementation-map.yaml:L20697-L20706] |
| 370 | `tests/structural/fatigue_analysis/conftest.py` | `sn_curve_data` | test_helper; [docs/registry/implementation-map.yaml:L20707-L20716] |
| 371 | `tests/structural/fatigue_analysis/test_fatigue_analysis_sn.py` | `run_fatigue_analysis` | test_helper; [docs/registry/implementation-map.yaml:L20717-L20727] |
| 372 | `tests/structural/fatigue_analysis/test_fatigue_analysis_timetrace.py` | `run_fatigue_analysis` | test_helper; [docs/registry/implementation-map.yaml:L20728-L20738] |
| 373 | `tests/subsea/pipeline/test_free_span_f105.py` | `ref_input` | test_helper; [docs/registry/implementation-map.yaml:L20739-L20748] |
| 374 | `tests/subsea/viv_analysis/test_viv_analysis_unit.py` | `TestVIVFatigueCalculator.test_cumulative_damage` | test_helper; [docs/registry/implementation-map.yaml:L20749-L20758] |
| 375 | `tests/test_automation/fixtures/engineering_fixtures.py` | `get_orcaflex_post_process_mock` | test_helper; [docs/registry/implementation-map.yaml:L20759-L20768] |
| 376 | `tests/test_automation/scripts/comprehensive_fix_campaign.py` | `ComprehensiveTestFixCampaign.generate_mock_data` | test_helper; [docs/registry/implementation-map.yaml:L20769-L20778] |
| 377 | `tests/test_complete_fatigue_pipeline.py` | `process_configuration` | test_helper; [docs/registry/implementation-map.yaml:L20779-L20788] |
| 378 | `tests/test_complete_fatigue_pipeline.py` | `main` | test_helper; [docs/registry/implementation-map.yaml:L20789-L20800] |
| 379 | `tests/test_fatigue_basic.py` | `run_all_tests` | test_helper; [docs/registry/implementation-map.yaml:L20801-L20810] |
| 380 | `tests/test_rainflow_with_effective_tension.py` | `process_effective_tension_file` | test_helper; [docs/registry/implementation-map.yaml:L20811-L20820] |
| 381 | `tests/test_rainflow_with_effective_tension.py` | `main` | test_helper; [docs/registry/implementation-map.yaml:L20821-L20831] |
| 382 | `tests/test_reservoir_framework.py` | `run_performance_tests` | test_helper; [docs/registry/implementation-map.yaml:L20832-L20841] |
| 383 | `tests/test_spectral_fatigue_validation.py` | `TestSNCurveDNVReference.test_dnv_d_curve_parameters` | test_helper; [docs/registry/implementation-map.yaml:L20842-L20853] |
| 384 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | test_helper; [docs/registry/implementation-map.yaml:L20854-L20864] |
| 385 | `tests/workflows/test_durable_workflows.py` | `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | test_helper; [docs/registry/implementation-map.yaml:L20865-L20875] |
| 386 | `tests/workflows/test_durable_workflows.py` | `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | test_helper; [docs/registry/implementation-map.yaml:L20876-L20886] |
| 387 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `PowerLawSNCurve.get_allowable_cycles` | independent; [docs/registry/implementation-map.yaml:L20887-L20897] |
| 388 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `PowerLawSNCurve.get_stress_range` | independent; [docs/registry/implementation-map.yaml:L20898-L20907] |
| 389 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `BilinearSNCurve.get_allowable_cycles` | independent; [docs/registry/implementation-map.yaml:L20908-L20917] |
| 390 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `BilinearSNCurve.get_stress_range` | independent; [docs/registry/implementation-map.yaml:L20918-L20928] |
| 391 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `BilinearSNCurve.get_allowable_cycles` | independent; [docs/registry/implementation-map.yaml:L20929-L20938] |
| 392 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `BilinearSNCurve.get_stress_range` | independent; [docs/registry/implementation-map.yaml:L20939-L20948] |

### Exact-pattern search and map follow-up

The package search used each counted family's `search_signals` verbatim as Python `re` expressions, case-sensitive; the exclusion precheck compared each pattern to `spectral_fatigue` case-insensitively and omitted none. Hits were assigned to the smallest enclosing function/class, or one module-level definition. Exact patterns from the pinned map:

wave-spectra

```json
[
  {
    "search": "algorithm",
    "pattern": "jonswap|pierson.moskowitz|bretschneider|ochi.hubble|torsethaugen|gaussian.swell|\\bISSC\\b|spectral_moment|normalize_to_significant|spectral_density|gamma\\s*\\*\\*|S_PM|S_pm|np\\.exp|math\\.exp|spectral_density|spectrum\\s*\\*|sigma\\s*="
  },
  {
    "search": "vocabulary",
    "pattern": "spectrum|spectral.density|wave.spectra|JONSWAP"
  },
  {
    "search": "constants_or_keys",
    "pattern": "0\\.07|0\\.09|0\\.287|peak_enhancement"
  }
]
```

sn-curves-and-miner-damage

```json
[
  {
    "search": "algorithm",
    "pattern": "rainflow|miner|cycles_to_failure|log_a|log10.*(?:cycles|stress)|(?:stress|stress_range|delta_sigma).*\\*\\*.*\\bm\\b|damage_sum|thickness_correction|endurance_limit|sn_curve|SNcurve|sn_data|stress_life"
  },
  {
    "search": "vocabulary",
    "pattern": "S.N.curve|fatigue.damage|C203|BS.?7608|RP.?2A|SNcurve|SN.?Curve"
  },
  {
    "search": "constants_or_keys",
    "pattern": "thickness_correction|endurance_limit|n_cycles|sn_curve|stress_ranges"
  }
]
```

| Counted family | Hit definitions and lines | Classification |
| --- | --- | --- |
| `sn-curves-and-miner-damage` | Module L6; `router` L42, L64; `_sn_curve` L139–L142 | Docstring; listed router; listed scalar validator. No further implementation. [src/digitalmodel/spectral_fatigue/workflow.py:L1-L6] [src/digitalmodel/spectral_fatigue/workflow.py:L37-L76] [src/digitalmodel/spectral_fatigue/workflow.py:L139-L143] |
| `wave-spectra` | Module L18; `_evaluate_location` L95 | Import helper; listed delegation to moments/damage library. No further implementation. [src/digitalmodel/spectral_fatigue/workflow.py:L16-L22] [src/digitalmodel/spectral_fatigue/workflow.py:L93-L98] |

The counted result is **0 further second implementations**, meeting the maximum of 1 for the package boundary; the inventory contains all 533 map rows (141 wave-spectra, 392 S-N/Miner), with no path absent at the code pin. The map's independent classification of `_sn_curve` and wave-spectrum classification of `_evaluate_location` overstate what those definitions compute; revise those relations and the unrelated #2543 affected-status tag in issue pending (3). [src/digitalmodel/spectral_fatigue/workflow.py:L93-L98] [src/digitalmodel/spectral_fatigue/workflow.py:L139-L143]

A repository-wide search at code revision `29a87f1aba14aa22067794846aa5c2ec8bf407c1` used the file set from `git ls-tree -r --name-only 29a87f1aba14aa22067794846aa5c2ec8bf407c1`, retaining only paths ending in `.py`, and read each blob with `git show <revision>:<path>`. Applying the same case-sensitive Python `re` expressions to those blobs, including `scripts/`, gives wave-spectra: 496 files, including 60 script files; S-N/Miner: 321 files, including 30 script files. These are file counts at the stated revision, not a recount of a later tree; external hits are map follow-up findings, not counted package misses. Shared spectral estimators also exist in `structural/fatigue/frequency_domain.py` and `structural/spectral_fatigue.py`; their independent correctness is outside this package review. [src/digitalmodel/structural/fatigue/frequency_domain.py:L101-L177] [src/digitalmodel/structural/spectral_fatigue.py:L1-L14]

Families covered in section 3 but lacking dedicated in-package map rows include Dirlik mixture estimation, narrow-band range estimation, Wirsching-Light correction and Benasciutti-Tovo correction: they are imported here, so they are map follow-up rather than additional counted families. External-hit classification and dedicated estimator families remain issue pending (3). [src/digitalmodel/spectral_fatigue/workflow.py:L16-L34] [src/digitalmodel/spectral_fatigue/workflow.py:L93-L98]

## 10. Coverage and open items

| Item | Coverage or unresolved work | Tracking / responsible role |
| --- | --- | --- |
| O1 | Complete negative-case coverage and enforce finite numbers, PSD shape/sign/order, occurrence totals, unique location IDs and output identity; section-6 S01–S18 retains guarded defaults as well as gaps. [src/digitalmodel/spectral_fatigue/workflow.py:L88-L127] [src/digitalmodel/spectral_fatigue/workflow.py:L130-L237] | issue pending (1); maintainers |
| O2 | Correct the default Dirlik bandwidth/irregularity substitution and qualify the narrow-band limit with C10 before relying on that estimator. [src/digitalmodel/fatigue/spectral_fatigue.py:L291-L327] | #2746, item 2; maintainers |
| O3 | Repair dependency frequency convention before relying on damage or life. The R01 keel-joint pass depends on this defect; corrected Dirlik margin is approximately 0.94 < 1.0. Re-baseline R01 and C04, including the keel-joint pass assertion, with the repair and reconcile the defect-dependent example README claim. [src/digitalmodel/fatigue/spectral_fatigue.py:L64-L71] [src/digitalmodel/fatigue/spectral_fatigue.py:L133-L143] | #2583; maintainers |
| O4 | Qualify BT correction and its broad-band range independently. [src/digitalmodel/fatigue/spectral_fatigue.py:L364-L390] | #2775; maintainers |
| O5 | Reconcile map classifications, dedicated estimator-family coverage, external search findings and documentation that groups supplied PSD and generated-grid workflows. [docs/domains/fatigue/fatg-legacy-spectral-fatigue.md:L54-L63] [src/digitalmodel/spectral_fatigue/workflow.py:L160-L171] | issue pending (3); maintainers |
| O6 | Reconcile the UNVERIFIED standard designation/edition in section 3; provide an independent router-level benchmark, resolved citations, signed gate evidence and a shared-log entry. [examples/workflows/spectral-fatigue/input.yml:L6-L9] [src/digitalmodel/spectral_fatigue/workflow.py:L60-L76] [tests/workflows/test_durable_workflows.py:L1941-L1950] | issue pending (2); maintainers |

### File coverage

All package Python files were read: empty `src/digitalmodel/spectral_fatigue/__init__.py` and `src/digitalmodel/spectral_fatigue/workflow.py`; no package-local tests, README or earlier documentation existed at the code pin. The implementation read spans the complete workflow. [src/digitalmodel/spectral_fatigue/workflow.py:L1-L237]

Documentation discovery used `rg -n 'spectral_fatigue|spectral-fatigue' docs src/digitalmodel/spectral_fatigue tests examples/workflows/spectral-fatigue`, supplemented with `rg -n 'spectral.fatigue' docs --glob '*.md'`; discovery references below were read, and completeness/validation claims in them were not adopted as qualification evidence. [docs/maps/digitalmodel-operator-map.md:L95-L100] [docs/registry/workflows.yaml:L169-L189] [docs/domains/fatigue/fatg-legacy-spectral-fatigue.md:L54-L71]

| Read surface | Coverage |
| --- | --- |
| `examples/workflows/spectral-fatigue/README.md` | The stated healthy keel-joint outcome depends on #2583; corrected Dirlik margin is approximately 0.94 < 1.0. This claim is not qualification evidence and must be reconciled with O3. [examples/workflows/spectral-fatigue/README.md:L1-L6] |
| `docs/registry/module-routing.yaml` | Existing module entry, development maturity. [docs/registry/module-routing.yaml:L646-L651] |
| `docs/registry/workflows.yaml` | Durable workflow and separate parametric atlas declaration. [docs/registry/workflows.yaml:L169-L189] |
| `docs/registry/implementation-map.yaml`, `docs/registry/implementation-map.md` | Two complete family inventories at the map pin; semantics of outside-folder inventory. [docs/registry/implementation-map.md:L45-L47] |
| `docs/maps/digitalmodel-operator-map.md` | Module routing and sibling workflows. [docs/maps/digitalmodel-operator-map.md:L85-L100] |
| `docs/domains/fatigue/fatg-legacy-spectral-fatigue.md` | Legacy comparison; supplied-PSD distinction remains O5. [docs/domains/fatigue/fatg-legacy-spectral-fatigue.md:L54-L71] |
| `docs/plans/parametric-refresh/adding-an-atlas.md` | Separate annual-damage handler and stress-gain reference. [docs/plans/parametric-refresh/adding-an-atlas.md:L70-L99] |
| `docs/plans/parametric-refresh/atlas-spec.md` | Generic fast-workflow planning claim; no performance claim adopted. [docs/plans/parametric-refresh/atlas-spec.md:L25-L32] |
| `docs/plans/2026-07-03-issue-1358-motion-forecast-keystone.md` | RAO-workflow precedent, rather than an implementation in this package. [docs/plans/2026-07-03-issue-1358-motion-forecast-keystone.md:L10-L12] |
| `docs/domains/reporting/standard-report-pack.md`, `docs/domains/fire_safety/foam-system-sizing.md` | References concern legacy reporting; no report renderer is implemented by this router. [docs/domains/reporting/standard-report-pack.md:L113-L120] [docs/domains/fire_safety/foam-system-sizing.md:L112-L118] [src/digitalmodel/spectral_fatigue/workflow.py:L54-L76] |
| `docs/vision/CALCULATIONS-VISION.md` | Vision-level fatigue validation claim; the two direct router cases here remain regression examples. [docs/vision/CALCULATIONS-VISION.md:L110-L115] [tests/workflows/test_durable_workflows.py:L1943-L1950] |

Supporting code/tests read: the fatigue library, engine dispatch, the three spectral test files and the two relevant durable-workflow definitions; sibling spectral implementations were compared for dependency and map context. [src/digitalmodel/fatigue/spectral_fatigue.py:L1-L395] [src/digitalmodel/engine.py:L340-L345] [tests/fatigue/test_spectral_fatigue.py:L1-L140] [tests/test_spectral_fatigue.py:L1-L14] [tests/test_spectral_fatigue_validation.py:L24-L42] [tests/workflows/test_durable_workflows.py:L364-L401] [tests/workflows/test_durable_workflows.py:L1893-L1950]

The calculation-citation contract was readable; its sidecar expectation is recorded as O6, rather than retrofitted into module code. The pack includes no private derived values and requires no private sidecar entry for its examples. [src/digitalmodel/spectral_fatigue/workflow.py:L60-L76] [examples/workflows/spectral-fatigue/input.yml:L1-L26]

### Offline results

| Run / test scope | BASE: passed / failed / skipped / NOT_RUN | AFTER: passed / failed / skipped / NOT_RUN |
| --- | --- | --- |
| Two router cases plus 63 dependency/comparison tests | 65 / 0 / 0 / 0 | 65 / 0 / 0 / 0 |
| Complete `tests-contracts` gate in `.claude/quality-gates.yaml`, with required pytest plugins | 1064 / 0 / 61 / 0 | 1064 / 0 / 61 / 0 |

The initial mixed-root invocation failed before collection with duplicate `tests.conftest` registration; the corrected invocation uses one temporary tree. The first contracts invocation had 1062 pass, 1 failure and 61 skips because the declared timeout plugin was missing; the missing plugin was supplied before repeating the domain. No test or marker was changed. The standard pytest addopts remained enabled. [tests/docs/test_agent_pack_node_ids.py:L33-L41] [tests/solvers/orcaflex/test_yaml_validator_timestep.py:L125-L125] [pyproject.toml:L177-L183] [pytest.ini:L48-L52] [.claude/quality-gates.yaml:L91-L111]

Documentation revision 1 rerun, 2026-10-19, before and after edits from `89c63f94550e49000274394679686c51e0677053`: the same focused nodes ran from an unmodified temporary source copy, with generated CSVs outside the checkout. The contracts gate ran against the checkout. Both used `uv run --no-project --with pytest python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>`, with the standard addopts enabled and dependencies installed in scratch.

| Revision 1 scope | Before: passed / failed / skipped | After: passed / failed / skipped |
| --- | --- | --- |
| Two router cases plus 63 dependency/comparison tests | 65 / 0 / 0 | 65 / 0 / 0 |
| Complete `tests-contracts` gate | 1063 / 0 / 62 | 1064 / 0 / 61 |

The additional before-run skip was the installed-metadata check; installing the package from the temporary source copy and running that node gave 1 pass. The other 61 skips concern existing missing or removed module layouts, not optional dependency packages. C01 rejects 8/8 invalid inputs; C02 occurrence sums are 1.0 for both locations; C06 has zero relative accumulation error; C09 captures 3 detail and 2 summary rows with no verdict mismatch and all three fingerprints. C07, C08 and C10 reproduce the documented qualification failures, independently of the passing regression counts. The repeated package sweep retains 18 documented behaviors and finds no additional unlisted failure path; both counted map families retain 0 further second implementations.
