# Corrosion module knowledge pack

| Header   | Value                                                                     |
| -------- | ------------------------------------------------------------------------- |
| Package  | `src/digitalmodel/corrosion`                                              |
| Code pin | `09089a121a16069c4b657d1a320a630a23ab1dd5`                                |
| Status   | DRAFT; software behavior documented, engineering qualification incomplete |
| Import   | `digitalmodel.corrosion`                                                  |

Summary: pairwise galvanic classification, result records, severity ordering and
a compatibility matrix are implemented. The inventory and synthetic example
support software consistency checks. Default material data and environmental
criteria have not been independently qualified; corrosion rates and physical
applicability remain NOT_EVALUATED. No corrosion acceptance decision is
established by a returned category.

### Revision history

| Revision | Date       | What changed and why                                                                                                                      |
| -------- | ---------- | ----------------------------------------------------------------------------------------------------------------------------------------- |
| 0        | 2026-10-28 | Document the public API, synthetic classification example and qualification gaps; connect the pack to the registry and consistency tests. |

## 1. Purpose and scope

The package screens pairs using differences between stored anodic indices and
returns software categories. It does not calculate penetration, loss of
thickness, service life, current demand or protection-system sizing.
[src/digitalmodel/corrosion/galvanic_screening.py:L134-L247]

This inventory covers definitions throughout the package, including public
class members under the shared AST checker contract. The initializer re-exports
existing definitions and constants; it defines no additional functions or
classes. Private helpers, imported third-party types, constants and generated
dataclass methods are not separate definition rows.
`[src/digitalmodel/corrosion/__init__.py:L7-L21]`
[scripts/docs/check_agent_pack_functions.py:L1-L10]

## 2. Inputs and outputs

Voltage inputs and outputs use V on a common potential reference. There is no
runtime unit conversion, finite-value check or physical-domain validation.
Defaults below are named without reproducing the unqualified material table or
its environmental limits. The result constructor performs no domain checks.

| Public symbol          | Inputs, units, defaults and validation                                                                                                                                                                                                                   | Output convention                                                                                                                                                                                                                                                             | Evidence                                                     |
| ---------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------ |
| `GalvanicScreenResult` | Required `metal_a`, `metal_b`, `environment` strings; `delta_v`, `allowable_delta_v` in V; `verdict` string; required `anodic`, `cathodic` strings or None. `notes` defaults to a new list. No validation.                                               | Frozen dataclass; the notes list remains mutable. Categories are OK, MARGINAL, PROTECT. None identifies equal-index pairs, not missing data.                                                                                                                                  | [src/digitalmodel/corrosion/galvanic_screening.py:L101-L113] |
| `verdict_for_delta`    | Required `delta_v`, `allowable_delta_v` in V; optional `marginal_band_v=MARGINAL_BAND_V` in V. No type, sign or finiteness checks.                                                                                                                       | Category string from successive upper-bound comparisons; supplied thresholds override defaults only when explicitly passed.                                                                                                                                                   | [src/digitalmodel/corrosion/galvanic_screening.py:L134-L144] |
| `screen_couple`        | Required `metal_a`, `metal_b` keys in ANODIC_INDEX_V. `environment='harsh_marine'`; keyword-only `environments=ENVIRONMENTS` maps keys to allowable V; `marginal_band_v=MARGINAL_BAND_V` in V. Unknown keys raise KeyError.                              | GalvanicScreenResult; verdict and absolute difference are pair-order symmetric. Larger stored index is labeled anodic; equal indices set both member labels to None. Returned delta is rounded after classification. Notes are generated text, not qualified design guidance. | [src/digitalmodel/corrosion/galvanic_screening.py:L147-L215] |
| `compatibility_matrix` | `environment='harsh_marine'`; keyword-only `metals=None` selects all stored keys in table order, otherwise a supplied name list; `environments=ENVIRONMENTS`, `marginal_band_v=MARGINAL_BAND_V`. Populated entries delegate key checks to screen_couple. | pandas DataFrame with the same ordered names on both axes and category strings in cells. With valid nonnegative thresholds it is symmetric with an OK diagonal. An empty list returns an empty frame without evaluating an environment.                                       | [src/digitalmodel/corrosion/galvanic_screening.py:L218-L242] |
| `verdict_rank`         | Required case-sensitive `verdict` string.                                                                                                                                                                                                                | Dimensionless ordering code; unknown categories raise KeyError. Ranking is specified in section 3.                                                                                                                                                                            | [src/digitalmodel/corrosion/galvanic_screening.py:L245-L247] |

Table 1. Public definitions and their implementation contracts.

No explicit public methods are defined on GalvanicScreenResult. Constructor
fields are documented in its row; generated dataclass methods are supplied by
the Python library. Both package Python files are accounted for above.
[src/digitalmodel/corrosion/galvanic_screening.py:L101-L113]
`[src/digitalmodel/corrosion/__init__.py:L1-L21]`

## 3. Method and governing sources

The implemented pair statistic is the absolute difference of stored indices.
The environment mapping supplies the allowable difference. The classifier tests
the allowable first, then allowable plus marginal band, and otherwise returns
PROTECT. This describes source behavior, not an established corrosion criterion.
[src/digitalmodel/corrosion/galvanic_screening.py:L134-L177]

The implementation adds `1e-12 V` to each upper comparison bound, rounds the
returned pair delta to six decimal places, and encodes the severity order as
OK=0, MARGINAL=1, PROTECT=2. These are software policies; their source is the
implementation, not a licensed acceptance table. Classification precedes
rounding, so a displayed delta need not reproduce a near-boundary verdict.
[src/digitalmodel/corrosion/galvanic_screening.py:L91-L91]
[src/digitalmodel/corrosion/galvanic_screening.py:L134-L144]
[src/digitalmodel/corrosion/galvanic_screening.py:L205-L214]

The source describes MIL-STD-889, _Dissimilar Metals_, and broadly republished
tables without a qualified edition, clause or traceable primary table. Those
descriptions do not establish the values' applicability. No primary material
table or environmental criterion is qualified by this pack, and no numeric
licensed-source material is reproduced.
[src/digitalmodel/corrosion/galvanic_screening.py:L38-L47]

| Topic                    | Status        | Evidence gap / requirement before engineering use                                                                                |
| ------------------------ | ------------- | -------------------------------------------------------------------------------------------------------------------------------- |
| Default metal potentials | NOT_EVALUATED | Exact primary source, edition, potential reference, alloy condition, electrolyte and applicability need qualification.           |
| Environmental thresholds | NOT_EVALUATED | Environment mapping and default marginal-band criterion need a traceable primary basis and intended-use review.                  |
| Corrosion rates          | NOT_EVALUATED | No rate model or measured rate evidence is supplied. Thickness loss and service life cannot be calculated from these categories. |
| Physical applicability   | NOT_EVALUATED | Electrolyte, temperature, exposed areas, electrical continuity, surface condition and protection details are not modeled.        |

Table 2. Qualification status; passing software tests does not close these gaps.

## 4. Worked examples and regression examples

This is a **software classification check** using explicitly synthetic inputs.
The allowable is chosen as 1 V and the marginal band as 0.25 V solely to test
interval membership. They are not corrosion acceptance thresholds and do not
use the default metal or environment table.

Independent hand calculation: adding the chosen widths gives an upper marginal
bound of 1 + 0.25 = 1.25 V. For nonnegative differences, the intended arithmetic
intervals are OK through 1 V inclusive, MARGINAL above 1 V through 1.25 V
inclusive, and PROTECT above 1.25 V. Thus 0.5 V and 1 V are OK; 1.125 V and
1.25 V are MARGINAL; 1.5 V is PROTECT. These exact binary-fraction examples
exercise both upper endpoints and interior values without relying on material
properties or the implementation's small comparison allowance. This derivation
is the basis for every numeric example input and expected category below.

```python
from digitalmodel.corrosion.galvanic_screening import verdict_for_delta

# Synthetic interval choices and hand derivation: section 4 above.
allowable_delta_v = 1.0
marginal_band_v = 0.25
deltas_v = (0.5, 1.0, 1.125, 1.25, 1.5)
verdicts = tuple(
    verdict_for_delta(delta_v, allowable_delta_v, marginal_band_v)
    for delta_v in deltas_v
)
print(verdicts)
```

Expected output: `('OK', 'OK', 'MARGINAL', 'MARGINAL', 'PROTECT')`.
The documentation test executes this exact block and compares literal categories
against the independent derivation; it also checks the supplied inputs.
`tests/docs/test_corrosion_agent_pack.py::test_documented_example_matches_independent_calculation`

## 5. QA gates and executable checks

Run from the repository root with declared test dependencies available. Scratch
basetemp and logs belong under the configured TMPDIR; the command disables the
pytest cache. No solver is needed for the corrosion example or AST checks.

```bash
PYTHONPATH=src uv run --no-project --with pytest python -m pytest \
  tests/corrosion tests/docs/test_corrosion_agent_pack.py \
  tests/docs/test_agent_pack_functions.py tests/test_main_module_routing.py \
  -q -rA -p no:cacheprovider --basetemp="$TMPDIR/corrosion-pack-tests"
uv run --no-project python scripts/docs/check_agent_pack_functions.py \
  --pack src/digitalmodel/corrosion/docs/agent-pack.md
```

| Check                                                     | Supported observation                                                                                                                                  | Limit                                                           |
| --------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------ | --------------------------------------------------------------- |
| `test_pack_matches_public_definitions`                    | Real pack matches the existing recursive AST inventory; registry resolves to this pack.                                                                | Source identity and locator agreement, not scientific validity. |
| `test_added_and_stale_symbols_are_rejected`               | Matching scratch copies pass; a new source definition and a stale table row each produce a failing CLI status with the correct directional diagnostic. | Synthetic package only; real sources are never mutated.         |
| `test_documented_example_matches_independent_calculation` | The actual documentation block returns the exact independently derived category tuple.                                                                 | Synthetic interval arithmetic only.                             |
| `test_pack_retains_ten_sections_and_qualification_limits` | Numbered form and explicit NOT_EVALUATED topics remain present.                                                                                        | Presence is not a qualification review.                         |

Table 3. Package-specific checks in `tests/docs/test_corrosion_agent_pack.py`.

## 6. Known failure modes and pitfalls

- Unknown metal, environment or rank keys raise KeyError. There is no key
  normalization. Nonfinite, negative or inconsistent numeric inputs are not
  rejected explicitly by the classifier or result constructor.
  [src/digitalmodel/corrosion/galvanic_screening.py:L101-L144]
  [src/digitalmodel/corrosion/galvanic_screening.py:L245-L247]
- Overriding the environment or marginal-band mapping does not replace the
  built-in metal potentials. Engineering inputs still need qualification.
  [src/digitalmodel/corrosion/galvanic_screening.py:L147-L177]
- Marginal notes contain fixed default-band wording even when a custom band is
  supplied. Notes are not parameter-aware acceptance evidence. Area-ratio and
  protection notes do not calculate a rate, area effect or protection demand.
  [src/digitalmodel/corrosion/galvanic_screening.py:L187-L203]
- Equal indices return None for both member labels, including different keys
  sharing an index. Symmetry does not establish field performance.
  [src/digitalmodel/corrosion/galvanic_screening.py:L179-L185]
- A frozen result does not freeze its notes list. Rounded output can hide a
  small boundary difference used by classification. Empty matrix input skips
  pairwise validation entirely.
  [src/digitalmodel/corrosion/galvanic_screening.py:L101-L113]
  [src/digitalmodel/corrosion/galvanic_screening.py:L205-L242]

## 7. Lessons log

An asserted default-table value is a regression expectation, not an independent
source qualification. A symbolic standards attribution without its edition and
traceable criterion leaves the engineering basis unresolved. A synthetic
classifier example can establish software behavior without promoting that basis.
[tests/corrosion/test_galvanic_screening.py:L21-L34]

Public submodule functions also need inventory rows even when the package
initializer does not re-export them. Re-exported definitions need no duplicate
row under the existing checker contract.
[scripts/docs/check_agent_pack_functions.py:L1-L10]

## 8. Maintenance and review

The module maintainer is responsible for API and source-qualification changes.
Updates to public definitions require synchronized inventory rows and valid
source locators. Changes to example inputs require a new independent derivation
and updated exact expected categories. Engineering qualification requires a
traceable primary basis and intended-use review, separate from these tests.

## 9. Dependencies and other implementations

The runtime module imports pandas, including when only the classifier is used;
dataclasses and typing come from the Python standard library.
[src/digitalmodel/corrosion/galvanic_screening.py:L50-L55]

Import `screen_couple`, `compatibility_matrix` and GalvanicScreenResult from
`digitalmodel.corrosion`. Import `verdict_for_delta` and `verdict_rank` directly
from `digitalmodel.corrosion.galvanic_screening`. A matrix calls screen_couple,
which calls verdict_for_delta; no separate corrosion command is defined here.
`[src/digitalmodel/corrosion/__init__.py:L7-L21]`
[src/digitalmodel/corrosion/galvanic_screening.py:L174-L177]
[src/digitalmodel/corrosion/galvanic_screening.py:L231-L242]

Documentation tests use pytest and PyYAML. The existing inventory checker uses
the standard library and reads ASTs without importing the target package.
Comparison with another corrosion implementation is NOT_EVALUATED.

## 10. Coverage and open items

The baseline at the code pin collected 72 corrosion tests, all passed, with
none failed or skipped. These cover default-value regression, pair symmetry,
equal-index pairs, categorical examples, monotonicity, anodic labeling, note
presence, unknown keys, custom environments, matrix dimensions/order, matrix
symmetry, subsets and environment ordering. They do not qualify the source
tables, physical criteria or notes as engineering guidance.
[tests/corrosion/test_galvanic_screening.py:L21-L157]

The shared checker baseline collected 19 tests, all passed, with none failed or
skipped. Coverage includes public class members, local re-exports, duplicate
names across files, locator bounds, stale/missing symbols and CLI failure.
`tests/docs/test_agent_pack_functions.py`

The module-routing baseline collected nine tests: four passed, five failed,
none skipped. The isolated environment lacks installed distribution metadata
and SciPy; failures concern known-command resolution and analysis dispatch,
not the corrosion API. Those routes are unchanged. The initial pytest-only
environment also lacked pandas; corrosion tests passed after provisioning it
in scratch. No routing repair or dependency declaration change is included.
`tests/test_main_module_routing.py`

The new checks cover the inventory/registry contract, synthetic drift controls,
the documented classification example and pack structure. Near-epsilon
boundaries, invalid numeric domains, result-note mutability and custom-band
note accuracy are inspected source behaviors without added runtime regression
tests here. Full-repository execution, solver integration, independent material
data qualification and physical validation are NOT_EVALUATED. Other packages in
the parent [knowledge-pack issue](https://github.com/vamseeachanta/digitalmodel/issues/3339)
remain follow-up; this pack does not close that issue.
