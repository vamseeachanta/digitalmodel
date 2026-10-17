# Asset integrity common knowledge pack

| Header | Value |
| --- | --- |
| Module | asset_integrity/common |
| Repository | digitalmodel |
| Path | `src/digitalmodel/asset_integrity/common` |
| Pack revision | 0 |
| Code revision | `bb18c1a346b31b4ce975befa41fc84d9a5ff29e5` |
| Analysis family | Engineering calculations (EC) |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |

Code citations resolve at the code revision above; implementation-map citations in section 9 resolve at that section's separate map pin.

## 1. Purpose and scope

`FAD` builds the legacy Option 1 load-ratio/stress-intensity-ratio table and delegates curve ordinates to `assessment.crack_fad.fad_curve_option1`. [src/digitalmodel/asset_integrity/common/fad.py:L1-L8]
`BS7910_2013` evaluates flaw assessment properties, critical depths and histogram-driven crack growth, with spreadsheet solution tables read during construction. [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L27-L70] [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L146-L165] [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L958-L1010]
`API579_components` implements legacy general and local metal-loss calculations; the documented canonical assessment route is `assessment/`, and the legacy API579 route is retained for compatibility. [src/digitalmodel/asset_integrity/common/API579_components.py:L46-L86] [src/digitalmodel/asset_integrity/common/API579_components.py:L229-L264] [docs/domains/asset-integrity/ffs-architecture.md:L5-L20]
The containing domain is registered as stable; this is registry metadata rather than a claim that every common helper is qualified. [docs/registry/module-routing.yaml:L15-L24]
Option 2 and Option 3 are placeholders returning `None`; external-surface circumferential and embedded stress-intensity methods are also placeholders. [src/digitalmodel/asset_integrity/common/fad.py:L57-L61] [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L466-L470]
Scope includes the common package's file, configuration, database, plotting and mathematical helpers; other asset-integrity packages keep their own scope. [src/digitalmodel/asset_integrity/common/data.py:L10-L55] [src/digitalmodel/asset_integrity/common/database.py:L29-L48] [src/digitalmodel/asset_integrity/common/visualizations.py:L3-L18] [src/digitalmodel/asset_integrity/common/math_solvers.py:L1-L39]

## 2. Inputs and outputs

### Inputs

| Input | Unit / convention | Range and default | Source / evidence |
| --- | --- | --- | --- |
| `Outer_Pipe.Material`, `Material`, `Grades`, `SMYS`, `SMUS`, `E` for `FAD` | Any consistent stress unit; ratios enter the curve | Required keys; no local fallback | [src/digitalmodel/asset_integrity/common/fad.py:L24-L55] [src/digitalmodel/asset_integrity/common/fad.py:L70-L75] |
| Legacy fracture material and primary membrane stress | Material stress and primary load enter as psi and are converted to MPa; secondary stress is formed from a stress-to-yield ratio | Required mapping; secondary conversion warrants dimensional review | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L227-L246] [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L799-L814] |
| Pipe ID, OD and wall thickness for fracture calculation | Inch input converted to mm; flaw angle converted from degrees to radians | Required geometry; embedded location also needs `p` | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L881-L894] |
| Flaw `a`, `c`, `p`, `theta`, orientation and location | Depth and half-length enter the fracture equations directly; no coordinate-frame transform | `thin_pipe` routes axial/circumferential and internal/external/embedded cases; some routes are stubs | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L146-L203] [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L466-L470] |
| FAD scan and life settings | Flaw scan uses start/end/step; histogram cycle counts are halved and scaled by life and safety factor | Required arrays; no universal qualified scan range established | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L95-L109] [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L1049-L1084] |
| `stress_intensity_factor_solution_tables`, histogram workbook | Excel columns and solution-table keys | Library filename resolution; constructor reads the tables | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L34-L49] [src/digitalmodel/asset_integrity/common/fracture_mechanics_components.py:L49-L63] |
| GML/LML wall grids and correction factors | LML annotations use inch; grid spacing is consumed from index/column coordinates | Inline or workbook input; plots default enabled | [src/digitalmodel/asset_integrity/common/API579_components.py:L23-L44] [src/digitalmodel/asset_integrity/common/API579_components.py:L229-L254] [src/digitalmodel/asset_integrity/common/API579_components.py:L843-L921] |
| YAML base and optional update | Mapping; no engineering units enforced by the loader | Update-load failure retains base values | [src/digitalmodel/asset_integrity/common/yml_utilities.py:L25-L49] [src/digitalmodel/asset_integrity/common/ymlInput.py:L8-L26] |
| ASCII external text | Whitespace/delimiter parsing; built-in float syntax | No locale-normalisation contract | [src/digitalmodel/asset_integrity/common/data.py:L214-L268] [src/digitalmodel/asset_integrity/common/data.py:L366-L404] |

### Outputs

| Output | Unit / convention | Limit / default | Source / evidence |
| --- | --- | --- | --- |
| `FAD['option_1']` | Dimensionless `L_r`, `K_r` DataFrame | Legacy grid plus a zero closing row at the collapse limit | [src/digitalmodel/asset_integrity/common/fad.py:L47-L55] |
| `K_r`, `L_r`, `K_r_allowable` | Dimensionless fracture/load ratios | Primary/secondary stress correction and toughness feed `K_r` | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L146-L165] [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L75-L93] |
| Critical and initial flaw tables | Flaw dimensions, life and ratios as configured | CSV outputs; repeated location iteration shares the same output filename | [src/digitalmodel/asset_integrity/common/fracture_mechanics_components.py:L65-L92] |
| GML results | MAWP, corrosion allowance, remaining life and acceptability ratio | Future rate can use a historical-rate floor; division by zero is not explicitly guarded | [src/digitalmodel/asset_integrity/common/API579_components.py:L46-L86] [src/digitalmodel/asset_integrity/common/API579_components.py:L796-L822] |
| LML results | Wall and flaw dimensions, pressure, RSF, Folias factor | Local-wall preconditions can raise; Level 2 profile sorting is an open defect (#2133) | [src/digitalmodel/asset_integrity/common/API579_components.py:L909-L993] [src/digitalmodel/asset_integrity/common/API579_components.py:L1049-L1061] |
| Polynomial roots; geometry distances | Same coordinate unit for distances; roots follow coefficient scaling | NumPy roots and SciPy pairwise distances; reported minimum uses the first point only | [src/digitalmodel/asset_integrity/common/math_solvers.py:L5-L15] [src/digitalmodel/asset_integrity/common/math_solvers.py:L66-L90] |

UNVERIFIED: a universal coordinate origin, force sign, phase convention and unit schema for every common-package file format has not been established; issue pending (6).

## 3. Method and governing standards

Licensed clause text was not checked against a licensed copy; the entries below identify repository references and do not reproduce standard tables.

| Method / reference | Implementation and qualification boundary | Evidence |
| --- | --- | --- |
| BS 7910, Guide to methods for assessing the acceptability of flaws in metallic structures, 2013, failure assessment diagram; source names clause 7.3.2 | `FAD.BS7910_2013_option_1` wraps the canonical curve; `get_plastic_collapse_load_ratio_limit` computes its material-strength ratio. Clause text not checked against the licensed copy | [src/digitalmodel/asset_integrity/common/fad.py:L1-L8] [src/digitalmodel/asset_integrity/common/fad.py:L24-L68] |
| BS 7910:2013, Annex M stress intensity | `get_reference_annexes` selects M.7.2.2 / M.7.2.4 / M.7.3.2 / M.7.3.4 / M.7.3.6; M.4.1 and M.4.3 contain direct formulas, table lookup uses M.2/M.4/M.7. Clause titles are not supplied by these methods. Clause text not checked against the licensed copy | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L167-L203] [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L295-L385] [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L387-L506] |
| BS 7910:2013, Annex P reference stress | P.9.2/P.9.4 route to P.18, P.10.2/P.10.4 to P.23 and P.10.6 to P.11; dynamic dispatch returns reference stress. Clause titles are not supplied. Clause text not checked against the licensed copy | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L707-L792] |
| BS 7910:2013, source names Part 6.4 and Annex D; toughness note names section 7.1.4.6 | Stress-intensity assembly, CTOD-derived toughness and plasticity correction; the J-integral alternative remains a TODO. Clause text not checked against the licensed copy | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L205-L262] [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L816-L865] |
| Histogram growth and Paris coefficients | `get_flaw_growth_from_one_cycle` chooses a configured Paris branch; depth advances while half-length is held unchanged | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L1071-L1084] [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L1111-L1122] |
| API 579-1/ASME FFS-1, Fitness-For-Service, Parts 4 and 5 | Legacy `API579GML`, `GMLAssessmentLengthEvaluation`, `GMLAcceptability`, `LMLAcceptability`, `LMLMAWPrEvaluation`, `LMLLevel2Evaluation`; no edition-specific licensed-clause qualification follows from the class name. Clause text not checked against the licensed copy | [src/digitalmodel/asset_integrity/common/API579_components.py:L560-L583] [src/digitalmodel/asset_integrity/common/API579_components.py:L764-L822] [src/digitalmodel/asset_integrity/common/API579_components.py:L843-L1061] |
| API 579 Part 9 versus BS 7910 Option 1 | The tests distinguish the canonical API Level 2 generic curve from BS Option 1; this package's legacy FAD wrapper calls the latter | [tests/asset_integrity/test_fad_curves.py:L1-L25] [src/digitalmodel/asset_integrity/common/fad.py:L38-L54] |
| General helper algorithms | Polynomial roots delegate to NumPy; K-neighbour interpolation ignores its neighbour-count argument; the distance helper takes its minimum from row zero | [src/digitalmodel/asset_integrity/common/math_solvers.py:L5-L15] [src/digitalmodel/asset_integrity/common/math_solvers.py:L33-L39] [src/digitalmodel/asset_integrity/common/math_solvers.py:L66-L90] |

The codes register lists BS 7910 as 2019 while `fad.py` explicitly identifies 2013; this edition mismatch needs correction, issue pending (2). [docs/domains/codes-register.md:L19-L20] [src/digitalmodel/asset_integrity/common/fad.py:L1-L8]
The historical crack-assessment plan names API 579:2016 Part 9, while the codes register lists 2021 for the assessment package; this does not establish an edition for the legacy metal-loss implementation, issue pending (2). [docs/plans/2026-09-24-issue-2157-crack-fe-assessment.md:L55-L58] [docs/domains/codes-register.md:L19-L20]
UNVERIFIED: edition-specific compliance, clause titles absent from source, and calculation-output Citation sidecars across legacy methods remain unqualified; issue pending (2).

## 4. Validated worked examples

The following table records observations from the offline test runs at the code pin; a result is restricted to its named node and does not qualify the full engineering workflow.

| ID / kind | Source and input | Expected / tolerance | Existing test node | BASE / AFTER |
| --- | --- | --- | --- | --- |
| V1 validated, algebra only | Closed-form factorisation of the cubic polynomial in the test; coefficients are embedded in the test | Real roots 1, 2, 3; absolute error at most 1e-8, dimensionless | `tests/asset_integrity/test_math_solvers.py::TestPolynomialSolveByCoefficients::test_cubic_three_real_roots` | PASS / PASS [tests/asset_integrity/test_math_solvers.py:L49-L54] |
| R1 regression, placeholder contract | Embedded FAD configuration; no external engineering reference | Option 2 remains None, exactly 1 sentinel | `tests/asset_integrity/test_fad.py::TestGetBS7910_2013FAD::test_option_2_is_none` | PASS / PASS [tests/asset_integrity/test_fad.py:L111-L141] |
| R2 regression, table boundary | Embedded FAD configuration; closing-row convention | Final `K_r` equals zero (dimensionless); pytest.approx comparison | `tests/asset_integrity/test_fad.py::TestBS7910Option1::test_kr_at_lr_max_is_zero` | PASS / PASS [tests/asset_integrity/test_fad.py:L151-L186] |

No independent fracture or metal-loss workflow is promoted to validated status in this pack; the legacy BS tests explicitly characterise the written algorithm and do not exercise construction with external tables. [tests/asset_integrity/test_bs7910_legacy_module.py:L1-L12]
The canonical-curve equality checks are regression comparisons of shared code, not independent references. [tests/asset_integrity/test_fad_curves.py:L159-L206]

### Offline test ledger

The form used is `PYTHONPATH=src uv run --no-sources python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>`; the configured warning policy and addopts remain active. [pytest.ini:L45-L60] [pytest.ini:L66-L69]

| Command group | Paths | BASE results | AFTER results |
| --- | --- | --- | --- |
| Complete direct-common selection | The ten worktree test files listed in section 10 | 140 collected, 2 collection errors; 0 executed; 140 NOT_RUN in this aborted command, subsequently rerun below | Identical: 140 collected, 2 collection errors, 0 executed |
| Independently collectable selection | Same selection without the two files that failed collection; no node deselection or filter change | 140 executed: 135 passed, 5 failed, 0 skipped | Identical: 135 passed, 5 failed, 0 skipped |
| Legacy engine plus standalone arithmetic, temporary test copy | `test_pyintegrity_gml_lml_api579.py` and `test_api579_calculations_standalone.py` under `tests/asset_integrity/` | 1 collection error; 0 executed | Identical: 1 collection error, 0 executed |

The first and third commands failed collection on invalid escape sequences in `data.py`; all five executed failures import that same file through the BS wrapper. [src/digitalmodel/asset_integrity/common/data.py:L214-L217] [tests/asset_integrity/test_fad_curves.py:L159-L163] [tests/asset_integrity/test_fad_curves.py:L195-L198]
The five failing nodes are the four parameter expansions of `tests/asset_integrity/test_fad_curves.py::test_three_legacy_entry_points_return_canonical_kr` and `tests/asset_integrity/test_fad_curves.py::test_get_k_r_allowable_beyond_cutoff_is_zero`. [tests/asset_integrity/test_fad_curves.py:L150-L206]
The blocked collection modules are `tests/asset_integrity/test_bs7910_legacy_module.py`, `tests/asset_integrity/test_yml_utilities_additional.py` and the legacy engine test file in the temporary copy. [tests/asset_integrity/test_bs7910_legacy_module.py:L24-L26] [tests/asset_integrity/test_yml_utilities_additional.py:L11-L11] [tests/asset_integrity/test_pyintegrity_gml_lml_api579.py:L6-L7]

Supplementary execution of `tests/asset_integrity/test_api579_calculations_standalone.py` alone produced 17 passed, 0 failed and 0 skipped; these tests evaluate arithmetic in the test bodies rather than importing the legacy implementation. [tests/asset_integrity/test_api579_calculations_standalone.py:L1-L8] [tests/asset_integrity/test_api579_calculations_standalone.py:L11-L43]
After the documentation edits, `tests/contracts` plus `tests/docs/test_digitalmodel_routing_contract.py` produced 493 passed, 1 failed and 0 skipped; every routing test passed. The one contract failure asserts discovery of the xlsxwriter site in data.py, whose invalid escape prevents AST parsing under the warning policy. This test and source are identical at the code pin. [tests/contracts/test_string_addressed_dependencies.py:L620-L635] [src/digitalmodel/asset_integrity/common/data.py:L214-L217]
Static AST enumeration of the 3 collection-blocked modules finds 42 NOT_RUN cases: 16 legacy BS helper cases, 23 YAML utility cases and 3 engine cases; literal parametrisation lists are expanded without importing source. Together with the 140 executed cases, this gives 182 discovered module-related cases; the 17 standalone arithmetic cases are separate. Runtime collection IDs for the blocked modules remain unavailable; the abort was not a solver or licence failure. No missing-package skip was accepted and no warning filter, assertion or collection guard was changed.

## 5. QA gates

AQG r0, section 10, Python calculations: G0 inputs/units/citations, G1 imports/tests, G2 hand or published check, G3 and G4 NOT_APPLICABLE, and G5 evidence. The checks below are pack-specific executable criteria; a listed criterion does not assert that the current module satisfies it.

| Gate | Applicability | Module-specific checks / numeric pass criterion and unit | Evidence and current result |
| --- | --- | --- | --- |
| G0 | Applicable | C0: resolve every code citation at the pin; 0 missing files or out-of-range citations (count). C0U: inspect the stress-conversion route and record all 3 material stress conversions plus primary/secondary load conversion sites; 0 undocumented sites (count) | Citation resolver and dimensional review; the secondary stress path needs issue pending (3). [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L227-L246] [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L799-L814] |
| G1 | Applicable | C1: run the section-4 command groups with 0 collection errors and 0 failed nodes (count); import the pure helpers through their existing tests, with no solver call | FAIL identically at BASE and AFTER: 3 blocked modules and 5 executed failures; issue pending (1). [tests/asset_integrity/test_bs7910_legacy_module.py:L24-L26] [tests/asset_integrity/test_fad_curves.py:L159-L163] |
| G2 | Applicable | C2: `Polynomial.solve_by_coefficients` cubic roots differ from 1, 2, 3 by at most 1e-8 (dimensionless absolute error); execute V1 | PASS at BASE and AFTER; applies to algebra only, not the fracture/metal-loss methods. [tests/asset_integrity/test_math_solvers.py:L49-L54] |
| G3 | NOT_APPLICABLE | No dynamic-solver response is computed by the FAD builder or the histogram-cycle update; this is the Python-calculation gate set | Text-only scope assessment. [src/digitalmodel/asset_integrity/common/fad.py:L24-L55] [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L1111-L1122] |
| G4 | NOT_APPLICABLE | No panel mesh or hydrodynamic coefficient comparison in the FAD build path; this is the Python-calculation gate set | Text-only scope assessment. [src/digitalmodel/asset_integrity/common/fad.py:L24-L55] |
| G5 | Applicable | C5: exactly 1 pinned offline result for each section-4 example, 0 missing sweep-site rows and 0 unmatched issue-draft markers (counts) | Example ledger, citation/sweep resolver, issue drafts and pack lint |

Additional executable regression checks:

| ID | Gate | Check and numeric criterion / unit | Existing evidence |
| --- | --- | --- | --- |
| C1P | G1 | FAD option-2 placeholder equals None: 1 matching sentinel (count) | R1 [tests/asset_integrity/test_fad.py:L111-L141] |
| C1B | G1 | FAD final `K_r` closing row equals 0 (dimensionless), using the existing pytest.approx assertion | R2 [tests/asset_integrity/test_fad.py:L151-L186] |
| C2R | G2 | Wrapper/canonical `K_r` differences at most 1e-12 (dimensionless absolute error) at 201 rows per material set; this is a regression-only comparison | FAIL at import; issue pending (1). [tests/asset_integrity/test_fad_curves.py:L159-L206] |

Counts: G0 has 2 checks, G1 has 3 checks, G2 has 2 checks, G3/G4 have 0 applicable checks, and G5 has 1 check; total 8.

## 6. Known failure modes and pitfalls

sweep: 213 found

The conservative structural sweep retains exception handlers, explicit empty returns, empty-data branches, numeric conversions, zero/default assignments, unit-scale sites and stubs, including intentional initialisation and CPU-probe fallbacks; a retained site is a review candidate, not proof of an erroneous answer. Each site has one row below at the code pin.

Exact textual sweep command (read as source only):

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|float\(|int\(|0\.05|25\.4|1000|unit|MPa|psi|inch|radians|cache' src/digitalmodel/asset_integrity/common -g '*.py'
```

The companion AST classification retains every exception handler, continue, explicit empty return, direct numeric zero/default (including dictionary zeros), scalar/vector numeric conversion, fractional constant, literal dimension/time scaling, explicit unit/angle conversion, pass statement and empty-data branch; manual source review adds implicit empty results, unit assumptions, fixed engineering fallbacks and overwrites; it parses source without importing the package.

| Finding | Symptom / cause | Detection check or gap | Tracking / evidence |
| --- | --- | --- | --- |
| Python warning policy blocks imports | Invalid escapes become a collection SyntaxError on Python 3.12 | C1; 2 collection errors in the worktree command, 1 in the temporary-copy command | issue pending (1) [src/digitalmodel/asset_integrity/common/data.py:L214-L217] |
| FAD options absent | Options 2 and 3 return None | C1P detects the placeholder contract; it does not supply an implementation | issue pending (3) [src/digitalmodel/asset_integrity/common/fad.py:L57-L61] |
| Applicability is only logged | Out-of-range flaw ratios do not stop evaluation | no detection check exists for enforced rejection; existing legacy tests check logging only | issue pending (3) [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L625-L705] |
| Finite-width fallback | Failed square-root/secant evaluation substitutes unity | no detection check exists | issue pending (3) [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L264-L288] |
| Fixed stress-intensity fallback | Missing/constant-mode setting selects embedded constants; discontinuity factors are fixed to unity | no detection check exists | issue pending (3) [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L387-L416] [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L794-L797] |
| Secondary stress dimensions | Converted yield stress is multiplied by another psi-to-MPa conversion after forming a dimensionless stress-to-yield ratio | no detection check exists | issue pending (3) [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L227-L246] [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L799-L814] |
| Table lookup clamps and mixes axes | Out-of-table inputs clamp to edge values; three interpolation factors are averaged; theta_location is not used in the row filters | no detection check exists for physical validity; legacy tests characterise this algorithm | issue pending (3) [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L529-L623] |
| Growth fallback and omission | Initial depth fallback and swallowed out-of-bounds result append can retain incomplete results | no detection check exists | issue pending (3) [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L1038-L1049] [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L1234-L1246] |
| Critical-depth scan edges | Result selects the row preceding df_index; first-failure, no-crossing and short scans need explicit expected outcomes | no detection check exists | issue pending (3) [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L111-L144] |
| Location CSV overwrite | Location loop writes all fracture-limit tables to the same filename | no detection check exists | issue pending (5) [src/digitalmodel/asset_integrity/common/fracture_mechanics_components.py:L71-L77] |
| Spatial profile sorting | Sorting removes adjacency and slice/length use different sample counts; known incident #2133 | no detection check exists in the common-package tests | #2133 [src/digitalmodel/asset_integrity/common/API579_components.py:L1049-L1061] |
| Wall/rate and branch boundaries | Remaining-life division has no positive-rate guard; LML reduction can read unassigned Mt/RSFa and has no branch at the upper boundary equality | no detection check exists | issue pending (4) [src/digitalmodel/asset_integrity/common/API579_components.py:L796-L822] [src/digitalmodel/asset_integrity/common/API579_components.py:L1008-L1047] |
| Grid and interpolation defaults | Empty grids are dropped; unit-pitch simulation and endpoint interpolation can conceal range limits | no detection check exists | issue pending (4) [src/digitalmodel/asset_integrity/common/API579_components.py:L503-L558] [src/digitalmodel/asset_integrity/common/API579_components.py:L72-L77] |
| YAML update fallbacks | Update-load errors continue using default values | no detection check exists for the legacy loader; the utility tests are blocked at import | #620 [src/digitalmodel/asset_integrity/common/ymlInput.py:L13-L24] [src/digitalmodel/asset_integrity/common/yml_utilities.py:L36-L47] |
| YAML validation tuple | is_file_valid_func returns a tuple, while the loader tests its truthiness as a boolean | no detection check exists that ran successfully | issue pending (5) [src/digitalmodel/asset_integrity/common/utilities.py:L26-L44] [src/digitalmodel/asset_integrity/common/yml_utilities.py:L25-L28] |
| Text/locale assumptions | ASCII float conversion has no thousands/decimal normalisation and no malformed-row completeness record | no detection check exists | issue pending (5) [src/digitalmodel/asset_integrity/common/data.py:L245-L260] [src/digitalmodel/asset_integrity/common/data.py:L366-L404] |
| Neighbour count and reported minimum | Interpolator ignores supplied n_neighbors; geometry minimum is drawn from distance row zero | no detection check exists for these counterexamples | issue pending (5) [src/digitalmodel/asset_integrity/common/math_solvers.py:L35-L39] [src/digitalmodel/asset_integrity/common/math_solvers.py:L76-L90] |
| Previous FAD duplication | Consolidated wrappers delegate to canonical implementation; historical incident #2160 | C2R exists, currently blocked at import | #2160 [src/digitalmodel/asset_integrity/common/fad.py:L38-L54] [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L75-L93] |
| Related reference-wall audit | #2345 concerns equivalent-LTA callers outside this common-package calculation; do not substitute the legacy path as a qualified reference | no detection check exists for common-package qualification | #2345 [src/digitalmodel/asset_integrity/common/API579_components.py:L901-L941] |
| Related disabled fracture route | Parent fracture entry can return without analysis when its flag is false; #2598 also records this omission | no detection check exists for a common-package status contract | #2598 [src/digitalmodel/asset_integrity/fracture_mechanics.py:L1-L23] |

### Sweep site inventory

Date for this source review: 2026-10-16. Tracking is by issue/draft; no historical occurrence date is inferred.

| Site | Kind / enclosing definition | Symptom and cause | Detection / tracking | Citation |
| --- | --- | --- | --- | --- |
| S001 | dictionary-zero; `API579_components.plot_and_save_gml_results` | Zero in a dictionary/example configuration; distinguish a supplied zero from missing input. | no detection check exists; issue pending (4) | [src/digitalmodel/asset_integrity/common/API579_components.py:L118-L118] |
| S002 | fractional-constant; `API579_components.lml` | Literal fractional constant in an equation, limit or plot placement; do not treat it as a universal engineering default. | no detection check exists; issue pending (4) | [src/digitalmodel/asset_integrity/common/API579_components.py:L300-L300] |
| S003 | fractional-constant; `API579_components.lml` | Literal fractional constant in an equation, limit or plot placement; do not treat it as a universal engineering default. | no detection check exists; issue pending (4) | [src/digitalmodel/asset_integrity/common/API579_components.py:L374-L374] |
| S004 | fractional-constant; `API579_components.b31g` | Literal fractional constant in an equation, limit or plot placement; do not treat it as a universal engineering default. | no detection check exists; issue pending (4) | [src/digitalmodel/asset_integrity/common/API579_components.py:L477-L477] |
| S005 | fractional-constant; `API579_components.b31g` | Literal fractional constant in an equation, limit or plot placement; do not treat it as a universal engineering default. | no detection check exists; issue pending (4) | [src/digitalmodel/asset_integrity/common/API579_components.py:L481-L481] |
| S006 | empty-branch; `API579_components.read_gml_grids_from_inline` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (4) | [src/digitalmodel/asset_integrity/common/API579_components.py:L508-L509] |
| S007 | empty-branch; `API579_components.read_gml_grids_from_simulations` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (4) | [src/digitalmodel/asset_integrity/common/API579_components.py:L516-L517] |
| S008 | conversion; `API579_components.simulate_wt_grid` | `int()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (4) | [src/digitalmodel/asset_integrity/common/API579_components.py:L527-L527] |
| S009 | conversion; `API579_components.simulate_wt_grid` | `int()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (4) | [src/digitalmodel/asset_integrity/common/API579_components.py:L532-L532] |
| S010 | conversion; `API579_components.simulate_wt_grid` | `int()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (4) | [src/digitalmodel/asset_integrity/common/API579_components.py:L534-L534] |
| S011 | empty-branch; `API579_components.read_gml_grids_from_excel` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (4) | [src/digitalmodel/asset_integrity/common/API579_components.py:L557-L558] |
| S012 | fallback; `API579_components.GMLAssessmentLengthEvaluation` | Outside the reduced-thickness branch, the length parameter is capped to a fixed value. | no detection check exists; issue pending (4) | [src/digitalmodel/asset_integrity/common/API579_components.py:L764-L794] |
| S013 | fallback; `API579_components.GMLAcceptability` | Historical corrosion rate can be floored; a nonpositive explicit future rate is unguarded. | no detection check exists; issue pending (4) | [src/digitalmodel/asset_integrity/common/API579_components.py:L796-L822] |
| S014 | fractional-constant; `API579_components.LMLAcceptability` | Literal fractional constant in an equation, limit or plot placement; do not treat it as a universal engineering default. | no detection check exists; issue pending (4) | [src/digitalmodel/asset_integrity/common/API579_components.py:L915-L915] |
| S015 | fallback; `API579_components.LMLMAWPrEvaluation` | Piecewise remaining-thickness floors leave boundary and variable-initialisation gaps. | no detection check exists; issue pending (4) | [src/digitalmodel/asset_integrity/common/API579_components.py:L1008-L1047] |
| S016 | empty-branch; `setupApplicationRuns.wrapper_applicationRuns` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/ApplicationManager.py:L61-L73] |
| S017 | catch; `ConfigureApplicationInputs.unify_application_and_default_and_custom_yamls` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/ApplicationManager.py:L173-L176] |
| S018 | catch; `ConfigureApplicationInputs.get_custom_file` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/ApplicationManager.py:L191-L195] |
| S019 | catch; `ConfigureApplicationInputs.generateYMLInput` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/ApplicationManager.py:L220-L225] |
| S020 | catch; `ConfigureApplicationInputs.configure_overwrite_filenames` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/ApplicationManager.py:L311-L312] |
| S021 | stub; `ConfigureApplicationInputs.configure_overwrite_filenames` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/ApplicationManager.py:L312-L312] |
| S022 | stub; `SaveApplicationResults.__init__` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/ApplicationManager.py:L321-L321] |
| S023 | catch; `SaveApplicationResults.saveApplicationResultsToDB` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/ApplicationManager.py:L371-L372] |
| S024 | catch; `SaveApplicationResults.saveApplicationResultsToDB` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/ApplicationManager.py:L406-L407] |
| S025 | stub; `BS7910_2013.get_minimum_allowable_initial_flaw` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L73-L73] |
| S026 | conversion; `BS7910_2013.get_K_r_allowable` | `float()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L89-L89] |
| S027 | conversion; `BS7910_2013.iterate_flaw_length_for_unstable_limits` | `int()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L102-L102] |
| S028 | conversion; `BS7910_2013.iterate_flaw_length_for_unstable_limits_for_single_c_value` | `int()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L134-L134] |
| S029 | units; `BS7910_2013.get_stress_intensity_factor_solution` | Unit or angular conversion; inconsistent upstream units alter the result. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L230-L230] |
| S030 | units; `BS7910_2013.get_stress_intensity_factor_solution` | Unit or angular conversion; inconsistent upstream units alter the result. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L234-L234] |
| S031 | units; `BS7910_2013.get_stress_intensity_factor_solution` | Unit or angular conversion; inconsistent upstream units alter the result. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L238-L238] |
| S032 | catch; `BS7910_2013.get_finite_width_correction_factor` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L274-L276] |
| S033 | catch; `BS7910_2013.get_finite_width_correction_factor` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L282-L284] |
| S034 | fractional-constant; `BS7910_2013.get_stress_intensity_factor_solution_Annex_M_4_3` | Literal fractional constant in an equation, limit or plot placement; do not treat it as a universal engineering default. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L358-L358] |
| S035 | default; `BS7910_2013.get_stress_intensity_factor_solution_Annex_M_4_3` | Initialises or resets `lamda_3`; a numeric sentinel may be consumed as a result. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L369-L369] |
| S036 | fallback; `BS7910_2013.get_stress_intensity_factor_solution_Annex_M_7_2_2` | Missing solution-mode key chooses fixed approximate stress-intensity values. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L387-L416] |
| S037 | default; `BS7910_2013.get_stress_intensity_solution_by_table_data` | Initialises or resets `theta`; a numeric sentinel may be consumed as a result. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L486-L486] |
| S038 | default; `BS7910_2013.dataframe_manual_interpolation_using_filters` | Initialises or resets `a_over_B_factor`; a numeric sentinel may be consumed as a result. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L595-L595] |
| S039 | default; `BS7910_2013.dataframe_manual_interpolation_using_filters` | Initialises or resets `a_over_c_factor`; a numeric sentinel may be consumed as a result. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L602-L602] |
| S040 | default; `BS7910_2013.dataframe_manual_interpolation_using_filters` | Initialises or resets `B_over_ri_factor`; a numeric sentinel may be consumed as a result. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L609-L609] |
| S041 | fractional-constant; `BS7910_2013.perform_flaw_dimension_acceptance_check` | Literal fractional constant in an equation, limit or plot placement; do not treat it as a universal engineering default. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L636-L636] |
| S042 | fallback; `BS7910_2013.get_stress_intensity_factor_at_discontinuities` | Discontinuity factors are unity without evaluating a geometric discontinuity. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L794-L797] |
| S043 | units; `BS7910_2013.init_assign_key_properties` | Unit or angular conversion; inconsistent upstream units alter the result. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L805-L805] |
| S044 | units; `BS7910_2013.init_assign_key_properties` | Unit or angular conversion; inconsistent upstream units alter the result. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L809-L809] |
| S045 | units; `BS7910_2013.init_assign_key_properties` | Unit or angular conversion; inconsistent upstream units alter the result. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L811-L811] |
| S046 | default; `BS7910_2013.init_assign_key_properties` | Initialises or resets `self.delta_P_m`; a numeric sentinel may be consumed as a result. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L813-L813] |
| S047 | default; `BS7910_2013.init_assign_key_properties` | Initialises or resets `self.delta_P_b`; a numeric sentinel may be consumed as a result. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L814-L814] |
| S048 | fallback; `BS7910_2013.get_critical_stress_intensity_factor` | Outside the yield/tensile-ratio interval, toughness uses a fixed m value; J-integral route remains a TODO. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L816-L831] |
| S049 | fallback; `BS7910_2013.get_plasticity_correction` | Piecewise rho/V fallbacks include zeros and unity; input-domain guard evidence is absent. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L833-L865] |
| S050 | default; `BS7910_2013.get_plasticity_correction` | Initialises or resets `rho`; a numeric sentinel may be consumed as a result. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L835-L835] |
| S051 | default; `BS7910_2013.get_plasticity_correction` | Initialises or resets `rho`; a numeric sentinel may be consumed as a result. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L852-L852] |
| S052 | default; `BS7910_2013.get_plasticity_correction` | Initialises or resets `rho`; a numeric sentinel may be consumed as a result. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L854-L854] |
| S053 | units; `BS7910_2013.assign_key_flaw_dimensions_for_calculations` | Literal scale or time conversion; dimensions and source units need checking before reuse. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L882-L882] |
| S054 | units; `BS7910_2013.assign_key_flaw_dimensions_for_calculations` | Literal scale or time conversion; dimensions and source units need checking before reuse. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L883-L883] |
| S055 | units; `BS7910_2013.assign_key_flaw_dimensions_for_calculations` | Unit or angular conversion; inconsistent upstream units alter the result. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L884-L884] |
| S056 | units; `BS7910_2013.assign_key_flaw_dimensions_for_calculations` | Literal scale or time conversion; dimensions and source units need checking before reuse. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L888-L889] |
| S057 | empty-branch; `BS7910_2013.get_approximate_flaw_growth_due_to_histograms` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L935-L937] |
| S058 | empty-branch; `BS7910_2013.get_approximate_flaw_growth_due_to_histograms` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L946-L954] |
| S059 | empty-branch; `BS7910_2013.get_flaw_growth_due_to_histograms` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L990-L992] |
| S060 | empty-branch; `BS7910_2013.get_flaw_growth_due_to_histograms` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L1001-L1008] |
| S061 | fallback; `BS7910_2013.get_flaw_growth_for_single_flaw_location` | Empty/invalid approximate growth result selects a fixed starting depth; service life starts at zero. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L1033-L1049] |
| S062 | empty-branch; `BS7910_2013.get_flaw_growth_for_single_flaw_location` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L1036-L1041] |
| S063 | catch; `BS7910_2013.get_flaw_growth_for_single_flaw_location` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L1042-L1043] |
| S064 | default; `BS7910_2013.get_flaw_growth_for_single_flaw_location` | Initialises or resets `service_life`; a numeric sentinel may be consumed as a result. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L1044-L1044] |
| S065 | conversion; `BS7910_2013.get_flaw_growth_for_single_flaw_location` | `int()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L1049-L1049] |
| S066 | default; `BS7910_2013.get_flaw_growth_for_single_flaw_location` | Initialises or resets `self.delta_P_m`; a numeric sentinel may be consumed as a result. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L1078-L1078] |
| S067 | units; `BS7910_2013.get_flaw_growth_for_single_flaw_location` | Literal scale or time conversion; dimensions and source units need checking before reuse. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L1079-L1082] |
| S068 | conversion; `BS7910_2013.get_approximate_flaw_growth_solution_for_single_flaw_location` | `int()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L1179-L1179] |
| S069 | default; `BS7910_2013.get_approximate_flaw_growth_solution_for_single_flaw_location` | Initialises or resets `self.delta_P_m`; a numeric sentinel may be consumed as a result. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L1193-L1193] |
| S070 | units; `BS7910_2013.get_approximate_flaw_growth_solution_for_single_flaw_location` | Literal scale or time conversion; dimensions and source units need checking before reuse. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L1194-L1197] |
| S071 | catch; `BS7910_2013.get_approximate_flaw_growth_solution_for_single_flaw_location` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L1238-L1241] |
| S072 | default; `render_df_table` | Numeric argument default in `render_df_table`; absence is not separately represented. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/DataFrame_To_Image.py:L17-L17] |
| S073 | catch; `DataFrame_To_xlsx_xlsxwriter` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/DataFrame_To_xlsx.py:L17-L19] |
| S074 | catch; `DataFrame_To_xlsx_openpyxl` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/DataFrame_To_xlsx.py:L35-L36] |
| S075 | catch; `DataFrame_To_xlsx_openpyxl` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/DataFrame_To_xlsx.py:L41-L43] |
| S076 | stub; `ReadFromExcel` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L7-L7] |
| S077 | default; `ReadData.df_filter_by_column_values` | Numeric argument default in `df_filter_by_column_values`; absence is not separately represented. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L12-L12] |
| S078 | default; `ReadData.from_xlsx` | Numeric argument default in `from_xlsx`; absence is not separately represented. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L34-L34] |
| S079 | implicit-empty; `ReadData.from_xlsx_get_WorkSheetRowNumberWithText` | Keyword absence falls through without returning a row. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L129-L137] |
| S080 | default; `ReadData.from_pdf` | Numeric argument default in `from_pdf`; absence is not separately represented. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L155-L155] |
| S081 | default; `ReadData.from_pdf_tabula` | Numeric argument default in `from_pdf_tabula`; absence is not separately represented. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L166-L166] |
| S082 | default; `ReadData.from_pdf_camelot` | Numeric argument default in `from_pdf_camelot`; absence is not separately represented. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L173-L173] |
| S083 | stub; `ReadData.update_extension_of_filename` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L185-L185] |
| S084 | implicit-empty; `ReadData.from_ascii_file_get_line_number_containing_keywords` | Keyword absence falls through; caller arithmetic can receive no row. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L187-L212] |
| S085 | dictionary-zero; `ReadData.from_ascii_file_get_line_number_containing_keywords` | Zero in a dictionary/example configuration; distinguish a supplied zero from missing input. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L194-L194] |
| S086 | conversion; `ReadData.from_ascii_file_get_structured_data_delimited_white_space` | `float()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L252-L252] |
| S087 | empty-result; `ReadData.get_file_list_from_folder` | No matched files yields an empty list without an explicit missing-input status. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L307-L327] |
| S088 | implicit-empty; `FromString.using_regex` | No regular-expression match falls through to an implicit absent result. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L360-L364] |
| S089 | empty-branch; `FromString.using_regex` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L363-L364] |
| S090 | conversion; `FromString.convert_fraction_to_float` | `float()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L368-L368] |
| S091 | catch; `FromString.convert_fraction_to_float` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/data.py:L369-L377] |
| S092 | conversion; `FromString.convert_fraction_to_float` | `float()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L373-L373] |
| S093 | catch; `FromString.convert_fraction_to_float` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/data.py:L374-L375] |
| S094 | default; `FromString.convert_fraction_to_float` | Initialises or resets `whole`; a numeric sentinel may be consumed as a result. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L375-L375] |
| S095 | conversion; `FromString.convert_fraction_to_float` | `float()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L376-L376] |
| S096 | conversion; `FromString.convert_fraction_to_float` | `float()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L376-L376] |
| S097 | conversion; `FromString.get_value_by_delimiter` | `float()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L397-L397] |
| S098 | conversion; `FromString.get_value_by_delimiter` | `float()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L402-L402] |
| S099 | catch; `SaveData.DataFrame_To_xlsx_xlsxwriter` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/data.py:L459-L461] |
| S100 | catch; `SaveData.DataFrame_To_xlsx_openpyxl` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/data.py:L480-L481] |
| S101 | catch; `SaveData.DataFrame_To_xlsx_openpyxl` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/data.py:L486-L488] |
| S102 | default; `SaveData.render_df_table` | Numeric argument default in `render_df_table`; absence is not separately represented. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L581-L581] |
| S103 | stub; `Transform.numpy_interp` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L679-L679] |
| S104 | units; `Transform.gis_deg_to_distance / get_gis_converted_df_superseded` | Geographic/projected-coordinate helpers assume latitude/longitude, a UTM zone and projection convention; failed rows retain absent coordinates. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L681-L734] |
| S105 | conversion; `Transform.gis_deg_to_distance` | `float()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L692-L692] |
| S106 | conversion; `Transform.gis_deg_to_distance` | `float()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L693-L693] |
| S107 | catch; `Transform.gis_deg_to_distance` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/data.py:L695-L696] |
| S108 | stub; `Transform.gis_distance_to_deg` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L710-L710] |
| S109 | conversion; `Transform.get_gis_converted_df_superseded` | Vectorised numeric conversion; external decimal/thousands separators are not normalised. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L719-L719] |
| S110 | conversion; `Transform.get_gis_converted_df_superseded` | Vectorised numeric conversion; external decimal/thousands separators are not normalised. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L720-L720] |
| S111 | conversion; `Transform.get_gis_converted_df_superseded` | Vectorised numeric conversion; external decimal/thousands separators are not normalised. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L725-L725] |
| S112 | conversion; `Transform.get_gis_converted_df_superseded` | Vectorised numeric conversion; external decimal/thousands separators are not normalised. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L726-L726] |
| S113 | empty-result; `Transform.dataframe_to_dict` | Absent frame or non-unique columns returns the initially empty dictionary. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L742-L751] |
| S114 | empty-result; `Transform.dataframe_to_json` | Absent frame returns the initially empty string; duplicate-column path transforms names. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L765-L780] |
| S115 | stub; `Transform.transform_list_to_unique_list` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L803-L803] |
| S116 | catch; `Transform.dataframe_to_html` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/data.py:L868-L869] |
| S117 | stub; `TransformData.__init__` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L890-L890] |
| S118 | units; `TransformData.linear` | Caller-selected affine scale and shift are applied without a unit contract. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L896-L907] |
| S119 | empty-branch; `transform_df_datetime_to_str` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/data.py:L964-L968] |
| S120 | catch; `get_db_connection` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/database.py:L9-L13] |
| S121 | stub; `test_saving_service_data_to_db` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/database.py:L26-L26] |
| S122 | retry; `Database.db_retry_decorator.wrapper` | Any exception triggers recursive retry without a retry bound or an explicit terminal failure. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/database.py:L50-L62] |
| S123 | catch; `Database.db_retry_decorator.wrapper` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/database.py:L58-L60] |
| S124 | catch; `Database.get_input_data` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/database.py:L67-L68] |
| S125 | catch; `Database.get_input_data` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/database.py:L74-L75] |
| S126 | catch; `Database.set_up_db_connection` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/database.py:L81-L84] |
| S127 | catch; `Database.enable_connection_and_cursor` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/database.py:L115-L117] |
| S128 | catch; `Database.enable_connection_and_cursor` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/database.py:L134-L136] |
| S129 | catch; `Database.enable_connection_and_cursor` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/database.py:L170-L172] |
| S130 | catch; `Database.enable_connection_and_cursor` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/database.py:L202-L204] |
| S131 | catch; `Database.enable_connection_and_cursor` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/database.py:L231-L235] |
| S132 | catch; `Database.get_columns_for_all_db_tables` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/database.py:L349-L352] |
| S133 | empty-branch; `Database.get_table_basic_statistics` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/database.py:L371-L372] |
| S134 | catch; `Database.get_table_basic_statistics` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/database.py:L373-L375] |
| S135 | empty-branch; `Database.get_timeline_statistics` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/database.py:L469-L496] |
| S136 | time-assumption; `Database.get_timeline_statistics` | Minute/second/microsecond truncation and integer interval counts can omit a partial final interval. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/database.py:L476-L479] |
| S137 | conversion; `Database.get_timeline_statistics` | `int()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/database.py:L479-L479] |
| S138 | empty-branch; `Database.get_timeline_statistics` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/database.py:L489-L493] |
| S139 | catch; `Database.get_timeline_statistics` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/database.py:L497-L500] |
| S140 | empty-branch; `Database.save_to_db` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/database.py:L540-L552] |
| S141 | catch; `Database.executeScriptsFromFile` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/database.py:L616-L617] |
| S142 | catch; `Database.executeQueryWithParameters` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/database.py:L637-L639] |
| S143 | catch; `Database.executeNoDataQuery` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/database.py:L653-L654] |
| S144 | empty; `Database.executeNoDataQuery` | Explicit empty or absent result; caller must distinguish no data from unsuccessful processing. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/database.py:L655-L655] |
| S145 | catch; `Database.executeNoDataQuery` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/database.py:L671-L672] |
| S146 | empty-branch; `Database.perform_extreme_event_analysis` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/database.py:L733-L743] |
| S147 | catch; `Database.perform_extreme_event_analysis` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/database.py:L739-L740] |
| S148 | empty-branch; `Database.format_table_statistics_df` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/database.py:L778-L779] |
| S149 | conversion; `Database.format_table_statistics_df` | `float()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/database.py:L779-L779] |
| S150 | conversion; `Database.format_table_statistics_df_for_extreme_events` | `float()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/database.py:L802-L802] |
| S151 | overwrite; `FractureMechanicsComponents.save_result_tables` | Every location uses the same fracture-limit CSV name, replacing previous location output. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/fracture_mechanics_components.py:L71-L77] |
| S152 | empty-branch; `FractureMechanicsComponents.create_viz_min_init_flaw_by_component` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/fracture_mechanics_components.py:L185-L193] |
| S153 | empty-branch; `FractureMechanicsComponents.create_viz_min_init_flaw_by_service_life` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/fracture_mechanics_components.py:L236-L244] |
| S154 | empty-branch; `FractureMechanicsComponents.create_viz_min_init_flaw_by_flaw_location` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/fracture_mechanics_components.py:L285-L293] |
| S155 | empty-branch; `FractureMechanicsComponents.create_viz_min_init_flaw_by_flaw_orientation` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/fracture_mechanics_components.py:L337-L345] |
| S156 | stub; `FractureMechanicsComponents.create_viz_flaw_growth_by_service_life` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/fracture_mechanics_components.py:L403-L403] |
| S157 | stub; `FractureMechanicsComponents.create_viz_flaw_growth_by_flaw_location` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/fracture_mechanics_components.py:L406-L406] |
| S158 | stub; `FractureMechanicsComponents.create_viz_flaw_growth_by_flaw_orientation` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (3) | [src/digitalmodel/asset_integrity/common/fracture_mechanics_components.py:L409-L409] |
| S159 | conversion; `Polynomial.solve_using_sympy` | `float()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/math_solvers.py:L30-L30] |
| S160 | units; `FFT_Methods.numpy_fft` | Fixed sample spacing and signal frequencies are demonstration assumptions; the sample-count expression also needs integer-type qualification. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/math_solvers.py:L44-L61] |
| S161 | resource-default; `ParallelProcessComponents.get_number_of_workers` | Worker count is a floored CPU percentage with no positive minimum or caller resource cap. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L8-L12] |
| S162 | conversion; `ParallelProcessComponents.available_cpu_count` | `int()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L63-L63] |
| S163 | catch; `ParallelProcessComponents.available_cpu_count` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L66-L67] |
| S164 | stub; `ParallelProcessComponents.available_cpu_count` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L67-L67] |
| S165 | catch; `ParallelProcessComponents.available_cpu_count` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L73-L74] |
| S166 | stub; `ParallelProcessComponents.available_cpu_count` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L74-L74] |
| S167 | catch; `ParallelProcessComponents.available_cpu_count` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L80-L81] |
| S168 | stub; `ParallelProcessComponents.available_cpu_count` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L81-L81] |
| S169 | conversion; `ParallelProcessComponents.available_cpu_count` | `int()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L86-L86] |
| S170 | catch; `ParallelProcessComponents.available_cpu_count` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L90-L91] |
| S171 | stub; `ParallelProcessComponents.available_cpu_count` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L91-L91] |
| S172 | conversion; `ParallelProcessComponents.available_cpu_count` | `int()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L96-L96] |
| S173 | catch; `ParallelProcessComponents.available_cpu_count` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L100-L101] |
| S174 | stub; `ParallelProcessComponents.available_cpu_count` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L101-L101] |
| S175 | catch; `ParallelProcessComponents.available_cpu_count` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L110-L111] |
| S176 | stub; `ParallelProcessComponents.available_cpu_count` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L111-L111] |
| S177 | conversion; `ParallelProcessComponents.available_cpu_count` | `int()` conversion; external decimal/thousands separators require an explicit caller contract; computed inputs may truncate. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L119-L119] |
| S178 | catch; `ParallelProcessComponents.available_cpu_count` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L123-L124] |
| S179 | stub; `ParallelProcessComponents.available_cpu_count` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L124-L124] |
| S180 | catch; `ParallelProcessComponents.available_cpu_count` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L132-L133] |
| S181 | stub; `ParallelProcessComponents.available_cpu_count` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L133-L133] |
| S182 | default; `ParallelProcessComponents.available_cpu_count` | Initialises or resets `res`; a numeric sentinel may be consumed as a result. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L140-L140] |
| S183 | catch; `ParallelProcessComponents.available_cpu_count` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L147-L148] |
| S184 | stub; `ParallelProcessComponents.available_cpu_count` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L148-L148] |
| S185 | catch; `ParallelProcessComponents.available_cpu_count` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L154-L156] |
| S186 | default; `ParallelProcessComponents.available_cpu_count` | Initialises or resets `res`; a numeric sentinel may be consumed as a result. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L158-L158] |
| S187 | catch; `ParallelProcessComponents.available_cpu_count` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L164-L165] |
| S188 | stub; `ParallelProcessComponents.available_cpu_count` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L165-L165] |
| S189 | stub; `dump_ordered_yaml.OrderedDumper` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/saveData.py:L30-L30] |
| S190 | stub; `dump_ordered_yaml.UnsortableList.sort` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/saveData.py:L35-L35] |
| S191 | stub; `<module>` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/update_deep.py:L30-L30] |
| S192 | stub; `get_valid_file_name` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/utilities.py:L11-L11] |
| S193 | catch; `VisualizationComponents.get_raw_data` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/visualization_components.py:L25-L28] |
| S194 | catch; `VisualizationComponents.set_up_db_connection` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/visualization_components.py:L50-L53] |
| S195 | empty-result; `VisualizationComponents.prepare_plot_input_data` | Unknown data label can substitute an empty DataFrame; some nested branches leave df unset. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/visualization_components.py:L109-L130] |
| S196 | units; `VisualizationComponents.get_scaled_df` | Caller-selected column scales are applied without enforcing source or destination units. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/visualization_components.py:L140-L146] |
| S197 | empty-branch; `VisualizationComponents.plot_a_data_set` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/visualization_components.py:L195-L205] |
| S198 | catch; `VisualizationComponents.plot_a_data_set` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/visualization_components.py:L206-L208] |
| S199 | stub; `VisualizationComponents.run_example` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/visualization_components.py:L241-L241] |
| S200 | empty-branch; `Visualization.from_df_array` | Empty-data branch changes processing or drops an empty contribution; retain explicit completeness evidence. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/visualizations.py:L74-L76] |
| S201 | catch; `Visualization.from_df_columns` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/visualizations.py:L143-L150] |
| S202 | catch; `Visualization.save_and_close` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/visualizations.py:L277-L278] |
| S203 | stub; `Visualization.gist1` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/visualizations.py:L284-L284] |
| S204 | catch; `Visualization.add_title_and_axis_labels` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/visualizations.py:L334-L335] |
| S205 | catch; `Visualization.add_title_and_axis_labels` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/visualizations.py:L340-L341] |
| S206 | catch; `Visualization.axis_autoformat` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/visualizations.py:L413-L414] |
| S207 | catch; `Visualization.orcaflex_range_plot` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/visualizations.py:L553-L554] |
| S208 | stub; `Visualization.orcaflex_range_plot` | Placeholder returns implicitly without a computed result; invocation does not establish implementation. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/visualizations.py:L554-L554] |
| S209 | catch; `ymlInput` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/ymlInput.py:L21-L24] |
| S210 | catch; `ymlInput` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/yml_utilities.py:L33-L34] |
| S211 | catch; `ymlInput` | Handler continues, returns a fallback or retries; upstream failure may not stop the caller. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/yml_utilities.py:L44-L47] |
| S212 | empty-result; `yml_read_stream` | Non-mapping YAML documents are ignored and an empty mapping can be returned. | no detection check exists; issue pending (5) | [src/digitalmodel/asset_integrity/common/yml_utilities.py:L66-L78] |
| S213 | catch; `yml_read_stream` | Handler raises an exception; diagnostic identity may change. | no detection check exists; #620 | [src/digitalmodel/asset_integrity/common/yml_utilities.py:L75-L76] |

The text sweep found no `continue`, explicit `return None`, explicit empty-list return, 1970 sentinel or cache-named write; the retained empty-dict return and implicit empty-result stubs remain in the inventory. This is a source-search observation, not proof that every error is detectable.

## 7. Lessons log

| Date | Entry | Lesson / pack change | Evidence | Shared entry |
| --- | --- | --- | --- | --- |
| 2026-10-16 | Initial draft | Separate the legacy grid's closing row from the canonical fracture curve, and distinguish shared-code regression checks from independent qualification | [src/digitalmodel/asset_integrity/common/fad.py:L24-L55] [tests/asset_integrity/test_fad_curves.py:L159-L206] | issue pending (6) |
| 2026-10-16 | Initial draft | Retain the import failure and all silent-error candidates explicitly, with missing detection checks | [src/digitalmodel/asset_integrity/common/data.py:L214-L217] [src/digitalmodel/asset_integrity/common/ymlInput.py:L13-L24] | issue pending (6) |

## 8. Ownership

| Responsibility | Assignment for this draft |
| --- | --- |
| Family quality manager | Engineering calculations quality manager |
| Deputy | Deputy engineering calculations quality manager |
| Producing role | Calculation documentation producer |
| Ambiguous clause, tolerance, unit contract or failed gate | Engineering calculations quality manager; cross-family decisions go to the maintainer |

Assignments describe the proposed review route; no review approval is recorded here.

## 9. Dependencies

### Calculation and support dependencies

`FAD` and the allowable-Kr method call the same canonical `assessment.crack_fad` curve; pipe evaluation delegates to custom sizing, material and capacity classes. [src/digitalmodel/asset_integrity/common/fad.py:L38-L54] [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L75-L93] [src/digitalmodel/asset_integrity/common/pipe_components.py:L44-L73]
`FractureMechanicsComponents` orchestrates FAD, table/histogram loading and CSV/plot output; constructor execution depends on external solution tables. [src/digitalmodel/asset_integrity/common/fracture_mechanics_components.py:L21-L92] [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L27-L40]
NumPy, pandas, SciPy, plotting, workbook and YAML dependencies are direct imports or lazy calls; database drivers and services are optional execution dependencies, not evidence of a successful connection. [src/digitalmodel/asset_integrity/common/math_solvers.py:L5-L15] [src/digitalmodel/asset_integrity/common/math_solvers.py:L66-L90] [src/digitalmodel/asset_integrity/common/database.py:L86-L110] [src/digitalmodel/asset_integrity/common/data.py:L34-L55]

### Every solver-name location (text only)

| Program name | All locations / role | Dependency limit |
| --- | --- | --- |
| SHEAR7 | One sample ASCII-reader path | Example text only; no binary/interface executed [src/digitalmodel/asset_integrity/common/data.py:L187-L206] |
| OrcaFlex | ASCII-reader examples: lines 215-217, 230-231, 233, 239 and 271-275 | Text file samples; no version qualification [src/digitalmodel/asset_integrity/common/data.py:L214-L243] [src/digitalmodel/asset_integrity/common/data.py:L270-L275] |
| OrcaFlex | `SaveData.WriteOrcaFlexYMLFile`, line 614 | Concatenates file text or YAML-serialises objects; requires oyaml, no native load route [src/digitalmodel/asset_integrity/common/data.py:L614-L624] |
| OrcaFlex | `Visualization.orcaflex_range_plot`, line 521 | Plots supplied range arrays; no solver launch [src/digitalmodel/asset_integrity/common/visualizations.py:L521-L559] |

Search used: `rg -n -i 'orcaflex|shear7' src/digitalmodel/asset_integrity/common -g '*.py'`. No solver version or native-load certification is inferred from a text writer.

### Implementation-map pin, family ranking and complete inventory

Map content commit: `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; named data revision: `270c8759c374b67a7f032953e7fcdb14e0224461`. The read-only branch is `codex/implmap-work`; citations to `docs/registry/implementation-map.yaml` resolve at the map content commit.

Only `orcaflex-text-model-writers` has rows inside this package: 3 in-package rows and 440 family implementations in map order; it is therefore the only counted family. [docs/registry/implementation-map.md:L19-L19]
The inventory below lists every implementation by its stable ordered map-row identity, with its complete path/symbol definition in the cited source row; this avoids copying identifying out-of-package discovery filenames into the pack.

| Family implementation ordinal | Location | Kind / relation | Full implementation identity at map pin |
| --- | --- | --- | --- |
| 001 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L4940-L4949] |
| 002 | outside package | script / independent | [docs/registry/implementation-map.yaml:L4950-L4958] |
| 003 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L4959-L4968] |
| 004 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L4969-L4978] |
| 005 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L4979-L4988] |
| 006 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L4989-L4998] |
| 007 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L4999-L5008] |
| 008 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5009-L5020] |
| 009 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5021-L5030] |
| 010 | outside package | writer / copy | [docs/registry/implementation-map.yaml:L5031-L5040] |
| 011 | outside package | writer / copy | [docs/registry/implementation-map.yaml:L5041-L5050] |
| 012 | outside package | writer / copy | [docs/registry/implementation-map.yaml:L5051-L5060] |
| 013 | outside package | writer / example | [docs/registry/implementation-map.yaml:L5061-L5070] |
| 014 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5071-L5080] |
| 015 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5081-L5090] |
| 016 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5091-L5100] |
| 017 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5101-L5110] |
| 018 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5111-L5120] |
| 019 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5121-L5132] |
| 020 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5133-L5142] |
| 021 | outside package | script / independent | [docs/registry/implementation-map.yaml:L5143-L5151] |
| 022 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5152-L5161] |
| 023 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5162-L5171] |
| 024 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5172-L5181] |
| 025 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5182-L5191] |
| 026 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5192-L5201] |
| 027 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5202-L5211] |
| 028 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5212-L5223] |
| 029 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5224-L5233] |
| 030 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5234-L5243] |
| 031 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5244-L5253] |
| 032 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5254-L5263] |
| 033 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5264-L5273] |
| 034 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5274-L5283] |
| 035 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5284-L5293] |
| 036 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5294-L5303] |
| 037 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5304-L5313] |
| 038 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5314-L5323] |
| 039 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5324-L5333] |
| 040 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5334-L5343] |
| 041 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5344-L5353] |
| 042 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5354-L5363] |
| 043 | outside package | script / independent | [docs/registry/implementation-map.yaml:L5364-L5372] |
| 044 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5373-L5382] |
| 045 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5383-L5392] |
| 046 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5393-L5402] |
| 047 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5403-L5415] |
| 048 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5416-L5425] |
| 049 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5426-L5437] |
| 050 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5438-L5447] |
| 051 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5448-L5457] |
| 052 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5458-L5467] |
| 053 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5468-L5477] |
| 054 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5478-L5487] |
| 055 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5488-L5497] |
| 056 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5498-L5507] |
| 057 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5508-L5519] |
| 058 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5520-L5531] |
| 059 | outside package | script / independent | [docs/registry/implementation-map.yaml:L5532-L5540] |
| 060 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5541-L5550] |
| 061 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5551-L5560] |
| 062 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5561-L5570] |
| 063 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5571-L5580] |
| 064 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5581-L5593] |
| 065 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5594-L5603] |
| 066 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5604-L5615] |
| 067 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5616-L5628] |
| 068 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5629-L5638] |
| 069 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5639-L5648] |
| 070 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5649-L5658] |
| 071 | outside package | script / independent | [docs/registry/implementation-map.yaml:L5659-L5667] |
| 072 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5668-L5677] |
| 073 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5678-L5687] |
| 074 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5688-L5697] |
| 075 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5698-L5707] |
| 076 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5708-L5717] |
| 077 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5718-L5727] |
| 078 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5728-L5737] |
| 079 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5738-L5749] |
| 080 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5750-L5759] |
| 081 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5760-L5769] |
| 082 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5770-L5781] |
| 083 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5782-L5791] |
| 084 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5792-L5801] |
| 085 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5802-L5811] |
| 086 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5812-L5823] |
| 087 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5824-L5833] |
| 088 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5834-L5843] |
| 089 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5844-L5853] |
| 090 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5854-L5863] |
| 091 | outside package | script / independent | [docs/registry/implementation-map.yaml:L5864-L5872] |
| 092 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5873-L5882] |
| 093 | outside package | script / independent | [docs/registry/implementation-map.yaml:L5883-L5891] |
| 094 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5892-L5901] |
| 095 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5902-L5911] |
| 096 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5912-L5921] |
| 097 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5922-L5933] |
| 098 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5934-L5943] |
| 099 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5944-L5953] |
| 100 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5954-L5965] |
| 101 | in package | writer / independent | [docs/registry/implementation-map.yaml:L5966-L5975] |
| 102 | in package | writer / independent | [docs/registry/implementation-map.yaml:L5976-L5985] |
| 103 | in package | script / independent | [docs/registry/implementation-map.yaml:L5986-L5994] |
| 104 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L5995-L6004] |
| 105 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6005-L6014] |
| 106 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6015-L6024] |
| 107 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6025-L6034] |
| 108 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6035-L6044] |
| 109 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6045-L6054] |
| 110 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6055-L6064] |
| 111 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6065-L6074] |
| 112 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6075-L6084] |
| 113 | outside package | writer / caller | [docs/registry/implementation-map.yaml:L6085-L6096] |
| 114 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6097-L6109] |
| 115 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6110-L6119] |
| 116 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6120-L6129] |
| 117 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6130-L6141] |
| 118 | outside package | writer / caller | [docs/registry/implementation-map.yaml:L6142-L6153] |
| 119 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6154-L6163] |
| 120 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6164-L6173] |
| 121 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6174-L6183] |
| 122 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6184-L6193] |
| 123 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6194-L6203] |
| 124 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6204-L6213] |
| 125 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6214-L6223] |
| 126 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6224-L6233] |
| 127 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6234-L6243] |
| 128 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6244-L6253] |
| 129 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6254-L6263] |
| 130 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6264-L6273] |
| 131 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6274-L6283] |
| 132 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6284-L6293] |
| 133 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6294-L6303] |
| 134 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6304-L6313] |
| 135 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6314-L6323] |
| 136 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6324-L6333] |
| 137 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6334-L6343] |
| 138 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6344-L6353] |
| 139 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6354-L6363] |
| 140 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6364-L6373] |
| 141 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6374-L6383] |
| 142 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6384-L6393] |
| 143 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6394-L6403] |
| 144 | outside package | writer / copy | [docs/registry/implementation-map.yaml:L6404-L6413] |
| 145 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6414-L6423] |
| 146 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6424-L6436] |
| 147 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6437-L6448] |
| 148 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6449-L6458] |
| 149 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6459-L6468] |
| 150 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6469-L6478] |
| 151 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6479-L6490] |
| 152 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6491-L6500] |
| 153 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6501-L6510] |
| 154 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6511-L6520] |
| 155 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6521-L6530] |
| 156 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6531-L6540] |
| 157 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6541-L6550] |
| 158 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6551-L6560] |
| 159 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6561-L6570] |
| 160 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6571-L6580] |
| 161 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6581-L6590] |
| 162 | outside package | writer / copy | [docs/registry/implementation-map.yaml:L6591-L6600] |
| 163 | outside package | writer / copy | [docs/registry/implementation-map.yaml:L6601-L6610] |
| 164 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6611-L6620] |
| 165 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6621-L6630] |
| 166 | outside package | writer / copy | [docs/registry/implementation-map.yaml:L6631-L6640] |
| 167 | outside package | writer / copy | [docs/registry/implementation-map.yaml:L6641-L6650] |
| 168 | outside package | script / independent | [docs/registry/implementation-map.yaml:L6651-L6659] |
| 169 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6660-L6672] |
| 170 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6673-L6682] |
| 171 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6683-L6692] |
| 172 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6693-L6704] |
| 173 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6705-L6714] |
| 174 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6715-L6724] |
| 175 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6725-L6734] |
| 176 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6735-L6744] |
| 177 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6745-L6754] |
| 178 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6755-L6764] |
| 179 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6765-L6774] |
| 180 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6775-L6784] |
| 181 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6785-L6794] |
| 182 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6795-L6804] |
| 183 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6805-L6814] |
| 184 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6815-L6824] |
| 185 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6825-L6834] |
| 186 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6835-L6844] |
| 187 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6845-L6854] |
| 188 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6855-L6864] |
| 189 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6865-L6874] |
| 190 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6875-L6884] |
| 191 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6885-L6894] |
| 192 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6895-L6904] |
| 193 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6905-L6914] |
| 194 | outside package | writer / wrapper | [docs/registry/implementation-map.yaml:L6915-L6927] |
| 195 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6928-L6937] |
| 196 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6938-L6947] |
| 197 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6948-L6957] |
| 198 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6958-L6967] |
| 199 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6968-L6977] |
| 200 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6978-L6987] |
| 201 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6988-L6997] |
| 202 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L6998-L7007] |
| 203 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7008-L7017] |
| 204 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7018-L7031] |
| 205 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7032-L7041] |
| 206 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7042-L7051] |
| 207 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7052-L7061] |
| 208 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7062-L7071] |
| 209 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7072-L7081] |
| 210 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7082-L7091] |
| 211 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7092-L7101] |
| 212 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7102-L7111] |
| 213 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7112-L7121] |
| 214 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7122-L7131] |
| 215 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7132-L7141] |
| 216 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7142-L7153] |
| 217 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7154-L7163] |
| 218 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7164-L7176] |
| 219 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7177-L7186] |
| 220 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7187-L7196] |
| 221 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7197-L7207] |
| 222 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7208-L7217] |
| 223 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7218-L7227] |
| 224 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7228-L7237] |
| 225 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7238-L7247] |
| 226 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7248-L7257] |
| 227 | outside package | writer / caller | [docs/registry/implementation-map.yaml:L7258-L7269] |
| 228 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7270-L7279] |
| 229 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7280-L7289] |
| 230 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7290-L7299] |
| 231 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7300-L7309] |
| 232 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7310-L7319] |
| 233 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7320-L7329] |
| 234 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7330-L7339] |
| 235 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7340-L7349] |
| 236 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7350-L7359] |
| 237 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7360-L7369] |
| 238 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7370-L7379] |
| 239 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7380-L7389] |
| 240 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7390-L7399] |
| 241 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7400-L7409] |
| 242 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7410-L7419] |
| 243 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7420-L7429] |
| 244 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7430-L7439] |
| 245 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7440-L7449] |
| 246 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7450-L7459] |
| 247 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7460-L7469] |
| 248 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7470-L7479] |
| 249 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7480-L7489] |
| 250 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7490-L7499] |
| 251 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7500-L7509] |
| 252 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7510-L7519] |
| 253 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7520-L7529] |
| 254 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7530-L7539] |
| 255 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7540-L7551] |
| 256 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7552-L7561] |
| 257 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7562-L7571] |
| 258 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7572-L7581] |
| 259 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7582-L7591] |
| 260 | outside package | writer / wrapper | [docs/registry/implementation-map.yaml:L7592-L7603] |
| 261 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7604-L7613] |
| 262 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7614-L7626] |
| 263 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7627-L7636] |
| 264 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7637-L7646] |
| 265 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7647-L7656] |
| 266 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7657-L7666] |
| 267 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7667-L7676] |
| 268 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7677-L7686] |
| 269 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7687-L7696] |
| 270 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7697-L7706] |
| 271 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7707-L7716] |
| 272 | outside package | writer / wrapper | [docs/registry/implementation-map.yaml:L7717-L7729] |
| 273 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7730-L7739] |
| 274 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7740-L7749] |
| 275 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7750-L7762] |
| 276 | outside package | writer / copy | [docs/registry/implementation-map.yaml:L7763-L7774] |
| 277 | outside package | writer / copy | [docs/registry/implementation-map.yaml:L7775-L7784] |
| 278 | outside package | writer / copy | [docs/registry/implementation-map.yaml:L7785-L7794] |
| 279 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7795-L7804] |
| 280 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7805-L7816] |
| 281 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7817-L7826] |
| 282 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7827-L7836] |
| 283 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7837-L7846] |
| 284 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7847-L7856] |
| 285 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7857-L7866] |
| 286 | outside package | writer / caller | [docs/registry/implementation-map.yaml:L7867-L7878] |
| 287 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7879-L7888] |
| 288 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7889-L7898] |
| 289 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7899-L7908] |
| 290 | outside package | writer / wrapper | [docs/registry/implementation-map.yaml:L7909-L7921] |
| 291 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7922-L7931] |
| 292 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7932-L7941] |
| 293 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7942-L7951] |
| 294 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7952-L7961] |
| 295 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7962-L7971] |
| 296 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7972-L7981] |
| 297 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7982-L7991] |
| 298 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L7992-L8001] |
| 299 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8002-L8011] |
| 300 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8012-L8021] |
| 301 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8022-L8031] |
| 302 | outside package | writer / caller | [docs/registry/implementation-map.yaml:L8032-L8043] |
| 303 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8044-L8053] |
| 304 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8054-L8063] |
| 305 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8064-L8073] |
| 306 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8074-L8083] |
| 307 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8084-L8093] |
| 308 | outside package | writer / copy | [docs/registry/implementation-map.yaml:L8094-L8103] |
| 309 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8104-L8113] |
| 310 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8114-L8123] |
| 311 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8124-L8133] |
| 312 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8134-L8143] |
| 313 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8144-L8155] |
| 314 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8156-L8165] |
| 315 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8166-L8175] |
| 316 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8176-L8185] |
| 317 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8186-L8195] |
| 318 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8196-L8205] |
| 319 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8206-L8215] |
| 320 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8216-L8227] |
| 321 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8228-L8237] |
| 322 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8238-L8247] |
| 323 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8248-L8257] |
| 324 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8258-L8267] |
| 325 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8268-L8279] |
| 326 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8280-L8290] |
| 327 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8291-L8300] |
| 328 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8301-L8310] |
| 329 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8311-L8323] |
| 330 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8324-L8333] |
| 331 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8334-L8343] |
| 332 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8344-L8353] |
| 333 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8354-L8363] |
| 334 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8364-L8373] |
| 335 | outside package | writer / copy | [docs/registry/implementation-map.yaml:L8374-L8383] |
| 336 | outside package | writer / copy | [docs/registry/implementation-map.yaml:L8384-L8393] |
| 337 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8394-L8403] |
| 338 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8404-L8413] |
| 339 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8414-L8423] |
| 340 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8424-L8435] |
| 341 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8436-L8445] |
| 342 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8446-L8457] |
| 343 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8458-L8470] |
| 344 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8471-L8480] |
| 345 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8481-L8492] |
| 346 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8493-L8505] |
| 347 | outside package | writer / wrapper | [docs/registry/implementation-map.yaml:L8506-L8517] |
| 348 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8518-L8527] |
| 349 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8528-L8537] |
| 350 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8538-L8547] |
| 351 | outside package | writer / caller | [docs/registry/implementation-map.yaml:L8548-L8560] |
| 352 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8561-L8572] |
| 353 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8573-L8582] |
| 354 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8583-L8596] |
| 355 | outside package | writer / wrapper | [docs/registry/implementation-map.yaml:L8597-L8608] |
| 356 | outside package | writer / wrapper | [docs/registry/implementation-map.yaml:L8609-L8619] |
| 357 | outside package | writer / caller | [docs/registry/implementation-map.yaml:L8620-L8632] |
| 358 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8633-L8647] |
| 359 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8648-L8657] |
| 360 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8658-L8667] |
| 361 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8668-L8677] |
| 362 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8678-L8687] |
| 363 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8688-L8701] |
| 364 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8702-L8714] |
| 365 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8715-L8724] |
| 366 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8725-L8739] |
| 367 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8740-L8749] |
| 368 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8750-L8759] |
| 369 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8760-L8773] |
| 370 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8774-L8783] |
| 371 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8784-L8793] |
| 372 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8794-L8803] |
| 373 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8804-L8813] |
| 374 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8814-L8823] |
| 375 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8824-L8833] |
| 376 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8834-L8843] |
| 377 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8844-L8856] |
| 378 | outside package | writer / independent | [docs/registry/implementation-map.yaml:L8857-L8866] |
| 379 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L8867-L8876] |
| 380 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L8877-L8886] |
| 381 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L8887-L8896] |
| 382 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L8897-L8908] |
| 383 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L8909-L8918] |
| 384 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L8919-L8928] |
| 385 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L8929-L8938] |
| 386 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L8939-L8948] |
| 387 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L8949-L8958] |
| 388 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L8959-L8970] |
| 389 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L8971-L8980] |
| 390 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L8981-L8990] |
| 391 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L8991-L9000] |
| 392 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9001-L9010] |
| 393 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9011-L9020] |
| 394 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9021-L9030] |
| 395 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9031-L9040] |
| 396 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9041-L9050] |
| 397 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9051-L9060] |
| 398 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9061-L9070] |
| 399 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9071-L9080] |
| 400 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9081-L9090] |
| 401 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9091-L9100] |
| 402 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9101-L9110] |
| 403 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9111-L9120] |
| 404 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9121-L9130] |
| 405 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9131-L9140] |
| 406 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9141-L9152] |
| 407 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9153-L9162] |
| 408 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9163-L9172] |
| 409 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9173-L9182] |
| 410 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9183-L9192] |
| 411 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9193-L9202] |
| 412 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9203-L9212] |
| 413 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9213-L9222] |
| 414 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9223-L9232] |
| 415 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9233-L9242] |
| 416 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9243-L9252] |
| 417 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9253-L9262] |
| 418 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9263-L9272] |
| 419 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9273-L9282] |
| 420 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9283-L9292] |
| 421 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9293-L9302] |
| 422 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9303-L9312] |
| 423 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9313-L9322] |
| 424 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9323-L9332] |
| 425 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9333-L9342] |
| 426 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9343-L9352] |
| 427 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9353-L9362] |
| 428 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9363-L9372] |
| 429 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9373-L9382] |
| 430 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9383-L9392] |
| 431 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9393-L9402] |
| 432 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9403-L9412] |
| 433 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9413-L9422] |
| 434 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9423-L9432] |
| 435 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9433-L9442] |
| 436 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9443-L9452] |
| 437 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9453-L9462] |
| 438 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9463-L9472] |
| 439 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9473-L9482] |
| 440 | outside package | writer / test_helper | [docs/registry/implementation-map.yaml:L9483-L9493] |

The local identities are `SaveData.saveDataYaml`, `SaveData.WriteOrcaFlexYMLFile`, and module-level example statements in `data.py`. [src/digitalmodel/asset_integrity/common/data.py:L417-L433] [src/digitalmodel/asset_integrity/common/data.py:L614-L624] [src/digitalmodel/asset_integrity/common/data.py:L910-L941]
The exact case-sensitive Python-re sweep uses each search_signals.pattern from this map revision, omitting a pattern only if it matches the package path below `src/digitalmodel/` with re.I; none is omitted here. Each match is assigned to its innermost function or class, or the module-level definition.

### Counted-family definition classification

| Source definition | Classification / reason | Evidence |
| --- | --- | --- |
| `ConfigureApplicationInputs.generateYMLInput` | Configuration or database orchestration; no additional model construction | [src/digitalmodel/asset_integrity/common/ApplicationManager.py:L203-L247] |
| `ConfigureApplicationInputs.unify_application_and_default_and_custom_yamls` | Configuration or database orchestration; no additional model construction | [src/digitalmodel/asset_integrity/common/ApplicationManager.py:L164-L180] |
| `SaveApplicationResults.saveApplicationResultsToDB` | Configuration or database orchestration; no additional model construction | [src/digitalmodel/asset_integrity/common/ApplicationManager.py:L323-L407] |
| `<module>` | Imports, documentation or serializer declarations; no further model-producing algorithm | [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L1-L1246] |
| `DataFrame_To_doc` | Generic serialization/export of supplied data; helper rather than another model algorithm | [src/digitalmodel/asset_integrity/common/DataFrame_To_doc.py:L14-L34] |
| `DataFrameArray_To_xlsx_openpyxl` | Generic serialization/export of supplied data; helper rather than another model algorithm | [src/digitalmodel/asset_integrity/common/DataFrame_To_xlsx.py:L54-L79] |
| `DataFrame_To_xlsx_openpyxl` | Generic serialization/export of supplied data; helper rather than another model algorithm | [src/digitalmodel/asset_integrity/common/DataFrame_To_xlsx.py:L30-L49] |
| `DataFrame_To_xlsx_xlsxwriter` | Generic serialization/export of supplied data; helper rather than another model algorithm | [src/digitalmodel/asset_integrity/common/DataFrame_To_xlsx.py:L10-L23] |
| `<module>` | Listed implementation; no miss | [src/digitalmodel/asset_integrity/common/data.py:L1-L970] |
| `ReadData.from_ascii_file_get_lines_as_string_arrays` | Reads or resolves input, or probes CPU resources; no model-generation result | [src/digitalmodel/asset_integrity/common/data.py:L270-L305] |
| `ReadData.from_ascii_file_get_structured_data_delimited_white_space` | Reads or resolves input, or probes CPU resources; no model-generation result | [src/digitalmodel/asset_integrity/common/data.py:L228-L268] |
| `ReadData.from_ascii_file_get_value` | Reads or resolves input, or probes CPU resources; no model-generation result | [src/digitalmodel/asset_integrity/common/data.py:L214-L226] |
| `ReadData.read_yml_file` | Reads or resolves input, or probes CPU resources; no model-generation result | [src/digitalmodel/asset_integrity/common/data.py:L139-L144] |
| `SaveData` | Class-level documentation / holder for listed writers; helper | [src/digitalmodel/asset_integrity/common/data.py:L410-L624] |
| `SaveData.DataFrameArray_To_xlsx_openpyxl` | Generic serialization/export of supplied data; helper rather than another model algorithm | [src/digitalmodel/asset_integrity/common/data.py:L496-L520] |
| `SaveData.DataFrame_To_xlsx_openpyxl` | Generic serialization/export of supplied data; helper rather than another model algorithm | [src/digitalmodel/asset_integrity/common/data.py:L472-L494] |
| `SaveData.DataFrame_To_xlsx_xlsxwriter` | Generic serialization/export of supplied data; helper rather than another model algorithm | [src/digitalmodel/asset_integrity/common/data.py:L449-L464] |
| `SaveData.WriteOrcaFlexYMLFile` | Listed implementation; no miss | [src/digitalmodel/asset_integrity/common/data.py:L614-L624] |
| `SaveData.df_to_sheet_in_existing_workbook` | Generic serialization/export of supplied data; helper rather than another model algorithm | [src/digitalmodel/asset_integrity/common/data.py:L522-L533] |
| `SaveData.df_to_table_as_docx` | Generic serialization/export of supplied data; helper rather than another model algorithm | [src/digitalmodel/asset_integrity/common/data.py:L553-L570] |
| `SaveData.saveDataJson` | Generic serialization/export of supplied data; helper rather than another model algorithm | [src/digitalmodel/asset_integrity/common/data.py:L412-L415] |
| `SaveData.saveDataYaml` | Listed implementation; no miss | [src/digitalmodel/asset_integrity/common/data.py:L417-L433] |
| `SaveData.save_ascii_file_from_array` | Generic serialization/export of supplied data; helper rather than another model algorithm | [src/digitalmodel/asset_integrity/common/data.py:L438-L447] |
| `SaveData.write_ascii_file_from_text` | Generic serialization/export of supplied data; helper rather than another model algorithm | [src/digitalmodel/asset_integrity/common/data.py:L609-L612] |
| `Database.executeScriptsFromFile` | Configuration or database orchestration; no additional model construction | [src/digitalmodel/asset_integrity/common/database.py:L596-L617] |
| `Database.get_create_table_sql_code_from_df` | Configuration or database orchestration; no additional model construction | [src/digitalmodel/asset_integrity/common/database.py:L807-L813] |
| `get_db_properties_for_service` | Reads or resolves input, or probes CPU resources; no model-generation result | [src/digitalmodel/asset_integrity/common/database.py:L16-L22] |
| `<module>` | Imports, documentation or serializer declarations; no further model-producing algorithm | [src/digitalmodel/asset_integrity/common/fracture_mechanics_components.py:L1-L426] |
| `ParallelProcessComponents.available_cpu_count` | Reads or resolves input, or probes CPU resources; no model-generation result | [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L51-L167] |
| `<module>` | Imports, documentation or serializer declarations; no further model-producing algorithm | [src/digitalmodel/asset_integrity/common/saveData.py:L1-L89] |
| `dump_ordered` | Generic serialization/export of supplied data; helper rather than another model algorithm | [src/digitalmodel/asset_integrity/common/saveData.py:L48-L56] |
| `dump_ordered_yaml` | Generic serialization/export of supplied data; helper rather than another model algorithm | [src/digitalmodel/asset_integrity/common/saveData.py:L27-L45] |
| `saveDataJson` | Generic serialization/export of supplied data; helper rather than another model algorithm | [src/digitalmodel/asset_integrity/common/saveData.py:L13-L15] |
| `saveDataYaml` | Generic serialization/export of supplied data; helper rather than another model algorithm | [src/digitalmodel/asset_integrity/common/saveData.py:L59-L78] |
| `VisualizationComponents.save_raw_data` | Generic serialization/export of supplied data; helper rather than another model algorithm | [src/digitalmodel/asset_integrity/common/visualization_components.py:L55-L77] |
| `Visualization.orcaflex_range_plot` | Plots supplied range data; does not generate a model | [src/digitalmodel/asset_integrity/common/visualizations.py:L521-L559] |
| `ymlInput` | Reads or resolves input, or probes CPU resources; no model-generation result | [src/digitalmodel/asset_integrity/common/ymlInput.py:L8-L26] |
| `WorkingWithYAML.get_library_yaml_file` | Reads or resolves input, or probes CPU resources; no model-generation result | [src/digitalmodel/asset_integrity/common/yml_utilities.py:L168-L177] |
| `WorkingWithYAML.save_diff_files` | Generic serialization/export of supplied data; helper rather than another model algorithm | [src/digitalmodel/asset_integrity/common/yml_utilities.py:L133-L166] |
| `ymlInput` | Reads or resolves input, or probes CPU resources; no model-generation result | [src/digitalmodel/asset_integrity/common/yml_utilities.py:L25-L49] |
| `yml_read_stream` | Reads or resolves input, or probes CPU resources; no model-generation result | [src/digitalmodel/asset_integrity/common/yml_utilities.py:L66-L78] |

Result: 41 source definitions classified, 0 further independent model-generation implementations; the counted-family miss total is 0. The broader family map includes generic serializers; this classification does not make their output a native-solver model.

### Other implementations and map follow-up

Whole-repository search, including `scripts/`, used `rg -n 'def (fad_curve_option1|LMLLevel2Evaluation|get_plastic_collapse_load_ratio_limit|get_stress_intensity_factor_solution|solve_by_coefficients|max_distance_for_spatial_sets)' . -g '*.py'`; fixture and test matches are not implementation misses.
The canonical FAD curve is outside this package, and a second polynomial helper exists in the pipeline solver support package. These are map follow-up findings, not additions to the counted-family result. [src/digitalmodel/asset_integrity/assessment/crack_fad.py:L1-L25] [src/digitalmodel/infrastructure/base_solvers/pipeline_solvers/math_solvers.py:L1-L30]
Map follow-up: add the section-3 FAD/ratio, fracture/reference-stress, Paris growth, metal-loss, interpolation and polynomial/distance families with in-package rows before counting them; issue pending (6). No A8 claim is made that those engineering families are already mapped.

## 10. Coverage and open items

### Source and documentation coverage

The main FAD, fracture and metal-loss routes were read as source; all 22 Python files were structurally scanned without executing their module-level statements. A complete deeper review of support-file paths is deferred under issue pending (6).

| File group | Coverage / evidence |
| --- | --- |
| `API579_components.py` | Source/AST sweep and dependency scan [src/digitalmodel/asset_integrity/common/API579_components.py:L1-L1061] |
| `ApplicationManager.py` | Source/AST sweep and dependency scan [src/digitalmodel/asset_integrity/common/ApplicationManager.py:L1-L407] |
| `BS7910_critical_flaw_limits.py` | Source/AST sweep and dependency scan [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L1-L1246] |
| `DataFrame_To_Image.py` | Source/AST sweep and dependency scan [src/digitalmodel/asset_integrity/common/DataFrame_To_Image.py:L1-L43] |
| `DataFrame_To_doc.py` | Source/AST sweep and dependency scan [src/digitalmodel/asset_integrity/common/DataFrame_To_doc.py:L1-L43] |
| `DataFrame_To_xlsx.py` | Source/AST sweep and dependency scan [src/digitalmodel/asset_integrity/common/DataFrame_To_xlsx.py:L1-L89] |
| `__init__.py` | Empty package marker (0 source lines) |
| `data.py` | Source/AST sweep and dependency scan [src/digitalmodel/asset_integrity/common/data.py:L1-L970] |
| `database.py` | Source/AST sweep and dependency scan [src/digitalmodel/asset_integrity/common/database.py:L1-L813] |
| `fad.py` | Source/AST sweep and dependency scan [src/digitalmodel/asset_integrity/common/fad.py:L1-L76] |
| `fracture_mechanics_components.py` | Source/AST sweep and dependency scan [src/digitalmodel/asset_integrity/common/fracture_mechanics_components.py:L1-L426] |
| `math_solvers.py` | Source/AST sweep and dependency scan [src/digitalmodel/asset_integrity/common/math_solvers.py:L1-L90] |
| `parallel_process_components.py` | Source/AST sweep and dependency scan [src/digitalmodel/asset_integrity/common/parallel_process_components.py:L1-L172] |
| `pipe_components.py` | Source/AST sweep and dependency scan [src/digitalmodel/asset_integrity/common/pipe_components.py:L1-L73] |
| `saveData.py` | Source/AST sweep and dependency scan [src/digitalmodel/asset_integrity/common/saveData.py:L1-L89] |
| `set_logging.py` | Source/AST sweep and dependency scan [src/digitalmodel/asset_integrity/common/set_logging.py:L1-L38] |
| `update_deep.py` | Source/AST sweep and dependency scan [src/digitalmodel/asset_integrity/common/update_deep.py:L1-L37] |
| `utilities.py` | Source/AST sweep and dependency scan [src/digitalmodel/asset_integrity/common/utilities.py:L1-L115] |
| `visualization_components.py` | Source/AST sweep and dependency scan [src/digitalmodel/asset_integrity/common/visualization_components.py:L1-L242] |
| `visualizations.py` | Source/AST sweep and dependency scan [src/digitalmodel/asset_integrity/common/visualizations.py:L1-L736] |
| `ymlInput.py` | Source/AST sweep and dependency scan [src/digitalmodel/asset_integrity/common/ymlInput.py:L1-L40] |
| `yml_utilities.py` | Source/AST sweep and dependency scan [src/digitalmodel/asset_integrity/common/yml_utilities.py:L1-L188] |

Documentation discovery used `rg -l 'asset_integrity[/\.]common|common/(fad|BS7910|API579)|fracture_mechanics_components|API579_components|BS7910_critical_flaw_limits' docs -g '*.md'` and a package file inventory; no package-local README or docs file existed at the code pin.

| Discovered document | Review status / evidence |
| --- | --- |
| `docs/domains/asset-integrity/ffs-architecture.md` | Canonical versus legacy route read [docs/domains/asset-integrity/ffs-architecture.md:L5-L20] |
| `docs/domains/codes-register.md` | Editions read [docs/domains/codes-register.md:L19-L20] |
| `docs/domains/data/database_manager_migration_guide.md` | Legacy migration reference found; deeper qualification deferred, issue pending (6) [docs/domains/data/database_manager_migration_guide.md:L220-L238] |
| `docs/registry/implementation-map.md` | Family summary read [docs/registry/implementation-map.md:L19-L19] |
| `docs/migration/legacy-batch-workflows.md` | Batch-workflow discovery reference read; deeper migration coverage deferred, issue pending (6) [docs/migration/legacy-batch-workflows.md:L42-L54] |
| `docs/plans/2026-09-24-issue-2157-crack-fe-assessment.md` | Historical legacy-defect and API edition references read; historical status is not a current test result [docs/plans/2026-09-24-issue-2157-crack-fe-assessment.md:L55-L58] |
| `docs/plans/2026-10-06-issue-2287-uniform-local-loss-screening.md` | Legacy-route qualification boundary read [docs/plans/2026-10-06-issue-2287-uniform-local-loss-screening.md:L16-L19] |
| `docs/domains/asset-integrity/ffs-readiness-review-2026-09-25.md` | Historical readiness row read; its test counts are not adopted [docs/domains/asset-integrity/ffs-readiness-review-2026-09-25.md:L35-L36] |

The lifecycle template, AQG r0 family gates and the calculation-output citation contract were readable. The inventory's approximate test count and stable-maturity wording are discovery leads; the executed counts in section 4 and domain registry evidence supersede those leads.

### Tests discovered and attempted

All test paths in this table are under `tests/asset_integrity/`; this table records the ten direct-common worktree files and the extra temporary-copy legacy-engine/arithmetic pair.

| Test file | Scope / result boundary |
| --- | --- |
| `test_fad.py` | FAD material ratios, keys and grid boundary; executed |
| `test_fad_curves.py` | Canonical closed forms and legacy delegation; 5 import-related failures |
| `test_bs7910_legacy_module.py` | Pure legacy helper characterisation; collection blocked |
| `test_math_solvers.py` | Polynomial and geometric helpers; executed |
| `test_update_deep.py` | Configuration merges; executed |
| `test_update_deep_additional.py` | Additional merges; executed |
| `test_yml_utilities_additional.py` | YAML loading and update fallback; collection blocked |
| `test_calculations.py` | Pipe, FAD and utility calculations; executed |
| 1 discovery test file with identifying names | Fixture-only load test; executed; public path expansion needs codenaming, issue pending (6) |
| `test_crack_fad.py` | Canonical assessment / legacy comparison; executed |
| `test_pyintegrity_gml_lml_api579.py` | Legacy workflow; attempted in temporary test copy; collection blocked |
| `test_api579_calculations_standalone.py` | Arithmetic in test code itself; no implementation qualification |

### Open register

| Item | Responsibility | Tracking / evidence |
| --- | --- | --- |
| Python 3.12 import/collection failures and rerun every blocked test | Maintainers | issue pending (1) [src/digitalmodel/asset_integrity/common/data.py:L214-L217] |
| BS edition mismatch; missing clause titles and legacy Citation sidecars | Maintainers | issue pending (2) [docs/domains/codes-register.md:L19-L20] [src/digitalmodel/asset_integrity/common/fad.py:L1-L8] |
| Fracture fallback, interpolation and dimensional guard gaps, including stubs | Maintainers | issue pending (3) [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L264-L288] [src/digitalmodel/asset_integrity/common/BS7910_critical_flaw_limits.py:L466-L470] |
| Metal-loss boundaries, units, empty grids and unqualified engineering reference | Maintainers | issue pending (4); #2133 for spatial sorting [src/digitalmodel/asset_integrity/common/API579_components.py:L1008-L1061] |
| I/O/helper edge cases and missing detection checks from the sweep | Maintainers | issue pending (5); #620 for swallowed exceptions [src/digitalmodel/asset_integrity/common/data.py:L366-L404] [src/digitalmodel/asset_integrity/common/math_solvers.py:L35-L39] |
| Independent fracture/metal-loss examples and full support-path qualification | Maintainers | issue pending (6) [tests/asset_integrity/test_bs7910_legacy_module.py:L1-L12] |
| UNVERIFIED: universal coordinate, origin, force/phase and file-unit contracts | Maintainers | issue pending (6) |
| UNVERIFIED: edition-specific compliance, absent clause titles and Citation sidecars | Maintainers | issue pending (2) |
| Unmapped section-3 method families and identifying discovery paths | Maintainers | issue pending (6) |
| Existing duplicate submodules key in another registry entry; normal parsing retains only the later mapping | Maintainers | issue pending (7) [docs/registry/module-routing.yaml:L303-L320] |
| Replace draft markers with issue numbers and complete independent review | Maintainers | issue pending (6) |

No private data or private sidecar was read or created; no client-derived numerical values are included in this pack.
