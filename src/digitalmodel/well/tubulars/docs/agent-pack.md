# Tubulars knowledge pack

| Header | Value |
|---|---|
| Module | well/tubulars |
| Repository | digitalmodel |
| Package | `src/digitalmodel/well/tubulars` |
| Pack revision | 0 |
| Code revision | `32fe066f387c13a66fd234eca2850a00c16e104a` |
| Analysis family | Engineering calculations (EC); AQG r0 Python calculations |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The public API exposes casing/tubing products and ratings, three stress-envelope models, production casing/tubing checks and sucker-rod Goodman checks. [src/digitalmodel/well/tubulars/__init__.py:L6-L81]

Casing ratings compute internal yield, body yield and four collapse regimes; the envelope layer uses a mean-diameter thin-wall hoop approximation and a VME expression with optional torsion. [src/digitalmodel/well/tubulars/casing.py:L128-L186] [src/digitalmodel/well/tubulars/design_envelope.py:L95-L148]

Production casing returns burst, collapse, tension and triaxial results; tubing returns burst, collapse and tension results, with a packer piston term. [src/digitalmodel/well/tubulars/casing_design.py:L669-L715] [src/digitalmodel/well/tubulars/tubing_design.py:L156-L197]

The parent `well` domain is marked `stable`; this is routing metadata rather than independent engineering qualification of these calculations. [docs/registry/module-routing.yaml:L376-L381]

Scope excludes the adjacent drilling, wellpath and workflow adapters; the production routines perform arithmetic checks rather than a spatial well model, and the connection strength method explicitly describes itself as screening only. [src/digitalmodel/well/tubulars/casing_design.py:L602-L612] [src/digitalmodel/well/tubulars/casing_design.py:L669-L715] [src/digitalmodel/well/tubulars/tubing_design.py:L156-L197]

## 2. Inputs and outputs

Input contracts below describe implemented guards, not sufficient engineering acceptance conditions. [src/digitalmodel/well/tubulars/casing.py:L202-L205] [src/digitalmodel/well/tubulars/casing_design.py:L95-L105]

| Input | Unit and range enforced | Default / route | Evidence |
|---|---|---|---|
| `Casing(od_in, wt_in, grade, weight_ppf)` | inches; OD and wall positive, wall less than half OD; grade strengths in psi | Weight optional; `casing` / `tubing` require exactly one of weight (lb/ft) or explicit wall | [src/digitalmodel/well/tubulars/casing.py:L44-L52] [src/digitalmodel/well/tubulars/casing.py:L192-L205] [src/digitalmodel/well/tubulars/casing.py:L282-L306] [src/digitalmodel/well/tubulars/casing.py:L323-L352] |
| Grade and size catalogs | Grade names and OD/weight keys; unknown keys raise `KeyError` | Material strengths and dimensions are referenced at source, without reproducing standard tables | [src/digitalmodel/well/tubulars/casing.py:L55-L75] [src/digitalmodel/well/tubulars/casing.py:L274-L279] [src/digitalmodel/well/tubulars/casing.py:L314-L320] |
| `TubularGeometry` | OD/ID inches; SMYS and positive collapse rating documented in psi; guards enforce OD > ID and SMYS > 0 | Five required arguments, no rating positivity guard | [src/digitalmodel/well/tubulars/design_envelope.py:L46-L88] |
| Envelope controls | Design factor and hoop/axial yield ratio dimensionless | `design_envelope_points`: factor 1, 100 points, no anisotropic curve unless ratio supplied; positive-factor guards in isotropic/anisotropic classes, absent in API class | [src/digitalmodel/well/tubulars/design_envelope.py:L172-L182] [src/digitalmodel/well/tubulars/design_envelope.py:L270-L276] [src/digitalmodel/well/tubulars/design_envelope.py:L365-L381] [src/digitalmodel/well/tubulars/design_envelope.py:L440-L485] |
| `PressureProfile` | Depth in ft, pressure in psi; equal lengths, at least two anchors, non-decreasing depths | NumPy interpolation; label empty by default | [src/digitalmodel/well/tubulars/casing_design.py:L87-L105] |
| Fluid and axial profiles | Density lb/gal (ppg); depth ft; weight lb/ft; extra tension lbf | Fluid surface pressure and extra tension default 0; axial grid 201 points; conversion and steel-density assumptions at source | [src/digitalmodel/well/tubulars/casing_design.py:L55-L68] [src/digitalmodel/well/tubulars/casing_design.py:L108-L125] [src/digitalmodel/well/tubulars/casing_design.py:L385-L401] |
| `ProductionCasingWell` | Depth, cement top and outer shoe ft; reservoir/frac pressure psi; fluid densities ppg | Required geometry/fluid/reservoir arguments; zero frac pressure; fluid defaults and assumed gas gradient at source | [src/digitalmodel/well/tubulars/casing_design.py:L652-L666] |
| External burst profile | Ordered surface ≤ cement top ≤ outer shoe ≤ total depth, ft | Pore density absent means mix-water gradient continues; mix-water default at source | [src/digitalmodel/well/tubulars/casing_design.py:L163-L197] |
| `TubingWell` / piston | Depth ft, reservoir and annulus pressures psi, packer bore inches, fluid ppg | Zero surface annulus pressure; absent bore uses tubing OD; gas-gradient default imported; piston bore must be positive | [src/digitalmodel/well/tubulars/tubing_design.py:L108-L153] [src/digitalmodel/well/tubulars/tubing_design.py:L171-L197] |
| Design factors | Dimensionless rating/load minima | Configurable defaults, including pressure-dependent burst relaxation; values are at source | [src/digitalmodel/well/tubulars/casing_design.py:L208-L230] |
| Sour screen | H2S ppm, absolute pressure psia, service temperature °F; type gas/oil | Type defaults gas; absent temperature filters zero-sentinel grades for sour conditions | [src/digitalmodel/well/tubulars/casing_design.py:L530-L585] |
| Rod / taper | Diameter inches > 0; loads lbf; tensile strength psi; taper length ft | Service factor 1, enforced in (0, 1]; per-section loads supplied by caller | [src/digitalmodel/well/tubulars/sucker_rod.py:L88-L111] [src/digitalmodel/well/tubulars/sucker_rod.py:L168-L196] |

| Output | Unit / representation | Evidence |
|---|---|---|
| Product ratings and regime | Burst/collapse psi; body yield lbf; collapse regime string | [src/digitalmodel/well/tubulars/casing.py:L219-L241] |
| Envelope arrays | `(n_points, 2)` NumPy arrays: force lbf then differential pressure psi | [src/digitalmodel/well/tubulars/design_envelope.py:L440-L485] |
| `ModeCheckResult` / `as_dict` | Rating and load psi for pressure/triaxial, lbf for tension; factor dimensionless; governing depth ft; Boolean pass | [src/digitalmodel/well/tubulars/casing_design.py:L233-L256] [src/digitalmodel/well/tubulars/casing_design.py:L365-L379] [src/digitalmodel/well/tubulars/casing_design.py:L428-L480] |
| Frac pressure / restrained load adders | Allowable surface pressure psi; ballooning/cooling force lbf; cooling input positive temperature drop °F | [src/digitalmodel/well/tubulars/casing_design.py:L404-L425] [src/digitalmodel/well/tubulars/casing_design.py:L486-L505] |
| Sour assessment | Boolean, partial/total psia, well type and tuple of grade names | [src/digitalmodel/well/tubulars/casing_design.py:L536-L577] |
| Goodman / dynacard bridge | Stress psi, loading dimensionless, Boolean pass; `RodSection` receives diameter, length, count and grade tensile | [src/digitalmodel/well/tubulars/sucker_rod.py:L118-L153] |

Axial force is positive in tension, negative in compression; differential pressure is internal minus external, positive for burst; profiles use true vertical depth measured from the surface anchors, and cooling uses a positive temperature drop. [src/digitalmodel/well/tubulars/design_envelope.py:L20-L24] [src/digitalmodel/well/tubulars/casing_design.py:L89-L105] [src/digitalmodel/well/tubulars/casing_design.py:L108-L117] [src/digitalmodel/well/tubulars/casing_design.py:L416-L425]

The package returns arrays, objects and dictionaries; file serialization belongs to the workflow layer, and no phase or global Cartesian frame is an input to these arithmetic interfaces. [src/digitalmodel/well/tubulars/design_envelope.py:L440-L485] [src/digitalmodel/well/tubulars/casing_design.py:L246-L256] [src/digitalmodel/well/tubulars/tubing_design.py:L156-L197]

## 3. Method and governing standards

All standard references here describe code/document attribution; clause text not checked against the licensed copy. [src/digitalmodel/well/tubulars/casing.py:L24-L35] [src/digitalmodel/well/tubulars/design_envelope.py:L26-L30] [src/digitalmodel/well/tubulars/sucker_rod.py:L20-L28] [src/digitalmodel/well/tubulars/casing_design.py:L28-L30]

| Method / implementation | Attribution and edition evidence | Qualification |
|---|---|---|
| Grade and dimensional lookup: `casing_grade`, `wall_for_size`, `tubing_wall_for_size` | API 5CT; code edition unspecified, register lists 2018 | Clause/table identifier absent; table values omitted here [src/digitalmodel/well/tubulars/casing.py:L44-L122] [docs/domains/codes-register.md:L107-L114] |
| `internal_yield_pressure`, `pipe_body_yield_strength`, `_collapse_constants`, `collapse_pressure` | API 5C3 / API TR 5C3; register lists 2018 | Four collapse branches and computed boundaries; precise standard clauses absent [src/digitalmodel/well/tubulars/casing.py:L128-L186] [docs/domains/codes-register.md:L107-L114] |
| `compute_hoop_stress`, `compute_vme_stress`, isotropic/API/anisotropic `compute` | API TR 5C3 / ISO 10400:2011 named in module; API TR 5C3 Section 7 named in tests | Mean-diameter approximation, quadratic root sweeps and rating corrections; API and ISO edition mapping unresolved [src/digitalmodel/well/tubulars/design_envelope.py:L26-L30] [src/digitalmodel/well/tubulars/design_envelope.py:L95-L148] [src/digitalmodel/well/tubulars/design_envelope.py:L198-L245] [src/digitalmodel/well/tubulars/design_envelope.py:L278-L339] [src/digitalmodel/well/tubulars/design_envelope.py:L391-L433] [tests/well/tubulars/test_design_envelope.py:L413-L419] |
| `reduced_yield_under_tension`, `collapse_pressure_under_tension` | API 5C3 combined loading in docstrings | Recompute collapse at reduced yield; compression does not increase rating and tensile yield exhaustion returns zero [src/digitalmodel/well/tubulars/casing_design.py:L284-L314] |
| Pressure profiles, mode checks, frac limit and restrained load adders | Workshop presentation attributed in code, plus API 5C3 | Operator-practice defaults are configurable; no independent source opened for those defaults [src/digitalmodel/well/tubulars/casing_design.py:L28-L30] [src/digitalmodel/well/tubulars/casing_design.py:L208-L230] [src/digitalmodel/well/tubulars/casing_design.py:L404-L505] |
| `sour_service_screen`, `grade_sour_ok` | NACE MR0175 / ISO 15156; code names Table A.3 for minimum temperatures | Edition/part unspecified; zero sentinel and pressure qualifiers require source qualification [src/digitalmodel/well/tubulars/casing_design.py:L509-L585] |
| Rod catalog and `modified_goodman_allowable` | API 11B, register edition 2018; API 11BR edition/clause absent | API 11BR is the spelling in code; designation not silently normalized [src/digitalmodel/well/tubulars/sucker_rod.py:L20-L82] [docs/domains/codes-register.md:L107-L114] |
| Packer piston `shut_in_piston_force_lbf` | Tubing-packer force analysis attributed in docstring; API 5C3 rating composition | Pressure-area arithmetic, not a coupled displacement solution [src/digitalmodel/well/tubulars/tubing_design.py:L108-L132] [src/digitalmodel/well/tubulars/tubing_design.py:L156-L197] |

UNVERIFIED: U1 — Normative compliance, complete titles, clause/table mapping, edition equivalence and applicability of these standard-derived formulas/constants remain unqualified; the register's dated entries and the code's mostly undated references are not a demonstrated edition match; issue pending (1). [docs/domains/codes-register.md:L107-L114] [src/digitalmodel/well/tubulars/design_envelope.py:L26-L30] [src/digitalmodel/well/tubulars/casing_design.py:L509-L527]

Goodman results carry a reference string; mode results carry neither a reference field nor a structured `Citation` sidecar; source resolution and source revision are not included in either payload; issue pending (1). [src/digitalmodel/well/tubulars/casing_design.py:L233-L256] [src/digitalmodel/well/tubulars/sucker_rod.py:L118-L135]

## 4. Validated worked examples

The offline record in this pack uses the code pin in the header, `PYTHONPATH=src uv run --no-sources python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>`, with the repository addopts unchanged; the module command collected and passed 170 cases at BASE, with zero failures, skips or NOT_RUN cases. [tests/well/tubulars/test_design_envelope.py:L144-L177] [tests/well/tubulars/test_casing_design.py:L58-L73]

| Case / classification | Independent basis, input and expected result | Tolerance and basis | Existing test node / offline record |
|---|---|---|---|
| V1 — validated closed-form limit | Pure axial stress with zero hoop/shear: equivalent stress equals the magnitude of the axial stress, 50,000 psi; inputs inline, no file; this validates a limiting arithmetic case rather than standards compliance | Relative 1e-6, the test's arithmetic tolerance | `tests/well/tubulars/test_design_envelope.py::TestVmeStress::test_pure_axial_gives_sigma_a` — PASS at BASE and AFTER; [tests/well/tubulars/test_design_envelope.py:L144-L150] |
| R1 — regression envelope | Inline geometry fixture and API ellipse at factor 1; expected burst equals the supplied rating, not an independently established capacity | Relative 5.0 %, sampling tolerance encoded in test | `tests/well/tubulars/test_design_envelope.py::TestKnownDesignCases::test_7in_l80_uniaxial_burst_capacity` — PASS at BASE and AFTER; [tests/well/tubulars/test_design_envelope.py:L421-L431] |
| R2 — regression worked burst | Inline product fixture; computed burst and rounded result are asserted; expected values are in the test, with source attribution not independently checked | Raw pressure absolute 1 psi; rounded result equality | `tests/well/tubulars/test_casing_design.py::TestGoldenBarlow::test_barlow_burst_rounds_to_14520_psi` — PASS at BASE and AFTER; [tests/well/tubulars/test_casing_design.py:L49-L67] |
| R3 — regression tubing collapse | Inline product fixture; expected regime, computed pressure and rounded pressure asserted; claimed published comparison is not independently qualified | Absolute 0.1 psi for computed result, exact rounded equality, relative 1.0 % for claimed reference | `tests/well/tubulars/test_tubing_design.py::TestGoldenN80Tubing::test_collapse_near_published_11160_psi` — PASS at BASE and AFTER; [tests/well/tubulars/test_tubing_design.py:L40-L43] [tests/well/tubulars/test_tubing_design.py:L109-L119] |
| R4 — regression Goodman | Inline tensile/minimum stress and service-factor values; expected arithmetic from the same documented formula, not independent fatigue evidence | Exact equality for the two unscaled values; `pytest.approx` for the scaled value | `tests/well/tubulars/test_sucker_rod.py::test_modified_goodman_allowable_formula` — PASS at BASE and AFTER; [tests/well/tubulars/test_sucker_rod.py:L20-L28] |
| R5 — regression published explorer | Frozen JSON worked-burst panel compared to live catalog; no independent source validation | Absolute 0.06 psi for raw pressure, exact rounded equality | `tests/well/tubulars/test_casing_design_explorer.py::test_published_barlow_golden_example` — PASS at BASE and AFTER; [tests/well/tubulars/test_casing_design_explorer.py:L108-L114] |

UNVERIFIED: U2 — The claimed published rating benchmarks and workshop worked-example provenance have not been independently opened and matched to a revision, page and input basis; classification remains regression, except the elementary V1 closed-form limit; issue pending (1). [tests/well/tubulars/test_casing.py:L18-L36] [tests/well/tubulars/test_casing_design.py:L3-L9] [tests/well/tubulars/test_tubing_design.py:L3-L8] [docs/domains/tubulars-casing-rod-validation-2026-06-28.md:L24-L35]

The durable envelope example is `examples/workflows/tubular-design-envelope/input.yml`; its adapter writes force in lbf and pressure in psi to CSV plus a JSON summary, while the sample catalog ratings are supplied inputs rather than qualification data. [examples/workflows/tubular-design-envelope/input.yml:L10-L25] [src/digitalmodel/well/workflow.py:L231-L303]

The module suite also passed 170/170 with the declared test extra enabled, both in the dependency-complete reference run and AFTER; each run had 0 failed, 0 skipped and 0 NOT_RUN cases, using the same source pin and uncommitted documentation tree. The extra-enabled form adds `--extra test` to the command above, with environments and basetemp directories in scratch. [tests/well/tubulars/test_casing.py:L1-L113] [tests/well/tubulars/test_tubing_design.py:L1-L277]

## 5. QA gates

AQG r0, section 10, Python calculations, requires G0 inputs/units/citations, G1 imports/tests, G2 hand or published check and G5 evidence; G3 and G4 are not applicable to this family. (AQG r0, sections 9–10, Python calculations.)

| Gate | Applicability and executable checks | Numeric criterion / unit | Evidence |
|---|---|---|---|
| G0 | Applicable: C01 validates `PressureProfile` malformed inputs; C02 checks `fluid_column_profile` gradient arithmetic | C01: 3 malformed profiles raise `ValueError`, unit cases; C02: bottom pressure equals the test's density/depth conversion within `pytest.approx` relative 1e-6, psi | [tests/well/tubulars/test_casing_design.py:L80-L84] [tests/well/tubulars/test_casing_design.py:L122-L128] |
| G1 | Applicable: C03 imports the public package and runs all six module test files in Python; no external program load route | 170 passed, 0 failed, 0 skipped and 0 NOT_RUN, unit test cases | [tests/well/tubulars/test_casing.py:L7-L15] [tests/well/tubulars/test_casing.py:L18-L113] [tests/well/tubulars/test_tubing_design.py:L49-L277] |
| G2 | Applicable: C04 checks pure-axial `compute_vme_stress` against the closed-form identity independently of the computed output | Expected 50,000 psi; relative error ≤ 1e-6, unit psi | [tests/well/tubulars/test_design_envelope.py:L144-L150] |
| G3 | NOT_APPLICABLE: arithmetic interfaces return curves/check results rather than integrating a dynamics model | No dynamics-duration criterion | [src/digitalmodel/well/tubulars/design_envelope.py:L440-L485] [src/digitalmodel/well/tubulars/tubing_design.py:L156-L197] |
| G4 | NOT_APPLICABLE: no diffraction mesh or hydrodynamic outputs in these interfaces | No panel-quality criterion | [src/digitalmodel/well/tubulars/__init__.py:L83-L152] |
| G5 | Applicable: C05 re-runs the frozen worked-burst panel against the live product; retain input pin, result and expected-value provenance together | Raw pressure error ≤ 0.06 psi and 0 rounded-pressure mismatches, unit psi/cases; this is an evidence drift check, not a signed report appendix | [tests/well/tubulars/test_casing_design_explorer.py:L108-L114] |

For C01–C05, execute the section-4 command against the cited test node, or against `tests/well/tubulars` for C03; preserve the result counts and the code pin with the evidence. [tests/well/tubulars/test_casing_design.py:L80-L128] [tests/well/tubulars/test_design_envelope.py:L144-L150] [tests/well/tubulars/test_casing_design_explorer.py:L108-L114]

| Additional check | Gate | Numeric criterion, quantity and unit | Executable evidence |
|---|---|---|---|
| C06 — exhausted tensile yield | G2 | Both specified at/beyond-yield stresses return 0 psi reduced yield; collapse returns 0 psi and `yield-exhausted` | `tests/well/tubulars/test_casing_design.py::TestBiaxialCollapseDerating::test_yield_exhausted_at_and_beyond_ratio_one` [tests/well/tubulars/test_casing_design.py:L429-L435] |
| C07 — absent pore density | G0 | Three explicit external-profile anchors match cumulative gradients within relative 1e-6, psi; checks explicit pore input only | `tests/well/tubulars/test_casing_design.py::TestProfiles::test_burst_external_three_segments` [tests/well/tubulars/test_casing_design.py:L106-L115] |
| C08 — no positive burst demand | G2 | 1 result has positive infinite factor and true pass flag, dimensionless; does not certify opposite-sign demand | `tests/well/tubulars/test_casing_design.py::TestBurstCheck::test_burst_check_no_load_is_infinite_df` [tests/well/tubulars/test_casing_design.py:L175-L181] |
| C09 — zero piston | G2 | Exactly 0 lbf for 0 psi on both sides | `tests/well/tubulars/test_tubing_design.py::TestPistonForce::test_zero_pressures_zero_force` [tests/well/tubulars/test_tubing_design.py:L165-L166] |
| C10 — no negative piston relief | G2 | Negative piston lbf; resulting tension equals buoyed weight within relative 1e-6, lbf | `tests/well/tubulars/test_tubing_design.py::TestCheckProductionTubing::test_negative_piston_force_not_credited` [tests/well/tubulars/test_tubing_design.py:L246-L259] |
| C11 — finite standard envelope | G1 | 0 nonfinite scalar values for the 100-point isotropic envelope, unit entries; does not exercise invalid inputs | `tests/well/tubulars/test_design_envelope.py::TestIsotropicVmeEnvelope::test_envelope_is_closed_or_bounded` [tests/well/tubulars/test_design_envelope.py:L232-L235] |
| C12 — zero hoop load | G2 | Hoop stress 0 psi within absolute 1 psi | `tests/well/tubulars/test_design_envelope.py::TestHoopStress::test_zero_pressure_gives_zero_hoop` [tests/well/tubulars/test_design_envelope.py:L126-L128] |
| C13 — rod service factor guard | G0 | 2 invalid rod constructions raise `ValueError`, unit cases | `tests/well/tubulars/test_sucker_rod.py::test_service_factor_validation` [tests/well/tubulars/test_sucker_rod.py:L74-L79] |
| C14 — factor relaxation boundary | G0 | 2 specified pressure cases select their documented dimensionless factor by exact equality | `tests/well/tubulars/test_casing_design.py::TestDesignFactors::test_burst_relaxes_below_sicp_threshold` [tests/well/tubulars/test_casing_design.py:L143-L146] |
| C15 — sour-grade filtering | G0 | 3 grade membership assertions at the specified temperatures, unit cases; no licensed threshold validation | `tests/well/tubulars/test_casing_design.py::TestSourService::test_sour_screen_filters_grades_by_temperature` [tests/well/tubulars/test_casing_design.py:L322-L328] |
| C16 — optional collapse derating | G2 | Tension-derated factor ≤ base factor and > 0, dimensionless | `tests/well/tubulars/test_casing_design.py::TestBiaxialCollapseDerating::test_check_collapse_with_tension_derates_df` [tests/well/tubulars/test_casing_design.py:L452-L462] |

These 16 checks comprise G0: 6, G1: 2, G2: 7 and G5: 1; the counts classify the check IDs above, not an independent qualification of the standards. [tests/well/tubulars/test_casing_design.py:L80-L128] [tests/well/tubulars/test_casing_design.py:L429-L462] [tests/well/tubulars/test_design_envelope.py:L144-L150]

## 6. Known failure modes and pitfalls

sweep: 73 found

The count is the number of reviewed assumption/fallback sites below, including grouped declarations at one site; a finding is not automatically a confirmed numerical defect. No swallowed `except`, record-skipping `continue`, `return None`, error-path empty dictionary, external-text numeric parser, epoch default or cache write occurs in the six Python files; integer/float conversions operate on NumPy results. [src/digitalmodel/well/tubulars/casing_design.py:L347-L379] [src/digitalmodel/well/tubulars/tubing_design.py:L181-L197] [src/digitalmodel/well/tubulars/sucker_rod.py:L176-L196]

Exact discovery commands, followed by inspection of every function and default:

```sh
rg -n --glob "*.py" "except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|int\(|float\(|default|ppg|psi|lbf|cache" src/digitalmodel/well/tubulars
rg -n --glob "*.py" "None|0\.0|0\.05|0\.875|65\.4|8\.4|8\.6|1e-12|maximum|max\(|interp|dedup|if .*else" src/digitalmodel/well/tubulars
```

| Site | Symptom / cause | Existing detection and limit | Tracking |
|---|---|---|---|
| S01 | Geometry assumes finite positive diameters and ratings; only OD/ID order and positive SMYS are guarded | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/design_envelope.py:L76-L88] |
| S02 | Mean-diameter thin-wall stress assumes inches and psi and nonzero wall; the helper has no geometry guard | C12 tests zero pressure only | issue pending (2); [src/digitalmodel/well/tubulars/design_envelope.py:L95-L119] |
| S03 | Omitted torsion defaults to zero psi; shear is not inferred from loads | C04 tests the zero-shear limit only | issue pending (2); [src/digitalmodel/well/tubulars/design_envelope.py:L122-L148] |
| S04 | Negative VME radicand is clamped to zero; invalid/nonfinite inputs lack an explicit rejection path | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/design_envelope.py:L146-L148] |
| S05 | Isotropic discriminant is clamped to zero; standard finite-envelope test does not exercise this fallback | C11 covers finite normal inputs only | issue pending (2); [src/digitalmodel/well/tubulars/design_envelope.py:L227-L230] |
| S06 | API envelope design factor is stored without the positive guard used by the other classes | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/design_envelope.py:L270-L276] |
| S07 | API discriminant and negative correction factor are clamped; compression can give a factor above the documented interval | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/design_envelope.py:L291-L294] |
| S08 | Anisotropic negative discriminant is clamped to zero without a diagnostic | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/design_envelope.py:L417-L420] |
| S09 | Convenience defaults choose factor 1 and 100 points; point-count validity is not checked | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/design_envelope.py:L440-L444] |
| S10 | Anisotropic curve is omitted when ratio is absent; a results dictionary begins empty but is filled before return; no error-path empty dictionary | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/design_envelope.py:L466-L483] |
| S11 | Grade strengths assume psi; the frozen grade record has no strength-range guard | no detection check exists | issue pending (1); [src/digitalmodel/well/tubulars/casing.py:L44-L67] |
| S12 | Dimensions assume inches and nominal lb/ft including couplings; no external unit conversion is performed | no detection check exists | issue pending (1); [src/digitalmodel/well/tubulars/casing.py:L79-L122] |
| S13 | Raw rating functions assume valid inches/psi inputs; elastic and empirical constants have no input-range guard | no detection check exists | issue pending (1); [src/digitalmodel/well/tubulars/casing.py:L128-L186] |
| S14 | Product validates finite-looking inequalities only; NaN geometry and invalid grade strengths are not rejected explicitly | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing.py:L192-L205] |
| S15 | Optional weight has no default derived value when explicit wall is supplied; caller must supply a consistent design weight | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing.py:L282-L306] |
| S16 | Tubing explicit-wall route also leaves nominal weight absent; no consistency check against design weight | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing.py:L323-L352] |
| S17 | Fluid-gradient conversion assumes ppg and ft and returns psi | C02 checks the implemented conversion, not its source | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L55-L56] |
| S18 | Buoyancy assumes a fixed steel density in ppg equivalent | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L58-L59] |
| S19 | Absent gas PVT selects a fixed dry-gas pressure gradient | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L61-L63] |
| S20 | Cooling restraint uses a fixed steel elastic modulus in psi | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L65-L66] |
| S21 | Ballooning restraint uses a fixed dimensionless Poisson ratio | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L67-L67] |
| S22 | Cooling restraint uses a fixed expansion coefficient per degree Fahrenheit | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L68-L68] |
| S23 | Rating rounding uses Python nearest-even ties; claimed API convention is not independently qualified | no detection check exists | issue pending (1); [src/digitalmodel/well/tubulars/casing_design.py:L74-L81] |
| S24 | Duplicate profile depths are accepted; interpolation clamps queries outside anchor range without an error | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L95-L105] |
| S25 | Fluid-column surface pressure defaults to zero psi; negative density or zero total depth is not explicitly rejected | C02 covers one positive density/depth | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L108-L117] |
| S26 | SITP subtracts a default gas gradient; negative surface pressure is returned without a warning | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L120-L125] |
| S27 | Mix-water density is assumed by default; absent pore density continues it below the shoe | C07 tests explicit pore density only | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L168-L184] |
| S28 | Duplicate-depth anchors are discarded by a filtered comprehension without recording the dropped anchors | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L188-L193] |
| S29 | Degenerate all-zero depth expands to an artificial 1 ft anchor instead of rejecting the well | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L194-L195] |
| S30 | Full evacuation explicitly assumes zero internal pressure over the full depth | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L200-L202] |
| S31 | Burst minimum factor defaults to the attributed practice value | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L218-L218] |
| S32 | Low-SICP burst factor has a separate default | C14 tests selection only | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L219-L219] |
| S33 | Pressure threshold changes burst default abruptly | C14 tests both sides by exact equality | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L220-L220] |
| S34 | Collapse factor defaults without a positivity/range guard | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L221-L221] |
| S35 | Tension factor defaults without a positivity/range guard | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L222-L222] |
| S36 | Compression factor is stored but not consumed by the orchestration | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L223-L223] |
| S37 | Triaxial factor defaults without a positivity/range guard | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L224-L224] |
| S38 | Depth grid uses 201 regular samples plus anchors; out-of-range anchors are filtered out without a diagnostic | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L259-L265] |
| S39 | Compression returns original yield; tensile stress at/beyond yield returns zero psi | C06 tests yield exhaustion; compression identity test also exists | #1311; [src/digitalmodel/well/tubulars/casing_design.py:L284-L297] |
| S40 | Exhausted yield returns zero collapse and a regime label rather than raising | C06 checks zero pressure and label | #1311; [src/digitalmodel/well/tubulars/casing_design.py:L311-L314] |
| S41 | Absent internal profile selects evacuation; absent weight bypasses collapse derating | C16 exercises optional derating | #1311; [src/digitalmodel/well/tubulars/casing_design.py:L327-L341] |
| S42 | Absent buoyancy density uses unit buoyancy factor even if an external fluid profile was supplied | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L343-L344] |
| S43 | Nonpositive collapse demand gets infinite factor; division denominator is floored at a small positive pressure | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L347-L355] |
| S44 | No positive burst/collapse demand reports infinite factor and can pass | C08 covers burst no-positive-demand semantics | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L368-L379] |
| S45 | Buoyed axial load assumes a uniform string, steel density and positive-tension sign; extra tension defaults zero lbf | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L385-L401] |
| S46 | Ballooning assumes fixed ends; cooling assumes full restraint and a positive temperature drop in Fahrenheit | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L404-L425] |
| S47 | Tension extra load defaults zero; nonpositive maximum returns infinite factor and pass | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L428-L443] |
| S48 | Triaxial extra load defaults zero; hoop uses thin wall and no radial term; zero VME returns infinite factor | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L446-L480] |
| S49 | Absent explicit frac factor uses burst default; nonpositive factor and negative allowable pressure are not rejected | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L500-L505] |
| S50 | Temperature-table zero sentinel means all temperatures in the comment, but direct comparison can reject subzero temperatures | no detection check exists | issue pending (1); [src/digitalmodel/well/tubulars/casing_design.py:L511-L522] |
| S51 | H2S threshold is fixed and compared strictly rather than inclusively | no detection check exists | issue pending (1); [src/digitalmodel/well/tubulars/casing_design.py:L525-L525] |
| S52 | Gas total-pressure qualifier is fixed and compared strictly | no detection check exists | issue pending (1); [src/digitalmodel/well/tubulars/casing_design.py:L526-L526] |
| S53 | Oil total-pressure qualifier is fixed and compared strictly | no detection check exists | issue pending (1); [src/digitalmodel/well/tubulars/casing_design.py:L527-L527] |
| S54 | Partial pressure assumes ppm and absolute psia; negative or nonfinite concentrations lack a guard | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L530-L533] |
| S55 | Assessment defaults to an empty grade tuple; screen defaults to gas and absent temperature, without an input completeness record | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L536-L550] |
| S56 | Non-sour condition returns all grades; missing temperature in sour conditions keeps only zero-sentinel entries | C15 tests supplied temperatures only | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L567-L577] |
| S57 | Direct grade screen treats the zero sentinel as a numerical minimum rather than an all-temperature flag | no detection check exists | issue pending (1); [src/digitalmodel/well/tubulars/casing_design.py:L580-L585] |
| S58 | Connection screening assumes tabulated efficiency times body yield; absent thread count for premium classes is data, not a solved connection model | no detection check exists | issue pending (1); [src/digitalmodel/well/tubulars/casing_design.py:L602-L638] |
| S59 | Well defaults: no frac pressure, preset frac/mix-water densities, absent pore density and default dry-gas gradient | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L662-L666] |
| S60 | Frac pressure ≤ zero silently omits the frac burst load case | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/casing_design.py:L695-L700] |
| S61 | Burst selects the lowest factor but triaxial always uses the leak profile, including when frac governs burst | no detection check exists | issue pending (3); [src/digitalmodel/well/tubulars/casing_design.py:L700-L713] |
| S62 | Production casing omits optional collapse-weight derating and does not apply cooling/ballooning adders automatically | no detection check exists | issue pending (3); [src/digitalmodel/well/tubulars/casing_design.py:L704-L713] |
| S63 | Shut-in gas profile uses default gradient, assumes ft/psi and requires positive depth | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/tubing_design.py:L68-L85] |
| S64 | Packer-fluid annulus defaults surface pressure to zero psi | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/tubing_design.py:L89-L102] |
| S65 | Piston assumes bore/diameters in inches and pressures in psi; annulus pressure defaults zero | C09 checks zero-pressure identity only | issue pending (2); [src/digitalmodel/well/tubulars/tubing_design.py:L108-L132] |
| S66 | TubingWell defaults zero surface casing pressure, absent packer bore and fixed gas gradient | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/tubing_design.py:L147-L153] |
| S67 | Absent bore uses tubing OD; negative piston force is clipped to zero additional tension | C10 checks clipping; explicit-bore test checks changed load | issue pending (2); [src/digitalmodel/well/tubulars/tubing_design.py:L181-L197] |
| S68 | Rod strengths assume psi and diameter inches; raw rod-area helper squares a negative diameter without rejecting it | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/sucker_rod.py:L34-L69] |
| S69 | Raw Goodman helper assumes psi and defaults service factor to 1 without validating the factor | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/sucker_rod.py:L75-L82] |
| S70 | Rod product defaults factor 1; service-factor interval is checked but nonfinite diameter can evade its positivity guard | C13 covers two ordinary invalid constructions only | issue pending (2); [src/digitalmodel/well/tubulars/sucker_rod.py:L92-L102] |
| S71 | Nonpositive allowable gives infinite loading; the pass flag separately compares peak and allowable, with no cycle-order guard | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/sucker_rod.py:L118-L135] |
| S72 | Dynacard conversion defaults count to 1 and passes inch/ft/psi values without conversion | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/sucker_rod.py:L137-L153] |
| S73 | Empty taper input returns an empty list with no completeness warning; length is metadata, not a load calculation | no detection check exists | issue pending (2); [src/digitalmodel/well/tubulars/sucker_rod.py:L176-L196] |

Historical scope references #1090 and #1091 are development records, not confirmed incidents; the corresponding validation note identifies their catalog coverage. [docs/domains/tubulars-casing-rod-validation-2026-06-28.md:L3-L5]

The combined-loading extension recorded in #1311 is present as an optional check, with exhaustion and monotonicity tests; the production orchestrator still calls collapse without the optional weight. [tests/well/tubulars/test_casing_design.py:L410-L469] [src/digitalmodel/well/tubulars/casing_design.py:L704-L709]

The old validation note reports historical test counts that differ from the current 170-case offline collection; it is not used as a current count; issue pending (5). [docs/domains/tubulars-casing-rod-validation-2026-06-28.md:L55-L60]

## 7. Lessons log

| Date | Change | Lesson / evidence | Shared entry |
|---|---|---|---|
| 2026-10-16 | Initial pack | Keep published-rating regression separate from the closed-form limit; retain explicit fallback and load-case identity findings. [tests/well/tubulars/test_casing.py:L18-L36] [tests/well/tubulars/test_design_envelope.py:L144-L150] [src/digitalmodel/well/tubulars/casing_design.py:L700-L713] | Entry pending; issue pending (6) |

## 8. Ownership

Proposed review roles: engineering calculations quality manager and deputy engineering calculations quality manager; proposed producing function: engineering calculations maintainers; unresolved engineering criteria go to the family quality manager, cross-family issues to the maintainers. (AQG r0, section 11, Sign-off and independence.)

UNVERIFIED: U3 — Named responsibility, deputy assignment and shared evidence/lessons registration are not established by package code; maintainer assignment and review clearance remain open; issue pending (6).

## 9. Dependencies and parallel implementations

NumPy supplies interpolation, grids and array arithmetic; `math` and `dataclasses` supply scalar arithmetic and records; the rod bridge imports `RodSection` only inside the conversion method. [src/digitalmodel/well/tubulars/casing_design.py:L38-L47] [src/digitalmodel/well/tubulars/casing_design.py:L103-L105] [src/digitalmodel/well/tubulars/design_envelope.py:L198-L245] [src/digitalmodel/well/tubulars/sucker_rod.py:L137-L153]

The `well` workflow adapter is a downstream caller/exporter, and the explorer generator imports the product/check layer; their generated files are not independent capacity methods. [src/digitalmodel/well/workflow.py:L231-L303] [scripts/capabilities/build_casing_design_explorer.py:L38-L56] [tests/well/tubulars/test_casing_design_explorer.py:L3-L8]

### Map inventory and counted family

Map at the code pin was read at `32fe066f387c13a66fd234eca2850a00c16e104a`; its data revision is `270c8759c374b67a7f032953e7fcdb14e0224461`, and its map bytes equal the map at this pack's code pin. [docs/registry/implementation-map.yaml:L1-L3]

The sole family with in-package implementation rows is `api-5ct-5l-grade-tables`: 1 in-package row and 39 total inventory rows; it is therefore the only counted family, with no ranking tie. [docs/registry/implementation-map.md:L26-L26] [docs/registry/implementation-map.yaml:L24471-L24479]

Every mapped implementation of that family, including external callers and examples, is inventoried below; a mapped row is an inventory classification, not evidence that two algorithms have identical engineering scope. [docs/registry/implementation-map.md:L47-L47]

| Map implementation path | Symbol (module means module-level definition) | Map evidence |
|---|---|---|
| `docs/domains/orcaflex/templates/components/materials/steel.csv` | `module` | [docs/registry/implementation-map.yaml:L24106-L24114] |
| `examples/demos/asset_integrity/ffs_lookup_build.py` | `module` | [docs/registry/implementation-map.yaml:L24115-L24124] |
| `examples/demos/asset_integrity/ffs_lookup_build.py` | `build_records` | [docs/registry/implementation-map.yaml:L24125-L24134] |
| `examples/demos/gtm/calc_values.py` | `module` | [docs/registry/implementation-map.yaml:L24135-L24144] |
| `examples/demos/gtm/demo_02_wall_thickness_multicode.py` | `_load_engineering_modules` | [docs/registry/implementation-map.yaml:L24145-L24154] |
| `examples/domains/structural/wall_thickness_example.py` | `run_single_design_check` | [docs/registry/implementation-map.yaml:L24155-L24164] |
| `examples/domains/structural/wall_thickness_example.py` | `run_phase_analysis` | [docs/registry/implementation-map.yaml:L24165-L24174] |
| `scripts/capabilities/build_casing_design_explorer.py` | `module` | [docs/registry/implementation-map.yaml:L24175-L24183] |
| `src/digitalmodel/base_configs/modules/pipe_capacity/pipe_capacity.yml` | `module` | [docs/registry/implementation-map.yaml:L24184-L24192] |
| `src/digitalmodel/base_configs/modules/vertical_riser/vertical_riser.yml` | `module` | [docs/registry/implementation-map.yaml:L24193-L24201] |
| `src/digitalmodel/data_systems/data/steel_material.yml` | `module` | [docs/registry/implementation-map.yaml:L24202-L24210] |
| `src/digitalmodel/data_systems/data_procurement/riser/database_clients/pipe_db_client.py` | `PipeSpecificationClient` | [docs/registry/implementation-map.yaml:L24211-L24220] |
| `src/digitalmodel/data_systems/data_procurement/riser/database_clients/pipe_db_client.py` | `PipeSpecificationClient._initialize_pipe_database` | [docs/registry/implementation-map.yaml:L24221-L24232] |
| `src/digitalmodel/data_systems/data_procurement/riser/database_clients/pipe_db_client.py` | `PipeSpecificationClient._generate_pipe_spec` | [docs/registry/implementation-map.yaml:L24233-L24242] |
| `src/digitalmodel/infrastructure/templates/api_std_2rd_template.yml` | `module` | [docs/registry/implementation-map.yaml:L24243-L24251] |
| `src/digitalmodel/materials/grades.py` | `module` | [docs/registry/implementation-map.yaml:L24252-L24261] |
| `src/digitalmodel/orcaflex/mooring_design.py` | `module` | [docs/registry/implementation-map.yaml:L24262-L24271] |
| `src/digitalmodel/orcaflex/mooring_design.py` | `MooringLineSegment` | [docs/registry/implementation-map.yaml:L24272-L24281] |
| `src/digitalmodel/orcaflex/mooring_design.py` | `MooringLineDesign.estimate_catenary` | [docs/registry/implementation-map.yaml:L24282-L24291] |
| `src/digitalmodel/orcaflex/mooring_design.py` | `MooringLineDesign.check_mbl` | [docs/registry/implementation-map.yaml:L24292-L24301] |
| `src/digitalmodel/orcaflex/mooring_design.py` | `MooringLineDesign.check_mbl_with_safety_factor` | [docs/registry/implementation-map.yaml:L24302-L24311] |
| `src/digitalmodel/orcaflex/riser_config.py` | `module` | [docs/registry/implementation-map.yaml:L24312-L24321] |
| `src/digitalmodel/orcaflex/riser_config.py` | `RiserPipeProperties.steel_mass_per_m` | [docs/registry/implementation-map.yaml:L24322-L24331] |
| `src/digitalmodel/orcaflex/riser_config.py` | `RiserPipeProperties.bending_stiffness` | [docs/registry/implementation-map.yaml:L24332-L24341] |
| `src/digitalmodel/orcaflex/riser_config.py` | `RiserPipeProperties.axial_stiffness` | [docs/registry/implementation-map.yaml:L24342-L24351] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py` | `module` | [docs/registry/implementation-map.yaml:L24352-L24361] |
| `src/digitalmodel/structural/analysis/__init__.py` | `module` | [docs/registry/implementation-map.yaml:L24362-L24370] |
| `src/digitalmodel/structural/analysis/cli.py` | `check` | [docs/registry/implementation-map.yaml:L24371-L24380] |
| `src/digitalmodel/structural/analysis/cli.py` | `sweep` | [docs/registry/implementation-map.yaml:L24381-L24390] |
| `src/digitalmodel/structural/analysis/cli.py` | `phases` | [docs/registry/implementation-map.yaml:L24391-L24400] |
| `src/digitalmodel/structural/analysis/cli.py` | `list_grades` | [docs/registry/implementation-map.yaml:L24401-L24410] |
| `src/digitalmodel/structural/analysis/cli.py` | `lookup` | [docs/registry/implementation-map.yaml:L24411-L24420] |
| `src/digitalmodel/structural/analysis/wall_thickness_parametric.py` | `module` | [docs/registry/implementation-map.yaml:L24421-L24430] |
| `src/digitalmodel/structural/analysis/wall_thickness_parametric.py` | `ParametricSweep.run` | [docs/registry/implementation-map.yaml:L24431-L24440] |
| `src/digitalmodel/subsea/catenary_riser/models.py` | `module` | [docs/registry/implementation-map.yaml:L24441-L24450] |
| `src/digitalmodel/subsea/pipeline/pipeline_pressure.py` | `module` | [docs/registry/implementation-map.yaml:L24451-L24460] |
| `src/digitalmodel/subsea/pipeline/pipeline_pressure_workflow.py` | `WallThicknessSizingWorkflow.run` | [docs/registry/implementation-map.yaml:L24461-L24470] |
| `src/digitalmodel/well/tubulars/casing_design.py` | `module` | [docs/registry/implementation-map.yaml:L24471-L24479] |
| `tests/fixtures/test_vectors/mooring_risers/riser_material_properties.yaml` | `module` | [docs/registry/implementation-map.yaml:L24480-L24491] |

Counted-family search used each `search_signals[].pattern` exactly as stored, Python `re`, case-sensitive, over all package `.py` files at the code pin; no signal matched the case-insensitive package-relative string `well/tubulars`, so none was omitted. [docs/registry/implementation-map.yaml:L24101-L24107]

```python
patterns = [r"(?:API.?5[CLT]|_api5l|API5L|API5CT|MATERIAL_LIBRARY|STEEL_GRADES|GRADE_REGISTRY|_GRADES|steel_material|smys|SMYS)",
            r"API.?5CT|API.?5L|steel.grade|casing.grade",
            r"\b(?:J55|K55|N80|L80|P110|Q125|X42|X52|X60|X65|X70|X80)\b"]
# For each matching source line, select the smallest enclosing AST FunctionDef,
# AsyncFunctionDef or ClassDef; all remaining lines share one module definition.
```

The audit assigns hits to 35 enclosing definitions; the following classification accounts for every hit, including docstrings and data holders. [src/digitalmodel/well/tubulars/casing.py:L44-L352] [src/digitalmodel/well/tubulars/casing_design.py:L284-L362] [src/digitalmodel/well/tubulars/design_envelope.py:L46-L433] [src/digitalmodel/well/tubulars/sucker_rod.py:L44-L57]

| File / hit definitions | Classification and reason |
|---|---|
| `__init__.py`: module | Export helper, 1 definition; no grade formula/table owned here. [src/digitalmodel/well/tubulars/__init__.py:L6-L152] |
| `casing.py`: module; `CasingGrade`; `casing_grade`; `Casing`; `casing`; `tubing` | 6 definitions: grade/dimension tables and lookup/data/product holders already listed in section 3. [src/digitalmodel/well/tubulars/casing.py:L44-L122] [src/digitalmodel/well/tubulars/casing.py:L192-L205] [src/digitalmodel/well/tubulars/casing.py:L282-L352] |
| `casing.py`: `internal_yield_pressure`; `pipe_body_yield_strength`; `_collapse_constants`; `collapse_pressure` | 4 definitions compute pressure/strength, not grade selection; listed in section 3. [src/digitalmodel/well/tubulars/casing.py:L128-L186] |
| `casing.py`: `burst_psi`; `body_yield_lbf`; `collapse_psi`; `collapse_regime`; `reference`; `to_tubular_geometry` | 6 delegates/formatters, not further independent grade definitions. [src/digitalmodel/well/tubulars/casing.py:L219-L263] |
| `casing_design.py`: module | 1 mapped module definition, including grade-selection temperature data. [docs/registry/implementation-map.yaml:L24471-L24479] [src/digitalmodel/well/tubulars/casing_design.py:L509-L527] |
| `casing_design.py`: `reduced_yield_under_tension`; `collapse_pressure_under_tension`; `check_collapse`; `ConnectionClass.joint_strength_lbf` | 4 strength/pressure checks with standard vocabulary hits, not further grade tables; all method classes are in section 3. [src/digitalmodel/well/tubulars/casing_design.py:L284-L362] [src/digitalmodel/well/tubulars/casing_design.py:L602-L612] |
| `design_envelope.py`: module; `TubularGeometry`; `TubularGeometry.__post_init__`; `IsotropicVmeEnvelope`; its `compute`; `ApiEllipseEnvelope`; its `_biaxial_factor` and `compute`; `AnisotropicVmeEnvelope`; its `compute` | 10 definitions: docstrings, input holder and stress algorithms; consume supplied SMYS and ratings rather than independently defining grade data. [src/digitalmodel/well/tubulars/design_envelope.py:L4-L30] [src/digitalmodel/well/tubulars/design_envelope.py:L46-L88] [src/digitalmodel/well/tubulars/design_envelope.py:L155-L433] |
| `sucker_rod.py`: module; `rod_grade` | 2 definitions: separate API 11B data and accessor, not API 5CT/5L grade data; `_GRADES` triggers the signal. [src/digitalmodel/well/tubulars/sucker_rod.py:L44-L57] |
| `tubing_design.py`: module | 1 definition: references/imports/docstring, no local grade table. [src/digitalmodel/well/tubulars/tubing_design.py:L28-L61] |

A8 self-check: inventory 39/39; counted families 1; hit definitions 35; further second implementations absent from both sections 3 and 9: 0; PASS against the maximum of 1, subject to independent classification review. [docs/registry/implementation-map.md:L26-L26] [src/digitalmodel/well/tubulars/casing.py:L44-L122] [src/digitalmodel/well/tubulars/casing_design.py:L509-L527]

### Repository-wide follow-up

Whole-repository method discovery, including `scripts/`, uses the following command; these additional families do not change the counted-family result:

```sh
rg -n --glob '*.py' 'compute_hoop_stress|compute_vme_stress|collapse_pressure|modified_goodman|TubularGeometry|ApiEllipse|IsotropicVme|internal_yield_pressure|NACE_MIN_SERVICE' .
```

The legacy `API_TR_5C3.get_collapse_pressure` in the integrity package computes elastic collapse only; the related structural implementation also has a separate collapse route, while `riser_joint_ffs.collapse_pressure_psi` is explicitly API RP 1111 rather than four-regime API 5C3. [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L630-L654] [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1221-L1235] [src/digitalmodel/asset_integrity/riser_joint_ffs.py:L232-L248]

Map follow-up: add the `casing.py` grade/dimension data already listed in section 3, and classify the rating/collapse, VME/biaxial/anisotropic, pressure/axial/piston, sour-service, connection and Goodman method families before their first in-package mapped revision; issue pending (4). [src/digitalmodel/well/tubulars/casing.py:L44-L186] [src/digitalmodel/well/tubulars/design_envelope.py:L155-L433] [src/digitalmodel/well/tubulars/casing_design.py:L268-L585] [src/digitalmodel/well/tubulars/casing_design.py:L591-L646] [src/digitalmodel/well/tubulars/sucker_rod.py:L75-L135] [src/digitalmodel/well/tubulars/tubing_design.py:L108-L132]

## 10. Coverage and open items

Source read: all six package Python files (`__init__.py`, `casing.py`, `casing_design.py`, `design_envelope.py`, `sucker_rod.py`, `tubing_design.py`); their exposed API and method bodies are the scope of this pack. [src/digitalmodel/well/tubulars/__init__.py:L6-L152]

Test files read and run: `tests/well/tubulars/test_casing.py`, `tests/well/tubulars/test_casing_design.py`, `tests/well/tubulars/test_casing_design_explorer.py`, `tests/well/tubulars/test_design_envelope.py`, `tests/well/tubulars/test_sucker_rod.py`, `tests/well/tubulars/test_tubing_design.py`; the test package initializer is empty. [tests/well/tubulars/test_casing.py:L1-L113] [tests/well/tubulars/test_casing_design.py:L1-L469] [tests/well/tubulars/test_casing_design_explorer.py:L1-L121] [tests/well/tubulars/test_design_envelope.py:L1-L518] [tests/well/tubulars/test_sucker_rod.py:L1-L102] [tests/well/tubulars/test_tubing_design.py:L1-L277]

Documentation discovery searched module names, import paths, casing-design and the five named standards under the package and `docs/`; broad substring matches about structural tubulars are separate methods rather than evidence for this package. [docs/maps/digitalmodel-operator-map.md:L72-L72] [docs/domains/asset-integrity/ffs-offering-catalog.md:L104-L108]

| Documents inspected / context | Evidence |
|---|---|
| `docs/domains/tubulars-casing-rod-validation-2026-06-28.md`; `docs/domains/codes-register.md` | Catalog validation claims and dated standards register. [docs/domains/tubulars-casing-rod-validation-2026-06-28.md:L1-L68] [docs/domains/codes-register.md:L107-L114] |
| `docs/registry/implementation-map.md`, its YAML map, `docs/registry/module-routing.yaml`, `docs/maps/digitalmodel-operator-map.md` | Map inventory and parent routing. [docs/registry/implementation-map.md:L26-L26] [docs/registry/module-routing.yaml:L376-L381] [docs/maps/digitalmodel-operator-map.md:L72-L72] |
| `docs/registry/workflows.yaml`; the two well workflow input files | Registered casing and envelope adapters. [docs/registry/workflows.yaml:L943-L999] [examples/workflows/tubular-design-envelope/input.yml:L10-L25] [examples/workflows/casing-design/input.yml:L1-L10] |
| `docs/research/ffs-worn-corroded-casing-collapse-burst-screen.md`; `docs/domains/asset-integrity/ffs-offering-catalog.md` | Worn-casing research/planned scope, not qualification of this module. [docs/research/ffs-worn-corroded-casing-collapse-burst-screen.md:L9-L29] [docs/domains/asset-integrity/ffs-offering-catalog.md:L104-L108] |
| `docs/vision/CALCULATIONS-VISION.md`; `docs/capability-map/capabilities-ia-spec-1444.md` | Broader capability/integration leads, not current module validation counts. [docs/vision/CALCULATIONS-VISION.md:L121-L125] [docs/capability-map/capabilities-ia-spec-1444.md:L41-L46] |
| `docs/plans/2026-07-29-issue-1898-equipment-catalog-reference-data.md`; `docs/domains/materials/line-pipe-validation-2026-06-28.md` | Equipment/catalog and material-product context. [docs/plans/2026-07-29-issue-1898-equipment-catalog-reference-data.md:L16-L20] [docs/domains/materials/line-pipe-validation-2026-06-28.md:L59-L62] |
| `docs/domains/_index.md`; `docs/domains/domains_index.md`; `docs/domains/artificial_lift/rod-pump-rp11l.md` | Navigation/adjacent pump method; no additional tubular capacity qualification inferred. [docs/domains/_index.md:L1-L12] [docs/domains/domains_index.md:L11-L17] [docs/domains/artificial_lift/rod-pump-rp11l.md:L161-L166] |
| 2 discovery files with identifying names | Paths withheld from this public index; codenaming follow-up, issue pending (5) |

| Item / responsibility | Evidence or limitation | Tracking |
|---|---|---|
| U1 / engineering calculations maintainers | Standard titles, editions, clauses, applicability, licensed-source checks and structured citations remain unqualified; clause text not checked against the licensed copy | issue pending (1) |
| U2 / engineering calculations maintainers | Independent rating/workshop sources not matched; V1 is only a closed-form limiting check | issue pending (1) |
| U3 / maintainers | Assign review deputy, retained evidence and shared-log identity; independent review not yet recorded | issue pending (6) |
| Input/default coverage / engineering calculations maintainers | Section-6 fallback sites lack complete boundary tests; normal-case passes do not close these gaps | issue pending (2) |
| Production orchestration / engineering calculations maintainers | Selected burst and triaxial profiles diverge; optional derating/adders need explicit scope | issue pending (3) |
| Implementation inventory / engineering calculations maintainers | Add/classify the families and definitions named in section 9 | issue pending (4) |
| Historical docs and discovery / maintainers | Preserve historical counts as historical; codename the two identifying discovery files | issue pending (5) |
| Worn/corroded casing extension / engineering calculations maintainers | Planned reduced-wall/ovality scope is not established by this pack. [docs/domains/asset-integrity/ffs-offering-catalog.md:L104-L108] | #2201 |

Pending draft references require issue numbers before review clearance; the pack remains DRAFT until independent review and evidence ownership are established. (AQG r0, section 11, Sign-off and independence.)
