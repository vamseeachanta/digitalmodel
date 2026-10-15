# Marine analysis knowledge pack

| Header | Value |
| --- | --- |
| Module | `marine_ops/marine_analysis` |
| Repository | `digitalmodel` |
| Path | `src/digitalmodel/marine_ops/marine_analysis/docs/agent-pack.md` |
| Pack revision | 1 |
| Code revision | `a1be651234e5541809cf1f33d57fce04d1a2ae5b` |
| Analysis family | Riser and mooring; diffraction for RAO interpretation |
| Quality manager role | Riser and mooring quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The main numerical path solves a uniform, positively weighted two-dimensional catenary between specified endpoints, with simplified touchdown geometry, a separate segmented lazy-wave construction and legacy dictionary adapters. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L30-L56] [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L71-L86] [src/digitalmodel/marine_ops/marine_analysis/catenary/lazy_wave.py:L319-L345] [src/digitalmodel/marine_ops/marine_analysis/catenary/adapter.py:L94-L109]

RAO processing reads text AQWA listings and OrcaFlex YAML into six-degree-of-freedom arrays; environmental loading calculates wind/current forces, and the coefficient database holds frequency-dependent added-mass and damping matrices. [src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py:L170-L188] [src/digitalmodel/marine_ops/marine_analysis/models/rao_data.py:L56-L75] [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L560-L626] [src/digitalmodel/marine_ops/marine_analysis/hydrodynamic_coefficients/coefficients.py:L28-L46]

The registry marks the containing `marine_ops` module beta; the README's production-ready wording is a README claim, unverified, rather than numerical qualification. [docs/registry/module-routing.yaml:L129-L134] [src/digitalmodel/marine_ops/marine_analysis/README.md:L1-L5]

The canonical catenary location and the compatibility facade are recorded in technical-debt item 4; the general solver's only accepted distributed load is positive submerged weight, and its solve path does not impose seabed-contact equilibrium. [ROADMAP.md:L133-L133] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L98-L118] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L288-L307]

## 2. Inputs and outputs

### Inputs

| Input | Unit | Accepted range / default | Source and evidence |
| --- | --- | --- | --- |
| `length`, `horizontal_span`, `vertical_span` | m | L > 0, X > 0, L ≥ hypot(X,Y); Y is signed; required | Caller; anchor origin, positive Y puts the fairlead higher. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L30-L39] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L98-L110] |
| `weight_per_length`, `ea_stiffness` | N/m, N | Both > 0; required | Caller, submerged weight and axial stiffness. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L36-L39] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L102-L105] |
| `water_depth`, `seabed_friction` | m; friction unit not declared | None, 0.0 | Depth selects a curve-height intersection; friction is declared but absent from the solve and result equations. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L38-L39] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L120-L228] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L288-L307] |
| Simplified `force`, `weight_per_length`, `vertical_distance` | N, N/m, m | Positive and F > w·d; required for force route | Caller; total fairlead force, with horizontal tangent at touchdown. [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L149-L174] [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L190-L220] |
| `angle_deg`, `vertical_distance` | degree, m | 0 < q < 90; d > 0 | Angle geometry uses q from vertical; `calculate_forces` instead accepts an angle from horizontal. [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L76-L99] [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L237-L253] |
| Lazy-wave geometry and weights | degree, m, N/m | 0 < q < 90; w > 0; buoyed weight < 0; hog > sag > 0; depth > sag; radius default None | Hang-off depth below mean sea level is a reporting datum; bend radius is derived and a supplied radius is cross-checked. [src/digitalmodel/marine_ops/marine_analysis/catenary/lazy_wave.py:L149-L200] [src/digitalmodel/marine_ops/marine_analysis/catenary/lazy_wave.py:L202-L263] |
| AQWA `.lis`; OrcaFlex `.yml` / `.yaml` | ω rad/s; heading and phase degree; displacement m/m and degree/m | Text inputs; six amplitude/phase pairs per record | AQWA has period/frequency/direction rows; YAML period rows are converted by ω = 2π/T. [src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py:L325-L353] [src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py:L253-L276] [src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py:L307-L335] [src/digitalmodel/marine_ops/marine_analysis/rao_processor.py:L24-L39] |
| Environmental conditions / geometry | m/s, degree, kg/m³, m, m², tonne | Required speeds, directions, dimensions and displacement; optional projected areas are estimated | Input dataclasses and coefficient axis units. [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L43-L56] [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L81-L121] |
| Hydrodynamic matrices | ω rad/s; matrix-entry units supplied externally | Matrix shape 6 × 6; type added_mass or damping | CSV import creates one matrix per frequency and warns on frequency mismatch. [src/digitalmodel/marine_ops/marine_analysis/hydrodynamic_coefficients/coefficients.py:L28-L46] [src/digitalmodel/marine_ops/marine_analysis/hydrodynamic_coefficients/coefficients.py:L153-L213] |

### Outputs and conventions

| Output | Unit / shape | Contract and evidence |
| --- | --- | --- |
| Catenary result | N, m; 100 samples | H, signed fairlead V, endpoint tension magnitudes, reported stretch, optional depth intersection, shape arrays and convergence metadata. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L43-L56] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L288-L332] |
| Simplified result | m, N, 1/m | S, X and angle-derived radius, or force-derived H, inverse parameter and suspended weight. [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L42-L50] [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L222-L229] |
| Lazy-wave result | N, m | Five segments, summed geometry, separate H, V and T, legacy summary and vertical closure residual. [src/digitalmodel/marine_ops/marine_analysis/catenary/lazy_wave.py:L429-L452] |
| RAO result / legacy dictionary | (nω, nheading), per DOF | All six arrays have matching dimensions; dictionary conversion retains numeric arrays and selected metadata. [src/digitalmodel/marine_ops/marine_analysis/models/rao_data.py:L99-L143] |
| Environmental result / JSON | N, N·m | Signed longitudinal/lateral forces and yaw moment, wind/current totals, coefficients and input geometry; workflow JSON is optional. [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L124-L138] [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/workflow.py:L25-L42] [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/workflow.py:L88-L99] |
| Coefficient database / plots | 6 × 6 per ω | Added-mass and damping CSV export; frequency-response and matrix plots. [src/digitalmodel/marine_ops/marine_analysis/hydrodynamic_coefficients/coefficients.py:L426-L448] [src/digitalmodel/marine_ops/marine_analysis/hydrodynamic_coefficients/plotting.py:L45-L68] [src/digitalmodel/marine_ops/marine_analysis/hydrodynamic_coefficients/plotting.py:L142-L162] |

Velocity RAOs use (m/s)/m and (degree/s)/m; acceleration RAOs use (m/s²)/m and (degree/s²)/m, with phases in degrees. [src/digitalmodel/marine_ops/marine_analysis/models/rao_data.py:L169-L215]

The phase validator labels Orcina as lag from wave crest and its `ISO_6954` branch as lead, with opposite phase signs; file extension is a fallback and explicit YAML convention takes priority. [src/digitalmodel/marine_ops/marine_analysis/rao_validators.py:L54-L58] [src/digitalmodel/marine_ops/marine_analysis/rao_validators.py:L186-L189] [src/digitalmodel/marine_ops/marine_analysis/rao_validators.py:L698-L759]

UNVERIFIED: U2 — Physical axis orientation, RAO origin/phase-origin transformation and universal agreement between those convention labels and every imported file remain unqualified; preserve source metadata and require the reader-specific checks in section 5, issue pending (2).

UNVERIFIED: U3 — Dimensional metadata for coupled coefficient matrices and independently validated environmental/coefficient benchmarks remain unqualified, issue pending (3).

## 3. Method and governing standards

### General boundary-value solver as coded

For a = H/w, q = X/(2a), m = asinh(Y/(2a·sinh(q))), the implementation uses uₐ = a(m−q), uᶠ = a(m+q), S = 2a·sqrt(1+sinh²(m))·sinh(q) and solves S + H·L/EA − L = 0. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L193-L219]

The endpoint components are Vᵢ = H·sinh(uᵢ/a), Tᵢ = hypot(H,Vᵢ), with shape y = a[cosh(u/a)−cosh(uₐ/a)] and x = u−uₐ; reported elongation is H·L/EA. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L286-L317]

The solver brackets a by doubling, accepts finite bracket residuals whose product is nonpositive, then calls Brent with absolute `xtol = 1e-6` m in a and `maxiter = 200` by default; bracketing attempts use at least 200 iterations, independently of a smaller requested root-iteration budget. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L66-L74] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L156-L191]

Geometry returns infinity for X/(2a) > 350; the separate utility hyperbolic functions saturate at floating-point maximum beyond argument magnitude 700 rather than supplying valid larger physical values. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L207-L219] [src/digitalmodel/marine_ops/marine_analysis/catenary/utils.py:L31-L48] [src/digitalmodel/marine_ops/marine_analysis/catenary/utils.py:L70-L83]

### Comparison with AQG r0 G2.3

The following is the hand-check specification, restricted to zero offset or low load without current, wind or waves; it is distinct from the general solver's fully suspended boundary conditions. (AQG r0, section 6, G2.3.)

| Term | Hand-check expression / interpretation | Coded comparison |
| --- | --- | --- |
| Parameter | a = H/w | Direct agreement in result construction. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L286-L292] |
| Suspended length | s = sqrt(h(h+2a)) | Simplified force route computes the equivalent a·sinh(acosh(1+h/a)); general route solves total S between two arbitrary signed endpoints. [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L209-L217] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L207-L219] |
| Top vertical load | V = w·s | General route uses signed endpoint V; V = w·s applies only where anchor V = 0; lazy-wave hang-off now uses w·S₁. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L288-L292] [src/digitalmodel/marine_ops/marine_analysis/catenary/lazy_wave.py:L357-L361] |
| Top tension | T = H+w·h | Simplified force route sets H = F−w·h; general route computes hypot(H,V), rather than imposing touchdown. [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L202-L214] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L288-L292] |
| Angle | θ = atan(H/V) from vertical | `solve_from_angle` uses the complement; force calculation uses an angle from horizontal, requiring conversion. [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L118-L138] [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L247-L253] |
| Horizontal span | x = a·asinh(s/a) | Angle route implements the same expression; force route uses equivalent acosh height geometry. [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L131-L138] [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L209-L214] |
| Grounded length | x+(L−s) = anchor-to-top distance; L−s ≥ 0 | General solver has no grounded-length balance; its optional touchdown result is a height intersection on the suspended curve. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L193-L219] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L297-L307] |
| Multiple segments | Constant H and signed V = Σwᵢsᵢ | Lazy-wave construction scales radii by weight ratios, sums segment geometry, and checks net descent. [src/digitalmodel/marine_ops/marine_analysis/catenary/lazy_wave.py:L383-L403] [src/digitalmodel/marine_ops/marine_analysis/catenary/lazy_wave.py:L532-L610] |

The independent elastic reference integrates V(s)=Vₐ+w·s, dx/ds=H/T+H/EA and dy/ds=V/T+V/EA to obtain X=(H/w)[asinh(Vᶠ/H)−asinh(Vₐ/H)]+HL/EA and Y=(Tᶠ−Tₐ)/w+(Vₐ+wL/2)L/EA, with ΔL=∫T(s)/EA ds. [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L43-L58] [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L119-L143]

These elastic coordinate increments agree term by term with the opened MoorPy `eval_func_cat`, ProfileType 1 EXF/EZF residuals at revision `1fb29f8eca2618543b2df7056adfdfce0265737b`; the fixture records that source revision. (MoorPy, *Catenary.py*, cited revision, ProfileType 1 endpoint residuals.) [tests/fixtures/test_vectors/mooring_risers/mooring_catenary_solver.yaml:L13-L20]

The production residual instead adds horizontal displacement HL/EA to geometric arc length, omits the vertical elastic increment and labels horizontal displacement as total elongation; the existing strict expected-failure test tracks this difference under #2050. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L193-L201] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L294-L295] [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L215-L247]

The residual enforces S = L−HL/EA: reducing EA shortens the geometric curve and raises H, the opposite direction to elastic extension; a converged root therefore cannot qualify the elastic physics (#2050). [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L193-L201] [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L43-L58]

Offline recomputation of the V3 geometry compares `CatenarySolver.solve` with the independent `_derive` endpoint integration; H is biased upward by the amounts below, and the inextensible H is 671477.6 N for all three rows. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L193-L219] [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L61-L114]

| EA (N) | Production H (N) | Independent elastic H (N) | H excess (N; relative) | Independent maximum T/EA (ppm) |
| --- | --- | --- | --- | --- |
| 64e9 | 671494.6 | 671447.4 | 47.1; 70.2 ppm | 20.1 |
| 2.6e8 | 675732.2 | 664202.7 | 11529.5; 1.7 % | 4939.3 |
| 1e8 | 682848.2 | 653129.5 | 29718.7; 4.6 % | 12772.7 |

Each row was computed from the V3 inputs with only EA changed, using the coded residual and the independent equilibrium integrals; at EA=2.6e8 N, strain remains below 5000.0 ppm while inextensible H is higher than independent elastic H by 1.1 %. [tests/fixtures/test_vectors/mooring_risers/mooring_catenary_solver.yaml:L62-L76] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L193-L219] [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L43-L114]

Until #2050 is resolved, G2.3 uses the inextensible route at or below the strain threshold, with independent elastic integration above it; the production elastic residual is excluded as a hand-check reference. [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L178-L200] [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L215-L247] (AQG r0, section 6, G2.3.)

The AQG switch requires elastic treatment when maximum axial strain T/EA exceeds 0.5 %; the present production solve always retains its HL/EA approximation and has no strain-based model switch. (AQG r0, section 6, G2.3.) [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L193-L201]

UNVERIFIED: U1 — Agreement with O. M. Faltinsen, *Sea Loads on Ships and Offshore Structures* (1990), catenary formulation, and H. M. Irvine, *Cable Structures* (1981), elastic formulation has not been established from their equation text; the attempted public resolver and publisher requests returned HTTP 404 and HTTP 403 respectively, issue pending (1).

The code names OCIMF *Prediction of Wind and Current Loads on VLCCs* (1994) and *Mooring Equipment Guidelines* (MEG4), and labels a phase branch `ISO_6954`; clause text not checked against the licensed copy, with edition and clause-title resolution left to issue pending (1). [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L13-L15] [src/digitalmodel/marine_ops/marine_analysis/rao_validators.py:L54-L58]

Wind/current forces use dynamic pressure times projected area times the corresponding coefficient, with yaw moment additionally multiplied by length overall; no tabulated coefficient values are reproduced here. [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L581-L593] [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L616-L626]

The coefficient utilities compute symmetry using `allclose`, a sampled Hilbert-transform causality comparison and damping ratio B/(2·sqrt(k·mass)); these checks do not establish a complete diffraction solution. [src/digitalmodel/marine_ops/marine_analysis/hydrodynamic_coefficients/coefficients.py:L62-L71] [src/digitalmodel/marine_ops/marine_analysis/hydrodynamic_coefficients/coefficients.py:L106-L128] [src/digitalmodel/marine_ops/marine_analysis/hydrodynamic_coefficients/coefficients.py:L329-L356]

No calculation constants or module code are introduced by this pack; citation-sidecar qualification for the existing numerical routes remains open under issue pending (1). [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L30-L56] [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L124-L138]

## 4. Validated worked examples

The register distinguishes closed-form geometry and independently derived references from self-consistency regressions; a reference PASS does not turn the production elastic XFAIL into a pass. [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L119-L143] [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L215-L247] [tests/marine_ops/marine_engineering/catenary/test_catenary_integration.py:L39-L53]

### Validated examples

| ID / independent basis | Inputs and expected value | Tolerance and existing test node | Offline result |
| --- | --- | --- | --- |
| V1 — touchdown closed form, a = H/w, S² = h(h+2a) | F = 10000 N, w = 50 N/m, h = 100 m; H = 5000 N, a = 100 m, S = 100√3 m, X = 100·asinh(√3) m, V = 5000√3 N | S: absolute 1e-4 m; X: absolute 0.01 m; H: absolute 0.01 N. `tests/marine_ops/marine_engineering/catenary/test_simplified.py::TestSimplifiedCatenarySolverForceBased::test_force_based_basic` [tests/marine_ops/marine_engineering/catenary/test_simplified.py:L40-L52] [tests/marine_ops/marine_engineering/catenary/test_simplified.py:L144-L159] | PASS at BASE and AFTER |
| V2 — same closed-form geometry, angle from vertical | q = 30 degree, h = 100 m; a = 100 m, S = 100√3 m, X = 100·asinh(√3) m | Absolute 1e-4 m. `tests/marine_ops/marine_engineering/catenary/test_simplified.py::TestSimplifiedCatenarySolverAngleBased::test_angle_based_basic` [tests/marine_ops/marine_engineering/catenary/test_simplified.py:L22-L37] [tests/marine_ops/marine_engineering/catenary/test_simplified.py:L58-L68] | PASS at BASE and AFTER |
| V3 — independent elastic reference, not production qualification | L = 1000 m, X = 800 m, Y = 100 m, w = 1962 N/m, EA = 64e9 N; expected H and signed endpoint forces are the independently integrated fixture outputs | Force tolerance 5e-7 N; parameter 5e-7 m; elongation 5e-13 m; derivation uses binary64-based round-off. `tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py::test_fixture_matches_independent_derivation` [tests/fixtures/test_vectors/mooring_risers/mooring_catenary_solver.yaml:L62-L120] [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L119-L162] | PASS at BASE and AFTER |
| V4 — inextensible general catenary identities, not production qualification | Same L, X, Y and w; EA → ∞; ΔL = 0 m, Vᶠ−Vₐ = wL, sqrt(L²−Y²) = 2a·sinh(X/(2a)) | Relative sqrt(ulp(1.0)), dimensionless floating-point allowance. `tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py::test_inextensible_chord_arc_and_end_forces` [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L14-L15] [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L178-L200] | PASS at BASE and AFTER |

### Regression examples and unresolved comparison

| ID / basis | Existing test node | Expected assertion, tolerance and offline result |
| --- | --- | --- |
| R1 — general-solver smoke, no independent force target | `tests/marine_ops/marine_engineering/catenary/test_catenary_integration.py::TestUnifiedModuleIntegration::test_phase1_solver_still_works` | Converged; H > 0 N; total fairlead tension > H. PASS at BASE and AFTER. [tests/marine_ops/marine_engineering/catenary/test_catenary_integration.py:L39-L53] |
| R2 — synthetic AQWA text → model → legacy dictionary | `tests/marine_ops/marine_engineering/test_unified_rao_reader.py::TestUnifiedRAOReader::test_data_model_to_dict_conversion` | Expected ω = 0.628 rad/s and surge amplitudes [1.0, 0.2, 1.05] m/m with relative 0.01; headings [0,90,180] degree via allclose. PASS at BASE and AFTER. [tests/marine_ops/marine_engineering/test_unified_rao_reader.py:L206-L231] |
| R3 — synthetic YAML file write → reader | `tests/marine_ops/marine_engineering/test_unified_rao_reader.py::TestUnifiedRAOReader::test_read_orcaflex_yml` | At least 1 frequency and 2 headings, displacement metadata retained; no independent physical target. PASS at BASE and AFTER. [tests/marine_ops/marine_engineering/test_unified_rao_reader.py:L138-L150] |
| R4 — modern lazy-wave geometry versus legacy | `tests/test_lazy_wave_legacy_comparison.py::TestLegacyComparison::test_hangoff_section_matches_legacy` | Hang-off S, X and radius compare at relative 1e-9; shared lineage makes this regression evidence. PASS at BASE and AFTER. [tests/test_lazy_wave_legacy_comparison.py:L69-L93] |
| L1 — production versus independent elastic reference | `tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py::test_solver_against_reconciled_fixture` | Existing strict XFAIL, assertion mismatch, #2050; not a validated production example. XFAIL at BASE and AFTER. [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L215-L247] |

### Planned reader coverage

Planned AQWA and OrcaFlex read/write/read cases are NOT_RUN: no committed test node at the pin checks a complete two-read round trip across all six amplitudes and phases; R2/R3 are existing partial reader regressions, not round-trip qualification. Add the two test nodes under issue pending (2), using amplitude absolute error ≤1e-12 m/m or degree/m and phase absolute error ≤1e-12 degree; C10/C11 remain NOT_RUN until those nodes exist. [tests/marine_ops/marine_engineering/test_unified_rao_reader.py:L138-L150] [tests/marine_ops/marine_engineering/test_unified_rao_reader.py:L206-L231] [src/digitalmodel/marine_ops/marine_analysis/models/rao_data.py:L126-L143]

The test commands used `PYTHONPATH=src uv run --no-sources python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>` without an addopts override, one command at a time, at BASE `1f397111a86441e42842ee6e9559df38a0715af8` and again on the uncommitted documentation tree above BASE; no licensed solver or licensed interface was invoked.

| Paths | BASE: collected / passed / failed / skipped / XFAIL / NOT_RUN | AFTER: collected / passed / failed / skipped / XFAIL / NOT_RUN |
| --- | --- | --- |
| `tests/marine_ops/marine_analysis/` and `tests/marine_ops/marine_engineering/catenary/` | 97 / 94 / 0 / 2 / 1 / 2 | 97 / 94 / 0 / 2 / 1 / 2 |
| `tests/marine_ops/marine_engineering/test_unified_rao_reader.py`, `tests/test_lazy_wave_force_equilibrium.py`, `tests/test_lazy_wave_kernel_defects.py`, `tests/test_lazy_wave_legacy_comparison.py`, `tests/subsea/catenary_riser/test_lazy_wave_quarantine.py`, `tests/marine_ops/marine_engineering/test_mooring_catenary.py` | 82 / 82 / 0 / 0 / 0 / 0 | 82 / 82 / 0 / 0 / 0 / 0 |

The `tests-contracts` domain used the same command form with paths `tests/contracts/ tests/compat/ tests/cross_repo/ tests/docs/ tests/test_apistd2rd_migration.py tests/test_module_reorganization.py tests/test_restructure_compat.py tests/test_units.py tests/test_validation_utils.py`; no test selection or existing skip was changed.

| Contracts result | BASE | AFTER, uncommitted tree above BASE |
| --- | --- | --- |
| Collected nodes / bodies run | 883 / 823 | 883 / 823 |
| Passed / failed / skipped | 821 / 2 / 61 | 822 / 1 / 61 |
| Routing contract, `test_registry_covers_operator_map_rows_and_required_fields` | FAIL: extra top-level module | PASS: metadata nested under the existing domain |

Contracts skips include 60 collected nodes and one collection-level module skip; the remaining failure is `tests/contracts/test_string_addressed_dependencies.py::test_the_three_known_defects_are_still_derived` at line 632, the unchanged Excel-engine assertion, also present in `origin/main`; that assertion and its source file were compared with main, without changing unrelated code. [tests/contracts/test_string_addressed_dependencies.py:L620-L635] [src/digitalmodel/asset_integrity/common/data.py:L449-L454]

Execution record: BASE is `1f397111a86441e42842ee6e9559df38a0715af8`; the code pin remains `a1be651234e5541809cf1f33d57fce04d1a2ae5b`, and AFTER is the uncommitted tree above BASE, changing only this pack and registry metadata. NOT_RUN above overlaps the two missing-fixture skips, rather than adding tests to the collected count; 95 and 82 test bodies respectively ran, including the expected failure.

The missing-fixture nodes are `tests/marine_ops/marine_analysis/test_rao_quality_checks.py::TestIntegrationWithRealData::test_real_semi_rao_quality_check` and `tests/marine_ops/marine_analysis/test_rao_quality_checks.py::TestIntegrationWithRealData::test_real_data_report_structure`, both NOT_RUN: `Real RAO data file not found`; fixture gaps belong to issue pending (2). [tests/marine_ops/marine_analysis/test_rao_quality_checks.py:L430-L490]

## 5. QA gates

The gate mapping below applies the riser/mooring family to this offline utility and identifies records still required at its consuming model; G4 includes RAO reader interpretation, without claiming mesh or hydrostatic qualification. (AQG r0, sections 3, 8 and 10.)

| Gate | Applicability and module-specific check | Numeric pass criterion and unit | Evidence / current disposition |
| --- | --- | --- | --- |
| G0 | Applicable; C01 input validation | Invalid L, X, w, EA: ≥1 ValueError per invalid case; accepted S ≥ X, m | Existing validation and simplified invalid-input tests; BASE/AFTER PASS. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L98-L110] [tests/marine_ops/marine_engineering/catenary/test_simplified.py:L105-L138] |
| G0 | Applicable; C02 unit and array contract | 6 DOFs; amplitude/phase shape differences = 0 cells; explicit rad/s, degree, m/m and degree/m | Reader model checks and R2; PASS for synthetic examples. [src/digitalmodel/marine_ops/marine_analysis/models/rao_data.py:L99-L114] [src/digitalmodel/marine_ops/marine_analysis/rao_processor.py:L24-L39] |
| G1 | Applicable; C03 offline read and downstream target load | Offline: 0 parse errors, 6 DOFs; consuming model: 0 target-program load errors and 0 warnings, version matches in 1 record | R2/R3 PASS offline; OrcaFlex consuming model: version-matched demo load where supported, otherwise licensed `LoadData`; AQWA consuming deck: target-version hydrostatics-stage load. Both target loads NOT_RUN: no consuming model/deck or licensed route supplied. [src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py:L170-L200] (AQG r0, section 5, G1.1–G1.3.) |
| G2 | Applicable; C04 strain-model choice | Max independent endpoint T/EA ≤ 0.005 dimensionless: use the inextensible route (EA → ∞); >0.005: use independent full elastic coordinate equations | Compute strain with `_derive`; use `solve_from_force` / `solve_from_angle` for touchdown or `_derive(elastic=False)` for a fully suspended line; exclude the production elastic residual from the G2.3 reference until #2050 is resolved. [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L61-L114] [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L178-L200] (AQG r0, section 6, G2.3.) |
| G2 | Applicable; C05 numerical convergence | converged = 1; finite H > 0 N; root xtol ≤ 1e-6 m in a; inextensible abs(Sgeometry−100√3) ≤ 1e-6 m for V1/V2 | V1/V2 comparison uses EA → ∞ and independently recomputes 2a·sqrt(1+sinh²(m))·sinh(X/(2a)); length criterion is additional to the solver's root xtol. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L174-L191] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L193-L219] |
| G2 | Applicable; C06 touchdown-length closure | 0 ≤ s ≤ L, m; abs(x+L−s−Xtarget) ≤ 1e-6 m; lazy-wave abs(vertical closure) ≤ 1e-6 m | Hand calculation plus screened zero-offset case and kernel closure tests; PASS on the sampled cases; arbitrary seabed contact unqualified, #2050. [src/digitalmodel/marine_ops/marine_analysis/catenary/lazy_wave.py:L386-L403] [src/digitalmodel/riser_configuration_screen/static.py:L179-L212] |
| G2 | Applicable; C07 force triangle / independent elastic comparison | Lazy wave: relative abs(V−wS)/(wS) ≤ 1e-12; angle relative error ≤ 1e-9 dimensionless; full elastic reference force absolute error ≤ 5e-7 N | Lazy-wave force tests PASS; production elastic comparison remains XFAIL, #2050. [tests/test_lazy_wave_force_equilibrium.py:L58-L94] [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L215-L247] |
| G2 | Applicable; C08 implementation equivalence | abs(Svariant−Sreference) ≤ 1e-4 m; abs(Xvariant−Xreference) ≤ 1e-4 m; rounded APIs use their output resolution: 0.01 m or 0.1 m | Section 9 matrix identifies disagreement rather than accepting every route; #1811. [tests/marine_ops/marine_engineering/catenary/test_simplified.py:L58-L68] [src/digitalmodel/orcaflex/mooring_design.py:L326-L334] [src/digitalmodel/orcaflex/riser_config.py:L220-L228] |
| G2 | Applicable; C09 bracket / overflow guard | 2 finite bracket residuals with product ≤ 0 m²; 0 overflow exceptions; final H > 0 N | Canonical bracketing plus `TestCatenaryBracketRegression` convergence/overflow nodes PASS at BASE; #554. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L153-L181] [tests/marine_ops/marine_engineering/test_mooring_catenary.py:L569-L610] |
| G3 | NOT_APPLICABLE to the utility: no time integrator or supplied dynamic model | Consuming analysis still requires its own short-dynamics record | Utility produces static geometry or parsed responses; it cannot certify a dynamic run. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L43-L56] [src/digitalmodel/marine_ops/marine_analysis/models/rao_data.py:L219-L230] (AQG r0, section 7.) |
| G4 | Applicable to AQWA reader interpretation; C10 phase preservation | All 6 parsed phases preserved to absolute 1e-12 degree on a synthetic read/write/read; correct lead label in 1 `.lis` convention check | NOT_RUN: no committed full read/write/read node; planned AQWA case in section 4, issue pending (2); physical convention / coordinate-origin qualification remains open. [src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py:L378-L395] [src/digitalmodel/marine_ops/marine_analysis/rao_validators.py:L731-L739] |
| G4 | Applicable to OrcaFlex reader interpretation; C11 phase preservation | All 6 parsed phases preserved to absolute 1e-12 degree; 1 YAML lag default and 1 explicit lead override checked | NOT_RUN: no committed full read/write/read node; planned OrcaFlex case in section 4, issue pending (2); new diffraction data still require an external G4 record. [src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py:L307-L335] [src/digitalmodel/marine_ops/marine_analysis/rao_validators.py:L721-L748] |
| G5 | Applicable; C12 report/evidence completeness | 0 missing case results, input revisions, units or dispositions in this register; consuming reports require 1 linked gate appendix and 0 uncited result records | RAO HTML/CSV regression tests PASS; full AQG appendix is a consuming-report requirement, issue pending (3). [src/digitalmodel/marine_ops/marine_analysis/rao_quality_report.py:L43-L76] [tests/marine_ops/marine_analysis/test_rao_quality_checks.py:L325-L362] (AQG r0, section 9.) |

C01–C12 define review criteria; a PASS for documentation completeness is not gate clearance for an analysis, and the unresolved elastic comparison is retained as a failure disposition under #2050. [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L215-L247]

## 6. Known failure modes and pitfalls

| Symptom / cause | Detection | Incident / date / pinned disposition |
| --- | --- | --- |
| Silent elastic error: S=L−HL/EA makes the curve shorter as EA falls and raises H; V3 H excess reaches 1.7 % at EA=2.6e8 N within the permitted inextensible strain band | C04/C07; use inextensible geometry or independent elastic endpoint integration, never the production elastic residual as the hand-check reference | #2050; strict XFAIL remains; section 3 quantifies the bias. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L193-L201] [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L43-L114] [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L215-L247] |
| Plausible but inconsistent force results from selecting a different catenary API or force/length convention | C08; section 9 uses V1/V2; angle adapter agreement is insufficient to qualify its force route | #1811, observed at this pin; historical issue inventory lists eight implementations, while the current facade routes to the canonical package (digitalmodel, issue #1811, implementation inventory). [ROADMAP.md:L133-L133] [src/digitalmodel/marine_ops/marine_analysis/catenary/adapter.py:L161-L177] [tests/marine_ops/marine_engineering/catenary/test_catenary_integration.py:L77-L92] |
| Legacy lazy-wave vertical force includes horizontal force; legacy output-derived expected values share the same lineage | C07; `test_legacy_vertical_force_excess_is_horizontal_force_pin` explicitly detects the excess | #2044, observed at this pin; current kernel uses V=wS and hypot(H,V), and corrected force-equilibrium tests PASS. [tests/test_lazy_wave_force_equilibrium.py:L20-L49] [src/digitalmodel/marine_ops/marine_analysis/catenary/lazy_wave.py:L357-L361] [tests/test_lazy_wave_force_equilibrium.py:L90-L94] |
| Missing RAO cells can become zero arrays; missing YAML headings can become a default grid; a shortened legacy AQWA header can yield empty arrays | C02 plus full-cell round-trip coverage; no detection check exists in the named test directories for arbitrary sparse grids | Issue pending (2), observed at this pin. [src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py:L247-L251] [src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py:L287-L303] [src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py:L325-L335] [src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py:L146-L180] |
| Phase default or file extension mistaken for physical convention; report labels always describe lag | C10/C11 plus explicit metadata; no detection check exists for physical origin transformation in the selected tests | Issue pending (2); report wording and validator convention branches differ. [src/digitalmodel/marine_ops/marine_analysis/rao_quality_report.py:L425-L432] [src/digitalmodel/marine_ops/marine_analysis/rao_validators.py:L721-L759] |
| Missing sign-changing bracket or sinh overflow | C09; existing integration edge cases and explicit historical regression nodes | #554; closed incident, current solver doubles the bracket and rejects large hyperbolic arguments. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L153-L181] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L207-L219] [tests/marine_ops/marine_engineering/test_mooring_catenary.py:L555-L610] |
| Quarantined lazy-wave analyzer returns no geometry instead of fabricated depth/tension fractions | C01/C08; `test_analyzer_refuses_rather_than_fabricating` and buoyancy-invariance contract | #1949; `analyze()` raises NotImplementedError; the current kernel separately enforces signs, radius binding and closure. [src/digitalmodel/subsea/catenary_riser/lazy_wave.py:L34-L73] [tests/subsea/catenary_riser/test_lazy_wave_quarantine.py:L63-L89] [src/digitalmodel/marine_ops/marine_analysis/catenary/lazy_wave.py:L202-L263] |
| Modern geometry angle and force helper accept different angle origins | C07; use complementary angle explicitly when crossing APIs | #1811; integration test supplies 30 degree to the modern force helper and 60 degree to the legacy adapter. [tests/marine_ops/marine_engineering/catenary/test_catenary_integration.py:L94-L109] |

Dates above refer to the pinned inspection record, 2026-10-15, rather than assuming an incident's report date is its correction date.

## 7. Lessons log

| Date | Change reference | Lesson and evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-15 | Initial pack revision | Separate numerical convergence, shared-lineage parity and independent physical qualification; the existing strict elastic XFAIL demonstrates why. [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L215-L247] | Initial equation comparison and example classification | Pending maintainer entry; issue pending (3) |

## 8. Ownership

Requested review roles: riser and mooring quality manager; diffraction quality manager as deputy for RAO readers. Requested producing role: module maintainer. Escalation route: the family quality manager for equations, tolerances, source editions and ambiguous conventions; the maintainer for unresolved scope or evidence. Clearance remains requested, not recorded.

## 9. Dependencies

The numerical path uses NumPy signed hyperbolic geometry and SciPy Brent root finding; the YAML reader parses documents into NumPy arrays and response containers, while the environmental route loads tabular coefficients and interpolates them. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L174-L191] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L207-L219] [src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py:L63-L71] [src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py:L203-L224] [src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py:L92-L109] [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L275-L279] [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L221-L245]

| Upstream / downstream inventory key | Relationship and evidence |
| --- | --- |
| `marine_ops/marine_analysis` | Owning utility; canonical catenary, readers, loading and coefficient database. [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L77-L118] [src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py:L170-L188] [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L560-L626] [src/digitalmodel/marine_ops/marine_analysis/hydrodynamic_coefficients/coefficients.py:L28-L46] |
| `marine_ops/marine_engineering` | Compatibility consumer, directly re-exporting canonical catenary objects. [src/digitalmodel/marine_ops/marine_engineering/__init__.py:L18-L26] |
| `subsea/catenary_riser` | Distinct domain APIs and legacy routes; lazy-wave quarantine redirects to the canonical kernel. [src/digitalmodel/subsea/catenary_riser/lazy_wave.py:L59-L73] |
| `subsea/mooring_analysis` | Distinct kN API; accepts mass per length and converts to weight, unlike the canonical N/m input. [src/digitalmodel/subsea/mooring_analysis/catenary.py:L51-L73] |
| `orcaflex` | Pure numerical inline touchdown solver, with N/m input and rounded kN outputs; Faltinsen attribution is in its docstring. [src/digitalmodel/orcaflex/mooring_design.py:L251-L286] [src/digitalmodel/orcaflex/mooring_design.py:L326-L334] |
| `field_development` / `riser_configuration_screen` | Geometry sampling and mobile-touchdown screening consume the canonical kernels. [src/digitalmodel/field_development/layout_model.py:L443-L489] [src/digitalmodel/riser_configuration_screen/static.py:L179-L215] |

### Offline catenary inventory and agreement

Comparison record at the code pin: V1 and V2 describe the same touchdown geometry; the canonical solver was supplied the closed-form S and X, h=100 m, w=50 N/m and EA=∞, returning H=5000.0 N, V=8660.3 N and T=10000.0 N. General comparison B uses the V3 inputs and the production residual, returning H=671494.6 N and V=1100062.4 N; this is regression output, not the independent elastic answer. [tests/marine_ops/marine_engineering/catenary/test_simplified.py:L58-L68] [tests/marine_ops/marine_engineering/catenary/test_simplified.py:L144-L159] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L193-L219] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L286-L295]

| Other implementation / path | Cases exercised offline / agreement with canonical |
| --- | --- |
| `src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py` | V1/V2 agree: S=173.2 m, X=131.7 m; a=100.0 m. [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L131-L138] [src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py:L209-L217] |
| `src/digitalmodel/marine_ops/marine_analysis/catenary/adapter.py` | V2 agrees; V1 force route differs: S=30000.0 m, X=571.0 m; retained legacy formula lacks the square root in length. [src/digitalmodel/marine_ops/marine_analysis/catenary/adapter.py:L161-L177] [src/digitalmodel/marine_ops/marine_analysis/catenary/adapter.py:L230-L249] |
| `src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py` | V2 agrees; V1 force route produces the same discrepant S=30000.0 m and X=571.0 m; lazy-wave geometry parity is R4, with force excess separately detected under #2044. [src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L32-L61] [tests/test_lazy_wave_force_equilibrium.py:L20-L49] |
| `src/digitalmodel/subsea/catenary_riser/legacy/catenary_equation.py` | V2 agrees; V1 force route produces S=30000.0 m and X=571.0 m. [src/digitalmodel/subsea/catenary_riser/legacy/catenary_equation.py:L34-L55] [src/digitalmodel/subsea/catenary_riser/legacy/catenary_equation.py:L63-L80] |
| `src/digitalmodel/subsea/mooring_analysis/catenary.py` | V1 with H=5.0 kN, mass/m=50/9.81 and h=100 m differs: S=117.5 m, X=54.3 m, T=7715.4 N; h is used as the hyperbolic horizontal argument. [src/digitalmodel/subsea/mooring_analysis/catenary.py:L65-L104] |
| `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | V1 direct H=5000 N, w=50 N/m, h=100 m produces the same S=117.5 m, X=54.3 m and T=7715.4 N disagreement; the direct geometric helper was exercised with only its required result attributes. [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L103-L172] |
| `src/digitalmodel/orcaflex/mooring_design.py::solve_catenary` | V1 agrees at rounded-output resolution: S=173.2 m, X=131.7 m, H=5000.0 N, T=10000.0 N; does not accept the arbitrary two-ended V3 boundary conditions. [src/digitalmodel/orcaflex/mooring_design.py:L288-L334] |
| `src/digitalmodel/orcaflex/riser_config.py::SCRDesignInput.calculate_geometry` | V2 geometry agrees at 0.1 m output resolution: S=173.2 m, X=131.7 m; force depends on the supplied pipe's derived weight, so no V1 force comparison is claimed. [src/digitalmodel/orcaflex/riser_config.py:L181-L228] |
| `src/digitalmodel/orcaflex/pipelay_analysis.py::JLayConfig.calculate_sagbend` | V1 geometry comparison uses equivalent H/w=100 m with a tower angle inside the API range; S=173.2 m and X=131.7 m agree at 0.1 m output resolution; no equality between its specified top tension and the V1 force triangle is claimed. [src/digitalmodel/orcaflex/pipelay_analysis.py:L211-L259] |
| `src/digitalmodel/riser_configuration_screen/static.py::scr_offsets` | V2 at zero excursion with 100 m grounded tail agrees: S=173.2 m, suspended X=131.7 m, H=5000.0 N, T=10000.0 N; grounded length=100.0 m; this wrapper solves a separate grounded-length closure. [src/digitalmodel/riser_configuration_screen/static.py:L179-L215] |
| `src/digitalmodel/field_development/layout_model.py::_catenary_riser_polyline` | V2 geometric consumer: endpoint rise=100.0 m; sampled chord length=173.1 m; analytical S=173.2 m; lower chord length is expected sampling behavior, not a force solve. [src/digitalmodel/field_development/layout_model.py:L443-L489] |
| `src/digitalmodel/subsea/catenary_riser/lazy_wave.py::LazyWaveAnalyzer` | Quarantine tests PASS; no numeric agreement claim because analyze always refuses, #1949. [src/digitalmodel/subsea/catenary_riser/lazy_wave.py:L34-L73] [tests/subsea/catenary_riser/test_lazy_wave_quarantine.py:L63-L89] |
| `tests/marine_ops/marine_engineering/test_mooring_catenary.py` and `tests/marine_ops/marine_engineering/legacy/test_mooring_catenary.py` | V1 and B agree because the active symbols are rebound to the canonical solver; retained local class bodies are not the executed API. [tests/marine_ops/marine_engineering/test_mooring_catenary.py:L238-L246] [tests/marine_ops/marine_engineering/legacy/test_mooring_catenary.py:L238-L246] |
| `tests/solve_catenary_correct.py` and `tests/solve_general_catenary.py` | V3 input set attempted; both return fsolve status 5 (no converged answer), so no numeric agreement; their nonnegative endpoint-coordinate restriction rejects the signed-anchor geometry. [tests/solve_catenary_correct.py:L6-L41] [tests/solve_general_catenary.py:L32-L77] |
| `tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py` | V3/V4 independently integrate elastic/inextensible equilibrium; production V3 comparison XFAIL, not agreement. [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L43-L114] [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L215-L247] |

### Repository-wide script and example routes

The pinned-tree search covers all tracked Python files, including `scripts/`, `examples/`, `docs/`, `src/` and `tests/`, using catenary definitions, hyperbolic calls and MoorPy/pycatenary imports; the additional computational routes are recorded below rather than excluded by directory. [scripts/python/digitalmodel/analysis/catenary_solver.py:L8-L50] [scripts/python/digitalmodel/analysis/catenary_moorpy.py:L3-L49] [scripts/python/digitalmodel/analysis/pycatenary_simple.py:L1-L20]

V1/V2 have identical S and X: each added geometric route receives h=100 m, X=100·asinh(√3) m, S=100√3 m and a=100 m as required by its signature; kN APIs receive w=0.05 kN/m, H=5 kN and T=10 kN. Function definitions were evaluated from `git show` at the pin, without executing the scripts' top-level plotting or file-writing examples; missing optional packages are recorded explicitly, with no agreement inferred. [tests/marine_ops/marine_engineering/catenary/test_simplified.py:L22-L52] [scripts/python/digitalmodel/analysis/catenary_solver.py:L101-L142] [scripts/python/digitalmodel/analysis/simple_catenary_x1y1_x2y2.py:L36-L64]

| Additional path / entry | V1/V2 offline agreement / limitation |
| --- | --- |
| `scripts/python/digitalmodel/analysis/catenary_solver.py::solve_catenary` | DISAGREES for V1/V2: at the exact geometry its `arc_length(0,X,100,0,-100)` returns 17320.5 m instead of 173.2 m because of the extra multiplier a outside the integral; fitting endpoints, endpoint slopes (0, √3) and S from that same initial guess returns a=34.4 m, endpoint heights 50.0 m and 50.0 m, reported length 7850.3 m; no acceptable geometry, #1811. [scripts/python/digitalmodel/analysis/catenary_solver.py:L16-L50] [scripts/python/digitalmodel/analysis/catenary_solver.py:L84-L93] |
| `scripts/python/digitalmodel/analysis/catenary_equation.py::catenaryEquation` | V2 agrees: S=173.2 m, X=131.7 m; V1 force route DISAGREES: S=30000.0 m, X=571.0 m; same missing-square-root lineage as the adapter, #1811. [scripts/python/digitalmodel/analysis/catenary_equation.py:L24-L48] [scripts/python/digitalmodel/analysis/catenary_equation.py:L54-L75] |
| `scripts/python/digitalmodel/analysis/simple_catenary_x1y1_x2y2.py::solve_a` | V1/V2 geometry agrees: fsolve with initial a=100 m, endpoints (0,0) and (X,100) returns status 1, a=100.0 m; independent arc reconstruction a·sinh(X/a)=173.2 m; no force output or length constraint. [scripts/python/digitalmodel/analysis/simple_catenary_x1y1_x2y2.py:L7-L34] |
| `scripts/python/digitalmodel/analysis/simple_catenary_equation.py::catenary` | V1/V2 curve geometry agrees when a=100 m is supplied: y(X)−y(0)=100.0 m; curve sampler only, with no tension or length solve. [scripts/python/digitalmodel/analysis/simple_catenary_equation.py:L6-L17] |
| `scripts/python/digitalmodel/analysis/catenary_moorpy.py::CatenaryCalculator` | NOT_RUN: `ModuleNotFoundError: No module named 'moorpy'`; no numeric agreement established. Also assigns bending stiffness to EA and calls `dsolve2.catenary`; these require qualification under #1811 before using this route as a reference. [scripts/python/digitalmodel/analysis/catenary_moorpy.py:L6-L22] [scripts/python/digitalmodel/analysis/catenary_moorpy.py:L24-L55] |
| `scripts/python/digitalmodel/analysis/pycatenary_simple.py` | NOT_RUN: `ModuleNotFoundError: No module named 'pycatenary'`; no V1/V2 agreement established; top-level `MooringLine` example, not an independent local equation implementation, #1811. [scripts/python/digitalmodel/analysis/pycatenary_simple.py:L1-L20] |
| `scripts/python/digitalmodel/analysis/pycatenary_simple2.py` | NOT_RUN: same absent `pycatenary` dependency; no V1/V2 agreement established; second `MooringLine` example with floor disabled, #1811. [scripts/python/digitalmodel/analysis/pycatenary_simple2.py:L1-L20] |
| `scripts/orcaflex/prepare_slay_campaign.py::catenary` | V1/V2 agree: S=173.2 m, X=131.7 m, H=5000.0 N, T=10000.0 N after converting its kN outputs; function-only calculation, no model generation or solver load. [scripts/orcaflex/prepare_slay_campaign.py:L24-L47] |
| `examples/demos/gtm/demo_04_shallow_water_pipelay.py::calc_catenary` | V1/V2 agree: S=173.2 m, X=131.7 m and T=10000.0 N after kN conversion; independent inline touchdown equations. [examples/demos/gtm/demo_04_shallow_water_pipelay.py:L349-L419] |
| `scripts/update_baltic_moorings_10m_catenary.py::calculate_catenary_anchor_position` | DISAGREES as a uniform-line reference: synthetic call (0,100,S,0,0,0.05,0,10) returns total offset 350.0 m, touchdown span 179.5 m and grounded tail 23.2 m; empirical length split and offset fallback, #1811; no original campaign values used. [scripts/update_baltic_moorings_10m_catenary.py:L60-L138] |
| `scripts/update_baltic_moorings_10m.py::calculate_catenary_footprint` | DISAGREES as a uniform-line reference: synthetic h=100 m, L=S and zero seabed datum gives footprint 23.2 m; linear length allowance, not equilibrium, #1811; no original campaign values used. [scripts/update_baltic_moorings_10m.py:L40-L57] |
| `docs/domains/mooring/screening/run_screening.py::solve_static` | V1 agrees through the canonical simplified force route; committed `test_fixed_case_matches_existing_force_based_test` PASS in the contracts run; no independent second implementation. [docs/domains/mooring/screening/run_screening.py:L35-L49] [tests/docs/test_mooring_screening_run_record.py:L40-L51] |
| `scripts/python/digitalmodel/legacy/validate_catenary_solver.py` | NOT_RUN: `ModuleNotFoundError: No module named 'src.marine_engineering'` from the obsolete solver import; no V1/V2 numeric agreement claim. It calls the imported solver and compares historical values rather than supplying another equilibrium equation, #1811. [scripts/python/digitalmodel/legacy/validate_catenary_solver.py:L11-L18] [scripts/python/digitalmodel/legacy/validate_catenary_solver.py:L45-L62] |

Additional search hits are classified explicitly: `scripts/python/digitalmodel/legacy/generate_integration_charts.py` duplicates the illustrative rescaled cosh chart, `scripts/examples/pyvista_3d_evaluation.py` and `tests/test_pyvista_integration.py` sample prescribed cosh curves, and `tests/check_catenary_geometry.py` diagnoses a supplied H rather than solving it; none supplies a V1/V2 force answer. [scripts/python/digitalmodel/legacy/generate_integration_charts.py:L938-L954] [scripts/examples/pyvista_3d_evaluation.py:L111-L122] [tests/test_pyvista_integration.py:L156-L167] [tests/check_catenary_geometry.py:L17-L41]

The example workflow delegates to `CatenarySolver`; the solver component builders copy supplied catenary model data, and the manual adapter tests and lazy-wave tests recompute expected identities around the imported APIs; these are consumers or test references, not additional independently selectable solvers. [examples/domains/fpso/fpso_mooring_analysis.py:L285-L316] [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L859-L879] [src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py:L1368-L1388] [tests/marine_ops/marine_engineering/test_adapter_manual.py:L69-L110] [tests/test_lazy_wave_catenary.py:L43-L65]

Pinned tree inventory record: historical shadow filenames `catenary_solver.py`, `catenary_solver_final.py`, `catenary_solver_backup.py`, `catenary_solver_v2.py` and `catenary_solver_fixed.py` below `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/` are absent from the pinned tree; #1811's historical count is not reused as a current count. The roadmap identifies the canonical path and facade (digitalmodel, issue #1811, catenary implementation inventory). [ROADMAP.md:L133-L133]

Routing and plotting helpers are not additional independent equilibrium solvers: the legacy router calls `CatenaryCalculator`, the configuration-screen wrapper calls canonical kernels, and the integration chart uses a rescaled illustrative cosh curve without solving S/H equilibrium. [src/digitalmodel/subsea/catenary_riser/legacy/catenary.py:L26-L35] [src/digitalmodel/riser_configuration_screen/static.py:L179-L215] [src/digitalmodel/marine_ops/marine_analysis/visualization/integration_charts.py:L938-L954]

The S-lay radius helper uses H/w without solving endpoint geometry, and the validation script recomputes legacy identities from canonical results rather than providing another equilibrium solver. [src/digitalmodel/orcaflex/pipelay_analysis.py:L136-L168] [src/digitalmodel/marine_ops/marine_analysis/validation/validate_catenary.py:L176-L215]

Other discovered consumers estimate lengths or assemble model data instead of solving a new catenary boundary-value problem; no alternate tension solution is claimed for them. [src/digitalmodel/data_systems/data_procurement/mooring/client.py:L204-L231] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_catenary.py:L21-L36]

## 10. Coverage and open items

| Item | Coverage / limitation | Responsible role / issue |
| --- | --- | --- |
| U1 — primary references and citation sidecars | UNVERIFIED primary-book agreement; Faltinsen 1990 and Irvine 1981 equation text not obtained; existing OCIMF/phase-label edition and clause titles and numerical citation sidecars still need qualification | Module maintainer; issue pending (1) |
| U2 — physical RAO conventions | UNVERIFIED axes, origins and reader-wide convention semantics; complete synthetic round trips have no committed test nodes; sparse-grid and period/frequency-selector behavior need durable tests | Module maintainer; issue pending (2) |
| U3 — coefficient units and benchmarks | UNVERIFIED coupled-matrix units and independently sourced environmental/coefficient examples | Module maintainer; issue pending (3) |
| E1 — elastic model and seabed boundary conditions | Full elastic coordinate equations and strain switch are not the production model; arbitrary contact/friction qualification not supplied | Module maintainer; #2050 |
| E2 — alternate implementations | V1 force-route differences, subsea geometry disagreement and script arc-length/heuristic disagreement remain; optional MoorPy/pycatenary comparisons are NOT_RUN for absent dependencies; obsolete script import remains; historical shadow inventory differs from the pinned tree | Module maintainer; #1811 |
| E3 — legacy lazy-wave force lineage | Legacy force excess remains detectable; corrected canonical force and fixture checks do not qualify all legacy consumers | Module maintainer; #2044 |
| E4 — bracketing coverage | Named suites and both historical bracket/overflow regression nodes were run; generalized extreme-geometry coverage remains with the incident | Module maintainer; #554 |
| E5 — quarantined domain analyzer | Quarantine contract was run; no dynamic or usable replacement result is claimed for the quarantined API | Module maintainer; #1949 |
| E6 — missing data and downstream qualification | Two real-data integration tests NOT_RUN for absent fixtures; complete solver load, statics/dynamics and diffraction-body gates require separate input records | Module maintainer; issue pending (2) |
| E7 — wider module and evidence publication | Environmental and coefficient source files were read for API/method scope; independent environmental/coefficient benchmarks, complete legacy-consumer coverage, durable round-trip tests, appendix integration and shared-log entry remain outside this qualification | Module maintainer; issue pending (3) |

Read coverage: the catenary solver, simplified, lazy-wave, adapter and utilities; processor, validators and quality report; unified, AQWA and OrcaFlex readers and their parser/data-model routes; environmental and coefficient modules; README; the named catenary test directories; historical consolidation/test-result records; technical-debt item 4. Historical reported counts and mathematical-correctness claims are not accepted as fresh qualification. [src/digitalmodel/marine_ops/marine_analysis/README.md:L1-L18] [docs/domains/catenary/catenary_test_results.md:L112-L137] [ROADMAP.md:L133-L133]

All open items above have an existing issue or a numbered issue draft; final issue numbers and independent review are required before clearance. No private input dataset or client-derived result is included.
