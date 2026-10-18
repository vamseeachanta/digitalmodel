# Catenary riser knowledge pack

| Header | Value |
| --- | --- |
| Module | catenary_riser |
| Repository | digitalmodel |
| Package | src/digitalmodel/subsea/catenary_riser |
| Pack revision | 1 |
| Historical code revision | 97dc074bad7e464d4b26ccd04bb83796ef4d0060 |
| Analysis family | OrcaFlex riser and mooring (MR) |
| Quality manager | Riser and mooring quality manager |
| Status | DRAFT |

Audit update: #2751 corrects the analytical and status routes on main `a84b677f080cb7cf35a5bb7297150bf090689fc2`. Historical line references below refer to the original pin; current corrected contracts use function names and [engineering-basis.md](engineering-basis.md). Status remains DRAFT pending native evidence and broader tracked qualification.

## 1. Purpose and scope

The public API exports material/fluid/configuration models, effective-weight calculations, simple catenary analysis and a lazy-wave analyzer; the latter raises rather than returning its former fabricated geometry. [src/digitalmodel/subsea/catenary_riser/__init__.py:L11-L49] [src/digitalmodel/subsea/catenary_riser/lazy_wave.py:L57-L74]

Explicit-force solutions warn when their suspended span differs from the supplied offset; the force solution governs geometry. The simple analyzer assumes uniform weight, zero bending stiffness, static equilibrium and no current; the routing registry marks the containing subsea domain stable, which does not establish numerical validation of this analyzer. [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L19-L31] [docs/registry/module-routing.yaml:L412-L417]

The report generator consumes supplied results; it does not run a live analysis. [src/digitalmodel/subsea/catenary_riser/report.py:L1-L17]

## 2. Inputs and outputs

| Input | Unit | Range/default and source |
| --- | --- | --- |
| Outer diameter, wall thickness, length | m | Required dataclass fields; no constructor range checks are defined. [src/digitalmodel/subsea/catenary_riser/models.py:L105-L126] |
| Material density; fluid density | kg/m³ | Required property fields; external fluid defaults to the seawater object. [src/digitalmodel/subsea/catenary_riser/models.py:L36-L53] [src/digitalmodel/subsea/catenary_riser/models.py:L105-L118] |
| Elastic modulus; strengths | MPa | Material metadata, not an elastic correction in the simple solver. [src/digitalmodel/subsea/catenary_riser/models.py:L36-L44] [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L116-L167] |
| Water depth; horizontal offset | m | Optional configuration fields, but both must resolve before analysis, including the explicit-tension route. [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L68-L75] |
| Horizontal tension; top tension | N | Optional; explicit horizontal tension takes precedence over top tension, then geometry solving. [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L86-L99] |
| Coating thickness; density | m; kg/m³ | Optional; absent or false-valued coating inputs give zero coating weight. [src/digitalmodel/subsea/catenary_riser/models.py:L161-L169] |
| Buoyancy module length, diameter, start length | m | Start length is measured from the bottom; negative module force denotes upward buoyancy. `buoyancy_force_per_length` requires `riser_outer_diameter` including riser coating. [src/digitalmodel/subsea/catenary_riser/models.py:L202-L226] |

| Output | Unit and convention | Source |
| --- | --- | --- |
| Component and effective weights | N/m; positive sinks, negative floats | [src/digitalmodel/subsea/catenary_riser/models.py:L156-L195] |
| Arc length, offset, grounded length | m; grounded length is clipped at zero, but `converged=False` if the suspended length exceeds available length | [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L116-L167] |
| Horizontal, top and touchdown tension | N; touchdown tension equals horizontal tension | [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L153-L167] |
| Top angle | degrees from vertical; `SimpleCatenaryAnalyzer.analyze_riser().top_angle` follows E3 | [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L140-L141] |
| Tension/angle distributions | m, N, degrees from horizontal | [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L202-L252] |
| Convergence | `CatenaryRiserResult.converged`: true only when the span solve converges and suspended length fits; CLI refuses false | `SimpleCatenaryAnalyzer.analyze_riser` |
| Reports | HTML and Markdown from supplied structures; invalid/missing positive finite utilisation or damage gives N/A; positive annual damage uses scientific notation; DFF and selection basis required for fatigue acceptance. The strength KPI is N/A with no strength results and N/A (incomplete) with unqualified rows; both formats always display it | [src/digitalmodel/subsea/catenary_riser/report.py:L1-L17] |

UNVERIFIED: a common global axis, origin and phase convention across legacy exports and modern results is not established; #2755.

## 3. Method and governing standards

| Method | Implementation and basis |
| --- | --- |
| Effective weight | Steel, coating and contents weights minus displaced-fluid buoyancy; model properties supply the component values. [src/digitalmodel/subsea/catenary_riser/effective_weight.py:L33-L56] |
| Simple geometry | `_solve_catenary_from_H` uses E1–E3: `s=sqrt(z²+2az)`, `x=a acosh(1+z/a)`, `T=H+wz`, top angle from vertical. See [engineering-basis.md](engineering-basis.md). [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L116-L167] |
| Top-tension inverse | `_solve_H_from_top_tension` uses E2: `H=T-wz`, rejecting non-finite or nonpositive H; no interior tension minimum exists. [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L169-L173] |
| Geometry inverse | `_solve_H_from_geometry` bisects the E1 span over the available-length interval and returns an explicit convergence flag; impossible geometry raises. Narrow reading: offset is the suspended span from the vertex, not a seabed endpoint distance. [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L175-L200] |
| Legacy angle route | `CatenaryCalculator._calculate_from_angle` calculates bend radius, suspended length and horizontal span; the force route is a separate formula. [src/digitalmodel/subsea/catenary_riser/legacy/catenary_equation.py:L34-L76] |
| Lazy wave | `LazyWaveAnalyzer.analyze` is quarantined; the error directs callers to the separate five-segment `LazyWaveSolver`. [src/digitalmodel/subsea/catenary_riser/lazy_wave.py:L57-L74] |
| Buoyancy estimate | `estimate_required_buoyancy` scales weight by arch-height/depth and a fixed multiplier; this is a simplified estimate. [src/digitalmodel/subsea/catenary_riser/lazy_wave.py:L174-L196] |

DNV-OS-F201, *Dynamic Risers*, and API RP 1111, *Design, Construction, Operation, and Maintenance of Offshore Hydrocarbon Pipelines*, are named without editions or clauses in the README; clause text not checked against the licensed copy. [src/digitalmodel/subsea/catenary_riser/README.md:L204-L207]

API 5L is named in material presets without a cited edition or table; preset strengths are not reproduced here, and clause text not checked against the licensed copy. [src/digitalmodel/subsea/catenary_riser/models.py:L56-L73]

The updated report sample names DNV-OS-F201:2010 and DNVGL-RP-C203:2016 §2.2 equation (2.2.1). The latter is the cited accumulated-damage criterion in E5; the illustrative DFF=2 input is not a prescribed safety-class selection. Other sample code/curve labels do not establish verified applicability. The sample now reports fatigue FAIL because 41 years is less than the illustrative 50-year requirement. [src/digitalmodel/subsea/catenary_riser/report.py:L113-L127] [src/digitalmodel/subsea/catenary_riser/report.py:L323-L329]

UNVERIFIED: governing editions, clauses, applicability and calculation Citation sidecars are not established for this package; #2755.

## 4. Validated worked examples

The independent closure nodes are checker self-tests: they call only the test-side reference, so they do not validate any package route. Package validation uses the direct comparisons below. The canonical solver, design geometry, lazy-wave solver and legacy geometry are distinct from the simple analyzer.

| Kind / case | Package function/output asserted | Expected quantity / tolerance | Executable test node |
| --- | --- | --- | --- |
| Checker self-test / SCR and SLWR | None; reference closure only | Closure ≤ 1e-6 m, refinement ≤ 1e-9 relative; no package-validation claim | `test_static_reconciliation.py::test_independent_geometry_closes_and_quadrature_converges` |
| Validated / SCR | `SCRDesignInput.calculate_geometry`; `CatenarySolver.solve` | Design length/span within 0.05 m; H within 50 N; solver converged; H and vertical force within 1e-8 relative; depth within 1e-6 m | `test_static_reconciliation.py::test_scr_design_and_general_solver_in_inextensible_limit` |
| Validated / SLWR | `LazyWaveSolver.solve`; `lazyWaveCatenaryEquation` legacy geometry | Segment length/span/drop within 1e-9 relative; H/global vertical force within 1e-12 relative; legacy length within 1e-9 relative | `test_static_reconciliation.py::test_slwr_equilibrium_and_recorded_legacy_force_divergence` |
| Regression / simple analyzer | `SimpleCatenaryAnalyzer.analyze_riser` geometry, tension, angle and convergence | E1–E3: s=1201.275032 m, x=529.928323 m (1e-6 m absolute); T=2757021.972 N (0.001 N absolute); vertical angle=10.448701° (1e-6° absolute); converged true | `test_catenary_riser_unit.py::TestSimpleCatenary::test_analyze_with_horizontal_tension`; `test_audit_regressions.py::test_unit_height_ratio`; `test_geometry_inverse_and_exhaustion` |
| Regression / independent weight values | `EffectiveWeightCalculator.calculate` component and net weights | Steel=2921.296755, contents=1373.753369, buoyancy=2038.028152, net=2257.021972 N/m, each 1e-6 N/m absolute; coating=0 | `test_catenary_riser_unit.py::TestEffectiveWeight::test_weight_components_add_up` |
| Regression / module annulus | `BuoyancyModule.buoyancy_force_per_length`; `EffectiveWeightCalculator.calculate_with_buoyancy_module`; `LazyWaveConfiguration.get_effective_weight_profile` | E4 module force=-3680.947171 N/m, 1e-6 N/m absolute; coated riser diameter passed by both callers | `test_audit_regressions.py::test_annular_module_and_both_callers` |
| Regression / legacy force | `CatenaryCalculator.calculate`; `catenaryEquation` | E1–E2: s=1732.050807569 m, x=1316.957896925 m, 1e-9 m absolute; H=1000000 N, b=.001/m | `test_audit_regressions.py::test_legacy_force_closed_form` |
| Regression / refusal | `LazyWaveAnalyzer.analyze` | Exactly 1 expected NotImplementedError; no fabricated result | `test_lazy_wave_quarantine.py::test_analyzer_refuses_rather_than_fabricating` |
| Regression / report | `render_html`; `render_markdown`; `FatigueResult.status`; `RiserReportData.overall_status` | Expected sections and supplied values present; missing/nonpositive/non-finite damage/utilisation gives N/A; E5 cited DFF used | `test_riser_report.py::test_html_contains_all_sections_and_numbers`; `test_audit_regressions.py::test_invalid_report_values_never_pass`; `test_fatigue_factor_is_a_cited_input` |

All test nodes above are under `tests/subsea/catenary_riser/`. The independent worksheet and governing equation citations are in [engineering-basis.md](engineering-basis.md). The audit's rounded examples are replaced with the unrounded circular-area calculation and explicit absolute tolerances. Legacy lazy-wave force remains a separate strict XFAIL tracked in #2044; it is not cleared by the SLWR geometry check.

Offline execution counts for this update are recorded in the fix PR. The historical pack run at the original pin selected 184 cases (182 PASS, 2 XFAIL); those counts are not evidence for this update.

## 5. QA gates

Each gate names the package function or output it asserts. G1–G3 native checks remain NOT_RUN: commercial bindings and solver execution are prohibited for this audit. Missing prerequisites cannot be interpreted as PASS. AQG r0 §§3–7 and §10 are historical gate references, not supplied native evidence.

| Gate / check | Applicability and package function/output asserted | Numeric pass criterion / evidence |
| --- | --- | --- |
| G0 / W1 | `EffectiveWeightCalculator.calculate`: run `test_catenary_riser_unit.py::TestEffectiveWeight::test_weight_components_add_up`; assert independent component and effective weights from E4, not only their sum | Steel 2921.296755, coating 0, contents 1373.753369, buoyancy 2038.028152, effective 2257.021972 N/m; each absolute error ≤ 1e-6 N/m; keep sum residual < 1e-6 N/m |
| G0 / W2 | `LazyWaveAnalyzer.analyze`: run `test_lazy_wave_quarantine.py::test_analyzer_refuses_rather_than_fabricating`; assert refusal | Exactly 1 expected NotImplementedError, 0 fabricated result objects |
| G1 / L1 | `WriteOrcaflexModel` output files: complete exports cannot be produced at the historical pin because the two type-data resources are absent (section 6). After restoring qualified data, assert each output loads in the exact target version; NOT_RUN, no binding imported here | 0 missing data resources, 0 native load errors/warnings; output object count equals manifest (difference 0); AQG r0 §5 |
| G2 / S1 | `CatenarySolver.solve`, `SCRDesignInput.calculate_geometry`, `LazyWaveSolver.solve` and `lazyWaveCatenaryEquation`: run the two direct package-comparison nodes in section 4 and assert their stated outputs. Closure-only nodes are checker self-tests and do not detect divergent package routes | SCR: converged true, H/vertical force ≤ 1e-8 relative, depth ≤ 1e-6 m; design length/span ≤ 0.05 m and H ≤ 50 N. SLWR: segment geometry ≤ 1e-9 relative, H/global vertical force ≤ 1e-12 relative, legacy length ≤ 1e-9 relative |
| G2 / S2 | `WriteOrcaflexModel` output static top tension, angle and touchdown span: assert each against independent equilibrium after prerequisites; NOT_RUN | Tension/span difference ≤ 5.0%, angle difference ≤ 1.0°; 0 unconverged models; static plan/elevation evidence; AQG r0 §6 |
| G3 / D1 | `WriteOrcaflexModel` output motion/line histories: assert required duration and message counts after native simulation; NOT_RUN | Duration ≥ 1.0 s, 0 instability messages, 0 unreviewed warnings; history and image record; AQG r0 §7 |
| G4 / Q1 | Conditional on each `WriteOrcaflexModel` output embedding displacement RAO datasets from `orcaflexModel`: assert a provenance record for every dataset, including inherited data. Never NOT_APPLICABLE merely because the pack is documentation; BLOCKED until resources and provenance exist | Provenance-record count equals embedded RAO-dataset count; 0 unmatched datasets and 0 missing source digest, units, axes/origin, phase, target revision or upstream qualification record; AQG r0 §§8,10 |
| G5 / R1 | `render_html`, `render_markdown`, `FatigueResult.status` and `RiserReportData.overall_status`: run section-4 rendering/status tests; assert expected sections, N/A for invalid values and cited DFF acceptance; issued appendix audited separately | 0 missing expected report sections, 0 invalid values classified PASS; issued reports require 0 missing gate records/signatures; AQG r0 §9 |

## 6. Known failure modes and pitfalls

Silent-failure and fallback sweep entries below identify source behavior, including intentional defaults; a listed default is not automatically a demonstrated defect.

Audit corrections (#2751); current function names supersede historical line entries:

| Function / symptom | Cause and disposition | Detection / tracking |
| --- | --- | --- |
| `SimpleCatenaryAnalyzer.analyze_riser`: incorrect height argument and uncited minimum removed | E1–E2 closed forms; inverse rejects T≤wz; span solver returns convergence; unavailable length sets false and CLI refuses | `test_analyze_with_horizontal_tension`, `test_unit_height_ratio`, `test_infeasible_top_tension_raises`, `test_geometry_inverse_and_exhaustion`; broader qualification #2756 |
| `SimpleCatenaryAnalyzer.analyze_riser().top_angle`: convention corrected | E3 degrees from vertical; distribution angles remain from horizontal | `test_analyze_with_horizontal_tension`, `test_unit_height_ratio` |
| `BuoyancyModule.buoyancy_force_per_length`: riser displaced area was counted twice | E4 annulus requires coated riser diameter; no disk fallback; both callers pass coating | `test_annular_module_and_both_callers` |
| `CatenaryCalculator.calculate` and `catenaryEquation`: force route returned S in m², chord-based H and extra gravity | E1–E2 square root, H=F-wd, b=w/H with w in N/m; reject inadmissible force inputs | `test_legacy_force_closed_form`, `test_legacy_infeasible_force`; broader qualification #2756 |
| `_status_of`, `FatigueResult.status`: invalid values formerly appeared PASS | Nonpositive/missing/non-finite values give N/A; E5 requires explicit DFF and selection basis; no fixed fatigue marginal multiple; incomplete roll-up is N/A; fatigue cells retain their specified precision; incomplete strength KPI is labelled | `test_invalid_report_values_never_pass`, `test_fatigue_factor_is_a_cited_input`, `test_fatigue_numeric_cells`, `test_strength_kpi_is_incomplete_with_invalid_rows`; remaining fallback scope #2754 |
| `legacy.orcaflexModel.build_model` / `WriteOrcaflexModel`: missing type-data resources | `pkgutil.get_data` requests `tests/test_data/catenary_riser/VesselTypes_Extreme.yml` and `VesselTypes_Fatigue.yml`; neither exists at the historical pin or current base. Both SCR and SLWR raise FileNotFoundError. SCR fails during package resource lookup before writing; SLWR reads a relative source path after opening its destination and can leave a partial export, which is not a usable model; no substitute data or solver clearance is supplied | `test_audit_pack.py::test_export_build_fails_for_missing_type_data` asserts the `build_model` failure; G1 BLOCKED for exports; G4 requires RAO provenance counts; #2753 and #2429 |


| Location / symptom | Cause and disposition | Detection / tracking |
| --- | --- | --- |
| `cli.py:108`: Simple CLI catches all exceptions, prints an error and exits; this is an explicit failure, not a silent success [src/digitalmodel/subsea/catenary_riser/cli.py:L108-L110] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `cli.py:165`: Weight CLI catches all exceptions and exits [src/digitalmodel/subsea/catenary_riser/cli.py:L165-L167] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `cli.py:224`: Lazy-wave quarantine exception is converted to a CLI error [src/digitalmodel/subsea/catenary_riser/cli.py:L224-L225] | Review the stated default, fallback or error path before engineering use. | W2 covers analyzer refusal only; #2753. |
| `cli.py:251`: Outer lazy-wave CLI catches every exception and exits [src/digitalmodel/subsea/catenary_riser/cli.py:L251-L253] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `models.py:159`: Steel-weight gravity is fixed inside a property [src/digitalmodel/subsea/catenary_riser/models.py:L159-L159] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `models.py:168`: Coating gravity is fixed; missing or false-valued inputs give zero coating weight [src/digitalmodel/subsea/catenary_riser/models.py:L168-L169] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `models.py:174`: Contents-weight gravity is fixed [src/digitalmodel/subsea/catenary_riser/models.py:L174-L174] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `models.py:179`: Displaced-fluid buoyancy gravity is fixed [src/digitalmodel/subsea/catenary_riser/models.py:L179-L179] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `models.py:238`: Buoyancy material gravity is fixed [src/digitalmodel/subsea/catenary_riser/models.py:L238-L238] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `models.py:243`: Buoyancy coating gravity is fixed; absent coating gives zero [src/digitalmodel/subsea/catenary_riser/models.py:L243-L245] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `models.py:248`: Displaced module-fluid gravity is fixed [src/digitalmodel/subsea/catenary_riser/models.py:L248-L248] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `models.py:293`: Duplicate module names overwrite profile entries; bare_riser overwrites a module with that name [src/digitalmodel/subsea/catenary_riser/models.py:L293-L295] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `models.py:433`: Water resolves to seawater and empty resolves to air [src/digitalmodel/subsea/catenary_riser/models.py:L433-L438] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `effective_weight.py:19`: Constructor gravity is stored, while calculate reads property weights with fixed gravity [src/digitalmodel/subsea/catenary_riser/effective_weight.py:L19-L21] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `simple_catenary.py:33`: Constructor gravity is stored; configuration properties supply weight [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L33-L40] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `simple_catenary.py:275`: False-valued limit arguments are omitted and an empty limit set passes [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L273-L286] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2756. |
| `lazy_wave.py:29`: Gravity constructor default is retained but analyze is quarantined [src/digitalmodel/subsea/catenary_riser/lazy_wave.py:L29-L32] | Review the stated default, fallback or error path before engineering use. | W2; #2753. |
| `lazy_wave.py:57`: Unreachable former implementation retains depth/offset defaults, tension fractions, bend angles and clipped utilization; no live solution reaches them [src/digitalmodel/subsea/catenary_riser/lazy_wave.py:L57-L171] | Review the stated default, fallback or error path before engineering use. | W2; #1947. |
| `lazy_wave.py:193`: Buoyancy estimate has a fixed multiplier and divides by depth without a range check [src/digitalmodel/subsea/catenary_riser/lazy_wave.py:L193-L196] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `legacy/catenaryRiserSummary.py:39`: Missing update argument falls back to defaults [src/digitalmodel/subsea/catenary_riser/legacy/catenaryRiserSummary.py:L39-L43] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `legacy/catenaryRiserSummary.py:47`: Filename extraction failure falls back to the default filename [src/digitalmodel/subsea/catenary_riser/legacy/catenaryRiserSummary.py:L47-L50] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `legacy/getFatigueLoading.py:28`: Bin counter starts at zero and nonpositive sea-state cells are omitted [src/digitalmodel/subsea/catenary_riser/legacy/getFatigueLoading.py:L28-L32] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `legacy/getFatigueLoading.py:43`: External sea-state height is converted with float, without locale normalization [src/digitalmodel/subsea/catenary_riser/legacy/getFatigueLoading.py:L43-L45] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `legacy/getFatigueLoading.py:46`: Period is converted with float, without locale normalization [src/digitalmodel/subsea/catenary_riser/legacy/getFatigueLoading.py:L46-L48] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `legacy/getFatigueLoading.py:49`: Seed is converted with int [src/digitalmodel/subsea/catenary_riser/legacy/getFatigueLoading.py:L49-L51] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `legacy/getFatigueLoading.py:142`: Any argument-processing failure switches to defaults [src/digitalmodel/subsea/catenary_riser/legacy/getFatigueLoading.py:L142-L146] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `legacy/getFatigueLoading.py:150`: Any filename-processing failure switches to the default name [src/digitalmodel/subsea/catenary_riser/legacy/getFatigueLoading.py:L150-L153] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `legacy/catenary.py:26`: Missing or unsupported calculation returns the input configuration unchanged [src/digitalmodel/subsea/catenary_riser/legacy/catenary.py:L26-L37] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `legacy/catenaryMethods.py:209`: Plot coordinate shift starts at zero [src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L209-L209] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `legacy/catenaryMethods.py:316`: Simple plot coordinate shift starts at zero [src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L316-L316] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `legacy/catenaryMethods.py:389`: Buoyancy calculation converts weight using fixed gravity [src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L389-L389] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `legacy/catenaryMethods.py:452`: External declination is truncated with int [src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L452-L452] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `legacy/pipeProperties.py:73`: Equivalent-pipe calculations apply inch-to-metre area conversion and configured gravity; input unit agreement is assumed [src/digitalmodel/subsea/catenary_riser/legacy/pipeProperties.py:L73-L181] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2753. |
| `report.py:67`: Classification uses a fixed marginal-utilization fraction [src/digitalmodel/subsea/catenary_riser/report.py:L67-L79] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2754. |
| `report.py:145`: Provenance defaults permit empty revision, run and timestamp fields [src/digitalmodel/subsea/catenary_riser/report.py:L145-L151] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2754. |
| `report.py:188`: Missing strength or fatigue results return None [src/digitalmodel/subsea/catenary_riser/report.py:L188-L197] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2754. |
| `report.py:211`: No result statuses produces N/A, not a numeric zero [src/digitalmodel/subsea/catenary_riser/report.py:L211-L213] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2754. |
| `report.py:225`: Missing strength_results becomes an empty list [src/digitalmodel/subsea/catenary_riser/report.py:L225-L228] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2754. |
| `report.py:229`: Missing fatigue_results becomes an empty list [src/digitalmodel/subsea/catenary_riser/report.py:L229-L232] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2754. |
| `report.py:233`: Missing code_checks becomes an empty list [src/digitalmodel/subsea/catenary_riser/report.py:L233-L236] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2754. |
| `report.py:343`: Any report-builder import failure is swallowed [src/digitalmodel/subsea/catenary_riser/report.py:L343-L344] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2754. |
| `report.py:354`: Fallback import failure or absent builder returns None [src/digitalmodel/subsea/catenary_riser/report.py:L354-L356] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2754. |
| `report.py:610` (`render_html`): Any builder incompatibility silently selects fallback HTML; the exception handler is at L635-L638 [src/digitalmodel/subsea/catenary_riser/report.py:L610-L638] | Review the stated default, fallback or error path before engineering use. | no detection check exists; #2754. |

sweep: 88 found

Commands used for the whole package (Python files, followed by manual inspection of the matched functions and unit conversions):

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|1970|float\(|int\(|= 0(\.0)?\b|0\.05|9\.81' src/digitalmodel/subsea/catenary_riser --glob '*.py'
rg -n '\.get\(|0\.0254|1000|default|cache' src/digitalmodel/subsea/catenary_riser --glob '*.py'
```

The sweep found no `continue` statement and no cache write of an empty result in the package; constructor metadata, sample data and plot-style constants are distinguished from error-path defaults above.

| Confirmed defect / lesson | Detection and current scope |
| --- | --- |
| Legacy lazy-wave vertical force adds horizontal force, issue #2044. [src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L161-L175] | Separate strict `test_legacy_lazy_wave_vertical_force_matches_equilibrium` remains XFAIL for two configurations; S1 covers legacy geometry only. [tests/subsea/catenary_riser/test_static_reconciliation.py:L154-L161] |
| Conflicting historical reference answers, issue #2050. [tests/fixtures/test_vectors/mooring_risers/mooring_catenary_solver.yaml:L105-L140] | No detection check exists in section 5 for that separate elastic reference; do not use the superseded answers as a hand reference. |
| Distinct catenary routes require broader qualification; #2756 (related #1811). | E1–E3 simple and legacy force regressions detect the corrected defects; S1 validates only its named canonical/design/lazy geometry outputs, not every route. |
| README presents a working lazy-wave example despite quarantine. [src/digitalmodel/subsea/catenary_riser/README.md:L117-L137] [src/digitalmodel/subsea/catenary_riser/lazy_wave.py:L57-L74] | W2; #2755. |

Additional literal-default inventory (normal initialization and prescribed configuration are retained as explicit audit entries rather than silently omitted):

| Location | Behavior and detection |
| --- | --- |
| `legacy/catenary_riser.py:144` [src/digitalmodel/subsea/catenary_riser/legacy/catenary_riser.py:L144-L144] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/catenary_riser.py:152` [src/digitalmodel/subsea/catenary_riser/legacy/catenary_riser.py:L152-L152] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/catenary_riser.py:95` [src/digitalmodel/subsea/catenary_riser/legacy/catenary_riser.py:L95-L95] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/orcaflexModel.py:13` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L13-L13] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/orcaflexModel.py:170` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L170-L170] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/orcaflexModel.py:228` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L228-L228] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/orcaflexModel.py:277` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L277-L277] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/orcaflexModel.py:279` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L279-L279] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/orcaflexModel.py:287` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L287-L287] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/orcaflexModel.py:389` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L389-L389] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/orcaflexModel.py:401` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L401-L401] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/orcaflexModel.py:402` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L402-L402] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/orcaflexModel.py:512` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L512-L512] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/orcaflexModel.py:513` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L513-L513] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/orcaflexModel.py:1125` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L1125-L1125] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/orcaflexModel.py:1132` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L1132-L1132] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/orcaflexModel.py:420` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L420-L420] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/orcaflexModel.py:573` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L573-L573] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/orcaflexModel.py:579` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L579-L579] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/orcaflexModel.py:620` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L620-L620] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/orcaflexModel.py:625` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L625-L625] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/orcaflexModel.py:626` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L626-L626] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |
| `legacy/orcaflexModel.py:431` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L431-L431] | Explicit zero/default in an assignment, call or configuration dictionary; no detection check exists for alternative input states; #2753. |

Additional branch and placeholder sweep:

| Location | Symptom and detection |
| --- | --- |
| `legacy/catenaryMethods.py:65` [src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L65-L68] | No selected force/angle/span branch falls through with implicit None; no detection check exists; #2753. |
| `legacy/pipeProperties.py:184` [src/digitalmodel/subsea/catenary_riser/legacy/pipeProperties.py:L184-L185] | Buoyancy factor helper is an unimplemented pass returning None; no detection check exists; #2753. |
| `legacy/orcaflexModel.py:518` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L518-L519] | Vessel-type helper is an unimplemented pass returning None; no detection check exists; #2753. |
| `legacy/orcaflexModel.py:522` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L522-L523] | Support-type helper is an unimplemented pass returning None; no detection check exists; #2753. |
| `legacy/orcaflexModel.py:1203` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L1203-L1204] | Winch helper is an unimplemented pass returning None; no detection check exists; #2753. |
| `legacy/orcaflexModel.py:1207` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L1207-L1208] | Group helper is an unimplemented pass returning None; no detection check exists; #2753. |
| `report.py:153` [src/digitalmodel/subsea/catenary_riser/report.py:L153-L155] | Unrecognized provenance mode falls back to uppercase input; no detection check exists; #2753. |
| `report.py:475` (`_status_span`) [src/digitalmodel/subsea/catenary_riser/report.py:L475-L481] | Unrecognized status has empty styling; no detection check exists; #2753. |
| `legacy/orcaflexModel.py:1222` [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L1222-L1288] | Empty load lists or disabled analysis flags produce no model files; no detection check exists; #2753. |
| `legacy/catenary_riser.py:32` [src/digitalmodel/subsea/catenary_riser/legacy/catenary_riser.py:L32-L167] | Disabled riser analysis branches omit their computations; no detection check exists; #2753. |

## 7. Lessons log

| Date | Lesson | Evidence / change | Shared-log entry |
| --- | --- | --- | --- |
| 2026-10-17 | A quarantine must remain visible in the knowledge pack; successful rendering does not validate physics. | Sections 1, 4 and 6 separate refusal, regression and equilibrium checks. [tests/subsea/catenary_riser/test_lazy_wave_quarantine.py:L1-L20] [tests/subsea/catenary_riser/test_riser_report.py:L1-L8] | Not assigned; #2755. |

## 8. Maintainers

Review assignment: riser and mooring quality manager; deputy: independent riser and mooring reviewer; producing discipline: subsea calculations. Proposed criterion changes, ambiguous references and inconsistent data will be referred to the family quality manager; cross-family conflicts will be referred to the maintainers. These are review assignments for this draft, not recorded approvals.

## 9. Dependencies and implementation inventory

The durable catenary workflow routes a versioned angle/depth input through the engine into the legacy calculator; its output is a YAML calculation record, not an OrcaFlex statics result. [docs/registry/workflows.yaml:L24-L31] [src/digitalmodel/subsea/catenary_riser/legacy/catenary.py:L26-L37] [examples/workflows/catenary/input.yml:L1-L14]

NumPy supplies the modern weight and geometry calculations; the legacy orchestration calls pipe properties, plotting and model building. [src/digitalmodel/subsea/catenary_riser/models.py:L7-L10] [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L116-L167] [src/digitalmodel/subsea/catenary_riser/legacy/catenary_riser.py:L19-L29]

No OrcaFlex version or native-load qualification is established by this pack; L1 remains NOT_RUN. The source's references to that program are inventoried below as text, including the legacy generator; they do not establish an installed dependency or licence.

| Dependency reference | Evidence |
| --- | --- |
| Text reference in `README.md` | [src/digitalmodel/subsea/catenary_riser/README.md:L9-L9] |
| Text reference in `README.md` | [src/digitalmodel/subsea/catenary_riser/README.md:L234-L234] |
| Text reference in `__init__.py` | [src/digitalmodel/subsea/catenary_riser/__init__.py:L4-L4] |
| Text reference in `legacy/__init__.py` | [src/digitalmodel/subsea/catenary_riser/legacy/__init__.py:L1-L1] |
| Text reference in `legacy/orcaflexModel.py` | [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L1-L1] |
| Text reference in `report.py` | [src/digitalmodel/subsea/catenary_riser/report.py:L15-L15] |

Implementation-map pin: `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; data revision: `270c8759c374b67a7f032953e7fcdb14e0224461`; map SHA-256: `580b2f1d9cd642faa7d94d9ecde25d6bf3fc33456ee69cc8b0966cf69bc2025c`. The map bytes at the code pin are identical, so the following citations resolve at that pin as well. Inventory identities are the family plus the one-based implementation ordinal and its exact source-map record; this avoids copying discovery identifiers from unrelated packages.

| Map family | In-package rows | All mapped rows | Counted |
| --- | --- | --- | --- |
| `catenary-mooring-line-statics` | 16 | 140 | Yes [docs/registry/implementation-map.yaml:L346-L348] |
| `effective-tension-riser-stack-up` | 7 | 264 | Yes [docs/registry/implementation-map.yaml:L2139-L2141] |
| `orcaflex-text-model-writers` | 5 | 440 | Yes [docs/registry/implementation-map.yaml:L4933-L4935] |
| `wave-spectra` | 2 | 141 | No [docs/registry/implementation-map.yaml:L13593-L13595] |
| `api-5ct-5l-grade-tables` | 1 | 39 | No [docs/registry/implementation-map.yaml:L24099-L24101] |

The exact Python regular expressions stored in each counted family's `search_signals` were used case-sensitively against all package Python files at the code pin; signals matching `subsea/catenary_riser` case-insensitively were omitted (only the statics vocabulary signal). Hits were assigned to the innermost function/class or one module-level definition. No pattern was unescaped, corrected or broadened.

| Further hit group | Classification |
| --- | --- |
| Simple inverse methods | The two inverse methods are listed in section 3; the former nested tension evaluator has been removed. [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L169-L200] |
| Effective-weight methods and properties | Weight bookkeeping rather than a second statics solver; buoyancy calculation and weight profile are helper inputs. [src/digitalmodel/subsea/catenary_riser/effective_weight.py:L33-L92] [src/digitalmodel/subsea/catenary_riser/models.py:L156-L195] [src/digitalmodel/subsea/catenary_riser/models.py:L275-L297] |
| CLI functions, result/configuration classes, plots and report tables | Delegation, data holding or presentation; not further solvers. [src/digitalmodel/subsea/catenary_riser/cli.py:L60-L110] [src/digitalmodel/subsea/catenary_riser/models.py:L304-L407] [src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L202-L342] |
| Legacy extract/save/fatigue functions | Reading, serializing or preparing inputs for listed definitions. [src/digitalmodel/subsea/catenary_riser/legacy/extractData.py:L7-L81] [src/digitalmodel/subsea/catenary_riser/legacy/saveData.py:L14-L47] [src/digitalmodel/subsea/catenary_riser/legacy/getFatigueLoading.py:L25-L90] |
| Legacy wave/current/wind helpers | Populate the listed model writer's data structures. [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L410-L516] |
| Module-level source references | Imports, explanatory text or orchestration, not separate solver launches. [src/digitalmodel/subsea/catenary_riser/legacy/catenary_riser.py:L19-L29] [src/digitalmodel/subsea/catenary_riser/__init__.py:L1-L6] |

Further counted-family implementations not listed in sections 3 or 9: 0; threshold: at most 1. Repository-wide implementations, including scripts, are enumerated below from the same map; this is an inventory, not validation of each algorithm. Unmapped method-family follow-up: effective-weight/buoyancy bookkeeping, input conditioning and report-status qualification; #2753, #2754. Noncounted map families remain listed and do not alter the counted-family result.

Repository-wide text search covered 5679 tracked Python files, including scripts; family hit-file counts (not implementation counts): catenary-mooring-line-statics: 357 files, 42 in scripts; effective-tension-riser-stack-up: 396 files, 23 in scripts; orcaflex-text-model-writers: 2637 files, 442 in scripts; wave-spectra: 496 files, 60 in scripts; api-5ct-5l-grade-tables: 323 files, 20 in scripts. Outside-package hits remain map-follow-up candidates rather than A8 misses; classification beyond the mapped inventory remains #1811 and #2755.

### Complete map-row inventory

| Family / implementation ordinal | Exact map record | Package location where applicable |
| --- | --- | --- |
| catenary-mooring-line-statics / 1 | [docs/registry/implementation-map.yaml:L353-L364] | Outside package; see exact map record |
| catenary-mooring-line-statics / 2 | [docs/registry/implementation-map.yaml:L365-L376] | Outside package; see exact map record |
| catenary-mooring-line-statics / 3 | [docs/registry/implementation-map.yaml:L377-L388] | Outside package; see exact map record |
| catenary-mooring-line-statics / 4 | [docs/registry/implementation-map.yaml:L389-L400] | Outside package; see exact map record |
| catenary-mooring-line-statics / 5 | [docs/registry/implementation-map.yaml:L401-L414] | Outside package; see exact map record |
| catenary-mooring-line-statics / 6 | [docs/registry/implementation-map.yaml:L415-L426] | Outside package; see exact map record |
| catenary-mooring-line-statics / 7 | [docs/registry/implementation-map.yaml:L427-L438] | Outside package; see exact map record |
| catenary-mooring-line-statics / 8 | [docs/registry/implementation-map.yaml:L439-L450] | Outside package; see exact map record |
| catenary-mooring-line-statics / 9 | [docs/registry/implementation-map.yaml:L451-L462] | Outside package; see exact map record |
| catenary-mooring-line-statics / 10 | [docs/registry/implementation-map.yaml:L463-L474] | Outside package; see exact map record |
| catenary-mooring-line-statics / 11 | [docs/registry/implementation-map.yaml:L475-L488] | Outside package; see exact map record |
| catenary-mooring-line-statics / 12 | [docs/registry/implementation-map.yaml:L489-L504] | Outside package; see exact map record |
| catenary-mooring-line-statics / 13 | [docs/registry/implementation-map.yaml:L505-L519] | Outside package; see exact map record |
| catenary-mooring-line-statics / 14 | [docs/registry/implementation-map.yaml:L520-L531] | Outside package; see exact map record |
| catenary-mooring-line-statics / 15 | [docs/registry/implementation-map.yaml:L532-L543] | Outside package; see exact map record |
| catenary-mooring-line-statics / 16 | [docs/registry/implementation-map.yaml:L544-L555] | Outside package; see exact map record |
| catenary-mooring-line-statics / 17 | [docs/registry/implementation-map.yaml:L556-L567] | Outside package; see exact map record |
| catenary-mooring-line-statics / 18 | [docs/registry/implementation-map.yaml:L568-L579] | Outside package; see exact map record |
| catenary-mooring-line-statics / 19 | [docs/registry/implementation-map.yaml:L580-L590] | Outside package; see exact map record |
| catenary-mooring-line-statics / 20 | [docs/registry/implementation-map.yaml:L591-L602] | Outside package; see exact map record |
| catenary-mooring-line-statics / 21 | [docs/registry/implementation-map.yaml:L603-L614] | Outside package; see exact map record |
| catenary-mooring-line-statics / 22 | [docs/registry/implementation-map.yaml:L615-L626] | Outside package; see exact map record |
| catenary-mooring-line-statics / 23 | [docs/registry/implementation-map.yaml:L627-L637] | Outside package; see exact map record |
| catenary-mooring-line-statics / 24 | [docs/registry/implementation-map.yaml:L638-L649] | Outside package; see exact map record |
| catenary-mooring-line-statics / 25 | [docs/registry/implementation-map.yaml:L650-L661] | Outside package; see exact map record |
| catenary-mooring-line-statics / 26 | [docs/registry/implementation-map.yaml:L662-L673] | Outside package; see exact map record |
| catenary-mooring-line-statics / 27 | [docs/registry/implementation-map.yaml:L674-L685] | Outside package; see exact map record |
| catenary-mooring-line-statics / 28 | [docs/registry/implementation-map.yaml:L686-L697] | Outside package; see exact map record |
| catenary-mooring-line-statics / 29 | [docs/registry/implementation-map.yaml:L698-L708] | Outside package; see exact map record |
| catenary-mooring-line-statics / 30 | [docs/registry/implementation-map.yaml:L709-L722] | Outside package; see exact map record |
| catenary-mooring-line-statics / 31 | [docs/registry/implementation-map.yaml:L723-L734] | Outside package; see exact map record |
| catenary-mooring-line-statics / 32 | [docs/registry/implementation-map.yaml:L735-L746] | Outside package; see exact map record |
| catenary-mooring-line-statics / 33 | [docs/registry/implementation-map.yaml:L747-L758] | Outside package; see exact map record |
| catenary-mooring-line-statics / 34 | [docs/registry/implementation-map.yaml:L759-L770] | Outside package; see exact map record |
| catenary-mooring-line-statics / 35 | [docs/registry/implementation-map.yaml:L771-L782] | Outside package; see exact map record |
| catenary-mooring-line-statics / 36 | [docs/registry/implementation-map.yaml:L783-L794] | Outside package; see exact map record |
| catenary-mooring-line-statics / 37 | [docs/registry/implementation-map.yaml:L795-L806] | Outside package; see exact map record |
| catenary-mooring-line-statics / 38 | [docs/registry/implementation-map.yaml:L807-L820] | Outside package; see exact map record |
| catenary-mooring-line-statics / 39 | [docs/registry/implementation-map.yaml:L821-L834] | Outside package; see exact map record |
| catenary-mooring-line-statics / 40 | [docs/registry/implementation-map.yaml:L835-L846] | Outside package; see exact map record |
| catenary-mooring-line-statics / 41 | [docs/registry/implementation-map.yaml:L847-L858] | Outside package; see exact map record |
| catenary-mooring-line-statics / 42 | [docs/registry/implementation-map.yaml:L859-L870] | Outside package; see exact map record |
| catenary-mooring-line-statics / 43 | [docs/registry/implementation-map.yaml:L871-L882] | Outside package; see exact map record |
| catenary-mooring-line-statics / 44 | [docs/registry/implementation-map.yaml:L883-L894] | Outside package; see exact map record |
| catenary-mooring-line-statics / 45 | [docs/registry/implementation-map.yaml:L895-L906] | Outside package; see exact map record |
| catenary-mooring-line-statics / 46 | [docs/registry/implementation-map.yaml:L907-L918] | Outside package; see exact map record |
| catenary-mooring-line-statics / 47 | [docs/registry/implementation-map.yaml:L919-L930] | Outside package; see exact map record |
| catenary-mooring-line-statics / 48 | [docs/registry/implementation-map.yaml:L931-L941] | Outside package; see exact map record |
| catenary-mooring-line-statics / 49 | [docs/registry/implementation-map.yaml:L942-L953] | Outside package; see exact map record |
| catenary-mooring-line-statics / 50 | [docs/registry/implementation-map.yaml:L954-L964] | Outside package; see exact map record |
| catenary-mooring-line-statics / 51 | [docs/registry/implementation-map.yaml:L965-L976] | Outside package; see exact map record |
| catenary-mooring-line-statics / 52 | [docs/registry/implementation-map.yaml:L977-L988] | Outside package; see exact map record |
| catenary-mooring-line-statics / 53 | [docs/registry/implementation-map.yaml:L989-L1002] | Outside package; see exact map record |
| catenary-mooring-line-statics / 54 | [docs/registry/implementation-map.yaml:L1003-L1018] | Outside package; see exact map record |
| catenary-mooring-line-statics / 55 | [docs/registry/implementation-map.yaml:L1019-L1030] | Outside package; see exact map record |
| catenary-mooring-line-statics / 56 | [docs/registry/implementation-map.yaml:L1031-L1042] | Outside package; see exact map record |
| catenary-mooring-line-statics / 57 | [docs/registry/implementation-map.yaml:L1043-L1054] | Outside package; see exact map record |
| catenary-mooring-line-statics / 58 | [docs/registry/implementation-map.yaml:L1055-L1066] | Outside package; see exact map record |
| catenary-mooring-line-statics / 59 | [docs/registry/implementation-map.yaml:L1067-L1078] | Outside package; see exact map record |
| catenary-mooring-line-statics / 60 | [docs/registry/implementation-map.yaml:L1079-L1090] | Outside package; see exact map record |
| catenary-mooring-line-statics / 61 | [docs/registry/implementation-map.yaml:L1091-L1102] | Outside package; see exact map record |
| catenary-mooring-line-statics / 62 | [docs/registry/implementation-map.yaml:L1103-L1114] | `src/digitalmodel/subsea/catenary_riser/lazy_wave.py` — `LazyWaveAnalyzer.analyze` |
| catenary-mooring-line-statics / 63 | [docs/registry/implementation-map.yaml:L1115-L1126] | `src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py` — `catenaryEquation` |
| catenary-mooring-line-statics / 64 | [docs/registry/implementation-map.yaml:L1127-L1138] | `src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py` — `catenaryForces` |
| catenary-mooring-line-statics / 65 | [docs/registry/implementation-map.yaml:L1139-L1150] | `src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py` — `sagHogEquation` |
| catenary-mooring-line-statics / 66 | [docs/registry/implementation-map.yaml:L1151-L1165] | `src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py` — `lazyWaveCatenaryEquation` |
| catenary-mooring-line-statics / 67 | [docs/registry/implementation-map.yaml:L1166-L1179] | `src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py` — `gethorizontalDistanceHangofftoTDPrange` |
| catenary-mooring-line-statics / 68 | [docs/registry/implementation-map.yaml:L1180-L1191] | `src/digitalmodel/subsea/catenary_riser/legacy/catenary_equation.py` — `CatenaryCalculator._calculate_from_angle` |
| catenary-mooring-line-statics / 69 | [docs/registry/implementation-map.yaml:L1192-L1208] | `src/digitalmodel/subsea/catenary_riser/legacy/catenary_riser.py` — `catenary_riser` |
| catenary-mooring-line-statics / 70 | [docs/registry/implementation-map.yaml:L1209-L1222] | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` — `orcaflexModel` |
| catenary-mooring-line-statics / 71 | [docs/registry/implementation-map.yaml:L1223-L1234] | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` — `lineTypes` |
| catenary-mooring-line-statics / 72 | [docs/registry/implementation-map.yaml:L1235-L1249] | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` — `SimpleCatenaryAnalyzer.analyze_riser` |
| catenary-mooring-line-statics / 73 | [docs/registry/implementation-map.yaml:L1250-L1261] | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` — `SimpleCatenaryAnalyzer._solve_catenary_from_H` |
| catenary-mooring-line-statics / 74 | [docs/registry/implementation-map.yaml:L1262-L1273] | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` — `SimpleCatenaryAnalyzer._solve_H_from_top_tension.calc_top_tension` |
| catenary-mooring-line-statics / 75 | [docs/registry/implementation-map.yaml:L1274-L1285] | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` — `SimpleCatenaryAnalyzer._solve_H_from_geometry` |
| catenary-mooring-line-statics / 76 | [docs/registry/implementation-map.yaml:L1286-L1297] | Outside package; see exact map record |
| catenary-mooring-line-statics / 77 | [docs/registry/implementation-map.yaml:L1298-L1311] | Outside package; see exact map record |
| catenary-mooring-line-statics / 78 | [docs/registry/implementation-map.yaml:L1312-L1323] | Outside package; see exact map record |
| catenary-mooring-line-statics / 79 | [docs/registry/implementation-map.yaml:L1324-L1335] | Outside package; see exact map record |
| catenary-mooring-line-statics / 80 | [docs/registry/implementation-map.yaml:L1336-L1347] | Outside package; see exact map record |
| catenary-mooring-line-statics / 81 | [docs/registry/implementation-map.yaml:L1348-L1361] | Outside package; see exact map record |
| catenary-mooring-line-statics / 82 | [docs/registry/implementation-map.yaml:L1362-L1373] | Outside package; see exact map record |
| catenary-mooring-line-statics / 83 | [docs/registry/implementation-map.yaml:L1374-L1387] | Outside package; see exact map record |
| catenary-mooring-line-statics / 84 | [docs/registry/implementation-map.yaml:L1388-L1399] | Outside package; see exact map record |
| catenary-mooring-line-statics / 85 | [docs/registry/implementation-map.yaml:L1400-L1411] | Outside package; see exact map record |
| catenary-mooring-line-statics / 86 | [docs/registry/implementation-map.yaml:L1412-L1425] | Outside package; see exact map record |
| catenary-mooring-line-statics / 87 | [docs/registry/implementation-map.yaml:L1426-L1437] | Outside package; see exact map record |
| catenary-mooring-line-statics / 88 | [docs/registry/implementation-map.yaml:L1438-L1453] | Outside package; see exact map record |
| catenary-mooring-line-statics / 89 | [docs/registry/implementation-map.yaml:L1454-L1465] | Outside package; see exact map record |
| catenary-mooring-line-statics / 90 | [docs/registry/implementation-map.yaml:L1466-L1477] | Outside package; see exact map record |
| catenary-mooring-line-statics / 91 | [docs/registry/implementation-map.yaml:L1478-L1489] | Outside package; see exact map record |
| catenary-mooring-line-statics / 92 | [docs/registry/implementation-map.yaml:L1490-L1503] | Outside package; see exact map record |
| catenary-mooring-line-statics / 93 | [docs/registry/implementation-map.yaml:L1504-L1515] | Outside package; see exact map record |
| catenary-mooring-line-statics / 94 | [docs/registry/implementation-map.yaml:L1516-L1527] | Outside package; see exact map record |
| catenary-mooring-line-statics / 95 | [docs/registry/implementation-map.yaml:L1528-L1541] | Outside package; see exact map record |
| catenary-mooring-line-statics / 96 | [docs/registry/implementation-map.yaml:L1542-L1553] | Outside package; see exact map record |
| catenary-mooring-line-statics / 97 | [docs/registry/implementation-map.yaml:L1554-L1565] | Outside package; see exact map record |
| catenary-mooring-line-statics / 98 | [docs/registry/implementation-map.yaml:L1566-L1579] | Outside package; see exact map record |
| catenary-mooring-line-statics / 99 | [docs/registry/implementation-map.yaml:L1580-L1590] | Outside package; see exact map record |
| catenary-mooring-line-statics / 100 | [docs/registry/implementation-map.yaml:L1591-L1602] | Outside package; see exact map record |
| catenary-mooring-line-statics / 101 | [docs/registry/implementation-map.yaml:L1603-L1614] | Outside package; see exact map record |
| catenary-mooring-line-statics / 102 | [docs/registry/implementation-map.yaml:L1615-L1626] | Outside package; see exact map record |
| catenary-mooring-line-statics / 103 | [docs/registry/implementation-map.yaml:L1627-L1638] | Outside package; see exact map record |
| catenary-mooring-line-statics / 104 | [docs/registry/implementation-map.yaml:L1639-L1652] | Outside package; see exact map record |
| catenary-mooring-line-statics / 105 | [docs/registry/implementation-map.yaml:L1653-L1664] | Outside package; see exact map record |
| catenary-mooring-line-statics / 106 | [docs/registry/implementation-map.yaml:L1665-L1676] | Outside package; see exact map record |
| catenary-mooring-line-statics / 107 | [docs/registry/implementation-map.yaml:L1677-L1690] | Outside package; see exact map record |
| catenary-mooring-line-statics / 108 | [docs/registry/implementation-map.yaml:L1691-L1702] | Outside package; see exact map record |
| catenary-mooring-line-statics / 109 | [docs/registry/implementation-map.yaml:L1703-L1716] | Outside package; see exact map record |
| catenary-mooring-line-statics / 110 | [docs/registry/implementation-map.yaml:L1717-L1730] | Outside package; see exact map record |
| catenary-mooring-line-statics / 111 | [docs/registry/implementation-map.yaml:L1731-L1743] | Outside package; see exact map record |
| catenary-mooring-line-statics / 112 | [docs/registry/implementation-map.yaml:L1744-L1756] | Outside package; see exact map record |
| catenary-mooring-line-statics / 113 | [docs/registry/implementation-map.yaml:L1757-L1769] | Outside package; see exact map record |
| catenary-mooring-line-statics / 114 | [docs/registry/implementation-map.yaml:L1770-L1782] | Outside package; see exact map record |
| catenary-mooring-line-statics / 115 | [docs/registry/implementation-map.yaml:L1783-L1794] | Outside package; see exact map record |
| catenary-mooring-line-statics / 116 | [docs/registry/implementation-map.yaml:L1795-L1806] | Outside package; see exact map record |
| catenary-mooring-line-statics / 117 | [docs/registry/implementation-map.yaml:L1807-L1820] | Outside package; see exact map record |
| catenary-mooring-line-statics / 118 | [docs/registry/implementation-map.yaml:L1821-L1832] | Outside package; see exact map record |
| catenary-mooring-line-statics / 119 | [docs/registry/implementation-map.yaml:L1833-L1844] | Outside package; see exact map record |
| catenary-mooring-line-statics / 120 | [docs/registry/implementation-map.yaml:L1845-L1858] | Outside package; see exact map record |
| catenary-mooring-line-statics / 121 | [docs/registry/implementation-map.yaml:L1859-L1870] | Outside package; see exact map record |
| catenary-mooring-line-statics / 122 | [docs/registry/implementation-map.yaml:L1871-L1882] | Outside package; see exact map record |
| catenary-mooring-line-statics / 123 | [docs/registry/implementation-map.yaml:L1883-L1894] | Outside package; see exact map record |
| catenary-mooring-line-statics / 124 | [docs/registry/implementation-map.yaml:L1895-L1906] | Outside package; see exact map record |
| catenary-mooring-line-statics / 125 | [docs/registry/implementation-map.yaml:L1907-L1918] | Outside package; see exact map record |
| catenary-mooring-line-statics / 126 | [docs/registry/implementation-map.yaml:L1919-L1932] | Outside package; see exact map record |
| catenary-mooring-line-statics / 127 | [docs/registry/implementation-map.yaml:L1933-L1946] | Outside package; see exact map record |
| catenary-mooring-line-statics / 128 | [docs/registry/implementation-map.yaml:L1947-L1960] | Outside package; see exact map record |
| catenary-mooring-line-statics / 129 | [docs/registry/implementation-map.yaml:L1961-L1972] | Outside package; see exact map record |
| catenary-mooring-line-statics / 130 | [docs/registry/implementation-map.yaml:L1973-L1984] | Outside package; see exact map record |
| catenary-mooring-line-statics / 131 | [docs/registry/implementation-map.yaml:L1985-L1996] | Outside package; see exact map record |
| catenary-mooring-line-statics / 132 | [docs/registry/implementation-map.yaml:L1997-L2008] | Outside package; see exact map record |
| catenary-mooring-line-statics / 133 | [docs/registry/implementation-map.yaml:L2009-L2020] | Outside package; see exact map record |
| catenary-mooring-line-statics / 134 | [docs/registry/implementation-map.yaml:L2021-L2032] | Outside package; see exact map record |
| catenary-mooring-line-statics / 135 | [docs/registry/implementation-map.yaml:L2033-L2046] | Outside package; see exact map record |
| catenary-mooring-line-statics / 136 | [docs/registry/implementation-map.yaml:L2047-L2060] | Outside package; see exact map record |
| catenary-mooring-line-statics / 137 | [docs/registry/implementation-map.yaml:L2061-L2074] | Outside package; see exact map record |
| catenary-mooring-line-statics / 138 | [docs/registry/implementation-map.yaml:L2075-L2090] | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` — `SimpleCatenaryAnalyzer.calculate_tension_along_riser` |
| catenary-mooring-line-statics / 139 | [docs/registry/implementation-map.yaml:L2091-L2106] | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` — `SimpleCatenaryAnalyzer.calculate_angle_along_riser` |
| catenary-mooring-line-statics / 140 | [docs/registry/implementation-map.yaml:L2107-L2123] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 1 | [docs/registry/implementation-map.yaml:L2146-L2155] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 2 | [docs/registry/implementation-map.yaml:L2156-L2165] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 3 | [docs/registry/implementation-map.yaml:L2166-L2175] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 4 | [docs/registry/implementation-map.yaml:L2176-L2185] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 5 | [docs/registry/implementation-map.yaml:L2186-L2195] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 6 | [docs/registry/implementation-map.yaml:L2196-L2205] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 7 | [docs/registry/implementation-map.yaml:L2206-L2215] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 8 | [docs/registry/implementation-map.yaml:L2216-L2225] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 9 | [docs/registry/implementation-map.yaml:L2226-L2235] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 10 | [docs/registry/implementation-map.yaml:L2236-L2247] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 11 | [docs/registry/implementation-map.yaml:L2248-L2259] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 12 | [docs/registry/implementation-map.yaml:L2260-L2269] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 13 | [docs/registry/implementation-map.yaml:L2270-L2282] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 14 | [docs/registry/implementation-map.yaml:L2283-L2291] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 15 | [docs/registry/implementation-map.yaml:L2292-L2301] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 16 | [docs/registry/implementation-map.yaml:L2302-L2311] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 17 | [docs/registry/implementation-map.yaml:L2312-L2324] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 18 | [docs/registry/implementation-map.yaml:L2325-L2334] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 19 | [docs/registry/implementation-map.yaml:L2335-L2344] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 20 | [docs/registry/implementation-map.yaml:L2345-L2356] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 21 | [docs/registry/implementation-map.yaml:L2357-L2365] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 22 | [docs/registry/implementation-map.yaml:L2366-L2375] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 23 | [docs/registry/implementation-map.yaml:L2376-L2387] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 24 | [docs/registry/implementation-map.yaml:L2388-L2397] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 25 | [docs/registry/implementation-map.yaml:L2398-L2407] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 26 | [docs/registry/implementation-map.yaml:L2408-L2417] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 27 | [docs/registry/implementation-map.yaml:L2418-L2427] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 28 | [docs/registry/implementation-map.yaml:L2428-L2437] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 29 | [docs/registry/implementation-map.yaml:L2438-L2447] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 30 | [docs/registry/implementation-map.yaml:L2448-L2457] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 31 | [docs/registry/implementation-map.yaml:L2458-L2470] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 32 | [docs/registry/implementation-map.yaml:L2471-L2484] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 33 | [docs/registry/implementation-map.yaml:L2485-L2494] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 34 | [docs/registry/implementation-map.yaml:L2495-L2504] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 35 | [docs/registry/implementation-map.yaml:L2505-L2514] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 36 | [docs/registry/implementation-map.yaml:L2515-L2524] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 37 | [docs/registry/implementation-map.yaml:L2525-L2534] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 38 | [docs/registry/implementation-map.yaml:L2535-L2544] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 39 | [docs/registry/implementation-map.yaml:L2545-L2556] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 40 | [docs/registry/implementation-map.yaml:L2557-L2566] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 41 | [docs/registry/implementation-map.yaml:L2567-L2576] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 42 | [docs/registry/implementation-map.yaml:L2577-L2586] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 43 | [docs/registry/implementation-map.yaml:L2587-L2595] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 44 | [docs/registry/implementation-map.yaml:L2596-L2605] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 45 | [docs/registry/implementation-map.yaml:L2606-L2615] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 46 | [docs/registry/implementation-map.yaml:L2616-L2625] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 47 | [docs/registry/implementation-map.yaml:L2626-L2635] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 48 | [docs/registry/implementation-map.yaml:L2636-L2645] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 49 | [docs/registry/implementation-map.yaml:L2646-L2655] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 50 | [docs/registry/implementation-map.yaml:L2656-L2667] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 51 | [docs/registry/implementation-map.yaml:L2668-L2679] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 52 | [docs/registry/implementation-map.yaml:L2680-L2689] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 53 | [docs/registry/implementation-map.yaml:L2690-L2701] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 54 | [docs/registry/implementation-map.yaml:L2702-L2715] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 55 | [docs/registry/implementation-map.yaml:L2716-L2726] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 56 | [docs/registry/implementation-map.yaml:L2727-L2740] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 57 | [docs/registry/implementation-map.yaml:L2741-L2750] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 58 | [docs/registry/implementation-map.yaml:L2751-L2760] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 59 | [docs/registry/implementation-map.yaml:L2761-L2774] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 60 | [docs/registry/implementation-map.yaml:L2775-L2787] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 61 | [docs/registry/implementation-map.yaml:L2788-L2797] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 62 | [docs/registry/implementation-map.yaml:L2798-L2807] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 63 | [docs/registry/implementation-map.yaml:L2808-L2817] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 64 | [docs/registry/implementation-map.yaml:L2818-L2827] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 65 | [docs/registry/implementation-map.yaml:L2828-L2839] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 66 | [docs/registry/implementation-map.yaml:L2840-L2852] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 67 | [docs/registry/implementation-map.yaml:L2853-L2862] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 68 | [docs/registry/implementation-map.yaml:L2863-L2874] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 69 | [docs/registry/implementation-map.yaml:L2875-L2884] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 70 | [docs/registry/implementation-map.yaml:L2885-L2896] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 71 | [docs/registry/implementation-map.yaml:L2897-L2907] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 72 | [docs/registry/implementation-map.yaml:L2908-L2917] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 73 | [docs/registry/implementation-map.yaml:L2918-L2927] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 74 | [docs/registry/implementation-map.yaml:L2928-L2938] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 75 | [docs/registry/implementation-map.yaml:L2939-L2948] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 76 | [docs/registry/implementation-map.yaml:L2949-L2960] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 77 | [docs/registry/implementation-map.yaml:L2961-L2970] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 78 | [docs/registry/implementation-map.yaml:L2971-L2980] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 79 | [docs/registry/implementation-map.yaml:L2981-L2990] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 80 | [docs/registry/implementation-map.yaml:L2991-L3000] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 81 | [docs/registry/implementation-map.yaml:L3001-L3010] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 82 | [docs/registry/implementation-map.yaml:L3011-L3022] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 83 | [docs/registry/implementation-map.yaml:L3023-L3032] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 84 | [docs/registry/implementation-map.yaml:L3033-L3042] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 85 | [docs/registry/implementation-map.yaml:L3043-L3052] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 86 | [docs/registry/implementation-map.yaml:L3053-L3062] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 87 | [docs/registry/implementation-map.yaml:L3063-L3072] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 88 | [docs/registry/implementation-map.yaml:L3073-L3082] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 89 | [docs/registry/implementation-map.yaml:L3083-L3092] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 90 | [docs/registry/implementation-map.yaml:L3093-L3102] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 91 | [docs/registry/implementation-map.yaml:L3103-L3112] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 92 | [docs/registry/implementation-map.yaml:L3113-L3122] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 93 | [docs/registry/implementation-map.yaml:L3123-L3135] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 94 | [docs/registry/implementation-map.yaml:L3136-L3145] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 95 | [docs/registry/implementation-map.yaml:L3146-L3155] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 96 | [docs/registry/implementation-map.yaml:L3156-L3168] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 97 | [docs/registry/implementation-map.yaml:L3169-L3178] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 98 | [docs/registry/implementation-map.yaml:L3179-L3188] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 99 | [docs/registry/implementation-map.yaml:L3189-L3198] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 100 | [docs/registry/implementation-map.yaml:L3199-L3208] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 101 | [docs/registry/implementation-map.yaml:L3209-L3218] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 102 | [docs/registry/implementation-map.yaml:L3219-L3231] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 103 | [docs/registry/implementation-map.yaml:L3232-L3241] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 104 | [docs/registry/implementation-map.yaml:L3242-L3251] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 105 | [docs/registry/implementation-map.yaml:L3252-L3264] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 106 | [docs/registry/implementation-map.yaml:L3265-L3274] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 107 | [docs/registry/implementation-map.yaml:L3275-L3287] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 108 | [docs/registry/implementation-map.yaml:L3288-L3300] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 109 | [docs/registry/implementation-map.yaml:L3301-L3311] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 110 | [docs/registry/implementation-map.yaml:L3312-L3321] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 111 | [docs/registry/implementation-map.yaml:L3322-L3331] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 112 | [docs/registry/implementation-map.yaml:L3332-L3341] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 113 | [docs/registry/implementation-map.yaml:L3342-L3351] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 114 | [docs/registry/implementation-map.yaml:L3352-L3361] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 115 | [docs/registry/implementation-map.yaml:L3362-L3377] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 116 | [docs/registry/implementation-map.yaml:L3378-L3387] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 117 | [docs/registry/implementation-map.yaml:L3388-L3397] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 118 | [docs/registry/implementation-map.yaml:L3398-L3407] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 119 | [docs/registry/implementation-map.yaml:L3408-L3417] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 120 | [docs/registry/implementation-map.yaml:L3418-L3432] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 121 | [docs/registry/implementation-map.yaml:L3433-L3442] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 122 | [docs/registry/implementation-map.yaml:L3443-L3452] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 123 | [docs/registry/implementation-map.yaml:L3453-L3462] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 124 | [docs/registry/implementation-map.yaml:L3463-L3472] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 125 | [docs/registry/implementation-map.yaml:L3473-L3482] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 126 | [docs/registry/implementation-map.yaml:L3483-L3492] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 127 | [docs/registry/implementation-map.yaml:L3493-L3502] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 128 | [docs/registry/implementation-map.yaml:L3503-L3512] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 129 | [docs/registry/implementation-map.yaml:L3513-L3522] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 130 | [docs/registry/implementation-map.yaml:L3523-L3532] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 131 | [docs/registry/implementation-map.yaml:L3533-L3542] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 132 | [docs/registry/implementation-map.yaml:L3543-L3555] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 133 | [docs/registry/implementation-map.yaml:L3556-L3565] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 134 | [docs/registry/implementation-map.yaml:L3566-L3575] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 135 | [docs/registry/implementation-map.yaml:L3576-L3585] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 136 | [docs/registry/implementation-map.yaml:L3586-L3597] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 137 | [docs/registry/implementation-map.yaml:L3598-L3607] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 138 | [docs/registry/implementation-map.yaml:L3608-L3617] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 139 | [docs/registry/implementation-map.yaml:L3618-L3627] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 140 | [docs/registry/implementation-map.yaml:L3628-L3637] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 141 | [docs/registry/implementation-map.yaml:L3638-L3647] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 142 | [docs/registry/implementation-map.yaml:L3648-L3657] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 143 | [docs/registry/implementation-map.yaml:L3658-L3667] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 144 | [docs/registry/implementation-map.yaml:L3668-L3677] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 145 | [docs/registry/implementation-map.yaml:L3678-L3687] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 146 | [docs/registry/implementation-map.yaml:L3688-L3697] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 147 | [docs/registry/implementation-map.yaml:L3698-L3707] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 148 | [docs/registry/implementation-map.yaml:L3708-L3717] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 149 | [docs/registry/implementation-map.yaml:L3718-L3727] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 150 | [docs/registry/implementation-map.yaml:L3728-L3737] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 151 | [docs/registry/implementation-map.yaml:L3738-L3747] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 152 | [docs/registry/implementation-map.yaml:L3748-L3757] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 153 | [docs/registry/implementation-map.yaml:L3758-L3767] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 154 | [docs/registry/implementation-map.yaml:L3768-L3777] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 155 | [docs/registry/implementation-map.yaml:L3778-L3787] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 156 | [docs/registry/implementation-map.yaml:L3788-L3797] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 157 | [docs/registry/implementation-map.yaml:L3798-L3807] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 158 | [docs/registry/implementation-map.yaml:L3808-L3819] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 159 | [docs/registry/implementation-map.yaml:L3820-L3829] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 160 | [docs/registry/implementation-map.yaml:L3830-L3839] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 161 | [docs/registry/implementation-map.yaml:L3840-L3849] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 162 | [docs/registry/implementation-map.yaml:L3850-L3862] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 163 | [docs/registry/implementation-map.yaml:L3863-L3872] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 164 | [docs/registry/implementation-map.yaml:L3873-L3885] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 165 | [docs/registry/implementation-map.yaml:L3886-L3898] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 166 | [docs/registry/implementation-map.yaml:L3899-L3909] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 167 | [docs/registry/implementation-map.yaml:L3910-L3919] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 168 | [docs/registry/implementation-map.yaml:L3920-L3929] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 169 | [docs/registry/implementation-map.yaml:L3930-L3939] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 170 | [docs/registry/implementation-map.yaml:L3940-L3949] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 171 | [docs/registry/implementation-map.yaml:L3950-L3959] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 172 | [docs/registry/implementation-map.yaml:L3960-L3969] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 173 | [docs/registry/implementation-map.yaml:L3970-L3979] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 174 | [docs/registry/implementation-map.yaml:L3980-L3989] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 175 | [docs/registry/implementation-map.yaml:L3990-L3999] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 176 | [docs/registry/implementation-map.yaml:L4000-L4009] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 177 | [docs/registry/implementation-map.yaml:L4010-L4019] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 178 | [docs/registry/implementation-map.yaml:L4020-L4029] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 179 | [docs/registry/implementation-map.yaml:L4030-L4039] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 180 | [docs/registry/implementation-map.yaml:L4040-L4049] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 181 | [docs/registry/implementation-map.yaml:L4050-L4059] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 182 | [docs/registry/implementation-map.yaml:L4060-L4069] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 183 | [docs/registry/implementation-map.yaml:L4070-L4079] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 184 | [docs/registry/implementation-map.yaml:L4080-L4089] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 185 | [docs/registry/implementation-map.yaml:L4090-L4099] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 186 | [docs/registry/implementation-map.yaml:L4100-L4109] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 187 | [docs/registry/implementation-map.yaml:L4110-L4119] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 188 | [docs/registry/implementation-map.yaml:L4120-L4129] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 189 | [docs/registry/implementation-map.yaml:L4130-L4139] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 190 | [docs/registry/implementation-map.yaml:L4140-L4149] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 191 | [docs/registry/implementation-map.yaml:L4150-L4159] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 192 | [docs/registry/implementation-map.yaml:L4160-L4169] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 193 | [docs/registry/implementation-map.yaml:L4170-L4179] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 194 | [docs/registry/implementation-map.yaml:L4180-L4189] | `src/digitalmodel/subsea/catenary_riser/cli.py` — `simple_catenary` |
| effective-tension-riser-stack-up / 195 | [docs/registry/implementation-map.yaml:L4190-L4199] | `src/digitalmodel/subsea/catenary_riser/cli.py` — `lazy_wave_analysis` |
| effective-tension-riser-stack-up / 196 | [docs/registry/implementation-map.yaml:L4200-L4209] | `src/digitalmodel/subsea/catenary_riser/lazy_wave.py` — `LazyWaveAnalyzer.analyze` |
| effective-tension-riser-stack-up / 197 | [docs/registry/implementation-map.yaml:L4210-L4222] | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` — `SimpleCatenaryAnalyzer.analyze_riser` |
| effective-tension-riser-stack-up / 198 | [docs/registry/implementation-map.yaml:L4223-L4232] | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` — `SimpleCatenaryAnalyzer._solve_catenary_from_H` |
| effective-tension-riser-stack-up / 199 | [docs/registry/implementation-map.yaml:L4233-L4242] | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` — `SimpleCatenaryAnalyzer._solve_H_from_top_tension` |
| effective-tension-riser-stack-up / 200 | [docs/registry/implementation-map.yaml:L4243-L4252] | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` — `SimpleCatenaryAnalyzer.check_design_limits` |
| effective-tension-riser-stack-up / 201 | [docs/registry/implementation-map.yaml:L4253-L4262] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 202 | [docs/registry/implementation-map.yaml:L4263-L4272] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 203 | [docs/registry/implementation-map.yaml:L4273-L4285] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 204 | [docs/registry/implementation-map.yaml:L4286-L4295] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 205 | [docs/registry/implementation-map.yaml:L4296-L4307] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 206 | [docs/registry/implementation-map.yaml:L4308-L4317] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 207 | [docs/registry/implementation-map.yaml:L4318-L4327] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 208 | [docs/registry/implementation-map.yaml:L4328-L4337] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 209 | [docs/registry/implementation-map.yaml:L4338-L4347] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 210 | [docs/registry/implementation-map.yaml:L4348-L4362] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 211 | [docs/registry/implementation-map.yaml:L4363-L4372] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 212 | [docs/registry/implementation-map.yaml:L4373-L4382] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 213 | [docs/registry/implementation-map.yaml:L4383-L4392] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 214 | [docs/registry/implementation-map.yaml:L4393-L4402] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 215 | [docs/registry/implementation-map.yaml:L4403-L4412] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 216 | [docs/registry/implementation-map.yaml:L4413-L4422] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 217 | [docs/registry/implementation-map.yaml:L4423-L4432] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 218 | [docs/registry/implementation-map.yaml:L4433-L4444] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 219 | [docs/registry/implementation-map.yaml:L4445-L4454] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 220 | [docs/registry/implementation-map.yaml:L4455-L4464] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 221 | [docs/registry/implementation-map.yaml:L4465-L4476] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 222 | [docs/registry/implementation-map.yaml:L4477-L4486] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 223 | [docs/registry/implementation-map.yaml:L4487-L4496] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 224 | [docs/registry/implementation-map.yaml:L4497-L4506] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 225 | [docs/registry/implementation-map.yaml:L4507-L4516] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 226 | [docs/registry/implementation-map.yaml:L4517-L4526] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 227 | [docs/registry/implementation-map.yaml:L4527-L4536] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 228 | [docs/registry/implementation-map.yaml:L4537-L4545] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 229 | [docs/registry/implementation-map.yaml:L4546-L4557] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 230 | [docs/registry/implementation-map.yaml:L4558-L4567] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 231 | [docs/registry/implementation-map.yaml:L4568-L4578] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 232 | [docs/registry/implementation-map.yaml:L4579-L4589] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 233 | [docs/registry/implementation-map.yaml:L4590-L4599] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 234 | [docs/registry/implementation-map.yaml:L4600-L4609] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 235 | [docs/registry/implementation-map.yaml:L4610-L4623] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 236 | [docs/registry/implementation-map.yaml:L4624-L4634] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 237 | [docs/registry/implementation-map.yaml:L4635-L4644] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 238 | [docs/registry/implementation-map.yaml:L4645-L4654] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 239 | [docs/registry/implementation-map.yaml:L4655-L4666] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 240 | [docs/registry/implementation-map.yaml:L4667-L4676] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 241 | [docs/registry/implementation-map.yaml:L4677-L4686] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 242 | [docs/registry/implementation-map.yaml:L4687-L4696] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 243 | [docs/registry/implementation-map.yaml:L4697-L4706] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 244 | [docs/registry/implementation-map.yaml:L4707-L4716] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 245 | [docs/registry/implementation-map.yaml:L4717-L4726] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 246 | [docs/registry/implementation-map.yaml:L4727-L4736] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 247 | [docs/registry/implementation-map.yaml:L4737-L4746] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 248 | [docs/registry/implementation-map.yaml:L4747-L4756] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 249 | [docs/registry/implementation-map.yaml:L4757-L4766] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 250 | [docs/registry/implementation-map.yaml:L4767-L4776] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 251 | [docs/registry/implementation-map.yaml:L4777-L4786] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 252 | [docs/registry/implementation-map.yaml:L4787-L4796] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 253 | [docs/registry/implementation-map.yaml:L4797-L4806] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 254 | [docs/registry/implementation-map.yaml:L4807-L4816] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 255 | [docs/registry/implementation-map.yaml:L4817-L4826] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 256 | [docs/registry/implementation-map.yaml:L4827-L4836] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 257 | [docs/registry/implementation-map.yaml:L4837-L4846] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 258 | [docs/registry/implementation-map.yaml:L4847-L4856] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 259 | [docs/registry/implementation-map.yaml:L4857-L4865] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 260 | [docs/registry/implementation-map.yaml:L4866-L4875] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 261 | [docs/registry/implementation-map.yaml:L4876-L4887] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 262 | [docs/registry/implementation-map.yaml:L4888-L4897] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 263 | [docs/registry/implementation-map.yaml:L4898-L4907] | Outside package; see exact map record |
| effective-tension-riser-stack-up / 264 | [docs/registry/implementation-map.yaml:L4908-L4917] | Outside package; see exact map record |
| orcaflex-text-model-writers / 1 | [docs/registry/implementation-map.yaml:L4940-L4949] | Outside package; see exact map record |
| orcaflex-text-model-writers / 2 | [docs/registry/implementation-map.yaml:L4950-L4958] | Outside package; see exact map record |
| orcaflex-text-model-writers / 3 | [docs/registry/implementation-map.yaml:L4959-L4968] | Outside package; see exact map record |
| orcaflex-text-model-writers / 4 | [docs/registry/implementation-map.yaml:L4969-L4978] | Outside package; see exact map record |
| orcaflex-text-model-writers / 5 | [docs/registry/implementation-map.yaml:L4979-L4988] | Outside package; see exact map record |
| orcaflex-text-model-writers / 6 | [docs/registry/implementation-map.yaml:L4989-L4998] | Outside package; see exact map record |
| orcaflex-text-model-writers / 7 | [docs/registry/implementation-map.yaml:L4999-L5008] | Outside package; see exact map record |
| orcaflex-text-model-writers / 8 | [docs/registry/implementation-map.yaml:L5009-L5020] | Outside package; see exact map record |
| orcaflex-text-model-writers / 9 | [docs/registry/implementation-map.yaml:L5021-L5030] | Outside package; see exact map record |
| orcaflex-text-model-writers / 10 | [docs/registry/implementation-map.yaml:L5031-L5040] | Outside package; see exact map record |
| orcaflex-text-model-writers / 11 | [docs/registry/implementation-map.yaml:L5041-L5050] | Outside package; see exact map record |
| orcaflex-text-model-writers / 12 | [docs/registry/implementation-map.yaml:L5051-L5060] | Outside package; see exact map record |
| orcaflex-text-model-writers / 13 | [docs/registry/implementation-map.yaml:L5061-L5070] | Outside package; see exact map record |
| orcaflex-text-model-writers / 14 | [docs/registry/implementation-map.yaml:L5071-L5080] | Outside package; see exact map record |
| orcaflex-text-model-writers / 15 | [docs/registry/implementation-map.yaml:L5081-L5090] | Outside package; see exact map record |
| orcaflex-text-model-writers / 16 | [docs/registry/implementation-map.yaml:L5091-L5100] | Outside package; see exact map record |
| orcaflex-text-model-writers / 17 | [docs/registry/implementation-map.yaml:L5101-L5110] | Outside package; see exact map record |
| orcaflex-text-model-writers / 18 | [docs/registry/implementation-map.yaml:L5111-L5120] | Outside package; see exact map record |
| orcaflex-text-model-writers / 19 | [docs/registry/implementation-map.yaml:L5121-L5132] | Outside package; see exact map record |
| orcaflex-text-model-writers / 20 | [docs/registry/implementation-map.yaml:L5133-L5142] | Outside package; see exact map record |
| orcaflex-text-model-writers / 21 | [docs/registry/implementation-map.yaml:L5143-L5151] | Outside package; see exact map record |
| orcaflex-text-model-writers / 22 | [docs/registry/implementation-map.yaml:L5152-L5161] | Outside package; see exact map record |
| orcaflex-text-model-writers / 23 | [docs/registry/implementation-map.yaml:L5162-L5171] | Outside package; see exact map record |
| orcaflex-text-model-writers / 24 | [docs/registry/implementation-map.yaml:L5172-L5181] | Outside package; see exact map record |
| orcaflex-text-model-writers / 25 | [docs/registry/implementation-map.yaml:L5182-L5191] | Outside package; see exact map record |
| orcaflex-text-model-writers / 26 | [docs/registry/implementation-map.yaml:L5192-L5201] | Outside package; see exact map record |
| orcaflex-text-model-writers / 27 | [docs/registry/implementation-map.yaml:L5202-L5211] | Outside package; see exact map record |
| orcaflex-text-model-writers / 28 | [docs/registry/implementation-map.yaml:L5212-L5223] | Outside package; see exact map record |
| orcaflex-text-model-writers / 29 | [docs/registry/implementation-map.yaml:L5224-L5233] | Outside package; see exact map record |
| orcaflex-text-model-writers / 30 | [docs/registry/implementation-map.yaml:L5234-L5243] | Outside package; see exact map record |
| orcaflex-text-model-writers / 31 | [docs/registry/implementation-map.yaml:L5244-L5253] | Outside package; see exact map record |
| orcaflex-text-model-writers / 32 | [docs/registry/implementation-map.yaml:L5254-L5263] | Outside package; see exact map record |
| orcaflex-text-model-writers / 33 | [docs/registry/implementation-map.yaml:L5264-L5273] | Outside package; see exact map record |
| orcaflex-text-model-writers / 34 | [docs/registry/implementation-map.yaml:L5274-L5283] | Outside package; see exact map record |
| orcaflex-text-model-writers / 35 | [docs/registry/implementation-map.yaml:L5284-L5293] | Outside package; see exact map record |
| orcaflex-text-model-writers / 36 | [docs/registry/implementation-map.yaml:L5294-L5303] | Outside package; see exact map record |
| orcaflex-text-model-writers / 37 | [docs/registry/implementation-map.yaml:L5304-L5313] | Outside package; see exact map record |
| orcaflex-text-model-writers / 38 | [docs/registry/implementation-map.yaml:L5314-L5323] | Outside package; see exact map record |
| orcaflex-text-model-writers / 39 | [docs/registry/implementation-map.yaml:L5324-L5333] | Outside package; see exact map record |
| orcaflex-text-model-writers / 40 | [docs/registry/implementation-map.yaml:L5334-L5343] | Outside package; see exact map record |
| orcaflex-text-model-writers / 41 | [docs/registry/implementation-map.yaml:L5344-L5353] | Outside package; see exact map record |
| orcaflex-text-model-writers / 42 | [docs/registry/implementation-map.yaml:L5354-L5363] | Outside package; see exact map record |
| orcaflex-text-model-writers / 43 | [docs/registry/implementation-map.yaml:L5364-L5372] | Outside package; see exact map record |
| orcaflex-text-model-writers / 44 | [docs/registry/implementation-map.yaml:L5373-L5382] | Outside package; see exact map record |
| orcaflex-text-model-writers / 45 | [docs/registry/implementation-map.yaml:L5383-L5392] | Outside package; see exact map record |
| orcaflex-text-model-writers / 46 | [docs/registry/implementation-map.yaml:L5393-L5402] | Outside package; see exact map record |
| orcaflex-text-model-writers / 47 | [docs/registry/implementation-map.yaml:L5403-L5415] | Outside package; see exact map record |
| orcaflex-text-model-writers / 48 | [docs/registry/implementation-map.yaml:L5416-L5425] | Outside package; see exact map record |
| orcaflex-text-model-writers / 49 | [docs/registry/implementation-map.yaml:L5426-L5437] | Outside package; see exact map record |
| orcaflex-text-model-writers / 50 | [docs/registry/implementation-map.yaml:L5438-L5447] | Outside package; see exact map record |
| orcaflex-text-model-writers / 51 | [docs/registry/implementation-map.yaml:L5448-L5457] | Outside package; see exact map record |
| orcaflex-text-model-writers / 52 | [docs/registry/implementation-map.yaml:L5458-L5467] | Outside package; see exact map record |
| orcaflex-text-model-writers / 53 | [docs/registry/implementation-map.yaml:L5468-L5477] | Outside package; see exact map record |
| orcaflex-text-model-writers / 54 | [docs/registry/implementation-map.yaml:L5478-L5487] | Outside package; see exact map record |
| orcaflex-text-model-writers / 55 | [docs/registry/implementation-map.yaml:L5488-L5497] | Outside package; see exact map record |
| orcaflex-text-model-writers / 56 | [docs/registry/implementation-map.yaml:L5498-L5507] | Outside package; see exact map record |
| orcaflex-text-model-writers / 57 | [docs/registry/implementation-map.yaml:L5508-L5519] | Outside package; see exact map record |
| orcaflex-text-model-writers / 58 | [docs/registry/implementation-map.yaml:L5520-L5531] | Outside package; see exact map record |
| orcaflex-text-model-writers / 59 | [docs/registry/implementation-map.yaml:L5532-L5540] | Outside package; see exact map record |
| orcaflex-text-model-writers / 60 | [docs/registry/implementation-map.yaml:L5541-L5550] | Outside package; see exact map record |
| orcaflex-text-model-writers / 61 | [docs/registry/implementation-map.yaml:L5551-L5560] | Outside package; see exact map record |
| orcaflex-text-model-writers / 62 | [docs/registry/implementation-map.yaml:L5561-L5570] | Outside package; see exact map record |
| orcaflex-text-model-writers / 63 | [docs/registry/implementation-map.yaml:L5571-L5580] | Outside package; see exact map record |
| orcaflex-text-model-writers / 64 | [docs/registry/implementation-map.yaml:L5581-L5593] | Outside package; see exact map record |
| orcaflex-text-model-writers / 65 | [docs/registry/implementation-map.yaml:L5594-L5603] | Outside package; see exact map record |
| orcaflex-text-model-writers / 66 | [docs/registry/implementation-map.yaml:L5604-L5615] | Outside package; see exact map record |
| orcaflex-text-model-writers / 67 | [docs/registry/implementation-map.yaml:L5616-L5628] | Outside package; see exact map record |
| orcaflex-text-model-writers / 68 | [docs/registry/implementation-map.yaml:L5629-L5638] | Outside package; see exact map record |
| orcaflex-text-model-writers / 69 | [docs/registry/implementation-map.yaml:L5639-L5648] | Outside package; see exact map record |
| orcaflex-text-model-writers / 70 | [docs/registry/implementation-map.yaml:L5649-L5658] | Outside package; see exact map record |
| orcaflex-text-model-writers / 71 | [docs/registry/implementation-map.yaml:L5659-L5667] | Outside package; see exact map record |
| orcaflex-text-model-writers / 72 | [docs/registry/implementation-map.yaml:L5668-L5677] | Outside package; see exact map record |
| orcaflex-text-model-writers / 73 | [docs/registry/implementation-map.yaml:L5678-L5687] | Outside package; see exact map record |
| orcaflex-text-model-writers / 74 | [docs/registry/implementation-map.yaml:L5688-L5697] | Outside package; see exact map record |
| orcaflex-text-model-writers / 75 | [docs/registry/implementation-map.yaml:L5698-L5707] | Outside package; see exact map record |
| orcaflex-text-model-writers / 76 | [docs/registry/implementation-map.yaml:L5708-L5717] | Outside package; see exact map record |
| orcaflex-text-model-writers / 77 | [docs/registry/implementation-map.yaml:L5718-L5727] | Outside package; see exact map record |
| orcaflex-text-model-writers / 78 | [docs/registry/implementation-map.yaml:L5728-L5737] | Outside package; see exact map record |
| orcaflex-text-model-writers / 79 | [docs/registry/implementation-map.yaml:L5738-L5749] | Outside package; see exact map record |
| orcaflex-text-model-writers / 80 | [docs/registry/implementation-map.yaml:L5750-L5759] | Outside package; see exact map record |
| orcaflex-text-model-writers / 81 | [docs/registry/implementation-map.yaml:L5760-L5769] | Outside package; see exact map record |
| orcaflex-text-model-writers / 82 | [docs/registry/implementation-map.yaml:L5770-L5781] | Outside package; see exact map record |
| orcaflex-text-model-writers / 83 | [docs/registry/implementation-map.yaml:L5782-L5791] | Outside package; see exact map record |
| orcaflex-text-model-writers / 84 | [docs/registry/implementation-map.yaml:L5792-L5801] | Outside package; see exact map record |
| orcaflex-text-model-writers / 85 | [docs/registry/implementation-map.yaml:L5802-L5811] | Outside package; see exact map record |
| orcaflex-text-model-writers / 86 | [docs/registry/implementation-map.yaml:L5812-L5823] | Outside package; see exact map record |
| orcaflex-text-model-writers / 87 | [docs/registry/implementation-map.yaml:L5824-L5833] | Outside package; see exact map record |
| orcaflex-text-model-writers / 88 | [docs/registry/implementation-map.yaml:L5834-L5843] | Outside package; see exact map record |
| orcaflex-text-model-writers / 89 | [docs/registry/implementation-map.yaml:L5844-L5853] | Outside package; see exact map record |
| orcaflex-text-model-writers / 90 | [docs/registry/implementation-map.yaml:L5854-L5863] | Outside package; see exact map record |
| orcaflex-text-model-writers / 91 | [docs/registry/implementation-map.yaml:L5864-L5872] | Outside package; see exact map record |
| orcaflex-text-model-writers / 92 | [docs/registry/implementation-map.yaml:L5873-L5882] | Outside package; see exact map record |
| orcaflex-text-model-writers / 93 | [docs/registry/implementation-map.yaml:L5883-L5891] | Outside package; see exact map record |
| orcaflex-text-model-writers / 94 | [docs/registry/implementation-map.yaml:L5892-L5901] | Outside package; see exact map record |
| orcaflex-text-model-writers / 95 | [docs/registry/implementation-map.yaml:L5902-L5911] | Outside package; see exact map record |
| orcaflex-text-model-writers / 96 | [docs/registry/implementation-map.yaml:L5912-L5921] | Outside package; see exact map record |
| orcaflex-text-model-writers / 97 | [docs/registry/implementation-map.yaml:L5922-L5933] | Outside package; see exact map record |
| orcaflex-text-model-writers / 98 | [docs/registry/implementation-map.yaml:L5934-L5943] | Outside package; see exact map record |
| orcaflex-text-model-writers / 99 | [docs/registry/implementation-map.yaml:L5944-L5953] | Outside package; see exact map record |
| orcaflex-text-model-writers / 100 | [docs/registry/implementation-map.yaml:L5954-L5965] | Outside package; see exact map record |
| orcaflex-text-model-writers / 101 | [docs/registry/implementation-map.yaml:L5966-L5975] | Outside package; see exact map record |
| orcaflex-text-model-writers / 102 | [docs/registry/implementation-map.yaml:L5976-L5985] | Outside package; see exact map record |
| orcaflex-text-model-writers / 103 | [docs/registry/implementation-map.yaml:L5986-L5994] | Outside package; see exact map record |
| orcaflex-text-model-writers / 104 | [docs/registry/implementation-map.yaml:L5995-L6004] | Outside package; see exact map record |
| orcaflex-text-model-writers / 105 | [docs/registry/implementation-map.yaml:L6005-L6014] | Outside package; see exact map record |
| orcaflex-text-model-writers / 106 | [docs/registry/implementation-map.yaml:L6015-L6024] | Outside package; see exact map record |
| orcaflex-text-model-writers / 107 | [docs/registry/implementation-map.yaml:L6025-L6034] | Outside package; see exact map record |
| orcaflex-text-model-writers / 108 | [docs/registry/implementation-map.yaml:L6035-L6044] | Outside package; see exact map record |
| orcaflex-text-model-writers / 109 | [docs/registry/implementation-map.yaml:L6045-L6054] | Outside package; see exact map record |
| orcaflex-text-model-writers / 110 | [docs/registry/implementation-map.yaml:L6055-L6064] | Outside package; see exact map record |
| orcaflex-text-model-writers / 111 | [docs/registry/implementation-map.yaml:L6065-L6074] | Outside package; see exact map record |
| orcaflex-text-model-writers / 112 | [docs/registry/implementation-map.yaml:L6075-L6084] | Outside package; see exact map record |
| orcaflex-text-model-writers / 113 | [docs/registry/implementation-map.yaml:L6085-L6096] | Outside package; see exact map record |
| orcaflex-text-model-writers / 114 | [docs/registry/implementation-map.yaml:L6097-L6109] | Outside package; see exact map record |
| orcaflex-text-model-writers / 115 | [docs/registry/implementation-map.yaml:L6110-L6119] | Outside package; see exact map record |
| orcaflex-text-model-writers / 116 | [docs/registry/implementation-map.yaml:L6120-L6129] | Outside package; see exact map record |
| orcaflex-text-model-writers / 117 | [docs/registry/implementation-map.yaml:L6130-L6141] | Outside package; see exact map record |
| orcaflex-text-model-writers / 118 | [docs/registry/implementation-map.yaml:L6142-L6153] | Outside package; see exact map record |
| orcaflex-text-model-writers / 119 | [docs/registry/implementation-map.yaml:L6154-L6163] | Outside package; see exact map record |
| orcaflex-text-model-writers / 120 | [docs/registry/implementation-map.yaml:L6164-L6173] | Outside package; see exact map record |
| orcaflex-text-model-writers / 121 | [docs/registry/implementation-map.yaml:L6174-L6183] | Outside package; see exact map record |
| orcaflex-text-model-writers / 122 | [docs/registry/implementation-map.yaml:L6184-L6193] | Outside package; see exact map record |
| orcaflex-text-model-writers / 123 | [docs/registry/implementation-map.yaml:L6194-L6203] | Outside package; see exact map record |
| orcaflex-text-model-writers / 124 | [docs/registry/implementation-map.yaml:L6204-L6213] | Outside package; see exact map record |
| orcaflex-text-model-writers / 125 | [docs/registry/implementation-map.yaml:L6214-L6223] | Outside package; see exact map record |
| orcaflex-text-model-writers / 126 | [docs/registry/implementation-map.yaml:L6224-L6233] | Outside package; see exact map record |
| orcaflex-text-model-writers / 127 | [docs/registry/implementation-map.yaml:L6234-L6243] | Outside package; see exact map record |
| orcaflex-text-model-writers / 128 | [docs/registry/implementation-map.yaml:L6244-L6253] | Outside package; see exact map record |
| orcaflex-text-model-writers / 129 | [docs/registry/implementation-map.yaml:L6254-L6263] | Outside package; see exact map record |
| orcaflex-text-model-writers / 130 | [docs/registry/implementation-map.yaml:L6264-L6273] | Outside package; see exact map record |
| orcaflex-text-model-writers / 131 | [docs/registry/implementation-map.yaml:L6274-L6283] | Outside package; see exact map record |
| orcaflex-text-model-writers / 132 | [docs/registry/implementation-map.yaml:L6284-L6293] | Outside package; see exact map record |
| orcaflex-text-model-writers / 133 | [docs/registry/implementation-map.yaml:L6294-L6303] | Outside package; see exact map record |
| orcaflex-text-model-writers / 134 | [docs/registry/implementation-map.yaml:L6304-L6313] | Outside package; see exact map record |
| orcaflex-text-model-writers / 135 | [docs/registry/implementation-map.yaml:L6314-L6323] | Outside package; see exact map record |
| orcaflex-text-model-writers / 136 | [docs/registry/implementation-map.yaml:L6324-L6333] | Outside package; see exact map record |
| orcaflex-text-model-writers / 137 | [docs/registry/implementation-map.yaml:L6334-L6343] | Outside package; see exact map record |
| orcaflex-text-model-writers / 138 | [docs/registry/implementation-map.yaml:L6344-L6353] | Outside package; see exact map record |
| orcaflex-text-model-writers / 139 | [docs/registry/implementation-map.yaml:L6354-L6363] | Outside package; see exact map record |
| orcaflex-text-model-writers / 140 | [docs/registry/implementation-map.yaml:L6364-L6373] | Outside package; see exact map record |
| orcaflex-text-model-writers / 141 | [docs/registry/implementation-map.yaml:L6374-L6383] | Outside package; see exact map record |
| orcaflex-text-model-writers / 142 | [docs/registry/implementation-map.yaml:L6384-L6393] | Outside package; see exact map record |
| orcaflex-text-model-writers / 143 | [docs/registry/implementation-map.yaml:L6394-L6403] | Outside package; see exact map record |
| orcaflex-text-model-writers / 144 | [docs/registry/implementation-map.yaml:L6404-L6413] | Outside package; see exact map record |
| orcaflex-text-model-writers / 145 | [docs/registry/implementation-map.yaml:L6414-L6423] | Outside package; see exact map record |
| orcaflex-text-model-writers / 146 | [docs/registry/implementation-map.yaml:L6424-L6436] | Outside package; see exact map record |
| orcaflex-text-model-writers / 147 | [docs/registry/implementation-map.yaml:L6437-L6448] | Outside package; see exact map record |
| orcaflex-text-model-writers / 148 | [docs/registry/implementation-map.yaml:L6449-L6458] | Outside package; see exact map record |
| orcaflex-text-model-writers / 149 | [docs/registry/implementation-map.yaml:L6459-L6468] | Outside package; see exact map record |
| orcaflex-text-model-writers / 150 | [docs/registry/implementation-map.yaml:L6469-L6478] | Outside package; see exact map record |
| orcaflex-text-model-writers / 151 | [docs/registry/implementation-map.yaml:L6479-L6490] | Outside package; see exact map record |
| orcaflex-text-model-writers / 152 | [docs/registry/implementation-map.yaml:L6491-L6500] | Outside package; see exact map record |
| orcaflex-text-model-writers / 153 | [docs/registry/implementation-map.yaml:L6501-L6510] | Outside package; see exact map record |
| orcaflex-text-model-writers / 154 | [docs/registry/implementation-map.yaml:L6511-L6520] | Outside package; see exact map record |
| orcaflex-text-model-writers / 155 | [docs/registry/implementation-map.yaml:L6521-L6530] | Outside package; see exact map record |
| orcaflex-text-model-writers / 156 | [docs/registry/implementation-map.yaml:L6531-L6540] | Outside package; see exact map record |
| orcaflex-text-model-writers / 157 | [docs/registry/implementation-map.yaml:L6541-L6550] | Outside package; see exact map record |
| orcaflex-text-model-writers / 158 | [docs/registry/implementation-map.yaml:L6551-L6560] | Outside package; see exact map record |
| orcaflex-text-model-writers / 159 | [docs/registry/implementation-map.yaml:L6561-L6570] | Outside package; see exact map record |
| orcaflex-text-model-writers / 160 | [docs/registry/implementation-map.yaml:L6571-L6580] | Outside package; see exact map record |
| orcaflex-text-model-writers / 161 | [docs/registry/implementation-map.yaml:L6581-L6590] | Outside package; see exact map record |
| orcaflex-text-model-writers / 162 | [docs/registry/implementation-map.yaml:L6591-L6600] | Outside package; see exact map record |
| orcaflex-text-model-writers / 163 | [docs/registry/implementation-map.yaml:L6601-L6610] | Outside package; see exact map record |
| orcaflex-text-model-writers / 164 | [docs/registry/implementation-map.yaml:L6611-L6620] | Outside package; see exact map record |
| orcaflex-text-model-writers / 165 | [docs/registry/implementation-map.yaml:L6621-L6630] | Outside package; see exact map record |
| orcaflex-text-model-writers / 166 | [docs/registry/implementation-map.yaml:L6631-L6640] | Outside package; see exact map record |
| orcaflex-text-model-writers / 167 | [docs/registry/implementation-map.yaml:L6641-L6650] | Outside package; see exact map record |
| orcaflex-text-model-writers / 168 | [docs/registry/implementation-map.yaml:L6651-L6659] | Outside package; see exact map record |
| orcaflex-text-model-writers / 169 | [docs/registry/implementation-map.yaml:L6660-L6672] | Outside package; see exact map record |
| orcaflex-text-model-writers / 170 | [docs/registry/implementation-map.yaml:L6673-L6682] | Outside package; see exact map record |
| orcaflex-text-model-writers / 171 | [docs/registry/implementation-map.yaml:L6683-L6692] | Outside package; see exact map record |
| orcaflex-text-model-writers / 172 | [docs/registry/implementation-map.yaml:L6693-L6704] | Outside package; see exact map record |
| orcaflex-text-model-writers / 173 | [docs/registry/implementation-map.yaml:L6705-L6714] | Outside package; see exact map record |
| orcaflex-text-model-writers / 174 | [docs/registry/implementation-map.yaml:L6715-L6724] | Outside package; see exact map record |
| orcaflex-text-model-writers / 175 | [docs/registry/implementation-map.yaml:L6725-L6734] | Outside package; see exact map record |
| orcaflex-text-model-writers / 176 | [docs/registry/implementation-map.yaml:L6735-L6744] | Outside package; see exact map record |
| orcaflex-text-model-writers / 177 | [docs/registry/implementation-map.yaml:L6745-L6754] | Outside package; see exact map record |
| orcaflex-text-model-writers / 178 | [docs/registry/implementation-map.yaml:L6755-L6764] | Outside package; see exact map record |
| orcaflex-text-model-writers / 179 | [docs/registry/implementation-map.yaml:L6765-L6774] | Outside package; see exact map record |
| orcaflex-text-model-writers / 180 | [docs/registry/implementation-map.yaml:L6775-L6784] | Outside package; see exact map record |
| orcaflex-text-model-writers / 181 | [docs/registry/implementation-map.yaml:L6785-L6794] | Outside package; see exact map record |
| orcaflex-text-model-writers / 182 | [docs/registry/implementation-map.yaml:L6795-L6804] | Outside package; see exact map record |
| orcaflex-text-model-writers / 183 | [docs/registry/implementation-map.yaml:L6805-L6814] | Outside package; see exact map record |
| orcaflex-text-model-writers / 184 | [docs/registry/implementation-map.yaml:L6815-L6824] | Outside package; see exact map record |
| orcaflex-text-model-writers / 185 | [docs/registry/implementation-map.yaml:L6825-L6834] | Outside package; see exact map record |
| orcaflex-text-model-writers / 186 | [docs/registry/implementation-map.yaml:L6835-L6844] | Outside package; see exact map record |
| orcaflex-text-model-writers / 187 | [docs/registry/implementation-map.yaml:L6845-L6854] | Outside package; see exact map record |
| orcaflex-text-model-writers / 188 | [docs/registry/implementation-map.yaml:L6855-L6864] | Outside package; see exact map record |
| orcaflex-text-model-writers / 189 | [docs/registry/implementation-map.yaml:L6865-L6874] | Outside package; see exact map record |
| orcaflex-text-model-writers / 190 | [docs/registry/implementation-map.yaml:L6875-L6884] | Outside package; see exact map record |
| orcaflex-text-model-writers / 191 | [docs/registry/implementation-map.yaml:L6885-L6894] | Outside package; see exact map record |
| orcaflex-text-model-writers / 192 | [docs/registry/implementation-map.yaml:L6895-L6904] | Outside package; see exact map record |
| orcaflex-text-model-writers / 193 | [docs/registry/implementation-map.yaml:L6905-L6914] | Outside package; see exact map record |
| orcaflex-text-model-writers / 194 | [docs/registry/implementation-map.yaml:L6915-L6927] | Outside package; see exact map record |
| orcaflex-text-model-writers / 195 | [docs/registry/implementation-map.yaml:L6928-L6937] | Outside package; see exact map record |
| orcaflex-text-model-writers / 196 | [docs/registry/implementation-map.yaml:L6938-L6947] | Outside package; see exact map record |
| orcaflex-text-model-writers / 197 | [docs/registry/implementation-map.yaml:L6948-L6957] | Outside package; see exact map record |
| orcaflex-text-model-writers / 198 | [docs/registry/implementation-map.yaml:L6958-L6967] | Outside package; see exact map record |
| orcaflex-text-model-writers / 199 | [docs/registry/implementation-map.yaml:L6968-L6977] | Outside package; see exact map record |
| orcaflex-text-model-writers / 200 | [docs/registry/implementation-map.yaml:L6978-L6987] | Outside package; see exact map record |
| orcaflex-text-model-writers / 201 | [docs/registry/implementation-map.yaml:L6988-L6997] | Outside package; see exact map record |
| orcaflex-text-model-writers / 202 | [docs/registry/implementation-map.yaml:L6998-L7007] | Outside package; see exact map record |
| orcaflex-text-model-writers / 203 | [docs/registry/implementation-map.yaml:L7008-L7017] | Outside package; see exact map record |
| orcaflex-text-model-writers / 204 | [docs/registry/implementation-map.yaml:L7018-L7031] | Outside package; see exact map record |
| orcaflex-text-model-writers / 205 | [docs/registry/implementation-map.yaml:L7032-L7041] | Outside package; see exact map record |
| orcaflex-text-model-writers / 206 | [docs/registry/implementation-map.yaml:L7042-L7051] | Outside package; see exact map record |
| orcaflex-text-model-writers / 207 | [docs/registry/implementation-map.yaml:L7052-L7061] | Outside package; see exact map record |
| orcaflex-text-model-writers / 208 | [docs/registry/implementation-map.yaml:L7062-L7071] | Outside package; see exact map record |
| orcaflex-text-model-writers / 209 | [docs/registry/implementation-map.yaml:L7072-L7081] | Outside package; see exact map record |
| orcaflex-text-model-writers / 210 | [docs/registry/implementation-map.yaml:L7082-L7091] | Outside package; see exact map record |
| orcaflex-text-model-writers / 211 | [docs/registry/implementation-map.yaml:L7092-L7101] | Outside package; see exact map record |
| orcaflex-text-model-writers / 212 | [docs/registry/implementation-map.yaml:L7102-L7111] | Outside package; see exact map record |
| orcaflex-text-model-writers / 213 | [docs/registry/implementation-map.yaml:L7112-L7121] | Outside package; see exact map record |
| orcaflex-text-model-writers / 214 | [docs/registry/implementation-map.yaml:L7122-L7131] | Outside package; see exact map record |
| orcaflex-text-model-writers / 215 | [docs/registry/implementation-map.yaml:L7132-L7141] | Outside package; see exact map record |
| orcaflex-text-model-writers / 216 | [docs/registry/implementation-map.yaml:L7142-L7153] | Outside package; see exact map record |
| orcaflex-text-model-writers / 217 | [docs/registry/implementation-map.yaml:L7154-L7163] | Outside package; see exact map record |
| orcaflex-text-model-writers / 218 | [docs/registry/implementation-map.yaml:L7164-L7176] | Outside package; see exact map record |
| orcaflex-text-model-writers / 219 | [docs/registry/implementation-map.yaml:L7177-L7186] | Outside package; see exact map record |
| orcaflex-text-model-writers / 220 | [docs/registry/implementation-map.yaml:L7187-L7196] | Outside package; see exact map record |
| orcaflex-text-model-writers / 221 | [docs/registry/implementation-map.yaml:L7197-L7207] | Outside package; see exact map record |
| orcaflex-text-model-writers / 222 | [docs/registry/implementation-map.yaml:L7208-L7217] | Outside package; see exact map record |
| orcaflex-text-model-writers / 223 | [docs/registry/implementation-map.yaml:L7218-L7227] | Outside package; see exact map record |
| orcaflex-text-model-writers / 224 | [docs/registry/implementation-map.yaml:L7228-L7237] | Outside package; see exact map record |
| orcaflex-text-model-writers / 225 | [docs/registry/implementation-map.yaml:L7238-L7247] | Outside package; see exact map record |
| orcaflex-text-model-writers / 226 | [docs/registry/implementation-map.yaml:L7248-L7257] | Outside package; see exact map record |
| orcaflex-text-model-writers / 227 | [docs/registry/implementation-map.yaml:L7258-L7269] | Outside package; see exact map record |
| orcaflex-text-model-writers / 228 | [docs/registry/implementation-map.yaml:L7270-L7279] | Outside package; see exact map record |
| orcaflex-text-model-writers / 229 | [docs/registry/implementation-map.yaml:L7280-L7289] | Outside package; see exact map record |
| orcaflex-text-model-writers / 230 | [docs/registry/implementation-map.yaml:L7290-L7299] | Outside package; see exact map record |
| orcaflex-text-model-writers / 231 | [docs/registry/implementation-map.yaml:L7300-L7309] | Outside package; see exact map record |
| orcaflex-text-model-writers / 232 | [docs/registry/implementation-map.yaml:L7310-L7319] | Outside package; see exact map record |
| orcaflex-text-model-writers / 233 | [docs/registry/implementation-map.yaml:L7320-L7329] | Outside package; see exact map record |
| orcaflex-text-model-writers / 234 | [docs/registry/implementation-map.yaml:L7330-L7339] | Outside package; see exact map record |
| orcaflex-text-model-writers / 235 | [docs/registry/implementation-map.yaml:L7340-L7349] | Outside package; see exact map record |
| orcaflex-text-model-writers / 236 | [docs/registry/implementation-map.yaml:L7350-L7359] | Outside package; see exact map record |
| orcaflex-text-model-writers / 237 | [docs/registry/implementation-map.yaml:L7360-L7369] | Outside package; see exact map record |
| orcaflex-text-model-writers / 238 | [docs/registry/implementation-map.yaml:L7370-L7379] | Outside package; see exact map record |
| orcaflex-text-model-writers / 239 | [docs/registry/implementation-map.yaml:L7380-L7389] | Outside package; see exact map record |
| orcaflex-text-model-writers / 240 | [docs/registry/implementation-map.yaml:L7390-L7399] | Outside package; see exact map record |
| orcaflex-text-model-writers / 241 | [docs/registry/implementation-map.yaml:L7400-L7409] | Outside package; see exact map record |
| orcaflex-text-model-writers / 242 | [docs/registry/implementation-map.yaml:L7410-L7419] | Outside package; see exact map record |
| orcaflex-text-model-writers / 243 | [docs/registry/implementation-map.yaml:L7420-L7429] | Outside package; see exact map record |
| orcaflex-text-model-writers / 244 | [docs/registry/implementation-map.yaml:L7430-L7439] | Outside package; see exact map record |
| orcaflex-text-model-writers / 245 | [docs/registry/implementation-map.yaml:L7440-L7449] | Outside package; see exact map record |
| orcaflex-text-model-writers / 246 | [docs/registry/implementation-map.yaml:L7450-L7459] | Outside package; see exact map record |
| orcaflex-text-model-writers / 247 | [docs/registry/implementation-map.yaml:L7460-L7469] | Outside package; see exact map record |
| orcaflex-text-model-writers / 248 | [docs/registry/implementation-map.yaml:L7470-L7479] | Outside package; see exact map record |
| orcaflex-text-model-writers / 249 | [docs/registry/implementation-map.yaml:L7480-L7489] | Outside package; see exact map record |
| orcaflex-text-model-writers / 250 | [docs/registry/implementation-map.yaml:L7490-L7499] | Outside package; see exact map record |
| orcaflex-text-model-writers / 251 | [docs/registry/implementation-map.yaml:L7500-L7509] | Outside package; see exact map record |
| orcaflex-text-model-writers / 252 | [docs/registry/implementation-map.yaml:L7510-L7519] | Outside package; see exact map record |
| orcaflex-text-model-writers / 253 | [docs/registry/implementation-map.yaml:L7520-L7529] | Outside package; see exact map record |
| orcaflex-text-model-writers / 254 | [docs/registry/implementation-map.yaml:L7530-L7539] | Outside package; see exact map record |
| orcaflex-text-model-writers / 255 | [docs/registry/implementation-map.yaml:L7540-L7551] | Outside package; see exact map record |
| orcaflex-text-model-writers / 256 | [docs/registry/implementation-map.yaml:L7552-L7561] | Outside package; see exact map record |
| orcaflex-text-model-writers / 257 | [docs/registry/implementation-map.yaml:L7562-L7571] | Outside package; see exact map record |
| orcaflex-text-model-writers / 258 | [docs/registry/implementation-map.yaml:L7572-L7581] | Outside package; see exact map record |
| orcaflex-text-model-writers / 259 | [docs/registry/implementation-map.yaml:L7582-L7591] | Outside package; see exact map record |
| orcaflex-text-model-writers / 260 | [docs/registry/implementation-map.yaml:L7592-L7603] | Outside package; see exact map record |
| orcaflex-text-model-writers / 261 | [docs/registry/implementation-map.yaml:L7604-L7613] | Outside package; see exact map record |
| orcaflex-text-model-writers / 262 | [docs/registry/implementation-map.yaml:L7614-L7626] | Outside package; see exact map record |
| orcaflex-text-model-writers / 263 | [docs/registry/implementation-map.yaml:L7627-L7636] | Outside package; see exact map record |
| orcaflex-text-model-writers / 264 | [docs/registry/implementation-map.yaml:L7637-L7646] | Outside package; see exact map record |
| orcaflex-text-model-writers / 265 | [docs/registry/implementation-map.yaml:L7647-L7656] | Outside package; see exact map record |
| orcaflex-text-model-writers / 266 | [docs/registry/implementation-map.yaml:L7657-L7666] | Outside package; see exact map record |
| orcaflex-text-model-writers / 267 | [docs/registry/implementation-map.yaml:L7667-L7676] | Outside package; see exact map record |
| orcaflex-text-model-writers / 268 | [docs/registry/implementation-map.yaml:L7677-L7686] | Outside package; see exact map record |
| orcaflex-text-model-writers / 269 | [docs/registry/implementation-map.yaml:L7687-L7696] | Outside package; see exact map record |
| orcaflex-text-model-writers / 270 | [docs/registry/implementation-map.yaml:L7697-L7706] | Outside package; see exact map record |
| orcaflex-text-model-writers / 271 | [docs/registry/implementation-map.yaml:L7707-L7716] | Outside package; see exact map record |
| orcaflex-text-model-writers / 272 | [docs/registry/implementation-map.yaml:L7717-L7729] | Outside package; see exact map record |
| orcaflex-text-model-writers / 273 | [docs/registry/implementation-map.yaml:L7730-L7739] | Outside package; see exact map record |
| orcaflex-text-model-writers / 274 | [docs/registry/implementation-map.yaml:L7740-L7749] | Outside package; see exact map record |
| orcaflex-text-model-writers / 275 | [docs/registry/implementation-map.yaml:L7750-L7762] | Outside package; see exact map record |
| orcaflex-text-model-writers / 276 | [docs/registry/implementation-map.yaml:L7763-L7774] | Outside package; see exact map record |
| orcaflex-text-model-writers / 277 | [docs/registry/implementation-map.yaml:L7775-L7784] | Outside package; see exact map record |
| orcaflex-text-model-writers / 278 | [docs/registry/implementation-map.yaml:L7785-L7794] | Outside package; see exact map record |
| orcaflex-text-model-writers / 279 | [docs/registry/implementation-map.yaml:L7795-L7804] | Outside package; see exact map record |
| orcaflex-text-model-writers / 280 | [docs/registry/implementation-map.yaml:L7805-L7816] | Outside package; see exact map record |
| orcaflex-text-model-writers / 281 | [docs/registry/implementation-map.yaml:L7817-L7826] | Outside package; see exact map record |
| orcaflex-text-model-writers / 282 | [docs/registry/implementation-map.yaml:L7827-L7836] | Outside package; see exact map record |
| orcaflex-text-model-writers / 283 | [docs/registry/implementation-map.yaml:L7837-L7846] | Outside package; see exact map record |
| orcaflex-text-model-writers / 284 | [docs/registry/implementation-map.yaml:L7847-L7856] | Outside package; see exact map record |
| orcaflex-text-model-writers / 285 | [docs/registry/implementation-map.yaml:L7857-L7866] | Outside package; see exact map record |
| orcaflex-text-model-writers / 286 | [docs/registry/implementation-map.yaml:L7867-L7878] | Outside package; see exact map record |
| orcaflex-text-model-writers / 287 | [docs/registry/implementation-map.yaml:L7879-L7888] | Outside package; see exact map record |
| orcaflex-text-model-writers / 288 | [docs/registry/implementation-map.yaml:L7889-L7898] | Outside package; see exact map record |
| orcaflex-text-model-writers / 289 | [docs/registry/implementation-map.yaml:L7899-L7908] | Outside package; see exact map record |
| orcaflex-text-model-writers / 290 | [docs/registry/implementation-map.yaml:L7909-L7921] | Outside package; see exact map record |
| orcaflex-text-model-writers / 291 | [docs/registry/implementation-map.yaml:L7922-L7931] | Outside package; see exact map record |
| orcaflex-text-model-writers / 292 | [docs/registry/implementation-map.yaml:L7932-L7941] | Outside package; see exact map record |
| orcaflex-text-model-writers / 293 | [docs/registry/implementation-map.yaml:L7942-L7951] | Outside package; see exact map record |
| orcaflex-text-model-writers / 294 | [docs/registry/implementation-map.yaml:L7952-L7961] | Outside package; see exact map record |
| orcaflex-text-model-writers / 295 | [docs/registry/implementation-map.yaml:L7962-L7971] | Outside package; see exact map record |
| orcaflex-text-model-writers / 296 | [docs/registry/implementation-map.yaml:L7972-L7981] | Outside package; see exact map record |
| orcaflex-text-model-writers / 297 | [docs/registry/implementation-map.yaml:L7982-L7991] | Outside package; see exact map record |
| orcaflex-text-model-writers / 298 | [docs/registry/implementation-map.yaml:L7992-L8001] | Outside package; see exact map record |
| orcaflex-text-model-writers / 299 | [docs/registry/implementation-map.yaml:L8002-L8011] | Outside package; see exact map record |
| orcaflex-text-model-writers / 300 | [docs/registry/implementation-map.yaml:L8012-L8021] | Outside package; see exact map record |
| orcaflex-text-model-writers / 301 | [docs/registry/implementation-map.yaml:L8022-L8031] | Outside package; see exact map record |
| orcaflex-text-model-writers / 302 | [docs/registry/implementation-map.yaml:L8032-L8043] | Outside package; see exact map record |
| orcaflex-text-model-writers / 303 | [docs/registry/implementation-map.yaml:L8044-L8053] | Outside package; see exact map record |
| orcaflex-text-model-writers / 304 | [docs/registry/implementation-map.yaml:L8054-L8063] | Outside package; see exact map record |
| orcaflex-text-model-writers / 305 | [docs/registry/implementation-map.yaml:L8064-L8073] | Outside package; see exact map record |
| orcaflex-text-model-writers / 306 | [docs/registry/implementation-map.yaml:L8074-L8083] | Outside package; see exact map record |
| orcaflex-text-model-writers / 307 | [docs/registry/implementation-map.yaml:L8084-L8093] | Outside package; see exact map record |
| orcaflex-text-model-writers / 308 | [docs/registry/implementation-map.yaml:L8094-L8103] | Outside package; see exact map record |
| orcaflex-text-model-writers / 309 | [docs/registry/implementation-map.yaml:L8104-L8113] | Outside package; see exact map record |
| orcaflex-text-model-writers / 310 | [docs/registry/implementation-map.yaml:L8114-L8123] | Outside package; see exact map record |
| orcaflex-text-model-writers / 311 | [docs/registry/implementation-map.yaml:L8124-L8133] | Outside package; see exact map record |
| orcaflex-text-model-writers / 312 | [docs/registry/implementation-map.yaml:L8134-L8143] | Outside package; see exact map record |
| orcaflex-text-model-writers / 313 | [docs/registry/implementation-map.yaml:L8144-L8155] | Outside package; see exact map record |
| orcaflex-text-model-writers / 314 | [docs/registry/implementation-map.yaml:L8156-L8165] | Outside package; see exact map record |
| orcaflex-text-model-writers / 315 | [docs/registry/implementation-map.yaml:L8166-L8175] | Outside package; see exact map record |
| orcaflex-text-model-writers / 316 | [docs/registry/implementation-map.yaml:L8176-L8185] | Outside package; see exact map record |
| orcaflex-text-model-writers / 317 | [docs/registry/implementation-map.yaml:L8186-L8195] | Outside package; see exact map record |
| orcaflex-text-model-writers / 318 | [docs/registry/implementation-map.yaml:L8196-L8205] | Outside package; see exact map record |
| orcaflex-text-model-writers / 319 | [docs/registry/implementation-map.yaml:L8206-L8215] | Outside package; see exact map record |
| orcaflex-text-model-writers / 320 | [docs/registry/implementation-map.yaml:L8216-L8227] | Outside package; see exact map record |
| orcaflex-text-model-writers / 321 | [docs/registry/implementation-map.yaml:L8228-L8237] | Outside package; see exact map record |
| orcaflex-text-model-writers / 322 | [docs/registry/implementation-map.yaml:L8238-L8247] | Outside package; see exact map record |
| orcaflex-text-model-writers / 323 | [docs/registry/implementation-map.yaml:L8248-L8257] | Outside package; see exact map record |
| orcaflex-text-model-writers / 324 | [docs/registry/implementation-map.yaml:L8258-L8267] | Outside package; see exact map record |
| orcaflex-text-model-writers / 325 | [docs/registry/implementation-map.yaml:L8268-L8279] | `src/digitalmodel/subsea/catenary_riser/legacy/getFatigueLoading.py` — `prepareFatiguePostProcessing` |
| orcaflex-text-model-writers / 326 | [docs/registry/implementation-map.yaml:L8280-L8290] | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` — `orcaflexModel` |
| orcaflex-text-model-writers / 327 | [docs/registry/implementation-map.yaml:L8291-L8300] | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` — `lineTypes` |
| orcaflex-text-model-writers / 328 | [docs/registry/implementation-map.yaml:L8301-L8310] | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` — `WriteOrcaflexModel` |
| orcaflex-text-model-writers / 329 | [docs/registry/implementation-map.yaml:L8311-L8323] | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` — `build_model` |
| orcaflex-text-model-writers / 330 | [docs/registry/implementation-map.yaml:L8324-L8333] | Outside package; see exact map record |
| orcaflex-text-model-writers / 331 | [docs/registry/implementation-map.yaml:L8334-L8343] | Outside package; see exact map record |
| orcaflex-text-model-writers / 332 | [docs/registry/implementation-map.yaml:L8344-L8353] | Outside package; see exact map record |
| orcaflex-text-model-writers / 333 | [docs/registry/implementation-map.yaml:L8354-L8363] | Outside package; see exact map record |
| orcaflex-text-model-writers / 334 | [docs/registry/implementation-map.yaml:L8364-L8373] | Outside package; see exact map record |
| orcaflex-text-model-writers / 335 | [docs/registry/implementation-map.yaml:L8374-L8383] | Outside package; see exact map record |
| orcaflex-text-model-writers / 336 | [docs/registry/implementation-map.yaml:L8384-L8393] | Outside package; see exact map record |
| orcaflex-text-model-writers / 337 | [docs/registry/implementation-map.yaml:L8394-L8403] | Outside package; see exact map record |
| orcaflex-text-model-writers / 338 | [docs/registry/implementation-map.yaml:L8404-L8413] | Outside package; see exact map record |
| orcaflex-text-model-writers / 339 | [docs/registry/implementation-map.yaml:L8414-L8423] | Outside package; see exact map record |
| orcaflex-text-model-writers / 340 | [docs/registry/implementation-map.yaml:L8424-L8435] | Outside package; see exact map record |
| orcaflex-text-model-writers / 341 | [docs/registry/implementation-map.yaml:L8436-L8445] | Outside package; see exact map record |
| orcaflex-text-model-writers / 342 | [docs/registry/implementation-map.yaml:L8446-L8457] | Outside package; see exact map record |
| orcaflex-text-model-writers / 343 | [docs/registry/implementation-map.yaml:L8458-L8470] | Outside package; see exact map record |
| orcaflex-text-model-writers / 344 | [docs/registry/implementation-map.yaml:L8471-L8480] | Outside package; see exact map record |
| orcaflex-text-model-writers / 345 | [docs/registry/implementation-map.yaml:L8481-L8492] | Outside package; see exact map record |
| orcaflex-text-model-writers / 346 | [docs/registry/implementation-map.yaml:L8493-L8505] | Outside package; see exact map record |
| orcaflex-text-model-writers / 347 | [docs/registry/implementation-map.yaml:L8506-L8517] | Outside package; see exact map record |
| orcaflex-text-model-writers / 348 | [docs/registry/implementation-map.yaml:L8518-L8527] | Outside package; see exact map record |
| orcaflex-text-model-writers / 349 | [docs/registry/implementation-map.yaml:L8528-L8537] | Outside package; see exact map record |
| orcaflex-text-model-writers / 350 | [docs/registry/implementation-map.yaml:L8538-L8547] | Outside package; see exact map record |
| orcaflex-text-model-writers / 351 | [docs/registry/implementation-map.yaml:L8548-L8560] | Outside package; see exact map record |
| orcaflex-text-model-writers / 352 | [docs/registry/implementation-map.yaml:L8561-L8572] | Outside package; see exact map record |
| orcaflex-text-model-writers / 353 | [docs/registry/implementation-map.yaml:L8573-L8582] | Outside package; see exact map record |
| orcaflex-text-model-writers / 354 | [docs/registry/implementation-map.yaml:L8583-L8596] | Outside package; see exact map record |
| orcaflex-text-model-writers / 355 | [docs/registry/implementation-map.yaml:L8597-L8608] | Outside package; see exact map record |
| orcaflex-text-model-writers / 356 | [docs/registry/implementation-map.yaml:L8609-L8619] | Outside package; see exact map record |
| orcaflex-text-model-writers / 357 | [docs/registry/implementation-map.yaml:L8620-L8632] | Outside package; see exact map record |
| orcaflex-text-model-writers / 358 | [docs/registry/implementation-map.yaml:L8633-L8647] | Outside package; see exact map record |
| orcaflex-text-model-writers / 359 | [docs/registry/implementation-map.yaml:L8648-L8657] | Outside package; see exact map record |
| orcaflex-text-model-writers / 360 | [docs/registry/implementation-map.yaml:L8658-L8667] | Outside package; see exact map record |
| orcaflex-text-model-writers / 361 | [docs/registry/implementation-map.yaml:L8668-L8677] | Outside package; see exact map record |
| orcaflex-text-model-writers / 362 | [docs/registry/implementation-map.yaml:L8678-L8687] | Outside package; see exact map record |
| orcaflex-text-model-writers / 363 | [docs/registry/implementation-map.yaml:L8688-L8701] | Outside package; see exact map record |
| orcaflex-text-model-writers / 364 | [docs/registry/implementation-map.yaml:L8702-L8714] | Outside package; see exact map record |
| orcaflex-text-model-writers / 365 | [docs/registry/implementation-map.yaml:L8715-L8724] | Outside package; see exact map record |
| orcaflex-text-model-writers / 366 | [docs/registry/implementation-map.yaml:L8725-L8739] | Outside package; see exact map record |
| orcaflex-text-model-writers / 367 | [docs/registry/implementation-map.yaml:L8740-L8749] | Outside package; see exact map record |
| orcaflex-text-model-writers / 368 | [docs/registry/implementation-map.yaml:L8750-L8759] | Outside package; see exact map record |
| orcaflex-text-model-writers / 369 | [docs/registry/implementation-map.yaml:L8760-L8773] | Outside package; see exact map record |
| orcaflex-text-model-writers / 370 | [docs/registry/implementation-map.yaml:L8774-L8783] | Outside package; see exact map record |
| orcaflex-text-model-writers / 371 | [docs/registry/implementation-map.yaml:L8784-L8793] | Outside package; see exact map record |
| orcaflex-text-model-writers / 372 | [docs/registry/implementation-map.yaml:L8794-L8803] | Outside package; see exact map record |
| orcaflex-text-model-writers / 373 | [docs/registry/implementation-map.yaml:L8804-L8813] | Outside package; see exact map record |
| orcaflex-text-model-writers / 374 | [docs/registry/implementation-map.yaml:L8814-L8823] | Outside package; see exact map record |
| orcaflex-text-model-writers / 375 | [docs/registry/implementation-map.yaml:L8824-L8833] | Outside package; see exact map record |
| orcaflex-text-model-writers / 376 | [docs/registry/implementation-map.yaml:L8834-L8843] | Outside package; see exact map record |
| orcaflex-text-model-writers / 377 | [docs/registry/implementation-map.yaml:L8844-L8856] | Outside package; see exact map record |
| orcaflex-text-model-writers / 378 | [docs/registry/implementation-map.yaml:L8857-L8866] | Outside package; see exact map record |
| orcaflex-text-model-writers / 379 | [docs/registry/implementation-map.yaml:L8867-L8876] | Outside package; see exact map record |
| orcaflex-text-model-writers / 380 | [docs/registry/implementation-map.yaml:L8877-L8886] | Outside package; see exact map record |
| orcaflex-text-model-writers / 381 | [docs/registry/implementation-map.yaml:L8887-L8896] | Outside package; see exact map record |
| orcaflex-text-model-writers / 382 | [docs/registry/implementation-map.yaml:L8897-L8908] | Outside package; see exact map record |
| orcaflex-text-model-writers / 383 | [docs/registry/implementation-map.yaml:L8909-L8918] | Outside package; see exact map record |
| orcaflex-text-model-writers / 384 | [docs/registry/implementation-map.yaml:L8919-L8928] | Outside package; see exact map record |
| orcaflex-text-model-writers / 385 | [docs/registry/implementation-map.yaml:L8929-L8938] | Outside package; see exact map record |
| orcaflex-text-model-writers / 386 | [docs/registry/implementation-map.yaml:L8939-L8948] | Outside package; see exact map record |
| orcaflex-text-model-writers / 387 | [docs/registry/implementation-map.yaml:L8949-L8958] | Outside package; see exact map record |
| orcaflex-text-model-writers / 388 | [docs/registry/implementation-map.yaml:L8959-L8970] | Outside package; see exact map record |
| orcaflex-text-model-writers / 389 | [docs/registry/implementation-map.yaml:L8971-L8980] | Outside package; see exact map record |
| orcaflex-text-model-writers / 390 | [docs/registry/implementation-map.yaml:L8981-L8990] | Outside package; see exact map record |
| orcaflex-text-model-writers / 391 | [docs/registry/implementation-map.yaml:L8991-L9000] | Outside package; see exact map record |
| orcaflex-text-model-writers / 392 | [docs/registry/implementation-map.yaml:L9001-L9010] | Outside package; see exact map record |
| orcaflex-text-model-writers / 393 | [docs/registry/implementation-map.yaml:L9011-L9020] | Outside package; see exact map record |
| orcaflex-text-model-writers / 394 | [docs/registry/implementation-map.yaml:L9021-L9030] | Outside package; see exact map record |
| orcaflex-text-model-writers / 395 | [docs/registry/implementation-map.yaml:L9031-L9040] | Outside package; see exact map record |
| orcaflex-text-model-writers / 396 | [docs/registry/implementation-map.yaml:L9041-L9050] | Outside package; see exact map record |
| orcaflex-text-model-writers / 397 | [docs/registry/implementation-map.yaml:L9051-L9060] | Outside package; see exact map record |
| orcaflex-text-model-writers / 398 | [docs/registry/implementation-map.yaml:L9061-L9070] | Outside package; see exact map record |
| orcaflex-text-model-writers / 399 | [docs/registry/implementation-map.yaml:L9071-L9080] | Outside package; see exact map record |
| orcaflex-text-model-writers / 400 | [docs/registry/implementation-map.yaml:L9081-L9090] | Outside package; see exact map record |
| orcaflex-text-model-writers / 401 | [docs/registry/implementation-map.yaml:L9091-L9100] | Outside package; see exact map record |
| orcaflex-text-model-writers / 402 | [docs/registry/implementation-map.yaml:L9101-L9110] | Outside package; see exact map record |
| orcaflex-text-model-writers / 403 | [docs/registry/implementation-map.yaml:L9111-L9120] | Outside package; see exact map record |
| orcaflex-text-model-writers / 404 | [docs/registry/implementation-map.yaml:L9121-L9130] | Outside package; see exact map record |
| orcaflex-text-model-writers / 405 | [docs/registry/implementation-map.yaml:L9131-L9140] | Outside package; see exact map record |
| orcaflex-text-model-writers / 406 | [docs/registry/implementation-map.yaml:L9141-L9152] | Outside package; see exact map record |
| orcaflex-text-model-writers / 407 | [docs/registry/implementation-map.yaml:L9153-L9162] | Outside package; see exact map record |
| orcaflex-text-model-writers / 408 | [docs/registry/implementation-map.yaml:L9163-L9172] | Outside package; see exact map record |
| orcaflex-text-model-writers / 409 | [docs/registry/implementation-map.yaml:L9173-L9182] | Outside package; see exact map record |
| orcaflex-text-model-writers / 410 | [docs/registry/implementation-map.yaml:L9183-L9192] | Outside package; see exact map record |
| orcaflex-text-model-writers / 411 | [docs/registry/implementation-map.yaml:L9193-L9202] | Outside package; see exact map record |
| orcaflex-text-model-writers / 412 | [docs/registry/implementation-map.yaml:L9203-L9212] | Outside package; see exact map record |
| orcaflex-text-model-writers / 413 | [docs/registry/implementation-map.yaml:L9213-L9222] | Outside package; see exact map record |
| orcaflex-text-model-writers / 414 | [docs/registry/implementation-map.yaml:L9223-L9232] | Outside package; see exact map record |
| orcaflex-text-model-writers / 415 | [docs/registry/implementation-map.yaml:L9233-L9242] | Outside package; see exact map record |
| orcaflex-text-model-writers / 416 | [docs/registry/implementation-map.yaml:L9243-L9252] | Outside package; see exact map record |
| orcaflex-text-model-writers / 417 | [docs/registry/implementation-map.yaml:L9253-L9262] | Outside package; see exact map record |
| orcaflex-text-model-writers / 418 | [docs/registry/implementation-map.yaml:L9263-L9272] | Outside package; see exact map record |
| orcaflex-text-model-writers / 419 | [docs/registry/implementation-map.yaml:L9273-L9282] | Outside package; see exact map record |
| orcaflex-text-model-writers / 420 | [docs/registry/implementation-map.yaml:L9283-L9292] | Outside package; see exact map record |
| orcaflex-text-model-writers / 421 | [docs/registry/implementation-map.yaml:L9293-L9302] | Outside package; see exact map record |
| orcaflex-text-model-writers / 422 | [docs/registry/implementation-map.yaml:L9303-L9312] | Outside package; see exact map record |
| orcaflex-text-model-writers / 423 | [docs/registry/implementation-map.yaml:L9313-L9322] | Outside package; see exact map record |
| orcaflex-text-model-writers / 424 | [docs/registry/implementation-map.yaml:L9323-L9332] | Outside package; see exact map record |
| orcaflex-text-model-writers / 425 | [docs/registry/implementation-map.yaml:L9333-L9342] | Outside package; see exact map record |
| orcaflex-text-model-writers / 426 | [docs/registry/implementation-map.yaml:L9343-L9352] | Outside package; see exact map record |
| orcaflex-text-model-writers / 427 | [docs/registry/implementation-map.yaml:L9353-L9362] | Outside package; see exact map record |
| orcaflex-text-model-writers / 428 | [docs/registry/implementation-map.yaml:L9363-L9372] | Outside package; see exact map record |
| orcaflex-text-model-writers / 429 | [docs/registry/implementation-map.yaml:L9373-L9382] | Outside package; see exact map record |
| orcaflex-text-model-writers / 430 | [docs/registry/implementation-map.yaml:L9383-L9392] | Outside package; see exact map record |
| orcaflex-text-model-writers / 431 | [docs/registry/implementation-map.yaml:L9393-L9402] | Outside package; see exact map record |
| orcaflex-text-model-writers / 432 | [docs/registry/implementation-map.yaml:L9403-L9412] | Outside package; see exact map record |
| orcaflex-text-model-writers / 433 | [docs/registry/implementation-map.yaml:L9413-L9422] | Outside package; see exact map record |
| orcaflex-text-model-writers / 434 | [docs/registry/implementation-map.yaml:L9423-L9432] | Outside package; see exact map record |
| orcaflex-text-model-writers / 435 | [docs/registry/implementation-map.yaml:L9433-L9442] | Outside package; see exact map record |
| orcaflex-text-model-writers / 436 | [docs/registry/implementation-map.yaml:L9443-L9452] | Outside package; see exact map record |
| orcaflex-text-model-writers / 437 | [docs/registry/implementation-map.yaml:L9453-L9462] | Outside package; see exact map record |
| orcaflex-text-model-writers / 438 | [docs/registry/implementation-map.yaml:L9463-L9472] | Outside package; see exact map record |
| orcaflex-text-model-writers / 439 | [docs/registry/implementation-map.yaml:L9473-L9482] | Outside package; see exact map record |
| orcaflex-text-model-writers / 440 | [docs/registry/implementation-map.yaml:L9483-L9493] | Outside package; see exact map record |
| wave-spectra / 1 | [docs/registry/implementation-map.yaml:L13600-L13611] | Outside package; see exact map record |
| wave-spectra / 2 | [docs/registry/implementation-map.yaml:L13612-L13623] | Outside package; see exact map record |
| wave-spectra / 3 | [docs/registry/implementation-map.yaml:L13624-L13635] | Outside package; see exact map record |
| wave-spectra / 4 | [docs/registry/implementation-map.yaml:L13636-L13647] | Outside package; see exact map record |
| wave-spectra / 5 | [docs/registry/implementation-map.yaml:L13648-L13662] | Outside package; see exact map record |
| wave-spectra / 6 | [docs/registry/implementation-map.yaml:L13663-L13674] | Outside package; see exact map record |
| wave-spectra / 7 | [docs/registry/implementation-map.yaml:L13675-L13686] | Outside package; see exact map record |
| wave-spectra / 8 | [docs/registry/implementation-map.yaml:L13687-L13697] | Outside package; see exact map record |
| wave-spectra / 9 | [docs/registry/implementation-map.yaml:L13698-L13708] | Outside package; see exact map record |
| wave-spectra / 10 | [docs/registry/implementation-map.yaml:L13709-L13720] | Outside package; see exact map record |
| wave-spectra / 11 | [docs/registry/implementation-map.yaml:L13721-L13732] | Outside package; see exact map record |
| wave-spectra / 12 | [docs/registry/implementation-map.yaml:L13733-L13746] | Outside package; see exact map record |
| wave-spectra / 13 | [docs/registry/implementation-map.yaml:L13747-L13757] | Outside package; see exact map record |
| wave-spectra / 14 | [docs/registry/implementation-map.yaml:L13758-L13768] | Outside package; see exact map record |
| wave-spectra / 15 | [docs/registry/implementation-map.yaml:L13769-L13779] | Outside package; see exact map record |
| wave-spectra / 16 | [docs/registry/implementation-map.yaml:L13780-L13790] | Outside package; see exact map record |
| wave-spectra / 17 | [docs/registry/implementation-map.yaml:L13791-L13801] | Outside package; see exact map record |
| wave-spectra / 18 | [docs/registry/implementation-map.yaml:L13802-L13812] | Outside package; see exact map record |
| wave-spectra / 19 | [docs/registry/implementation-map.yaml:L13813-L13823] | Outside package; see exact map record |
| wave-spectra / 20 | [docs/registry/implementation-map.yaml:L13824-L13834] | Outside package; see exact map record |
| wave-spectra / 21 | [docs/registry/implementation-map.yaml:L13835-L13845] | Outside package; see exact map record |
| wave-spectra / 22 | [docs/registry/implementation-map.yaml:L13846-L13856] | Outside package; see exact map record |
| wave-spectra / 23 | [docs/registry/implementation-map.yaml:L13857-L13867] | Outside package; see exact map record |
| wave-spectra / 24 | [docs/registry/implementation-map.yaml:L13868-L13878] | Outside package; see exact map record |
| wave-spectra / 25 | [docs/registry/implementation-map.yaml:L13879-L13890] | Outside package; see exact map record |
| wave-spectra / 26 | [docs/registry/implementation-map.yaml:L13891-L13902] | Outside package; see exact map record |
| wave-spectra / 27 | [docs/registry/implementation-map.yaml:L13903-L13914] | Outside package; see exact map record |
| wave-spectra / 28 | [docs/registry/implementation-map.yaml:L13915-L13926] | Outside package; see exact map record |
| wave-spectra / 29 | [docs/registry/implementation-map.yaml:L13927-L13938] | Outside package; see exact map record |
| wave-spectra / 30 | [docs/registry/implementation-map.yaml:L13939-L13950] | Outside package; see exact map record |
| wave-spectra / 31 | [docs/registry/implementation-map.yaml:L13951-L13964] | Outside package; see exact map record |
| wave-spectra / 32 | [docs/registry/implementation-map.yaml:L13965-L13976] | Outside package; see exact map record |
| wave-spectra / 33 | [docs/registry/implementation-map.yaml:L13977-L13988] | Outside package; see exact map record |
| wave-spectra / 34 | [docs/registry/implementation-map.yaml:L13989-L14004] | Outside package; see exact map record |
| wave-spectra / 35 | [docs/registry/implementation-map.yaml:L14005-L14017] | Outside package; see exact map record |
| wave-spectra / 36 | [docs/registry/implementation-map.yaml:L14018-L14030] | Outside package; see exact map record |
| wave-spectra / 37 | [docs/registry/implementation-map.yaml:L14031-L14043] | Outside package; see exact map record |
| wave-spectra / 38 | [docs/registry/implementation-map.yaml:L14044-L14060] | Outside package; see exact map record |
| wave-spectra / 39 | [docs/registry/implementation-map.yaml:L14061-L14072] | Outside package; see exact map record |
| wave-spectra / 40 | [docs/registry/implementation-map.yaml:L14073-L14084] | Outside package; see exact map record |
| wave-spectra / 41 | [docs/registry/implementation-map.yaml:L14085-L14096] | Outside package; see exact map record |
| wave-spectra / 42 | [docs/registry/implementation-map.yaml:L14097-L14110] | Outside package; see exact map record |
| wave-spectra / 43 | [docs/registry/implementation-map.yaml:L14111-L14122] | Outside package; see exact map record |
| wave-spectra / 44 | [docs/registry/implementation-map.yaml:L14123-L14134] | Outside package; see exact map record |
| wave-spectra / 45 | [docs/registry/implementation-map.yaml:L14135-L14146] | Outside package; see exact map record |
| wave-spectra / 46 | [docs/registry/implementation-map.yaml:L14147-L14158] | Outside package; see exact map record |
| wave-spectra / 47 | [docs/registry/implementation-map.yaml:L14159-L14172] | Outside package; see exact map record |
| wave-spectra / 48 | [docs/registry/implementation-map.yaml:L14173-L14186] | Outside package; see exact map record |
| wave-spectra / 49 | [docs/registry/implementation-map.yaml:L14187-L14199] | Outside package; see exact map record |
| wave-spectra / 50 | [docs/registry/implementation-map.yaml:L14200-L14213] | Outside package; see exact map record |
| wave-spectra / 51 | [docs/registry/implementation-map.yaml:L14214-L14225] | Outside package; see exact map record |
| wave-spectra / 52 | [docs/registry/implementation-map.yaml:L14226-L14237] | Outside package; see exact map record |
| wave-spectra / 53 | [docs/registry/implementation-map.yaml:L14238-L14249] | Outside package; see exact map record |
| wave-spectra / 54 | [docs/registry/implementation-map.yaml:L14250-L14261] | Outside package; see exact map record |
| wave-spectra / 55 | [docs/registry/implementation-map.yaml:L14262-L14275] | Outside package; see exact map record |
| wave-spectra / 56 | [docs/registry/implementation-map.yaml:L14276-L14287] | Outside package; see exact map record |
| wave-spectra / 57 | [docs/registry/implementation-map.yaml:L14288-L14301] | Outside package; see exact map record |
| wave-spectra / 58 | [docs/registry/implementation-map.yaml:L14302-L14315] | Outside package; see exact map record |
| wave-spectra / 59 | [docs/registry/implementation-map.yaml:L14316-L14328] | Outside package; see exact map record |
| wave-spectra / 60 | [docs/registry/implementation-map.yaml:L14329-L14342] | Outside package; see exact map record |
| wave-spectra / 61 | [docs/registry/implementation-map.yaml:L14343-L14356] | Outside package; see exact map record |
| wave-spectra / 62 | [docs/registry/implementation-map.yaml:L14357-L14368] | Outside package; see exact map record |
| wave-spectra / 63 | [docs/registry/implementation-map.yaml:L14369-L14380] | Outside package; see exact map record |
| wave-spectra / 64 | [docs/registry/implementation-map.yaml:L14381-L14392] | Outside package; see exact map record |
| wave-spectra / 65 | [docs/registry/implementation-map.yaml:L14393-L14408] | Outside package; see exact map record |
| wave-spectra / 66 | [docs/registry/implementation-map.yaml:L14409-L14421] | Outside package; see exact map record |
| wave-spectra / 67 | [docs/registry/implementation-map.yaml:L14422-L14433] | Outside package; see exact map record |
| wave-spectra / 68 | [docs/registry/implementation-map.yaml:L14434-L14445] | Outside package; see exact map record |
| wave-spectra / 69 | [docs/registry/implementation-map.yaml:L14446-L14459] | Outside package; see exact map record |
| wave-spectra / 70 | [docs/registry/implementation-map.yaml:L14460-L14477] | Outside package; see exact map record |
| wave-spectra / 71 | [docs/registry/implementation-map.yaml:L14478-L14489] | Outside package; see exact map record |
| wave-spectra / 72 | [docs/registry/implementation-map.yaml:L14490-L14501] | Outside package; see exact map record |
| wave-spectra / 73 | [docs/registry/implementation-map.yaml:L14502-L14513] | Outside package; see exact map record |
| wave-spectra / 74 | [docs/registry/implementation-map.yaml:L14514-L14525] | Outside package; see exact map record |
| wave-spectra / 75 | [docs/registry/implementation-map.yaml:L14526-L14539] | Outside package; see exact map record |
| wave-spectra / 76 | [docs/registry/implementation-map.yaml:L14540-L14551] | Outside package; see exact map record |
| wave-spectra / 77 | [docs/registry/implementation-map.yaml:L14552-L14563] | Outside package; see exact map record |
| wave-spectra / 78 | [docs/registry/implementation-map.yaml:L14564-L14577] | Outside package; see exact map record |
| wave-spectra / 79 | [docs/registry/implementation-map.yaml:L14578-L14591] | Outside package; see exact map record |
| wave-spectra / 80 | [docs/registry/implementation-map.yaml:L14592-L14605] | Outside package; see exact map record |
| wave-spectra / 81 | [docs/registry/implementation-map.yaml:L14606-L14617] | Outside package; see exact map record |
| wave-spectra / 82 | [docs/registry/implementation-map.yaml:L14618-L14629] | Outside package; see exact map record |
| wave-spectra / 83 | [docs/registry/implementation-map.yaml:L14630-L14641] | Outside package; see exact map record |
| wave-spectra / 84 | [docs/registry/implementation-map.yaml:L14642-L14655] | Outside package; see exact map record |
| wave-spectra / 85 | [docs/registry/implementation-map.yaml:L14656-L14667] | Outside package; see exact map record |
| wave-spectra / 86 | [docs/registry/implementation-map.yaml:L14668-L14679] | Outside package; see exact map record |
| wave-spectra / 87 | [docs/registry/implementation-map.yaml:L14680-L14691] | Outside package; see exact map record |
| wave-spectra / 88 | [docs/registry/implementation-map.yaml:L14692-L14703] | Outside package; see exact map record |
| wave-spectra / 89 | [docs/registry/implementation-map.yaml:L14704-L14715] | Outside package; see exact map record |
| wave-spectra / 90 | [docs/registry/implementation-map.yaml:L14716-L14727] | Outside package; see exact map record |
| wave-spectra / 91 | [docs/registry/implementation-map.yaml:L14728-L14739] | Outside package; see exact map record |
| wave-spectra / 92 | [docs/registry/implementation-map.yaml:L14740-L14751] | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` — `orcaflexModel` |
| wave-spectra / 93 | [docs/registry/implementation-map.yaml:L14752-L14763] | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` — `wave` |
| wave-spectra / 94 | [docs/registry/implementation-map.yaml:L14764-L14775] | Outside package; see exact map record |
| wave-spectra / 95 | [docs/registry/implementation-map.yaml:L14776-L14791] | Outside package; see exact map record |
| wave-spectra / 96 | [docs/registry/implementation-map.yaml:L14792-L14806] | Outside package; see exact map record |
| wave-spectra / 97 | [docs/registry/implementation-map.yaml:L14807-L14818] | Outside package; see exact map record |
| wave-spectra / 98 | [docs/registry/implementation-map.yaml:L14819-L14830] | Outside package; see exact map record |
| wave-spectra / 99 | [docs/registry/implementation-map.yaml:L14831-L14842] | Outside package; see exact map record |
| wave-spectra / 100 | [docs/registry/implementation-map.yaml:L14843-L14854] | Outside package; see exact map record |
| wave-spectra / 101 | [docs/registry/implementation-map.yaml:L14855-L14866] | Outside package; see exact map record |
| wave-spectra / 102 | [docs/registry/implementation-map.yaml:L14867-L14878] | Outside package; see exact map record |
| wave-spectra / 103 | [docs/registry/implementation-map.yaml:L14879-L14892] | Outside package; see exact map record |
| wave-spectra / 104 | [docs/registry/implementation-map.yaml:L14893-L14906] | Outside package; see exact map record |
| wave-spectra / 105 | [docs/registry/implementation-map.yaml:L14907-L14920] | Outside package; see exact map record |
| wave-spectra / 106 | [docs/registry/implementation-map.yaml:L14921-L14932] | Outside package; see exact map record |
| wave-spectra / 107 | [docs/registry/implementation-map.yaml:L14933-L14944] | Outside package; see exact map record |
| wave-spectra / 108 | [docs/registry/implementation-map.yaml:L14945-L14956] | Outside package; see exact map record |
| wave-spectra / 109 | [docs/registry/implementation-map.yaml:L14957-L14968] | Outside package; see exact map record |
| wave-spectra / 110 | [docs/registry/implementation-map.yaml:L14969-L14980] | Outside package; see exact map record |
| wave-spectra / 111 | [docs/registry/implementation-map.yaml:L14981-L14992] | Outside package; see exact map record |
| wave-spectra / 112 | [docs/registry/implementation-map.yaml:L14993-L15004] | Outside package; see exact map record |
| wave-spectra / 113 | [docs/registry/implementation-map.yaml:L15005-L15016] | Outside package; see exact map record |
| wave-spectra / 114 | [docs/registry/implementation-map.yaml:L15017-L15028] | Outside package; see exact map record |
| wave-spectra / 115 | [docs/registry/implementation-map.yaml:L15029-L15040] | Outside package; see exact map record |
| wave-spectra / 116 | [docs/registry/implementation-map.yaml:L15041-L15052] | Outside package; see exact map record |
| wave-spectra / 117 | [docs/registry/implementation-map.yaml:L15053-L15064] | Outside package; see exact map record |
| wave-spectra / 118 | [docs/registry/implementation-map.yaml:L15065-L15076] | Outside package; see exact map record |
| wave-spectra / 119 | [docs/registry/implementation-map.yaml:L15077-L15088] | Outside package; see exact map record |
| wave-spectra / 120 | [docs/registry/implementation-map.yaml:L15089-L15100] | Outside package; see exact map record |
| wave-spectra / 121 | [docs/registry/implementation-map.yaml:L15101-L15112] | Outside package; see exact map record |
| wave-spectra / 122 | [docs/registry/implementation-map.yaml:L15113-L15124] | Outside package; see exact map record |
| wave-spectra / 123 | [docs/registry/implementation-map.yaml:L15125-L15136] | Outside package; see exact map record |
| wave-spectra / 124 | [docs/registry/implementation-map.yaml:L15137-L15148] | Outside package; see exact map record |
| wave-spectra / 125 | [docs/registry/implementation-map.yaml:L15149-L15160] | Outside package; see exact map record |
| wave-spectra / 126 | [docs/registry/implementation-map.yaml:L15161-L15172] | Outside package; see exact map record |
| wave-spectra / 127 | [docs/registry/implementation-map.yaml:L15173-L15184] | Outside package; see exact map record |
| wave-spectra / 128 | [docs/registry/implementation-map.yaml:L15185-L15198] | Outside package; see exact map record |
| wave-spectra / 129 | [docs/registry/implementation-map.yaml:L15199-L15210] | Outside package; see exact map record |
| wave-spectra / 130 | [docs/registry/implementation-map.yaml:L15211-L15222] | Outside package; see exact map record |
| wave-spectra / 131 | [docs/registry/implementation-map.yaml:L15223-L15234] | Outside package; see exact map record |
| wave-spectra / 132 | [docs/registry/implementation-map.yaml:L15235-L15246] | Outside package; see exact map record |
| wave-spectra / 133 | [docs/registry/implementation-map.yaml:L15247-L15258] | Outside package; see exact map record |
| wave-spectra / 134 | [docs/registry/implementation-map.yaml:L15259-L15270] | Outside package; see exact map record |
| wave-spectra / 135 | [docs/registry/implementation-map.yaml:L15271-L15284] | Outside package; see exact map record |
| wave-spectra / 136 | [docs/registry/implementation-map.yaml:L15285-L15298] | Outside package; see exact map record |
| wave-spectra / 137 | [docs/registry/implementation-map.yaml:L15299-L15314] | Outside package; see exact map record |
| wave-spectra / 138 | [docs/registry/implementation-map.yaml:L15315-L15328] | Outside package; see exact map record |
| wave-spectra / 139 | [docs/registry/implementation-map.yaml:L15329-L15342] | Outside package; see exact map record |
| wave-spectra / 140 | [docs/registry/implementation-map.yaml:L15343-L15361] | Outside package; see exact map record |
| wave-spectra / 141 | [docs/registry/implementation-map.yaml:L15362-L15377] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 1 | [docs/registry/implementation-map.yaml:L24106-L24114] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 2 | [docs/registry/implementation-map.yaml:L24115-L24124] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 3 | [docs/registry/implementation-map.yaml:L24125-L24134] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 4 | [docs/registry/implementation-map.yaml:L24135-L24144] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 5 | [docs/registry/implementation-map.yaml:L24145-L24154] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 6 | [docs/registry/implementation-map.yaml:L24155-L24164] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 7 | [docs/registry/implementation-map.yaml:L24165-L24174] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 8 | [docs/registry/implementation-map.yaml:L24175-L24183] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 9 | [docs/registry/implementation-map.yaml:L24184-L24192] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 10 | [docs/registry/implementation-map.yaml:L24193-L24201] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 11 | [docs/registry/implementation-map.yaml:L24202-L24210] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 12 | [docs/registry/implementation-map.yaml:L24211-L24220] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 13 | [docs/registry/implementation-map.yaml:L24221-L24232] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 14 | [docs/registry/implementation-map.yaml:L24233-L24242] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 15 | [docs/registry/implementation-map.yaml:L24243-L24251] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 16 | [docs/registry/implementation-map.yaml:L24252-L24261] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 17 | [docs/registry/implementation-map.yaml:L24262-L24271] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 18 | [docs/registry/implementation-map.yaml:L24272-L24281] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 19 | [docs/registry/implementation-map.yaml:L24282-L24291] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 20 | [docs/registry/implementation-map.yaml:L24292-L24301] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 21 | [docs/registry/implementation-map.yaml:L24302-L24311] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 22 | [docs/registry/implementation-map.yaml:L24312-L24321] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 23 | [docs/registry/implementation-map.yaml:L24322-L24331] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 24 | [docs/registry/implementation-map.yaml:L24332-L24341] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 25 | [docs/registry/implementation-map.yaml:L24342-L24351] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 26 | [docs/registry/implementation-map.yaml:L24352-L24361] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 27 | [docs/registry/implementation-map.yaml:L24362-L24370] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 28 | [docs/registry/implementation-map.yaml:L24371-L24380] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 29 | [docs/registry/implementation-map.yaml:L24381-L24390] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 30 | [docs/registry/implementation-map.yaml:L24391-L24400] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 31 | [docs/registry/implementation-map.yaml:L24401-L24410] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 32 | [docs/registry/implementation-map.yaml:L24411-L24420] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 33 | [docs/registry/implementation-map.yaml:L24421-L24430] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 34 | [docs/registry/implementation-map.yaml:L24431-L24440] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 35 | [docs/registry/implementation-map.yaml:L24441-L24450] | `src/digitalmodel/subsea/catenary_riser/models.py` — `<module>` |
| api-5ct-5l-grade-tables / 36 | [docs/registry/implementation-map.yaml:L24451-L24460] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 37 | [docs/registry/implementation-map.yaml:L24461-L24470] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 38 | [docs/registry/implementation-map.yaml:L24471-L24479] | Outside package; see exact map record |
| api-5ct-5l-grade-tables / 39 | [docs/registry/implementation-map.yaml:L24480-L24488] | Outside package; see exact map record |

## 10. Coverage and open items

The core interfaces, quarantine, independent static checks, legacy calculator and model-writer dependencies are covered here; detailed legacy export qualification and the broader documentation inventory will remain follow-up work rather than implied validation.

| Open item / coverage limit | Responsible discipline / tracking |
| --- | --- |
| UNVERIFIED: shared global axes, origin and phase conventions across legacy exports and modern results. | Riser and mooring review; #2755. |
| UNVERIFIED: governing standard editions, clauses, applicability and calculation Citation sidecars. | Riser and mooring review; #2755. |
| Native G1–G3 and conditional G4 evidence are not produced; target solver version is not established. | Riser and mooring review; #2429. |
| Simple and legacy E1–E3 formula defects fixed; broader route qualification remains open. | Numerical methods; #2756 (related #1811). |
| Historical elastic fixture and legacy force definitions require distinct treatment. | Numerical methods; #2050, #2044. |
| Input defaults, conversions, unit assumptions and absent failure-state checks are enumerated in section 6. | Calculation maintainers; #2753. |
| Invalid-value and cited-DFF status tests are supplied; broader report fallback and provenance completeness remain open. | Reporting maintainers; #2754. |
| README claims of production readiness and coverage are unverified; the counted tests are not a coverage percentage. | Documentation maintainers; #2755. [src/digitalmodel/subsea/catenary_riser/README.md:L219-L225] |
| Broader docs naming this module were discovered; main-path review is recorded below, and full cross-document reconciliation remains open. | Documentation maintainers; #2755. |
| External implementation-map discoveries are indexed by map-row citations without copying identifying paths. | Registry maintainers; #2755. |
| Shared-log entry and second-provider clearance are not yet assigned. | Riser and mooring review; #2755. |

### Source and test coverage

- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/README.md:L1-L241].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/__init__.py:L1-L88].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/cli.py:L1-L262].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/effective_weight.py:L1-L92].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/lazy_wave.py:L1-L196].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/legacy/FEAmodel1.yml:L1-L114].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/legacy/FEAmodel2.yml:L1-L177].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/legacy/__init__.py:L1-L3].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/legacy/catenary.py:L1-L37].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L1-L497].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/legacy/catenaryRiserSummary.py:L1-L115].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/legacy/catenary_equation.py:L1-L76].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/legacy/catenary_riser.py:L1-L167].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/legacy/catenary_riser_summary.py:L1-L117].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/legacy/environment.yml:L1-L14].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/legacy/extractData.py:L1-L81].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/legacy/fileList.py:L1-L15].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/legacy/getFatigueLoading.py:L1-L171].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py:L1-L1288].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/legacy/pipeProperties.py:L1-L185].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/legacy/saveData.py:L1-L47].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/models.py:L1-L460].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/report.py:L1-L825].
- Package source/discovery: [src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L1-L286].

The main-path documentation includes the package README, domain README, durable workflow registry, generic static coverage note and canonicalization handoff.

- [docs/domains/subsea/catenary_riser/README.md:L1-L17].
- [docs/domains/risers/scr-slwr-coverage-2026-10.md:L1-L289].
- [docs/session-handoffs/2026-10-12-catenary-canonicalization.md:L1-L32].
- [docs/registry/workflows.yaml:L1-L1351].

The offline test scope comprises the entire package test directory and the related CLI, summary, canonical catenary and lazy-wave regression files listed below; section 4 selects seven representative cases, not the entire collected suite.

- [tests/subsea/catenary_riser/__init__.py:L1-L1].
- [tests/subsea/catenary_riser/conftest.py:L1-L30].
- [tests/subsea/catenary_riser/independent_static_equilibrium.py:L1-L83].
- [tests/subsea/catenary_riser/test_catenary.py:L1-L53].
- [tests/subsea/catenary_riser/test_catenary_riser.py:L1-L55].
- [tests/subsea/catenary_riser/test_catenary_riser_cli.py:L1-L241].
- [tests/subsea/catenary_riser/test_catenary_riser_unit.py:L1-L428].
- [tests/subsea/catenary_riser/test_lazy_wave_quarantine.py:L1-L94].
- [tests/subsea/catenary_riser/test_riser_report.py:L1-L178].
- [tests/subsea/catenary_riser/test_static_reconciliation.py:L1-L178].
- [tests/specialized/cli/test_catenary_riser_cli.py:L1-L642].
- [tests/test_catenary_riser_summary.py:L1-L747].
- [tests/test_catenary_module.py:L1-L190].
- [tests/test_lazy_wave_catenary.py:L1-L387].
- [tests/test_lazy_wave_force_equilibrium.py:L1-L94].
- [tests/test_lazy_wave_kernel_defects.py:L1-L335].
- [tests/test_lazy_wave_legacy_comparison.py:L1-L322].
- [tests/workflows/test_durable_workflows.py:L1-L2444].

### Broader documentation discovery

The following documents were found by `rg -l -i "catenary.riser|catenary_riser" docs --glob "*.md"`; discovery does not qualify their historical claims, and detailed reconciliation remains #2755.

- [docs/registry/implementation-map.md:L1-L59].
- [docs/domains/development/test_summary_with_validations.md:L1-L159].
- [docs/domains/development/pre_development_testing_checklist.md:L1-L339].
- [docs/domains/offshore-engineering/riser-fatigue-fundamentals.md:L1-L131].
- [docs/session-handoffs/2026-10-12-catenary-canonicalization.md:L1-L32].
- [docs/domains/risers/scr-slwr-coverage-2026-10.md:L1-L289].
- [docs/domains/catenary/catenary_usage_analysis.md:L1-L684].
- [docs/domains/catenary/catenary_consolidation_plan.md:L1-L1036].
- [docs/domains/catenary/CATENARY_CONSOLIDATION_EXECUTIVE_SUMMARY.md:L1-L349].
- [docs/domains/infrastructure/SKILL_VERSIONING.md:L1-L431].
- [docs/domains/orcaflex/SECTION_FIDELITY_ANALYSIS.md:L1-L203].
- [docs/domains/infrastructure/SKILL_VERSIONING_QUICK_REF.md:L1-L214].
- [docs/domains/infrastructure/SKILL_VERSIONING_IMPLEMENTATION.md:L1-L414].
- [docs/plans/2026-05-05-issue-487-towing-marine-operations.md:L1-L38].
- [docs/plans/2026-05-05-issue-508-orcaflex-spec-upgrader-v2.md:L1-L37].
- [docs/plans/2026-05-05-issue-518-model-library-regression-tests.md:L1-L44].
- [docs/plans/2026-05-05-issue-280-orcaflex-format-converter-license-validation.md:L1-L60].
- [docs/plans/2026-05-05-issue-494-flexible-pipe-modeling.md:L1-L38].
- [docs/research/field-development-source-evidence-1794.md:L1-L251].
- [docs/plans/2026-05-05-issue-519-general-environment-groups-fidelity.md:L1-L47].
- [docs/field-development/subsea-production-systems-mapping.md:L1-L160].
- [docs/vision/CALCULATIONS-VISION.md:L1-L266].
- [docs/domains/project-docs/UPDATED_BASELINE_AFTER_ASSETUTILITIES_FIX.md:L1-L184].
- [docs/domains/project-docs/FINAL_TEST_BASELINE.md:L1-L195].
- [docs/domains/project-docs/TEST_RESULTS_SUMMARY.md:L1-L292].
- [docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE2.md:L1-L619].
- [docs/domains/project-docs/MODULE_SURVEY.md:L1-L294].
- [docs/domains/project-docs/MODULE_PORTFOLIO_2026.md:L1-L510].
- [docs/domains/testing/CLI_TESTING_PHASE2_SESSION_SUMMARY.md:L1-L501].
- [docs/domains/orcaflex/templates/USAGE_GUIDE.md:L1-L929].
- [docs/domains/project-docs/MODULE_IMPLEMENTATION_PHASE4.md:L1-L318].
- [docs/domains/testing/CLI_TESTING_FRAMEWORK_COMPLETE.md:L1-L682].
- [docs/domains/project-docs/MODULE_IMPLEMENTATION_RECOMMENDATIONS.md:L1-L389].
- [docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE3.md:L1-L529].
- [docs/domains/testing/coverage_analysis_report.md:L1-L200].
- [docs/domains/project-docs/MODEL_GENERATOR_IMPLEMENTATION_SUMMARY.md:L1-L400].
- [docs/domains/orcaflex/templates/risers/scr_catenary/README.md:L1-L389].
- [docs/domains/orcaflex/templates/risers/scr_hybrid/README.md:L1-L146].
- [docs/domains/orcaflex/QUICKSTART_MODEL_GENERATOR.md:L1-L319].
- [docs/domains/orcaflex/property_routing/walkthrough_examples.md:L1-L291].
- [docs/domains/orcaflex/SEMANTIC_DIFF_TAXONOMY.md:L1-L455].
- [docs/domains/orcaflex/MODULAR_GENERATOR_METHODS.md:L1-L585].
- [docs/domains/SPEC_AUDIT_REPORT.md:L1-L185].
- [docs/domains/subsea/catenary_riser/README.md:L1-L17].
- [docs/domains/orcaflex/templates/README.md:L1-L213].
- [docs/domains/orcaflex/SEMANTIC_EQUIVALENCE_CLAIM_BOUNDARY.md:L1-L240].
- [docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md:L1-L424].
- [docs/domains/orcaflex/examples/ORCAFLEX_MODELS.md:L1-L326].
- [docs/domains/orcaflex/examples/catalog/complete_catalog.md:L1-L278].
- [docs/domains/orcaflex/examples/catalog/examples_catalog.md:L1-L44].

Two discovery documents with potentially identifying names are not copied into this inventory; codenaming and reconciliation remain #2755.
