# SCR and SLWR static coverage and program benchmarking analysis

## Summary

At source revision `c9ae7591129b40732846b50661045f99ab249aba`, the OO lazy-wave
solver satisfies independent depth closure, the input force angle and global
vertical equilibrium on two generic deep-water cases. Legacy geometry agrees,
but legacy vertical force exceeds equilibrium by 14.1% and 21.3%. Lazy-wave
design-input sizing underestimates suspended length by 5.6% and 18.7% against
the independent equilibrium solution; it is a sizing screen, not an equivalent
kernel. SCR design geometry and the general catenary solver agree with the
independent inextensible check; elastic qualification remains unresolved.
The design interface now rejects zero departure angle and nonpositive
hang-off-to-seabed span before calculating geometry.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-14 | Static function coverage and four generic analytical cases are recorded to distinguish equilibrium kernels from sizing screens before configuration ranking. |

Table 1. Initial issue of the static coverage analysis.

## Scope and evidence basis

All production paths below are inspected at the source revision above. Paths in
the inventory are relative to `src/digitalmodel/`; test paths are repository
relative. The new input fixture is
`tests/fixtures/test_vectors/mooring_risers/scr_slwr_generic_cases.json`.
Each numeric input carries units, assumed status and its reason. These are
generic analytical cases, not measurements or literature validation cases.
No earlier project result supplies a reference value. The fixture owns inputs;
expected results are recomputed by the independent check rather than stored.

The checker is
`tests/subsea/catenary_riser/independent_static_equilibrium.py`. It imports only
`math`; an AST test enforces that dependency boundary. Shared physical inputs
(including the calculated submerged weight) are supplied by the reconciliation
test. The checker does not independently qualify pipe-material properties.
Licensed solver interfaces are not used. No dynamic response or design
acceptance is established by this analysis.

The source metadata catalog and domain index were inspected read-only. Their
private precedents and licensed tables are not used as benchmark data. The
knowledge-base SCR design outline supplies method context only; its static
geometry scope does not supply a numerical benchmark.

Standards associations below mean that a module names the standard as its
calculation basis. They do not establish exact-edition compliance. No licensed
coefficient or table is reproduced here. The relevant titles are API STD 2RD,
*Dynamic Risers for Floating Production Systems*; DNV-ST-F201, *Dynamic Risers*;
DNV-RP-C203, *Fatigue Design of Offshore Steel Structures*; DNV-RP-C205,
*Environmental Conditions and Environmental Loads*; and DNV-RP-F105,
*Free Spanning Pipelines*. Mathematical cable kernels have no inspected
implementation claim against those standards. The API STD 2RD title is recorded
in `docs/domains/codes-register.md` at the inspected source revision.

## Independent calculation and acceptance criteria

For a planar, inextensible cable, horizontal force `H` is constant and
`dV/ds = w`. Consequently `dx/dV = H/(w*sqrt(H²+V²))` and
`dz/dV = V/(w*sqrt(H²+V²))`. Composite Simpson integration supplies horizontal
and vertical projections. Arc length follows directly from integrated weight
balance. For each vertex-to-interface segment, with height a positive magnitude,
endpoint work gives `T_end - H = |w|*height`; the input angle from
vertical gives `H = w*drop*sin(angle)/(1-sin(angle))`.

For SLWR, five vertex-to-interface segments are integrated independently.
Continuous vertical tension at a bare/buoyed interface fixes its height split
through equality of `|w|*height`. Signed descent is `d1-d2-d3+d4+d5`, and
global vertical equilibrium is `V_top = sum(w_i*S_i)`, with zero vertical
reaction at touchdown. All five segments, including negative buoyed weight,
enter that sum. Global equilibrium is not inferred solely from the first span.

Acceptance for these analytical comparisons is depth closure within `1e-6 m`,
angle recovery within `1e-9 degree`, global force equilibrium within a
relative tolerance of `1e-12`, and doubled-panel geometry agreement within a
relative tolerance of `1e-9`. Integration uses 4,096 panels per segment, checked
against 8,192. OO/legacy segment geometry is compared within `1e-9` relative.
SCR design outputs are rounded to 0.1 m and 0.1 kN; comparison allowances are
0.05 m and 0.05 kN. The general solver is compared in the inextensible limit
with assumed `EA = 1e20 N`, within `1e-8` relative force tolerance. These are
numerical regression criteria, not engineering design limits.

## Four-case results

All hang-off elevations are zero relative to the waterline. Depths are 1,500.0
and 2,000.0 m; SCR angles are 12.0 and 16.0 degrees, SLWR angles 8.0 and 12.0
degrees. SLWR sag and hog elevations are assumed at 0.1 and 0.4 of depth.
Pipe and buoyancy inputs are explicit in the fixture. The test harness maps
the sizing screen's depths below waterline to elevations above seabed before
calling either equilibrium kernel.

| Case | Suspended length (m) | Offset (m) | Horizontal force (kN) | Vertical force (kN) |
| --- | ---: | ---: | ---: | ---: |
| scr-shallow | 1852.3 | 887.0 | 468.2 | 2202.5 |
| scr-deep | 2654.1 | 1493.4 | 904.9 | 3155.8 |
| slwr-shallow | 3917.6 | 2619.7 | 259.5 | 1846.6 |
| slwr-deep | 6084.4 | 4576.7 | 561.8 | 2643.0 |

Table 2. Independent equilibrium results; forces are resolved at hang-off.

The OO lazy-wave solution meets the comparison criteria on both cases.
Legacy length, offset and horizontal force agree, but its vertical force is
2,106.1 and 3,204.8 kN against independent values of 1,846.6 and 2,643.0 kN.
The excess is exactly `H`: 259.5 and 561.8 kN. The resulting legacy angles are
7.0 and 9.9 degrees against the input 8.0 and 12.0 degrees. This is the
unchanged legacy expression `Fv = Fh + w*S`, not a geometry disagreement.

Sizing-screen total lengths are 3,699.7 and 4,949.6 m against the independent
3,917.6 and 6,084.4 m. The screen uses empirical upper/lower span multipliers
and balances an approximate lower weight; it returns no horizontal force,
vertical force or depth-closure residual. Those unavailable quantities are
not invented or treated as zero.

For SCR, rounded design geometry meets its rounding allowances and the
inextensible BVP meets the independent force criteria. At the pipe's actual
calculated stiffness, general-solver horizontal force is 468.6 and 906.5 kN,
0.1% and 0.2% above the inextensible reference. Reported elongation is 0.2 and
0.6 m. The solver uses `H*L/EA` rather than full tension-dependent strain;
the existing strict expected failure in
`tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py`
records that distinction. These elastic numbers demonstrate model difference;
they are not independently qualified elastic reference answers.

## Canonical paths and production boundary

- SCR: `marine_ops/marine_analysis/catenary/solver.py::CatenarySolver.solve`
  remains the general static BVP path. Its agreement here is qualified only
  in the inextensible limit. `SCRDesignInput.calculate_geometry` is a rounded
  angle-driven screening interface, not an elastic substitute.
- SLWR: `marine_ops/marine_analysis/catenary/lazy_wave.py::LazyWaveSolver.solve`
  is the static equilibrium path supported by these checks. Legacy geometry
  agrees, but legacy force output does not meet equilibrium. The design-input
  sizing screen cannot substitute for this path.
- `engine.py` still routes the catenary basename through
  `subsea/catenary_riser/legacy/catenary.py::Catenary.router` at the inspected
  revision. Migration requires mapping the legacy configuration, plotting and
  model-generation consumers, plus end-to-end regressions; this is a larger
  schema/entry-point gap under [#809](https://github.com/vamseeachanta/digitalmodel/issues/809).
- `subsea/catenary_riser/lazy_wave.py` remains quarantined as settled by
  [#1949](https://github.com/vamseeachanta/digitalmodel/issues/1949). Its CLI
  rejects analysis; neither is counted as a functioning equilibrium kernel.

## Function coverage inventory

The tables group functions only where their units, purpose and test basis agree.
Tests listed here were inspected; only the suites in the validation section
are asserted to have been run for this change. Indirect coverage is identified
explicitly. Imported third-party classes and private functions are excluded.

| Module and public surface | Calculation; input → output units | Inspected tests | Grade and code basis |
| --- | --- | --- | --- |
| `marine_ops/marine_analysis/catenary/solver.py`: `CatenarySolver.solve`; `CatenaryInput`, `CatenaryResults` | Unstretched length/spans [m], weight [N/m], EA [N] → shape/elongation [m], endpoint forces [N], convergence | `tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py`, `test_catenary_integration.py`, `test_catenary_perf.py` | Static BVP; mathematical model, no standards implementation established |
| `marine_ops/marine_analysis/catenary/lazy_wave.py`: `derive_hangoff_bend_radius`, `LazyWaveConfiguration.hangoff_vertical_span`, construction validation | Drop [m], departure angle [deg], signed weights [N/m], sag/hog elevations [m] → radius/span [m], valid configuration | `tests/test_lazy_wave_kernel_defects.py`, `tests/test_lazy_wave_force_equilibrium.py` | Static geometry and validation; mathematical model |
| Same: `LazyWaveSolver.solve`, `to_legacy_dict`; `LazyWaveSegment`, `LazyWaveResults` | Configuration → five segments [m], force [N], length/offset/closure [m], legacy dictionary | `tests/test_lazy_wave_catenary.py`, `tests/test_lazy_wave_legacy_comparison.py`, `tests/subsea/catenary_riser/test_static_reconciliation.py` | Static, inextensible equilibrium; mathematical model |
| `marine_ops/marine_analysis/catenary/simplified.py`: `SimplifiedCatenarySolver.solve_from_angle`, `solve_from_force`, `calculate_forces`, `solve_catenary_dict`, `calculate_forces_dict` | Vertical angle [deg], drop [m], weight [N/m] or top force [N] → arc/offset/radius [m], forces [N] | `tests/marine_ops/marine_engineering/catenary/test_simplified.py` | Static angle/force model; input-field comment contradicts the method's vertical-angle convention |
| `marine_ops/marine_analysis/catenary/adapter.py`: `catenaryEquation`, `catenaryForces`, `validate_catenary_data` | Legacy dictionaries, spans [m], angles [deg], weight [N/m], force [N] → geometry/force or validation | `tests/marine_ops/marine_engineering/test_catenary_adapter.py` | Static compatibility; lineage parity is not independent validation |
| Same: `convert_legacy_to_modern` | Legacy dictionary → optional modern input; complete BVP data unavailable | No direct assertion established in the inspected adapter tests | Conversion support; coverage/completeness gap |
| `subsea/catenary_riser/legacy/catenaryMethods.py`: `catenaryEquation`, `catenaryForces`, `sagHogEquation`, `lazyWaveCatenaryEquation` | Legacy spans [m], angle [deg], weights [N/m] → segments [m], forces [N] | `tests/test_lazy_wave_legacy_comparison.py`, `tests/test_lazy_wave_force_equilibrium.py`, new reconciliation tests | Static legacy; lazy-wave vertical force fails equilibrium |
| Same: `lazyWavePlot`, `simple_catenary_plot`, `genericPlot` | Sample geometry [m] → plots | Isolated assertions not established | Presentation, no physical qualification |
| Same: `buoyancyProperties`, `gethorizontalDistanceHangofftoTDPrange` | Configuration dictionaries → buoyancy properties and offset ranges [m]; complete unit contract not established | Direct coverage not established | Static support; contract gap |
| `subsea/catenary_riser/simple_catenary.py`: `SimpleCatenaryAnalyzer.analyze_riser` | Uniform weight [N/m], depth/offset [m] or tensions [N] → shape [m], force [N], curvature [1/m], angle [deg] | `tests/subsea/catenary_riser/test_catenary_riser_unit.py` | Static flexible cable; source names DNV-OS-F201/API RP 1111, not an API STD 2RD implementation |
| Same: `calculate_tension_along_riser`, `calculate_angle_along_riser`, `check_design_limits` | Arc positions [m] → force [N], angle from horizontal [deg]; supplied limits → flags | Same unit suite covers limit flags; direct distribution calls not established | Static post-processing / supplied-limit screen |
| `subsea/catenary_riser/effective_weight.py`: `EffectiveWeightCalculator.calculate`, `calculate_with_buoyancy_module` | Dimensions [m], densities [kg/m³] → weight components [N/m] | Same unit suite covers `calculate`; module-method direct coverage not established | Static section calculation; DNV-OS-F201 is named, exact compliance unverified |
| `subsea/catenary_riser/models.py`: `RiserConfiguration.inner_diameter`, `pipe_cross_section`, `internal_area`, `external_area`, `displaced_volume_per_length`, `steel_weight_per_length`, `coating_weight_per_length`, `contents_weight_per_length`, `buoyancy_per_length`, `effective_weight_per_length` | Dimensions [m], densities [kg/m³] → diameter [m], area [m²], volume [m³/m], forces [N/m] | Same unit suite; several properties covered indirectly | Static data model |
| Same: `BuoyancyModule.buoyancy_force_per_length`, `LazyWaveConfiguration.total_buoyancy_length`, `get_effective_weight_profile`, `EffectiveWeightResult.to_dict`, `get_material`, `get_fluid` | Module/configuration → force [N/m], length [m], weight profile [N/m], SI lookup/serialization | Same unit suite | Static support; does not qualify a lazy-wave solver |
| `subsea/catenary_riser/cli.py`: `cli`, `simple_catenary`, `effective_weight`, `lazy_wave_analysis`, `main` | Input configuration in SI → displayed/file results; lazy-wave rejection | `tests/subsea/catenary_riser/test_catenary_riser_cli.py`, `test_lazy_wave_quarantine.py`, `tests/specialized/cli/test_catenary_riser_cli.py` | Interface; deliberate lazy-wave failure is not analysis coverage |
| `subsea/catenary_riser/report.py`: `CodeCheck.status`, `StrengthResult.status`, `FatigueResult.status`, `FatigueResult.fatigue_life_years`, `Provenance.mode_label`, `RiserReportData.overall_status`, `max_strength_utilisation`, `min_fatigue_life_years` | Supplied utilization/damage [1], annual damage [1/year] → life [year], margins/status and presentation labels | `tests/subsea/catenary_riser/test_riser_report.py` | Reporting; physical evidence is not recalculated |
| Same: `sample_riser_report_data`, `render_html`, `render_markdown`, `generate_riser_report` | Supplied data → sample/rendered report/file | Same report suite | Presentation; sample labels do not establish standards implementation |
| `orcaflex/riser_config.py`: `RiserPipeProperties.inner_diameter`, `nominal_wt`, `steel_area`, `internal_area`, `total_od`, `displaced_volume_per_m`, `steel_mass_per_m`, `coating_mass_per_m`, `contents_mass_per_m`, `total_mass_per_m`, `submerged_weight_per_m`, `bending_stiffness`, `axial_stiffness` | Geometry [m], density [kg/m³], E [Pa] → area [m²], volume [m³/m], mass [kg/m], weight [N/m], EI [N·m²], EA [N] | `tests/orcaflex/test_riser_config.py`; direct ID, OD, mass/weight/EI checks, others indirect | Static section calculation; source names API RP 2RD |
| Same: `SCRDesignInput.calculate_geometry` | Depth/elevation [m], vertical angle [deg], pipe weight [N/m] → rounded length/offset/parameter [m], forces [kN] | Same config suite; new reconciliation suite | Static inextensible screen; empirical total-length allowance |
| Same: `LazyWaveDesignInput.buoyancy_volume_per_m`, `buoyancy_mass_per_m`, `net_uplift_per_m`, `size_buoyancy_section` | Module diameter [m], density [kg/m³], depths [m], angle [deg] → volume [m³/m], mass [kg/m], uplift [N/m], estimated lengths [m] | Same config suite; new reconciliation suite | Empirical sizing screen; not equilibrium; source names API RP 2RD |
| Same: `TTRDesignInput.calculate_stroke`, `calculate_top_tension` | Thermal/heave inputs → stroke [m], force [kN] | Same config suite | Screening; outside SCR/SLWR numerical comparison |
| Same: `calculate_weight_in_water`, `estimate_scr_hang_off_angle` | Pipe dimensions/densities → weight [N/m]; depth/offset [m] → angle [deg] | Same config suite | Static weight / straight-line angle screen; optional length unused |
| `riser_fatigue/touchdown.py`: `assess_touchdown_fatigue`, `report_markdown` | Stress bins [MPa], cycles [1], thickness [mm], SCF/DFF [1], periods [year] → corrected stress, damage [1], life [year], margin/status/report | `tests/riser_fatigue/test_touchdown.py` | Fatigue calculation from supplied histograms; DNV-RP-C203 |
| `riser_fatigue/workflow.py`: `resolve_dff`, `router` | Explicit/cited DFF [1], wave stress/cycles, VIV stress [MPa], frequency [Hz], exposure → combined damage [1], life [year], CSV | `tests/riser_fatigue/test_resolve_dff.py`, `test_riser_fatigue_workflow.py` | Screening; dynamic stress front-end absent; DNV-RP-C203 |
| `fatigue/c203_sn_tables.py`: `BilinearClass`, `FreeCorrosionClass`, exported records | S–N slopes/intercepts/exponents [1], reference thickness [mm], stress [MPa], cycles [1] | `tests/fatigue/test_c203_consumers.py`, `test_c203_variable_amplitude.py`, `tests/structural/fatigue/test_sn_curves_yaml.py` | Data basis; DNV-RP-C203; later-edition equivalence not established |
| `viv_screening/workflow.py`: `router`, `save_echo` | Current [m/s], geometry [m], EI [N·m²], mass [kg/m], tension [N] → modes [Hz], lock-in, amplitude ratio [1], damage rate [1/year], life [year], echo | `tests/workflows/test_viv_screening_workflow.py`, `test_durable_workflows.py`, `tests/workflow_api/test_run_workflow_viv_screening.py`, `test_viv_screening_publication.py` | Screening; DNV-RP-C205, DNV-RP-F105, DNV-RP-C203 |
| `orcaflex/viv_screening.py`: `strouhal_number`, `VIVScreeningInput.reynolds_number`, `st`, `vortex_shedding_frequency`, `stability_parameter`, `check_reduced_velocity` | Fluid/current inputs → Reynolds/Strouhal/stability/reduced velocity [1], frequency [Hz], lock-in flags | `tests/orcaflex/test_viv_screening.py`; `st` indirect | Screening; DNV-RP-C205 |
| Same: `BeamProperties.moment_of_inertia`, `bending_stiffness`, `boundary_constants`, `natural_frequency`, `natural_frequencies`, `viv_screening`, `estimate_response_amplitude` | Geometry [m], E [Pa], mass [kg/m], tension [N] → I [m⁴], EI [N·m²], frequencies [Hz], flags, amplitude ratio [1] | Same VIV suite | Screening; DNV-RP-F105 frequency / DNV-RP-C205 response association |
| `structural/analysis/wall_thickness_codes/api_std_2rd.py`: `ApiStd2rdStrategy.run_checks`, `compute_yield_moment`, `compute_plastic_moment`, `compute_plastic_tension` | Geometry [m], strengths/pressure [Pa], force [N], moment [N·m] → burst/collapse/combined utilization [1], capacities [N, N·m] | `tests/test_wall_thickness_api_std_2rd.py`, `tests/test_riser_case_screening.py`; separate yield-moment coverage not established | Supplied-load strength screening; API STD 2RD |
| `structural/analysis/wall_thickness_codes/dnv_st_f201.py`: `DnvStF201Strategy.get_factors`, `run_checks`, `compute_plastic_moment`, `compute_plastic_tension` | SI pipe/load data, factors [1] → burst utilization [1], capacities [N, N·m] | `tests/test_wall_thickness_codes/test_dnv_st_f201.py`; plastic-method protocol presence only | Burst-only supplied-load check; DNV-ST-F201; no dynamic response solver |
| `subsea/riser/riser_case_screening.py`: `RiserLoadCase.to_loads`, `screen_riser_cases`, `run_riser_screening`, `build_calc_note` | Pressure [Pa], tension [N], moment [N·m], geometry [m] → ranked utilization/status, engine/report output | `tests/test_riser_case_screening.py`; direct engine/note coverage not established there | Screening; API STD 2RD; report's F201 availability wording needs the burst-only distinction |

Table 3. Inspected public function coverage and numerical grade.

## SCR gaps

1. General-solver elastic formulation remains approximate; independent full-strain
   qualification is tracked by [#2050](https://github.com/vamseeachanta/digitalmodel/issues/2050).
2. Unified schema, SI boundaries and canonical entry routing remain under
   [#809](https://github.com/vamseeachanta/digitalmodel/issues/809) and
   [#1811](https://github.com/vamseeachanta/digitalmodel/issues/1811). Zero-angle
   and nonpositive-span rejection are corrected here with three failing-first
   regressions. The simplified angle-comment contradiction remains a
   documentation gap before wider use.
3. Current-loaded, bending-stiffness, seabed-interaction and dynamic response
   are not established by the inspected cable kernels. Stress extraction is
   tracked by [#810](https://github.com/vamseeachanta/digitalmodel/issues/810),
   VIV integration by [#812](https://github.com/vamseeachanta/digitalmodel/issues/812).
4. Exact fatigue basis/citations remain under
   [#1588](https://github.com/vamseeachanta/digitalmodel/issues/1588). F201 burst
   availability does not establish combined/collapse or dynamic qualification;
   the remaining strength scope is under
   [#878](https://github.com/vamseeachanta/digitalmodel/issues/878).
5. Atlas validation and a report that refuses incomplete evidence remain under
   [#815](https://github.com/vamseeachanta/digitalmodel/issues/815) and
   [#814](https://github.com/vamseeachanta/digitalmodel/issues/814). The supplied
   result renderer is not a SCR-specific analysis/report pipeline.

## SLWR gaps

1. Legacy force consumers and production routing remain unreconciled under
   [#2044](https://github.com/vamseeachanta/digitalmodel/issues/2044) and
   [#809](https://github.com/vamseeachanta/digitalmodel/issues/809). The merged
   force fix corrects the OO path and fixture; it does not correct legacy Fv.
2. Sizing and equilibrium interfaces have different physical contracts.
   Coating treatment in buoyed weight differs from bare-pipe weight. A unified
   input basis requires an explicit definition of displaced volume and retained
   coating before any substitution or configuration sweep.
3. Excursion-to-stress coupling and constrained ranking remain under
   [#1947](https://github.com/vamseeachanta/digitalmodel/issues/1947) and
   [#815](https://github.com/vamseeachanta/digitalmodel/issues/815). An optimum
   against sizing heuristics or a quarantined analyser is not established.
4. Dynamic, fatigue and VIV qualification require external response evidence
   through [#810](https://github.com/vamseeachanta/digitalmodel/issues/810) and
   [#812](https://github.com/vamseeachanta/digitalmodel/issues/812). Any gap that
   needs licensed solver output is assigned to a later campaign, not estimated.

## Validation and way forward

At the inspected source revision, the required baseline suite completed with
237 passed, zero failed and one existing expected failure. The reporting
environment check completed with 16 passed and no regenerated golden files.
The initial reconciliation suite completed with 11 passed. Its initial collection
failed because the independent checker did not yet exist; implementation then
enabled the physical assertions. This is a new analytical reference feature,
not a claim that the legacy or sizing defects have been repaired. One initial
diagnostic assertion incorrectly presumed a sizing difference over 10%; it
was corrected to distinguish the paths at the screen's rounding resolution.
No existing assertion or expected-failure marker is changed. Three additional
SCR boundary cases failed before the validation fix: zero angle returned
geometry, zero span raised a division error, and negative span returned
geometry. All pass after enforcing a strictly positive angle and span; the
combined new reconciliation/configuration suite completes with 32 passed.
The numerical kernel is unchanged; only invalid-input rejection is added.

The required suite is `tests/test_lazy_wave_kernel_defects.py`,
`tests/test_lazy_wave_force_equilibrium.py`, `tests/test_lazy_wave_legacy_comparison.py`,
`tests/test_lazy_wave_catenary.py`, `tests/marine_ops/marine_engineering/catenary`,
`tests/subsea/catenary_riser`, `tests/riser_fatigue`,
`tests/orcaflex/test_riser_config.py` and `tests/orcaflex/test_viv_screening.py`.
Publication evidence records the exact tested commits and commands in the draft
pull request. Review and merge remain separate actions at the published head.

The next implementation will preserve the OO SLWR equilibrium path, reconcile
consumer contracts before changing production routing, and qualify elastic SCR
behaviour before treating it as an independent design answer. Dynamic calibration,
configuration ranking and report generation remain separate later work under
the open [riser epic](https://github.com/vamseeachanta/digitalmodel/issues/807).
