# Pipelay module knowledge pack

| Header | Value |
|---|---|
| Module / repository | `pipelay` / `digitalmodel` |
| Package | `src/digitalmodel/pipelay` |
| Pack revision | 2, 2026-10-28 |
| Code scope | Source and tests in the same Git revision as this pack |
| Status | DRAFT; independent clearance pending |

Table 1. Pack identification. Current source citations name symbols; the historical map inventory below retains its original revision and line citations.

## 1. Purpose and scope

`digitalmodel.pipelay.workflow.router` dispatches analytical J-lay or reel-lay
screening, writes strict JSON, and merges the output into the supplied configuration.
`run_jlay` delegates geometry to `JLayConfig.calculate_sagbend`;
`run_reel_lay` reports core strain, residual curvature and out-of-straightness.
The package also contains `validation.py`, which validates inputs and records defaults.
No native model, dynamics, cyclic-plasticity or ovalisation solution is produced.
Source: [workflow.py](../workflow.py), [validation.py](../validation.py).

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| 1 | 2026-10-18 | Analytical screening audit and historical map inventory were recorded. |
| 2 | 2026-10-28 | The current map discussion distinguishes local calculations from delegates; the historical inventory is retained for traceability. |

## 2. Inputs and outputs

All lengths use metres, forces use kN at the workflow boundary, densities use
kg/m³ and strains use dimensionless fractions. Angle is measured from vertical.
Unknown keys in the pipelay settings and its pipe/config/operability/output/case
blocks raise ValueError. Missing design-basis fields raise before output.

| Input | Unit | Allowed range / handling | Default | Source |
|---|---|---|---|---|
| `pipelay.method` | text | J-lay/reel-lay aliases; unsupported methods raise | J-Lay, recorded when applied | `workflow.router` |
| `config.water_depth_m` | m | finite positive; required | none | `validation.validate_settings` |
| `config.tower_angle_deg` | degree | finite, greater than zero, at most 15; required | none | `JLayConfig` |
| `config.top_tension_kN` | kN | required; exceeds w*h; relative closure error at most 0.001 | none | `JLayConfig.calculate_sagbend` |
| `config.allowable_sagbend_strain` | fraction | finite, strictly between zero and 0.01 | code default at the cited line; no standard allowable asserted | `validation.JLAY_DEFAULTS` [src/digitalmodel/pipelay/validation.py:L10] |
| `config.reel_core_radius_m` | m | finite positive; required | none | `validation.validate_settings` |
| `config.allowable_reeling_strain` | fraction | finite, strictly between zero and 0.05 | code default at the cited line; no standard allowable asserted | `validation.REEL_DEFAULTS` |
| `config.straightener_efficiency` | fraction | finite, zero to one inclusive | 0.95 | `validation.REEL_DEFAULTS` |
| `config.residual_out_of_straightness_span_m` | m | finite positive | 12.0 | `validation.REEL_DEFAULTS` |
| `config.allowable_overbend_strain` | fraction | finite, strictly between zero and one | code default at the cited line; no standard allowable asserted | `validation.COMMON_DEFAULTS` [src/digitalmodel/pipelay/validation.py:L9]; `calculate_stinger_radius` |
| `pipe.outer_diameter`, `pipe.wall_thickness` | m | required; finite, 0<t<D/2 | none | `PipelayPipeProperties.validate_annulus` |
| `pipe.steel_density`, `pipe.seawater_density` | kg/m³ | required; finite positive | none | `validation.PIPE_REQUIRED`; `PipelayPipeProperties` |
| `pipe.coating_thickness`, `pipe.coating_density` | m, kg/m³ | required; finite non-negative | none | `validation.PIPE_REQUIRED`; `PipelayPipeProperties` |
| `pipe.youngs_modulus` | Pa | finite positive | code default at the cited line; no standard allowable asserted | `PipelayPipeProperties` |
| `pipe.smys` | Pa | finite positive | code default at the cited line; no standard allowable asserted | `PipelayPipeProperties` [src/digitalmodel/orcaflex/pipelay_analysis.py:L54] |
| `pipe.poissons_ratio` | fraction | finite positive; unused by this screening formula | code default at the cited line; no standard allowable asserted | `PipelayPipeProperties` |
| `operability` | m, km/day, km/day/m | optional; finite field constraints; unknown keys rejected | code default at the cited line; no standard allowable asserted | `LayOperability` |
| `outputs.directory`, `outputs.summary_json` | path | explicit output paths or recorded defaults | `results/pipelay`, `pipelay_summary.json` | `validation.OUTPUT_DEFAULTS`; `workflow._write_summary` |

Table 2. Validated workflow inputs. Defaults are implementation assumptions, never qualified design allowables. Direct analytical model defaults do not replace workflow completeness checks.

`applied_defaults` lists omitted optional pipe/config/method fields; `router`
also records output defaults. `top_tension_kN` is derived, while
`supplied_top_tension_kN` preserves the consistency-check input. `sagbend`
includes closure error/tolerance, horizontal tension, radius, strain, stress
proxy, offset and arc length. Radius and strain retain full calculation
precision so a rounded value cannot determine acceptance. Offset/length are
rounded to 0.1 m, stress to 0.1 MPa and reported utilisation to four decimals.
Reel strain retains six displayed decimals; status uses the unrounded ratio.
For perfect straightening, `residual_radius_m` is null and
`residual_radius_status` is `unbounded_zero_curvature`; residual strain is zero.
J-lay operability reports `lay_and_welding`; reel screening reports `lay_only`.
Source: `run_jlay`, `run_reel_lay`, `_operability`, `_write_summary`.

## 3. Method and governing references

[screening-basis.md](screening-basis.md), E1–E7, derives every changed formula
from static force balance and geometry and identifies the reel and lay-rate
assumptions. V1–V4 define the input checks. The tension consistency tolerance
and strain plausibility bounds are audit-selected validation criteria.
No standard-tabulated allowable is reproduced or newly introduced.

The inherited source references name DNV-OS-F101 Sections 5 and 11 but omit
edition qualification. Those references do not establish compliance. No new
standards-derived constant is introduced and no standard Citation sidecar is
claimed. Edition/clause and output provenance qualification remain open under
[1817](https://github.com/vamseeachanta/digitalmodel/issues/1817).

## 4. Worked screening examples

Independent arithmetic anchors are documented in screening-basis.md; these
qualify the regression formulas, not a native installation model.

| Example | Executable node ID | Assertions |
|---|---|---|
| J-lay template | `tests/pipelay/test_pipelay_workflow.py::test_jlay_template_runs_offline` | w=1658.33 N/m, a=82.8394 m, strain=0.00195438, T=2624.9 kN, offset=301.7453 m, length=1580.6702 m; relative tolerance 0.001 |
| Reel template | `tests/pipelay/test_pipelay_workflow.py::test_reel_lay_template_runs_offline` | core strain=0.017069, residual curvature=0.00625 1/m, radius=160 m, strain=0.000853, offset=0.1125 m; relative tolerance 0.001; core formula uses input values |
| Tight core | `tests/pipelay/test_pipelay_workflow.py::test_reeling_strain_exceeding_limit_fails` | `run_reel_lay` utilisation exceeds one and status is fail |
| Unsupported method | `tests/pipelay/test_pipelay_workflow.py::test_router_rejects_unknown_method` | `router` raises ValueError |
| Feasible higher tension | `tests/pipelay/test_pipelay_workflow.py::test_jlay_higher_tension_reduces_sagbend_strain` | consistent angle changes with tension; `run_jlay` strain decreases |

Table 3. Retained workflow checks with added independent numerical anchors.

## 5. Executable QA gates

Every applicable check names the package function/output and a committed test.
No manual procedure is counted as an executed gate. C02 establishes input
schema/ranges, not provenance or automatic unit inference. C07 verifies test
artifact readback, not a signed issued-report appendix.

| Gate / check | Function or output asserted | Executable test and criterion |
|---|---|---|
| G0 / C01 | `workflow.router` | `tests/pipelay/test_pack_gates.py::test_c01_router_rejects_unsupported_method`: unsupported method raises ValueError |
| G0 / C02 | `workflow.router` | `tests/pipelay/test_pack_gates.py::test_c02_router_rejects_unknown_key`: wrong diameter key raises ValueError; zero output files. Missing/finite/geometry boundaries are parameterized in `test_audit_regressions.py` |
| G2 / C03 | `workflow.run_jlay` sagbend and derived tension | `tests/pipelay/test_pack_gates.py::test_c03_jlay_hand_check`: a=82.8394 m, strain=0.00195438, T=2624.872 kN, relative tolerance 0.001 |
| G2 / C04 | `workflow.run_reel_lay` residual outputs and status | `tests/pipelay/test_pack_gates.py::test_c04_reel_residual_hand_check`: curvature=0.00625 1/m, radius=160 m, strain=0.000853, offset=0.1125 m, relative tolerance 0.001; tight core fails |
| G2 / C05 | `workflow.run_jlay` strain and closure error | `tests/pipelay/test_pack_gates.py::test_c05_feasible_tension_trend`: strain decreases; closure error at most 0.001 |
| G5 / C06 | `workflow.router` and `_write_summary` output | `tests/pipelay/test_pack_gates.py::test_c06_summary_matches_router`: both methods/statuses match; no native-model/time-history output |
| G5 / C07 | `workflow.router` summary bytes | `tests/pipelay/test_audit_regressions.py::test_c07_summary_digest_readback`: both templates parse with non-finite tokens rejected; input and output SHA-256 match rereads; payload equals returned fields |

Table 4. Seven executable checks. G1 native load, G3 dynamics and G4 diffraction
are outside this analytical interface; C06 asserts the absence of native output.
No native gate is claimed passed. Exact test counts and execution evidence are
recorded in the audit repair PR.

## 6. Failure modes and limits

`run_jlay` rejects missing fields, inconsistent/insufficient tension, zero angle
and non-positive submerged weight before forming a result. Both workflow
functions reject unknown keys and non-finite or implausible numerical limits.
`router` rejects invalid overbend strain before output and `_write_summary`
serializes with `allow_nan=False` before directory creation. Existing output
files are not erased when a later invalid request is rejected.

Tests exercise these branches in `tests/pipelay/test_audit_regressions.py`.
The legacy 27-item default/fallback inventory is superseded by the validated
input contract and its executable tests. Applied defaults remain assumptions;
unit/provenance/destination confirmation is tracked in
[2763](https://github.com/vamseeachanta/digitalmodel/issues/2763).
The S-lay depth-table strict expected failure remains unchanged and outside
this J-lay/reel-lay repair.

## 7. Lessons log

| Date | Reference | Finding and prevention | Evidence |
|---|---|---|---|
| 2026-10-16 | initial pack | Screening regression is distinct from installation qualification | initial pack revision |
| 2026-10-18 | [2745](https://github.com/vamseeachanta/digitalmodel/issues/2745) | Formula invariants can pass incorrect physics; use independent anchors, force closure and fail-before-output checks | `test_audit_regressions.py`, `test_pack_gates.py`, screening-basis.md |

Table 5. Durable shared entries are `pipelay-screening-scope` and
`pipelay-independent-anchors`, respectively, in the
[review and clearance index](../../../../docs/registry/pipelay-review-clearance.json).
The entries retain the findings and link their prevention to C01–C07 evidence.
The index is discoverable from the shared documentation entry point.

## 8. Maintenance and review

The [review and clearance index](../../../../docs/registry/pipelay-review-clearance.json)
records the responsibilities and evidence requirements under issue #2767.
Responsibilities are defined by role; assignments remain `NOT_EVALUATED` until
maintainer confirmation is retained. No personnel assignment is made here.

| Responsibility | Required action | Current evidence |
|---|---|---|
| Maintainer | Confirm intended use, assign primary and alternate independent review, and dispose findings | Assignment confirmation pending |
| Independent reviewer | Review the exact source, pack and index revision, check C01–C07 evidence and record findings and recommendation | Assignment and review pending |
| Alternate reviewer | Provide the same independent review when the primary reviewer is unavailable or has a conflict; record activation | Assignment and activation pending |

Review responsibility table. Role definitions do not establish assignments.

Pack review requires the exact reviewed revision, assignment evidence,
independent review reference, C01–C07 commands and outcomes with retained log
and digest references, and a maintainer disposition of findings and unresolved
qualification limits. A changed revision requires renewed review. The index
lists exact function, output and executable assertion references for every
check; its `NOT_EVALUATED` execution fields carry no run receipt. Passing the
index tests establishes traceability only. Test outcomes are recorded with the
change that runs them, rather than asserted by this static index.

Pack review and native installation clearance are separate index records, both
`NOT_EVALUATED`. Native clearance additionally requires separately authorized
scope, sourced case inputs, retained native results at a recorded solver version,
qualified criteria and benchmarks, independent review and maintainer disposition.
Issues #2763–#2766 retain the input, provenance, benchmark and map qualification
work; #2431 retains the native installation workflow. This slice adds no
engineering equation, coefficient or acceptance limit. The pack remains DRAFT
until independent review is recorded and does not grant installation clearance.

## 9. Dependencies and historical map inventory

The live analytical dependencies are `JLayConfig`, `PipelayPipeProperties`,
`LayOperability` and `calculate_stinger_radius` from
`digitalmodel.orcaflex.pipelay_analysis`; the workflow tests also exercise
`digitalmodel.engine`. No commercial solver is needed by these calculations.

### Current bounded map reconciliation

The canonical [implementation map](../../../../docs/registry/implementation-map.yaml)
now distinguishes calculation bodies from delegates for the inspected call graph.
The source basis is revision `09089a121a16069c4b657d1a320a630a23ab1dd5`.
The historical inventory below remains a record of its named revision; its
classifications, search counts and follow-up statements are superseded only for
the symbols in this bounded reconciliation.

| Source module | Explicit symbol set | Family membership | Role and direct calculation delegates |
|---|---|---|---|
| `orcaflex/pipelay_analysis.py` | `JLayConfig.calculate_sagbend` | `catenary-mooring-line-statics`; `effective-tension-riser-stack-up` | Independent analytical sagbend calculation |
| `pipelay/workflow.py` | `run_jlay` | `catenary-mooring-line-statics`; `effective-tension-riser-stack-up` | Delegates to `JLayConfig.calculate_sagbend` and optional `_operability`; local strain utilisation and status do not add another catenary solution |
| `pipelay/workflow.py` | `_reel_result` | `pipelay-bending-screening` | Independent reeling-strain and assumed residual-curvature calculation |
| `pipelay/workflow.py` | `run_reel_lay` | `pipelay-bending-screening` | Delegates to `_reel_result` and optional `_operability` |
| `orcaflex/pipelay_analysis.py` | `calculate_stinger_radius` | `pipelay-bending-screening` | Independent minimum-radius calculation |
| `pipelay/workflow.py` | `router` | `pipelay-bending-screening` | Delegates to `run_jlay`, `run_reel_lay` and `calculate_stinger_radius`; writes the resulting evidence |
| `orcaflex/pipelay_analysis.py` | `LayOperability.lay_rate_at_hs` | `lay-rate-operability` | Independent assumed rate degradation and cutoff calculation |
| `orcaflex/pipelay_analysis.py` | `LayOperability.generate_operability_table` | `lay-rate-operability` | Delegates grid-point rates to `LayOperability.lay_rate_at_hs` |
| `pipelay/workflow.py` | `_operability` | `lay-rate-operability` | Delegates to `LayOperability.generate_operability_table`; locally selects the operating limit |

Table 5. Explicit current symbol set, hand-inspected in [workflow.py](../workflow.py)
and [pipelay_analysis.py](../../orcaflex/pipelay_analysis.py). Optional operability
calls are recorded even when a particular input omits them.

Counting the distinct path/symbol pairs in Table 5 gives **9 unique callable
members: 4 calculation bodies and 5 delegates**. Repeated catenary/tension
membership gives 11 map rows across 4 represented families, not 11 independent
calculations. Inside `pipelay/workflow.py` there are 5 unique members: one
calculation body and four delegates. Local utilisation and operating-limit
arithmetic are described within the delegate rows. The independent AST checks in
`tests/docs/test_pipelay_implementation_classification.py` assert this explicit
set and its direct call edges, including missing-member, nonexistent-symbol,
wrong-role and wrong-target mutations.

Catenary uses the existing family. No existing family describes reeling strain,
residual curvature, stinger bending or assumed lay-rate degradation, so the two
new families are bounded to the source files above. This is a **partial
inventory**: pipe-property accessors, validation, output helpers, S-lay methods,
other installation routes and the broader map audit remain outside this slice.
A map row or passing inventory test is neither numerical qualification nor
native solver qualification. No equation, default, numerical output or workflow
wiring changes in this reconciliation. The historical package-aware
classification follow-up is addressed for Table 5 only; broader qualification
and map audit remain open.

### Historical implementation-map inventory

The following inventory is retained at map revision
`3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` (data revision
`270c8759c374b67a7f032953e7fcdb14e0224461`). Its package line citations and
search counts refer to code revision `7aad603be1c6136106dbb2381ce5096b050654f8`,
not this repaired implementation. Reconciliation is tracked in
[2766](https://github.com/vamseeachanta/digitalmodel/issues/2766).


The map family with an in-package row is `effective-tension-riser-stack-up`: one row, `run_jlay`; this is therefore the sole counted family and ranks first, with no tie. The map calls it independent, while source inspection shows delegation for sagbend geometry plus a local utilisation criterion. [docs/registry/implementation-map.yaml:L3698-L3708] [src/digitalmodel/pipelay/workflow.py:L62-L93]

The inventory below preserves every one of that family's 264 rows, including examples, tests, wrappers, callers and implementations outside the package; it is a transcription of the map classifications, not a new physical qualification of them. [docs/registry/implementation-map.md:L15-L20] [docs/registry/implementation-map.yaml:L2139-L4932]

Exact case-sensitive Python `re` signals from the named map revision:

```text
effective_tension|effectiveTension|top_tension|tensioner|stack_up|internal_pressure.*(?:area|A_i)|external_pressure.*(?:area|A_o)
effective.tension|tensioner|stack.up
internal_pressure|external_pressure|fluid_density|buoyancy_module
```

None of these signals matches `pipelay` when tested against the package-relative path case-insensitively, so none is removed; the exact package search yields two line hits, both inside listed `run_jlay`, and zero further second implementations, satisfying the at-most-one criterion. [docs/registry/implementation-map.yaml:L2139-L2144] [src/digitalmodel/pipelay/workflow.py:L71-L83]

A whole-repository search of tracked Python and shell sources, including `scripts/`, also covers `calculate_sagbend`, `run_jlay`, `run_reel_lay`, `residual_curvature`, `reeling_strain` and `calculate_stinger_radius`; upstream implementations are map follow-up rather than additional in-package misses. In particular the actual J-lay hyperbolic geometry and stinger formula are outside the package. [src/digitalmodel/orcaflex/pipelay_analysis.py:L222-L260] [src/digitalmodel/orcaflex/pipelay_analysis.py:L307-L324]

Map follow-up: catenary geometry has map rows upstream but no row inside this package; reel bending/residual-curvature screening, pipe-section/stinger bending and lay-rate operability need package-aware family classification before counting. These non-counted families do not change the result above; [#2766](https://github.com/vamseeachanta/digitalmodel/issues/2766). [docs/registry/implementation-map.yaml:L1019-L1029] [src/digitalmodel/pipelay/workflow.py:L49-L59] [src/digitalmodel/pipelay/workflow.py:L96-L161] [src/digitalmodel/pipelay/workflow.py:L179-L186]

| Map row | Implementation path | Symbol (module-level if empty) | Recorded relation | Map evidence |
|---|---|---|---|---|
| 1 | `docs/domains/orcaflex/examples/modular/A01/A01 Catenary riser/run_orcaflex.py` | `main` | independent | [docs/registry/implementation-map.yaml:L2146-L2155] |
| 2 | `docs/domains/orcaflex/examples/modular/A01/A01 Lazy wave riser/run_orcaflex.py` | `main` | independent | [docs/registry/implementation-map.yaml:L2156-L2165] |
| 3 | `docs/domains/orcaflex/examples/modular/C10/C10 Multiple statics/run_orcaflex.py` | `main` | independent | [docs/registry/implementation-map.yaml:L2166-L2175] |
| 4 | `docs/domains/orcaflex/examples/modular/E08/PipelayConfig/run_orcaflex.py` | `main` | independent | [docs/registry/implementation-map.yaml:L2176-L2185] |
| 5 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r01_catenary` | independent | [docs/registry/implementation-map.yaml:L2186-L2195] |
| 6 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r02_lazy_wave` | independent | [docs/registry/implementation-map.yaml:L2196-L2205] |
| 7 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r05_lay_table` | independent | [docs/registry/implementation-map.yaml:L2206-L2215] |
| 8 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r06_multi_static` | independent | [docs/registry/implementation-map.yaml:L2216-L2225] |
| 9 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `extract_vessel_params` | example | [docs/registry/implementation-map.yaml:L2226-L2235] |
| 10 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `vessel_band_specs` | example | [docs/registry/implementation-map.yaml:L2236-L2247] |
| 11 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `find_required_tension` | example | [docs/registry/implementation-map.yaml:L2248-L2259] |
| 12 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `_build_infeasible_result` | example | [docs/registry/implementation-map.yaml:L2260-L2269] |
| 13 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `run_single_case` | example | [docs/registry/implementation-map.yaml:L2270-L2282] |
| 14 | `examples/demos/gtm/results_store_demo04.py` | `(module-level)` | example | [docs/registry/implementation-map.yaml:L2283-L2291] |
| 15 | `examples/demos/gtm/tests/test_demo_04_results_store.py` | `_good_case` | example | [docs/registry/implementation-map.yaml:L2292-L2301] |
| 16 | `examples/demos/gtm/tests/test_demo_04_results_store.py` | `_infeasible_case` | example | [docs/registry/implementation-map.yaml:L2302-L2311] |
| 17 | `examples/demos/gtm/tests/test_demo_04_results_store.py` | `test_999_sentinel_and_none_null_round_trip` | example | [docs/registry/implementation-map.yaml:L2312-L2324] |
| 18 | `examples/domains/workflows/04_pipeline_installation.py` | `analyze_installation_catenary` | example | [docs/registry/implementation-map.yaml:L2325-L2334] |
| 19 | `examples/workflows/riser-stackup-registry-batch/run.py` | `_schedule_row` | example | [docs/registry/implementation-map.yaml:L2335-L2344] |
| 20 | `examples/workflows/riser-stackup-registry-batch/run.py` | `_assemble_row` | example | [docs/registry/implementation-map.yaml:L2345-L2356] |
| 21 | `scripts/capabilities/build_wall_thickness_3d.py` | `(module-level)` | independent | [docs/registry/implementation-map.yaml:L2357-L2365] |
| 22 | `scripts/capabilities/build_wall_thickness_3d.py` | `_analyze_point` | independent | [docs/registry/implementation-map.yaml:L2366-L2375] |
| 23 | `scripts/capabilities/build_wall_thickness_3d.py` | `build_study` | wrapper | [docs/registry/implementation-map.yaml:L2376-L2387] |
| 24 | `scripts/capabilities/build_wall_thickness_3d_page.py` | `_axes` | independent | [docs/registry/implementation-map.yaml:L2388-L2397] |
| 25 | `scripts/capabilities/build_wall_thickness_3d_page.py` | `_pack_rows` | independent | [docs/registry/implementation-map.yaml:L2398-L2407] |
| 26 | `scripts/capabilities/build_wall_thickness_explorer.py` | `_run` | independent | [docs/registry/implementation-map.yaml:L2408-L2417] |
| 27 | `scripts/orcaflex/check_generic_scr_slwr.py` | `metrics` | independent | [docs/registry/implementation-map.yaml:L2418-L2427] |
| 28 | `scripts/orcaflex/check_generic_scr_slwr.py` | `calculate` | independent | [docs/registry/implementation-map.yaml:L2428-L2437] |
| 29 | `scripts/orcaflex/prepare_slay_campaign.py` | `catenary` | independent | [docs/registry/implementation-map.yaml:L2438-L2447] |
| 30 | `scripts/orcaflex/prepare_slay_campaign.py` | `case_spec` | independent | [docs/registry/implementation-map.yaml:L2448-L2457] |
| 31 | `scripts/orcaflex/prepare_slay_campaign.py` | `prepare_case` | independent | [docs/registry/implementation-map.yaml:L2458-L2470] |
| 32 | `scripts/orcaflex/prepare_slay_campaign.py` | `prepare` | independent | [docs/registry/implementation-map.yaml:L2471-L2484] |
| 33 | `scripts/parametric/build_orcaflex_library.py` | `main` | independent | [docs/registry/implementation-map.yaml:L2485-L2494] |
| 34 | `scripts/python/digitalmodel/analysis/APIST2RD_Methods.py` | `APISTD2RDCalculation` | independent | [docs/registry/implementation-map.yaml:L2495-L2504] |
| 35 | `scripts/python/digitalmodel/analysis/RigidPipe.py` | `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` | independent | [docs/registry/implementation-map.yaml:L2505-L2514] |
| 36 | `scripts/python/digitalmodel/analysis/RiserStackup.py` | `StackUpDiagram` | independent | [docs/registry/implementation-map.yaml:L2515-L2524] |
| 37 | `scripts/python/digitalmodel/analysis/StackUp_3000Ft.py` | `main` | independent | [docs/registry/implementation-map.yaml:L2525-L2534] |
| 38 | `scripts/python/digitalmodel/analysis/StackUp_9500Ft.py` | `main` | independent | [docs/registry/implementation-map.yaml:L2535-L2544] |
| 39 | `scripts/reporting/build_riser_pipe_comparison.py` | `evaluate` | caller | [docs/registry/implementation-map.yaml:L2545-L2556] |
| 40 | `scripts/reporting/build_riser_pipe_comparison.py` | `_validate` | independent | [docs/registry/implementation-map.yaml:L2557-L2566] |
| 41 | `scripts/reporting/build_riser_pipe_comparison.py` | `numeric_values` | independent | [docs/registry/implementation-map.yaml:L2567-L2576] |
| 42 | `scripts/reporting/build_riser_pipe_comparison.py` | `_input_table` | independent | [docs/registry/implementation-map.yaml:L2577-L2586] |
| 43 | `scripts/riser_database/build_tables.py` | `(module-level)` | independent | [docs/registry/implementation-map.yaml:L2587-L2595] |
| 44 | `src/digitalmodel/asset_integrity/custom/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` | independent | [docs/registry/implementation-map.yaml:L2596-L2605] |
| 45 | `src/digitalmodel/code_checks/dnv_os_f101.py` | `local_buckling_lcc` | independent | [docs/registry/implementation-map.yaml:L2606-L2615] |
| 46 | `src/digitalmodel/code_checks/workflow.py` | `_run_api_rp_2rd` | independent | [docs/registry/implementation-map.yaml:L2616-L2625] |
| 47 | `src/digitalmodel/code_checks/workflow.py` | `_run_dnv_os_f201` | independent | [docs/registry/implementation-map.yaml:L2626-L2635] |
| 48 | `src/digitalmodel/drilling_riser/assembly.py` | `minimum_top_tension_16q` | independent | [docs/registry/implementation-map.yaml:L2636-L2645] |
| 49 | `src/digitalmodel/drilling_riser/assembly.py` | `_unit_count` | independent | [docs/registry/implementation-map.yaml:L2646-L2655] |
| 50 | `src/digitalmodel/drilling_riser/assembly.py` | `check_rig_capability` | independent | [docs/registry/implementation-map.yaml:L2656-L2667] |
| 51 | `src/digitalmodel/drilling_riser/campaign.py` | `case_spec` | independent | [docs/registry/implementation-map.yaml:L2668-L2679] |
| 52 | `src/digitalmodel/drilling_riser/campaign.py` | `_ring_tensioners` | independent | [docs/registry/implementation-map.yaml:L2680-L2689] |
| 53 | `src/digitalmodel/drilling_riser/campaign.py` | `set_line_tension` | independent | [docs/registry/implementation-map.yaml:L2690-L2701] |
| 54 | `src/digitalmodel/drilling_riser/campaign.py` | `calibrate_line_tension` | independent | [docs/registry/implementation-map.yaml:L2702-L2715] |
| 55 | `src/digitalmodel/drilling_riser/campaign.py` | `_statics_path` | independent | [docs/registry/implementation-map.yaml:L2716-L2726] |
| 56 | `src/digitalmodel/drilling_riser/campaign.py` | `robust_statics` | independent | [docs/registry/implementation-map.yaml:L2727-L2740] |
| 57 | `src/digitalmodel/drilling_riser/campaign.py` | `_tensioner_vertical_n` | independent | [docs/registry/implementation-map.yaml:L2741-L2750] |
| 58 | `src/digitalmodel/drilling_riser/campaign.py` | `tensioner_vertical_target_n` | independent | [docs/registry/implementation-map.yaml:L2751-L2760] |
| 59 | `src/digitalmodel/drilling_riser/campaign.py` | `physical_state_checks` | independent | [docs/registry/implementation-map.yaml:L2761-L2774] |
| 60 | `src/digitalmodel/drilling_riser/campaign.py` | `RiserCampaignAdapter.prepare` | independent | [docs/registry/implementation-map.yaml:L2775-L2787] |
| 61 | `src/digitalmodel/drilling_riser/campaign.py` | `RiserCampaignAdapter.extract` | independent | [docs/registry/implementation-map.yaml:L2788-L2797] |
| 62 | `src/digitalmodel/drilling_riser/envelope.py` | `compute_operating_envelope` | independent | [docs/registry/implementation-map.yaml:L2798-L2807] |
| 63 | `src/digitalmodel/drilling_riser/global_model/build.py` | `equivalent_string_top_tension_n` | independent | [docs/registry/implementation-map.yaml:L2808-L2817] |
| 64 | `src/digitalmodel/drilling_riser/global_model/build.py` | `_tensioner_geometry` | independent | [docs/registry/implementation-map.yaml:L2818-L2827] |
| 65 | `src/digitalmodel/drilling_riser/global_model/build.py` | `winch_tension_n` | independent | [docs/registry/implementation-map.yaml:L2828-L2839] |
| 66 | `src/digitalmodel/drilling_riser/global_model/build.py` | `build_generic_spec` | independent | [docs/registry/implementation-map.yaml:L2840-L2852] |
| 67 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `effective_tension_chain` | independent | [docs/registry/implementation-map.yaml:L2853-L2862] |
| 68 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `tension_references` | independent | [docs/registry/implementation-map.yaml:L2863-L2874] |
| 69 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `tensioner_lateral_stiffness_n_m` | independent | [docs/registry/implementation-map.yaml:L2875-L2884] |
| 70 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `reference_periods` | caller | [docs/registry/implementation-map.yaml:L2885-L2896] |
| 71 | `src/digitalmodel/drilling_riser/global_model/hang_off.py` | `hung_weight_n` | independent | [docs/registry/implementation-map.yaml:L2897-L2907] |
| 72 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `OpenWaterRiserSpec._closes` | independent | [docs/registry/implementation-map.yaml:L2908-L2917] |
| 73 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `build_open_water_generic_spec` | independent | [docs/registry/implementation-map.yaml:L2918-L2927] |
| 74 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `tension_references` | independent | [docs/registry/implementation-map.yaml:L2928-L2938] |
| 75 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `reference_periods` | independent | [docs/registry/implementation-map.yaml:L2939-L2948] |
| 76 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `release_reference` | independent | [docs/registry/implementation-map.yaml:L2949-L2960] |
| 77 | `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `ring_vertical_residual_n` | independent | [docs/registry/implementation-map.yaml:L2961-L2970] |
| 78 | `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `open_water_physical_checks` | independent | [docs/registry/implementation-map.yaml:L2971-L2980] |
| 79 | `src/digitalmodel/drilling_riser/global_model/spec.py` | `Tensioners._failed` | independent | [docs/registry/implementation-map.yaml:L2981-L2990] |
| 80 | `src/digitalmodel/drilling_riser/global_model/spec.py` | `RiserGlobalModelSpec._geometry_closes` | independent | [docs/registry/implementation-map.yaml:L2991-L3000] |
| 81 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `_missing_open_water` | independent | [docs/registry/implementation-map.yaml:L3001-L3010] |
| 82 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `missing_channels` | independent | [docs/registry/implementation-map.yaml:L3011-L3022] |
| 83 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `extract` | independent | [docs/registry/implementation-map.yaml:L3023-L3032] |
| 84 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `extract_open_water` | independent | [docs/registry/implementation-map.yaml:L3033-L3042] |
| 85 | `src/digitalmodel/drilling_riser/postprocess/checks.py` | `t_min` | independent | [docs/registry/implementation-map.yaml:L3043-L3052] |
| 86 | `src/digitalmodel/drilling_riser/postprocess/checks.py` | `tension_setting_max` | independent | [docs/registry/implementation-map.yaml:L3053-L3062] |
| 87 | `src/digitalmodel/drilling_riser/postprocess/checks.py` | `tensioner_stroke` | independent | [docs/registry/implementation-map.yaml:L3063-L3072] |
| 88 | `src/digitalmodel/drilling_riser/schedule_assembly.py` | `ScheduleAssembly.minimum_top_tension_16q_kn` | independent | [docs/registry/implementation-map.yaml:L3073-L3082] |
| 89 | `src/digitalmodel/drilling_riser/schedule_assembly.py` | `_load_oc_assembly` | independent | [docs/registry/implementation-map.yaml:L3083-L3092] |
| 90 | `src/digitalmodel/drilling_riser/stackup.py` | `effective_tension` | independent | [docs/registry/implementation-map.yaml:L3093-L3102] |
| 91 | `src/digitalmodel/drilling_riser/telemetry_inputs.py` | `parse_snapshots` | independent | [docs/registry/implementation-map.yaml:L3103-L3112] |
| 92 | `src/digitalmodel/drilling_riser/telemetry_inputs.py` | `snapshot_to_tension_n` | independent | [docs/registry/implementation-map.yaml:L3113-L3122] |
| 93 | `src/digitalmodel/drilling_riser/tsj_workflow.py` | `router` | independent | [docs/registry/implementation-map.yaml:L3123-L3135] |
| 94 | `src/digitalmodel/drilling_riser/tsj_workflow.py` | `_section_result` | independent | [docs/registry/implementation-map.yaml:L3136-L3145] |
| 95 | `src/digitalmodel/drilling_riser/tsj_workflow.py` | `_effective_tension_n` | independent | [docs/registry/implementation-map.yaml:L3146-L3155] |
| 96 | `src/digitalmodel/drilling_riser/workflow.py` | `router` | caller | [docs/registry/implementation-map.yaml:L3156-L3168] |
| 97 | `src/digitalmodel/drilling_riser/workflow.py` | `_write_profile_csv` | independent | [docs/registry/implementation-map.yaml:L3169-L3178] |
| 98 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_riser_length_to_shear7_result` | independent | [docs/registry/implementation-map.yaml:L3179-L3188] |
| 99 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` | independent | [docs/registry/implementation-map.yaml:L3189-L3198] |
| 100 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` | independent | [docs/registry/implementation-map.yaml:L3199-L3208] |
| 101 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.save_fatigue_summary_result` | independent | [docs/registry/implementation-map.yaml:L3209-L3218] |
| 102 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_fatigue_damage_results_to_df` | independent | [docs/registry/implementation-map.yaml:L3219-L3231] |
| 103 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` | independent | [docs/registry/implementation-map.yaml:L3232-L3241] |
| 104 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` | independent | [docs/registry/implementation-map.yaml:L3242-L3251] |
| 105 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.orcaflex_model` | independent | [docs/registry/implementation-map.yaml:L3252-L3264] |
| 106 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.customize_fj_types` | independent | [docs/registry/implementation-map.yaml:L3265-L3274] |
| 107 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.links` | wrapper | [docs/registry/implementation-map.yaml:L3275-L3287] |
| 108 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.add_support_spring` | independent | [docs/registry/implementation-map.yaml:L3288-L3300] |
| 109 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.add_tensioners` | independent | [docs/registry/implementation-map.yaml:L3301-L3311] |
| 110 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.tensioner_vessel_locations` | independent | [docs/registry/implementation-map.yaml:L3312-L3321] |
| 111 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.spring_properties` | independent | [docs/registry/implementation-map.yaml:L3322-L3331] |
| 112 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.get_connection_definitions` | independent | [docs/registry/implementation-map.yaml:L3332-L3341] |
| 113 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.get_fj_line_definitions` | independent | [docs/registry/implementation-map.yaml:L3342-L3351] |
| 114 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.get_line_items_vertical` | independent | [docs/registry/implementation-map.yaml:L3352-L3361] |
| 115 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.prepare_stack_up_properties_df` | caller | [docs/registry/implementation-map.yaml:L3362-L3377] |
| 116 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_elevation_from_endA_in_df` | independent | [docs/registry/implementation-map.yaml:L3378-L3387] |
| 117 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_shear7_x_by_L_in_df` | independent | [docs/registry/implementation-map.yaml:L3388-L3397] |
| 118 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_elevation_relative_to_mudline_in_df` | independent | [docs/registry/implementation-map.yaml:L3398-L3407] |
| 119 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_elevation_relative_to_MSL_in_df` | independent | [docs/registry/implementation-map.yaml:L3408-L3417] |
| 120 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_inservice_weights_and_tensions` | caller | [docs/registry/implementation-map.yaml:L3418-L3432] |
| 121 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.assign_key_properties_to_df` | independent | [docs/registry/implementation-map.yaml:L3433-L3442] |
| 122 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_stack_in_service_weights` | independent | [docs/registry/implementation-map.yaml:L3443-L3452] |
| 123 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_effective_tension` | independent | [docs/registry/implementation-map.yaml:L3453-L3462] |
| 124 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_stretch_in_riser` | independent | [docs/registry/implementation-map.yaml:L3463-L3472] |
| 125 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.title` | independent | [docs/registry/implementation-map.yaml:L3473-L3482] |
| 126 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.riser_joints_to_shear7_zones` | independent | [docs/registry/implementation-map.yaml:L3483-L3492] |
| 127 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.add_block_3` | independent | [docs/registry/implementation-map.yaml:L3493-L3502] |
| 128 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_riser_length_to_shear7_result` | independent | [docs/registry/implementation-map.yaml:L3503-L3512] |
| 129 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` | independent | [docs/registry/implementation-map.yaml:L3513-L3522] |
| 130 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` | independent | [docs/registry/implementation-map.yaml:L3523-L3532] |
| 131 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.save_fatigue_summary_result` | independent | [docs/registry/implementation-map.yaml:L3533-L3542] |
| 132 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_fatigue_damage_results_to_df` | copy | [docs/registry/implementation-map.yaml:L3543-L3555] |
| 133 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` | independent | [docs/registry/implementation-map.yaml:L3556-L3565] |
| 134 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` | independent | [docs/registry/implementation-map.yaml:L3566-L3575] |
| 135 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py` | `_write_tensions_csv` | independent | [docs/registry/implementation-map.yaml:L3576-L3585] |
| 136 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._line_state` | independent | [docs/registry/implementation-map.yaml:L3586-L3597] |
| 137 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._solve_line` | independent | [docs/registry/implementation-map.yaml:L3598-L3607] |
| 138 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._result` | independent | [docs/registry/implementation-map.yaml:L3608-L3617] |
| 139 | `src/digitalmodel/orcaflex/mooring_design.py` | `solve_catenary` | independent | [docs/registry/implementation-map.yaml:L3618-L3627] |
| 140 | `src/digitalmodel/orcaflex/pipelay_analysis.py` | `JLayConfig.calculate_sagbend` | independent | [docs/registry/implementation-map.yaml:L3628-L3637] |
| 141 | `src/digitalmodel/orcaflex/riser_config.py` | `SCRDesignInput.calculate_geometry` | independent | [docs/registry/implementation-map.yaml:L3638-L3647] |
| 142 | `src/digitalmodel/orcaflex/riser_config.py` | `TTRDesignInput.calculate_stroke` | independent | [docs/registry/implementation-map.yaml:L3648-L3657] |
| 143 | `src/digitalmodel/orcaflex/riser_config.py` | `TTRDesignInput.calculate_top_tension` | independent | [docs/registry/implementation-map.yaml:L3658-L3667] |
| 144 | `src/digitalmodel/orcaflex/riser_input_schema.py` | `_TopTensionedInput.to_legacy` | independent | [docs/registry/implementation-map.yaml:L3668-L3677] |
| 145 | `src/digitalmodel/orcaflex/viv_screening.py` | `BeamProperties.natural_frequency` | independent | [docs/registry/implementation-map.yaml:L3678-L3687] |
| 146 | `src/digitalmodel/parametric/generate.py` | `_code_check_utilisation` | independent | [docs/registry/implementation-map.yaml:L3688-L3697] |
| 147 | `src/digitalmodel/pipelay/workflow.py` | `run_jlay` | independent | [docs/registry/implementation-map.yaml:L3698-L3707] |
| 148 | `src/digitalmodel/riser_configuration_screen/static.py` | `_lazy_row` | independent | [docs/registry/implementation-map.yaml:L3708-L3717] |
| 149 | `src/digitalmodel/riser_configuration_screen/static.py` | `scr_offsets` | independent | [docs/registry/implementation-map.yaml:L3718-L3727] |
| 150 | `src/digitalmodel/riser_configuration_screen/workflow.py` | `_validate` | independent | [docs/registry/implementation-map.yaml:L3728-L3737] |
| 151 | `src/digitalmodel/riser_configuration_screen/workflow.py` | `_utilisation` | independent | [docs/registry/implementation-map.yaml:L3738-L3747] |
| 152 | `src/digitalmodel/riser_configuration_screen/workflow.py` | `_checks` | independent | [docs/registry/implementation-map.yaml:L3748-L3757] |
| 153 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/groups_builder.py` | `GroupsBuilder._build_from_spec` | independent | [docs/registry/implementation-map.yaml:L3758-L3767] |
| 154 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py` | `LineTypeBuilder.build` | independent | [docs/registry/implementation-map.yaml:L3768-L3777] |
| 155 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py` | `WinchBuilder.should_generate` | independent | [docs/registry/implementation-map.yaml:L3778-L3787] |
| 156 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py` | `WinchBuilder.build` | independent | [docs/registry/implementation-map.yaml:L3788-L3797] |
| 157 | `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` | `_apply_overrides` | independent | [docs/registry/implementation-map.yaml:L3798-L3807] |
| 158 | `src/digitalmodel/solvers/orcaflex/mooring.py` | `Mooring.mooring_pre_tension_analysis` | independent | [docs/registry/implementation-map.yaml:L3808-L3819] |
| 159 | `src/digitalmodel/solvers/orcaflex/mooring.py` | `Mooring.evaluate_line_length_next_iteration` | independent | [docs/registry/implementation-map.yaml:L3820-L3829] |
| 160 | `src/digitalmodel/solvers/orcaflex/opp_range_graph.py` | `OPPRangeGraph.postProcessRange` | independent | [docs/registry/implementation-map.yaml:L3830-L3839] |
| 161 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.postProcessRange` | independent | [docs/registry/implementation-map.yaml:L3840-L3849] |
| 162 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.orcaflex_model` | independent | [docs/registry/implementation-map.yaml:L3850-L3862] |
| 163 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.customize_fj_types` | copy | [docs/registry/implementation-map.yaml:L3863-L3872] |
| 164 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.links` | wrapper | [docs/registry/implementation-map.yaml:L3873-L3885] |
| 165 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.add_support_spring` | copy | [docs/registry/implementation-map.yaml:L3886-L3898] |
| 166 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.add_tensioners` | copy | [docs/registry/implementation-map.yaml:L3899-L3909] |
| 167 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.tensioner_vessel_locations` | copy | [docs/registry/implementation-map.yaml:L3910-L3919] |
| 168 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.spring_properties` | independent | [docs/registry/implementation-map.yaml:L3920-L3929] |
| 169 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.get_connection_definitions` | copy | [docs/registry/implementation-map.yaml:L3930-L3939] |
| 170 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.get_fj_line_definitions` | copy | [docs/registry/implementation-map.yaml:L3940-L3949] |
| 171 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.get_line_items_vertical` | independent | [docs/registry/implementation-map.yaml:L3950-L3959] |
| 172 | `src/digitalmodel/structural/analysis/cli.py` | `check` | independent | [docs/registry/implementation-map.yaml:L3960-L3969] |
| 173 | `src/digitalmodel/structural/analysis/wall_thickness.py` | `WallThicknessAnalyzer.check_combined_loading` | independent | [docs/registry/implementation-map.yaml:L3970-L3979] |
| 174 | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py` | `ApiStd2rdStrategy._check_combined` | independent | [docs/registry/implementation-map.yaml:L3980-L3989] |
| 175 | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py` | `DnvStF101Strategy._check_combined_loading` | independent | [docs/registry/implementation-map.yaml:L3990-L3999] |
| 176 | `src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py` | `run_wall_thickness_compare` | independent | [docs/registry/implementation-map.yaml:L4000-L4009] |
| 177 | `src/digitalmodel/structural/analysis/wall_thickness_lookup.py` | `MTLookupGenerator.generate` | independent | [docs/registry/implementation-map.yaml:L4010-L4019] |
| 178 | `src/digitalmodel/structural/analysis/wall_thickness_parametric.py` | `ParametricSweep.run` | independent | [docs/registry/implementation-map.yaml:L4020-L4029] |
| 179 | `src/digitalmodel/structural/analysis/wall_thickness_phases.py` | `PipelinePhase.to_design_loads` | independent | [docs/registry/implementation-map.yaml:L4030-L4039] |
| 180 | `src/digitalmodel/structural/analysis/wall_thickness_phases.py` | `create_standard_phases` | independent | [docs/registry/implementation-map.yaml:L4040-L4049] |
| 181 | `src/digitalmodel/structural/fatigue_apps/examples/load_scaling_runner.py` | `LoadScalingProcessor.process_all_conditions` | independent | [docs/registry/implementation-map.yaml:L4050-L4059] |
| 182 | `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `IntegratedFatigueProcessor.process_fatigue_condition` | independent | [docs/registry/implementation-map.yaml:L4060-L4069] |
| 183 | `src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py` | `IntegratedFatigueProcessorWithNaming._process_condition_base` | independent | [docs/registry/implementation-map.yaml:L4070-L4079] |
| 184 | `src/digitalmodel/structural/fatigue_apps/load_scaling.py` | `LoadScalingProcessor.load_reference_data` | independent | [docs/registry/implementation-map.yaml:L4080-L4089] |
| 185 | `src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py` | `ReferenceSeaStateProcessor.process_fatigue_condition` | independent | [docs/registry/implementation-map.yaml:L4090-L4099] |
| 186 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `LoadScaler.process_fatigue_condition` | independent | [docs/registry/implementation-map.yaml:L4100-L4109] |
| 187 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `LoadScaler.process_sea_state` | independent | [docs/registry/implementation-map.yaml:L4110-L4119] |
| 188 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `main` | independent | [docs/registry/implementation-map.yaml:L4120-L4129] |
| 189 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor_with_progress.py` | `ProductionDataHandlerWithProgress.process_single_analysis` | independent | [docs/registry/implementation-map.yaml:L4130-L4139] |
| 190 | `src/digitalmodel/structural/parametric_coordinator.py` | `ParametricCoordinator._run_wall_thickness` | independent | [docs/registry/implementation-map.yaml:L4140-L4149] |
| 191 | `src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` | copy | [docs/registry/implementation-map.yaml:L4150-L4159] |
| 192 | `src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py` | `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` | copy | [docs/registry/implementation-map.yaml:L4160-L4169] |
| 193 | `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` | copy | [docs/registry/implementation-map.yaml:L4170-L4179] |
| 194 | `src/digitalmodel/subsea/catenary_riser/cli.py` | `simple_catenary` | independent | [docs/registry/implementation-map.yaml:L4180-L4189] |
| 195 | `src/digitalmodel/subsea/catenary_riser/cli.py` | `lazy_wave_analysis` | independent | [docs/registry/implementation-map.yaml:L4190-L4199] |
| 196 | `src/digitalmodel/subsea/catenary_riser/lazy_wave.py` | `LazyWaveAnalyzer.analyze` | independent | [docs/registry/implementation-map.yaml:L4200-L4209] |
| 197 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer.analyze_riser` | caller | [docs/registry/implementation-map.yaml:L4210-L4222] |
| 198 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer._solve_catenary_from_H` | independent | [docs/registry/implementation-map.yaml:L4223-L4232] |
| 199 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer._solve_H_from_top_tension` | independent | [docs/registry/implementation-map.yaml:L4233-L4242] |
| 200 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer.check_design_limits` | independent | [docs/registry/implementation-map.yaml:L4243-L4252] |
| 201 | `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `developed_min_radius` | independent | [docs/registry/implementation-map.yaml:L4253-L4262] |
| 202 | `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `required_effective_ei` | independent | [docs/registry/implementation-map.yaml:L4263-L4272] |
| 203 | `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `screen_bend_stiffener` | independent | [docs/registry/implementation-map.yaml:L4273-L4285] |
| 204 | `src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py` | `armour_tension_stress` | independent | [docs/registry/implementation-map.yaml:L4286-L4295] |
| 205 | `src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py` | `verify_flexible_pipe` | independent | [docs/registry/implementation-map.yaml:L4296-L4307] |
| 206 | `src/digitalmodel/subsea/pipeline/global_buckling/hobbs_lateral.py` | `effective_driving_force` | independent | [docs/registry/implementation-map.yaml:L4308-L4317] |
| 207 | `src/digitalmodel/subsea/riser/riser_case_screening.py` | `RiserLoadCase.to_loads` | independent | [docs/registry/implementation-map.yaml:L4318-L4327] |
| 208 | `src/digitalmodel/subsea/riser/riser_case_screening.py` | `run_riser_screening` | independent | [docs/registry/implementation-map.yaml:L4328-L4337] |
| 209 | `src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py` | `StackUpDiagram` | independent | [docs/registry/implementation-map.yaml:L4338-L4347] |
| 210 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.prepare_riser_model` | caller | [docs/registry/implementation-map.yaml:L4348-L4362] |
| 211 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.read_riser_input_data` | independent | [docs/registry/implementation-map.yaml:L4363-L4372] |
| 212 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.assign_key_properties_to_self` | independent | [docs/registry/implementation-map.yaml:L4373-L4382] |
| 213 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.get_fea_properties` | independent | [docs/registry/implementation-map.yaml:L4383-L4392] |
| 214 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.prepare_tensioner_data` | independent | [docs/registry/implementation-map.yaml:L4393-L4402] |
| 215 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.evaluate_stack_up_properties` | independent | [docs/registry/implementation-map.yaml:L4403-L4412] |
| 216 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.save_data` | independent | [docs/registry/implementation-map.yaml:L4413-L4422] |
| 217 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.alter_properties_for_Shear7_software` | independent | [docs/registry/implementation-map.yaml:L4423-L4432] |
| 218 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.calculate_natural_frequency` | caller | [docs/registry/implementation-map.yaml:L4433-L4444] |
| 219 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator._frequency_tension_controlled` | independent | [docs/registry/implementation-map.yaml:L4445-L4454] |
| 220 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.calculate_effective_tension` | independent | [docs/registry/implementation-map.yaml:L4455-L4464] |
| 221 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.check_stability` | caller | [docs/registry/implementation-map.yaml:L4465-L4476] |
| 222 | `src/digitalmodel/viv_screening/inputs.py` | `_beam` | independent | [docs/registry/implementation-map.yaml:L4477-L4486] |
| 223 | `src/digitalmodel/workflows/workflow_automation/cli.py` | `riser_analysis_cmd` | independent | [docs/registry/implementation-map.yaml:L4487-L4496] |
| 224 | `src/digitalmodel/workflows/workflow_automation/templates.py` | `create_riser_analysis_template` | independent | [docs/registry/implementation-map.yaml:L4497-L4506] |
| 225 | `src/digitalmodel/workflows/workflow_automation/workflows.py` | `CompleteRiserAnalysisWorkflow.create` | independent | [docs/registry/implementation-map.yaml:L4507-L4516] |
| 226 | `tests/asset_integrity/test_calculations.py` | `_make_pipe_cfg` | test_helper | [docs/registry/implementation-map.yaml:L4517-L4526] |
| 227 | `tests/capabilities/test_wall_thickness_3d_page.py` | `_study` | test_helper | [docs/registry/implementation-map.yaml:L4527-L4536] |
| 228 | `tests/contracts/test_force_units_boundary_contract.py` | `(module-level)` | test_helper | [docs/registry/implementation-map.yaml:L4537-L4545] |
| 229 | `tests/contracts/test_force_units_boundary_contract.py` | `test_envelope_von_mises_n_to_kn_seam` | test_helper | [docs/registry/implementation-map.yaml:L4546-L4557] |
| 230 | `tests/drilling_riser/global_model/conftest.py` | `synthetic_spec` | test_helper | [docs/registry/implementation-map.yaml:L4558-L4567] |
| 231 | `tests/drilling_riser/global_model/test_batch_params.py` | `test_physical_checks_use_the_remaining_tensioners` | test_helper | [docs/registry/implementation-map.yaml:L4568-L4578] |
| 232 | `tests/drilling_riser/global_model/test_campaign.py` | `_fake` | test_helper | [docs/registry/implementation-map.yaml:L4579-L4589] |
| 233 | `tests/drilling_riser/global_model/test_open_water.py` | `open_water_dict` | test_helper | [docs/registry/implementation-map.yaml:L4590-L4599] |
| 234 | `tests/drilling_riser/global_model/test_open_water_campaign.py` | `_doc` | test_helper | [docs/registry/implementation-map.yaml:L4600-L4609] |
| 235 | `tests/drilling_riser/global_model/test_orcaflex_solver.py` | `test_statics_tension_and_modes_agree_with_hand_checks` | test_helper | [docs/registry/implementation-map.yaml:L4610-L4623] |
| 236 | `tests/drilling_riser/global_model/test_statics_path.py` | `test_branch_tolerance_and_ring_buoyancy` | test_helper | [docs/registry/implementation-map.yaml:L4624-L4634] |
| 237 | `tests/drilling_riser/global_model/test_tensioner_representation.py` | `_vertical` | test_helper | [docs/registry/implementation-map.yaml:L4635-L4644] |
| 238 | `tests/drilling_riser/postprocess/test_aggregate.py` | `_mc` | test_helper | [docs/registry/implementation-map.yaml:L4645-L4654] |
| 239 | `tests/drilling_riser/postprocess/test_aggregate.py` | `_case` | test_helper | [docs/registry/implementation-map.yaml:L4655-L4666] |
| 240 | `tests/drilling_riser/test_drive_off_worked_example.py` | `_recorded_arguments` | test_helper | [docs/registry/implementation-map.yaml:L4667-L4676] |
| 241 | `tests/drilling_riser/test_schedule_assembly.py` | `_deltas` | test_helper | [docs/registry/implementation-map.yaml:L4677-L4686] |
| 242 | `tests/infrastructure/unit/test_pipe_capacity_codes.py` | `_base_cfg` | test_helper | [docs/registry/implementation-map.yaml:L4687-L4696] |
| 243 | `tests/orcaflex/test_generic_scr_slwr.py` | `test_analytical_reference_reproduces_from_basis` | test_helper | [docs/registry/implementation-map.yaml:L4697-L4706] |
| 244 | `tests/parametric/test_atlas.py` | `_code_check_atlas` | test_helper | [docs/registry/implementation-map.yaml:L4707-L4716] |
| 245 | `tests/reporting/test_riser_pipe_comparison.py` | `reference_input` | test_helper | [docs/registry/implementation-map.yaml:L4717-L4726] |
| 246 | `tests/riser_configuration_screen/test_workflow.py` | `settings` | test_helper | [docs/registry/implementation-map.yaml:L4727-L4736] |
| 247 | `tests/solvers/orcaflex/analysis/scripts/verify_configs.py` | `verify_configuration` | test_helper | [docs/registry/implementation-map.yaml:L4737-L4746] |
| 248 | `tests/solvers/orcaflex/modular_generator/builders/test_buoys_builder.py` | `_make_slay_spec` | test_helper | [docs/registry/implementation-map.yaml:L4747-L4756] |
| 249 | `tests/solvers/orcaflex/modular_generator/builders/test_lines_builder.py` | `_make_pipeline_spec` | test_helper | [docs/registry/implementation-map.yaml:L4757-L4766] |
| 250 | `tests/solvers/orcaflex/modular_generator/builders/test_linetype_builder.py` | `_make_pipeline_spec` | test_helper | [docs/registry/implementation-map.yaml:L4767-L4776] |
| 251 | `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` | `_write_slay_campaign` | test_helper | [docs/registry/implementation-map.yaml:L4777-L4786] |
| 252 | `tests/solvers/orcaflex/modular_generator/schema/test_campaign.py` | `_make_base_spec_data` | test_helper | [docs/registry/implementation-map.yaml:L4787-L4796] |
| 253 | `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py` | `extract_static_range_results` | test_helper | [docs/registry/implementation-map.yaml:L4797-L4806] |
| 254 | `tests/solvers/orcaflex/modular_generator/test_post_validator.py` | `_make_valid_model` | test_helper | [docs/registry/implementation-map.yaml:L4807-L4816] |
| 255 | `tests/solvers/orcaflex/test_slay_layout.py` | `test_support_types_are_defined_before_vessel_references` | test_helper | [docs/registry/implementation-map.yaml:L4817-L4826] |
| 256 | `tests/specialized/umbilical_analysis/conftest.py` | `installation_parameters` | test_helper | [docs/registry/implementation-map.yaml:L4827-L4836] |
| 257 | `tests/subsea/catenary_riser/test_catenary.py` | `run_catenary_riser` | test_helper | [docs/registry/implementation-map.yaml:L4837-L4846] |
| 258 | `tests/test_automation/fixtures/engineering_fixtures.py` | `get_catenary_analysis_mock` | test_helper | [docs/registry/implementation-map.yaml:L4847-L4856] |
| 259 | `tests/test_automation/scripts/batch_fix_tests.py` | `(module-level)` | test_helper | [docs/registry/implementation-map.yaml:L4857-L4865] |
| 260 | `tests/test_rainflow_with_effective_tension.py` | `process_effective_tension_file` | test_helper | [docs/registry/implementation-map.yaml:L4866-L4875] |
| 261 | `tests/test_rainflow_with_effective_tension.py` | `main` | test_helper | [docs/registry/implementation-map.yaml:L4876-L4887] |
| 262 | `tests/test_wall_thickness_compare_workflow.py` | `_cfg` | test_helper | [docs/registry/implementation-map.yaml:L4888-L4897] |
| 263 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | test_helper | [docs/registry/implementation-map.yaml:L4898-L4907] |
| 264 | `tests/workflows/test_viv_screening_workflow.py` | `screening_input` | test_helper | [docs/registry/implementation-map.yaml:L4908-L4917] |

Table 11. All implementation-map rows of the sole counted family, in map order; a map row is not a verified solver result.


## 10. Coverage and open items

| Tracking | Remaining scope |
|---|---|
| [2763](https://github.com/vamseeachanta/digitalmodel/issues/2763) | Explicit units, destination confirmation and design-basis provenance beyond schema checks |
| [2764](https://github.com/vamseeachanta/digitalmodel/issues/2764) | Durable issued input/output digest manifest beyond C07 tests |
| [2765](https://github.com/vamseeachanta/digitalmodel/issues/2765) | Independent physical benchmarks beyond analytical hand anchors |
| [2766](https://github.com/vamseeachanta/digitalmodel/issues/2766) | Canonical family-map reconciliation |
| [2767](https://github.com/vamseeachanta/digitalmodel/issues/2767) | Review responsibilities, shared lesson and clearance index |
| [1817](https://github.com/vamseeachanta/digitalmodel/issues/1817) | Exact standard edition/clause and standards-output provenance qualification; S-lay expected failure |
| [2431](https://github.com/vamseeachanta/digitalmodel/issues/2431) | Native installation stages, dynamics and issued-report qualification |

Table 7. Filed tracking replaces the five unfiled drafts. These broader
qualification items are distinct from the 13 bounded repair sections in audit 2745.
