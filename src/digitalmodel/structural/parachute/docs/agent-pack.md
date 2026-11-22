# Parachute calculation knowledge pack

| Header | Value |
| --- | --- |
| Module | structural/parachute |
| Repository | digitalmodel |
| Package | src/digitalmodel/structural/parachute |
| Pack revision | 2 |
| Code revision | 21942e8cfc7dfb985c5bfa693245df7d197cf76c; viewer and viewer-test locators updated for #1684 |
| Analysis family | Engineering calculations (EC); AQG r0 Python calculations |
| Quality role | Engineering calculations family quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The calculation path returns circular-canopy drag, summed dual-canopy drag, aerodynamic lift, tire traction, load-case summaries and a reduced-order deployment history. [src/digitalmodel/structural/parachute/parachute_drag.py:L37-L74] [src/digitalmodel/structural/parachute/chute_assessment.py:L81-L128] [src/digitalmodel/structural/parachute/chute_assessment.py:L131-L293]

The structural path contains a two-dimensional axial/bending frame solver, tube section properties, member stresses and bolt, weld and pin shear checks; the three-dimensional path supplies geometry, CAD exports and an HTML viewer. [src/digitalmodel/structural/parachute/frame_solver.py:L21-L25] [src/digitalmodel/structural/parachute/frame_model.py:L23-L46] [src/digitalmodel/structural/parachute/member_check.py:L22-L209] [src/digitalmodel/structural/parachute/freecad_frame_builder.py:L137-L185] [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L185-L203]

The routing registry describes the structural domain as stable; this is routing metadata, not evidence of physical qualification of a particular frame or canopy. [docs/registry/module-routing.yaml:L515-L520] [src/digitalmodel/structural/parachute/docs/worked-synthetic-canopy-drag.md:L179-L188]

Scope includes every Python file in this package, with no separately excluded subpackage; the synthetic example explicitly excludes equipment selection, measured coefficients, frame qualification and deployment safety recommendations. [src/digitalmodel/structural/parachute/__init__.py:L1-L21] [src/digitalmodel/structural/parachute/docs/worked-synthetic-canopy-drag.md:L18-L28]

## 2. Inputs and outputs

Input contract (the caller supplies a consistent unit system for the general frame solver): [src/digitalmodel/structural/parachute/frame_solver.py:L120-L138]

| Input | Unit | Allowed range enforced | Default | Source and evidence |
| --- | --- | --- | --- | --- |
| `speed_mph`, `chute_diameter_ft` | mph, ft | Drag rejects negative speed; the area helper rejects nonpositive diameter, but the drag entry point substitutes zero area | Required | [src/digitalmodel/structural/parachute/parachute_drag.py:L25-L62] |
| `cd`, `cx`, `rho` | 1, 1, slug/ft³ | No sign or finiteness checks | Required | [src/digitalmodel/structural/parachute/parachute_drag.py:L37-L63] |
| Tube outer diameter and wall | in | Wall must be positive and less than half the outer diameter | Geometry presets exist; no qualified design default asserted here | [src/digitalmodel/structural/parachute/frame_model.py:L23-L65] |
| Nodes, member connectivity, fixed nodes, `(fx, fy, mz)`, `E` | Consistent length, force, force-length, force/area | Zero-length elements rejected; all three degrees at fixed nodes restrained | Required by `solve_frame` | [src/digitalmodel/structural/parachute/frame_solver.py:L27-L45] [src/digitalmodel/structural/parachute/frame_solver.py:L120-L138] [src/digitalmodel/structural/parachute/frame_solver.py:L181-L197] |
| Axial, shear, moment, area, inertia, diameter | lbf, lbf, in-lbf, in², in⁴, in | Nonpositive area or inertia silently yields zero component stress | Required by `tube_stress` | [src/digitalmodel/structural/parachute/member_check.py:L22-L49] |
| Connection geometry, count, allowable | in, count, psi | Nonpositive effective area or capacity produces infinity | Required | [src/digitalmodel/structural/parachute/member_check.py:L74-L155] |
| Weight, lift, downforce, friction coefficient | lbf, lbf, lbf, 1 | No positivity checks; negative normal force sets a liftoff flag | Assessment presets include zero downforce | [src/digitalmodel/structural/parachute/chute_assessment.py:L114-L143] |
| Transient initial speed, weight, duration, time step | mph, lbf, s, s | All four must be positive | Required; drag and inflation parameters have presets | [src/digitalmodel/structural/parachute/chute_assessment.py:L189-L218] |
| CAD environment: tube diameter, wall, bend radius; output directory | in; directory | Python `float` conversion; section wall checks downstream | Environment presets; directory defaults to current directory | [src/digitalmodel/structural/parachute/generate_frame_cad.py:L9-L44] |

Output contract: [src/digitalmodel/structural/parachute/parachute_drag.py:L12-L22] [src/digitalmodel/structural/parachute/chute_assessment.py:L22-L69]

| Output | Unit and representation | Range/default behavior | Evidence |
| --- | --- | --- | --- |
| `DragResult`, `DualChuteResult` | lbf, N; speed mph/ft/s and area ft² for single drag | Computed from caller inputs; rounded conversion factor | [src/digitalmodel/structural/parachute/parachute_drag.py:L9-L22] [src/digitalmodel/structural/parachute/parachute_drag.py:L60-L74] [src/digitalmodel/structural/parachute/chute_assessment.py:L81-L100] |
| `FrameResult` | Displacement length, rotation radians; reactions force/moment; local member end forces | Sorted node-ID order, three degrees per node | [src/digitalmodel/structural/parachute/frame_solver.py:L103-L117] [src/digitalmodel/structural/parachute/frame_solver.py:L140-L145] [src/digitalmodel/structural/parachute/frame_solver.py:L199-L230] |
| Member/connection dictionaries | Stress psi; area in²; unity dimensionless; Boolean pass | Member pass uses unity at most 1; empty input members yield an empty result list | [src/digitalmodel/structural/parachute/member_check.py:L158-L209] |
| Deployment history | s, mph, lbf, g; impulse lbf-s | Explicit speed update clips at zero; final sample at requested duration | [src/digitalmodel/structural/parachute/chute_assessment.py:L55-L69] [src/digitalmodel/structural/parachute/chute_assessment.py:L217-L293] |
| YAML string | Weight/forces lbf and force N, speed mph; forces rounded to one decimal | Governing case required; traction/history not exported | [src/digitalmodel/structural/parachute/chute_assessment.py:L296-L321] |
| CAD and viewer | STEP, IGES, native CAD document, JSON embedded in HTML | Coordinates passed through; pinned CDN by default, or digest-verified local three.js modules and MIT notice inlined for offline HTML | [src/digitalmodel/structural/parachute/freecad_frame_builder.py:L168-L185] [src/digitalmodel/structural/parachute/generate_frame_cad.py:L38-L51] [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L22-L26] [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L127-L203] |

The two-dimensional preset uses a transverse/vertical frame with upward-positive vertical coordinates and an origin at the left mount; the three-dimensional preset uses right-positive transverse, forward-positive longitudinal and up-positive vertical coordinates with an origin at the shared junction. [src/digitalmodel/structural/parachute/frame_model.py:L66-L80] [src/digitalmodel/structural/parachute/frame_geometry_3d.py:L113-L125]

Axial stress accepts tension-positive force; element rotations use the stated local/global transformation, and no harmonic phase convention is defined for these static and time-marching calculations. [src/digitalmodel/structural/parachute/member_check.py:L26-L38] [src/digitalmodel/structural/parachute/frame_solver.py:L52-L117] [src/digitalmodel/structural/parachute/chute_assessment.py:L204-L208]

UNVERIFIED: U1 — the physical applicability of empirical defaults, sizing limits and preset geometry has not been established by independent measurements; the transient holds the opening-shock multiplier throughout the history (SF77), affecting impulse and speed decay; issue pending (3).

UNVERIFIED: U2 — CAD exchange-unit scaling remains unverified by a CAD round trip; issue pending (2). Viewer bend midpoints match the sag apex within 1e-3 in in supplemental native Chromium; Edge/Chrome acceptance remains open.

## 3. Method and governing standards

| Method | Implementation and calculation basis | Governing-source status |
| --- | --- | --- |
| Circular drag | `calculate_drag_force`: speed conversion, circular projected area, then `rho*V²*cd*A*cx/2`; no inflation history in this function | NASA, *Drag Equation*, reference-area discussion; the repository example separates synthetic coefficients from numerical consistency. [src/digitalmodel/structural/parachute/parachute_drag.py:L25-L74] [src/digitalmodel/structural/parachute/docs/worked-synthetic-canopy-drag.md:L44-L56] |
| Dual drag | `calculate_dual_chute_drag` delegates to the single calculation twice and sums forces | No interference correction is computed. [src/digitalmodel/structural/parachute/chute_assessment.py:L81-L100] |
| Lift and traction | `calculate_aero_lift` computes `rho*V²*cl*A/2`; `calculate_tire_traction` computes normal force from weight minus lift plus downforce, then multiplies by friction coefficient | Empirical caller inputs; not canopy or tire qualification. [src/digitalmodel/structural/parachute/chute_assessment.py:L103-L128] |
| Load cases and transient | `assess_all_load_cases` chooses the first maximum; `simulate_deployment_transient` uses exponential inflation, explicit speed updates, weight-to-mass conversion and trapezoidal impulse | Reduced-order model; `opening_shock_factor` (Cx) remains in the drag at every time step, multiplied by an inflation ramp rising monotonically to 1.0. No opening-shock overshoot or return to steady Cx=1.0 is modelled. For Cx>1, post-inflation drag and deceleration at a given speed are overstated relative to steady drag, biasing impulse and speed decay; `peak_force_lbs`, `impulse_lbf_s` and the speed history are unqualified. No time-step convergence or experimental benchmark asserted; U1/U3, issue pending (3). [src/digitalmodel/structural/parachute/chute_assessment.py:L232-L242] [docs/domains/structural/parachute/parachute-aerodynamics.md:L19-L19] [docs/domains/structural/parachute/parachute-aerodynamics.md:L49-L51] [src/digitalmodel/structural/parachute/chute_assessment.py:L147-L186] [src/digitalmodel/structural/parachute/chute_assessment.py:L204-L279] |
| Tube sections | `tube_section_properties`: annular area/inertia, `S=I/(OD/2)`, `r=sqrt(I/A)` | Geometrical closed forms. [src/digitalmodel/structural/parachute/frame_model.py:L23-L46] |
| Frame stiffness | `FrameElement2D.local_stiffness`, `transformation_matrix`, `global_stiffness`, `member_end_forces`; `solve_frame` assembles and solves the restrained linear system | Euler–Bernoulli bending plus axial; the documentation names matrix structural analysis references. [src/digitalmodel/structural/parachute/frame_solver.py:L21-L25] [src/digitalmodel/structural/parachute/frame_solver.py:L52-L117] [src/digitalmodel/structural/parachute/frame_solver.py:L140-L230] [docs/domains/structural/parachute/structural-analysis-method.md:L64-L69] |
| Stress and unity | `tube_stress`, `von_mises_tube`, `unity_ratio`, `check_all_members`; signed axial and bending add in the scalar function, while the batch function uses absolute end-force maxima and a coded yield fraction | The material document cautions that individual ASD component allowables are not directly combined-stress allowables. [src/digitalmodel/structural/parachute/member_check.py:L22-L71] [src/digitalmodel/structural/parachute/member_check.py:L174-L206] [docs/domains/structural/parachute/material-properties-4130.md:L71-L98] |
| Connections | `bolt_shear_check`, `weld_throat_check`, `pin_shear_check`: force divided by effective shear area, then divided by caller allowable | Documentation names AISC *Steel Construction Manual*, 15th edition, and AWS D1.1 *Structural Welding Code — Steel*, edition/clause unspecified; clause text not checked against the licensed copy. [src/digitalmodel/structural/parachute/member_check.py:L74-L155] [docs/domains/structural/parachute/structural-analysis-method.md:L149-L189] |
| Material and sizing presets | A material dictionary and first-matching-row sizing lookup feed the calculations | Material code names ASTM A519 and MIL-S-6758 without editions or clauses; sizing code names a commercial catalogue and racing rule without a version. Clause text not checked against the licensed copy. [src/digitalmodel/structural/parachute/frame_model.py:L10-L20] [src/digitalmodel/structural/parachute/stroud_sizing.py:L19-L73] |
| Geometry and CAD | `Member3D.length` returns chord length; CAD arc construction and `compute_arc_midpoint` each implement the sag relation, with straight/midpoint fallbacks | The viewer sweeps a Catmull–Rom curve through 17 circular-arc samples including the sag apex; the viewer-curve part of Draft 2 is addressed by #1684. CAD units and fallback diagnostics remain open. [src/digitalmodel/structural/parachute/frame_geometry_3d.py:L53-L61] [src/digitalmodel/structural/parachute/freecad_frame_builder.py:L63-L123] [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L52-L103] [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L356-L361] |

The documentation also discusses ASME B31.3 and Section VIII Division 2 stress intensity, but `member_check.py` exposes the tube/von-Mises and connection checks above rather than that documented principal-stress calculation; editions and clauses are unspecified, and clause text not checked against the licensed copy. [docs/domains/structural/parachute/structural-analysis-method.md:L113-L131] [src/digitalmodel/structural/parachute/member_check.py:L22-L209]

UNVERIFIED: U3 — governing editions, clause applicability, material-property provenance, the combined-stress allowable basis and calculation Citation sidecars remain unqualified; the transient Cx treatment conflicts with the documented peak/steady distinction (SF77); issue pending (3).

## 4. Validated worked examples and regression examples

Validation here means agreement with an independent closed form or published teaching inputs, not experimental qualification; comparison helpers that call production helpers are treated as regression evidence. [tests/structural/parachute/test_frame_solver.py:L83-L114] [tests/structural/parachute/test_parachute_drag.py:L120-L134] [tests/structural/parachute/test_documented_synthetic_canopy_drag.py:L66-L100]

| Case | Independent source, inputs and expected value | Tolerance and basis | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- |
| V1 validated: cantilever displacement | Closed form `P*L³/(3*E*I)`; test defines `L=100 in`, `E=29e6 psi`, `I=10 in⁴`, `A=5 in²`, `P=-1000 lbf` | Relative error below 0.1%; test's numerical criterion, not an accuracy claim | `tests/structural/parachute/test_frame_solver.py::TestCantileverBeam::test_tip_deflection` | PASS at the code pin. [tests/structural/parachute/test_frame_solver.py:L83-L114] |
| V2 validated: annular area | Closed form `pi*(OD²-ID²)/4`, `OD=1.5 in`, wall `0.120 in`, independently evaluated in the test | Absolute error below 0.001 in², as asserted | `tests/structural/parachute/test_frame_model.py::TestTubeSectionProperties::test_area_1_5od_0_120wall` | PASS at the code pin. [tests/structural/parachute/test_frame_model.py:L43-L50] |
| V3 validated: teaching canopy | NASA, *Adventures in Rocket Science*, 2008, activity 23 part 2, printed p.124; the test derives steady forces directly in SI and executes the roadmap example; displayed expectations 0.7 N and 2.7 N | Force N relative tolerance `1e-6`, accommodating rounded conversions; assertions use unrounded SI values | `tests/structural/parachute/test_parachute_drag.py::test_documented_cited_drag_example` | PASS at the code pin. [tests/structural/parachute/test_parachute_drag.py:L32-L81] |
| V4 validated: synthetic drag arithmetic | NASA drag-law closed form independently evaluated in SI, with synthetic caller inputs; displayed expectations 475.3 N and 1069.5 N | Relative `5e-7` in N, `1e-12` in lbf, absolute tolerance 0; rounded conversion vs roundoff | `tests/structural/parachute/test_documented_synthetic_canopy_drag.py::test_documented_canopy_drag` | PASS at the code pin. [tests/structural/parachute/test_documented_synthetic_canopy_drag.py:L22-L41] [tests/structural/parachute/test_documented_synthetic_canopy_drag.py:L66-L100] [src/digitalmodel/structural/parachute/docs/worked-synthetic-canopy-drag.md:L87-L100] |
| R1 regression: dual sum | Two calls to the production single-canopy function form the reference; no independent interference benchmark | Absolute difference below 1.0 lbf | `tests/structural/parachute/test_chute_assessment.py::TestDualChuteDrag::test_dual_is_sum_of_two_singles` | PASS at the code pin. [tests/structural/parachute/test_chute_assessment.py:L37-L44] |
| R2 regression: YAML governing result | Round-trip reference is the module's own governing result | Governing ID equal; force difference below 1.0 lbf | `tests/structural/parachute/test_chute_assessment.py::TestExportResultsYaml::test_yaml_governing_matches_results` | PASS at the code pin. [tests/structural/parachute/test_chute_assessment.py:L161-L166] |
| R3 regression: transient monotonicity | Initial value and successive speeds from the module's time history; no independently validated transient force history; monotonicity does not detect persistent Cx or qualify impulse/speed decay (SF77) | Final speed lower; adjacent increase no greater than `1e-9 mph` | `tests/structural/parachute/test_chute_assessment.py::TestTransientDeploymentModel::test_vehicle_speed_decreases_monotonically` | PASS at the code pin. [tests/structural/parachute/test_chute_assessment.py:L194-L213] |

External source status: NASA, *Drag Equation*, reference-area discussion; NASA, *Adventures in Rocket Science*, 2008, activity 23 part 2; NIST SP 811, 2008, Appendix B.8 and footnote 23 are cited by the repository derivations below. Preparation response metadata is not shipped; the historical external-source check is NOT_REPRODUCIBLE from the repository. No external-source access was repeated for this revision; the checked evidence is the repository derivation and arithmetic tests. [docs/roadmaps/structural-parachute-example.md:L24-L38] [docs/roadmaps/structural-parachute-example.md:L86-L109] [src/digitalmodel/structural/parachute/docs/worked-synthetic-canopy-drag.md:L44-L100]

Reproduction form: set writable `TMPDIR` and `MPLCONFIGDIR`, then `PYTHONPATH=src python -m pytest tests/structural/parachute -q -rA -m "not solver"`; no pytest configuration override is used. Measured results: 192 collected, 184 passed, 0 failed, 8 skipped, 0 collection errors. Seven FreeCAD-dependent cases are NOT_RUN because the program is absent; one browser case is skipped because Edge/Chrome cannot launch on ARM64. The viewer-only run also gives 31 passed and 1 browser skip with `PYTHONUTF8=0` and `locale.getpreferredencoding(False) == "CP1252"`; explicit UTF-8 writes and reads close SF76. Supplemental native Chromium 159.0.8154.6 executes the browser assertions with zero network requests and page errors, but does not close Edge/Chrome acceptance. [tests/structural/parachute/test_freecad_frame_builder.py:L15-L24] [tests/structural/parachute/test_freecad_frame_builder.py:L37-L116]

## 5. QA gates

Required family mapping: AQG r0, section 10, row “Python calculations”, and section 9 for G5.

| Gate | Applicability | Checks and evidence |
| --- | --- | --- |
| G0 | Applicable: inputs, units and citations | I1, I2, I3; test rejection and independent-unit comparisons below |
| G1 | Applicable: Python imports/tests; CAD load route is FreeCAD document creation and export tests, no solver route | L1, L2; Python path PASS, seven CAD cases NOT_RUN because FreeCAD is absent |
| G2 | Applicable: hand or published check | H1–H3 and the traction part of H4; the lift part of H4 is regression evidence |
| G3 | NOT_APPLICABLE: the family mapping excludes short licensed dynamics | The reduced-order Python transient is covered as a regression, not a solver dynamics clearance |
| G4 | NOT_APPLICABLE: no diffraction-body calculation in this package | Geometry and CAD checks remain G0/G1 responsibilities |
| G5 | Applicable: traceable QA evidence and result export | E1, E2; export is partial evidence, not an automatically generated complete AQG appendix |

Checks below are reproducible tests or explicit evidence audits; thresholds are acceptance criteria for these checks, not additional physical design limits. [tests/structural/parachute/test_frame_solver.py:L96-L154] [tests/structural/parachute/test_chute_assessment.py:L151-L166]

| ID | Gate | Executable check | Numeric pass criterion and unit | Evidence |
| --- | --- | --- | --- | --- |
| I1 | G0 | Run `TestTubeSectionProperties.test_zero_wall_raises` and `.test_wall_exceeds_radius_raises` in the section-4 tube test file | 2 ValueError outcomes; 0 accepted invalid walls (cases) | PASS. [tests/structural/parachute/test_frame_model.py:L68-L74] |
| I2 | G0 | Run `TestDragForce.test_negative_speed_raises` and `.test_zero_speed_zero_force` in the section-4 drag test file | 1 rejected negative speed (case); zero-speed force exactly 0 lbf | PASS. [tests/structural/parachute/test_parachute_drag.py:L165-L171] |
| I3 | G0 | Run V4; compare `calculate_drag_force` with independent density, area and speed conversion | Relative error at most `5e-7` (N/N) and `1e-12` (lbf/lbf), absolute tolerance 0 | PASS. [tests/structural/parachute/test_documented_synthetic_canopy_drag.py:L66-L100] |
| L1 | G1 | Run the full module test command in section 4 and enumerate all statuses | 192 cases accounted for, 0 failures, 184 passes; 7 absent-program CAD cases and 1 unavailable-browser case are skipped | Python PASS; SF76 closed under CP1252. CAD NOT_RUN and Edge/Chrome acceptance open. [tests/structural/parachute/test_freecad_frame_builder.py:L15-L24] |
| L2 | G1 | Run `TestFrameToJson` in `tests/structural/parachute/test_frame_viewer_3d.py` | 15 node, 16 member, 15 connection records; 0 count mismatches | PASS. [tests/structural/parachute/test_frame_viewer_3d.py:L42-L52] |
| H1 | G2 | Run V1; compare `solve_frame` displacement with the independent cantilever formula | Relative displacement error below 0.1% (in/in) | PASS. [tests/structural/parachute/test_frame_solver.py:L96-L114] |
| H2 | G2 | Run `TestCantileverBeam.test_fixed_end_reaction` and `.test_fixed_end_moment` in the section-4 solver test file | Reaction error below 0.1% (lbf/lbf); moment magnitude error below 1.0% (in-lbf/in-lbf) | PASS. [tests/structural/parachute/test_frame_solver.py:L116-L154] |
| H3 | G2 | Run V2; compare `tube_section_properties` area with annular closed form | Absolute error below 0.001 in² | PASS. [tests/structural/parachute/test_frame_model.py:L46-L50] |
| H4 | G2 for traction only; lift regression | Run `TestAeroLift.test_lift_at_200mph` and `TestTireTraction.test_traction_basic` in the section-4 assessment test file | Lift regression difference below 1.0 lbf; independent friction-capacity error below 0.1 lbf | PASS traction arithmetic (G2); lift expectation reuses production `mph_to_fps` with a 1.0 lbf tolerance and receives no G2 credit; empirical applicability not established. [tests/structural/parachute/test_chute_assessment.py:L70-L73] [tests/structural/parachute/test_chute_assessment.py:L90-L97] |
| E1 | G5 | Run `TestExportResultsYaml.test_yaml_has_required_keys` and R2 | 3 load-case records; governing ID equal; force round-trip error below 1.0 lbf | PASS; exported content omits traction/history. [tests/structural/parachute/test_chute_assessment.py:L154-L166] [src/digitalmodel/structural/parachute/chute_assessment.py:L296-L321] |
| E2 | G5 | Audit two retained module-run records with node-status lists, input digests and the code pin | Intended criterion: 177 unique nodes per run, 0 status differences and 0 unaccounted cases | NOT_RUN for a reproducible historical evidence audit: the two preparation records and their digests are not shipped. Current test counts do not replace that missing evidence; a production QA appendix generator remains #3309. [src/digitalmodel/structural/parachute/chute_assessment.py:L296-L321] |

## 6. Known failure modes and pitfalls

sweep: 77 found

The following inspection register includes silent fallbacks, unit assumptions and intentional initializations, not a claim that every match is a confirmed defect; code locations identify the symptom at the code pin, with unresolved causes grouped into issue drafts.

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|float\(|int\(|\.get\(|default' src/digitalmodel/structural/parachute --glob '*.py'
rg -n 'else 0|else 0\.0|return null|if \(!|\|\||max\(|min\(|rho|psi|inch|slug|ft|mph|cache|write' src/digitalmodel/structural/parachute --glob '*.py'
```

| ID | Location and symptom | Existing detection check or gap | Tracking |
| --- | --- | --- | --- |
| SF01 | FreeCAD import failure becomes an availability flag; public CAD entry points then raise explicitly. [src/digitalmodel/structural/parachute/freecad_frame_builder.py:L19-L38] | no detection check exists for the unavailable-program guard | issue pending (2) |
| SF02 | Chord longer than the permitted circular diameter becomes a straight tube. [src/digitalmodel/structural/parachute/freecad_frame_builder.py:L79-L82] | no detection check exists | issue pending (2) |
| SF03 | Any arc-sweep exception becomes a straight tube, without a warning. [src/digitalmodel/structural/parachute/freecad_frame_builder.py:L104-L123] | no detection check exists | issue pending (2) |
| SF04 | Missing outer-diameter key uses a dimension preset. [src/digitalmodel/structural/parachute/freecad_frame_builder.py:L148-L149] | no detection check exists | issue pending (2) |
| SF05 | Missing wall key uses a dimension preset. [src/digitalmodel/structural/parachute/freecad_frame_builder.py:L148-L155] | no detection check exists | issue pending (2) |
| SF06 | A bend with nonpositive radius takes the straight-tube branch. [src/digitalmodel/structural/parachute/freecad_frame_builder.py:L152-L155] | no detection check exists | issue pending (2) |
| SF07 | Connection spheres use a default display radius, independent of connection strength. [src/digitalmodel/structural/parachute/freecad_frame_builder.py:L126-L134] | no detection check exists for physical interpretation | issue pending (2) |
| SF08 | Inch coordinates/radii are passed directly into CAD vectors and cylinders. [src/digitalmodel/structural/parachute/freecad_frame_builder.py:L41-L55] | no detection check exists for exchange-unit scale | issue pending (2) |
| SF09 | Output directory defaults to the current directory. [src/digitalmodel/structural/parachute/generate_frame_cad.py:L20-L24] | no detection check exists for output containment | issue pending (2) |
| SF10 | External outer-diameter text uses float conversion and a preset; grouped decimal separators raise. [src/digitalmodel/structural/parachute/generate_frame_cad.py:L22-L24] | no detection check exists for locale/finite inputs | issue pending (1) |
| SF11 | External wall text uses float conversion and a preset. [src/digitalmodel/structural/parachute/generate_frame_cad.py:L23-L24] | I1 covers wall rejection only; no detection check exists for locale/finite inputs | issue pending (1) |
| SF12 | External bend-radius text uses float conversion and a preset. [src/digitalmodel/structural/parachute/generate_frame_cad.py:L24-L24] | no detection check exists for locale/finite inputs | issue pending (1) |
| SF13 | Nonpositive diameter bypasses the area guard and becomes zero force. [src/digitalmodel/structural/parachute/parachute_drag.py:L57-L63] | I2 covers speed only; no detection check exists for diameter bypass | issue pending (1) |
| SF14 | Density and coefficients have no sign/finite validation; the unit contract is customary. [src/digitalmodel/structural/parachute/parachute_drag.py:L37-L63] | I3 covers valid unit conversion; no detection check exists for invalid coefficients | issue pending (1) |
| SF15 | Nonpositive area becomes zero axial stress. [src/digitalmodel/structural/parachute/member_check.py:L40-L40] | no detection check exists | issue pending (1) |
| SF16 | Nonpositive inertia becomes zero bending stress. [src/digitalmodel/structural/parachute/member_check.py:L41-L41] | no detection check exists | issue pending (1) |
| SF17 | Nonpositive area becomes zero shear stress. [src/digitalmodel/structural/parachute/member_check.py:L42-L43] | no detection check exists | issue pending (1) |
| SF18 | Nonpositive unity capacity returns infinity, an explicit failure sentinel. [src/digitalmodel/structural/parachute/member_check.py:L67-L71] | no detection check exists for this invalid-capacity branch | issue pending (1) |
| SF19 | Bolt nonpositive effective area/capacity produces infinity; signed diameter/count validation is absent. [src/digitalmodel/structural/parachute/member_check.py:L92-L101] | no detection check exists for invalid geometry | issue pending (1) |
| SF20 | Weld nonpositive effective area/capacity produces infinity; two negative dimensions can multiply positive. [src/digitalmodel/structural/parachute/member_check.py:L120-L127] | no detection check exists for invalid geometry | issue pending (1) |
| SF21 | Pin nonpositive effective area/capacity produces infinity; signed diameter/count validation is absent. [src/digitalmodel/structural/parachute/member_check.py:L146-L154] | no detection check exists for invalid geometry | issue pending (1) |
| SF22 | Batch member allowable uses a fixed fraction of yield. [src/digitalmodel/structural/parachute/member_check.py:L174-L175] | no detection check exists for governing-code provenance | issue pending (3) |
| SF23 | An empty force list returns an empty member-check list without an explicit completeness check. [src/digitalmodel/structural/parachute/member_check.py:L177-L209] | no detection check exists | #3309 |
| SF24 | One supplied section is used for every member, with separate maxima from both ends. [src/digitalmodel/structural/parachute/member_check.py:L179-L186] | no detection check exists for mixed-section routing | #3309 |
| SF25 | Signed axial and bending terms can cancel at the sampled fiber; opposite-fiber maximum is not selected in the scalar function. [src/digitalmodel/structural/parachute/member_check.py:L52-L64] | no detection check exists for opposite-sign inputs | issue pending (1) |
| SF26 | Single sizing lookup outside the table returns the final model. [src/digitalmodel/structural/parachute/stroud_sizing.py:L62-L66] | no detection check exists | issue pending (1) |
| SF27 | Dual sizing lookup outside the table returns the final model. [src/digitalmodel/structural/parachute/stroud_sizing.py:L69-L73] | no detection check exists | issue pending (1) |
| SF28 | Sizing accepts negative or nonfinite weight/speed without explicit rejection. [src/digitalmodel/structural/parachute/stroud_sizing.py:L40-L59] | no detection check exists | issue pending (1) |
| SF29 | A missing load connection returns None. [src/digitalmodel/structural/parachute/frame_geometry_3d.py:L96-L101] | no detection check exists for the missing-connection branch | issue pending (1) |
| SF30 | Member bend radius defaults to zero; member length is a chord even for bends. [src/digitalmodel/structural/parachute/frame_geometry_3d.py:L40-L61] | no detection check exists for arc-length interpretation | issue pending (2) |
| SF31 | Frame material defaults to a copied preset dictionary. [src/digitalmodel/structural/parachute/frame_model.py:L49-L57] | no detection check exists for physical applicability or fallback disclosure | issue pending (3) |
| SF32 | Bar outer diameter has a dimension preset. [src/digitalmodel/structural/parachute/frame_model.py:L60-L61] | no detection check exists for physical applicability or fallback disclosure | issue pending (3) |
| SF33 | Bar wall thickness has a dimension preset. [src/digitalmodel/structural/parachute/frame_model.py:L62-L62] | no detection check exists for physical applicability or fallback disclosure | issue pending (3) |
| SF34 | Strut outer diameter has a dimension preset. [src/digitalmodel/structural/parachute/frame_model.py:L63-L63] | no detection check exists for physical applicability or fallback disclosure | issue pending (3) |
| SF35 | Strut wall thickness has a dimension preset. [src/digitalmodel/structural/parachute/frame_model.py:L64-L65] | no detection check exists for physical applicability or fallback disclosure | issue pending (3) |
| SF36 | Three-dimensional geometry material defaults to a copied preset dictionary. [src/digitalmodel/structural/parachute/frame_geometry_3d.py:L64-L73] | no detection check exists for physical qualification | issue pending (3) |
| SF37 | Three-dimensional tube diameter has a dimension preset. [src/digitalmodel/structural/parachute/frame_geometry_3d.py:L108-L109] | no detection check exists for physical applicability or fallback disclosure | issue pending (3) |
| SF38 | Three-dimensional tube wall has a dimension preset. [src/digitalmodel/structural/parachute/frame_geometry_3d.py:L110-L110] | no detection check exists for physical applicability or fallback disclosure | issue pending (3) |
| SF39 | Three-dimensional bend radius has a dimension preset. [src/digitalmodel/structural/parachute/frame_geometry_3d.py:L111-L112] | no detection check exists for physical applicability or fallback disclosure | issue pending (3) |
| SF40 | Near-zero chord returns its midpoint instead of an arc diagnostic. [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L70-L71] | no detection check exists for the fallback | issue pending (2) |
| SF41 | Excessive chord returns its midpoint instead of a radius diagnostic. [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L73-L75] | no detection check exists for the fallback | issue pending (2) |
| SF42 | Vertical-reference fallback switches axis; a still-degenerate perpendicular returns the midpoint. [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L87-L95] | no detection check exists for the degenerate branch | issue pending (2) |
| SF43 | JSON export fills a missing diameter with a display preset. [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L140-L158] | L2 covers record counts only; no detection check exists for missing section data | issue pending (2) |
| SF44 | JavaScript replaces missing/zero diameter with a display preset. [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L371-L371] | no detection check exists for physical applicability or fallback disclosure | issue pending (2) |
| SF45 | JavaScript uses a fallback color for unknown member assembly. [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L370-L370] | no detection check exists for physical applicability or fallback disclosure | issue pending (2) |
| SF46 | JavaScript uses the shared group for unknown member assembly. [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L379-L382] | no detection check exists for physical applicability or fallback disclosure | issue pending (2) |
| SF47 | JavaScript returns null for a short member; the member loop omits a null mesh. [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L335-L353] | no detection check exists for omitted members | issue pending (2) |
| SF48 | Unknown node assembly receives a default color. [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L406-L410] | no detection check exists for physical applicability or fallback disclosure | issue pending (2) |
| SF49 | Unknown node assembly uses the shared group. [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L411-L413] | no detection check exists for physical applicability or fallback disclosure | issue pending (2) |
| SF50 | The connection-marker loop deliberately skips free connections without a special marker. [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L416-L433] | L2 checks serialized counts; no detection check exists for visual completeness | issue pending (2) |
| SF51 | Missing toggle elements return without binding a handler. [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L453-L457] | no detection check exists for browser controls | issue pending (2) |
| SF52 | Viewer CLI writes to a relative default output location. [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L493-L507] | no detection check exists for CLI output containment | issue pending (2) |
| SF53 | Dual canopy diameter has an empirical preset. [src/digitalmodel/structural/parachute/chute_assessment.py:L72-L73] | no detection check exists for physical applicability or fallback disclosure | issue pending (3) |
| SF54 | Lift coefficient has an empirical preset. [src/digitalmodel/structural/parachute/chute_assessment.py:L75-L76] | no detection check exists for physical applicability or fallback disclosure | issue pending (3) |
| SF55 | Frontal area has an empirical preset. [src/digitalmodel/structural/parachute/chute_assessment.py:L77-L77] | no detection check exists for physical applicability or fallback disclosure | issue pending (3) |
| SF56 | Dry friction coefficient has an empirical preset. [src/digitalmodel/structural/parachute/chute_assessment.py:L78-L78] | no detection check exists for physical applicability or fallback disclosure | issue pending (3) |
| SF57 | Assessment downforce defaults to zero. [src/digitalmodel/structural/parachute/chute_assessment.py:L139-L139] | no detection check exists for physical applicability or fallback disclosure | issue pending (3) |
| SF58 | Dual drag coefficient has a preset. [src/digitalmodel/structural/parachute/chute_assessment.py:L142-L143] | no detection check exists for physical applicability or fallback disclosure | issue pending (3) |
| SF59 | Negative normal load gives negative friction capacity while setting a liftoff flag. [src/digitalmodel/structural/parachute/chute_assessment.py:L114-L128] | H4 covers valid traction only; no detection check exists for negative capacity policy | issue pending (1) |
| SF60 | Tied governing forces select the first case and leave other ties unmarked. [src/digitalmodel/structural/parachute/chute_assessment.py:L179-L184] | E1 checks the normal governing export; no detection check exists for ties | issue pending (1) |
| SF61 | Transient car-drag coefficient has a preset. [src/digitalmodel/structural/parachute/chute_assessment.py:L201-L201] | no detection check exists for physical applicability or fallback disclosure | issue pending (3) |
| SF62 | Transient inflation time constant has a preset. [src/digitalmodel/structural/parachute/chute_assessment.py:L202-L203] | no detection check exists for physical applicability or fallback disclosure | issue pending (3) |
| SF63 | Weight-to-mass conversion assumes customary gravity. [src/digitalmodel/structural/parachute/chute_assessment.py:L217-L218] | no detection check exists for physical applicability or fallback disclosure | issue pending (3) |
| SF64 | Nonpositive inflation time constant means instantaneous full inflation. [src/digitalmodel/structural/parachute/chute_assessment.py:L230-L233] | no detection check exists for rejecting a negative time constant | issue pending (1) |
| SF65 | Time step and updated speed are clipped to nonnegative values. [src/digitalmodel/structural/parachute/chute_assessment.py:L259-L261] | R3 checks monotonicity; no detection check exists for time-step convergence | issue pending (3) |
| SF66 | Zero weight fallback yields zero lift ratio, although earlier validation rejects zero weight. [src/digitalmodel/structural/parachute/chute_assessment.py:L269-L269] | no detection check exists for physical applicability or fallback disclosure | issue pending (1) |
| SF67 | Advisory text uses a fixed lift-ratio threshold. [src/digitalmodel/structural/parachute/chute_assessment.py:L270-L279] | no detection check exists for physical applicability or fallback disclosure | issue pending (3) |
| SF68 | YAML defaults a reference string, rounds force values and omits traction/transient details. [src/digitalmodel/structural/parachute/chute_assessment.py:L296-L321] | E1 detects governing-force mismatch, not missing full QA evidence | #3309 |
| SF69 | Two-dimensional geometry uses inches and a preset origin/axis convention. [src/digitalmodel/structural/parachute/frame_model.py:L66-L88] | no detection check exists for an independent measured geometry | issue pending (5) |
| SF70 | Three-dimensional geometry uses inches and a different origin/axis convention. [src/digitalmodel/structural/parachute/frame_geometry_3d.py:L113-L130] | no detection check exists for a measured geometry transform | issue pending (5) |
| SF71 | Zero load-vector entries and zero fixed-displacement values are intentional assembly/constraint operations. [src/digitalmodel/structural/parachute/frame_solver.py:L174-L197] | H1 and H2 detect the cantilever displacement/reaction result | none; intentional numerical initialization |
| SF72 | Zero impulse initialization starts trapezoidal accumulation. [src/digitalmodel/structural/parachute/chute_assessment.py:L263-L267] | no detection check exists for an independent impulse benchmark | #3309 |
| SF73 | Missing member labels use a generated label. [src/digitalmodel/structural/parachute/frame_solver.py:L214-L217] | no detection check exists for label provenance | #3309 |
| SF74 | Missing result labels use a generated label. [src/digitalmodel/structural/parachute/member_check.py:L197-L200] | no detection check exists for label provenance | #3309 |
| SF75 | CAD document name uses an identifying preset; the name supplies no geometry validation. [src/digitalmodel/structural/parachute/freecad_frame_builder.py:L137-L143] | no detection check exists for name codenaming | issue pending (5) |
| SF76 | Viewer HTML now uses explicit UTF-8 writes and test reads. [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L185-L203] | `TestGenerateViewerHtml::test_writes_utf8_explicitly` checks the write encoding; 31 viewer tests pass under CP1252 with UTF-8 mode disabled, with 1 browser availability skip. | Closed by #1684; the former five failures below pass under CP1252. |
| SF77 | The full Cx-scaled drag is multiplied by a monotone inflation ramp; Cx is held for the whole history, with no opening-shock overshoot or steady-state recovery to 1.0. For Cx>1, post-inflation drag/deceleration at a given speed are overstated relative to steady drag, biasing impulse and speed decay. Peak force, impulse and speed history lack physical qualification. [src/digitalmodel/structural/parachute/chute_assessment.py:L232-L242] [docs/domains/structural/parachute/parachute-aerodynamics.md:L19-L19] [docs/domains/structural/parachute/parachute-aerodynamics.md:L49-L51] | R3 checks monotonic speed only; no detection check exists for peak-to-steady Cx recovery or independent impulse/speed history. | U1/U3; issue pending (3) |

SF76 formerly affected these five L1 nodes (all in `tests/structural/parachute/test_frame_viewer_3d.py`):

- `tests/structural/parachute/test_frame_viewer_3d.py::TestGenerateViewerHtml::test_creates_file`
- `tests/structural/parachute/test_frame_viewer_3d.py::TestGenerateViewerHtml::test_contains_three_js`
- `tests/structural/parachute/test_frame_viewer_3d.py::TestGenerateViewerHtml::test_contains_frame_data`
- `tests/structural/parachute/test_frame_viewer_3d.py::TestGenerateViewerHtml::test_contains_html_structure`
- `tests/structural/parachute/test_frame_viewer_3d.py::TestGenerateViewerHtml::test_contains_toggle_controls`

The sweep found no cache-writing path or error-path empty-dictionary return; empty containers in the history and stiffness routines are initialized and then populated, while the empty member-input outcome is listed above. [src/digitalmodel/structural/parachute/chute_assessment.py:L220-L267] [src/digitalmodel/structural/parachute/frame_solver.py:L140-L230] [src/digitalmodel/structural/parachute/member_check.py:L177-L209]

Other source conflicts: the material code and document differ in yield, ultimate and shear-modulus entries, and the document discusses an ASME stress-intensity path absent from the member calculation; no detection check exists for source reconciliation, issue pending (3). [src/digitalmodel/structural/parachute/frame_model.py:L10-L20] [docs/domains/structural/parachute/material-properties-4130.md:L20-L34] [docs/domains/structural/parachute/structural-analysis-method.md:L113-L131] [src/digitalmodel/structural/parachute/member_check.py:L22-L209]

Issue leads #214 and #1666 concern wider frame-load work and #1677 concerns deployment dynamics; these are scope leads, not proof of a historical calculation incident, and no detection check exists here for completion of their wider deliverables. [docs/roadmaps/structural-parachute-example.md:L133-L147]

Additional published issue leads read during preparation: #1703, “Cross-tool structural comparison framework”; #3309, “Module report with drag and frame worked example”. Their requested integrated evidence is not established by this pack’s arithmetic checks (repository issues #1703 and #3309, read 2026-11-07).

## 7. Lessons log

| Date | Change reference | Lesson | Evidence | Pack change | Shared entry |
| --- | --- | --- | --- | --- | --- |
| 2026-11-07 | Initial pack | Independent closed forms check arithmetic; a source-qualified physical basis remains separate | [tests/structural/parachute/test_documented_synthetic_canopy_drag.py:L66-L100] | Sections 3–6 distinguish arithmetic, regression and empirical gaps | Pending maintainer registration; issue pending (3) |
| 2026-11-07 | Pack revision 1 | A successful UTF-8 run and monotone speed history do not establish locale portability or peak-to-steady force validity | [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L202-L202] [src/digitalmodel/structural/parachute/chute_assessment.py:L232-L242] | Require an encoding check for text writes and a peak/steady check for transient multipliers; SF76/SF77 record the gaps | issue pending (1); issue pending (3) |

## 8. Ownership

Review assignment for this draft: engineering calculations family quality manager; deputy: engineering calculations deputy quality manager; producing function: calculation documentation. Escalation for classification, missing sources and acceptance criteria goes to the family quality manager, then to the maintainer for unresolved scope decisions. Clearance is pending independent review under AQG r0 section 11.

## 9. Dependencies and implementation inventory

Package dependencies visible in code are the Python standard library, PyYAML for assessment/export and NumPy for frame algebra; FreeCAD/Part are optional CAD dependencies, and the viewer uses three.js 0.167.0 through CDN imports by default or verified local modules inlined with their MIT notice (`--threejs-dir` / `DIGITALMODEL_THREEJS_DIR`). [src/digitalmodel/structural/parachute/chute_assessment.py:L6-L19] [src/digitalmodel/structural/parachute/frame_solver.py:L6-L10] [src/digitalmodel/structural/parachute/freecad_frame_builder.py:L19-L38] [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L22-L34] [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L493-L509]

Assessment exports load-case YAML for downstream structural work; CAD exports and viewer HTML are separate outputs, and no external structural solver is launched by the frame solve. [src/digitalmodel/structural/parachute/chute_assessment.py:L296-L321] [src/digitalmodel/structural/parachute/freecad_frame_builder.py:L168-L185] [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L185-L203] [src/digitalmodel/structural/parachute/frame_solver.py:L196-L230]

Map source pin: `21942e8cfc7dfb985c5bfa693245df7d197cf76c` (the header code revision); map data revision: `270c8759c374b67a7f032953e7fcdb14e0224461`. All implementation-map citations resolve in the shipped tree and at the header code revision; each record range was re-read at this pin. [docs/registry/implementation-map.yaml:L1-L4]

The package has one map family, `wind-and-current-force-routes`: five in-package rows, with all 135 family rows inventoried below. It is the only counted family, ranked by its five in-package rows. Each ordinal is its one-based implementation-row position in that family at the map pin; its source range supplies the exact path, symbol and relation without repeating identifying discovery names. [docs/registry/implementation-map.yaml:L15369-L16768]

| Map row | Scope / definition when in package | Map relation | Map record at the header code revision |
| --- | --- | --- | --- |
| 1 | Outside package | example | [docs/registry/implementation-map.yaml:L15376-L15385] |
| 2 | Outside package | example | [docs/registry/implementation-map.yaml:L15386-L15395] |
| 3 | Outside package | example | [docs/registry/implementation-map.yaml:L15396-L15404] |
| 4 | Outside package | example | [docs/registry/implementation-map.yaml:L15405-L15414] |
| 5 | Outside package | example | [docs/registry/implementation-map.yaml:L15415-L15424] |
| 6 | Outside package | example | [docs/registry/implementation-map.yaml:L15425-L15436] |
| 7 | Outside package | example | [docs/registry/implementation-map.yaml:L15437-L15446] |
| 8 | Outside package | example | [docs/registry/implementation-map.yaml:L15447-L15456] |
| 9 | Outside package | example | [docs/registry/implementation-map.yaml:L15457-L15466] |
| 10 | Outside package | example | [docs/registry/implementation-map.yaml:L15467-L15476] |
| 11 | Outside package | example | [docs/registry/implementation-map.yaml:L15477-L15486] |
| 12 | Outside package | example | [docs/registry/implementation-map.yaml:L15487-L15496] |
| 13 | Outside package | example | [docs/registry/implementation-map.yaml:L15497-L15506] |
| 14 | Outside package | example | [docs/registry/implementation-map.yaml:L15507-L15516] |
| 15 | Outside package | example | [docs/registry/implementation-map.yaml:L15517-L15526] |
| 16 | Outside package | independent | [docs/registry/implementation-map.yaml:L15527-L15535] |
| 17 | Outside package | independent | [docs/registry/implementation-map.yaml:L15536-L15544] |
| 18 | Outside package | independent | [docs/registry/implementation-map.yaml:L15545-L15554] |
| 19 | Outside package | independent | [docs/registry/implementation-map.yaml:L15555-L15563] |
| 20 | Outside package | independent | [docs/registry/implementation-map.yaml:L15564-L15573] |
| 21 | Outside package | independent | [docs/registry/implementation-map.yaml:L15574-L15583] |
| 22 | Outside package | independent | [docs/registry/implementation-map.yaml:L15584-L15593] |
| 23 | Outside package | independent | [docs/registry/implementation-map.yaml:L15594-L15605] |
| 24 | Outside package | independent | [docs/registry/implementation-map.yaml:L15606-L15615] |
| 25 | Outside package | independent | [docs/registry/implementation-map.yaml:L15616-L15625] |
| 26 | Outside package | independent | [docs/registry/implementation-map.yaml:L15626-L15635] |
| 27 | Outside package | independent | [docs/registry/implementation-map.yaml:L15636-L15645] |
| 28 | Outside package | independent | [docs/registry/implementation-map.yaml:L15646-L15660] |
| 29 | Outside package | independent | [docs/registry/implementation-map.yaml:L15661-L15669] |
| 30 | Outside package | independent | [docs/registry/implementation-map.yaml:L15670-L15679] |
| 31 | Outside package | independent | [docs/registry/implementation-map.yaml:L15680-L15689] |
| 32 | Outside package | independent | [docs/registry/implementation-map.yaml:L15690-L15699] |
| 33 | Outside package | independent | [docs/registry/implementation-map.yaml:L15700-L15709] |
| 34 | Outside package | independent | [docs/registry/implementation-map.yaml:L15710-L15719] |
| 35 | Outside package | independent | [docs/registry/implementation-map.yaml:L15720-L15729] |
| 36 | Outside package | independent | [docs/registry/implementation-map.yaml:L15730-L15739] |
| 37 | Outside package | independent | [docs/registry/implementation-map.yaml:L15740-L15749] |
| 38 | Outside package | independent | [docs/registry/implementation-map.yaml:L15750-L15759] |
| 39 | Outside package | independent | [docs/registry/implementation-map.yaml:L15760-L15769] |
| 40 | Outside package | caller | [docs/registry/implementation-map.yaml:L15770-L15782] |
| 41 | Outside package | independent | [docs/registry/implementation-map.yaml:L15783-L15792] |
| 42 | Outside package | independent | [docs/registry/implementation-map.yaml:L15793-L15802] |
| 43 | Outside package | independent | [docs/registry/implementation-map.yaml:L15803-L15812] |
| 44 | Outside package | independent | [docs/registry/implementation-map.yaml:L15813-L15822] |
| 45 | Outside package | independent | [docs/registry/implementation-map.yaml:L15823-L15832] |
| 46 | Outside package | independent | [docs/registry/implementation-map.yaml:L15833-L15842] |
| 47 | Outside package | independent | [docs/registry/implementation-map.yaml:L15843-L15852] |
| 48 | Outside package | independent | [docs/registry/implementation-map.yaml:L15853-L15862] |
| 49 | Outside package | independent | [docs/registry/implementation-map.yaml:L15863-L15872] |
| 50 | Outside package | independent | [docs/registry/implementation-map.yaml:L15873-L15882] |
| 51 | Outside package | independent | [docs/registry/implementation-map.yaml:L15883-L15892] |
| 52 | Outside package | independent | [docs/registry/implementation-map.yaml:L15893-L15902] |
| 53 | Outside package | independent | [docs/registry/implementation-map.yaml:L15903-L15912] |
| 54 | Outside package | independent | [docs/registry/implementation-map.yaml:L15913-L15922] |
| 55 | Outside package | independent | [docs/registry/implementation-map.yaml:L15923-L15932] |
| 56 | Outside package | independent | [docs/registry/implementation-map.yaml:L15933-L15941] |
| 57 | Outside package | copy | [docs/registry/implementation-map.yaml:L15942-L15951] |
| 58 | Outside package | copy | [docs/registry/implementation-map.yaml:L15952-L15963] |
| 59 | Outside package | copy | [docs/registry/implementation-map.yaml:L15964-L15973] |
| 60 | Outside package | copy | [docs/registry/implementation-map.yaml:L15974-L15985] |
| 61 | Outside package | copy | [docs/registry/implementation-map.yaml:L15986-L15995] |
| 62 | Outside package | independent | [docs/registry/implementation-map.yaml:L15996-L16005] |
| 63 | Outside package | copy | [docs/registry/implementation-map.yaml:L16006-L16015] |
| 64 | Outside package | copy | [docs/registry/implementation-map.yaml:L16016-L16025] |
| 65 | Outside package | copy | [docs/registry/implementation-map.yaml:L16026-L16035] |
| 66 | Outside package | copy | [docs/registry/implementation-map.yaml:L16036-L16050] |
| 67 | Outside package | independent | [docs/registry/implementation-map.yaml:L16051-L16059] |
| 68 | Outside package | independent | [docs/registry/implementation-map.yaml:L16060-L16069] |
| 69 | Outside package | independent | [docs/registry/implementation-map.yaml:L16070-L16079] |
| 70 | Outside package | independent | [docs/registry/implementation-map.yaml:L16080-L16089] |
| 71 | Outside package | independent | [docs/registry/implementation-map.yaml:L16090-L16102] |
| 72 | Outside package | copy | [docs/registry/implementation-map.yaml:L16103-L16112] |
| 73 | Outside package | copy | [docs/registry/implementation-map.yaml:L16113-L16122] |
| 74 | Outside package | copy | [docs/registry/implementation-map.yaml:L16123-L16132] |
| 75 | Outside package | independent | [docs/registry/implementation-map.yaml:L16133-L16145] |
| 76 | Outside package | independent | [docs/registry/implementation-map.yaml:L16146-L16155] |
| 77 | Outside package | independent | [docs/registry/implementation-map.yaml:L16156-L16165] |
| 78 | Outside package | independent | [docs/registry/implementation-map.yaml:L16166-L16175] |
| 79 | Outside package | independent | [docs/registry/implementation-map.yaml:L16176-L16185] |
| 80 | Outside package | independent | [docs/registry/implementation-map.yaml:L16186-L16195] |
| 81 | Outside package | independent | [docs/registry/implementation-map.yaml:L16196-L16205] |
| 82 | Outside package | independent | [docs/registry/implementation-map.yaml:L16206-L16215] |
| 83 | Outside package | independent | [docs/registry/implementation-map.yaml:L16216-L16225] |
| 84 | Outside package | independent | [docs/registry/implementation-map.yaml:L16226-L16235] |
| 85 | Outside package | independent | [docs/registry/implementation-map.yaml:L16236-L16245] |
| 86 | calculate_dual_chute_drag | independent | [docs/registry/implementation-map.yaml:L16246-L16257] |
| 87 | assess_all_load_cases | caller | [docs/registry/implementation-map.yaml:L16258-L16270] |
| 88 | simulate_deployment_transient | independent | [docs/registry/implementation-map.yaml:L16271-L16281] |
| 89 | export_results_yaml | independent | [docs/registry/implementation-map.yaml:L16282-L16291] |
| 90 | calculate_drag_force | independent | [docs/registry/implementation-map.yaml:L16292-L16301] |
| 91 | Outside package | independent | [docs/registry/implementation-map.yaml:L16302-L16311] |
| 92 | Outside package | independent | [docs/registry/implementation-map.yaml:L16312-L16321] |
| 93 | Outside package | independent | [docs/registry/implementation-map.yaml:L16322-L16331] |
| 94 | Outside package | independent | [docs/registry/implementation-map.yaml:L16332-L16341] |
| 95 | Outside package | independent | [docs/registry/implementation-map.yaml:L16342-L16351] |
| 96 | Outside package | independent | [docs/registry/implementation-map.yaml:L16352-L16361] |
| 97 | Outside package | caller | [docs/registry/implementation-map.yaml:L16362-L16373] |
| 98 | Outside package | caller | [docs/registry/implementation-map.yaml:L16374-L16385] |
| 99 | Outside package | independent | [docs/registry/implementation-map.yaml:L16386-L16395] |
| 100 | Outside package | independent | [docs/registry/implementation-map.yaml:L16396-L16405] |
| 101 | Outside package | independent | [docs/registry/implementation-map.yaml:L16406-L16415] |
| 102 | Outside package | independent | [docs/registry/implementation-map.yaml:L16416-L16425] |
| 103 | Outside package | independent | [docs/registry/implementation-map.yaml:L16426-L16435] |
| 104 | Outside package | independent | [docs/registry/implementation-map.yaml:L16436-L16445] |
| 105 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16446-L16454] |
| 106 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16455-L16464] |
| 107 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16465-L16474] |
| 108 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16475-L16484] |
| 109 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16485-L16494] |
| 110 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16495-L16505] |
| 111 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16506-L16515] |
| 112 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16516-L16526] |
| 113 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16527-L16536] |
| 114 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16537-L16546] |
| 115 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16547-L16556] |
| 116 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16557-L16566] |
| 117 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16567-L16577] |
| 118 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16578-L16588] |
| 119 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16589-L16599] |
| 120 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16600-L16610] |
| 121 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16611-L16621] |
| 122 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16622-L16632] |
| 123 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16633-L16642] |
| 124 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16643-L16652] |
| 125 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16653-L16662] |
| 126 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16663-L16672] |
| 127 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16673-L16682] |
| 128 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16683-L16692] |
| 129 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16693-L16702] |
| 130 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16703-L16712] |
| 131 | Outside package | test_helper | [docs/registry/implementation-map.yaml:L16713-L16724] |
| 132 | Outside package | independent | [docs/registry/implementation-map.yaml:L16725-L16735] |
| 133 | Outside package | independent | [docs/registry/implementation-map.yaml:L16736-L16746] |
| 134 | Outside package | independent | [docs/registry/implementation-map.yaml:L16747-L16758] |
| 135 | Outside package | independent | [docs/registry/implementation-map.yaml:L16759-L16768] |

Exact package grep, using Python `re`, case-sensitive and line by line, retains all three signals because none matches `structural/parachute` under case-insensitive matching; hits are assigned to their enclosing definition, with module statements grouped together. [docs/registry/implementation-map.yaml:L15369-L15375]

```python
re.search('(?:0\\.5|1\\s*/\\s*2).*(?:rho|density).*(?:cd|drag|area)|(?:wind|current|drag|lift)_forces?|calculate_(?:wind|current)|OCIMF|current_profile|wind_height_coefficient', line)
re.search('OCIMF|wind.load|current.load|wind.force|current.force|drag.coefficient|current.profile', line)
re.search('Cxw|Cyw|CXw|CYw|CXc|CYc|drag_coefficient|lift_coefficient|wind_speed|current_velocity', line)
```

| Hit definition | Classification | Evidence at the code pin |
| --- | --- | --- |
| `<module>` (1 hit lines) | Import helper | [src/digitalmodel/structural/parachute/chute_assessment.py:L11-L19] |
| `LoadCaseResult` (2 hit lines) | Data holder | [src/digitalmodel/structural/parachute/chute_assessment.py:L42-L52] |
| `calculate_dual_chute_drag` (2 hit lines) | Listed row 86; delegates to single drag | [src/digitalmodel/structural/parachute/chute_assessment.py:L81-L100] |
| `calculate_aero_lift` (1 hit lines) | Own lift formula, explicitly listed in section 3 | [src/digitalmodel/structural/parachute/chute_assessment.py:L103-L111] |
| `assess_all_load_cases` (7 hit lines) | Listed row 87; orchestration helper | [src/digitalmodel/structural/parachute/chute_assessment.py:L131-L186] |
| `simulate_deployment_transient` (4 hit lines) | Listed row 88; own transient/car-drag algorithm | [src/digitalmodel/structural/parachute/chute_assessment.py:L189-L293] |
| `export_results_yaml` (3 hit lines) | Listed row 89; serialization helper | [src/digitalmodel/structural/parachute/chute_assessment.py:L296-L321] |
| `calculate_drag_force` (2 hit lines) | Listed row 90; own drag formula | [src/digitalmodel/structural/parachute/parachute_drag.py:L37-L74] |

A8 inventory PASS: 135 of 135 rows, including all five in-package rows. Counted-family grep PASS: eight hit definitions, 22 hit lines, zero further unlisted implementations after the section-3 lift entry; no path-vocabulary signal excluded. This assessment uses the explicit inventory and classifications above. [src/digitalmodel/structural/parachute/chute_assessment.py:L81-L111] [src/digitalmodel/structural/parachute/parachute_drag.py:L37-L74]

Whole-repository discovery used the same three case-sensitive map expressions on 6,747 tracked Python files at the code pin, including scripts: 342 matching files and 2,175 matching lines, with 27 matching script files. These are discovery candidates, not a count of confirmed independent implementations; classification outside the package remains issue pending (4). [docs/registry/implementation-map.yaml:L1-L4]

Map follow-up: the lift formula needs a map row; tube sections, frame stiffness, stress/unity, connections, sizing, tire traction and CAD arc geometry need family coverage with package rows before they can become counted families; issue pending (4). [src/digitalmodel/structural/parachute/chute_assessment.py:L103-L128] [src/digitalmodel/structural/parachute/frame_model.py:L23-L46] [src/digitalmodel/structural/parachute/frame_solver.py:L52-L117] [src/digitalmodel/structural/parachute/member_check.py:L22-L209] [src/digitalmodel/structural/parachute/stroud_sizing.py:L40-L73] [src/digitalmodel/structural/parachute/freecad_frame_builder.py:L63-L123]

## 10. Coverage and open items

Coverage record: all 11 package Python files, the package worked-example document and all 10 test files were read; the table below records the test-file distribution at the code pin, with CAD cases distinguished from executed calculations. [src/digitalmodel/structural/parachute/__init__.py:L1-L21] [src/digitalmodel/structural/parachute/docs/worked-synthetic-canopy-drag.md:L1-L28]

| Test file under `tests/structural/parachute/` | Collected | Passed | Skipped / CAD NOT_RUN | Source inspected |
| --- | --- | --- | --- | --- |
| `test_chute_assessment.py` | 26 | 26 | 0 | [tests/structural/parachute/test_chute_assessment.py:L30-L278] |
| `test_documented_synthetic_canopy_drag.py` | 2 | 2 | 0 | [tests/structural/parachute/test_documented_synthetic_canopy_drag.py:L84-L110] |
| `test_frame_geometry_3d.py` | 41 | 41 | 0 | [tests/structural/parachute/test_frame_geometry_3d.py:L14-L242] |
| `test_frame_model.py` | 23 | 23 | 0 | [tests/structural/parachute/test_frame_model.py:L26-L145] |
| `test_frame_solver.py` | 14 | 14 | 0 | [tests/structural/parachute/test_frame_solver.py:L25-L232] |
| `test_frame_viewer_3d.py` | 32 | 31 | 1 browser | [tests/structural/parachute/test_frame_viewer_3d.py:L42-L374] |
| `test_freecad_frame_builder.py` | 7 | 0 | 7 | [tests/structural/parachute/test_freecad_frame_builder.py:L37-L116] |
| `test_member_check.py` | 16 | 16 | 0 | [tests/structural/parachute/test_member_check.py:L30-L168] |
| `test_parachute_drag.py` | 23 | 23 | 0 | [tests/structural/parachute/test_parachute_drag.py:L32-L305] |
| `test_stroud_sizing.py` | 8 | 8 | 0 | [tests/structural/parachute/test_stroud_sizing.py:L12-L64] |
| Total | 192 | 184 | 8 | Section-4 command; 7 CAD cases NOT_RUN and 1 Edge/Chrome case unavailable. Viewer writes/reads pass under CP1252 (SF76 closed). Historical node-status records are not shipped (E2). |

Package files read: `__init__.py`, `chute_assessment.py`, `parachute_drag.py`, `stroud_sizing.py`, `frame_model.py`, `frame_solver.py`, `member_check.py`, `frame_geometry_3d.py`, `freecad_frame_builder.py`, `generate_frame_cad.py`, `frame_viewer_3d.py`; their methods and pitfalls are located in sections 2, 3, 6 and 9. [src/digitalmodel/structural/parachute/__init__.py:L1-L21] [src/digitalmodel/structural/parachute/generate_frame_cad.py:L20-L56] [src/digitalmodel/structural/parachute/frame_viewer_3d.py:L493-L513]

Documentation discovery used `rg -n -i parachute docs --glob '*.md' --glob '*.yaml'`; the inspected Markdown findings are the package synthetic example, `docs/roadmaps/structural-parachute-example.md`, `docs/registry/implementation-map.md`, `docs/integrations/open-source-cae-register.md`, and the domain `README.md`, `parachute-aerodynamics.md`, `material-properties-4130.md`, `structural-analysis-method.md`, plus 1 discovery file with an identifying name, which is omitted here for codenaming; issue pending (5). [src/digitalmodel/structural/parachute/docs/worked-synthetic-canopy-drag.md:L1-L28] [docs/domains/structural/parachute/README.md:L6-L13] [docs/registry/implementation-map.md:L24-L24] [docs/integrations/open-source-cae-register.md:L18-L18]

The calculation-citation contract was readable, but the numerical outputs here contain no Citation sidecar in the result types or YAML export; source reconciliation and sidecar adoption remain issue pending (3). [src/digitalmodel/structural/parachute/parachute_drag.py:L12-L22] [src/digitalmodel/structural/parachute/chute_assessment.py:L22-L69] [src/digitalmodel/structural/parachute/chute_assessment.py:L296-L321]

| Open item | Responsible role | Tracking and closure evidence |
| --- | --- | --- |
| Invalid inputs and silent zero/lookup outcomes, including signed-stress and missing-load-node behavior; viewer encoding (SF76) closed by #1684 | Calculation maintainer | issue pending (1); add finite/domain and missing-data tests for section-6 branches; viewer writes/reads now use explicit UTF-8 and pass under CP1252 |
| U2: CAD unit scaling, bend fallbacks and seven unavailable CAD test cases; viewer curve corrected by #1684 | CAD maintainer | issue pending (2); round-trip measured dimensions, forced-fallback checks and all seven CAD tests passing |
| U1: empirical coefficients, sizing rules and preset physical applicability; U3: source editions, material discrepancy, allowable basis and missing Citation sidecars | Calculation maintainer | issue pending (3); authoritative sources, applicable clauses and independent physical/transient sensitivity checks, including Cx peak-to-steady recovery and impulse/speed-history benchmarks (SF77) |
| Implementation-map follow-up for lift and the uncovered method families listed in section 9; repository-wide discovery candidates need classification | Map maintainer | issue pending (4); revise the map before counting the new families |
| Identifying discovery material, preset design-basis provenance and private-sidecar separation | Documentation maintainer | issue pending (5); codename discovery material; sidecar entry: to be created; no private values included here |
| Full module worked example, independent frame/load integration and complete generated QA appendix | Calculation maintainer | #3309; independent integrated evidence beyond the bounded arithmetic examples |
| Wider deployment and cross-tool comparison | Structural maintainer | #1677 and #1703; independent time histories and comparison evidence rather than scalar opening-factor agreement |

The five numbered issue drafts are retained beside the pack in [ISSUES_DRAFT.md](ISSUES_DRAFT.md); their numbers are local draft references, not published issue numbers. Publication and replacement of the markers remain pending.

No second-provider clearance is recorded by this draft; the required next action is independent citation, gate, example and silent-failure review under AQG r0 section 11.
