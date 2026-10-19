# Planing hull module knowledge pack

| Header | Value |
| --- | --- |
| Module | `hydrodynamics/planing_hull` |
| Repository | `digitalmodel` |
| Path | `src/digitalmodel/hydrodynamics/planing_hull` |
| Pack revision | 0 |
| Code revision | `f4032a2ecaac110244919db23c5656d1c0d1cd5a` |
| Analysis family | Engineering calculations (`EC`); Python calculations, AQG r0 §10 |
| Quality manager role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The public API exposes geometry, strip forces, a time integrator, the RAO result container and `compute_rao`; the time integrator advances heave and pitch in regular head seas. [src/digitalmodel/hydrodynamics/planing_hull/__init__.py:L25-L35] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L300-L368]

The implementation uses constant deadrise, fixed maximum half-beam for added mass, estimated mass and inertia, linear restoring and prescribed wave excitation; these are model assumptions rather than an experimentally qualified operating envelope. [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L63-L72] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L121-L198] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L274-L294]

The parent hydrodynamics routing entry says `production`; it does not provide a separate maturity assessment for this submodule at the code pin. [docs/registry/module-routing.yaml:L163-L184]

This pack covers the four Python files in the package and their dedicated offline tests; the existing nonzero-amplitude example checks qualitative bounds rather than agreement with independent measurements. [src/digitalmodel/hydrodynamics/planing_hull/__init__.py:L25-L35] [tests/unit/hydrodynamics/test_planing_hull.py:L304-L330]

## 2. Inputs and outputs

Inputs are Python objects and scalars; geometry stores lengths in metres and deadrise in degrees, converting the latter to radians through `deadrise_rad`. [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L5-L6] [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L33-L67]

| Input / source | Unit | Allowed range or implemented guard | Default | Evidence |
| --- | --- | --- | --- | --- |
| `PlaningHullGeometry.length` | m | `> 0` | Required | [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L20-L21] [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L43-L46] |
| `beam` | m | `> 0` | Required | [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L22-L23] [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L47-L50] |
| `deadrise` | degree | Strictly between 0 and 90 | Required | [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L24-L25] [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L51-L54] |
| `chine_height` | m | `>= 0`; stored, absent from force formulae | Required | [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L26-L27] [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L55-L58] [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L78-L211] |
| `lcg` | m from bow | Strictly between 0 and length | Required | [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L28-L30] [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L59-L62] |
| Strip model / motion solver `n_strips` | count | Used directly as divisor and NumPy sample count; no explicit range guard | 20 | [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L53-L72] |
| Strip model / motion solver `rho` | kg/m³ | No explicit finite or positive guard | 1025.0 | [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L34-L35] [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L53-L63] |
| Motion solver / `compute_rao` `dt` | s | Used directly; no explicit range guard | 0.005 | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L99-L111] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L339-L366] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L375-L383] |
| Motion solver `damping_fraction` | 1 | No explicit range guard | 0.10 | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L99-L111] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L192-L196] |
| `integrate` / `compute_rao` `speed` | m/s | No explicit range guard | Required | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L314-L323] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L423-L435] |
| `wave_freq` | rad/s | No explicit positive guard; convenience path divides by frequency | Required | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L318-L321] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L423-L425] |
| `wave_steepness` | 1, `k*a` | Exactly zero short-circuits `compute_rao`; no sign or finite guard | Required | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L399-L400] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L413-L425] |
| `integrate.t_total` | s | At least two samples forced by `max`, including nonpositive durations | Required | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L322-L344] |
| `compute_rao.n_periods` | count of incident-wave periods | No explicit lower bound; three periods discarded | 10 | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L375-L406] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L423-L449] |
| Strip `c`, `v_n`, contact `dt`, submergence, trim, wetted length | m; m/s; s; m; rad; m respectively | Nonpositive entry/contact/submergence/speed returns zero; lift ignores wetted length | Required by method | [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L78-L211] |

| Output | Unit / meaning | Default or limiting behaviour | Evidence |
| --- | --- | --- | --- |
| `aspect_ratio`, `half_beam`, `deadrise_rad` | 1; m; rad | Derived from stored geometry | [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L64-L77] |
| `strip_x`, `dx` | m, bow-to-stern strip centres and length increment | Uniform midpoint strips | [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L63-L69] |
| Added mass per length; half-wetted beam | kg/m; advertised m | Half-wetted beam returns zero for nonpositive entry/contact time; dimensional defect in section 6 | [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L78-L119] |
| Entry force, buoyancy, lift per length | Advertised N/m, upward | Entry-force dimension defect in section 6 | [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L121-L211] |
| `integrate`: time, heave, pitch arrays | s; m; rad | Equal lengths; zero initial displacement and velocity | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L325-L368] |
| `PlaningRAO`: heave, pitch, second-harmonic heave | m/m; rad/rad; m/m | Five zero fields for zero steepness or insufficient steady samples | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L44-L71] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L413-L449] |
| Heave and pitch phase | rad, raw FFT argument | Docstring describes wave-crest reference; implementation returns bin arguments | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L59-L62] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L451-L479] |

The strip coordinate starts at the bow and increases toward the stern; the moment arm is `lcg-x`, with excitation moment multiplied by its negative, and entry-force sign is upward for positive downward strip entry. [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L63-L69] [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L128-L135] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L280-L292]

The wave routine documents head seas with phase `k*x + (omega+k*speed)*t`; excitation uses only the positive portion of its vertical velocity, while the surface-elevation routine is separate from that force route. [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L218-L248] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L280-L294]

UNVERIFIED: a consistent global origin, physical positive-pitch convention and calibrated phase reference are not established by the local-coordinate comments and raw FFT arguments; issue pending (3). [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L218-L248] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L280-L292] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L438-L479]

## 3. Method and governing standards

The implemented equations below describe the code; dimensional concerns and missing qualification prevent treating them as independent reference calculations. [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L10-L26] [tests/unit/hydrodynamics/test_planing_hull.py:L304-L330]

| Definition | Implemented method | Basis and limitation | Evidence |
| --- | --- | --- | --- |
| `PlaningStripModel.__init__` | `dx=L/N`, midpoint strip locations, constant `tan(beta)` | Uniform discretisation | [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L53-L72] |
| `added_mass_per_length` | `(pi/2)*rho*c²` | Algebraic quadratic mass law | [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L78-L94] |
| `half_wetted_beam` | `sqrt(2*v_n*dt/tan(beta))`, otherwise zero | Contact time proxy; dimensional concern | [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L96-L119] |
| `water_entry_force_per_length` | `(pi/2)*rho*v_n²/tan(beta)`, otherwise zero | `dt` unused; dimensional and coefficient concerns | [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L121-L146] |
| `buoyancy_force_per_length` | `rho*9.81*h²/tan(beta)` for positive depth | Wedge hydrostatics; no chine cap | [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L152-L174] |
| `planing_lift_per_length` | `0.5*rho*speed²*beam*pi*abs(trim)` for positive speed | Ignores wetted length; trim sign discarded | [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L180-L211] |
| `_estimate_mass`, `_estimate_iyy` | `m=rho*B²*L*0.6/(4*tan(beta))`, `Iyy=m*(0.25*L)²` | Heuristic mass and radius of gyration | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L121-L141] |
| `_total_added_mass` | Sum strip masses and squared longitudinal arms at `c=B/2` | Fixed full-beam wetting, independent of time | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L147-L165] |
| `_restoring_coefficients` | `Awp=0.75*L*B`, `C33=rho*g*Awp`, `C55=C33*L²/12`; damping from critical fractions | `speed` unused; frequency-squared floor in square roots | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L167-L198] |
| `_wave_number` | `k=omega²/G` | Deep-water dispersion; no depth argument | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L204-L207] |
| `_wave_surface` | `a*cos(phi)+(k*a²/2)*cos(2*phi)` | Own surface equation; not called by excitation | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L209-L230] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L280-L294] |
| `_wave_vertical_velocity` | `a*(omega+k*speed)*sin(phi)` | Own kinematics equation used by excitation | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L232-L248] |
| `_excitation_forces` | Sum positive-entry slam forces and moments over strips | Delegates velocity and slam evaluation; no hull-state feedback in force | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L254-L294] |
| `integrate` and nested `_deriv` | Four-stage RK4 on two diagonal forced oscillators | Calls wave number and excitation; stores pre-step state; nominal end time excluded | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L331-L368] |
| `compute_rao` | Repeats `k=wave_freq²/G`, integrates incident periods, discards three, selects nearest encounter and second-harmonic FFT bins; amplitude `2*abs(FFT)/n` | Own dispersion expression plus delegating integration/FFT postprocessing; no window or frequency interpolation | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L413-L479] |
| `_nearest_freq_index` | Nearest absolute-frequency difference, clamped to array | Empty frequency array silently selects index zero | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L483-L488] |

UNVERIFIED: the docstring attributions to Wagner (1932), Tavakoli et al. (2026), *Physics of Fluids* 38, 027129, sections II–III, and the stated Savitsky approximation have not been checked against primary publications; issue pending (4). [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L6-L26] [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L186-L191]

No licensed design-standard clause or `Citation` sidecar is specified by these calculation bodies; their provenance is limited to docstring literature attributions and the numerical expressions above. [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L6-L26] [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L29-L35] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L26-L37] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L474-L480]

## 4. Validated worked examples and regression examples

The independent limiting solution for the homogeneous heave/pitch equations with zero initial state and zero forcing is identically zero; this validates only the unforced limit, with the implemented residual bounds below. [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L310-L312] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L332-L354] [tests/unit/hydrodynamics/test_planing_hull.py:L184-L196]

All cases use in-code inputs rather than an input file; PASS means the named node passed offline at the code pin before and after documentation edits, with the dedicated module test command in section 10. [tests/unit/hydrodynamics/test_planing_hull.py:L26-L40] [tests/unit/hydrodynamics/test_planing_hull.py:L119-L136] [tests/unit/hydrodynamics/test_planing_hull.py:L184-L196] [tests/unit/hydrodynamics/test_planing_hull.py:L274-L330]

| Case / classification | Source and input | Expected value and tolerance basis | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- |
| Unforced motion / validated limiting case | Closed-form zero solution of homogeneous equations; fixture geometry, speed 3.0 m/s, frequency 5.0 rad/s, steepness 0, duration 2.0 s | After 0.5 s, absolute heave < 1e-6 m and pitch < 1e-6 rad; numerical-zero bounds set by test | `tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningMotionSolver::test_zero_steepness_produces_small_response` | BASE PASS; AFTER PASS. [tests/unit/hydrodynamics/test_planing_hull.py:L184-L196] |
| Added-mass scaling / regression property | In-code quadratic law, half beams 0.04 and 0.08 m | Mass ratio 4.0 (1), relative tolerance 1e-6 from test; does not verify coefficient | `tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningStripModel::test_added_mass_scales_with_beam_squared` | BASE PASS; AFTER PASS. [tests/unit/hydrodynamics/test_planing_hull.py:L119-L124] |
| Zero entry / regression branch | In-code zero-velocity branch, contact increment 0.001 s | Zero advertised N/m, absolute tolerance 1e-10 from test; dimensions remain disputed | `tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningStripModel::test_water_entry_force_zero_for_zero_velocity` | BASE PASS; AFTER PASS. [tests/unit/hydrodynamics/test_planing_hull.py:L133-L136] |
| Zero second harmonic / regression shortcut | In-code zero-steepness return | Zero (1), absolute tolerance 1e-4 from test; bypasses integration | `tests/unit/hydrodynamics/test_planing_hull.py::TestComputeRao::test_second_harmonic_zero_at_zero_steepness` | BASE PASS; AFTER PASS. [tests/unit/hydrodynamics/test_planing_hull.py:L274-L282] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L413-L421] |
| Nonzero RAO bounds / regression sanity case | Synthetic fixture, speed computed from dimensionless Froude input, wave frequency 6.28 rad/s and steepness 0.10 | Heave strictly 0–10 (1), pitch strictly 0–20 (1); broad test bounds, no published amplitude target | `tests/unit/hydrodynamics/test_planing_hull.py::TestComputeRao::test_fridsma_validation_froude_119` | BASE PASS; AFTER PASS. [tests/unit/hydrodynamics/test_planing_hull.py:L304-L330] |

## 5. QA gates

Gate applicability follows Python calculations: G0 inputs/units/citations, G1 imports/tests, G2 independent checks, G3 and G4 inapplicable, and G5 evidence (AQG r0, §10 “Family mapping”, §9 “G5 QA appendix”).

| Gate | Disposition | Module-specific route and limitation |
| --- | --- | --- |
| G0 | Applicable | `C-geometry`, `C-strips`; caller must additionally qualify unchecked finite/range inputs and coefficient provenance. [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L42-L62] [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L53-L72] |
| G1 | Applicable | Python import route through `digitalmodel.hydrodynamics.planing_hull`, then the dedicated pytest file; no external-program writer or load route occurs in the public exports. [src/digitalmodel/hydrodynamics/planing_hull/__init__.py:L25-L35] [tests/unit/hydrodynamics/test_planing_hull.py:L13-L19] |
| G2 | Applicable | `C-rest`, `C-scale`, `C-entry`, `C-harmonic`, `C-bounds`, `C-dimensions`; independently checked unforced limit does not qualify nonzero response. [tests/unit/hydrodynamics/test_planing_hull.py:L119-L136] [tests/unit/hydrodynamics/test_planing_hull.py:L184-L196] [tests/unit/hydrodynamics/test_planing_hull.py:L274-L330] |
| G3 | NOT_APPLICABLE | Python calculation family has no separate licensed short-dynamics gate (AQG r0, §10 “Family mapping”); its local integrator is still exercised by G1. [tests/unit/hydrodynamics/test_planing_hull.py:L159-L196] |
| G4 | NOT_APPLICABLE | No diffraction mesh or panel-solver calculation in this package's exported route (AQG r0, §10 “Family mapping”). [src/digitalmodel/hydrodynamics/planing_hull/__init__.py:L25-L35] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L423-L479] |
| G5 | Applicable | `C-evidence`: attach node outcomes, code pin, input provenance, unresolved defects and dispositions before calculation results are used (AQG r0, §9 “G5 QA appendix”). |

These are reproducible checks, not clearance of the physical model; the dimensional check fails on the unchanged code pin and remains an open defect. [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L96-L146]

| Check ID / gate | Executable route | Numeric pass criterion and unit | Evidence / observed result |
| --- | --- | --- | --- |
| `C-geometry` / G0 | Dedicated nodes `test_invalid_length_raises` and `test_invalid_deadrise_raises` in the test file below | 2 ValueError results; 0 invalid fixtures accepted (count) | BASE/AFTER PASS; only those two invalid cases covered. [tests/unit/hydrodynamics/test_planing_hull.py:L69-L87] |
| `C-strips` / G0 | `TestPlaningStripModel` layout, width and default-count nodes in the test file below | 10 configured centres; default 20 centres (count); minimum x >= 0 m, maximum x <= L m, dx > 0 m | BASE/AFTER PASS. [tests/unit/hydrodynamics/test_planing_hull.py:L101-L111] [tests/unit/hydrodynamics/test_planing_hull.py:L138-L141] |
| `C-import` / G1 | Run entire `tests/unit/hydrodynamics/test_planing_hull.py` | 26 collected, 26 passed, 0 failed, 0 skipped (count) | BASE/AFTER PASS; public classes/functions imported. [tests/unit/hydrodynamics/test_planing_hull.py:L13-L19] [tests/unit/hydrodynamics/test_planing_hull.py:L47-L330] |
| `C-shape` / G1 | `tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningMotionSolver::test_integration_returns_time_series` | Time sample count > 1; heave and pitch sample counts equal time count, 0 mismatches (count) | BASE/AFTER PASS. [tests/unit/hydrodynamics/test_planing_hull.py:L159-L170] |
| `C-finite` / G1 | `test_no_nan_in_integration`, `test_rao_values_are_finite` in the test file below | 0 NaNs in heave/pitch time histories (count); 5 finite RAO/phase fields (count) | BASE/AFTER PASS; time-history assertion does not reject infinity. [tests/unit/hydrodynamics/test_planing_hull.py:L172-L182] [tests/unit/hydrodynamics/test_planing_hull.py:L249-L260] |
| `C-rest` / G2 | Section-4 unforced-motion node | Absolute heave < 1e-6 m and absolute pitch < 1e-6 rad after 0.5 s | BASE/AFTER PASS; closed-form limit. [tests/unit/hydrodynamics/test_planing_hull.py:L184-L196] |
| `C-scale` / G2 | Section-4 added-mass node | Ratio 4.0 (1), relative discrepancy <= 1e-6 (1) | BASE/AFTER PASS; coefficient untested. [tests/unit/hydrodynamics/test_planing_hull.py:L119-L124] |
| `C-entry` / G2 | Section-4 zero-entry node | Absolute force <= 1e-10 advertised N/m | BASE/AFTER PASS; only zero branch. [tests/unit/hydrodynamics/test_planing_hull.py:L133-L136] |
| `C-harmonic` / G2 | Section-4 zero-harmonic node and `test_second_harmonic_increases_with_steepness` in the test file below | At zero steepness, absolute harmonic <= 1e-4 (1); 0 decreases over four steepness samples (count) | BASE/AFTER PASS. [tests/unit/hydrodynamics/test_planing_hull.py:L274-L302] |
| `C-bounds` / G2 | Section-4 nonzero-RAO node | Heave > 0 and < 10 (1); pitch > 0 and < 20 (1) | BASE/AFTER PASS; qualitative bounds only. [tests/unit/hydrodynamics/test_planing_hull.py:L304-L330] |
| `C-dimensions` / G2 | Substitute SI dimensions into `half_wetted_beam` and `water_entry_force_per_length`; separately differentiate the stated added-mass law with its stated contact law | 0 output-dimension mismatches (count), beam unit m and force-per-length unit N/m; derivative/code coefficient ratio 1.0 (1) | BASE finding: 2 dimension mismatches; derivative/code ratio 2.0 (1). Code formula gives square root of m and N/m² respectively; issue pending (1). [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L12-L26] [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L96-L146] |
| `C-evidence` / G5 | Resolve pack citations at the pin, compare section-4 nodes against the pytest outcome list and check six gate rows | 0 unresolved citations (count); 5 of 5 case results recorded (count); 6 gate dispositions (count) | Documentation evidence check; independent clearance remains pending (AQG r0, §9 “G5 QA appendix”, §11 “Sign-off and independence”). |

The complete test-file path used by abbreviated node names above is `tests/unit/hydrodynamics/test_planing_hull.py`; the fixture values and test assertions are retained there. [tests/unit/hydrodynamics/test_planing_hull.py:L26-L40] [tests/unit/hydrodynamics/test_planing_hull.py:L47-L330]

## 6. Known failure modes and pitfalls

sweep: 32 found

The count below is 32 reviewed sites or groups of adjacent sites, including intentional numerical zeros and unit assumptions; it is not a count of 32 confirmed defects. [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L20-L77] [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L34-L211] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L35-L488]

Exact sweep commands (run on the package at the code pin):

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|return 0|= 0(\.0)?\b|1970|int\(|float\(|max\(|min\(|rho|\[m|\[rad|\[s|fraction|0\.6|0\.25|0\.75' src/digitalmodel/hydrodynamics/planing_hull --glob '*.py'
rg -n 'except|continue|return None|return \[\]|return \{\}|1970|open\(|save|cache|int\(|float\(' src/digitalmodel/hydrodynamics/planing_hull --glob '*.py'
```

| Site | Symptom / cause | Detection / tracking | Evidence at code pin |
| --- | --- | --- | --- |
| 1. `compute_rao`, short steady window | Fewer than 4 samples after discard returns five zeros, indistinguishable from calm input | no detection check exists; issue pending (2) | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L437-L449] |
| 2. `compute_rao`, zero steepness | Returns five zeros before validating frequency, duration or discretisation; intentional shortcut can mask invalid settings | `C-harmonic` covers valid calm input; no detection check exists for invalid calm inputs; issue pending (2) | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L413-L421] |
| 3. `integrate`, zero wave number | Sets amplitude to zero when k is not positive, whereas convenience path divides by k | no detection check exists; issue pending (2) | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L331-L332] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L423-L425] |
| 4. `_nearest_freq_index`, empty input | Empty array returns index 0 rather than signalling missing frequency data | no detection check exists; issue pending (3) | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L483-L488] |
| 5. `half_wetted_beam`, nonpositive entry/contact | Nonpositive velocity or contact duration returns zero; invalid time can look dry | no detection check exists; issue pending (2) | [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L117-L119] |
| 6. `water_entry_force_per_length`, nonpositive entry | Zero for nonpositive velocity; `dt` never affects positive force | `C-entry` covers zero only; no detection check exists for negative velocity or ignored time; issue pending (2) | [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L133-L146] |
| 7. `buoyancy_force_per_length`, nonpositive depth | Zero for nonpositive submergence | no detection check exists; issue pending (2) | [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L171-L174] |
| 8. `planing_lift_per_length`, nonpositive speed | Zero for nonpositive speed; negative trim becomes positive via `abs`; wetted length ignored | no detection check exists; issue pending (2) | [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L199-L211] |
| 9. Pitch normalisation | Nonpositive `k*wave_amp` silently sets pitch RAO to zero; negative steepness can yield negative heave ratios | no detection check exists; issue pending (2) | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L469-L479] |
| 10. `_restoring_coefficients`, natural frequencies | Two `max(...,1e-10)` floors can conceal negative stiffness/effective-mass ratios; damping is not validated | no detection check exists; issue pending (2) | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L188-L196] |
| 11. `integrate`, duration conversion | `int(t_total/dt)` truncates duration; `max(2,...)` forces two samples for nonpositive duration | `C-shape` covers normal arrays only; no detection check exists for invalid duration; issue pending (2) | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L339-L366] |
| 12. `compute_rao`, discard conversion | `int(3*period/dt)` truncates discard; neither minimum window nor convergence is enforced | no detection check exists; issue pending (3) | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L437-L449] |
| 13. Nearest-bin selection and integer clamp | Nearest bin is accepted even outside represented frequencies; alias and leakage errors have no reported bound | no detection check exists; issue pending (3) | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L451-L464] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L483-L488] |
| 14. Output `float()` conversions | FFT phases and RAO fields are cast without finite validation; casts consume numeric arrays, not external text | `C-finite` for one valid input; no detection check exists for invalid inputs; issue pending (2) | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L466-L479] |
| 15. Geometry length units/validation | Metres and positive length assumed; no explicit `isfinite` check | `C-geometry` tests negative length only; issue pending (2) | [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L20-L21] [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L43-L46] |
| 16. Geometry beam units/validation | Metres and positive beam assumed; NaN is not rejected by the `<=` guard | no detection check exists; issue pending (2) | [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L22-L23] [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L47-L50] |
| 17. Geometry deadrise units/validation | Degrees stored, radians derived; strict angular range, no explicit type check | `C-geometry` checks upper range; conversion property test checks one angle; issue pending (2) | [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L24-L25] [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L51-L54] [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L64-L67] |
| 18. Geometry chine units/validation | Metres assumed; NaN passes negative guard; chine height not used by force law | no detection check exists; issue pending (2) | [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L26-L27] [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L55-L58] [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L78-L211] |
| 19. Geometry LCG and derived outputs | Bow-origin metres, strict interior LCG; ratio dimensionless, half beam metres; no global frame contract | no detection check exists for global-frame agreement; issue pending (3) | [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L28-L30] [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L59-L77] |
| 20. Strip constructor | Density default 1025.0 kg/m³ and 20 strips; no finite/positive guards for density or strip count; midpoint x in metres | `C-strips` covers positive counts; no detection check exists for invalid density/count; issue pending (2) | [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L34-L72] |
| 21. Motion constructor and convenience defaults | Step 0.005 s, 20 strips, density 1025.0 kg/m³, damping fraction 0.10, 10 incident periods in convenience call; no explicit range checks | `C-import` exercises defaults; no detection check exists for invalid defaults overrides; issue pending (2) | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L35-L37] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L99-L115] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L375-L383] |
| 22. Mass estimate | Fill fraction 0.6 is an assumed coefficient, no mass input | no detection check exists; issue pending (4) | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L121-L132] |
| 23. Pitch inertia estimate | Gyration radius 0.25*L assumed | no detection check exists; issue pending (4) | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L134-L141] |
| 24. Restoring estimate | Waterplane coefficient 0.75 and uniform pitch factor L²/12 assumed; speed unused | no detection check exists; issue pending (4) | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L167-L198] |
| 25. Added-mass summation | A33 and A55 start at zero; full half-beam used for all strips; empty discretisation would leave zeros | `C-strips` covers positive strip count; no detection check exists for empty strips or wetting model; issue pending (2) | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L147-L165] |
| 26. Excitation accumulation / clipping | Force and moment start at zero; negative wave velocities clipped to zero; hull displacement absent from excitation | `C-rest` checks unforced limit; no detection check exists for nonzero force reference; issue pending (4) | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L277-L294] |
| 27. RK4 array/state initialisation | Time/heave/pitch and four-component state start at zero; derivative array starts at zero then all four entries assigned | `C-rest`, `C-shape`; intentional initial condition, no lost derivative entry | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L339-L359] |
| 28. Wave kinematics / gravity | Deep water, G=9.81 m/s², frequency rad/s and head-sea encounter phase assumed; surface has second order while velocity is sinusoidal | no detection check exists for independent kinematics or sign/phase reference; issue pending (3) | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L35-L37] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L204-L248] |
| 29. Strip formula units | Added mass uses kg/m and squares negative c without a guard; contact formula gives square root of m despite m docstring; slam gives N/m² despite N/m docstring; stated derivative coefficient differs | `C-scale` checks mass scaling; `C-dimensions` detects dimensional and coefficient defects, issue pending (1); no detection check exists for negative c, issue pending (2) | [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L12-L26] [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L78-L146] |
| 30. Buoyancy and lift units | Buoyancy fixes gravity at 9.81 m/s²; wedge formula continues above chine; lift small-trim approximation has no trim envelope | no detection check exists; issue pending (4) | [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L152-L211] |
| 31. Force and motion sign/units | Per-length forces integrated with dx; pitch lever negative LCG offset; heave m and pitch rad, no explicit global frame transform | no detection check exists for physical sign convention; issue pending (3) | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L280-L294] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L325-L353] |
| 32. RAO/phase interpretation | `2*abs(FFT)/n` and raw `np.angle` returned as dimensionless response and radians; discarded-window origin not corrected | `C-finite`, `C-bounds` check valid numbers only; no detection check exists for amplitude or phase accuracy; issue pending (3) | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L49-L64] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L437-L479] |

No swallowed exception, record-skipping `continue`, `return None`, empty-list/dictionary error return, epoch fallback, external-text numeric conversion or empty-result cache write occurs in the four inspected calculation files; the numeric conversions and fallback sites are explicitly recorded above. [src/digitalmodel/hydrodynamics/planing_hull/__init__.py:L1-L35] [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L1-L77] [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L1-L211] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L1-L488]

Issue #619 is a capability-gap lead for class-rule ship slamming, not a confirmed incident in this module; the public calculation route contains strip forces and motion only, so no class-rule pressure-compliance detection check exists here. [src/digitalmodel/hydrodynamics/planing_hull/__init__.py:L25-L35] [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L121-L146]

## 7. Lessons log

This initial log records findings derived from the pinned implementation; further entries are appended after review.

| Date | Record | Lesson | Evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- | --- |
| 2026-10-18 | Initial calculation audit | Passing zero/qualitative tests do not establish nonzero physical accuracy | [tests/unit/hydrodynamics/test_planing_hull.py:L274-L330] | Sections 4–6 distinguish limiting validation from regression | Pending; issue pending (4) |
| 2026-10-18 | Initial calculation audit | A five-zero RAO can mean insufficient samples | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L437-L449] | Sections 2 and 6 identify ambiguity | Pending; issue pending (2) |
| 2026-10-18 | Initial dimensional audit | Output unit labels require a separate dimensional check | [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L96-L146] | `C-dimensions` and section-6 finding | Pending; issue pending (1) |

## 8. Ownership

Review responsibility for this draft: engineering calculations quality manager; deputy review responsibility: engineering calculations deputy quality manager; producing discipline: engineering calculations. Independent provider review and gate sign-off follow AQG r0, §11 “Sign-off and independence”.

Escalation for ambiguous equations, missing inputs, tolerance changes or unresolved gate failures is to the family quality manager; cross-family or scope decisions are for the maintainers (AQG r0, §11 “Sign-off and independence”).

These are review responsibilities for this pack, not a claim that any person has signed or cleared the calculation.

## 9. Dependencies and implementation inventory

The calculation files depend on Python `math`, `dataclasses`, typing and NumPy; local dependencies are geometry → strip model → motion/RAO calculation, with public re-exports from `__init__`. [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L9-L10] [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L29-L32] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L26-L33] [src/digitalmodel/hydrodynamics/planing_hull/__init__.py:L25-L35]

The dedicated test file consumes all five public objects; its data are Python fixture values, with no solver-version or external-file input in that test route. [tests/unit/hydrodynamics/test_planing_hull.py:L13-L40] [tests/unit/hydrodynamics/test_planing_hull.py:L240-L330]

The inventory below uses map commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` and declared data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; explicit `@` revisions on map citations distinguish that snapshot from the code pin. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L1-L3]

Exactly one family has an implementation row in the package: `linear-wave-kinematics`, with 1 in-package row and 21 rows overall; it is therefore the only counted family. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26253-L26502]

| Map row | Implementation path | Qualified definition | Map relation | Snapshot evidence |
| --- | --- | --- | --- | --- |
| 1 | `src/digitalmodel/hydrodynamics/diffraction/analytical_cylinder.py` | `wavenumbers` | `independent` | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26260-L26269] |
| 2 | `src/digitalmodel/hydrodynamics/diffraction/analytical_cylinder.py` | `wavenumbers.propagating` | `independent` | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26270-L26279] |
| 3 | `src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py` | `wave_number` | `independent` | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26280-L26289] |
| 4 | `src/digitalmodel/motion_forecast/reconstruct.py` | `wavenumber` | `independent` | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26290-L26299] |
| 5 | `src/digitalmodel/motion_forecast/reconstruct.py` | `wavenumber.f` | `independent` | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26300-L26309] |
| 6 | `src/digitalmodel/solvers/openfoam/validation/wave_tank.py` | `dispersion_wavenumber` | `independent` | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26310-L26319] |
| 7 | `src/digitalmodel/solvers/openfoam/wave_models.py` | `_solve_dispersion` | `independent` | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26320-L26329] |
| 8 | `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `solve_dispersion` | `independent` | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26330-L26339] |
| 9 | `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `velocity_transfer` | `independent` | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26340-L26349] |
| 10 | `tests/hydrodynamics/diffraction/test_analytical_cylinder.py` | `test_propagating_root_satisfies_dispersion_relation` | `test_helper` | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26350-L26361] |
| 11 | `tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py` | `TestWaveNumber.test_deep_water_limit` | `test_helper` | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26362-L26373] |
| 12 | `tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py` | `TestWaveNumber.test_dispersion_relation_satisfied` | `test_helper` | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26374-L26384] |
| 13 | `tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py` | `TestWaveNumber.test_scalar_input` | `test_helper` | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26385-L26395] |
| 14 | `tests/motion_forecast/test_reconstruct.py` | `test_finite_depth_wavenumber_reduces_to_deep` | `test_helper` | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26396-L26405] |
| 15 | `tests/motion_forecast/test_reconstruct.py` | `test_finite_depth_dispersion_holds_in_shallow_water` | `test_helper` | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26406-L26415] |
| 16 | `tests/solvers/openfoam/validation/test_wave_tank.py` | `test_dispersion_satisfies_relation` | `test_helper` | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26416-L26425] |
| 17 | `tests/solvers/openfoam/validation/test_wave_tank.py` | `test_dispersion_deep_water_limit` | `test_helper` | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26426-L26435] |
| 18 | `src/digitalmodel/hydrodynamics/planing_hull/solver.py` | `PlaningMotionSolver._wave_number` | `independent` | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26436-L26447] |
| 19 | `src/digitalmodel/solvers/openfoam/hull_free_surface.py` | `deep_water_wavelength` | `independent` | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26448-L26458] |
| 20 | `src/digitalmodel/solvers/openfoam/validation/wave_excited_body_rao.py` | `group_velocity` | `independent` | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26459-L26474] |
| 21 | `src/digitalmodel/hydrodynamics/sloshing.py` | `oval_tank_periods` | `independent` | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26475-L26486] |

The case-sensitive Python `re` search used these map strings exactly, including their escaping; no signal matched the package path below `src/digitalmodel/` when compared case-insensitively, so none was excluded.

```text
algorithm: \b(?:tanh|sinh|cosh)\s*\(|solve_dispersion|wave_number|wavenumber|velocity_transfer
vocabulary: wave.kinematics|dispersion|orbital.velocity|Airy
constants_or_keys: water_depth|omega|seabed|wave_number|wavenumber
```

The signal source and the sole in-package map row are retained in the snapshot. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26256-L26258] [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26436-L26447]

The AST audit assigned each matching line to its innermost enclosing function/class, with module-level text treated as one definition; seven package definitions matched, all in `solver.py`, and all are listed in section 3 or the inventory below. [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L167-L368] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L375-L480]

| Matching definition | Classification at code pin | Evidence |
| --- | --- | --- |
| `_restoring_coefficients` | Natural-frequency/damping calculation; frequency vocabulary, not another wave-kinematics implementation | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L167-L198] |
| `_wave_number` | Independent dispersion calculation, map row 18 | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L204-L207] |
| `_wave_surface` | Own wave-elevation equation, additional method already listed in section 3; map follow-up | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L209-L230] |
| `_wave_vertical_velocity` | Own orbital/encounter velocity equation, additional method already listed in section 3; map follow-up | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L232-L248] |
| `_excitation_forces` | Force accumulator delegates to the listed velocity and slam methods | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L274-L294] |
| `integrate` | Calls listed dispersion and excitation, then integrates motion | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L331-L368] |
| `compute_rao` | Delegates integration but also contains its own dispersion expression; explicitly listed in section 3; map follow-up | [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L423-L435] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L451-L479] |

A8 result: PASS, complete 21-row map-family inventory and 0 further second implementations absent from both sections 3 and 9; the duplicated dispersion expression and additional wave equations are disclosed above, rather than classified away as wrappers. [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L204-L248] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L423-L425]

The supplementary whole-repository scan enumerated every tracked Python file at the code pin, including `scripts/`; the counted signals hit 882 files, including 66 script files. This is discovery coverage, not a claim that every hit is an implementation; the mapped implementations are inventoried above and further non-package candidates remain map follow-up, issue pending (5).

A supplementary tracked-text search also covered non-Python files (196 matching files, including 1 script); its command was `git grep -I -l -E 'wave.?kinematics|orbital.?velocity|solve_dispersion|wave_number|wavenumber|Wagner|half_wetted_beam|water_entry_force_per_length|planing_lift_per_length' HEAD -- .` (execution record).

Two confirmed non-package map gaps are `rectangular_tank_periods` and `cylindrical_tank_periods` in `hydrodynamics/sloshing.py`: each independently evaluates finite-depth dispersion periods and neither is among the 21 rows above; these findings do not change A8 and are associated with #2620. [src/digitalmodel/hydrodynamics/sloshing.py:L100-L139]


The map follow-up also covers families without in-package rows: strip added mass and wedge entry/contact, hydrostatic wedge buoyancy and steady lift, mass/inertia/restoring estimates, RK4 motion integration and harmonic extraction; these do not enter the counted-family result until the map contains their in-package rows, issue pending (5). [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L78-L211] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L121-L198] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L300-L488]

## 10. Coverage and open items

All four package Python files were read: `__init__.py`, `geometry.py`, `strip_model.py`, `solver.py`; no nested source package or package-local README/documentation existed at the code pin. [src/digitalmodel/hydrodynamics/planing_hull/__init__.py:L1-L35] [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L1-L77] [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L1-L211] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L1-L488]

The documentation search found three documents under `docs/` containing planing-hull references: the implementation-map summary, a resistance-validation applicability discussion and a propeller/rudder applicability discussion; the latter two exclude planing hulls from their respective other-method envelopes, not from this module's code. [docs/registry/implementation-map.md:L29-L29] [docs/domains/openfoam/referent-free-resistance-validation-2026-08-19.md:L173-L175] [docs/domains/hydrodynamics/propeller-rudder-literature.md:L134-L141]

The routing entry, repository docs index/operator map, module test file and implementation-map contract test were also inspected; the contract test records the planing-hull dispersion symbol as a map seed, not a numerical response test. [docs/registry/module-routing.yaml:L163-L184] [docs/maps/digitalmodel-operator-map.md:L45-L45] [docs/README.md:L1-L15] [tests/docs/test_implementation_map.py:L134-L143]

Test inventory: 26 dedicated nodes, comprising 5 geometry, 8 strip-model, 5 motion-solver, 2 result-container and 6 convenience-RAO tests; no licensed solver is needed by their imports or bodies. [tests/unit/hydrodynamics/test_planing_hull.py:L9-L330]

The command form was `PYTHONPATH=src uv run --no-sources python -m pytest tests/unit/hydrodynamics/test_planing_hull.py -q -rA -p no:cacheprovider --basetemp=<scratch>` with the installed uv executable, shared cache and scratch temporary directory; no addopts override was used. Execution record at the code pin: BASE 26 run / 26 passed / 0 failed / 0 skipped / 0 NOT_RUN; AFTER 26 run / 26 passed / 0 failed / 0 skipped / 0 NOT_RUN.

All dedicated node outcomes (BASE PASS / AFTER PASS):

```text
tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningHullGeometry::test_create_with_required_fields
tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningHullGeometry::test_fridsma_hull_dimensions
tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningHullGeometry::test_invalid_length_raises
tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningHullGeometry::test_invalid_deadrise_raises
tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningHullGeometry::test_deadrise_in_radians_property
tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningStripModel::test_builds_correct_number_of_strips
tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningStripModel::test_strip_positions_span_hull_length
tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningStripModel::test_strip_width_positive
tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningStripModel::test_added_mass_per_strip_positive
tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningStripModel::test_added_mass_scales_with_beam_squared
tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningStripModel::test_water_entry_force_positive_for_downward_velocity
tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningStripModel::test_water_entry_force_zero_for_zero_velocity
tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningStripModel::test_default_n_strips
tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningMotionSolver::test_solver_creates_with_defaults
tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningMotionSolver::test_solver_configurable_time_step
tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningMotionSolver::test_integration_returns_time_series
tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningMotionSolver::test_no_nan_in_integration
tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningMotionSolver::test_zero_steepness_produces_small_response
tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningRAO::test_create_rao
tests/unit/hydrodynamics/test_planing_hull.py::TestPlaningRAO::test_rao_fields_are_floats
tests/unit/hydrodynamics/test_planing_hull.py::TestComputeRao::test_returns_planing_rao_instance
tests/unit/hydrodynamics/test_planing_hull.py::TestComputeRao::test_rao_values_are_finite
tests/unit/hydrodynamics/test_planing_hull.py::TestComputeRao::test_rao_non_negative
tests/unit/hydrodynamics/test_planing_hull.py::TestComputeRao::test_second_harmonic_zero_at_zero_steepness
tests/unit/hydrodynamics/test_planing_hull.py::TestComputeRao::test_second_harmonic_increases_with_steepness
tests/unit/hydrodynamics/test_planing_hull.py::TestComputeRao::test_fridsma_validation_froude_119
```

| Open item | Qualification needed | Responsible role | Tracking |
| --- | --- | --- | --- |
| Strip formula dimensions and derivative coefficient | Correct or substantiate contact/force expressions; `C-dimensions` currently finds two dimension defects and coefficient ratio 2.0 (1) | Calculation maintainer | issue pending (1). [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L12-L26] [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L96-L146] |
| Input validation and silent fallbacks | Finite/range inputs, invalid calm settings, short-window status, zero/dry branch coverage, ignored parameters and damping floors | Calculation maintainer | issue pending (2). [src/digitalmodel/hydrodynamics/planing_hull/geometry.py:L42-L62] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L331-L344] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L413-L479] |
| Unverified coordinate/phase convention and spectral accuracy | Establish global/pitch/phase contract, synthetic-signal reference, alias limits and time/window convergence | Calculation maintainer | issue pending (3). [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L218-L248] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L437-L488] |
| Unverified primary-publication attributions, empirical coefficients and nonzero benchmark | Confirm the three literature attributions in section 3; supply independently sourced nonzero RAO targets and applicability bounds; shared lesson entries remain pending | Calculation maintainer | issue pending (4). [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L6-L26] [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L186-L191] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L121-L198] [tests/unit/hydrodynamics/test_planing_hull.py:L304-L330] |
| Map-family coverage and supplementary discoveries | Add absent in-package families and additional wave methods; add the rectangular/cylindrical sloshing dispersion definitions under existing #2620 | Map maintainer | issue pending (5); #2620. [src/digitalmodel/hydrodynamics/planing_hull/strip_model.py:L78-L211] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L204-L248] [src/digitalmodel/hydrodynamics/planing_hull/solver.py:L423-L479] |

Draft numbers identify issue drafts awaiting repository issue numbers; independent review checks their replacement before clearance. The pack remains DRAFT, with no physical-model clearance claimed (AQG r0, §11 “Sign-off and independence”).
