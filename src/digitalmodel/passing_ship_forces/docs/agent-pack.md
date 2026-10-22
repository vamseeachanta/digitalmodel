# Passing-ship forces module knowledge pack

| Header | Value |
|---|---|
| Package | `src/digitalmodel/passing_ship_forces` |
| Code pin | `3554b14fde67f44b1c52b81b6c72106282eaff29` (`origin/main` at inspection) |
| Revision / status | 0 / DRAFT; workflow arithmetic, hydrodynamic qualification incomplete |
| QA family | EC; routed engineering calculation |

The workflow validates configuration, calls a legacy slender-body calculator and
writes force and peak-summary CSVs. The worked integrals establish arithmetic
anchors; the original paper and physical operating envelope remain NOT_EVALUATED.

### Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 0 | 2026-10-22 | Record routed contracts and independent integral examples without duplicating the module report. |

## 1. Purpose and scope

This is the routed legacy force workflow, separate from the spec-era
`hydrodynamics/passing_ship` package. The physical calculation is delegated.
[src/digitalmodel/passing_ship_forces/workflow.py:L1-L33]
PR #3248 covers a module report; this pack covers public contracts, arithmetic,
failure paths and executable checks. No report or production code is changed.
The code pin applies to every source locator. New tests implement independent
synthetic arithmetic rather than copying stored force results.

## 2. Inputs and outputs

Inventory keys are `file_stem.symbol`. Only `router` is locally defined and public;
the facade explicitly re-exports it. Imported Path/Any/calculator conveniences
are not declared workflow API. The calculator dependencies are identified below.

| Public function / class | Input, units, validation and defaults | Output contract | Evidence |
|---|---|---|---|
| `__init__.router` | Explicit re-export of workflow.router; identical cfg contract below | Same mutated cfg dictionary | [src/digitalmodel/passing_ship_forces/__init__.py:L1-L5] [src/digitalmodel/passing_ship_forces/workflow.py:L54-L116] |
| `workflow.router` | cfg mapping containing passing_ship_forces; required vessel/density/separation/velocity/stagger fields; optional depth, image count, quadrature and output location below | Mutates/replaces cfg['passing_ship_forces'], writes two CSVs, returns same cfg; exceptions propagate | [src/digitalmodel/passing_ship_forces/workflow.py:L54-L116] |

| Configuration input | Units, validation, default | Evidence |
|---|---|---|
| moored_vessel / passing_vessel | Required mappings with length > zero and midship_area > zero; common length and area units; calculator also rejects NaN via positivity test; positive infinity is not refused | [src/digitalmodel/passing_ship_forces/workflow.py:L146-L152] [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L120-L135] |
| water_density / separation_distance | Required > zero; mass/length³ and length; calculator rejects NaN but accepts positive infinity | [src/digitalmodel/passing_ship_forces/workflow.py:L183-L191] [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L234-L240] |
| passing_velocity | Required finite signed length/time; sign denotes direction in input description, but calculation squares velocity | [src/digitalmodel/passing_ship_forces/workflow.py:L204-L212] [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L250-L281] |
| water_depth | Optional None → deep water; otherwise > zero in common length units; finiteness not checked | [src/digitalmodel/passing_ship_forces/workflow.py:L194-L201] |
| n_images / quadrature_nodes | int coercion; images ≥ one even in deep water; nodes passed through to NumPy. Software defaults DEFAULT_N_IMAGES and DEFAULT_QUAD_NODES at locator; truncation/convergence NOT_EVALUATED | [src/digitalmodel/passing_ship_forces/workflow.py:L62-L77] [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L108-L115] |
| stagger.values | Nonempty list, float conversion; mutually exclusive with range; common length units; finite/ordering checks absent | [src/digitalmodel/passing_ship_forces/workflow.py:L155-L171] |
| stagger.range | Required finite start/stop, int-coerced num ≥ two; normalized defaults False, when truthy multiplies by moored length; inclusive linspace; decreasing/constant ranges allowed | [src/digitalmodel/passing_ship_forces/workflow.py:L172-L180] |
| output_dir / configuration paths | output_dir defaults 'results'; relative to _config_dir_path, else _config_file_path parent, else current directory; output prefix from config filename stem, else basename, else package name | [src/digitalmodel/passing_ship_forces/workflow.py:L215-L233] |

| Output | Units and behaviour | Evidence |
|---|---|---|
| forces | List of dictionaries with stagger, surge_force, sway_force, yaw_moment; length, force, force, moment; SI → N and N m, consistent customary inputs → corresponding force/moment; no unit conversion or output unit metadata | [src/digitalmodel/passing_ship_forces/workflow.py:L80-L90] [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L56-L61] |
| Peak summary | Signed maximum-absolute force/moment with its stagger; first tied absolute maximum wins. max_sway_attraction = signed maximum, max_sway_repulsion = signed minimum, even if both have the same sign | [src/digitalmodel/passing_ship_forces/workflow.py:L119-L143] |
| method / depth metadata | Method string from implementation; depth_mode deep or finite_image_method; n_images None in deep mode, configured count otherwise; n_stagger count | [src/digitalmodel/passing_ship_forces/workflow.py:L130-L143] |
| results_csv / summary_csv | Strings, repository-relative when inside resolved source root, absolute otherwise; CSV names have input stem and package suffix; directories created, existing files overwritten | [src/digitalmodel/passing_ship_forces/workflow.py:L93-L115] [src/digitalmodel/passing_ship_forces/workflow.py:L236-L251] |
| Replaced input mapping | Vessel geometry, density, velocity, separation, depth retained with results; stagger specification, quadrature settings and output_dir lost | [src/digitalmodel/passing_ship_forces/workflow.py:L98-L116] |

## 3. Method and governing standards

| Method / constant | Equation or algorithm | Source and qualification |
|---|---|---|
| Parabolic hull | S(x)=A(1−4x²/L²), derivative −8Ax/L²; finite hull interval [−L/2,L/2] | Repository theory [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L20-L46]; public original paper NOT_EVALUATED, no opened full text |
| Deep-water forces | X=rho U²/(2pi) integral(S1' F); Y=rho U² eta/pi integral(S1' G); yaw replaces S1' by S1' x+S1; analytic inner kernels F,G | Same theory locator; the geometric integers/pi follow the displayed integral, not empirical coefficients; physical formulation UNVERIFIED |
| Finite-depth correction | eta_n=sqrt(eta²+4n²h²); sum surge over images; lateral/yaw terms weighted eta/eta_n | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L284-L314]; physical boundary-condition and image truncation qualification NOT_EVALUATED |
| Outer quadrature | Fixed-order Gauss–Legendre, mapped to hull interval; cached nodes | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L204-L231]; software choice, no rule-based minimum nodes established |
| Numerical defaults | DEFAULT_N_IMAGES, DEFAULT_QUAD_NODES; inherited image and quadrature resolution | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L108-L115]; NOT_EVALUATED for general convergence, values not restated |
| Stagger and peaks | Inclusive equally spaced range; optional moored-length scale; argmax(abs(component)) | [src/digitalmodel/passing_ship_forces/workflow.py:L119-L143] [src/digitalmodel/passing_ship_forces/workflow.py:L155-L180]; exact arithmetic/selection definitions, no acceptance limit |

Bibliographic lead: *Dynamic Effects of Ship Passage on Moored Vessels*, 1975,
Journal of the Waterways, Harbors and Coastal Engineering Division, volume 101,
WW3, page 247; the original paper and subsequent method-of-images publication
are named in code but were not available as an opened source in this review.
[src/digitalmodel/hydrodynamics/wang_passing_ship.py:L8-L15]
No value or applicability limit is taken from those unread publications. The
independent integral below verifies implementation arithmetic conditional on the
repository's displayed theory. It is not an independent hydrodynamic benchmark.

## 4. Validated worked examples and regression examples

All geometry and fluid inputs below are synthetic. Equal hulls: L1=L2=2 m,
A1=A2=1 m²; eta=1 m, rho=1 kg/m³, deep water and zero stagger. The theory in
section 3 gives S'=-2x and the sway integral

`I = 4 integral[-1,1] integral[-1,1] x y / (1+(y−x)²)^(3/2) dy dx`.

With u=y−x, the inner overlap integral is `2/3−u+u³/6` for 0≤u≤2;
symmetry gives `I=8 integral[0,2] (2/3−u+u³/6)/(1+u²)^(3/2) du`.
An antiderivative of the integrand is
`(2u/3+7/6)/sqrt(1+u²)+sqrt(1+u²)/6`.
The endpoint difference gives `I=(16sqrt(5)−32)/3`. Thus
`Y=rho U² eta I/pi`. Surge/yaw are zero by odd symmetry.

| Example | Independent expected answer | Test node ID |
|---|---|---|
| Abeam at U=1 m/s | Y=(16sqrt(5)−32)/(3pi) N, displayed 0.4 N; surge and yaw zero; peak stagger zero. Test asserts the unrounded literal evaluation of the radical | `tests/passing_ship_forces/test_agent_pack_examples.py::test_abeam_closed_integral` |
| Abeam at U=2 m/s | Y=4(16sqrt(5)−32)/(3pi) N, displayed 1.6 N; test uses independently evaluated literal | `tests/passing_ship_forces/test_agent_pack_examples.py::test_doubled_speed_closed_integral` |
| Stationary | U=0 → rho U²=0 → all three force/moment components zero at staggers −1,0,1 m; three CSV rows preserve these values | `tests/passing_ship_forces/test_agent_pack_examples.py::test_stationary_zero_forces_and_csv` |
| Normalised range | start −1, stop 1, three samples, L1=2 m: [−2,0,2] m exactly; U=0 isolates configuration arithmetic | `tests/passing_ship_forces/test_agent_pack_examples.py::test_normalized_range_literal_staggers` |

## 5. QA gates and executable checks

| Gate | Executable check | Scope |
|---|---|---|
| Required geometry | `tests/hydrodynamics/test_passing_ship_forces_workflow.py::test_router_requires_vessels` | Missing mapping refusal |
| Ambiguous stagger | `tests/hydrodynamics/test_passing_ship_forces_workflow.py::test_router_rejects_stagger_with_both_forms` | Exactly one form |
| Integral anchor | `tests/passing_ship_forces/test_agent_pack_examples.py::test_abeam_closed_integral` | Independent nonzero closed integral |
| CSV contract | `tests/passing_ship_forces/test_agent_pack_examples.py::test_stationary_zero_forces_and_csv` | Literal zero values and row count |
| Static pack contract | `tests/docs/test_marine_agent_packs.py::test_new_pack_contracts` | Public AST names and locator bounds |
| Citation collection | `tests/docs/test_marine_agent_packs.py::test_all_new_pack_node_ids_collect` | Every cited ID, including gates |

Run both `tests/passing_ship_forces` and
`tests/hydrodynamics/test_passing_ship_forces_workflow.py` plus
`tests/hydrodynamics/test_wang_passing_ship.py` through pytest with NumPy/SciPy;
these are offline calculations and require no licensed solver.

## 6. Known failure modes and pitfalls

| Evidence | Defect / limit | Follow-up |
|---|---|---|
| [src/digitalmodel/passing_ship_forces/workflow.py:L183-L201] [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L234-L240] | Positive-infinite density/separation/depth pass positivity checks; NaN depth eventually rejected by image helper | Reject nonfinite physical scalars explicitly |
| [src/digitalmodel/passing_ship_forces/workflow.py:L62-L65] | int coercion truncates fractional counts; no local quadrature minimum/convergence check | Require integral counts and qualified resolution |
| [src/digitalmodel/passing_ship_forces/workflow.py:L168-L180] | Explicit stagger lacks finite checks; normalized uses truthiness, so a nonempty string also scales | Validate finite samples and boolean flag |
| [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L258-L281] | Signed velocity is squared; input direction does not reverse outputs | Clarify sign/frame convention before direction-sensitive use |
| [src/digitalmodel/passing_ship_forces/workflow.py:L123-L142] | Peaks are sampled, tie chooses first; attraction/repulsion labels retain sign without clipping | Check sweep coverage and distinguish signed extrema |
| [src/digitalmodel/passing_ship_forces/workflow.py:L98-L115] | Input settings replaced and quadrature/stagger specification lost | Preserve reproducibility inputs externally |
| [src/digitalmodel/passing_ship_forces/workflow.py:L236-L251] | Output overwrite and absolute path fallback; unit metadata absent | Retain provenance separately and sanitise paths in any publication |
| [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L20-L54] | Slender parabolic hull/image model has no enforced clearance, depth or physical applicability envelope | Qualify geometry, separation and bottom correction against public benchmarks |

## 7. Lessons log

A closed integral adds evidence beyond comparison to the same library routine.
Exact zero-speed and range cases check workflow behaviour separately. Those
checks cannot establish the empirical applicability of the underlying theory.

## 8. Ownership

Package maintainers maintain the contract and test citations. A hydrodynamics
reviewer must qualify the original formulation, frames, clearance and convergence
before using the results in a mooring assessment. No approval is implied.

## 9. Dependencies and other implementations

The workflow imports NumPy and the legacy calculator; pathlib/csv provide output.
[src/digitalmodel/passing_ship_forces/workflow.py:L38-L49]
The calculator's public WangVessel and wang_passing_forces are dependencies, not
new definitions owned by this package. Their source contract and numerical
scheme are recorded here for traceability.
[src/digitalmodel/hydrodynamics/wang_passing_ship.py:L120-L145]
[src/digitalmodel/hydrodynamics/wang_passing_ship.py:L399-L421]
Stored legacy regression evidence is exercised by the existing calculator tests;
this pack adds independent synthetic arithmetic without republishing that data.

## 10. Coverage and open items

The two public inventory entries cover the definition and explicit facade alias.
Four worked examples cover a nonzero analytic force, velocity scaling, exact
stationary results/CSV and normalized input generation. Existing tests cover
finite-depth routing and legacy numerical regressions. NOT_EVALUATED: original
paper, physical envelope and general quadrature/image convergence. Open defects
in section 6 require separate implementation changes; this pack preserves code.
