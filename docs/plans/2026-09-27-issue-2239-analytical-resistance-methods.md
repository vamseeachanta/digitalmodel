# Plan for #2239: mesh-derived hull resistance and AI-assisted analytical methods

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/2239
**Status:** plan-review draft **r2**, 2026-09-27. **Not approved.** No implementation will start before explicit
approval on the issue, and no `status:plan-approved` label will be self-applied.
**Tier:** T3. Planning mode: parallel-readonly. Code inspected at `origin/main` `02cedacc`.
**Revision note:** r2 folds in the twelve findings of the Codex r2 review. The folded r3 patches are listed at the end.

## Resource Intelligence Summary

### Existing repo code (verified at 02cedacc)

| Path | What it provides | Demonstrated limitation relevant here |
|---|---|---|
| `src/digitalmodel/naval_architecture/holtrop_mennen.py` | Public component functions (form factor, friction, wave, appendage, bulb, transom, correlation allowance) and a scalar `total_resistance` | No structured result carrying the components with their evaluation conditions. `RHO_SW=1025.0` and `NU_SW=1.1892e-6` are fixed constants (`holtrop_coefficients.py:13-14`), so the fluid cannot be chosen. A model-scale Reynolds number is still reachable by using model dimensions and speed. `total_resistance` always adds C_A (`holtrop_mennen.py:170-194`). |
| `src/digitalmodel/naval_architecture/holtrop_coefficients.py` | c1–c15, m1, λ, length of run | Defects reported in #2020 (open). The fixture YAML is not loaded by the test file; the reported 31–51 % discrepancy is issue-state evidence, not reproduced here. |
| `src/digitalmodel/naval_architecture/resistance.py` | Dimensionless ITTC-57 C_F, a citation-aware wrapper, imperial Reynolds and Froude helpers, SHP | Dimensional helpers are imperial. There is no ITTC-78 transfer and no form-factor determination. |
| `src/digitalmodel/visualization/design_tools/hull_hydrostatics.py` | Hydrostatics integrated over sectional profiles (lines 13-25, 59-64) | Does not read a surface mesh. |
| `src/digitalmodel/hydrodynamics/diffraction/geometry_quality.py` | Panel-mesh quality checks | Reuse for watertightness and normals is **unverified**; it will be assessed at the start of W0. |

Absence searches (`git grep -i -E` over `src/*.py tests/*.py tests/*.yaml data/*`) returned no Michell/thin-ship, ITTC-78
or Prohaska code, and no Wigley/KCS/KVLCC2/DTMB-5415 resistance data. The one "michell" hit is unrelated. These searches
cover those paths only and are not a repository-wide proof of absence.

### Related issues and ownership (rechecked 2026-09-27)
- **#2020 (open), Holtrop defects.** W2 is gated on it. "Fixed" means a merged commit that closes #2020, with the fixture
  vectors loaded by a test and passing within the tolerance #2020 sets. This plan will not fix #2020.
- **#1173 (open), RANS calm-water validation.** It owns the RANS rung and its benchmark runs; this plan consumes its published results only.
- **#2023 (open), hull-agnostic CFD pipeline.** W0 owns `mesh_hydrostatics.py` and its output schema; #2023 consumes that
  schema and does not fork the adapter. The interface is the W0 output contract below.

### Private data
W6 alone touches restricted data, and only through its private data owner. The reference, pin and any findings stay in the
private repository. Nothing derived from private data appears in this public plan.
Public requirements that apply everywhere:
- match Reynolds and Froude regimes before comparing;
- compare like-for-like component decompositions;
- preserve input-revision provenance.

## Method-selection matrix

| Method | Mathematical status | Input requirement | Physical scope | Applicability rule (enforced) | Evidence expected |
|---|---|---|---|---|---|
| Flat plate, laminar (Blasius) | Exact similarity solution under boundary-layer assumptions | none | Laminar skin friction | Re_x < 5×10⁵ | Verification anchor only |
| ITTC-57 line; ITTC-78 transfer | Correlation line; standard procedure (ITTC 7.5-02-03-01.4) | S, L_wl, fluid | Friction; model→ship transfer | Froude-matched only; k declared | Identity plus independent forward cases |
| Prohaska | Least-squares fit over low-Fn data | C_T(Fn) data | Form factor | Fn window 0.10–0.20, ≥ 6 points, conditioning checks | Fit residual and CI |
| Holtrop–Mennen 1984 | Empirical regression | Hull coefficients + declared tf, hb, cstern | Bare hull + allowances | Published parameter ranges; warns outside | Published example (after #2020) |
| Michell thin ship | Exact within linear thin-ship theory; **evaluated by numerical quadrature, never "closed form"** | Half-breadth field y(x,z) | Inviscid wave resistance | B/L ≤ 0.1 and C_B ≤ 0.6 → applicable; otherwise it returns `outside_validated_applicability` | Pinned linear-theory reference |
| Neumann–Kelvin / Dawson panel | Numerical integral-equation solution | Surface panels | Wave resistance | Not in scope for the first milestone | – |
| RANS (interFoam) | Numerical PDE solution | Volume mesh | Total resistance | Owned by #1173 | #1173 results |
| Symbolic regression / learned surrogate | Surrogate, never an exact solution | Hull coefficients | Residuary coefficient | Declared training domain; refuses outside it | Locked held-out families |

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-09-27-issue-2239-analytical-resistance-methods.md` |
| Mesh adapter + contract | `src/digitalmodel/naval_architecture/mesh_hydrostatics.py`; `tests/naval_architecture/test_mesh_hydrostatics.py` |
| Friction and transfer kernel | `src/digitalmodel/naval_architecture/friction_scaling.py`; `tests/naval_architecture/test_friction_scaling.py` |
| Holtrop structured result (after #2020) | `holtrop_mennen.py` (extend); existing test file |
| Michell | `src/digitalmodel/naval_architecture/michell.py`; `tests/naval_architecture/test_michell.py` |
| Validation harness | `src/digitalmodel/naval_architecture/resistance_validation.py`; `tests/naval_architecture/benchmarks/` (rights-cleared data only) |
| Benchmark source manifest | `tests/naval_architecture/benchmarks/SOURCES.yaml` |
| Analytical-discovery record | `docs/research/2239-analytical-discovery.md` |
| Plan reviews | `docs/plans/evidence/2239/` |

## Work packages (future tense; each separately authorized)

### W0: geometry contract and mesh adapter
- **Conventions (a contract to reject violations, not to repair them silently).** Right-handed axes, x positive forward,
  y positive to port, z positive up. The draft datum is the baseline (keel) at z = 0. Trim is T_fwd − T_aft, negative by
  the stern, applied about the midship waterline point, so the mean draft is preserved. LCB is positive forward of midship,
  reported as a percentage of L_wl. Units must be declared (m or mm); an undeclared mesh is refused.
- **Mesh checks.** Watertight and manifold; consistent outward normals (a reversed mesh is refused unless flipped explicitly);
  no degenerate or non-finite faces.
- **Clipping.** The adapter will cut the mesh at the waterline and cap it. **Cap faces are tagged artificial.** They count
  toward volume but never toward wetted area.
- **Features.** A_BT and A_T need identified sections. The caller supplies bulb and transom section stations, or the adapter
  refuses to compute them. tf, hb and cstern are **declared caller inputs**, never inferred.
- **Output schema (consumed by W2, W3 and #2023).** V, S (physical faces only), L_wl, B_wl, C_B, C_P, C_M, C_WP, LCB, A_BT,
  A_T and a half-breadth grid y(x_i, z_j) on declared stations. Each value is tagged `computed` or `declared`, and each carries the
  input hash.
- **Verification is separated from accuracy.** Integration verification means the same polyhedron integrated two ways
  agrees to 1e-9 relative. Geometric accuracy means *independently generated* tessellations of the analytic Wigley hull converge to the
  analytic references. Refining a single mesh is not accepted as accuracy evidence.
- **Wigley reference.** y(x,z) = (B/2)(1 − (2x/L)²)(1 − (z/T)²) on −L/2 ≤ x ≤ L/2, −T ≤ z ≤ 0 (with z measured from the
  waterline for this definition), L/B = 10, L/T = 16. Exact V = 4BLT/9 and C_B = 4/9. S has no closed form; its
  reference is an adaptive quadrature of the analytic surface to 1e-9 relative. No monotone-convergence requirement.
- **Tolerances per property**, relative with an absolute floor. V and S: 2e-3 relative. L_wl and B_wl: 1e-3 L. LCB: 1e-3 L
  absolute, since it is zero for symmetric hulls. A_BT and A_T: 1e-3 B·T absolute.

### W1: friction and model-to-ship transfer (SI; fluid as a parameter)
- ITTC-57 C_F(Re), with Citation sidecars.
- **Transfer per ITTC 7.5-02-03-01.4** (1978 ITTC performance prediction method). The simplified resistance transfer
  conserves the residual C_R = C_T,m − (1+k)·C_F,m (− C_A,m if the model carries one). It reconstructs
  C_T,s = (1+k)·C_F,s + C_R + C_A,s + ΔC_F, where each allowance term is declared or zero. Froude similarity, geometric similarity,
  a form factor invariant with Reynolds number, and the omitted corrections (air resistance, appendages, roughness where
  not declared) are stated in the result's metadata.
- **Prohaska** fits C_T/C_F = (1+k) + c·Fn⁴/C_F by least squares. Admissible data: Fn 0.10–0.20, at least 6 points. The fit
  reports its residual, a confidence interval on k and a conditioning number. It is refused below the minimum point count or at
  a condition number above 1e6.
- The imperial helpers in `resistance.py` stay unchanged for their callers.

### W2: Holtrop structured result (gated on #2020)
A `HoltropResult` will carry each component with its fluid, Reynolds and Froude numbers, and flags for applicability and C_A
inclusion. The fluid becomes a parameter; the existing functions keep their signatures.

### W3: Michell thin-ship wave resistance
- **Before W3 is authorized**, one legally usable linear-theory reference (curve or tabulated values for the Wigley hull) will be
  pinned in `SOURCES.yaml`. The pin records the Michell form used, its normalization (C_W on S or on L²), the hull
  parameters, and the digitization uncertainty. Until then W3 is not started.
- **Numerics.** The substitution λ = cosh u will remove the endpoint singularity. The inner integrals will be evaluated on the
  W0 half-breadth grid. Two error controls are separate:
  - **Quadrature:** refinement to < 0.1 %.
  - **Tail truncation:** bounded analytically by the exp(−λ² g T/U²) decay, truncated where that bound is < 1e-8 of the running total.
- **Acceptance** against the pinned reference is pointwise over Fn 0.20–0.40. A result passes within max(5 % relative,
  2 × reference uncertainty), with an absolute floor of 5e-5 on C_W near its hollows.
- **Scope.** Numerical verification against linear theory is kept separate from experimental validation: Michell results are
  never scored against towing-tank C_T directly.

### W4: validation harness and public benchmarks
- **Source manifest.** Every source in `SOURCES.yaml` will carry rights evidence (licence or permission, redistribution
  yes/no), version, URL, SHA-256 of the raw file, attribution, units, conditions and extraction method.
  - **No redistribution right:** the data is not committed. Only a reference is kept, and the dependent tests are skipped with the reason.
  - **Redistribution right:** raw evidence is kept beside the derived values.
- **Comparison contract.** Each comparison declares:
  - its observable, for example EFD C_T at Fn, or a C_R inferred with a stated k;
  - its normalization (S, ρ, ν);
  - its Reynolds and Froude numbers;
  - its decomposition assumptions.

  Missing metadata is a refusal. So are mismatched regimes and invalid pairings, such as Michell C_W against EFD C_T, or
  RANS pressure resistance against wave resistance.
- **Benchmarks.**
  - Wigley: analytic geometry. Numerical verification only, until rights-cleared EFD data is pinned.
  - KCS.
  - KVLCC2: full-bodied, C_B about 0.81. It is used **only** to demonstrate that Michell returns `outside_validated_applicability`. Any
    numerical agreement there is recorded as exploratory and never as validation.
- **Leakage controls.**
  - **Identity:** each hull, family and source carries an immutable ID, a geometry hash and a normalized-content hash, so renamed duplicates are caught by hash.
  - **Roles:** fixed roles of train, calibration, validation and locked test.
  - **Provenance:** fitted parameters and target derivations carry their provenance.
  - **Rejections:** the harness rejects overlapping lineage across roles, calibration on validation or test data, preprocessing fitted on held-out
    data, missing conditions, and inputs outside the declared domain.
  - **Locked test set:** it is evaluated once per released model version.

### W5: bounded AI analytical-discovery track (three-day cut-off)
- **Asymptotic Michell.** Candidates are low-Fn expansions for the Wigley family. The oracle is W3 quadrature, with error
  below 0.1 %, on the grid Fn 0.10–0.20 in steps of 0.01. A candidate passes if its relative error is below 5 % for Fn ≤ 0.15 **and** the
  log-log slope of error against Fn is within 20 % of the claimed order.
- **Residuary surrogate.** Symbolic regression runs on rights-cleared public series data, split by family with a locked held-out
  family. The baseline is Holtrop C_R on the same inputs, after #2020. A candidate passes only if its held-out RMSE beats the baseline by
  at least 20 % and its maximum error stays below 2× the baseline maximum. Outside its training domain it refuses.
- **Record.** Each candidate's equation, provenance, tests and rejection reason is logged in
  `docs/research/2239-analytical-discovery.md`. At the cut-off, a negative result is reported unless a candidate met the
  predeclared criteria.

### W6: private application (outside this repository)
Once W0–W4 pass, W6 runs through the restricted data owner. Cached commercial-tool records serve as comparison evidence,
are never treated as truth, and are never altered. Each new result is a separate evidence record in the private owner.

### W7: engineering-opportunity inventory
W7 is a ranked inventory, built from `git grep` over the repositories actually checked out and citing code paths. Each selection gets
its own issue. Nothing is implemented under W7.

## TDD Test List (W0–W1; tolerances are relative unless marked absolute)

| Test | Verifies | Input | Expected |
|---|---|---|---|
| test_box_hydrostatics_exact | Adapter vs exact solid | L×B×T box | V = LBT; S = LB + 2LT + 2BT (cap excluded), 1e-9 |
| test_cap_faces_excluded_from_wetted_area | Artificial faces | clipped box | S unchanged when cap area changes |
| test_integration_two_ways_agree | Integration verification | same polyhedron, divergence theorem vs tetra sum | 1e-9 |
| test_wigley_independent_tessellations | Geometric accuracy | ≥ 3 independently generated Wigley meshes | V → 4BLT/9 and S → quadrature reference within 2e-3 at the finest |
| test_trim_sign_moves_lcb | Signed trim | box, trim −1 m vs +1 m | LCB aft for stern trim, forward for bow trim; mean draft preserved |
| test_units_mm_equals_m | Unit contract | same hull in mm and m | identical outputs; undeclared → refused |
| test_axis_transform_invariance | Axis contract | hull rotated to another declared frame | identical outputs |
| test_reversed_normals_refused | Mesh check | inverted mesh | refusal with reason |
| test_open_or_nonmanifold_refused | Mesh check | holed / non-manifold mesh | refusal |
| test_degenerate_and_nonfinite_refused | Mesh check | zero-area / NaN faces | refusal |
| test_dry_hull_refused | Clipping | draft below keel | refusal |
| test_bulb_transom_need_declared_stations | Features | hull without stations | A_BT, A_T refused, others computed |
| test_ittc57_reference_values | Friction | Re = 1e6; 1e9 | 0.0046875; 0.075/49 = 0.00153061 (1e-12 abs) |
| test_invalid_reynolds_refused | Inputs | Re ≤ 0, ν ≤ 0 | refusal |
| test_transfer_forward_case_k | Independent transfer | C_T,m = 4.0e-3, Re_m = 1e7, Re_s = 1e9, k = 0.2, no allowances | C_T,s = 1.2·0.00153061 + 0.0004 = 2.236735e-3 |
| test_transfer_forward_case_k0 | Same, k = 0 | as above | C_T,s = 0.00153061 + 0.001 = 2.530612e-3 |
| test_transfer_allowances_declared | Allowances | C_A,s = 4e-4 | adds exactly 4e-4; undeclared → 0 with flag |
| test_transfer_roundtrip_identity | Supplementary identity | forward then inverse | 1e-12 abs |
| test_transfer_refuses_froude_mismatch | Regime | Fn_m ≠ Fn_s | refusal |
| test_prohaska_recovers_k_with_noise | Fit | synthetic k = 0.15, 1 % noise, 8 points | k within its reported 95 % CI |
| test_prohaska_refuses_few_points | Conditioning | 4 points | refusal |

W3–W5 tests (Michell reference, tail control, applicability refusal on KVLCC2, the harness negative tests for circular
calibration, renamed duplicates, missing Reynolds/Froude metadata, normalization mismatch and extrapolation) will be
written in their packages, before their implementation.

## Acceptance criteria (first implementation milestone, W0–W1)
- Every W0–W1 test above passes at its stated tolerance, and the geometric-accuracy test uses independent tessellations.
- The output schema is documented and consumed unchanged by one #2023 call site, or #2023 records its acceptance.
- The kernel is SI, takes the fluid as a parameter, and carries a Citation sidecar on every standards-derived constant.
  Existing `resistance.py` callers and tests stay green.
- A private-content review of the diff finds nothing. Beyond an identifier and dimension scan, a second reviewer confirms that no
  finding, direction of effect or conclusion drawn from restricted data is stated.

## Budget, reduced-scope outcomes and stop/go
- W0: 4–6 days. This will be re-estimated after the mesh library is selected (candidate: `trimesh`, licence to be checked), since
  general clipping and feature handling dominate. W1: 2 days. W3: 2–3 days after the reference is pinned. W4: 3–4 days
  plus the rights checks. W5: three days, hard cut-off.
- **Stop:** if W0 cannot meet the Wigley accuracy tolerance with independent tessellations, no downstream package starts.
- **Reduced scope:** without rights-cleared EFD data, W4 delivers numerical verification only. W6 then cannot claim
  validation, and every private result is labelled `verified_numerics_only`.

## Adversarial Review Summary

| Provider | Verdict | Key findings |
|---|---|---|
| Claude (r1, inline) | MINOR | Michell validity, the #2020 gate, the unit conflict and the regime trap; folded into r1 |
| Codex (r2, read-only, inline bundle) | MAJOR (12 findings) | Folded into this r2; see below |
| Gemini/agy | UNAVAILABLE on this host (workspace-hub #3896) | T3 degrades to T2; recorded |

Codex r2 findings and where r2 addresses them:
1. Convergence of a refined polyhedron is not accuracy → W0 separates verification from accuracy, uses independent Wigley tessellations, sets per-property floors, and pins the Wigley equation and references.
2. W0 geometry definitions → the conventions, artificial cap faces, declared feature stations and declared tf/hb/cstern, plus the output schema, are now in W0.
3. Extrapolation procedure → W1 cites ITTC 7.5-02-03-01.4, gives the conserved residual, and lists the stated assumptions and the Prohaska window, point count and conditioning.
4. Weak tests → independent forward cases, explicit ITTC-57 reference values, signed trim, and the unit, axis, normal, degenerate, dry and feature tests.
5. W3 underspecified → a pinned reference with its form and normalization, separate quadrature and tail control, a pointwise acceptance with absolute floor, and verification kept apart from validation.
6. Applicability → the enforced applicability rules; KVLCC2 refusal only; invalid pairings refused.
7. Leakage → immutable IDs and hashes, fixed roles, provenance tracking, a rejection list, and a locked test set.
8. Private-derived content → removed from the plan. The #2239 comment was edited the same day to state the requirements generically.
9. Overstated gaps → the code claims are narrowed to demonstrated limitations; `geometry_quality.py` reuse is marked unverified.
10. Gates and ownership → the #2020 closure evidence, adapter ownership against #2023/#1173, re-estimated budgets and reduced-scope outcomes.
11. Licence record → the `SOURCES.yaml` fields; commits are blocked without redistribution rights.
12. W5 falsification → the Fn grid, oracle accuracy, error and order criteria, surrogate baseline, thresholds and cut-off rule.
