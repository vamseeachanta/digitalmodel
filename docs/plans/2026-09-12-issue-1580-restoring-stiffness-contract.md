# Plan for #1580: Define a solver-neutral diffraction restoring-stiffness contract

> **Status:** draft
> **Complexity:** T3
> **Date:** 2026-09-12
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1580
> **Client:** N/A
> **Lane:** lane:codex
> **Review artifacts:** scripts/review/results/2026-09-12-plan-1580-claude.md | ...-codex.md | ...-gemini.md

---

## Resource Intelligence Summary

### Existing repo code

- Found: `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:263-266` — the canonical 6x6 external-stiffness field is `VesselSpec.external_stiffness`, reached from `DiffractionSpec` via `spec.vessel` / `spec.get_bodies()[i].vessel`. It is declared as `Optional[list[list[float]]]` with the description `"6x6 external stiffness matrix"` and **no** unit, reference-point, DOF-order, sign-convention or composition-mode field beside it, and **no** validator — not on shape, not on symmetry.
- Found: `src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py:734-742` — the AQWA backend passes `vessel.external_stiffness` straight to `_build_matrix_cards(..., keyword="FISK")`. `_build_matrix_cards` at `aqwa_backend.py:752-774` formats each value with `f"{v:.3e}"` and applies no scaling. The same deck writes vessel mass unconverted at `aqwa_backend.py:622-625` (`mass = body.vessel.inertia.mass`, kilograms), so the AQWA deck's force base is newtons.
- Found: `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py:362-375` — the OrcaWave backend assigns `body[stiffness_key] = [list(row) for row in vessel.external_stiffness]`, again with no scaling, and hard-codes `body["BodyExternalStiffnessMatrixOriginType"] = "Body origin"` at `orcawave_backend.py:375`. The same function converts mass at `orcawave_backend.py:345` (`body["BodyMass"] = kg_to_tonnes(inertia.mass)`) and the module docstring at `orcawave_backend.py:13` states `"SI unit system: lengths in metres, mass in tonnes, density in t/m^3"`, so the OrcaWave project's force base is kilonewtons.
- Found: `src/digitalmodel/hydrodynamics/diffraction/deck_validation.py:30-105` — `validate_mesh()` is the existing **license-free** restoring check and is the closest prior art for a verification contract. It runs Capytaine on the prepared hull mesh, reads `K = np.asarray(res.hydrostatic_stiffness)` (`deck_validation.py:72`), and returns `k33`, `k44`, `k55` (`deck_validation.py:73-81`). Its acceptance criterion is bound to a comparator: `expected = rho * g * expected_waterplane_m2` and `checks["k33_within_tol"] = abs(k33/expected - 1.0) <= tol` (`deck_validation.py:90-97`). It returns `{"available": False, ...}` rather than raising when Capytaine is absent (`deck_validation.py:53-54`). K44 is *reported* but is bound to no comparator.
- Found: `src/digitalmodel/hydrodynamics/diffraction/report_computations.py:37-71` — `compute_stability()` already inverts the relation this issue asks to verify: `result["gm_transverse"] = C[3][3] / rho_g_v` at `report_computations.py:65`, with `rho_g_v = rho * g * V` and defaults `rho: float = 1025.0, g: float = 9.81` (`report_computations.py:39-40`). This is the post-run direction only; nothing binds an input-side target to it.
- Found: `src/digitalmodel/hydrodynamics/diffraction/output_schemas.py:334-355` — `HydrostaticResults` carries `stiffness_matrix: np.ndarray  # 6x6 matrix` with no unit, reference-point or composition attribute; the sibling `DiffractionResults.unit_system` at `output_schemas.py:389` is a free string defaulting to `"SI"`.
- Found: `src/digitalmodel/hydrodynamics/diffraction/diffraction_units.py:21-56` — named converters exist for mass (`kg_to_tonnes`), density and inertia. There is **no** force, stiffness or moment converter in the module.
- Found: `src/digitalmodel/hydrodynamics/diffraction/reverse_parsers.py:643-655` — the reverse path reconstructs `external_stiffness = self._parse_6x6_matrix(body_data, "Stiffness")` from an OrcaWave project with no unit annotation, so a round trip through the reverse parser loses whatever unit basis the forward path used.
- Gap: no module normalizes a restoring matrix between canonical and native units; no validator rejects an asymmetric, wrongly shaped or unit-less matrix; no artifact records which matrix was applied.

### Standards

| Standard | Status | Source |
|---|---|---|
| Hydrostatic restoring / metacentric relation | closed-form; no vendor standard required | Classical ship hydrostatics; the relation is derived in §"Governing physics" below and is verifiable in-repo against `report_computations.py:44-47` |
| AQWA deck FISK card semantics | gap — vendor manual, not committed | Vendor-licensed; routes per `.claude/rules/codes-standards-data-routing.md`. Resolution step named in Risks. |
| OrcaWave `BodyExternalStiffnessMatrix` semantics | gap — vendor manual, not committed | Same routing constraint; the in-repo exemplar referenced at `orcawave_backend.py:301-306` is the fallback evidence. |

No standards-derived numeric constant will be introduced, so `.claude/rules/calc-citation-contract.md` will not fire on this work. The single physical constant used, `g = 9.80665 m/s^2`, is the existing schema default at `input_schemas.py:295-299`.

### LLM Wiki pages consulted

No relevant wiki pages. The work is a schema-and-adapter contract inside a public repo and introduces no standards-derived or client-derived knowledge, so `.claude/rules/wiki-sibling-routing.md` does not apply (`Client: N/A`).

### Documents consulted

- Issue #1580 body — states the problem, five acceptance criteria and the boundary that inertia-tensor origin semantics belong to #1579 and output-unit normalization stays coordinated with #1550.
- Issue #1582 "Normalize diffraction matrix units across AQWA and OrcaWave backends" — OPEN, `status:needs-plan`, same lane. Owns added-mass and damping unit normalization. This plan will consume its unit vocabulary and must not restate it for those matrices.
- Issue #1579 "Make diffraction inertia-tensor origin strict and consistent across backends" — OPEN, `status:needs-plan`, same lane. Owns the origin enum that `orcawave_backend.py:353-360` currently spells as `_TENSOR_ORIGIN_MAP = {"body_origin": ..., "centre_of_mass": ...}`. This plan will reuse that spelling rather than define a second one.
- Issue #1550 — CLOSED. Its workstreams are the precedent for the failure mode here. `tests/hydrodynamics/diffraction/test_native_units_invariant.py:1-26` records the W5 finding that `solver.report_extractors` stays in OrcFxAPI-native te/kN while `orcawave_runner._build_results_from_object` converts to SI/kg, and that a half-conversion "is not a mislabel — it is a numerically wrong natural period".
- `tests/hydrodynamics/diffraction/test_matrix_unit_completeness.py:1-30` — the W4 precedent: four producers disagreed on the unit-key dict shape and 18 or 36 cells of every exported 6x6 matrix carried `unit=""`. The test asserts completeness at the producer/consumer seam rather than pinning one dict shape. The restoring contract will follow the same seam-level assertion style.
- `tests/hydrodynamics/diffraction/test_multi_solver_units_guard.py:1-12` — the W1 precedent: `MultiSolverComparator` differenced raw matrix entries across solvers with nothing checking they shared a unit basis, "silently producing `mean_error` / `max_error` / `rms_error` wrong by 1000x".
- Epic #1825 "[Epic] Diffraction results agree across solvers — one strict contract, three-way benchmarks" — OPEN, parent of #1580. Cross-solver agreement is the epic's acceptance surface, so a restoring-stiffness discrepancy of 1000x inside it would invalidate every benchmark that applies a non-zero external stiffness.
- `.claude/rules/reproducibility-is-not-correctness.md` — requires every artifact encoding physics to carry at least one assertion against a comparator the producing system did not make, and names the comparator classes. The C44 target test will be class `closed-form`; the cross-backend equivalence test will be class `conservation` (a round-trip identity through declared units).
- No relevant drive files. The drive-index search surface is not reachable from this Windows host, and the work introduces no measured or client-supplied dataset, so `docs/architecture/agent-data-handling-contract.md` fixture rules reduce to "generic synthetic only".

### Gaps identified

- No canonical declaration of units, DOF order, reference point, sign convention or composition mode for a restoring matrix anywhere in the diffraction package.
- No fail-closed validation: an asymmetric 6x6, a unit-less 6x6 and an unstated reference point are all accepted today (proved below).
- No force/moment unit converter in `diffraction_units.py`, so neither backend can convert even if it wanted to.
- No input-side C44 or GM target, and no binding of `deck_validation.py`'s reported `k44` to any comparator.
- No pre-execution artifact that states which restoring matrix a generated deck will apply, in which units, about which point.
- No provenance field on `HydrostaticResults` recording the applied native and normalized matrices.

### Evidence (embedded verification)

**Issue statuses** (verified 2026-09-12T11:44:00Z via `gh issue view <n> --json number,state,title,labels`):

```
1580	OPEN	Define a solver-neutral diffraction restoring-stiffness contract	labels=cat:engineering,domain:hydro,lane:codex,machine:licensed-win-1,status:needs-plan
1579	OPEN	Make diffraction inertia-tensor origin strict and consistent across backends	labels=cat:engineering,machine:licensed-win-1,domain:hydro,lane:codex,status:needs-plan
1582	OPEN	Normalize diffraction matrix units across AQWA and OrcaWave backends	labels=cat:engineering,machine:licensed-win-1,domain:hydro,lane:codex,status:needs-plan
1550	CLOSED	Added-mass/damping units inconsistency in DiffractionResults: matrix labeled kg/kg·m² but magnitudes consistent with te/te·m² (~1000× off)	labels=cat:data
1825	OPEN	[Epic] Diffraction results agree across solvers — one strict contract, three-way benchmarks	labels=cat:engineering,priority:high,domain:hydro,epic,machine:multi
```

Repository state at planning time: branch `main`, HEAD `6f808a61`.

**File existence** (verified 2026-09-12T11:45:00Z):

- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py`
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py`
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py`
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/deck_validation.py`
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/output_schemas.py`
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/diffraction_units.py`
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/report_computations.py`
- EXISTS: `tests/hydrodynamics/diffraction/test_aqwa_backend_damping.py`
- MISSING (new — this plan will create): `src/digitalmodel/hydrodynamics/diffraction/restoring_contract.py`
- MISSING (new — this plan will create): `tests/hydrodynamics/diffraction/test_restoring_contract_schema.py`
- MISSING (new — this plan will create): `tests/hydrodynamics/diffraction/test_restoring_contract_normalization.py`
- MISSING (new — this plan will create): `tests/hydrodynamics/diffraction/test_restoring_c44_gm_relation.py`
- MISSING (new — this plan will create): `tests/hydrodynamics/diffraction/test_restoring_backend_equivalence.py`
- MISSING (new — this plan will create): `tests/hydrodynamics/diffraction/test_restoring_provenance.py`

**Line excerpts.**

`src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:263-270`:

```
    external_stiffness: Optional[list[list[float]]] = Field(
        None,
        description="6x6 external stiffness matrix",
    )
    external_damping: Optional[list[list[float]]] = Field(
        None,
        description="6x6 external damping matrix",
    )
```

`src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py:734-742`:

```
            if vessel.external_stiffness is not None and self._matrix_has_nonzero(
                vessel.external_stiffness
            ):
                cards.extend(
                    self._build_matrix_cards(
                        vessel.external_stiffness,
                        keyword="FISK",
                    )
                )
```

`src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py:371-375`:

```
    if vessel.external_stiffness is not None:
        body[stiffness_key] = [list(row) for row in vessel.external_stiffness]
    else:
        body[stiffness_key] = _zero_6x6()
    body["BodyExternalStiffnessMatrixOriginType"] = "Body origin"
```

`src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py:345` and `aqwa_backend.py:622-625` — the mass cards that fix each backend's force base:

```
orcawave_backend.py:345:        body["BodyMass"] = kg_to_tonnes(inertia.mass)
aqwa_backend.py:622:            mass = body.vessel.inertia.mass
aqwa_backend.py:624:            mass_str = _fmt_float(mass)
aqwa_backend.py:625:            cards.append(f"{_WS:>5s}{idx:>1d}{_WS:>9s}98000{mass_str}")
```

`src/digitalmodel/hydrodynamics/diffraction/deck_validation.py:90-104` — the existing comparator-bound restoring check:

```
    checks = {"heave_stiffness_positive": k33 > 0}
    if expected_waterplane_m2:
        expected = rho * g * expected_waterplane_m2
        out["k33_expected"] = expected
        out["k33_ratio"] = k33 / expected if expected else None
        checks["k33_within_tol"] = (
            abs(out["k33_ratio"] - 1.0) <= tol if k33 > 0 else False
        )
    if expected_mass_t and vol is not None:
        out["mass_ratio"] = (
            out["mesh_mass_t"] / expected_mass_t if expected_mass_t else None
        )
        checks["floats_at_expected_mass"] = abs(out["mass_ratio"] - 1.0) <= tol
    out["checks"] = checks
    out["passed"] = all(checks.values())
```

`src/digitalmodel/hydrodynamics/diffraction/report_computations.py:64-66` — the post-run GM inversion:

```
    rho_g_v = rho * g * V
    result["gm_transverse"] = C[3][3] / rho_g_v if rho_g_v > 0 else None
    result["gm_longitudinal"] = C[4][4] / rho_g_v if rho_g_v > 0 else None
```

**Gap proofs** (verified 2026-09-12T11:45:00Z):

- `grep -nE "kN|newton|force_|stiffness_" src/digitalmodel/hydrodynamics/diffraction/diffraction_units.py` → no match. The named-converter module covers mass, density, inertia, frequency and angle only; no force or stiffness converter exists.
- `grep -nE "composition_mode|stiffness_units|GM_transverse" src/digitalmodel/hydrodynamics/diffraction/*.py` → no match. The ten hits for `reference_point` and `metacentric` all resolve elsewhere: `input_schemas.py:144-164` is `VesselGeometry.reference_point` (a mesh datum, not a stiffness datum) and `report_builders_hydrostatics.py:71`, `report_data_models.py:142`, `report_computations.py:65` are post-run GM reporting. Nothing on the input side.
- `grep -n "external_stiffness" src/ tests/` → the field is read in exactly three production sites (`aqwa_backend.py:734-739`, `orcawave_backend.py:371-372`, `reverse_parsers.py:644,654`) and written in one schema site (`input_schemas.py:263`). No validator, no converter, no provenance writer.

**Reproduction proofs.**

The issue claims that "reusing one numeric matrix across AQWA and OrcaWave can therefore produce different physical restoring models without a fail-closed check". A read-only probe was run against the working tree: it constructs one `DiffractionSpec` carrying a single 6x6 matrix with `K[2][2] = 2.0e6` and `K[3][3] = 5.0e7`, then emits both backends.

```
$ D:\ws\digitalmodel\.venv\Scripts\python.exe <scratchpad>/repro_1580.py

=== A. asymmetric 6x6 accepted by the schema? ===
VesselSpec.external_stiffness[3][4] = 1000000.0
VesselSpec.external_stiffness[4][3] = 0.0
-> schema accepted an asymmetric matrix with NO error

=== B. schema fields present on VesselSpec ===
['control_surface', 'external_damping', 'external_stiffness', 'fixed_dofs', 'geometry', 'inertia', 'name', 'type']
  'stiffness_units' present: False
  'stiffness_reference_point' present: False
  'stiffness_composition' present: False

=== C. AQWA Deck 7 FISK cards (deck mass base = kg, so force base = N) ===
'      FISK         3 0.000e+00 0.000e+00 2.000e+06 0.000e+00 0.000e+00 0.000e+00'
'      FISK         4 0.000e+00 0.000e+00 0.000e+00 5.000e+07 0.000e+00 0.000e+00'

=== D. AQWA Deck 3 mass card ===
'          MATE'
'     1         98000 1.000e+07'

=== E. OrcaWave body dict (project mass base = t, so force base = kN) ===
BodyExternalStiffnessMatrix... = [[0.0, ...], [0.0, ...], [0.0, 0.0, 2000000.0, 0.0, 0.0, 0.0],
                                  [0.0, 0.0, 0.0, 50000000.0, 0.0, 0.0], ...]
BodyExternalStiffnessMatrixOriginType = Body origin
BodyMass = 10000.0 (tonnes; kg_to_tonnes applied)
```

- Reproduced at: 2026-09-12T11:40:00Z
- Failure mode observed matches issue claim: **YES**.

Three distinct defects are demonstrated by that output, each bound to its criterion:

1. **Unit divergence.** The same canonical number `2.0e6` reaches the AQWA deck as `2.000e+06` alongside a mass card of `1.000e+07` kg, and reaches the OrcaWave project as `2000000.0` alongside `BodyMass = 10000.0` tonnes. Taking each project's own declared mass base as the governing case, the AQWA deck applies 2.0e6 N/m and the OrcaWave project applies 2.0e6 kN/m = 2.0e9 N/m. The applied heave restoring differs by a factor of 1000 between backends from one input, and nothing in either path raises.
2. **Asymmetry accepted.** `K[3][4] = 1.0e6` with `K[4][3] = 0.0` passes construction and would be emitted. A restoring matrix that is not symmetric is not derivable from a potential and is non-physical for a conservative restoring system; no check exists.
3. **Reference point unstated on one side.** OrcaWave receives an explicit, hard-coded `BodyExternalStiffnessMatrixOriginType = "Body origin"` (`orcawave_backend.py:375`); the AQWA FISK cards carry no origin declaration at all. The two backends therefore cannot be shown to refer the same matrix to the same point, and the canonical input never stated which point it meant.

The probe wrote nothing into the repository and modified no source or test file.

<!-- Distinct sources consulted: issue #1580 body, issues #1579 / #1582 / #1550 / #1825, seven production modules, four existing test modules, two workspace-hub rules. Count: well above the minimum 3. -->

---

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-09-12-issue-1580-restoring-stiffness-contract.md` |
| Implementation — contract module | `src/digitalmodel/hydrodynamics/diffraction/restoring_contract.py` |
| Implementation — schema | `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` |
| Implementation — AQWA adapter | `src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py` |
| Implementation — OrcaWave adapter | `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py` |
| Implementation — provenance | `src/digitalmodel/hydrodynamics/diffraction/output_schemas.py` |
| Implementation — license-free verification | `src/digitalmodel/hydrodynamics/diffraction/deck_validation.py` |
| Implementation — reverse round trip | `src/digitalmodel/hydrodynamics/diffraction/reverse_parsers.py` |
| Tests — schema fail-closed | `tests/hydrodynamics/diffraction/test_restoring_contract_schema.py` |
| Tests — normalization | `tests/hydrodynamics/diffraction/test_restoring_contract_normalization.py` |
| Tests — C44 / GM closed form | `tests/hydrodynamics/diffraction/test_restoring_c44_gm_relation.py` |
| Tests — cross-backend equivalence | `tests/hydrodynamics/diffraction/test_restoring_backend_equivalence.py` |
| Tests — provenance and pre-execution evidence | `tests/hydrodynamics/diffraction/test_restoring_provenance.py` |
| Plan review — Claude | `scripts/review/results/2026-09-12-plan-1580-claude.md` |
| Plan review — Codex | `scripts/review/results/2026-09-12-plan-1580-codex.md` |
| Plan review — Gemini | `scripts/review/results/2026-09-12-plan-1580-gemini.md` |
| Docs updates | `docs/plans/README.md` (index entry) |
| Wiki updates | none — `Client: N/A` |

---

## Deliverable

A `restoring_contract` module and an accompanying `RestoringStiffnessSpec` schema block that will make every diffraction restoring matrix carry its units, DOF order, reference point, sign convention and composition mode; that will convert those canonical values into each backend's native units explicitly at emission; that will fail closed on unknown units, ambiguous reference points, unsupported composition modes and asymmetric matrices; that will write a pre-execution evidence sidecar beside every generated deck; and that will verify the declared C44 against the closed-form relation `C44 = rho*g*V*GM_T` before any licensed solver is invoked.

---

## Governing physics — the C44 / GM relation

The hydrostatic roll-restoring coefficient of a freely floating body will be bound to the transverse metacentric height by

```
    C44 = rho * g * V * GM_T                       [N.m/rad]
```

with the following conditions, each of which the implementation will state in the artifact rather than assume:

- **Symbols and units.** `rho` = water density [kg/m^3]; `g` = gravitational acceleration [m/s^2]; `V` = displaced volume [m^3]; `GM_T` = transverse metacentric height [m]. The product carries N.m per radian of roll.
- **Decomposition.** `GM_T = KB + BM_T - KG`, where `BM_T = I_T / V` and `I_T` is the second moment of waterplane area about the longitudinal axis through the centre of flotation [m^4]. `KB` and `KG` are the vertical centre of buoyancy and centre of gravity, each measured from the same datum.
- **Reference point.** The equality holds for moments taken about the **centre of gravity**, with the body free to heave. Referred to any other point the coefficient changes, so `reference_point` will be a declared field and the transform will be applied explicitly; the relation will never be used to imply that two differently referred matrices are equal.
- **Equilibrium condition.** The equality additionally requires floating equilibrium, `M = rho * V`. A spec whose declared mass and declared displaced volume disagree beyond tolerance will fail the C44 check with that disagreement named, rather than reporting a GM the body does not have.
- **Sign convention.** Right-handed axes, `z` positive upward, free surface at `z = 0`, angles in radians, DOF order `(surge, sway, heave, roll, pitch, yaw) = (x, y, z, Rx, Ry, Rz)`. The restoring generalized force is `F = -K * xi`, so `C44 > 0` denotes a stabilising roll moment and `C44 <= 0` denotes a body that is not transversely stable at that draught.
- **Independent second form.** The same coefficient is expressible without `GM_T` as `C44 = rho * g * (I_T + V * (z_B - z_G))`. The two forms will be asserted equal in the test suite, which makes the check a conservation-class comparator on top of the closed-form one and catches a sign or datum error in `KB`/`KG` that a single-form check would pass.

Prior art for the inverse direction is `report_computations.py:64-66`, which computes `GM_T = C[3][3] / (rho * g * V)` from a solver result. This plan will supply the forward, input-side direction and will reuse the same symbol meanings so the two cannot drift apart.

### Canonical synthetic verification case

A generic rectangular box will be the single committed fixture. No client, vessel or project identifier will appear.

| Quantity | Value |
|---|---|
| Length L | 100.000 m |
| Beam B | 20.000 m |
| Draught T | 5.000 m |
| KG (from keel) | 8.000 m |
| rho | 1025.0 kg/m^3 |
| g | 9.80665 m/s^2 |
| V = L*B*T | 10 000.000 m^3 |
| I_T = L*B^3/12 | 66 666.667 m^4 |
| BM_T = I_T/V | 6.667 m |
| KB = T/2 | 2.500 m |
| GM_T = KB + BM_T - KG | 1.167 m |
| M = rho*V | 1.025e7 kg |
| **C44 = rho*g*V*GM_T** | **1.17271189583e8 N.m/rad** |
| C44 via rho*g*(I_T + V*(z_B - z_G)) | 1.17271189583e8 N.m/rad (relative difference 6.4e-16) |
| C44 expressed in OrcaWave native units | 1.1727118958e5 kN.m/rad |

Table 1. Closed-form roll-restoring target for the committed synthetic box fixture; units in the header; values computed at 2026-09-12T11:46:00Z.

A second, deliberately unstable variant with `KG = 10.000 m` gives `GM_T = -0.833 m` and `C44 = -8.3765e7 N.m/rad`. That case exists so the negative-GM branch is exercised: a negative C44 will be reported as a stability finding and will fail closed against a positive declared target, rather than being emitted silently.

---

## Composition semantics — the decision

An ambiguous composition mode is the defect this issue names, so the plan decides it rather than deferring it.

**Decision: the canonical restoring matrix AUGMENTS each backend's native hydrostatic matrix. `augment` will be the only mode emitted natively; `replace` will fail closed as unsupported; `correct_to_target` will be admitted only as a derived augment and will fail closed when its baseline is unobtainable.**

Grounds, each bound to the code that establishes it:

- **`augment` matches both backends' native semantics.** `orcawave_backend.py:319` sets `body["BodyHydrostaticStiffnessMethod"] = "Displacement"`, so the OrcaWave project always computes its own hydrostatic stiffness, and `BodyExternalStiffnessMatrix` (`orcawave_backend.py:362-375`) is applied in addition to it. The AQWA FISK cards are emitted in Deck 7 (`aqwa_backend.py:710-745`) alongside, not instead of, the deck's own hydrostatic computation. Neither backend exposes a documented switch that suppresses its native hydrostatic matrix.
- **`replace` is therefore not expressible.** Emitting a "replacement" matrix into either native field would produce native-plus-canonical, which is not what the author asked for, and the error would be silent. `replace` will raise `UnsupportedCompositionMode` naming both backends and the reason, per the acceptance criterion "unsupported composition modes fail closed".
- **`correct_to_target` is expressible only as a derived delta.** Where an author states a target (for example a target C44, or a target 6x6), the contract will compute `K_delta = K_target - K_baseline` and emit `K_delta` as an augment, recording `K_target`, `K_baseline`, `K_delta` and the baseline's provenance. `K_baseline` will come from the license-free Capytaine path already present at `deck_validation.py:30-105`. When Capytaine is absent — the module returns `{"available": False, ...}` at `deck_validation.py:53-54` — `correct_to_target` will raise rather than degrade to a bare augment, because degrading would apply the full target on top of an unknown native matrix.
- **The field name already says `external`.** `input_schemas.py:263` is `external_stiffness`. Interpreting it as anything but additive would change the physics of every existing spec silently, which is the failure mode `.claude/rules/reproducibility-is-not-correctness.md` exists to prevent.

Expression in the schema: `composition` will be a required enum on the new block, with values `augment`, `replace` and `correct_to_target`. There will be **no default**, so an author cannot inherit a mode they did not choose; omitting it fails validation with the three options and their meanings in the message.

**Legacy migration.** A bare `external_stiffness` with any non-zero entry and no accompanying `restoring:` block will fail closed with `AmbiguousRestoringUnits`, because the existing numbers are consistent with both N/m and kN/m and the repository holds no evidence that resolves which was intended (see Reproduction proofs, defect 1). The error message will print both candidate interpretations and the resulting C44 and GM_T under each, so the author resolves it by physics rather than by guess. An all-zero legacy matrix carries no physical content and will continue to pass. A single explicit escape, `restoring.legacy_units_assumed`, will let an author record the interpretation they assert, which is then written into provenance as `units_source: author_asserted` rather than `declared`.

---

## Pseudocode

```
# UnitBasis is imported from matrix_units.py, which #1582 owns. The members the
# restoring matrix needs are SI_N_M (N/m, N.m/rad, and the N and N.m coupling
# forms) and KN_M (kN/m, kN.m/rad — the OrcaWave native basis). No second
# stiffness-specific unit enum is introduced.
from matrix_units import UnitBasis           # SI_N_M | KN_M | ...  (owned by #1582)

enum ReferencePoint:        body_origin | centre_of_mass | explicit_xyz
enum CompositionMode:       augment | replace | correct_to_target

class RestoringStiffnessSpec:                     # new block on VesselSpec
    matrix:            6x6 floats                 # required when target absent
    target_c44:        float | None               # alternative to a full matrix
    units:             UnitBasis                  # required, no default
    reference_point:   ReferencePoint             # required, no default
    reference_xyz:     [x,y,z] | None             # required iff explicit_xyz
    composition:       CompositionMode            # required, no default
    dof_order:         literal "surge,sway,heave,roll,pitch,yaw"
    sign_convention:   literal "restoring_negative"   # F = -K.xi
    symmetry_tol:      float = 1e-9               # relative

function validate(spec):
    if units not in UnitBasis:               raise UnknownRestoringUnits(list options)
    if reference_point is explicit_xyz and reference_xyz is None:
                                             raise AmbiguousReferencePoint
    if composition == replace:               raise UnsupportedCompositionMode(reason, backends)
    if matrix is not 6x6:                    raise ValueError(shape)
    asym = max |K[i][j] - K[j][i]| / max(|K|, eps)
    if asym > symmetry_tol:                  raise AsymmetricRestoringMatrix(i, j, asym, tol)
    if any non-finite entry:                 raise ValueError

function to_canonical(spec) -> K_SI:              # single normalization point
    K = spec.matrix scaled by unit factor(spec.units -> SI_N_M)
    K = refer_to(K, spec.reference_point -> centre_of_mass, geometry, inertia)
    return K                                      # canonical = SI newtons, about CoM

function refer_to(K, from_point, to_point, offset r):
    # rigid-body congruence transform; T maps generalized displacement at
    # `to_point` onto generalized displacement at `from_point`
    T = [[I3, -skew(r)], [0, I3]]
    return T.transpose() @ K @ T

function to_native(K_SI, backend):
    if backend == "aqwa":     return K_SI                     # deck base is kg/N/m
    if backend == "orcawave": return K_SI / 1000.0            # project base is t/kN/m
    raise UnknownBackend

function derived_c44_check(K_SI, environment, hydrostatics):
    V   = displaced volume;  rho = environment.water_density;  g = environment.gravity
    if |mass - rho*V| / (rho*V) > tol:   fail("not in floating equilibrium", values)
    GM  = K_SI[3][3] / (rho * g * V)
    GM_geometric = KB + I_T/V - KG
    if |GM - GM_geometric| > tol:        fail("C44 inconsistent with geometry", both values)
    return {c44, gm_transverse, gm_geometric, rho, g, V, comparator_class: "closed-form"}

function emit(spec, backend, out_dir):
    K_SI    = to_canonical(spec)
    K_nat   = to_native(K_SI, backend)
    record  = { native: K_nat, native_units, canonical: K_SI, canonical_units,
                reference_point, composition, dof_order, sign_convention,
                derived: derived_c44_check(...), symmetry_residual,
                source_commit, generated_at }
    write   out_dir / "<deck-stem>.restoring.json"        <- pre-execution evidence
    write   a comment block inside the deck itself         <- survives loss of sidecar
    return  K_nat
```

---

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `src/digitalmodel/hydrodynamics/diffraction/restoring_contract.py` | canonical spec dataclasses, unit table, reference-point transform, composition resolution, C44/GM derivation, evidence-record writer |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` | add `RestoringStiffnessSpec` and its enums; add `VesselSpec.restoring`; add the fail-closed validator on non-zero legacy `external_stiffness` at `input_schemas.py:263-266` |
| Consume (not create) | `src/digitalmodel/hydrodynamics/diffraction/matrix_units.py` | the force/moment unit vocabulary and scale factors belong to [#1582](https://github.com/vamseeachanta/digitalmodel/issues/1582), whose plan creates this module with a `UnitBasis` enum. This plan will import that enum rather than add a second one. Should #1582 not land first, this plan creates only the minimum `UnitBasis` members the restoring matrix needs, in that same module and under that same name, so #1582 extends rather than replaces it. A `StiffnessUnits` enum in a second module is explicitly rejected — that is the four-way divergence `test_matrix_unit_completeness.py:14-23` records. |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py` | route `build_deck7` (`aqwa_backend.py:710-745`) through `to_native(..., "aqwa")`; emit the deck comment block; write the sidecar from `generate_single` / `generate_modular` (`aqwa_backend.py:140,180`) |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py` | route `orcawave_backend.py:371-375` through `to_native(..., "orcawave")`; set `BodyExternalStiffnessMatrixOriginType` from the declared reference point instead of the hard-coded `"Body origin"` |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/output_schemas.py` | add an `applied_restoring` provenance record to `HydrostaticResults` (`output_schemas.py:334-355`) carrying native values, normalized values, units, reference point and composition mode; extend `to_dict` / `from_dict` |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/deck_validation.py` | bind the already-reported `k44` (`deck_validation.py:79`) to a comparator, mirroring the existing `k33_within_tol` construction at `deck_validation.py:90-97` |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/reverse_parsers.py` | carry units and reference point back out of an OrcaWave project at `reverse_parsers.py:643-655` so a round trip does not lose the basis |
| Create | `tests/hydrodynamics/diffraction/test_restoring_contract_schema.py` | fail-closed validation suite |
| Create | `tests/hydrodynamics/diffraction/test_restoring_contract_normalization.py` | unit and reference-point normalization suite |
| Create | `tests/hydrodynamics/diffraction/test_restoring_c44_gm_relation.py` | closed-form C44 / GM target suite |
| Create | `tests/hydrodynamics/diffraction/test_restoring_backend_equivalence.py` | one canonical input, two backends, physical equivalence after declared conversion |
| Create | `tests/hydrodynamics/diffraction/test_restoring_provenance.py` | sidecar evidence and result-provenance suite |
| Update | `docs/plans/README.md` | index this plan |

---

## TDD Test List

Tests will be written before implementation. Every row names the file it lands in.

### `tests/hydrodynamics/diffraction/test_restoring_contract_schema.py`

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_unknown_units_fail_closed` | an unrecognised unit token is rejected, and the message lists the supported set | `units="kgf/m"` | `UnknownRestoringUnits`, message names the `UnitBasis` members `SI_N_M` and `KN_M` |
| `test_no_second_unit_enum_is_defined` | structural fence against the #1582 divergence | import `restoring_contract` | the module defines no unit enum of its own; its `units` annotation resolves to `matrix_units.UnitBasis` |
| `test_missing_units_fail_closed` | units carry no default | block with `matrix` and no `units` | `ValidationError` naming `units` |
| `test_ambiguous_reference_point_fails_closed` | `explicit_xyz` without coordinates is rejected | `reference_point="explicit_xyz"`, `reference_xyz=None` | `AmbiguousReferencePoint` |
| `test_missing_reference_point_fails_closed` | reference point carries no default | block without `reference_point` | `ValidationError` naming `reference_point` |
| `test_replace_composition_fails_closed` | `replace` is rejected with the backend reason | `composition="replace"` | `UnsupportedCompositionMode`, message names AQWA FISK and `BodyHydrostaticStiffnessMethod` |
| `test_missing_composition_fails_closed` | composition carries no default | block without `composition` | `ValidationError` listing the three modes |
| `test_asymmetric_matrix_fails_closed` | the probe's defect 2 is now caught | `K[3][4]=1e6`, `K[4][3]=0.0` | `AsymmetricRestoringMatrix` naming `(3,4)` and the residual |
| `test_symmetric_within_tolerance_passes` | float round-off does not trip the check | `K[3][4]=1e6`, `K[4][3]=1e6*(1+1e-12)` | constructs, residual `< 1e-9` |
| `test_wrong_shape_fails_closed` | a 5x6 or ragged matrix is rejected | 5 rows of 6 | `ValueError` naming the shape |
| `test_non_finite_entry_fails_closed` | `nan` / `inf` cannot reach a deck | `K[2][2]=float("nan")` | `ValueError` |
| `test_legacy_nonzero_external_stiffness_fails_closed` | the migration gate fires | `external_stiffness` non-zero, no `restoring` block | `AmbiguousRestoringUnits`, message prints C44 and GM under both N and kN readings |
| `test_legacy_all_zero_external_stiffness_passes` | zero carries no physical content | `external_stiffness` all zeros, no `restoring` block | constructs, no warning |
| `test_legacy_units_assertion_is_recorded_not_guessed` | the explicit escape is auditable | `restoring.legacy_units_assumed="SI_N_M"` | constructs; provenance `units_source == "author_asserted"` |

### `tests/hydrodynamics/diffraction/test_restoring_contract_normalization.py`

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_si_input_is_identity_to_canonical` | canonical is SI newtons | `units=SI_N_M`, `K[2][2]=2.0e6` | canonical `K[2][2] == 2.0e6` exactly |
| `test_kn_input_scales_to_canonical` | declared kN inputs convert once | `units=KN_M`, `K[2][2]=2.0e3` | canonical `K[2][2] == 2.0e6` |
| `test_round_trip_through_native_is_lossless` | conservation-class round-trip identity | canonical → `to_native` → back | relative difference `< 1e-12` in all 36 cells |
| `test_aqwa_native_equals_canonical` | AQWA deck base is N (`aqwa_backend.py:622-625`) | canonical `K` | native `K` identical |
| `test_orcawave_native_is_canonical_over_1000` | OrcaWave project base is kN (`orcawave_backend.py:13,345`) | canonical `K[3][3]=1.17271189583e8` | native `1.17271189583e5` |
| `test_reference_transform_body_origin_to_com_changes_c44` | the transform is real, not a relabel | `K` about body origin, CoM offset `r=[0,0,-2.5]` | referred `C44` differs from the raw value by the predicted congruence term |
| `test_reference_transform_is_involutive` | transform then inverse-transform recovers the input | `K`, offset `r` | relative difference `< 1e-12` |
| `test_reference_transform_preserves_symmetry` | congruence cannot manufacture asymmetry | symmetric `K`, any `r` | residual `< 1e-12` |
| `test_correct_to_target_lowers_to_delta_augment` | `correct_to_target` becomes `K_target - K_baseline` | target C44, Capytaine baseline stub | emitted matrix equals the delta; record carries target, baseline and delta |
| `test_correct_to_target_without_baseline_fails_closed` | absence of Capytaine does not degrade to a bare augment | `deck_validation.validate_mesh` returning `{"available": False}` | `RestoringBaselineUnavailable`, not a silent augment |
| `test_dof_order_is_pinned_across_backends` | the AQWA FISK row order matches the OrcaWave key order | canonical `K` with six distinct diagonal values | row `n` of the FISK cards corresponds to the same DOF as key `n` of the OrcaWave matrix |

### `tests/hydrodynamics/diffraction/test_restoring_c44_gm_relation.py`

Comparator class for this module: **closed-form** (plus one conservation cross-check). The expected values are derived from the geometry, not captured from the implementation.

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_c44_matches_rho_g_v_gm_for_synthetic_box` | the governing relation, stated as physics | `L=100, B=20, T=5, KG=8, rho=1025, g=9.80665` | `C44 == 1.17271189583e8 N.m/rad` within `1e-9` relative |
| `test_c44_second_form_agrees_with_gm_form` | the two independent expressions agree | same fixture | `rho*g*(I_T + V*(z_B - z_G))` matches the GM form within `1e-12` relative |
| `test_gm_recovers_from_c44_by_the_report_path` | forward and inverse directions are consistent with `report_computations.py:65` | `C44` from the fixture | `compute_stability`-style inversion returns `GM_T == 1.1666667 m` within `1e-9` |
| `test_c44_scales_linearly_with_gravity` | `g` enters once and only once | `g=9.80665` then `g=1.0` | ratio equals `9.80665` within `1e-12` |
| `test_c44_scales_linearly_with_density` | `rho` enters once and only once | `rho=1025` then `rho=1000` | ratio equals `1.025` within `1e-12` |
| `test_negative_gm_yields_negative_c44_and_is_flagged` | the unstable branch is exercised, not skipped | `KG=10.0` | `C44 == -8.3765135417e7`; check result marks transverse instability |
| `test_negative_c44_fails_a_positive_declared_target` | a sign error cannot pass silently | declared target `+1.17271e8`, geometry giving `-8.3765e7` | fail-closed with both values named |
| `test_mass_volume_disequilibrium_fails_the_check` | the equality's precondition is enforced | `mass = 0.5 * rho * V` | fail naming the equilibrium residual, not a GM value |
| `test_c44_target_in_kn_converts_before_comparison` | the comparison happens in one basis | target declared `units=KN_M` as `1.17271189583e5` | passes against the same SI geometry |
| `test_c44_target_mismatch_by_1000_is_caught` | the exact defect the probe demonstrated | target `1.17271189583e5` declared as `SI_N_M` | fail-closed; message names the 1000x ratio |
| `test_tolerance_is_fixed_before_the_run` | the tolerance is a module constant, not fitted to an observed gap | inspect the constant | tolerance defined in the module, not derived from any computed residual |

### `tests/hydrodynamics/diffraction/test_restoring_backend_equivalence.py`

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_one_canonical_input_yields_equivalent_applied_matrices` | the headline acceptance criterion | canonical `K` with non-zero diagonal **and** non-zero symmetric coupling `K[2][4]=K[4][2]` | AQWA FISK values and OrcaWave matrix values, each converted back through their own declared native units, agree with canonical within `1e-9` relative in all 36 cells |
| `test_raw_numeric_equality_between_backends_is_not_asserted` | the test compares physics, not digits | same input | AQWA native and OrcaWave native raw values differ by exactly 1000 |
| `test_pre_fix_behaviour_would_fail_this_test` | the guard has a failure mode | canonical `K` emitted without conversion | assertion fails, proving the check can fail |
| `test_coupling_block_survives_both_adapters` | off-diagonal terms are not dropped (the `test_matrix_unit_completeness.py` defect class) | `K[0][4]=K[4][0]=5.0e6` | present and correctly scaled in both artifacts |
| `test_zero_matrix_emits_no_fisk_and_a_zero_orcawave_block` | existing skip behaviour at `aqwa_backend.py:734` is preserved | all-zero canonical `K` | no FISK cards; OrcaWave block is `_zero_6x6()` |
| `test_reference_point_reaches_the_orcawave_origin_type` | `orcawave_backend.py:375` stops being hard-coded | `reference_point="centre_of_mass"` | `BodyExternalStiffnessMatrixOriginType == "Centre of mass"` |
| `test_aqwa_deck_declares_its_reference_point` | the AQWA side stops being silent | `reference_point="centre_of_mass"` | the emitted deck carries the reference point in its comment block |

### `tests/hydrodynamics/diffraction/test_restoring_provenance.py`

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_sidecar_written_beside_aqwa_deck` | pre-execution evidence exists | `generate_single(spec, tmp_path)` | `<stem>.restoring.json` present beside the `.dat` |
| `test_sidecar_written_beside_orcawave_project` | same on the other backend | OrcaWave generation into `tmp_path` | `<stem>.restoring.json` present beside the `.yml` |
| `test_sidecar_records_every_required_field` | acceptance criterion 4, asserted at the seam | any valid spec | keys `native`, `native_units`, `canonical`, `canonical_units`, `reference_point`, `composition`, `dof_order`, `sign_convention`, `derived.c44`, `derived.gm_transverse`, `symmetry_residual`, `comparator_class` all present and non-empty |
| `test_sidecar_native_matches_the_deck_text` | the evidence describes the artifact it sits beside | AQWA deck plus sidecar | every sidecar `native` entry is found in the FISK card text at the same row and column |
| `test_deck_comment_block_states_units_and_reference` | evidence survives loss of the sidecar | generated deck text | comment block names units, reference point and composition mode |
| `test_evidence_is_verifiable_without_a_licensed_run` | acceptance criterion 3 | generation only, no solver invoked | the C44 and GM_T in the sidecar are computable from the deck plus sidecar alone |
| `test_hydrostatic_results_carry_applied_restoring` | acceptance criterion 4 on the result side | `HydrostaticResults` with provenance | `to_dict()` / `from_dict()` round-trip preserves every provenance field |
| `test_provenance_records_comparator_class` | `.claude/rules/reproducibility-is-not-correctness.md` §1 | any valid spec | `comparator_class == "closed-form"` for the C44 check; never absent |

---

## Acceptance Criteria

- [ ] Contract tests pass: `uv run pytest tests/hydrodynamics/diffraction/test_restoring_contract_schema.py tests/hydrodynamics/diffraction/test_restoring_contract_normalization.py tests/hydrodynamics/diffraction/test_restoring_c44_gm_relation.py tests/hydrodynamics/diffraction/test_restoring_backend_equivalence.py tests/hydrodynamics/diffraction/test_restoring_provenance.py -v`
- [ ] No regression in the diffraction package: `uv run pytest tests/hydrodynamics/diffraction/ -q`
- [ ] No regression across the repository: `uv run pytest -q` at the repo's CI scope
- [ ] Issue AC1 — one canonical synthetic input produces applied restoring matrices that agree between AQWA and OrcaWave within `1e-9` relative after each backend's declared unit conversion, with non-zero diagonal and non-zero symmetric coupling terms.
- [ ] Issue AC2 — unknown units, ambiguous reference points, `composition: replace`, and asymmetric matrices each raise a named, typed error; each is covered by a test that fails when the guard is removed.
- [ ] Issue AC3 — a generated AQWA deck and a generated OrcaWave project each carry a `<stem>.restoring.json` sidecar and an in-deck comment block from which the applied matrix, its units and its reference point are verifiable with no licensed solver run.
- [ ] Issue AC4 — `HydrostaticResults` provenance records native values, normalized values, units, reference point and composition mode, and survives a `to_dict` / `from_dict` round trip.
- [ ] Issue AC5 — `C44 = rho*g*V*GM_T` is verified against the synthetic box of Table 1 at `1.17271189583e8 N.m/rad` within `1e-9` relative, using synthetic mass `1.025e7 kg` and `g = 9.80665 m/s^2`, and the second form agrees within `1e-12`.
- [ ] Composition mode is decided and documented in the schema: `augment` emitted, `replace` fail-closed, `correct_to_target` lowered to a delta augment and fail-closed without a baseline.
- [ ] Only generic synthetic fixtures are committed; `scripts/legal/legal-sanity-scan.sh` passes and no client, vessel or project identifier appears in any new file.
- [ ] CI lint toolchain run per `.claude/rules/verify-ci-lint-toolchain.md` before push.
- [ ] Review artifacts posted to `scripts/review/results/`.
- [ ] Issue comment posted summarizing the landed contract, per the must-fire issue-comment rule.

---

## Adversarial Review Summary

<!-- Filled in after the review pass completes. Do not post to GitHub until this section is populated. -->

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | pending | pending |
| Codex | pending | pending |
| Gemini | pending | pending |

**Overall result:** pending

Revisions made based on review:
- (none yet)

---

## Risks and Open Questions

- **Risk — dependency on #1579.** [#1579](https://github.com/vamseeachanta/digitalmodel/issues/1579) owns inertia-tensor origin and reference-point semantics. This plan consumes that vocabulary and shall not redefine it: the reference-point enum will reuse the `body_origin` / `centre_of_mass` spelling already present at `orcawave_backend.py:353-360`. A parallel planning session has produced `docs/plans/2026-09-12-issue-1579-inertia-tensor-origin.md`, which records that `VesselInertia.inertia_tensor_origin` is a bare `str` today (`input_schemas.py:209-215`) and proposes tightening it while keeping the same two member spellings. Those spellings are therefore the ones this plan adopts. Coordination checkpoint: re-confirm #1579's final enum before writing `restoring_contract.py`; if it changes, this work adopts it rather than introducing a second enum.
- **Risk — dependency on and live collision with #1582.** [#1582](https://github.com/vamseeachanta/digitalmodel/issues/1582) owns added-mass and damping unit normalization. A parallel planning session has produced `docs/plans/2026-09-12-issue-1582-matrix-unit-normalization.md` (untracked in the working tree at 2026-09-12T11:50:00Z, alongside untracked plans for #1579 and #1581). That plan creates `src/digitalmodel/hydrodynamics/diffraction/matrix_units.py` carrying a `UnitBasis` enum and the scale factors, and states that `diffraction_units.py` will be left as-is. **This plan has been written to consume `matrix_units.UnitBasis` rather than introduce a parallel stiffness-unit enum**; an earlier draft would have added force converters to `diffraction_units.py`, which is precisely the divergence `test_matrix_unit_completeness.py:14-23` documents. Two merge points require sequencing at implementation time: both plans modify `output_schemas.py` (this one adds `applied_restoring` to `HydrostaticResults`; #1582 adds `MatrixUnitProvenance` and `resolve_unit(i, j)` to `HydrodynamicMatrix`), and both add provenance test modules. Recommended order: land #1582's `matrix_units.py` first, then this plan imports it. `external_damping` (`input_schemas.py:267-270`) is deliberately left unchanged here even though it exhibits the identical pass-through at `aqwa_backend.py:725-733` and `orcawave_backend.py:386-390`.
- **Risk — coordination with #1550.** [#1550](https://github.com/vamseeachanta/digitalmodel/issues/1550) is CLOSED and its output-unit decisions are settled: `orcawave_runner._build_results_from_object` converts to SI/kg and `solver.report_extractors` stays OrcFxAPI-native te/kN (`test_native_units_invariant.py:1-26`). This plan coordinates with that split and does not duplicate it — the canonical input basis will be SI newtons, which matches the SI/kg result path, and the native-te/kN report path is out of scope.
- **Open — a suspected live defect on the native report path, offered as a hypothesis.** `solver/report_extractors.py:221` calls `compute_stability(hydrostatics)` with the function's defaults `rho=1025.0` kg/m^3 and `g=9.81` (`report_computations.py:39-40`), while `test_native_units_invariant.py:1-12` states that this path's `restoringMatrix` stays in OrcFxAPI-native kN. If both hold, `GM_T = C[3][3] / (rho*g*V)` at `report_computations.py:65` understates GM_T by a factor of 1000 on that path. This is a hypothesis, not a finding: it has not been confirmed against an actual `.owr`, which requires a licensed OrcaWave artifact. The test that settles it is a single read of `hydrostaticResults.restoringMatrix[3][3]` from any existing `.owr` compared against `rho*g*V*GM_T` for the same body. If confirmed, it warrants its own issue rather than absorption into this one — per `.claude/rules/mechanism-before-publication.md`, a mechanism merely consistent with a symptom is a hypothesis until the source or the output settles it.
- **Open — the AQWA FISK reference datum.** `orcawave_backend.py:375` declares an explicit origin type; the AQWA FISK cards emitted at `aqwa_backend.py:734-742` declare none. Whether AQWA refers a frequency-independent stiffness matrix to the structure's centre of gravity, to the body origin, or to the deck's defined position is a vendor-manual question, and the manual is vendor-licensed so it is not committed here (`.claude/rules/codes-standards-data-routing.md`). Resolution step, to be completed before the AQWA adapter is written: read the AQWA reference manual's Deck 7 FISK entry at its licensed location and record the datum in the plan and in the module docstring. Until it is recorded, the cross-backend equivalence test shall assert equivalence only for matrices whose declared reference point makes the two datums provably coincident, and shall fail closed otherwise rather than assume they match.
- **Risk — migration cost of the legacy fail-closed gate.** Every committed spec carrying a non-zero `external_stiffness` without a `restoring:` block will begin failing validation. The count and the list of affected spec files shall be enumerated on the live filesystem before implementation begins, and the migration of each shall be part of the same change set. A coverage claim that "all specs are migrated" shall be backed by that enumeration, not asserted.
- **Risk — the equivalence test could pass for the wrong reason.** A test that converts both native outputs back through the same conversion function it is testing proves only that the function is its own inverse. The cross-backend test therefore compares each native value against the closed-form canonical target of Table 1, which neither backend produced, and includes `test_pre_fix_behaviour_would_fail_this_test` so the guard is shown to have a failure mode.
- **Risk — Capytaine availability.** `correct_to_target` depends on the optional Capytaine path (`deck_validation.py:45-54`). The mode fails closed when Capytaine is absent, so the dependency does not silently change physics; it does mean `correct_to_target` is unusable on hosts without the optional dependency, which shall be stated in the schema field description.
- **Open — should a target GM_T be accepted directly, alongside a target C44?** Accepting `target_gm_transverse` would be a convenience and the conversion is the same relation. It is deferred from this plan to keep one input form under test, and is flagged for the user at approval.

---

## Complexity: T3

**T3** — the change spans two solver adapters, the input schema, the unit module, the output schema, the license-free validation module and the reverse parser (seven production files), adds one new module and five new test modules, and carries a physics-bearing acceptance criterion that must hold against a closed-form comparator rather than against a captured baseline. It also consumes two sibling contracts still in planning (#1579, #1582) under epic #1825, so a wrong decision here propagates into every three-way benchmark the epic depends on. That is systemic scope, which is the T3 criterion; T2 would understate the review depth this warrants.
