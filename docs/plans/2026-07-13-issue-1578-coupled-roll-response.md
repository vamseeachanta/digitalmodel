# Plan for #1578: Coupled Roll Response from Sloshing Coefficients

> **Status:** draft — r2 MAJOR findings resolved inline in r3; user approval required
> **Complexity:** T2
> **Date:** 2026-07-13
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1578
> **Client:** N/A
> **Lane:** lane:codex
> **Review artifacts:** `scripts/review/results/2026-07-13-plan-1578-r{1,2}-consolidated.md`

---

## Resource Intelligence Summary

### Existing code

- `src/digitalmodel/solvers/openfoam/sloshing_coupling.py` interpolates swept
  moment coefficients and evaluates tank moment, but has no vessel response,
  reduction percentage, complex impedance, condition provenance, or response
  applicability contract.
- `moment_coefficients()` currently clamps out-of-band frequency/fill with a
  warning. #1578 requires reject-by-default behavior and explicit labeled
  extrapolation; a warning-only clamp cannot support engineering response.
- `tests/solvers/openfoam/test_sloshing_coupling.py` covers coefficient
  interpolation, moment signs, tuning, and one-way strength, but not a coupled
  oscillator or independent analytical response oracle.
- The coupling implementation is 682 lines and its test file is 410 lines,
  already above universal limits. New behavior must use focused modules and
  split touched legacy surfaces rather than deepen the monolith.
- Issue [#1577](https://github.com/vamseeachanta/digitalmodel/issues/1577) will
  define physical `K44`/`B44` inputs; raw harmonic amplitudes must never enter
  the response evaluator. Issue [#1574](https://github.com/vamseeachanta/digitalmodel/issues/1574)
  owns privacy cleanup in the touched reusable module. Both are hard dependencies.

### Sources and standards

- The source-neutral SDOF identity will use
  `I*theta_ddot+Bv*theta_dot+C*theta=F+M_tank` with
  `M_tank=-Kt*theta-Bt*theta_dot`.
- Existing `MomentCoefficients.moment()` establishes the reaction sign; the new
  tests will derive the same sign independently from complex impedance.
- `tests/solvers/openfoam/validation/test_wave_excited_body_rao.py` provides
  frequency-response test structure but no roll/tank coupling contract.
- Drive query `roll response sloshing damping coupled oscillator` returned five
  unrelated results across six indexes; two indexes were unreachable and three
  stale. No drive result is a formula authority.
- No standards-derived constant is emitted. The calculation Citation sidecar
  does not apply; every identity will be embedded and dimensionally tested.

### Gaps and reproduction

No public function accepts vessel inertia/restoring/damping/excitation plus
physical tank coefficients and returns baseline/coupled response. A repository
search finds only moment, interpolation, tuning, and strength helpers. The issue
gap is therefore present without production data.

Missing: exact SI request/result schema, complex response equations, phase,
reduction definition, singularity/stability checks, amplitude/condition binding,
support/extrapolation policy, provenance flags, and analytical TDD.

Distinct sources: issue #1578, coupling code/tests, response validation precedent,
issues #1577/#1574, and drive query (7).

---

## Artifact Map

| Artifact | Path |
|---|---|
| Plan | `docs/plans/2026-07-13-issue-1578-coupled-roll-response.md` |
| Response model | `src/digitalmodel/solvers/openfoam/roll_response.py` |
| Coefficient models | `src/digitalmodel/solvers/openfoam/sloshing_coefficients.py` |
| Interpolation model | `src/digitalmodel/solvers/openfoam/sloshing_interpolation.py` |
| Compatibility facade | `src/digitalmodel/solvers/openfoam/sloshing_coupling.py` |
| Focused tests | `tests/solvers/openfoam/test_roll_response.py` |
| Split regressions | `tests/solvers/openfoam/test_sloshing_{coefficients,interpolation,coupling}.py` |
| Review evidence | `scripts/review/results/2026-07-13-plan-1578-*.md` |

## Deliverable

A source-neutral frequency-domain SDOF evaluator will combine per-condition
vessel properties with physical tank K44/B44 and return baseline/coupled complex
roll response, reduction, provenance, support flags, and fail-closed validity.

## Physics and Interface Contract

`CoefficientSampleV1` will bind one immutable #1577 v2 reduction payload hash to
its verified K44/B44, omega, roll amplitude, declared fill fraction, and opaque
condition ID. `CoefficientSetV1` will carry exact unit enums, sorted axes, every
sample, a canonical-JSON SHA-256, and schema version. Its builder verifies that
the payload omega/amplitude/K/B match the envelope and rejects copied, duplicate,
or mismatched payloads. Fill is supplied by the source-neutral case-definition
caller because #1577 intentionally has no case identifier; the builder binds it
into the set digest. The response API accepts this set, never caller-supplied
K/B or caller-supplied support bounds.

`RollResponseRequest` will require finite typed quantities:

```text
omega: Quantity(value>0, unit=rad/s)
fill_fraction: [0,1]
excitation: ComplexQuantity(real,imag, unit=N*m)
vessel_inertia: Quantity(value>0, unit=kg*m^2)
vessel_restoring: Quantity(value>0, unit=N*m/rad)
vessel_damping: Quantity(value>=0, unit=N*m*s/rad)
coefficient_set: validated CoefficientSetV1
support_policy: reject | clamp | linear-extrapolate
max_extrapolation_fraction: [0,0.25], default 0
```

No geometry, client, project, file path, or free-form source text belongs in the
numeric request. A separate caller may map its private sources to opaque IDs.

The frozen complex impedances are:

```text
Z0 = Cv - omega^2*I + i*omega*Bv
Zc = (Cv+Kt) - omega^2*I + i*omega*(Bv+Bt)
Theta0 = Fcomplex/Z0
Thetac = Fcomplex/Zc
reduction_percent = 100*(1 - abs(Thetac)/abs(Theta0))
```

This follows by moving reaction `-Kt*theta-Bt*theta_dot` to the left side. The
result `roll-response-v1` will contain complex real/imaginary impedance and
response components, magnitude in rad/deg, phase, reduction, total stiffness/
damping, opaque provenance, interpolation/extrapolation flags, and validity.

Response phase will use `atan2` in the same global complex reference as the
input excitation; the transfer phase is `arg(Theta)-arg(F)` when `F != 0`.
Zero excitation will return zero baseline/coupled response and an explicit
`reduction_percent=null` because a ratio is undefined. Nonzero excitation with
near-zero impedance will reject as singular when
`abs(Z) <= 1e-12*max(abs(C),abs(omega^2*I),abs(omega*B),1 N*m/rad)`.
Total restoring must remain positive and total damping nonnegative. Unknown or
dimensionally wrong unit enums, NaN, and Inf reject before arithmetic.

Coefficient support will be a complete rectilinear Cartesian grid with strictly
increasing unique omega/fill/amplitude axes, at least two points per varying
axis, and exactly one sample per node. Missing/ragged/duplicate nodes reject.
Trilinear interpolation order is amplitude, fill, then omega and has golden
corner/edge/interior vectors. Default out-of-support policy is `reject`.
Explicit clamp/extrapolation acts independently per axis and returns signed
dimensionless distance `(query-nearest_bound)/(axis_max-axis_min)` for every
axis, original/effective coordinates, and a non-nominal flag. A zero-span axis
requires exact equality and cannot be extrapolated.

Because K/B depend on roll amplitude, accepted coupled response must be self-
consistent. After omega/fill interpolation, K(A) and B(A) are linear within each
amplitude cell. For each cell define
`Aref=max(abs(Alo),abs(Ahi),1e-6 rad)` and `x=A/Aref`; construct the exact quartic
`A^2*|Zc(A)|^2-|F|^2=0` in x and divide coefficients by their maximum absolute
value before solving. A root is real when
`abs(Im(x)) <= 1e-12 + 1e-10*abs(Re(x))`, must give `A>=0`, and must lie in the
allowed interval within `1e-12 rad + 1e-9*max(abs(A),abs(bound))`. Shared roots
are deduplicated with that same absolute+relative amplitude tolerance.

Every candidate will then be checked using the dimensionally explicit fixed-
point residual
`abs(A-abs(F/Zc(A))) <= 1e-10 rad + 1e-8*max(A,abs(F/Zc(A)))`, after the existing
scale-aware impedance check. Exactly one verified root is required; zero rejects
as unsupported/non-convergent and multiple roots reject as ambiguous. Clamp/
extrapolation extends only explicitly permitted segments and records per-axis
distance. This avoids using coefficients at a different response amplitude.

An optional `require_reduction` policy will reject
`reduction_percent < -1e-8 percentage points`. Values within that zero band are
reported as zero within tolerance. Otherwise amplification will be prominently
flagged; it will never be silently called anti-roll performance. Positive tank
damping alone is not assumed to guarantee reduction at every frequency because
tank stiffness may retune resonance.

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `roll_response.py` | focused request/result/complex evaluator |
| Create | `sloshing_coefficients.py` | physical coefficient/provenance models from #1577 |
| Create | `sloshing_interpolation.py` | reject-default 3D support policy |
| Modify | `sloshing_coupling.py` | compatibility facade; remove extracted sections and private identifiers |
| Create/split | focused test modules | analytical response and existing interpolation/moment regressions |
| Modify | package exports | publish v1 evaluator without ambiguous aliases |

All modules/tests will remain ≤400 lines and functions ≤50 lines. No client
identifier, private result, or project-coded default will enter public code.

## TDD Test List

| Test | Verification |
|---|---|
| `test_zero_tank_matches_baseline` | Kt=Bt=0 gives identical complex response and 0% reduction |
| `test_positive_damping_reduces_at_resonance` | independent closed form gives lower amplitude |
| `test_reaction_sign_moves_terms_left` | direct time-domain residual matches Zc sign |
| `test_wrong_sign_damping_rejects` | negative total/tank damping fails |
| `test_require_reduction_rejects_amplification` | stiffness-retuned amplification cannot pass anti-roll policy |
| `test_amplification_flag_without_policy` | negative reduction is explicit, not hidden |
| `test_resonance_with_finite_damping` | exact resonant magnitude/phase match analytic solution |
| `test_singular_zero_damping_resonance_fails` | zero impedance rejects |
| `test_zero_excitation_has_null_reduction` | zero/zero ratio is not fabricated |
| `test_condition_retuning` | each condition queries its own omega/fill/amplitude coefficients |
| `test_provenance_mismatch_rejects` | wrong condition/amplitude/schema cannot reuse coefficients |
| `test_coefficient_set_hash_binds_v2_payload` | copied/mismatched reduction envelope rejects |
| `test_rectilinear_grid_rejects_missing_duplicate_ragged` | topology is closed before interpolation |
| `test_trilinear_corner_edge_interior_goldens` | interpolation has independent numeric oracles |
| `test_single_self_consistent_amplitude_root` | quartic root gives response amplitude used for K/B |
| `test_zero_and_multiple_amplitude_roots_reject` | unsupported or ambiguous nonlinear screening fails |
| `test_near_zero_and_cell_boundary_root_dedup` | absolute+relative root tolerances are deterministic |
| `test_quartic_candidate_uses_amplitude_residual` | polynomial scaling cannot replace physical residual verification |
| `test_outside_support_rejects_by_default` | no warning-only clamp |
| `test_explicit_clamp_and_extrapolation_flags` | non-nominal coordinates/distance are complete |
| `test_per_axis_normalized_distance` | unlike units never share one scalar support distance |
| `test_units_and_dimensional_identity` | typed quantities resolve to N*m/rad; wrong enums reject |
| `test_phase_quadrants` | complex phase uses stable `atan2` convention |
| `test_deterministic_result_schema` | ordering/serialization and opaque provenance are stable |
| `test_raw_harmonic_alias_rejects` | #1577 deprecated amplitudes cannot masquerade as K/B |
| `test_split_regression` | legacy moment/interpolation/tuning behavior remains covered after split |
| `test_size_limits` | every touched file/function passes universal limits |

## Implementation Sequence

1. Wait for separately approved/merged #1574 and #1577; pin exact SHAs/schema.
2. Add RED request validation and independent zero-tank/damping/sign/resonance/
   singular analytical tests.
3. Implement focused physical coefficient and response request/result/evaluator.
4. Add RED 3D support, provenance, retuning, clamp/extrapolation, and raw-alias
   rejection tests; implement reject-default interpolation policy.
5. Add RED amplification and deterministic schema tests; implement policy flags.
6. Split the oversized legacy implementation/tests while preserving focused
   regressions and removing identifiers through the #1574-approved change.
7. Run acceptance and T2 adversarial code/artifact review.

## Acceptance Criteria

- [ ] #1574 and #1577 are merged; exact SHAs and v2 coefficient schema are pinned.
- [ ] RED evidence precedes each implementation slice.
- [ ] `PYTHONPATH=src uv run python -m pytest -q tests/solvers/openfoam/test_roll_response.py`
      passes all analytical/validation cases.
- [ ] `PYTHONPATH=src uv run python -m pytest -q tests/solvers/openfoam/test_sloshing_coefficients.py tests/solvers/openfoam/test_sloshing_interpolation.py tests/solvers/openfoam/test_sloshing_coupling.py` passes.
- [ ] `PYTHONPATH=src uv run python -m pytest -q tests/solvers/openfoam` passes.
- [ ] `uv run ruff check src/digitalmodel/solvers/openfoam/roll_response.py src/digitalmodel/solvers/openfoam/sloshing_coefficients.py src/digitalmodel/solvers/openfoam/sloshing_interpolation.py tests/solvers/openfoam` passes.
- [ ] `PYTHONPATH=src uv run python -m compileall -q src/digitalmodel/solvers/openfoam` passes; AST size tests enforce 400/50.
- [ ] With `WORKSPACE_HUB_ROOT` and `DIGITALMODEL_REL_FROM_HUB` set, the SHA-
      verified legal command passes:
      `EXPECTED_SHA="$(git rev-parse HEAD)" && test "$(git -C "$WORKSPACE_HUB_ROOT/$DIGITALMODEL_REL_FROM_HUB" rev-parse HEAD)" = "$EXPECTED_SHA" && (cd "$WORKSPACE_HUB_ROOT" && bash scripts/legal/legal-sanity-scan.sh --repo="$DIGITALMODEL_REL_FROM_HUB" --diff-only)`; `git diff --check` passes.
- [ ] Default out-of-support behavior rejects; every allowed non-nominal result
      exposes original/effective coordinates, distance, and policy.
- [ ] Zero excitation, singular resonance, wrong-sign damping, amplification,
      condition retuning, and dimensional identities match the frozen contract.
- [ ] T2 code/artifact review has no MAJOR and issue receives a summary comment.
- [ ] No client data, remote run, public result promotion, self-merge, or close.

## Adversarial Review Summary

| Provider | Verdict | Findings |
|---|---|---|
| Claude | MAJOR | provenance envelope, amplitude consistency, grid, units, phase/tolerance |
| Codex | MAJOR | coefficient-set binding, fixed-point policy, closed interpolation topology |
| Gemini | MAJOR | bypassable raw K/B request, dimensionless support distances, exact commands |

**Overall:** r1/r2 MAJOR findings are resolved inline in r3. Per the loop-break
rule r3 is not redispatched; explicit user approval remains required. No agent
may apply `status:plan-approved` or create its marker.

## Risks and Open Questions

- #1574 and #1577 are hard dependencies; this issue cannot repair their scope
  under its own approval.
- Linear frequency-domain response is an SDOF screening model, not nonlinear FSI.
- Stiffness retuning can amplify response even with positive damping; the result
  contract therefore separates physical validity from anti-roll acceptance.
- Existing monolith/test sizes require a behavior-preserving split before adding
  response logic.

## Complexity: T2

The task is source-neutral and bounded but changes reusable physics, interpolation
policy, schema, provenance, and an oversized module/test architecture.
