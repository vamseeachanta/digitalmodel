# Plan for #1578: Coupled Roll Response from Sloshing Coefficients

> **Status:** draft
> **Complexity:** T2
> **Date:** 2026-07-13
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1578
> **Client:** N/A
> **Lane:** lane:codex
> **Review artifacts:** `scripts/review/results/2026-07-13-plan-1578-{claude,codex,gemini}.md`

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

`RollResponseRequest` will require finite SI fields:

```text
omega_rad_s > 0
excitation_moment_nm >= 0
vessel_inertia_kg_m2 > 0
vessel_restoring_nm_per_rad > 0
vessel_damping_nm_s_per_rad >= 0
tank_stiffness_nm_per_rad: finite
tank_damping_nm_s_per_rad >= 0
condition_id, coefficient_set_id, amplitude_id: nonempty opaque IDs
coefficient_omega_rad_s, coefficient_fill, coefficient_amplitude_rad
support bounds and interpolation/extrapolation disposition
```

No geometry, client, project, file path, or free-form source text belongs in the
numeric request. A separate caller may map its private sources to opaque IDs.

The frozen complex impedances are:

```text
Z0 = Cv - omega^2*I + i*omega*Bv
Zc = (Cv+Kt) - omega^2*I + i*omega*(Bv+Bt)
Theta0 = F/Z0
Thetac = F/Zc
reduction_percent = 100*(1 - abs(Thetac)/abs(Theta0))
```

This follows by moving reaction `-Kt*theta-Bt*theta_dot` to the left side. The
result `roll-response-v1` will contain complex real/imaginary impedance and
response components, magnitude in rad/deg, phase, reduction, total stiffness/
damping, opaque provenance, interpolation/extrapolation flags, and validity.

Zero excitation will return zero baseline/coupled response and an explicit
`reduction_percent=null` because a ratio is undefined. Nonzero excitation with
near-zero impedance will reject as singular. Total restoring must remain
positive and total damping nonnegative; NaN/Inf and wrong units reject.

Coefficient support is three-dimensional: omega, fill, and roll amplitude.
Default policy is `reject`. Explicit `clamp` or `linear-extrapolate` will require
a caller flag, return the original/query/effective coordinates and distance from
support, and mark the result non-nominal. Condition or amplitude reuse without
an exact opaque binding will reject. #1577 schema/version and physical units are
mandatory; deprecated raw moment aliases reject.

An optional `require_reduction` policy will reject a negative reduction beyond
a numerical tolerance. Otherwise amplification will be returned and prominently
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
| `test_outside_support_rejects_by_default` | no warning-only clamp |
| `test_explicit_clamp_and_extrapolation_flags` | non-nominal coordinates/distance are complete |
| `test_units_and_dimensional_identity` | impedance terms all resolve to N*m/rad |
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
- [ ] `uv run --no-project pytest -q tests/solvers/openfoam/test_roll_response.py`
      passes all analytical/validation cases.
- [ ] Focused coefficient/interpolation/coupling regressions pass after split.
- [ ] Full `tests/solvers/openfoam/` regression passes.
- [ ] Ruff, compileall, ≤400 file/≤50 function, legal/privacy, and
      `git diff --check` gates pass.
- [ ] Default out-of-support behavior rejects; every allowed non-nominal result
      exposes original/effective coordinates, distance, and policy.
- [ ] Zero excitation, singular resonance, wrong-sign damping, amplification,
      condition retuning, and dimensional identities match the frozen contract.
- [ ] T2 code/artifact review has no MAJOR and issue receives a summary comment.
- [ ] No client data, remote run, public result promotion, self-merge, or close.

## Adversarial Review Summary

| Provider | Verdict | Findings |
|---|---|---|
| Claude | pending | exact pushed draft required |
| Codex | pending | exact pushed draft required |
| Gemini | pending | exact pushed draft required |

**Overall:** draft; implementation requires adversarial review and explicit user
approval. No agent may apply `status:plan-approved` or create its marker.

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
