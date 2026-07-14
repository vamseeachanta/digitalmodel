# Plan for #1577: Sloshing Harmonic Normalization

> **Status:** draft — r2 MAJOR findings resolved inline in r3; user approval required
> **Complexity:** T2
> **Date:** 2026-07-13
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1577
> **Client:** N/A
> **Lane:** lane:codex
> **Review artifacts:** `scripts/review/results/2026-07-13-plan-1577-r{1,2}-consolidated.md`

---

## Resource Intelligence Summary

### Existing code

- `src/digitalmodel/solvers/openfoam/validation/sloshing_sweep.py:105-168`
  currently fits every supplied sample to `M=M0+Mc*cos(wt)+Ms*sin(wt)` and
  emits raw `-Ms`/`-Mc` moment amplitudes as `in_phase_coeff`/`quad_coeff`.
  It carries no explicit settled window, physical normalization, fit residual,
  cycle stability, harmonic content, or unit contract.
- `tests/solvers/openfoam/validation/test_sloshing_sweep.py:163-223` proves raw
  amplitude/phase splitting with synthetic signals, but its pure-damping oracle
  names a moment amplitude `B` and therefore does not detect missing division by
  `omega*theta0`.
- `src/digitalmodel/solvers/openfoam/sloshing_coupling.py` consumes stiffness
  in `N*m/rad` and damping in `N*m/(rad/s)`, so the raw reducer output is not
  dimensionally safe for direct coupling.
- The reducer module is 485 lines, above the universal 400-line limit. Moving
  reduction into a focused module will bring the legacy sweep module below the
  limit rather than adding more behavior to it.
- Issue [#1574](https://github.com/vamseeachanta/digitalmodel/issues/1574) owns
  removal of existing project-specific identifiers from reusable sloshing
  modules. #1577 will not reproduce those identifiers and will not implement
  until #1574's separately reviewed cleanup is merged.

### Standards and sources

- The governing public convention will be the issue's source-neutral identity:
  `theta=theta0*sin(omega*t)`, `M=-K*theta-B*theta_dot`.
- `tests/solvers/openfoam/test_time_history.py` supplies strictly increasing
  time-series and phase-lag precedents; the new reducer will remain independent
  of any client geometry or result.
- `tests/solvers/openfoam/validation/test_wave_excited_body.py` and
  `test_maccamy_fuchs.py` provide harmonic-fit and settled-window test patterns,
  but not roll-moment normalization or cycle stability.
- Drive query `sloshing harmonic damping normalization settled cycles` returned
  five unrelated results across six indexes; two indexes were unreachable and
  three stale. No drive document will be used as a formula authority.
- No standards-derived constant will be emitted, so the calculation Citation
  sidecar contract does not apply. Units and sign identities will be derived
  explicitly from the frozen equations.

### Gaps and reproduction

The current pure-damping fixture uses `M=-3*cos(omega*t)` at non-unit
`theta0`/`omega`, yet expects `quad_coeff=3`. A physical damping coefficient
must instead be `3/(omega*theta0)`. This static mismatch reproduces the issue;
no production data is required.

Missing surfaces are: explicit request/result model, settled-cycle selection,
time-grid validation, physical unit conversion, normalized K/B, energy sign
oracle, residual/harmonic diagnostics, stability thresholds, schema migration,
and compatibility handling.

Distinct sources: issue #1577, reducer, reducer tests, coupling model, time-
history tests, two validation precedents, issue #1574, and drive search (9+).

---

## Artifact Map

| Artifact | Path |
|---|---|
| Plan | `docs/plans/2026-07-13-issue-1577-sloshing-harmonic-normalization.md` |
| New reducer | `src/digitalmodel/solvers/openfoam/harmonic_reduction.py` |
| Sweep adapter | `src/digitalmodel/solvers/openfoam/validation/sloshing_sweep.py` |
| Exports | `src/digitalmodel/solvers/openfoam/__init__.py`; `validation/__init__.py` |
| Focused tests | `tests/solvers/openfoam/test_harmonic_reduction.py` |
| Sweep regression | `tests/solvers/openfoam/validation/test_sloshing_sweep.py` |
| Review evidence | `scripts/review/results/2026-07-13-plan-1577-*.md` |

## Deliverable

A source-neutral harmonic reducer will convert an explicitly settled roll-
reaction moment window into raw harmonic terms and normalized physical roll
stiffness/damping, with deterministic diagnostics and fail-closed validity.

## Interface and Physics Contract

`HarmonicReductionRequest` will require:

```text
drive_period_s > 0
roll_amplitude > 0
roll_amplitude_unit: rad | deg
phase_origin_s: finite, with theta=theta0*sin(omega*(t-phase_origin_s))
roll_angle_rad[] and roll_velocity_rad_s[]: finite histories
moment_unit: N*m | kN*m
settled_start_s >= first sample
settled_cycles: integer >= 5
min_samples_per_cycle: integer >= 20 (default 20)
max_amplitude_drift_fraction: [0,0.20] (default 0.02)
max_phase_drift_deg: [0,20] (default 2)
```

The reducer will accept finite, equal-length time, moment, angle, and velocity
arrays with strictly increasing seconds. Angle/velocity will be independently
validated against the amplitude/phase identity within `1e-6` relative and
`1e-9` absolute SI tolerances. The window `[start,start+cycles*T]` must be fully
covered. Linear interpolation will create exact boundary samples. No native gap
may exceed `T/min_samples_per_cycle`; outside samples cannot affect a fit.

After SI conversion, trapezoidal quadrature weights on the irregular grid will
drive one weighted least-squares fit:

```text
M(t) = M0 - K44*theta(t) - B44*theta_dot(t)
omega = 2*pi/T
```

Trapezoid weights will be `w0=(t1-t0)/2`, `wN=(tN-tN-1)/2`, and
`wi=(t[i+1]-t[i-1])/2`. Each design column will be divided by its weighted L2
norm `sqrt(sum(w*x^2))`; a zero norm rejects. SVD rank uses
`sigma > max(m,n)*machine_epsilon*sigma_max`; rank must be 3 and
`sigma_max/sigma_min <= 1e8`.

Raw terms use the fixed absolute-time basis `M=M0+a*cos(omega*t)+b*sin(omega*t)`.
With `phi=omega*phase_origin_s`:

```text
a = K44*theta0*sin(phi) - B44*omega*theta0*cos(phi)
b = -K44*theta0*cos(phi) - B44*omega*theta0*sin(phi)
in_phase_coeff = -b   [N*m raw amplitude]
quad_coeff = -a       [N*m raw amplitude]
```

A simultaneous timestamp/phase-origin shift leaves physical K/B invariant.

The result schema `sloshing-harmonic-reduction-v2` will retain `M0`, raw `a`,
raw `b`, raw amplitude/phase, theta0, omega, window, sample/cycle counts, K44,
B44, units, first-harmonic NRMSE, harmonics 2-5 energy ratio, per-cycle amplitude/
phase, maximum drift, and validity flags. Ordering and JSON serialization will
be deterministic; no source or case identifier is accepted by this numeric API.

Each cycle will be fitted independently. Adjacent amplitude drift is
`abs(A[i+1]-A[i])/max(A[i+1],A[i],1e-12 N*m)`; phase drift is the absolute
wrapped difference in degrees and is undefined/rejected when either amplitude
is at the floor. Exceeding configured drift rejects. NRMSE and harmonic ratio
are diagnostics, not hidden filters.

Diagnostics will use the same trapezoidal weights but independent equations:

```text
NRMSE = sqrt(sum(w*r^2)/sum(w*(M-Mbar_w)^2))
E2_5/E1 = sum(n=2..5, An^2) / max(A1^2, (1e-12 N*m)^2)
work = trapezoid_integral(M_measured*theta_dot_measured, t)
```

Harmonics 1--5 plus a constant will be fitted simultaneously with the same
rank/condition checks. Centered-moment RMS or A1 below `1e-12 N*m` makes phase/
NRMSE undefined and rejects. Define
`Bscale=max(abs(B44),abs(K44)/omega,1 N*m*s/rad)`,
`Btol=1e-12*Bscale`, and
`Wtol=1e-10*max(pi*settled_cycles*omega*theta0^2*Bscale,1e-12 J)`.
`abs(B44)<=Btol` with `abs(work)<=Wtol` is accepted as zero damping; `B44>Btol`
requires `work<-Wtol`; every other sign state rejects. Mutation tests will
reverse the production damping sign and independently synthesize negative-B/
positive-work cases while leaving the measured-work oracle unchanged.

The old eight-field row will not silently change meaning. Inventory identifies
only `validation/sloshing_sweep.py` CSV fields and
`sloshing_coupling.SloshingCFDResult.from_dict` (plus focused tests) as readers
of `in_phase_coeff`/`quad_coeff`. Schema v2 will retain those fields as explicit
deprecated raw moment amplitudes for that reader only; physical consumers use
K44/B44. #1578 will migrate coupling to the v2 envelope. Removal requires a
separate approved major-schema issue after repository-wide `rg` proves zero
readers; unknown schema/units fail closed.

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `harmonic_reduction.py` | focused request/result/fit/diagnostic implementation under 400 lines |
| Create | `test_harmonic_reduction.py` | non-tautological TDD and hostile time-series matrix |
| Modify | `validation/sloshing_sweep.py` | remove embedded fit, call v2 reducer, reduce file below 400 lines |
| Modify | existing sweep tests | compatibility/schema/window regression |
| Modify | package exports | publish the generic v2 interface |

## TDD Test List

| Test | Verification |
|---|---|
| `test_pure_damping_normalizes_by_omega_theta0` | independently generated M yields exact B44 and near-zero K44 |
| `test_pure_stiffness_normalizes_by_theta0` | exact K44 and near-zero B44 |
| `test_mixed_signal_recovers_coefficients` | independent K/B signal recovers both plus raw a/b |
| `test_time_and_phase_origin_shift_invariant` | shifting time and phase together leaves K/B identical |
| `test_motion_history_mismatch_rejects` | angle/velocity must agree with amplitude/phase contract |
| `test_energy_oracle_dissipative_negative_work` | numerical work sign agrees with positive B44 |
| `test_wrong_sign_rejects` | energy/coefficient sign conflict fails |
| `test_pure_stiffness_irregular_grid_uses_zero_bands` | roundoff-scale B/work is accepted as zero |
| `test_self_consistent_negative_damping_rejects` | negative B with positive measured work still rejects |
| `test_transient_samples_excluded` | large pre-window transient cannot affect result |
| `test_requires_five_complete_cycles` | short/partial window fails |
| `test_irregular_adequate_grid_passes` | nonuniform but bounded sampling fits correctly |
| `test_clustered_grid_uses_trapezoid_weights` | clustered samples cannot bias a known oracle |
| `test_missing_endpoint_is_interpolated` | whole-cycle boundaries use the frozen interpolation rule |
| `test_rank_and_condition_fail_closed` | degenerate/ill-conditioned designs reject |
| `test_gap_duplicate_reverse_nonfinite_fail` | ambiguous time grids fail closed |
| `test_zero_amplitude_frequency_fail` | singular normalizations reject |
| `test_cycle_amplitude_instability_fails` | adjacent drift above threshold rejects |
| `test_wrapped_phase_instability_fails` | phase wrapping is handled; real drift rejects |
| `test_harmonic_diagnostics` | injected 2nd-5th components produce expected energy ratio/NRMSE |
| `test_near_zero_signal_rejects_undefined_phase` | numeric diagnostic floors are deterministic |
| `test_units_convert_without_drift` | deg/rad and N*m/kN*m give identical SI coefficients |
| `test_v2_schema_and_raw_aliases` | physical fields cannot be confused with deprecated raw amplitudes |
| `test_no_identifier_fields` | request/result schema rejects arbitrary metadata/private labels |
| `test_module_size_limits` | implementation ≤400 lines, functions ≤50 lines; legacy file ≤400 |

## Implementation Sequence

1. Wait for separately approved/merged #1574 cleanup and pin its SHA.
2. Add RED request validation, pure K/B/mixed, unit, energy, and window tests.
3. Implement the focused request/result models and normalized first-harmonic fit.
4. Add RED grid, per-cycle stability, residual/harmonic, and determinism tests;
   implement diagnostics and fail-closed validity.
5. Add RED sweep v2/compatibility tests; move reduction out of the oversized
   legacy module and update exports/readers without silent semantic replacement.
6. Run exact acceptance and T2 adversarial code/artifact review.

## Acceptance Criteria

- [ ] #1574 is merged and its exact SHA is recorded before implementation.
- [ ] RED evidence precedes each implementation slice.
- [ ] `PYTHONPATH=src uv run python -m pytest -q tests/solvers/openfoam/test_harmonic_reduction.py`
      passes every named oracle/edge case.
- [ ] `PYTHONPATH=src uv run python -m pytest -q tests/solvers/openfoam/validation/test_sloshing_sweep.py tests/solvers/openfoam/test_sloshing_coupling.py`
      passes with an explicit v2 compatibility boundary.
- [ ] `uv run ruff check src/digitalmodel/solvers/openfoam/harmonic_reduction.py src/digitalmodel/solvers/openfoam/validation/sloshing_sweep.py tests/solvers/openfoam/test_harmonic_reduction.py tests/solvers/openfoam/validation/test_sloshing_sweep.py` passes.
- [ ] `PYTHONPATH=src uv run python -m compileall -q src/digitalmodel/solvers/openfoam` passes.
- [ ] New/modified files satisfy ≤400 lines/file and ≤50 lines/function.
- [ ] `PYTHONPATH=src uv run python -m pytest -q tests/solvers/openfoam` passes.
- [ ] With `WORKSPACE_HUB_ROOT` and `DIGITALMODEL_REL_FROM_HUB` set, the SHA-
      verified legal command passes:
      `EXPECTED_SHA="$(git rev-parse HEAD)" && test "$(git -C "$WORKSPACE_HUB_ROOT/$DIGITALMODEL_REL_FROM_HUB" rev-parse HEAD)" = "$EXPECTED_SHA" && (cd "$WORKSPACE_HUB_ROOT" && bash scripts/legal/legal-sanity-scan.sh --repo="$DIGITALMODEL_REL_FROM_HUB" --diff-only)`; `git diff --check` passes.
- [ ] T2 code/artifact review has no MAJOR; issue receives a summary comment.
- [ ] No client data, queue execution, self-merge, self-close, or public result
      promotion occurs.

## Adversarial Review Summary

| Provider | Verdict | Findings |
|---|---|---|
| Claude | MAJOR | phase reference, independent sign oracle, diagnostics, compatibility, commands |
| Codex | MAJOR | phase origin, exact readers, weighted equations, dependency block |
| Gemini | MAJOR | configurable grid, floors/conditioning, sign policy, commands |

**Overall:** r1/r2 MAJOR findings are resolved inline in r3. Per the loop-break
rule r3 is not redispatched; explicit user approval remains required. No agent
may apply `status:plan-approved` or create its marker.

## Risks and Open Questions

- #1574 is a hard privacy dependency because the touched legacy module already
  contains identifiers that this issue may not preserve or expand.
- The v2 schema is intentionally breaking for physical consumers; raw aliases
  exist for one release only and are never presented as physical K/B.
- First-harmonic diagnostics do not make a nonlinear response linear; high
  residual/harmonic content remains a downstream applicability decision.

## Complexity: T2

This changes a reusable numerical contract, units/signs, schema, tests, and an
oversized legacy module, but remains a bounded source-neutral reducer refactor.
