# Plan for #1577: Sloshing Harmonic Normalization

> **Status:** draft
> **Complexity:** T2
> **Date:** 2026-07-13
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1577
> **Client:** N/A
> **Lane:** lane:codex
> **Review artifacts:** `scripts/review/results/2026-07-13-plan-1577-{claude,codex,gemini}.md`

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
moment_unit: N*m | kN*m
settled_start_s >= first sample
settled_cycles: integer >= 5
min_samples_per_cycle: integer >= 20 (default 20)
max_amplitude_drift_fraction: [0,0.20] (default 0.02)
max_phase_drift_deg: [0,20] (default 2)
```

The reducer will accept finite, equal-length arrays with strictly increasing
seconds. The requested window will be `[start,start+cycles*T]`; it must be fully
covered, include each cycle, and have no inter-sample gap above `T/20`. Samples
outside the window will not affect any fit.

After converting moment to `N*m` and amplitude to radians, the fit will be:

```text
M(t) = M0 + a*cos(omega*t) + b*sin(omega*t)
omega = 2*pi/T
K44 = -b/theta0                 [N*m/rad]
B44 = -a/(omega*theta0)         [N*m/(rad/s)]
```

The result schema `sloshing-harmonic-reduction-v2` will retain `M0`, raw `a`,
raw `b`, raw amplitude/phase, theta0, omega, window, sample/cycle counts, K44,
B44, units, first-harmonic NRMSE, harmonics 2-5 energy ratio, per-cycle amplitude/
phase, maximum drift, and validity flags. Ordering and JSON serialization will
be deterministic; no source or case identifier is accepted by this numeric API.

Each cycle will be fitted independently. Drift will use maximum adjacent
relative amplitude change (with a defined near-zero floor) and wrapped phase
difference in degrees. Exceeding configured drift rejects rather than returning
an accepted coefficient. NRMSE and harmonic ratio are diagnostics, not hidden
filters; downstream policy may impose stricter bounds.

A direct numerical work check will compute `integral(M*theta_dot dt)` over whole
cycles. Positive dissipative `B44` must produce negative work by the reaction
moment. A sign conflict between fitted B44 and work will reject.

The old eight-field row will not silently change meaning. Sweep collection will
emit a versioned v2 nested reduction record and explicit deprecated raw aliases
only where an existing reader requires them. `in_phase_coeff`/`quad_coeff` will
remain labeled raw moment amplitudes during one compatibility release; physical
consumers must use `K44`/`B44`. Unknown schema/units will fail closed.

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
| `test_energy_oracle_dissipative_negative_work` | numerical work sign agrees with positive B44 |
| `test_wrong_sign_rejects` | energy/coefficient sign conflict fails |
| `test_transient_samples_excluded` | large pre-window transient cannot affect result |
| `test_requires_five_complete_cycles` | short/partial window fails |
| `test_irregular_adequate_grid_passes` | nonuniform but bounded sampling fits correctly |
| `test_gap_duplicate_reverse_nonfinite_fail` | ambiguous time grids fail closed |
| `test_zero_amplitude_frequency_fail` | singular normalizations reject |
| `test_cycle_amplitude_instability_fails` | adjacent drift above threshold rejects |
| `test_wrapped_phase_instability_fails` | phase wrapping is handled; real drift rejects |
| `test_harmonic_diagnostics` | injected 2nd-5th components produce expected energy ratio/NRMSE |
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
- [ ] `uv run --no-project pytest -q tests/solvers/openfoam/test_harmonic_reduction.py`
      passes every named oracle/edge case.
- [ ] `uv run --no-project pytest -q tests/solvers/openfoam/validation/test_sloshing_sweep.py tests/solvers/openfoam/test_sloshing_coupling.py`
      passes with an explicit v2 compatibility boundary.
- [ ] `uv run --no-project ruff check` on changed Python paths and
      `uv run --no-project python -m compileall -q src/digitalmodel/solvers/openfoam`
      pass.
- [ ] New/modified files satisfy ≤400 lines/file and ≤50 lines/function.
- [ ] Full `tests/solvers/openfoam/` regression passes.
- [ ] Legal/no-private-identifier and `git diff --check` gates pass.
- [ ] T2 code/artifact review has no MAJOR; issue receives a summary comment.
- [ ] No client data, queue execution, self-merge, self-close, or public result
      promotion occurs.

## Adversarial Review Summary

| Provider | Verdict | Findings |
|---|---|---|
| Claude | pending | exact pushed draft required |
| Codex | pending | exact pushed draft required |
| Gemini | pending | exact pushed draft required |

**Overall:** draft; implementation requires adversarial review and explicit user
approval. No agent may apply `status:plan-approved` or create its marker.

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
