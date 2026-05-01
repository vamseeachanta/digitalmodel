# #2571 implementation review — B1528 SIROCCO time-trace benchmark report

Verdict: **APPROVE with documented environment caveat**

## Scope challenged

Reviewed the #2571 implementation against the approved plan:

- `src/digitalmodel/naval_architecture/b1528_sirocco_time_trace.py`
- `src/digitalmodel/naval_architecture/data/b1528_sirocco_time_trace.yml`
- `tests/naval_architecture/test_b1528_sirocco_time_trace.py`
- `docs/domains/marine-engineering/b1528-sirocco-time-trace-report.md`
- generated outputs under `outputs/b1528_sirocco/time_trace/`

## Findings

### Dynamic architecture

PASS. The governing state update is first-order Nomoto:

```text
r_dot = (K * alpha_R - r) / T
psi_dot = r
x_dot = U cos(psi)
y_dot = U sin(psi)
```

Rudder force and yaw moment are emitted as diagnostics only. They are not fed back into `r_dot`, so the implementation avoids double-counting a moment-balance model and Nomoto `K/T` response.

### Rudder-local inflow feedback

PASS. The implementation computes:

```text
v_R = x_R * r
beta_R = atan2(-x_R * r, U)
alpha_R = delta_cmd - beta_R
U_R = hypot(U, v_R)
```

Diagnostics use `U_R` and `alpha_R`, so effective attack angle changes after the yaw-rate transient.

### Benchmark boundary

PASS. The report uses a `benchmark-source-gap` panel because the #2569 benchmark evidence is narrative heading/SOG context without instrumented x/y trajectory and includes tug/current/anchor effects. It does not fabricate a quantitative overlay.

### Caveats / overclaim control

PASS. Markdown/HTML/provenance text states:

- source-gap sensitivity mode;
- diagnostic-only rudder force/yaw moment;
- not a full MMG simulation;
- not an incident reconstruction;
- not an IMO compliance assessment;
- no class compliance conclusion.

### Tests and artifacts

PASS. Tests cover packaged YAML loading, zero-rudder straight trace, ±rudder symmetry, local inflow feedback, timestep sensitivity, report artifact generation, and caveat presence.

## Validation evidence

Observed before review artifact creation:

- `UV_NO_SYNC=1 PYTHONPATH=src uv run --no-sync pytest tests/naval_architecture/test_b1528_sirocco_time_trace.py -q -p no:randomly -p no:cov -p no:benchmark` — `7 passed in 15.73s`.
- `UV_NO_SYNC=1 PYTHONPATH=src uv run --no-sync --with ruff ruff check src/digitalmodel/naval_architecture/b1528_sirocco_time_trace.py src/digitalmodel/naval_architecture/__init__.py tests/naval_architecture/test_b1528_sirocco_time_trace.py` — `All checks passed!`.
- Smoke generation wrote `3` runs / `1803` rows plus CSV/JSON/provenance/Markdown/HTML/manifest.

A later sequential rerun of pytest+ruff, and then targeted pytest alone, timed out in the local environment after prior successful validation. No code changes were made after the successful targeted test/ruff gates except documentation text. Treat this as an environment/runner caveat to preserve in closeout, not as a known numerical failure.

## Residual risk

Low to medium: the workflow is a preliminary source-gap/sensitivity model with assumed Nomoto coefficients, not a calibrated B1528 reconstruction.
