# Deep-water wave-number worked example

This example calls the existing `wave_number` helper with synthetic gravity
`4 m/s^2` and angular frequencies `(1, 2) rad/s`. These inputs are arithmetic
probes rather than environmental data. Hand derivation [H] below gives exact
wave numbers `(1/4, 1) 1/m`; the generated page shows the computed values,
units, derivation, input basis, limits and revision history after its summary.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-25 | Add a reproducible arithmetic example with an independent hand calculation. |

## Build and check

Run from the repository root with Python, NumPy and pytest available:

```bash
PYTHONPATH=src uv run --no-project --with pytest python docs/reports/modules/parametric_hull_analysis/build_worked_example.py --output-dir "$TMPDIR/worked-example"
PYTHONPATH=src uv run --no-project --with pytest python -m pytest tests/hydrodynamics/parametric_hull_analysis -q -rA -p no:cacheprovider
```

Set `TMPDIR` to an external temporary directory. The builder requires NumPy;
pytest is needed for tests. The existing module suite also needs its scientific
and test dependencies. Provision dependencies before running offline; the
builder itself uses no network, input dataset, licensed software or solver.

The sole generated artifact is `worked-example.html` in `--output-dir`.
It is a self-contained HTML page, with no remote assets. No generated output
is committed. Rebuilds have identical content without execution timestamps
or output-directory paths. Importing the builder performs no calculation or
file write. The helper source is loaded directly with `runpy.run_path` to avoid
package-wide chart and solver-adapter imports and helper bytecode caches.
No helper implementation is copied or changed.

The subprocess tests parse visible HTML table cells, compare their values and
units with independent exact fractions from [H], check the output location,
and compare rebuild bytes in the same and different output directories.

## Hand derivation [H]

[H] Independent hand calculation for linear free-surface gravity waves.
Assume small amplitude, inviscid incompressible irrotational flow, deep water,
no current and no surface tension. At the undisturbed free surface the linear
conditions are `eta_t = phi_z` and `phi_t + g*eta = 0`, so
`phi_tt + g*phi_z = 0`. For the harmonic potential
`phi ~ exp(k*z)*cos(k*x-omega*t)`, substitution gives `omega^2 = g*k`.
Equivalently, `omega^2 = g*k*tanh(k*h)` tends to this expression in deep water.
Therefore `k = omega^2/g`, with `omega` an angular frequency in `rad/s`.
The dimensional balance is `[s^-2]/[m*s^-2] = [1/m]`.

The values `g=4 m/s^2` and `omega=(1, 2) rad/s` are specified synthetic
arithmetic probes. Substitution by hand gives `1^2/4 = 1/4 1/m` and
`2^2/4 = 1 1/m`. These values are the independent basis for the tests;
they are not inferred from the helper's output. The generated page includes
the same derivation as its local citation [H].

## Limits and follow-up

`NOT_EVALUATED`: finite-depth iteration, heading conventions, forward-speed
RAO corrections, parametric hull sweeps, shallow-water corrections and BEM
solver execution. The example makes no full-module qualification claim.
The parent report issue remains open; the full report and shared indexes
remain follow-up.
