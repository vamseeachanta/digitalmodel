# Quasi-static floating wind module analysis

[Read the module report](index.html). The report executes committed synthetic
modal and scalar impedance controls through public response functions.
The other public functions are inventoried with explicit qualification limits.

Rebuild from the repository root with NumPy and SciPy available:

```sh
PYTHONPATH=src python scripts/reports/build_fowt_quasi_static_module_report.py --scratch ../report-scratch --output-dir docs/reports/modules/fowt_quasi_static
```

Scratch must be outside the checkout. Only the four deliverables are written to
the output directory. No native solver, named study data or campaign is used.
The manifest binds source bytes and artifact digests, excluding its own digest.
Two independent rebuilds must reproduce every committed deliverable byte.

The positive natural roots follow from det(K−ω²I)=product(j²−ω²), giving T=2π/j.
The scalar response follows from 2/(2−i)=(4+2i)/5. These independent calculations
are the expected values in the hand-value and report tests. Numerical tolerances
and normalized units are stated in the report; these controls are not design data.
Campaign convergence, mooring design acceptance and native BEM are NOT_EVALUATED.

Lesson: exact sampled roots require explicit retention as well as interval
bracketing; the endpoint and exact-grid regressions preserve this requirement.
