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
Calculated report values are rounded to 12 decimal places before serialization
and hashing. The maximum rounding contribution is 5e-13 in the stated normalized
units, below the 1e-12 absolute test tolerance. Adjacent binary64 rounding in both
directions is exercised by byte comparisons of all four deliverables. This bound
applies to these synthetic controls. Root bracketing uses frequency-scaled
binary64 tolerance, exercised at frequency scales of 1e-8, 1 and 1e8.
The inventory status ARITHMETIC_CONTROL: diagonal synthetic does not qualify
added-mass interpolation, branch ordering or modal labelling.
The dedicated layout is committed in the builder; only the canonical brand
stylesheet is imported, with explicit UTF-8 reads.

The positive natural roots follow from det(K−ω²I)=product(j²−ω²), giving T=2π/j.
The scalar response follows from 2/(2−i)=(4+2i)/5. These independent calculations
are the expected values in the hand-value and report tests. Numerical tolerances
and normalized units are stated in the report; these controls are not design data.
Campaign convergence, mooring design acceptance and native BEM are NOT_EVALUATED.

Lesson: exact sampled roots require explicit retention as well as interval
bracketing; the endpoint and exact-grid regressions preserve this requirement.
Revision labels are derived from history, and generated pages are excluded from
presentation dependencies to keep unrelated rebuilds from invalidating evidence.
