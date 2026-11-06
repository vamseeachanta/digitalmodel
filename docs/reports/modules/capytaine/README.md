# Capytaine module analysis

[Read the module analysis](index.html). Synthetic coefficients are supplied, not BEM-computed. The single-degree and existing coupled force-balance checks qualify only manual algebra. Native solve, mesh convergence, physical accuracy, delegated RAO and NetCDF remain NOT_EVALUATED.

Rebuild from the repository root with NumPy, SciPy and xarray available:

```sh
PYTHONPATH=src python scripts/reports/build_capytaine_module_report.py
```

Use `--output-dir` for an external copy; in-checkout output is limited to this report directory. Calculations run in memory. The optional `--scratch` is retained for compatibility: if supplied it must resolve outside the checkout and must not overlap the output. No scratch directory or intermediate file is created. The optional native solver is not required.

The dimensional response is in m for force in N; no wave-amplitude normalization is supplied. Phase uses deg, heading uses rad and omega uses rad/s. Inputs and hand derivations are committed in the builder and independent tests, as identified by the report source register.

`manifest.json` binds the builder, local method and test sources, palette and the other three report files using SHA-256. Its own digest is excluded to avoid self-reference. The deterministic rebuild test checks all four exact deliverable byte streams. The shared palette is read as UTF-8; a change to `docs/api/_assets/brand.css` requires regenerating this report and its manifest before the deterministic checks can pass.

Deferred method changes: explicit Green-function selection, consumption of configured hydrostatic stiffness, label-based added-mass and plot pairing, and a residual criterion for singular response fallback. These require separate regressions and fixes; this report does not close the remaining module review scope.

Run the report controls:

```sh
PYTHONPATH=src python -m pytest tests/reporting/test_capytaine_module_report.py
```
