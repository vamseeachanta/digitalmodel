# Capytaine module analysis

[Read the module analysis](index.html). Synthetic coefficients are supplied, not BEM-computed. The single-degree and existing coupled force-balance checks qualify only manual algebra. Native solve, mesh convergence, physical accuracy, delegated RAO and NetCDF remain NOT_EVALUATED.

Rebuild from the repository root with NumPy, SciPy and xarray available:

```sh
PYTHONPATH=src python scripts/reports/build_capytaine_module_report.py --scratch "$TMPDIR/capytaine-work"
```

Scratch must resolve outside the checkout. Use `--output-dir` for an external copy; in-checkout output is limited to this report directory. Calculations run in memory; no intermediate file is saved in the checkout. The optional native solver is not required.

The dimensional response is in m for force in N; no wave-amplitude normalization is supplied. Phase uses deg, heading uses rad and omega uses rad/s. Inputs and hand derivations are committed in the builder and independent tests, as identified by the report source register.

`manifest.json` binds the builder, local method and test sources, palette and the other three report files using SHA-256. Its own digest is excluded to avoid self-reference. The deterministic rebuild test checks all four exact deliverable byte streams.

Run the report controls:

```sh
PYTHONPATH=src python -m pytest tests/reporting/test_capytaine_module_report.py
```
