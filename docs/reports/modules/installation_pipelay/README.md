# Pipelay stress and integrity analysis

[Open the module report](index.html). The report covers every public function in
the pipelay stress and integrity modules, with independent synthetic stress and
route-equilibrium arithmetic. Supplied strength and fraction are arithmetic
controls, not standard criteria. Engineering qualification is NOT_EVALUATED.

The committed inputs are in [scripts/reports/build_installation_pipelay_module_report.py](../../../../scripts/reports/build_installation_pipelay_module_report.py); the independent
derivations are in the hand-value test. No solver or extra runtime package is required.

Rebuild from the repository root, with TMPDIR set to an external scratch directory:

```sh
PYTHONPATH=src uv run --no-project python scripts/reports/build_installation_pipelay_module_report.py --scratch "$TMPDIR/pipelay-report"
```

Use `--output-dir` to publish into a separate directory. Temporary staging remains
outside the checkout. The four deliverables are `index.html`, `changes.json`,
`manifest.json` and this README. The manifest hashes every source dependency and
the other three deliverables; deterministic rebuild checks the manifest itself.

The coating-model assumptions and weld-repair interpretation remain
NOT_EVALUATED. Their required evidence and the remaining method qualification
gaps are listed in the report. Revision history and the change panel accompany
the summary; engineering comments can be exported beside the report.
