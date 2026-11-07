# Structural fatigue module report

[Module report](report.html) — synthetic counted-cycle arithmetic, method map,
independent derivation, evidence and explicit qualification limits.

The artificial curve has no material identity. Standard coefficients,
mean-stress corrections, rainflow-to-damage qualification, frequency-domain
methods and physical remaining life remain `NOT_EVALUATED` by this report.
The package's existing standalone examples retain their separate contracts.

From the repository root, set `FATIGUE_REPORT_PYTHON` to a Python interpreter
containing the repository dependencies. Keep the supplied `TMPDIR`,
`UV_CACHE_DIR` and `PRE_COMMIT_HOME` exported, then rebuild with:

```bash
PYTHONDONTWRITEBYTECODE=1 MPLCONFIGDIR="$TMPDIR/matplotlib" \
  PYTHONPATH=src uv run --no-project --python "$FATIGUE_REPORT_PYTHON" \
  python docs/reports/modules/fatigue/build_report.py --output-dir "$TMPDIR/fatigue-report"
```

The output directory receives `report.html`, `changes.json` and `sources.json`.
There are no intermediate files. The page embeds its style, calculation record,
source record and changes; it can be read independently of its directory.
Source paths identify repository evidence, with content digests in
[sources.json](sources.json). [changes.json](changes.json) preserves revision changes.

The independent example and parsed-output checks are in
[test_module_report.py](../../../../tests/structural/fatigue/test_module_report.py).
Rebuild determinism establishes reproducibility, not physical validation.
