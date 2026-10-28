# Fatigue applications module report

The [module analysis](report.html) documents the load-scaling, counting and damage
processing routes. Its executed example is limited to an explicitly supplied
synthetic uniform-bar lookup and the offline stress converter. Real stress-transfer
tables, stress-concentration factors, counting qualification, fatigue damage and
service life remain `NOT_EVALUATED`.

The synthetic area, SI conversions, table and probes are derived independently in
[the hand-calculation basis](../../../../tests/structural/fatigue_apps/test_tension_stress_conversion_hand_calcs.py).
The SI prefix factors are defined by [NIST](https://www.nist.gov/pml/owm/metric-si-prefixes).
The [source record](sources.json) retains source locators and code digests; the
[change record](changes.json) accompanies the report revision.

From the repository root, with the converter dependencies installed and `TMPDIR`
set to the supplied scratch folder, rebuild all deliverables using:

```bash
PYTHONPATH=src uv run --no-project --python python3 python docs/reports/modules/fatigue_apps/build_report.py --output-dir "$TMPDIR/fatigue-apps-report" --scratch-dir "$TMPDIR/fatigue-apps-calculation"
```

The output folder receives `report.html`, `changes.json` and `sources.json`.
The explicit scratch folder contains the synthetic lookup and converter output
directory. No default lookup, production input, counting-to-damage route or solver
is used. The page includes all styles and calculation data; it needs no external
resources. Repository links resolve from this module report folder.

Run [the report acceptance tests](../../../../tests/structural/fatigue_apps/test_module_report.py)
with the repository pytest configuration. The tests compare unrounded values with
independent force/area arithmetic, parse displayed quantities and units, mutate a
computed result and compare isolated rebuilds with the delivered bytes.
Reproducibility does not establish physical validation. Broader numerical
qualification remains open on the parent module report issue.
