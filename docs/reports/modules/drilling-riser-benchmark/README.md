# Generic drilling riser benchmark

This internal program benchmarking analysis prepares a comparison around the
existing generic campaign. It has no completed native benchmark or established
operating window. Open `index.html` for the generated report, including its
revision history, inputs, methods, worked example, charts and evidence gaps.

From the repository root, with NumPy, PyYAML, Pydantic, Beautiful Soup, Jinja2 and
Plotly available:

```sh
PYTHONPATH=src python scripts/reporting/build_drilling_riser_benchmark.py --output docs/reports/modules/drilling-riser-benchmark
```

`cases.json` retains the existing proposed grid. Every offset, current speed and
profile remains a required input marked “reference to be selected: candidates
must be public”. Its numeric proposals are inherited from the existing
preparation code; no additional loading range is invented. Model inputs already
committed under `docs/domains/orcaflex/library/campaigns/generic-drilling-riser/models/`
are reused. The regression test regenerates them with the repository generator
and applies both repository YAML validators. No second model set is committed.

The report uses the shared typed report engine. It follows the module-report
artifact convention used by the cathodic protection and pipelay reports in pull
requests #3224 and #3252. The separate drilling-riser stack-up documentation in
pull request #3226 remains the detailed module reference.

## Screen and result contracts

The screen calls the existing constant-tension column solution and stack-up
product equation. Angle estimates are column slopes, the top force is a legacy
assumed screen reading, and chord extension is not mechanical stroke. Sources,
limitations and independent hand calculations are included in the report and
tests. The full sweep is diagnostic; it has no engineering acceptance verdict.

`benchmark.read_result` reads a small JSON summary. It never reads native solver
files or imports a solver. The schema is demonstrated by
`tests/drilling_riser/fixtures/benchmark_synthetic.json`, which is synthetic
arithmetic only and never selected by the report. A record contains:

- `case_id` and `case_digest`, binding to the `case_digest` returned by `benchmark.screen_cases`
  (case values, sources, template and model-input digests, and calculation fingerprints);
- `origin` (`stored` or `synthetic`) and a recorded `solver_version`;
- `metrics`, keyed by the quantities in `benchmark.METRICS`, each with `value`,
  `unit`, exact `definition` and a nonempty `source`.

The comparison interface accepts a criterion dictionary keyed by metric, each
with a finite nonnegative `value` in percent and a nonempty `source`. It computes
signed percentage difference against the absolute screen reference and grades
its magnitude. A PASS means agreement under that supplied criterion only.
Missing or malformed files, stale identities, incompatible quantities, absent
criteria and undefined percentages produce `NOT_EVALUATED`. A valid stored value
can still be shown while its comparison is `NOT_EVALUATED`.

For the synthetic reader example, the screen reference is 100 kN and the stored
quantity is 110 kN. The signed difference is 10.0 %. A synthetic criterion of
10.0 % passes equality; 9.0 % fails. These are test inputs, not engineering
criteria. Mechanical stroke has no comparable screen and remains
`NOT_EVALUATED`, even if a stored stroke value is available.

The builder reserves `results/` beneath this report for future committed,
unchanged JSON summaries. It rejects untracked, modified and symbolic-link
summaries. No result files or agreement criteria are supplied by this change;
all result cells remain marked placeholders. Actual assembly channels must keep
their native definitions and require a separate validated comparison adapter.

## Verification

Run the benchmark tests with the repository source available:

```sh
PYTHONPATH=src python -m pytest tests/drilling_riser/test_benchmark.py -q -p no:cacheprovider --basetemp=<external-scratch-directory>
```

The tests check independent arithmetic, rejected evidence, schema and grid
coverage, generated-model equivalence, YAML validation, chart data bindings and
byte-for-byte report rebuilding. The report build writes only its final HTML
and change register. No solver, browser or network access is required.
