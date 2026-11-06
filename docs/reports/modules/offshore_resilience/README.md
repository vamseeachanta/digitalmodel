# Offshore resilience module analysis

Draft for maintainer review.

[Read the computed module analysis](index.html).

The inputs are locally selected illustrative controls defined in
`scripts/reports/build_offshore_resilience_module_report.py`, pending maintainer
acceptance. Issue 3308 specifies deliverables, not numerical inputs. Combined
mass is checked by addition, depth midpoint by averaging and checklist states
by independent enumeration. The report cites the arithmetic textbook basis.
Constructor controls are synthetic, not a physical design basis. The completion
ratio measures passed checks, not the fraction entered. Engineering criteria
for regulatory screening, costs, sensors and lifecycle decisions remain
NOT_EVALUATED; the report identifies the missing sources and decisions.

Rebuild from the repository root with the source import path enabled:

```bash
PYTHONPATH=src uv run --no-sync python scripts/reports/build_offshore_resilience_module_report.py
```

The `--output-dir` option selects an independent output directory; by default the four
deliverables are written beside this README. No intermediate files are needed.
Inside the checkout, only the named report directory is permitted as output.
Concurrent output replacement is not supported; use an exclusive output directory.
The calculation uses the Python standard library and the source package only.

`index.html` contains the calculated results and public-interface inventory.
`changes.json` retains the revision history. `manifest.json` carries SHA-256
digests for the builder, four reviewed modules, hand and report tests, and the
three other artifacts. Its own hash is excluded to avoid circular hashing;
the rebuild test compares all four delivered files byte for byte.

The independent hand checks are in
`tests/structural/offshore_resilience/test_module_report_hand_values.py`.
The rebuild, payload, display and digest checks are in
`tests/reporting/test_offshore_resilience_module_report.py`.
Passing arithmetic cases do not establish operational approval.
