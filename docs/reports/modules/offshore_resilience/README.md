# Offshore resilience cost arithmetic worked example

## Summary

This example calls `ModularCostComparison` with synthetic inputs and renders
the cost arithmetic requested in issue #3308. The inputs are not market
estimates. Scope is `NOT_EVALUATED` for platform classification, installed cost,
installation feasibility and structural adequacy.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-25 | Add a reproducible synthetic cost example for issue #3308. |

## Basis and independent hand calculation

Source: issue #3308, synthetic worked-example specification. The conventional
cost is 100 currency units, minimum-facility cost is 70 currency units, and
avoided installation cost is 10 currency units. The avoided cost is excluded
from the minimum-facility input and added once as an additional saving.

- Net savings = (100 - 70) + 10 = 40 currency units.
- Savings fraction = 40 / 100 = 0.4 dimensionless.

These are independent hand calculations, used as the test expectations.
The builder obtains its results from `net_savings_usd` and `savings_fraction`
in [minimum_facility.py](../../../../src/digitalmodel/structural/offshore_resilience/minimum_facility.py).
Despite the API's `_usd` field names, the example uses abstract currency units;
there is no monetary calibration or currency conversion. The required
`installation_weather_window_days` argument is set to zero days as an unused
placeholder chosen for this example. Neither cost property uses it; no
installation duration is assessed.

## Reproduce

Run from the repository root with a writable output directory:

```bash
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src uv run --no-project --with pytest python \
  docs/reports/modules/offshore_resilience/build_worked_example.py \
  --output-dir "$TMPDIR/worked-example"
```

The builder writes only `worked_example.html` inside the selected directory.
It has no import-time execution, external input data, runtime network requests
or licensed software dependency. Once Python and pytest are available locally,
the command can run offline. The same inputs produce identical content across
rebuilds and output locations; the revision date is fixed metadata, rather than
the build time. Generated HTML is not committed or added to a shared index.

Run the checks with:

```bash
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src uv run --no-project --with pytest python \
  -m pytest tests/structural/offshore_resilience -q -rA -p no:cacheprovider
```

Tests run the builder in subprocesses from temporary directories, read visible
rendered values and units, compare them with the hand sum, compare rebuild
bytes, check import side effects and check that Git does not ignore the
delivered files.

## Limits and way forward

No classification rules are invoked. The scenario does not establish installed
cost or structural adequacy. It provides no full-module qualification or parent
issue completion claim. A full report, shared indexes, a documented cost basis
and separate engineering assessments remain follow-up work.
