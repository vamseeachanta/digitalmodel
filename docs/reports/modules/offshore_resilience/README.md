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
| 1 | 2026-10-25 | Replace unsupported input attribution with an existing regression case and document its scaling. |

## Basis and independent hand calculation

Numerical source: `TestModularCostComparison.test_savings_fraction` in
[test_minimum_facility.py](../../../../tests/structural/offshore_resilience/test_minimum_facility.py).
This existing executable regression specification defines conventional cost
40,000,000, minimum-facility cost 28,000,000 and avoided installation cost
4,000,000. Each is divided by 1,000,000 to define abstract inputs of 40, 28
and 4 currency units. This is a regression fixture, not an engineering cost
basis. Issue #3308 requests a worked example but supplies no numerical inputs.
The avoided cost is excluded
from the minimum-facility input and added once as an additional saving.

- Net savings = (40 - 28) + 4 = 16 currency units.
- Savings fraction = 16 / 40 = 0.4 dimensionless.

These are independent hand calculations, used as the test expectations.
The cost expressions are the regression case's definitions, not design rules.
Arithmetic reference: OpenStax, *Prealgebra 2e*, section 2.1,
[Use the Language of Algebra](https://openstax.org/books/prealgebra-2e/pages/2-1-use-the-language-of-algebra),
for addition, subtraction, division and order of operations. This reference
supplies arithmetic conventions, not cost inputs or engineering validation.
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
