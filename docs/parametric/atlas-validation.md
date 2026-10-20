# Atlas validation availability

A grid supports interpolation; it does not establish interpolation accuracy.
The query confidence contract requires a measured held-out error. An absent,
null, nonnumeric, Boolean, negative or nonfinite `max_rel_error` is unavailable.
An explicit `status: unavailable` also prevents a numeric field from qualifying.
A finite nonnegative numeric error, including measured zero, remains available.
This is a metadata contract, not independent verification of the measurement.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| 1 | 2026-10-20 | Distinguish unavailable validation from measured zero and reject incomplete or nonfinite sweep grids. |

`Atlas.max_rel_error` and `local_error` return NaN when validation is unavailable.
`save` writes a null manifest error and `status: unavailable`; loading preserves
that state. Query handlers and the operability screen escalate without a value
or confidence band. Library provenance remains visible in query escalation.
`Atlas.predict` remains a geometric interpolation primitive and can be used for
holdout evaluation; an in-range prediction alone does not qualify a query answer.
The catalog displays `unavailable`, including for unvalidated libraries.

`atlas_from_summary` requires finite, unique, nonempty axis definitions and a
finite response at each unique declared Cartesian node. Equal row counts alone
are insufficient. If each row belongs to the declared product, all tuples are
unique, and their count equals the product cardinality, coverage is complete.
The bridge performs no holdout evaluation and records that reason explicitly.

## Worked example: synthetic dimensionless identity

The inputs below are invented dimensionless arithmetic data, not engineering
measurements or solver results. With knots `(1,1)` and `(3,3)`, the interpolation
weight at `x=2` is `(2-1)/(3-1)=1/2`; the interpolated answer is
`1*(1-1/2)+3*(1/2)=2`. For measured relative error `e`, the existing direct-value
band is `[v*(1-e), v*(1+e)]`. Measured `e=0` gives `[2,2]`; absent evidence gives
no band and an escalation. No acceptance limit is assumed.

```python
import pandas as pd
from digitalmodel.parametric.atlas import Atlas, Axis
from digitalmodel.parametric.query import _handle_value

atlas = Atlas(
    basename="viv_analysis", atlas_id="identity-example", physics="linear",
    response="response", axes=[Axis("x", grid=[1.0, 3.0])],
    grid=pd.DataFrame({"x": [1.0, 3.0], "response": [1.0, 3.0]}),
)
assert atlas.predict({"x": 2.0}).value == 2.0
assert _handle_value(atlas, {"x": 2.0})["reason"] == "validation unavailable"
# Synthetic measured-zero case solely to demonstrate the metadata distinction.
atlas.validation = {"max_rel_error": 0.0}
assert _handle_value(atlas, {"x": 2.0})["confidence"]["band"] == [2.0, 2.0]
```

A second independent example uses `x,y` in `{1,2,3}` and response `x+y`.
The Cartesian product contains nine distinct nodes. At `(1.5,2.5)`, equal
bilinear weights on `(1,2),(1,3),(2,2),(2,3)` give `(3+4+4+5)/4=4`.
Replacing the last node with a duplicate leaves nine rows but fails coverage.
Regression tests include that substitution, undeclared nodes, nonfinite values,
all confidence handlers, measured zero, persistence and the operability screen.

## Basis and limits

The governing repository specification is
[atlas-spec.md, Decisions 2–4](../plans/parametric-refresh/atlas-spec.md):
held-out validation artifacts, interpolation, confidence bands and escalation.
The arithmetic above is an independent hand calculation of linear and bilinear
interpolation; it adds no standards-derived coefficients or acceptance limits.
The specification remains a draft; this change implements the missing-evidence
behavior described in [issue 2118](https://github.com/vamseeachanta/digitalmodel/issues/2118).

Committed atlas payloads remain unchanged. The two operability publishing
scripts still default direct validation dictionary reads to zero and require a
follow-up. Holdout evaluation for bridge-built atlases is also outside this
change; see [issue 816](https://github.com/vamseeachanta/digitalmodel/issues/816).
