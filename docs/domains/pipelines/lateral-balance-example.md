# Lateral stability force balance example

This executable example illustrates the existing lateral stability API with
synthetic assumptions. Each execution includes an accepted case, a rejected
case and a zero-load limit. It covers part of issue #242 and does not establish
complete DNV qualification or validate a design.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-27 | Added synthetic force balances and independent tests to illustrate the existing lateral API. |

## Force balance basis

The equation source is the repository's
[`lateral_stability_check` documentation](../../../src/digitalmodel/subsea/on_bottom_stability/simplified.py).
For nonnegative horizontal load magnitude H, upward lift L, submerged weight W,
friction coefficient mu and explicit safety factor gamma:

```text
Effective normal force = W - L
Resistance R = mu * (W - L)
Demand D = gamma * H
Utilisation U = D / R
Accepted by this lateral balance when U <= 1.0
```

Resistance and demand are forces per unit length in N/m. Friction, safety
factor and utilisation are dimensionless. Passive soil resistance is zero by
construction. The unity boundary is the existing API criterion and is unchanged.
This example uses the API's lift-reduced friction balance; it does not substitute
the separate absolute-stability method. No standard text, tables or coefficient
values were used to construct the example.

## Synthetic inputs

Every input value in this table is a synthetic assumption chosen here for hand
arithmetic, rather than a recommended design input or a value from a standard.
The example passes all five inputs explicitly, including friction and safety
factor, on every call.

| Input | Accepted | Rejected | Zero load | Basis |
| --- | ---: | ---: | ---: | --- |
| Submerged weight W (N/m) | 1200.0 | 1200.0 | 1200.0 | Synthetic positive weight assumption |
| Horizontal load H (N/m) | 400.0 | 600.0 | 0.0 | Synthetic load change; zero denotes no load |
| Lift L (N/m) | 200.0 | 200.0 | 0.0 | Synthetic upward load assumption; zero denotes no load |
| Friction mu | 0.6 | 0.6 | 0.6 | Synthetic friction assumption |
| Safety factor gamma | 1.2 | 1.2 | 1.2 | Synthetic demand multiplier assumption |

## Hand calculation

For the accepted and rejected cases, R = 0.6 × (1200.0 − 200.0) = 600.0 N/m.
Only H changes between these cases:

| Case | Demand D (N/m) | Resistance R (N/m) | Utilisation D/R | Lateral status |
| --- | ---: | ---: | ---: | --- |
| Accepted | 1.2 × 400.0 = 480.0 | 600.0 | 0.8 | Accepted |
| Rejected | 1.2 × 600.0 = 720.0 | 600.0 | 1.2 | Rejected |
| Zero load | 1.2 × 0.0 = 0.0 | 0.6 × 1200.0 = 720.0 | 0.0 | Accepted within applicability |

The exact boundary is H = R/gamma = 600.0/1.2 = 500.0 N/m, giving U = 1.0.
The tests independently compute these equations and assert the API's numerical
utilisation and verdict, including equality at the boundary.

## Applicability and input requirements

This is an illustrative static lateral balance, not DNV qualification. All inputs
must be finite and explicitly supplied. W and mu must be positive; H and L must
be nonnegative magnitudes; L must be less than W to retain contact and positive
friction resistance. Gamma must be at least unity for this example so demand is
not reduced. These are example input requirements, not a change to library
thresholds or a claim that the inputs meet a standard's selection requirements.

Zero means absence of load; it does not mean unknown load. With H = L = 0,
utilisation is zero only within those same requirements. A zero horizontal
demand cannot establish applicability when weight, friction, lift or safety
factor is invalid or missing. The tests probe those requirements with zero,
negated table inputs, lift equal to W or W + L = 1400.0 N/m, a sub-unity factor
of 0.8, and nonfinite values. Those are synthetic rejection probes, not design
constants.

## Run and check

From the repository root, with the source tree on the import path:

```bash
PYTHONPATH=src uv run --no-project python examples/lateral_balance.py
PYTHONPATH=src uv run --no-project --with pytest python -m pytest tests/test_on_bottom_stability.py tests/subsea/on_bottom_stability/test_lateral_balance_example.py -q -rA -p no:cacheprovider
```

The script prints JSON containing the input basis, applicability statement,
explicit inputs, resistance, demand, utilisation and verdict for each case.
No external data, licensed tables or solver is required.

## Remaining issue scope

The example does not derive wave or current loads, select friction or safety
factors, evaluate displacement or cyclic soil response, assess separate vertical
stability, or qualify the implementation against DNV RP F109 (2007). Those
require an established design basis, edition-specific method and applicability
review, and independently sourced validation cases. Use the rejected case to
identify a demand exceeding the assumed friction resistance; any design change
requires reassessing loads, contact, weight, soil assumptions and the applicable
method before making an engineering acceptance decision.
