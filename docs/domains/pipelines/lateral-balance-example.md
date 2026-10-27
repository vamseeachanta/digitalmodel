# Lateral stability force balance example

This executable example checks the existing lateral stability API against a
published static-friction example. Each execution includes an accepted case,
a rejected case and a zero-load limit. It covers part of issue #242. The source
describes a crate on a floor; this arithmetic check establishes neither pipeline
design inputs nor DNV qualification.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-27 | Added synthetic force balances and independent tests to illustrate the existing lateral API. |
| 1 | 2026-10-27 | Replaced assumed inputs with cited textbook values and added the acceptance criterion to the output. |

## Force balance basis

The independent source is OpenStax, *University Physics Volume 1*, 2016 edition,
May 2018 release, section 6.2, equation 6.1 and Example 6.10, cases (c) and (d).
The [archived textbook](https://d3bxy9euw4e147.cloudfront.net/oscms-prodcms/media/documents/UniversityPhysicsVolume1-LR.pdf)
is published under CC BY 4.0; its copyright page identifies that release and
licence. Download for free at
[OpenStax](https://openstax.org/details/books/university-physics-volume-1).
Only numerical inputs and independently expressed arithmetic are used here;
the textbook and its figures are not reproduced.

Equation 6.1 limits static friction to mu times normal force. The source's
vertical equilibrium gives normal force equal to weight, with no applied lift.
The existing
[`lateral_stability_check` API](../../../src/digitalmodel/subsea/on_bottom_stability/simplified.py)
uses the following algebraic capacity ratio, including caller-supplied lift
and demand factoring. The source comparison sets L = 0 and gamma = 1 explicitly.
For nonnegative horizontal load magnitude H, upward lift L, submerged weight W,
friction coefficient mu and explicit safety factor gamma:

```text
Effective normal force = W - L
Static friction capacity R = mu * (W - L)
Demand D = gamma * H
Utilisation U = D / R
Accepted by this lateral balance when U <= 1.000
```

Resistance and demand are forces per unit length in N/m. Friction, safety
factor and utilisation are dimensionless. Passive soil resistance is zero by
construction. Capacity is the maximum static friction, not the friction actually
mobilized below that limit. The unity boundary is the existing API criterion.
No standard text, tables or coefficient values are used.

## Sourced inputs and unit mapping

The source gives mass 20.0 kg, gravitational acceleration 9.80 m/s², static
friction coefficient 0.700 and horizontal forces 120.0 N and 180.0 N.
Weight is therefore 196.0 N. The second force is 1.5 times the first, matching
source case (d). Source forces are divided by one metre to exercise the N/m
API without changing the capacity ratio. This is a unit normalization, not a
physical length from the source or a submerged-weight calculation. The source
coefficient describes the crate-floor interface and is not a pipe-soil value.
The example passes all five inputs explicitly on every call.

| Input | Accepted | Rejected | Zero load | Basis |
| --- | ---: | ---: | ---: | --- |
| API weight W (N/m) | 196.0 | 196.0 | 196.0 | Source weight: 20.0 × 9.80, divided by one metre |
| Horizontal load H (N/m) | 120.0 | 180.0 | 0.0 | Source cases (c)/(d), divided by one metre; zero-load extension |
| Lift L (N/m) | 0.0 | 0.0 | 0.0 | No applied lift in the source free-body diagram |
| Friction mu | 0.700 | 0.700 | 0.700 | Source static coefficient, not its kinetic coefficient |
| Safety factor gamma | 1.0 | 1.0 | 1.0 | Unfactored textbook balance; no design factor is selected |

Table 1. Source inputs and explicit normalization for the existing API.

## Hand calculation

For all three cases, R = 0.700 × (196.0 − 0.0) = 137.2 N/m.
The source rounds this capacity to 137 N; the calculation retains the unrounded
product. Only H changes between the accepted and rejected cases:

| Case | Demand D (N/m) | Capacity R (N/m) | Utilisation D/R | Criterion U ≤ 1.000 |
| --- | ---: | ---: | ---: | --- |
| Accepted | 1.0 × 120.0 = 120.0 | 137.2 | 0.875 | Accepted |
| Rejected | 1.0 × 180.0 = 180.0 | 137.2 | 1.312 | Rejected |
| Zero load | 1.0 × 0.0 = 0.0 | 137.2 | 0.000 | Accepted within applicability |

Table 2. Independently calculated demands and static capacity ratios.

The boundary is H = R/gamma = 137.2/1.0 = 137.2 N/m, giving U = 1.000.
Verdicts use unrounded ratios; the displayed ratios have three decimal places.
Tests assert hand-worked numerical literals with tolerances and check equality
at the boundary. The zero-load case and exact-boundary probe are algebraic
extensions of the source, not additional published validation cases.

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
negated table inputs, lift equal to W or 2W = 392.0 N/m, a sub-unity factor
of 0.5, and nonfinite values. Those are invalid-input probes, not design constants.

## Run and check

From the repository root, with the source tree on the import path:

```bash
PYTHONPATH=src uv run --no-project python examples/lateral_balance.py
PYTHONPATH=src uv run --no-project --with pytest python -m pytest tests/test_on_bottom_stability.py tests/subsea/on_bottom_stability/test_lateral_balance_example.py -q -rA -p no:cacheprovider
```

The script prints JSON containing the input basis, numerical acceptance criterion
and its source, applicability statement, explicit inputs, capacity, demand,
utilisation and verdict. The cited inputs are embedded; no network access,
licensed tables or solver is required to run the example.

## Remaining issue scope

The example does not derive wave or current loads, select friction or safety
factors, evaluate displacement or cyclic soil response, assess separate vertical
stability, or qualify the implementation against DNV RP F109 (2007). Those
require an established design basis, edition-specific method and applicability
review, and independently sourced validation cases. Use the rejected case to
identify a demand exceeding the calculated static friction capacity; any design change
requires reassessing loads, contact, weight, soil assumptions and the applicable
method before making an engineering acceptance decision.
