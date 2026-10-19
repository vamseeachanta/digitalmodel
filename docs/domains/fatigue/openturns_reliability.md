# Constant-amplitude fatigue reliability

`digitalmodel.structural.fatigue_reliability.FatigueReliability` evaluates
failure probability and reliability index for an unbounded single-slope S–N
model. Install the optional backend with `pip install 'digitalmodel[reliability]'`.
The module imports without OpenTURNS; random failure probabilities require it.

```python
from digitalmodel.structural.fatigue_reliability import FatigueReliability


def assess(A, m, mean_stress, cov_stress, cov_intercept, design_cycles):
    model = FatigueReliability(
        A=A,
        m=m,
        stress_range=mean_stress,
        cov_stress=cov_stress,
        cov_sn=cov_intercept,
    )
    return (
        model.failure_probability(design_cycles),
        model.reliability_index(design_cycles),
    )
```

All engineering inputs are required. `A` and `stress_range` are arithmetic
means. `m` is deterministic; the intercept and stress are independent lognormal
variables. Both life arguments are cycle counts. Calendar life requires a
separately justified conversion. Stress units must match those used to define
`A`, whose units are cycles times stress raised to `m`.

The S–N equation `N = A S^(-m)` is the existing repository
[`PowerLawSNCurve` model contract](../../../src/digitalmodel/structural/fatigue/sn_curves.py).
This class accepts its equation's coefficients, rather than a curve object:
fatigue limits, cycle cutoffs and multiple slopes are outside the model.
Coefficients from a curve with those features must not be treated as a full
representation of that curve.

For each positive input `X`, the lognormal moments give
`v_X = ln(1 + CoV_X²)` and `mu_lnX = ln(E[X]) - v_X/2`.
These equations follow by inversion of the mean and coefficient of variation in
the [NIST/SEMATECH e-Handbook, section 1.3.6.6.9](https://www.itl.nist.gov/div898/handbook/eda/section3/eda3669.htm)
(online edition accessed 2026). No material coefficient is taken from that source.

Taking logarithms of the S–N equation gives the margin
`g = ln A - m ln S - ln Nd`. Independence gives
`mu_g = mu_lnA - m mu_lnS - ln Nd` and `sigma_g² = v_A + m² v_S`.
The [same handbook, section 1.3.6.6.1](https://www.itl.nist.gov/div898/handbook/eda/section3/eda3661.htm)
(online edition accessed 2026) gives normal standardization, yielding
`Pf = Phi(-mu_g/sigma_g)` and `beta = mu_g/sigma_g`.
OpenTURNS evaluates the margin CDF directly through its
[`Normal.computeCDF` API](https://openturns.github.io/openturns/1.26/user_manual/_generated/openturns.Normal.html).
This is an exact distribution calculation under these assumptions.

Failure includes equality: `N <= Nd`. When both CoVs are zero, equality has
probability one; beta is negative infinity. A deterministic surviving case has
probability zero and positive infinite beta. For a random margin, beta is
calculated directly so it remains finite when the reported probability rounds
to an endpoint.

No governing reliability target, inspection basis or material calibration is
provided. Acceptance is not evaluated. Rainflow input, variable-amplitude damage,
correlation, sampling, sensitivity and nonlinear curve support remain later work.
