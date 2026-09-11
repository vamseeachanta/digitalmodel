# Hull-force convergence

Reaching an iteration budget is not evidence of convergence. A free-surface
pressure-force oscillation can remain at the budget boundary, and its amplitude can
even be increasing while cycle means and asymptote estimators appear acceptable.
Reduction therefore measures the oscillation envelope as well as its mean.

`force_cycle_average` estimates each case's wobble period as twice the median spacing
of its pressure-force extrema. It smooths pressure force over approximately one eighth
of that period and measures peak-to-peak range in consecutive two-period windows at
the end of the history. The windows must scale with the period: a fixed iteration
width covers different fractions of a cycle in cases with different periods and can
systematically understate their swings by different amounts.

The envelope is `decaying` when the newest range is below 80% of the preceding range,
`rising` when it exceeds 125%, and `flat` otherwise. At least two complete, adequately
sampled windows are required; without them the result is indeterminate.

A run is reported as settled only when:

1. the envelope is `decaying`; and
2. either the existing cycle-power gate passes or the Aitken and damped-cosine total
   force estimators agree within 2%.

A flat, rising, or indeterminate envelope is not settled. The existing cycle-power
and estimator output remains available separately so callers can diagnose which part
of the three-part decision prevented settling.

## Settled is not the same as precise

The settling verdict describes whether the force history is approaching a steady
state. It does not quantify how well the mean of the remaining oscillation is known.
Conversely, a bounded, sustained oscillation can have a precisely determined mean
without approaching a steady state.

The reported central value is the plain mean over the final four complete wobble
periods. Its standard error is calculated from half-period block means so that the
oscillation's autocorrelation is retained. A force shall be quoted as the central
value plus or minus its standard error, with the settling verdict carried as separate
metadata. Use the uncertainty to state the precision of the mean and the settling
verdict to state whether a steady state is being approached. Neither quantity
substitutes for the other.
