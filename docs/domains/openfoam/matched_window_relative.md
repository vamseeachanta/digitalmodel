# Matched-window relative hull resistance

Use `force_cycle_average` when the result of interest is the **absolute force of one
run**. Use `matched_window_relative` when the result of interest is the **relative
difference between two comparable runs** that still contain the slow free-surface
pressure wobble.

Completing a planned iteration budget does not establish convergence. In particular,
cycle averages can look stable while the pressure-force envelope is growing. The tool
therefore reports both input envelope verdicts and exits with status 3 instead of
reporting a relative if either envelope is rising.

## Why matched windows

The total force in a free-surface resistance run can carry a slowly decaying pressure
wobble with a period of roughly 1,000–2,600 iterations. A full-cycle average gives a
useful absolute estimate, but two independent cycle boundaries can sample different
wobble phases. In a representative comparison, the relative inferred from separate
cycle averages ranged generically from about -5% to +2.5%, even though the windowed
absolute histories had a correlation of about 0.96.

Matched windows average both histories over exactly the same iteration interval. A
window spanning at least two wobble periods suppresses the shared oscillation, while
sweeping the common window end measures the residual placement sensitivity. This is
appropriate only when both runs have the same speed, mesh family, and numerics. The
wobble must be common-mode; a correlation below 0.5 triggers a warning because the
runs may not be in a comparable state.

## Command line

```bash
python -m digitalmodel.solvers.openfoam.matched_window_relative \
  run-a/postProcessing/forces_hull/0/force.dat \
  run-b/postProcessing/forces_hull/0/force.dat \
  --component all --labels baseline,candidate --json relative.json
```

The default window is twice the mean wobble period estimated from the two pressure
force extrema histories. The tool uses the same extrema finder as
`force_cycle_average`; if either history has fewer than three extrema, it falls back
to a 2,500-iteration window. The default end is the last iteration common to both
histories. Window ends are swept over the preceding 800 iterations at a 100-iteration
step.

For each selected component, the report contains the two windowed absolute means,
the mean relative `(A / B - 1)` and its sample standard deviation, placement count,
window length, wobble periods covered, and correlation of the windowed absolutes.
It also reports the relative at the same final end for windows of 0.5, 1, 1.5, and 2
times the selected length. A stable comparison should be insensitive to these window
lengths and normally show strong positive correlation across the sweep.

Both the matched averaging window and each envelope measurement scale with the
period inferred from that history's own pressure-force extrema. This matters when
case periods differ: a fixed-width window covers unequal fractions of their cycles
and can suppress peak-to-peak swing by unequal amounts, creating a false case trend.
For absolute convergence, the companion reduction requires a non-rising envelope
and either a passing cycle gate or estimator agreement within 2%; see
[Hull-force convergence](force_convergence.md).

The command exits with status 2 if either history is shorter than the requested
window or their iteration ranges do not overlap by at least one complete window.
It exits with status 3 when either input has a rising envelope, because a relative
from a demonstrably unconverged run is not meaningful.
