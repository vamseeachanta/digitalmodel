# Tensioned-beam mode order

`tensioned_beam_periods` returns the fundamental mode first, followed by higher
modes: periods decrease and frequencies increase. This executable example labels
each returned mode without sorting or reversing the solver results.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-26 | Add a runnable example to expose mode identity and period order. |

## Independent reference

The synthetic inputs and mesh below come from
[`test_tensioned_string_limit_matches_closed_form`](../../tests/drilling_riser/global_model/test_hand_checks.py):
length `L = 300 m`, constant tension `Te = 2.0e6 N`, mass per length
`m = 800 kg/m`, bending stiffness `EI = 1 N m²`, and `300` equal elements.
These are test inputs, not field measurements.

For an ideal string fixed at both ends, substitute
`w(x,t) = sin(n*pi*x/L) * cos(omega_n*t)` into `m*w_tt - Te*w_xx = 0`.
Then `omega_n = (n*pi/L)*sqrt(Te/m)` and
`T_n = 2*L/(n*sqrt(Te/m))`. For these inputs, wave speed is `50 m/s` and
`T_n = 12/n s`. Independently, the first five periods are
`(12.0, 6.0, 4.0, 3.0, 2.4) s`; `n = 1` is the fundamental mode.

The finite-element result includes discretisation error, small retained bending
stiffness and mass regularisation, so it need not equal the ideal-string solution
exactly. The example retains the reference test's relative tolerance `5e-3`
(`0.5%`) for every mode. It reports the maximum relative period difference with
an upward rounded bound in steps of `0.1%`; this includes all three effects and
does not isolate mesh error alone.

## Executable example

Run this block from the repository root with `src` on `PYTHONPATH` and the
module's NumPy, SciPy and Pydantic dependencies available. The documentation
smoke test executes this same block.

```python
import math

from digitalmodel.drilling_riser.global_model.hand_checks import (
    BeamSegment,
    tensioned_beam_periods,
)

# Inputs, element count and tolerance: the reference test linked above.
length_m, tension_n, mass_per_m_kg = 300.0, 2.0e6, 800.0
element_count, ei_nm2 = 300, 1.0
segments = [
    BeamSegment(length_m / element_count, ei_nm2, tension_n, tension_n, mass_per_m_kg)
    for _ in range(element_count)
]
# Independent hand calculation above: T_n = 12/n s, for n = 1 through 5.
reference_periods_s = (12.0, 6.0, 4.0, 3.0, 2.4)
periods_s = tensioned_beam_periods(segments, n_modes=len(reference_periods_s))
modes = [
    {
        "number": number,
        "label": "fundamental" if number == 1 else "higher mode",
        "period_s": period_s,
        "frequency_hz": 1.0 / period_s,
    }
    for number, period_s in enumerate(periods_s, start=1)
]
for mode, reference_s in zip(modes, reference_periods_s, strict=True):
    assert math.isclose(mode["period_s"], reference_s, rel_tol=5e-3)
    print(
        f'{mode["number"]} ({mode["label"]}): '
        f'T={mode["period_s"]:.1f} s; f={mode["frequency_hz"]:.3f} Hz'
    )

relative_errors = [
    abs(period_s / reference_s - 1.0)
    for period_s, reference_s in zip(periods_s, reference_periods_s, strict=True)
]
# Convert a relative fraction to an upward rounded bound in tenths of a percent.
error_bound_percent = math.ceil(max(relative_errors) * 1000) / 10
print(f'Maximum relative period difference: within {error_bound_percent:.1f}%')
```

The first printed row is `1 (fundamental): T=12.0 s; f=0.083 Hz`.
Display rounding does not enter the comparisons. The smoke test also asserts
strictly descending periods and strictly increasing reciprocal frequencies.
The numerical implementation and downstream mode indices are unchanged.
