# Two-parameter spectrum default frequency grid

The direct `WaveSpectra.pierson_moskowitz` call now uses an upper frequency
of ten times the peak frequency when `freq_max` is omitted or `None`.
Explicit bounds retain the existing uniform grid. This numerical policy reduces
the omitted second moment; it does not establish compliance with a design standard.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-19 | Derive a peak-relative cutoff and independent regression moments to address the truncated tail. |
| 2 | 2026-10-19 | Preserve the fatigue workflow grid, document reduced peak resolution and confirm source equation references to bound the effect of the automatic cutoff. |

## Sources and independent calculation

[*Offshore Hydromechanics*, first edition, 2001](https://ocw.tudelft.nl/courses/offshore-hydromechanics/),
section 5.4.3, equations (5.123)-(5.124), describes the modified two-parameter
spectrum. Equations (5.115)-(5.118) define spectral moments, significant
height and zero-crossing period. The [public spectrum definition](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Waves%2CWavespectra.htm),
section ISSC, also states the height-and-peak-frequency form. These sources were
accessed on 2026-10-19; no licensed standard was used to obtain coefficients.

The open textbook's [Part 1](https://ocw.tudelft.nl/wp-content/uploads/Part_1.pdf)
was inspected at printed pages 5-41, 5-42 and 5-44 to confirm these equation
references. Its period relations are rounded; the peak-frequency expression
and exact moments below are independent derivations.

Write the spectrum as

```text
S(w) = A w^(-5) exp(-B/w^4).
```

Setting its derivative to zero at `wp = 2 pi / Tp` gives
`B = (5/4) wp^4`. Substituting `u = B/w^4` into the moment integral gives

```text
m_n = (A/4) B^((n-4)/4) Gamma(1-n/4), n < 4.
```

The height relation gives `m0 = Hs^2/16`, hence `A = Hs^2 B/4` and

```text
m_n = (Hs^2/16) B^(n/4) Gamma(1-n/4)
Tz = 2 pi sqrt(m0/m2) = Tp / (5 pi/4)^(1/4).
```

These are hand-derived expectations, independent of the spectrum generator.
The existing normalization to discrete `m0 = Hs^2/16` remains in place.

## Cutoff

Since `exp(-B/w^4) <= 1`, the omitted moment above `R wp`, divided by the
exact infinite-domain moment, is bounded by

```text
q_n <= (5/4)^(1-n/4) / ((1-n/4) Gamma(1-n/4) R^(4-n)), n < 4.
```

Selecting `R = 10` gives an upper bound below `1.3 %` for the missing second
moment and below `0.7 %` for the zero-crossing period bias, since
`Tz_truncated/Tz <= 1/sqrt(1-q_2)`. Normalizing the truncated spectrum
does not change this period ratio. The low-frequency omission at the retained
lower bound is exponentially small for the regression periods considered here.
The cutoff and test budgets are numerical choices derived here, rather than
empirical coefficients or standard acceptance criteria.

The regression uses manufactured `Hs = 4 m` so that `m0 = 1 m^2`, and peak
periods `5, 6, 10 s` from issue #2613, plus a manufactured `20 s` case. At the
existing default sample count, moment and period budgets of `2.0 %` and
`1.0 %` round the tail bounds upward to leave a quadrature allowance.
They test this numerical change only. The tests also check caller-supplied
bounds and reject invalid inputs to the automatic grid.

## Known limits and later slices

- The cutoff is finite and does not eliminate the entire moment tail.
- The default point count and lower frequency remain unchanged. Extreme periods
  or coarse caller-specified grids need a separate convergence assessment.
- With 100 points the new spacing is approximately `0.1 wp`. At `Tp = 5 s`
  it is `10.1 %` of the peak frequency, versus `1.6 %` previously. Resolving a
  narrow-band response can therefore require more points or explicit bounds,
  even when the zeroth-to-second-moment budgets pass.
- The spectral fatigue workflow explicitly retains its former `0.02-2.0 rad/s`
  grid with 100 points. Its clamped stress RAO, fourth moment and default Dirlik
  damage therefore retain their pre-cutoff-change values. Selecting a grid from
  the RAO bandwidth and assessing tail convergence remain separate work.
- JONSWAP, Bretschneider and ISSC defaults are unchanged. Parameter objects
  still pass their explicit bounds to `generate_spectrum`.
- Energy period, wave power, other spectrum defaults and the caller audit remain
  later slices. DNV-RP-C205 compliance has not been evaluated.
- The fourth moment diverges for this ideal infinite spectrum; finite-grid
  fourth moments and spectral width remain cutoff-dependent.
