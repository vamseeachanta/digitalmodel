# Extreme-Value Post-Processing (Gumbel / Weibull)

Fits **Gumbel (Type I)** and **3-parameter Weibull** extreme-value
distributions to a set of **block maxima** and reports **return-period
extremes** (10 / 50 / 100 / 1000-yr). This is the offline, license-free demo
for the `extreme-value-post` use case (issues #961, #941, #938).

- **Gumbel return value**: `x_T = μ + σ · (−ln(−ln(1 − 1/T)))`
- **Weibull return value**: inverse CDF `weibull_min.ppf(1 − 1/T)` with shape,
  location and scale fitted from the maxima.
- Goodness-of-fit reported via a Kolmogorov–Smirnov statistic and p-value.

Weibull fitting requires at least 10 finite, nonconstant maxima; 50 or more
are recommended because small samples often yield a degenerate fit. A fit
raises `ValueError` before computing return values if its shape is below 1
or its location is within one millionth of the sample range of the minimum.
Increasing the sample count alone does not establish fit reliability. This
example has 30 illustrative block maxima; its fitted values are a demonstration.

Block maxima are the inputs (one maximum per block — e.g. per storm-year of an
OrcaFlex dynamic mooring/riser campaign). How they were produced is out of
scope here; the calculation is pure `numpy` + `scipy` over a numeric array.

## Fully offline / license-free

This example uses **only** `digitalmodel.orcaflex.postprocessor.fit_gumbel` /
`fit_weibull` over a committed synthetic CSV
(`data/block_maxima.csv`). There is **NO** `OrcFxAPI`, **NO** OrcaFlex `.sim`,
and **NO** license requirement. The synthetic data is illustrative — not from
any real vessel or client model.

## Run

```bash
uv run python examples/workflows/extreme-value-post/run.py
```

This prints the fitted parameters and return-period extremes for each
distribution and writes `results/extreme_value_post/extreme_value_summary.json`.

Each return-value line also gives the probability of at least one exceedance
over `design_life` years (25 by default in `input.yml`). Assuming independent
years, `encounter_probability(T, N)` evaluates `1 - (1 - 1/T)^N`: the annual
exceedance probability is `1/T`. A 100-year return period gives a 22.2 % chance
over 25 years; a 10,000-year return period gives about 0.25 % over that life.
A return period does not schedule events. Design extremes come from the fitted
statistics rather than elapsed calendar time.

The helpers are available from `digitalmodel.orcaflex.postprocessor`.
`return_period_for_probability(p, N)` calculates the required return period for
a target lifetime probability. Inputs must be finite: `T >= 1`, `N > 0` and
`0 < p <= 1`. A zero target probability requires an infinite return period
and is rejected. Both helpers raise `ValueError` for values outside these ranges.
The inverse returns infinity when the required period exceeds floating-point range.

## Invocation finding / router follow-up

The postprocessor is a **library API** (`fit_gumbel` / `fit_weibull`), not a
workflow-router basename of its own. The registry entry `extreme-value-post`
currently maps to basename `orcaflex_post_process`, which the engine routes
through the OrcaFlex path (`requires-license`) — so it **cannot** drive this
offline demo today. Hence this example ships a small runnable `run.py` plus an
`input.yml` describing the inputs.

**Follow-up (ref #941 / #938):** wire a dedicated *offline* router basename
(e.g. `extreme_value_post`) in `digitalmodel.engine` that loads block maxima
from the `input.yml` `extreme_value_post:` block and calls the postprocessor
directly, so the use case becomes dispatchable via
`uv run python -m digitalmodel <input.yml>` without a license. The registry
flip (`src/digitalmodel/usecase_registry/*`) is handled separately.

References: Gumbel (1958); DNV-RP-C205 Sec. 3.4; API RP 2SK.
