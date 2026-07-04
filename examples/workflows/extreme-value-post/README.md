# Extreme-Value Post-Processing (Gumbel / Weibull)

Fits **Gumbel (Type I)** and **3-parameter Weibull** extreme-value
distributions to a set of **block maxima** and reports **return-period
extremes** (10 / 50 / 100 / 1000-yr). This is the offline, license-free demo
for the `extreme-value-post` use case (issues #961, #941, #938).

- **Gumbel return value**: `x_T = μ + σ · (−ln(−ln(1 − 1/T)))`
- **Weibull return value**: inverse CDF `weibull_min.ppf(1 − 1/T)` (3-param, `loc=0`)
- Goodness-of-fit reported via a Kolmogorov–Smirnov statistic and p-value.

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
