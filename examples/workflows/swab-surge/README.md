# Swab and surge pressure-window screening

The synthetic example compares the equivalent mud weight while pulling out of
the hole against the pore-pressure limit and while running in against the
fracture limit. Both comparisons use the trip speed in `input.yml`.

Run the offline workflow from the repository root.

```sh
uv run python -m digitalmodel examples/workflows/swab-surge/input.yml
```

The run writes `results/swab_surge/swab_surge_summary.json` beside the input.
Read `screening_status` and `screening_reason` together. A value of `pass`
means both mud weights are within the supplied limits, including equality.
A value of `fail` identifies a kick risk, a loss risk, or both. A missing pore
or fracture limit gives `warning` because the pressure window was not evaluated.

The result applies to the specified trip speed and the empirical steady-flow
model. Successful execution and an in-window result do not qualify the method
for field use.
