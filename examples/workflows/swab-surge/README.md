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
In a partial window, the supplied limit is not validated or included in the
summary, even if it is non-finite; the status remains `warning`.
Non-finite numeric section or trip inputs, including values under extra keys
echoed into the summary, non-finite evaluated mud weights or complete-window
limits, and equal or reversed complete-window limits raise `ValueError` before
the output directory or summary is created. Finite extra values remain in the
summary as supplied.

The method uses Burkhardt (1961) clinging-constant effective velocity with
Bingham-plastic annular friction, a screening class of API RP 13D,
*Rheology and Hydraulics of Oil-well Drilling Fluids*.

Input units in `input.yml` are:

- Pipe outside and inside diameters and hole diameter: inches.
- True vertical depth (TVD): feet.
- Mud weight and pore-pressure and fracture limits: ppg (lb/gal).
- Plastic viscosity (PV): cP.
- Yield point (YP): lbf/100 ft².
- Trip speed: ft/min.
- Clinging constant: dimensionless; pipe configuration: `closed` or `open`.

The result applies to the specified trip speed with these limits:

- Steady flow only; no pressure transient or gel breaking is represented.
- Friction length is taken as TVD, so deviated wells are not represented.
- Mud properties are fixed throughout the calculation.

Successful execution and an in-window result do not qualify the method for
field use.
