# Mooring Fatigue

Estimates metallic mooring-line fatigue damage for chain and steel-wire lines by converting deterministic tension-range bins to stress ranges, applying the existing DNV S-N/Miner fatigue helpers, and reporting line damage, fatigue life, and DFF margin. Synthetic ropes are out of scope because their fatigue behavior uses different damage models.

Run:

```bash
uv run python -m digitalmodel examples/workflows/mooring-fatigue/input.yml
```

Expected outputs are written to `examples/workflows/mooring-fatigue/results/`, including the resolved result configuration `input.yml`, per-bin damage CSV, and per-line summary CSV.

Each case must supply finite positive `design_life_years` and `dff`, plus an
explicit S-N curve and environment. The packaged configuration leaves these
inputs unset. Results record that design controls came from case inputs.

Each line must declare its material and a `sn_curve_basis` mapping containing
`material`, `curve`, `environment`, `detail`, `reference`, and `protection_basis`.
The material and selected curve must match those declarations. A generic S-N
detail category alone does not establish applicability to chain or wire.
The examples are assumed, based on previous experience, and illustrate screening
only. Their declarations do not establish component qualification; a real
assessment requires a justified detail selection and protection basis.

Supply exactly one of `stress_range_bins`, `tension_range_bins`,
`stress_history_MPa`, or `tension_history_kN`. Supply one alias per quantity,
including cycle counts, area, and repetition counts. Bin counts represent the
complete design-life exposure. Histories require finite positive `n_repeats`
(or `n_histories`), explicitly specifying occurrences over the design life;
there is no duration conversion or implicit single occurrence. Repetitions and
their basis are retained in the line summary. Tension inputs require finite
positive area in square millimetres.

Missing, ambiguous, non-finite, non-positive, or numerically unassessable demand
raises `ValueError` before output files are written. Constant histories cannot
establish an assessment. A line passes only when its fatigue life is at least
`design_life_years * dff`; the case passes only when every line passes.
