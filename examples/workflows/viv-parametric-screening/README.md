# VIV Parametric Screening (synthetic pilot)

Analytical DNV-RP-C205 §9 / DNV-RP-F105 §4-5 vortex-induced-vibration screening for a
synthetic tubular member: reduced velocity per mode, cross-flow / in-line lock-in
flags, an A/D response-amplitude estimate (DNV-RP-C205 Fig 9-3) and a fatigue-proxy
screening surrogate. All inputs are SYNTHETIC (no client or geometry data); only public
DNV citation strings enter the outputs. The pure calc needs no OrcFxAPI.

Run:

```bash
uv run python -m digitalmodel examples/workflows/viv-parametric-screening/input.yml
```

Expected outputs are written to `examples/workflows/viv-parametric-screening/results/`:
`results.json` (native engineering schema: screening, input, beam, per-mode `modes`,
DNV `citations`; `meta.generated_at` omitted so it is byte-stable) and `cases.csv` (the
per-mode table).

The base case is the exact-replay case for the #1505 Hugging Face pilot
(`D=0.2032 m, V=1.0 m/s, span=60 m` -> cross-flow lock-in at mode 2, A/D ~= 0.98). The
`>=3` parameter variations are produced by EXTERNAL
`run_workflow("viv-parametric-screening", params=variant)` calls -- see
`scripts/pilots/run_viv_pilot.py`.
