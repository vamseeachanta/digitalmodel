# VIV Analysis

Computes natural frequency, vortex-shedding frequency, and safety factors for a single free-spanning X65 tubular member.

Run:

```bash
uv run python -m digitalmodel examples/workflows/viv-analysis/input.yml
```

Expected outputs are written to `examples/workflows/viv-analysis/results/`, including natural-frequency, shedding-frequency, safety-factor CSVs and the resolved result configuration `input.yml`.
