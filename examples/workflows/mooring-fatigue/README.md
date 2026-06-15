# Mooring Fatigue

Estimates metallic mooring-line fatigue damage for chain and steel-wire lines by converting deterministic tension-range bins to stress ranges, applying the existing DNV S-N/Miner fatigue helpers, and reporting line damage, fatigue life, and DFF margin. Synthetic ropes are out of scope because their fatigue behavior uses different damage models.

Run:

```bash
uv run python -m digitalmodel examples/workflows/mooring-fatigue/input.yml
```

Expected outputs are written to `examples/workflows/mooring-fatigue/results/`, including the resolved result configuration `input.yml`, per-bin damage CSV, and per-line summary CSV.
