# Jumper Installation

Runs the headless deepwater rigid-jumper installation screening workflow through
the durable workflow contract. The workflow loads the bundled GTM vessel and
rigid-jumper catalogs, evaluates the five installation phases, and writes a
small Hs sweep for downstream reporting.

Run:

```bash
uv run python -m digitalmodel examples/workflows/jumper-installation/input.yml
```

Expected outputs are written to `examples/workflows/jumper-installation/results/`:

- `input.yml`
- `input_jumper_installation_summary.json`
- `input_jumper_installation_cases.csv`
