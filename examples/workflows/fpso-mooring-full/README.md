# FPSO Mooring Full

Runs the headless FPSO spread-mooring demo through the durable workflow contract.
The workflow reads the demo defaults from `input.yml`, calculates OCIMF
wind/current loading, adds a JONSWAP spectrum-derived wave drift term, solves
static offset, and writes per-line catenary tensions.

Run:

```bash
uv run python -m digitalmodel examples/workflows/fpso-mooring-full/input.yml
```

Expected outputs are written to `examples/workflows/fpso-mooring-full/results/`:

- `input.yml`
- `fpso_mooring_full_summary.json`
- `fpso_mooring_full_line_tensions.csv`
