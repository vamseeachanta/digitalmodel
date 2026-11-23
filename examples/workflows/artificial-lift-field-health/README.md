# Artificial Lift Field Health

Runs a headless field-wide artificial-lift health screen through the durable
workflow contract. The workflow evaluates three deterministic synthetic
dynacards with the package dynacard solver and writes a per-well status table
plus a field summary.

Run:

```bash
uv run python -m digitalmodel examples/workflows/artificial-lift-field-health/input.yml
```

Expected outputs are written to
`examples/workflows/artificial-lift-field-health/results/`:

- `input.yml`
- `input_field_health_summary.json`
- `input_field_health_wells.csv`
