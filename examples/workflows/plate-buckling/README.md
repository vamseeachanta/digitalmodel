# Plate Buckling Workflow

Computes DNV-RP-C201 usage factors for a representative stiffened deck plate under combined longitudinal, transverse, and shear stress.

Run:

```bash
uv run python -m digitalmodel examples/workflows/plate-buckling/input.yml
```

Expected outputs:

- `examples/workflows/plate-buckling/results/input.yml`
- Returned `plate_buckling` result entries with serviceability, ultimate, and DNV-RP-C201 usage factors.
