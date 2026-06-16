# Synthetic Rope Mooring Fatigue
Screens synthetic fibre-rope mooring service life across tension-tension fatigue, creep-rupture, and minimum-tension axial-compression limits.
This workflow is synthetic-only; metallic chain or wire lines should use `mooring-fatigue`.
The example keeps one polyester line healthy and drives another to creep-governed failure.
Run with `uv run python -m digitalmodel examples/workflows/synthetic-rope-mooring-fatigue/input.yml`.
