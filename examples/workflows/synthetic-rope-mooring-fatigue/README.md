# Synthetic Rope Mooring Fatigue
Screens synthetic fibre-rope mooring service life across tension-tension fatigue, creep-rupture, and minimum-tension axial-compression limits. A line passes only if all three pass.
This workflow is synthetic-only; metallic chain or wire lines should use `mooring-fatigue`.
The polyester tension-tension T-N curve is **DNV-OS-E301 (Oct 2010) Table F3**: `N = aD * R^(-m)` with `aD = 0.259`, `m = 13.46`, `R = tension range / characteristic strength (MBL)`; the fatigue safety factor `dff = 60` follows DNV-OS-E301 F702 for polyester. Creep carries a separate `creep_safety_factor`. Nylon/HMPE must supply their own validated `tn_curve` (and creep for HMPE) or the run escalates.
The example keeps one polyester line healthy and drives another to creep-governed failure.
Run with `uv run python -m digitalmodel examples/workflows/synthetic-rope-mooring-fatigue/input.yml`.
