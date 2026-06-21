# Lifting Lug / Padeye Strength Screening

Screens a circular **padeye (lifting lug)** under a sling load against three governing failure modes, using AISC allowable-stress design:

- **Pin-hole bearing**: `σ_b = P / (d_pin · t)` ≤ `0.90·Fy`
- **Net-section tension** (two ligaments beside the hole): `σ_t = P / (2·t·(R − r))` ≤ `0.60·Fy`
- **Shear tear-out** to the loaded edge: `τ = P / (2·t·(R − r))` ≤ `0.40·Fy`

where `t` = main-plate thickness + 2·cheek-plate thickness (total thickness at the hole), `R` = padeye outer radius (hole centre to loaded edge), `r` = hole radius, and `P = static_load · DAF · skew_factor` is the factored sling load.

The workflow reports each check's demand, allowable, and utilisation, names the governing check, and emits a top-level `screening_status` (pass/fail). The example drives an S355 padeye to a shear-tear-out-governed failure (utilisation ≈ 1.16) with the bearing check near its limit.

Run with `uv run python -m digitalmodel examples/workflows/lifting-lug-design/input.yml`.

Reference: AISC ASD allowable stresses (0.60/0.40/0.90·Fy); DNV-ST-0378 / DNV 2.7-3 (offshore lifting appliances — padeye design).
