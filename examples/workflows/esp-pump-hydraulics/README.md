# ESP Pump Hydraulics

Sizes an **electric submersible pump (ESP)** for a producing well from the total dynamic head (TDH) and the selected pump's per-stage curve values.

```
TDH = net_lift + friction_head + discharge_head            [m]
  net_lift       = dynamic fluid level (depth the pump lifts from)
  friction_head  = Hazen-Williams tubing loss over the pump setting depth
  discharge_head = wellhead pressure expressed as head

stages_required = ceil(TDH / head_per_stage)
brake_power     = stages × bhp_per_stage × specific_gravity   [hp]
```

Tubing friction uses the Hazen-Williams correlation `hf = 10.67·L·Q^1.852 / (C^1.852·d^4.87)` (SI). The screen **passes** when the required stages fit the pump housing (`max_stages`), the brake power is within the motor rating, and the design rate is inside the pump's recommended operating range; otherwise it reports the governing check and `screening_status: fail`.

The example sizes a 1200 m³/d deep producer: TDH ≈ 2425 m needs 405 stages against a 400-stage housing → `fail` on stage count (select a higher-head pump or larger tubing).

Run with `uv run python -m digitalmodel examples/workflows/esp-pump-hydraulics/input.yml`.

Reference: standard ESP sizing procedure (Centrilift / Schlumberger ESP handbooks); Hazen-Williams pipe-friction correlation.
