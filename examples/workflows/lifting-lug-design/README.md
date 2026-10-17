# Lifting Lug / Padeye Strength Screening

Screens a circular **padeye (lifting lug)** under a sling load against three governing failure modes, using AISC allowable-stress design:

- **Pin-hole bearing**: `σ_b = P / (d_pin · t)` ≤ `0.90·Fy`
- **Net-section tension** (two ligaments beside the hole): `σ_t = P / (2·t·(R − r))` ≤ `0.60·Fy`
- **Shear tear-out** to the loaded edge: `τ = P / (2·t·(R − r))` ≤ `0.40·Fy`

where `t` = main-plate thickness + 2·cheek-plate thickness (total thickness at the hole), `R` = padeye outer radius (hole centre to loaded edge), `r` = hole radius, and `P = static_load · DAF · skew_factor` is the factored sling load.

The workflow reports each check's demand, allowable, and utilisation, names the governing check, and emits a top-level `screening_status` (pass/fail). The example drives an S355 padeye to a shear-tear-out-governed failure (utilisation ≈ 1.16) with the bearing check near its limit.

The example specifies two-leg rigging without a spreader. Its SKL of 1.10 follows DNVGL-ST-E273, *2.7-3 Portable offshore units*, Edition April 2016, §3.5.5 (Lift points – in-plane loads), pp. 34–35; §3.5.4 addresses sling-length inaccuracies. The clause specifies minimum SKL of 1.10 for two/three legs and 1.33 for four matched legs (EN 13414-1 tolerances). The supplied 420 kN is already the most-loaded leg's static tension at the extreme CoG, including the sling-angle effect. SKL covers tolerance effects separately. Leg count and source are recorded in the JSON summary as input metadata; this workflow takes a supplied leg tension and applies the explicit SKL, rather than solving rigging statics. [DNV rules and standards](https://www.dnv.com/rules-standards/).

The cited POU clause assumes no spreader bars and a sling angle at least 30 degrees from vertical; §1.4.3 normally limits MGW to 100 t. Rigging outside clause scope requires an independently justified explicit factor. The `SlingConfig` screen enforces these limits, rejects a below-minimum input with `pass: false`, and reports the governing minimum separately from the entered factor. Marine-operation lifts require an explicit SKL assessed against DNV-ST-N001, *Marine operations and marine warranty*, Section 16, using the governing project edition.

This remains an AISC strength screen, not a complete DNV padeye design verification. The standard's RSF equation has additional design-load factors and applicability limits.

Run with `uv run python -m digitalmodel examples/workflows/lifting-lug-design/input.yml`.

Reference: AISC ASD allowable stresses (0.60/0.40/0.90·Fy); DNV-ST-0378 / DNV 2.7-3 (offshore lifting appliances — padeye design).
