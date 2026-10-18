# Lifting Lug / Padeye Strength Screening

Screens a circular **padeye (lifting lug)** under a sling load against three governing failure modes, using AISC allowable-stress design:

- **Pin-hole bearing**: `σ_b = P / (d_pin · t)` ≤ `0.90·Fy`
- **Net-section tension** (two ligaments beside the hole): `σ_t = P / (2·t·(R − r))` ≤ `0.60·Fy`
- **Shear tear-out** to the loaded edge: `τ = P / (2·t·(R − r))` ≤ `0.40·Fy`

where `t` = main-plate thickness + 2·cheek-plate thickness (total thickness at the hole), `R` = padeye outer radius (hole centre to loaded edge), `r` = hole radius, and `P = static_load · DAF · skew_factor` is the factored sling load.

The workflow reports each check's demand, allowable, and utilisation, names the governing check, and emits a top-level `screening_status` (pass/fail). The example drives an S355 padeye to a shear-tear-out-governed failure (utilisation ≈ 1.16) with the bearing check near its limit.

The example specifies two-leg rigging without a spreader. Its SKL of 1.10 follows DNVGL-ST-E273, *2.7-3 Portable offshore units*, Edition April 2016, §3.5.5 (Lift points – in-plane loads), pp. 34–35; §3.5.4 addresses sling-length inaccuracies. The clause specifies minimum SKL of 1.10 for two/three legs and 1.33 for four matched legs (EN 13414-1 tolerances). The supplied 420 kN is already the most-loaded leg's static tension at the extreme CoG, including the sling-angle effect. SKL covers tolerance effects separately. `skew_load_factor` is required. When `num_slings` is supplied, the workflow
checks the shared minimum and calculates load with the greater of the entered
factor and that minimum. A below-minimum factor fails the screen even if all
strength checks pass. The summary records the entered and governing factors,
minimum, source and `skew_load_factor_below_minimum` flag. Supplied leg counts
are limited to integers 1–4; omit the count only when an independently assessed
explicit factor is supplied. The workflow takes the most-loaded leg tension
as input and does not solve rigging statics. [DNV rules and standards](https://www.dnv.com/rules-standards/).

The cited POU clause assumes no spreader bars and a sling angle at least 30 degrees from vertical; §1.4.3 normally limits MGW to 100 t. Rigging outside clause scope requires an independently justified explicit factor. The `SlingConfig` screen enforces these limits, rejects a below-minimum input with `pass: false`, and reports the governing minimum separately from the entered factor. Marine-operation lifts require an explicit SKL assessed against DNV-ST-N001, *Marine operations and marine warranty*, Section 16, using the governing project edition.

This remains an AISC strength screen, not a complete DNV padeye design verification. The standard's RSF equation has additional design-load factors and applicability limits.

Run with `uv run python -m digitalmodel examples/workflows/lifting-lug-design/input.yml`.

Reference: AISC ASD allowable stresses (0.60/0.40/0.90·Fy); DNV-ST-0378 / DNV 2.7-3 (offshore lifting appliances — padeye design).

The sling tension screen also requires an independently supplied extreme-CoG
`most_loaded_share` or supported 2/3-leg CoG statics before an unqualified pass.
Without either, `load_share_assumed` is true and `pass` is false. The explicit
`allow_equal_load_share: true` input accepts the equal-share assumption and is
preserved on serialization; it cannot bypass the SKL minimum. This follows the
§3.5.5 distinction between extreme-CoG PL and tolerance-only SKL.
