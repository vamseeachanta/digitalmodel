---
standard: DNVGL-RP-B401:2017
edition: "2017"
structure_type: subsea_riser_base_structure
source_type: abstracted_client_calculation
discipline: cathodic_protection
---

# Subsea Riser Base Structure — DNVGL-RP-B401:2017 CP Design

## Source
Standard: DNVGL-RP-B401:2017
Structure type: Production Riser Base Structure (RBS) — 4 units
Scope: Subsea structures supporting production flexible riser terminations
Design life: 27 years (25 yr operational + 2 yr wet storage)
Water depth: 1710–1900 m

## Scope

4 production Riser Base Structures (RBS). Each RBS group includes the main structure,
yoke frame, piping (insulated), electric connectors (mixed insulated/bare), and a flowline
jumper (insulated). Anodes cover seawater-exposed structural parts only; no anodes on piping.

Note: Surface areas and totals are given per-group (all 4 RBS combined) unless stated.

## Environment

| Parameter | Value | Unit |
|-----------|-------|------|
| Water depth range | 1710–1900 | m |
| Seawater resistivity | 0.31 | Ω·m |
| Sediment resistivity | 1.00 | Ω·m |
| Min protection potential (seawater) | −0.800 | V vs Ag/AgCl |
| Min protection potential (sediment) | −0.900 | V vs Ag/AgCl |

## Design Life

| Item | Design Life |
|------|-------------|
| Operational | 25 years |
| Wet storage (pre-installation) | 2 years |
| Total | 27 years |

## Coating Systems

| Zone / Component | Coating System | Category |
|-----------------|----------------|----------|
| Main structure | Paint + primer (CAT III equivalent) | CAT III |
| Yoke frame | Paint + primer (CAT III equivalent) | CAT III |
| Piping on structure | 5LPP insulation coating | Insulation |
| Flowline jumper | Insulation coating | Insulation |
| Electric connector ends | Part bare, part insulated | Mixed |

## Coating Breakdown Factors (27-year total)

Per DNVGL-RP-B401:2017 Table 6-5 (25 yr values interpolated to 27 yr):

| Coating | Initial | Mean (27 yr) | Final (27 yr) |
|---------|---------|-------------|--------------|
| CAT III (Paint/Primer) | 0.020 | 0.128 | 0.236 |
| Insulation (5LPP / similar) | 0.0003 | 0.000435 | 0.000570 |
| Bare | 1.000 | 1.000 | 1.000 |

Note: 27-yr CBF values are interpolated from Table 6-5 using the DNV formula CBF = a + b×t² where applicable.
Paint III (27 yr): mean 0.128, final 0.236 (interpolated).

## Current Density (Structures >300 m water depth)

Per DNVGL-RP-B401:2017 Table 6-3:

| Condition | Initial (A/m²) | Mean (A/m²) | Final (A/m²) |
|-----------|---------------|-------------|-------------|
| Seawater exposed (>300 m) | 0.220 | 0.110 | 0.170 |
| Buried / sediment | 0.020 | 0.020 | 0.020 |

Design contingency: +10% surface area applied to all structural surfaces.

## Surface Areas per RBS (Structural Components)

### Per RBS Group (×4 combined), With 10% Contingency Applied

| Component | Coating | Net Area (m²) | +10% Area (m²) |
|-----------|---------|--------------|----------------|
| Yoke frame | CAT III | 32.4 × 4 = 129.6 | 142.6 |
| Main structure | CAT III | 60.8 × 4 = 243.2 | 267.5 |
| Piping (insulated) | Insulation | 2.9 × 4 = 11.6 | 12.8 |
| Connectors (bare) | Bare | (partial) | (included in below) |
| Connectors (insulated) | Insulation | (partial) | (included in piping above) |
| Flowline jumper | Insulation | 40.5 × 4 = 162.0 | 178.2 |

Note: Per-RBS surface areas: Yoke 32.4 m², Structure 60.8 m², Piping insulated 2.9 m², Jumper insulated 40.5 m².

## Anode Properties

### SO-90 — Stand-Off Anode (Main Structure)

| Parameter | Value | Unit |
|-----------|-------|------|
| Type | Stand-off | — |
| Dimensions (L × W × H) | 1080 × 189 × 180 | mm |
| Core | Pipe, 60.3 mm OD | — |
| Net mass per anode | 91.44 | kg |
| Electrochemical capacity | 2000 | A·h/kg |
| Closed-circuit potential | −1.05 | V vs Ag/AgCl |
| Utilisation factor | 0.90 | — |
| Anode resistance — initial | 0.119 | Ω |
| Anode resistance — final | 0.175 | Ω |
| Individual anode current — initial | 2.100 | A |
| Individual anode current — final | 1.430 | A |

### FM-65 — Long Flush-Mounted Anode (Main Structure and Jumper)

| Parameter | Value | Unit |
|-----------|-------|------|
| Type | Long flush-mounted | — |
| Dimensions (L × W × H) | 950 × 186 × 136 | mm |
| Core | Flat bar, 60 mm | — |
| Net mass per anode | 64.04 | kg |
| Electrochemical capacity | 2000 | A·h/kg |
| Closed-circuit potential | −1.05 | V vs Ag/AgCl |
| Utilisation factor | 0.85 | — |
| Anode resistance — initial | 0.273 | Ω |
| Anode resistance — final | 0.339 | Ω |
| Individual anode current — initial | 0.916 | A |
| Individual anode current — final | 0.740 | A |

### FM-15 — Short Flush-Mounted Anode (Yoke Frame — same as PLET design)

| Parameter | Value | Unit |
|-----------|-------|------|
| Type | Short flush-mounted | — |
| Net mass per anode | ~15 | kg (nominal) |
| Note | Yoke-specific; same anode type used on PLETs | — |

## Design Results

### Current and Mass Requirements (per 4-RBS Group)

| Parameter | Value | Unit |
|-----------|-------|------|
| Total design current | 9.290 | A |
| Total net anode mass required | 679.65 | kg |

### Anode Count Summary

| Anode Type | Application | Count per Group | Mass per Group (kg) |
|------------|-------------|-----------------|---------------------|
| SO-90 | Main structure — stand-off | 16 | 16 × 91.44 = 1,463.0 |
| FM-65 | Main structure + jumper — flush | 24 | 24 × 64.04 = 1,536.96 |
| FM-15 | Yoke frame | 6 | ~90 (nominal) |
| **Total** | **—** | **46** | **~3,090 (gross)** |

Note: Per RBS (×4 units): 4 SO-90 + 6 FM-65 (main) + 1–2 FM-15 (yoke).
Anode counts are for the combined 4-RBS group; governing case drives the total.

## CP Protection Philosophy

- No anodes mounted on piping — all piping is insulation-coated; incidental CP from structure anodes
- Flowline jumpers have no own anodes — protected by electrical continuity with connected structures
- Electrical continuity requirement: ≤0.2 Ω between jumper and RBS
- 10% contingency added to structural surface areas (company specification)
- Wet storage period (2 yr) is included in 27-year total design life CBF calculations
- Yoke frame anodes (FM-15 type) are same model as PLET yoke anodes — standardised across field

## Gaps Found

- Individual per-RBS anode layout drawings are not included in the calculation note; exact
  mounting positions are deferred to the fabrication drawings.
- FM-15 net mass is nominal (~15 kg); exact dimensions and resistance values are not explicitly
  tabulated in the source document (only the count is given).
- The connector bare area contribution is small and included within the 10% contingency; it is
  not broken out as a separate line item in the source calculation.

## Python cfg dict

```python
cfg = {
    "inputs": {
        "calculation_type": "DNVGL_RP_B401_2017",
        "standard": "DNVGL-RP-B401:2017",
        "design_data": {
            "structure_type": "riser_base_structure",
            "design_life_total": 27,         # years (25 operational + 2 wet storage)
            "design_life_operational": 25,
            "wet_storage_years": 2,
            "water_depth_m": [1710, 1900],
            "unit_count": 4,                 # 4 RBS
        },
        "environment": {
            "resistivity_seawater_ohm_m": 0.31,
            "resistivity_sediment_ohm_m": 1.00,
            "min_potential_seawater_V": -0.800,
            "min_potential_sediment_V": -0.900,
        },
        "current_density": {
            # Seawater-exposed structures >300 m
            "initial_A_m2": 0.220,
            "mean_A_m2": 0.110,
            "final_A_m2": 0.170,
            "buried_A_m2": 0.020,
        },
        "surface_area_contingency": 0.10,    # +10% on structural surfaces
        "coating_breakdown_27yr": {
            # Per DNVGL-RP-B401:2017 Table 6-5, interpolated to 27 years
            "cat_III_paint": {"initial": 0.020, "mean": 0.128, "final": 0.236},
            "insulation":    {"initial": 0.0003, "mean": 0.000435, "final": 0.000570},
            "bare":          {"initial": 1.000, "mean": 1.000, "final": 1.000},
        },
        "surface_areas_per_rbs_m2": {
            "yoke_cat_III": 32.4,
            "structure_cat_III": 60.8,
            "piping_insulated": 2.9,
            "jumper_insulated": 40.5,
        },
        "anodes": {
            "SO_90": {
                "type": "stand_off",
                "dimensions_mm": {"length": 1080, "width": 189, "height": 180},
                "core": "pipe_60.3mm_OD",
                "net_mass_kg": 91.44,
                "capacity_Ah_kg": 2000,
                "ccp_V": -1.05,
                "utilisation": 0.90,
                "Ra_initial_ohm": 0.119,
                "Ra_final_ohm": 0.175,
                "Ia_initial_A": 2.100,
                "Ia_final_A": 1.430,
            },
            "FM_65": {
                "type": "long_flush_mounted",
                "dimensions_mm": {"length": 950, "width": 186, "height": 136},
                "core": "flat_bar_60mm",
                "net_mass_kg": 64.04,
                "capacity_Ah_kg": 2000,
                "ccp_V": -1.05,
                "utilisation": 0.85,
                "Ra_initial_ohm": 0.273,
                "Ra_final_ohm": 0.339,
                "Ia_initial_A": 0.916,
                "Ia_final_A": 0.740,
            },
            "FM_15": {
                "type": "short_flush_mounted",
                "net_mass_kg": 15.0,   # nominal
                "note": "yoke_frame_same_as_plet",
            },
        },
        "design_results_4rbs_group": {
            "total_current_A": 9.290,
            "total_net_mass_kg": 679.65,
            "anode_count": {
                "SO_90": 16,    # 4 per RBS
                "FM_65": 24,    # 6 per RBS
                "FM_15": 6,     # yoke
            },
        },
        "electrical_continuity_max_ohm": 0.2,
    }
}
# Run: CathodicProtection().router(cfg)
```
