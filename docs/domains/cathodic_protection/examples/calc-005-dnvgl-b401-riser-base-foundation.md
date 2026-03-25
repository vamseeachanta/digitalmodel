---
standard: DNVGL-RP-B401:2017
edition: "2017"
structure_type: subsea_riser_base_foundation
source_type: abstracted_client_calculation
discipline: cathodic_protection
---

# Subsea Riser Base Foundation — DNVGL-RP-B401:2017 CP Design

## Source
Standard: DNVGL-RP-B401:2017
Structure type: Production Riser Base Foundation (RBF) — 4 units
Scope: Subsea mud mat / pile foundations supporting riser base structures
Design life: 27 years (25 yr operational + 2 yr wet storage)
Water depth: 1710–1900 m

## Scope

4 production Riser Base Foundations (RBF). Each RBF consists of:
- Main structural body (mud mat / grillage), partially seawater-exposed and partially sediment-buried
- Hatch covers (2 per RBF) — removable inspection covers, seawater-exposed, painted

The main structure has a large sediment-buried area (bare steel) and a smaller seawater-exposed area (painted).
Hatch covers are treated as a separate sub-calculation.

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

| Zone | Coating | Notes |
|------|---------|-------|
| Seawater-exposed structural steel | CAT III paint | Painted sections |
| Sediment-buried structural steel | Bare (uncoated) | Below mudline |
| Hatch covers | CAT III paint | Seawater-exposed |

## Coating Breakdown Factors (27-year total)

Per DNVGL-RP-B401:2017 Table 6-5 (interpolated to 27 yr):

| Coating | Initial | Mean (27 yr) | Final (27 yr) |
|---------|---------|-------------|--------------|
| CAT III (Paint) | 0.020 | 0.128 | 0.236 |
| Bare | 1.000 | 1.000 | 1.000 |

## Current Density

Per DNVGL-RP-B401:2017 Table 6-3 (>300 m water depth):

| Condition | Initial (A/m²) | Mean (A/m²) | Final (A/m²) |
|-----------|---------------|-------------|-------------|
| Seawater exposed (>300 m) | 0.220 | 0.110 | 0.170 |
| Buried / sediment | 0.020 | 0.020 | 0.020 |

Design contingency: +10% surface area applied to structural surfaces.

## Surface Areas per RBF

### Main Structure — Per RBF

| Zone | Condition | Coating | Net Area (m²) |
|------|-----------|---------|--------------|
| Upper / exposed faces | Seawater | CAT III | 89.5 |
| Lower / buried faces | Sediment | Bare | 320.5 |

Total per RBF: 89.5 m² seawater-exposed + 320.5 m² sediment-buried.

### Hatch Cover — Per RBF

| Item | Condition | Coating | Area per cover (m²) | Covers per RBF |
|------|-----------|---------|---------------------|----------------|
| Hatch cover | Seawater | CAT III | 1.5 | 2 |

Total hatch area per RBF: 2 × 1.5 = 3.0 m²

## Anode Properties

### FM-65 — Long Flush-Mounted Anode (Main Foundation)

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

Note: Same FM-65 anode type as used on the riser base structure (calc-004).

### FM-02 — Short Flush-Mounted Anode (Hatch Covers)

| Parameter | Value | Unit |
|-----------|-------|------|
| Type | Short flush-mounted | — |
| Dimensions (L × W × H) | 150 × 75 × 70 | mm |
| Core | Flat bar, 40 mm | — |
| Net mass per anode | 2.03 | kg |
| Electrochemical capacity | 2000 | A·h/kg |
| Closed-circuit potential | −1.05 | V vs Ag/AgCl |
| Utilisation factor | 0.80 | — |
| Anode resistance — initial | 0.472 | Ω |
| Anode resistance — final | 0.920 | Ω |
| Individual anode current — initial | 0.529 | A |
| Individual anode current — final | 0.272 | A |

## Design Results

### Main Structure (per 4-RBF Group)

| Parameter | Value | Unit |
|-----------|-------|------|
| Total design current | 12.101 | A |
| Total net anode mass required | 1,291.25 | kg |
| Anode type | FM-65 | — |
| Number of FM-65 (total, 4 RBF) | 88 | — |
| FM-65 per RBF | 22 | — |

### Hatch Covers (per 4-RBF Group)

| Parameter | Value | Unit |
|-----------|-------|------|
| Total design current | 0.073 | A |
| Total net anode mass required | 3.78 | kg |
| Anode type | FM-02 | — |
| Number of FM-02 (total, 4 RBF) | 16 | — |
| FM-02 per RBF | 4 | — |
| FM-02 per hatch cover | 2 | — |

### Combined Summary (per 4-RBF Group)

| Anode Type | Count | Net Mass (kg) |
|------------|-------|---------------|
| FM-65 (main) | 88 | 88 × 64.04 = 5,635.5 |
| FM-02 (hatch) | 16 | 16 × 2.03 = 32.5 |
| **Total** | **104** | **5,668.0** |

## CP Protection Philosophy

- Foundation lower section (sediment-buried) uses bare steel with buried current density (0.020 A/m²)
- Sediment-buried zone requires the higher −0.900 V vs Ag/AgCl protection potential (anaerobic)
- Hatch covers are treated as isolated sub-items with their own small (FM-02) anodes
- The 10% surface area contingency is applied to all zones
- No anodes on piping — structural anodes only (consistent with connected RBS design)
- Electrical continuity between foundation and RBS structure maintained by mechanical connection

## Gaps Found

- Exact sediment burial depth assumed to be full lower face area (320.5 m²); actual burial progression
  is not modelled dynamically — conservative uniform burial is assumed for the buried zone.
- FM-02 hatch cover anode dimensions give small resistance values at end-of-life (Ra_fin = 0.920 Ω)
  which dominates the hatch cover current output. Two per cover ensure adequate redundancy.
- The 27-yr CBF for CAT III (mean 0.128, final 0.236) is extrapolated beyond the Table 6-5 25-yr
  boundary; the formula CBF = a + b×t² is used for the extrapolation (consistent with S5 design basis).

## Python cfg dict

```python
cfg = {
    "inputs": {
        "calculation_type": "DNVGL_RP_B401_2017",
        "standard": "DNVGL-RP-B401:2017",
        "design_data": {
            "structure_type": "riser_base_foundation",
            "design_life_total": 27,         # years
            "design_life_operational": 25,
            "wet_storage_years": 2,
            "water_depth_m": [1710, 1900],
            "unit_count": 4,                 # 4 RBF
            "hatch_covers_per_rbf": 2,
        },
        "environment": {
            "resistivity_seawater_ohm_m": 0.31,
            "resistivity_sediment_ohm_m": 1.00,
            "min_potential_seawater_V": -0.800,
            "min_potential_sediment_V": -0.900,
        },
        "current_density": {
            "initial_A_m2": 0.220,
            "mean_A_m2": 0.110,
            "final_A_m2": 0.170,
            "buried_A_m2": 0.020,
        },
        "surface_area_contingency": 0.10,
        "coating_breakdown_27yr": {
            "cat_III_paint": {"initial": 0.020, "mean": 0.128, "final": 0.236},
            "bare":          {"initial": 1.000, "mean": 1.000, "final": 1.000},
        },
        "surface_areas_per_rbf_m2": {
            "structure_seawater_cat_III": 89.5,
            "structure_sediment_bare": 320.5,
            "hatch_cover_seawater_cat_III": 1.5,  # per cover; 2 covers per RBF
        },
        "anodes": {
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
                "application": "main_foundation",
            },
            "FM_02": {
                "type": "short_flush_mounted",
                "dimensions_mm": {"length": 150, "width": 75, "height": 70},
                "core": "flat_bar_40mm",
                "net_mass_kg": 2.03,
                "capacity_Ah_kg": 2000,
                "ccp_V": -1.05,
                "utilisation": 0.80,
                "Ra_initial_ohm": 0.472,
                "Ra_final_ohm": 0.920,
                "Ia_initial_A": 0.529,
                "Ia_final_A": 0.272,
                "application": "hatch_covers",
            },
        },
        "design_results_4rbf_group": {
            "main_structure": {
                "current_A": 12.101,
                "net_mass_kg": 1291.25,
                "FM_65_total": 88,
                "FM_65_per_rbf": 22,
            },
            "hatch_covers": {
                "current_A": 0.073,
                "net_mass_kg": 3.78,
                "FM_02_total": 16,
                "FM_02_per_rbf": 4,
                "FM_02_per_cover": 2,
            },
            "combined": {
                "total_anodes": 104,
                "total_net_mass_kg": 5668.0,
            },
        },
    }
}
# Run: CathodicProtection().router(cfg)
```
