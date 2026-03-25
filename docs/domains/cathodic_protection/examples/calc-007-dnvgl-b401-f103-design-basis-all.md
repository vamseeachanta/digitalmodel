---
standard: DNVGL-RP-B401:2017
edition: "2017"
structure_type: deepwater_subsea_field_cp_design_basis
source_type: abstracted_client_calculation
discipline: cathodic_protection
---

# Deepwater Subsea Field — DNVGL-RP-B401:2017 / DNVGL-RP-F103:2016 Cathodic Protection Design Basis

## Source
Standards: DNVGL-RP-F103 (2016), DNVGL-RP-B401 (2017), ISO 15589-2 (2012), API RP 1111
Structure type: Comprehensive CP design basis — submarine flowlines, structures (FLETs, PLETs, RBS),
flowline jumpers, walking mitigation system
Project scope: deepwater oil field, ~200 km offshore, 1710–1900 m water depth
Design life: 25 years (+2 years wet storage for items requiring it)

## Scope of Structures Covered
### Flowlines and Pipelines
| Line | OD (in) | WT (mm) | Coating | Op. Temp. °C (°F) |
|------|---------|---------|---------|-------------------|
| Production (×8) | 10.75 | 25.4 | 5LPP | 107.2 (225) |
| Water Injection riser | 12.75 | 38.1 | 5LPP | 62.8 (145) |
| Water Injection flowline | 14.00 | 30.5 | 5LPP | 62.8 (145) |
| Gas Injection riser | 12.75 | 38.1 | 3LPP | 51.7 (125) |
| Gas Injection flowline | 14.00 | 31.8 | 3LPP | 51.7 (125) |

Note: Production line max temp can reach 121°C (250°F) briefly during well testing (<0.25% of 25-yr life).

### Subsea Structures
- Production: 10 FLETs (4 single-hub, 6 double-hub) + 4 riser FLETs with associated RBS + foundations
- Water Injection: 3 PLETs
- Gas Injection: 4 PLETs
- Flowline jumpers: multiple (FLET-to-FLET, FLET-to-Manifold, PLET-to-Manifold, FLET-to-RBS)
- Walking mitigation system (separate design — see calc-006)

## Environment
| Parameter | Value | Unit | Notes |
|-----------|-------|------|-------|
| Water depth | 1710–1900 | m | Range across field |
| Seawater resistivity (near seabed, 3.9°C) | 0.31 | Ω·m | |
| Seawater resistivity (near surface, 27.3°C) | 0.18 | Ω·m | |
| Sediment resistivity | 1.00 | Ω·m | Per ISO 15589-2; 2× seawater for very soft soils |

## Protective Potentials
| Environment | Min Potential | Unit |
|-------------|---------------|------|
| Aerobic (seawater) | −0.800 | V vs Ag/AgCl |
| Anaerobic (sediment) | −0.900 | V vs Ag/AgCl |

## Current Density
### Flowlines and Pipelines (Table 6-2, DNVGL-RP-F103)
| Pipe Condition | Fluid Temp ≤25°C | >25–50°C | 50–80°C | 80–120°C |
|----------------|-----------------|----------|---------|----------|
| Non-buried (A/m²) | 0.050 | 0.060 | 0.075 | 0.100 |
| Buried (A/m²) | 0.020 | 0.030 | 0.040 | 0.060 |

For heated piping: add 0.001 A/m² per °C above 25°C (single wall = fluid temperature).

### Structures and Flowline Jumpers (Table 6-3, DNVGL-RP-B401)
| Water Depth | Condition | Initial (A/m²) | Mean (A/m²) | Final (A/m²) |
|-------------|-----------|---------------|------------|-------------|
| >300 m | Exposed | 0.220 | 0.110 | 0.170 |
| >300 m | Buried | 0.020 | 0.020 | 0.020 |

Safety factor: 1.1 for linepipes (per company spec); 10% additional surface area for structures.

## Coating Breakdown Factors

### Flowlines / Linepipes (Table 6-4)
DNV formula: CBFm = a + b × tm²; CBFf = a + b × tf²

| Coating | a | b | 25 yr Mean | 25 yr Final | 27 yr Mean | 27 yr Final |
|---------|---|---|-----------|------------|-----------|------------|
| 3LPP | 0.001 | 0.00003 | 0.001375 | 0.00175 | 0.001405 | 0.00181 |
| 5LPP | 0.0003 | 0.00001 | 0.000425 | 0.00055 | 0.000435 | 0.00057 |

For 2-year wet storage only:
| Coating | 2 yr Mean | 2 yr Final |
|---------|----------|-----------|
| 3LPP | 0.00103 | 0.00106 |
| 5LPP | 0.00031 | 0.00032 |

### Structures and Flowline Jumpers (Table 6-5, 25-year design life)
| Coating Type | Initial | Mean | Final |
|-------------|---------|------|-------|
| Bare | 1.0 | 1.0 | 1.0 |
| CAT I | 0.10 | 0.725 | 1.0 |
| CAT II | 0.05 | 0.2375 | 0.425 |
| CAT III | 0.02 | 0.12 | 0.22 |
| 3LPP | 0.001 | 0.001375 | 0.00175 |
| Insulation (5LPP) | 0.0003 | 0.000425 | 0.00055 |

## Anode Properties (Table 6-6)
| Condition | Capacity (A·h/kg) | CCP (V vs Ag/AgCl) |
|-----------|------------------|--------------------|
| Seawater exposed (30°C) | 2000 | −1.05 |
| Seawater exposed (60°C) | 1500 | −1.05 |
| Self-buried (30°C) | 1500 | −1.00 |
| Self-buried (60°C) | 680 | −1.00 |
| Sediment exposed | 1500 | −0.95 |

Anode alloy: Indium-activated Aluminium (Al/Zn/In)

## Anode Utilisation Factors (Table 6-7)
| Anode Type | U |
|------------|---|
| Long stand-off (L ≥ 4r) | 0.90 |
| Short stand-off (L < 4r) | 0.85 |
| Short flush mounted (L ≥ 4W and ≥ 4T) | 0.80 |
| Long flush mounted | 0.85 |
| Bracelet (W ≥ 50 mm) | 0.80 |
| Bracelet (W < 50 mm) | 0.70 |

## Design Methodology

### Current Requirement
I = Σ(i × A × fc)   for each coating/zone combination
Design safety factor: 1.1 (linepipes, per company spec); +10% surface area (structures)

### Anode Mass Requirement
W = Im × t × 8760 / (ε × U)   where Im = mean current (A), t = design life (yr), ε = capacity (A·h/kg)

### Anode Resistance Formulas
- Bracelet / short flush: Ra = 0.315 × ρ / √A   (A = exposed anode surface area m²)
- Long flush mounted: Ra = ρ / (2 × S)   where S = arithmetic mean of anode length and width
- Stand-off: Ra = ρ / (2πL × [ln(4L/r) − 1])   where L = anode length, r = equivalent radius

Final anode mass when consumed to utilisation factor: W_final = W_initial × (1 − U)
For long/short slender stand-off: length reduced by 10% at end of life.

### Individual Anode Current Output
Ia = (Ec° − Ea°) / Ra

### Anode Number Requirements
By mass: Nw = W / Wa   (Wa = individual anode net mass)
By current: Na = I / Ia
Governing: max(Nw, Na)

## Attenuation Methodology

### Gibson's Method (non-homogeneous pipe)
Equations 9–15: Coupled system using polarisation resistance and cumulative anode resistance per section.
Polarisation resistance: P = (Ecorr − Ep) / i   (Ω·m²)
Attenuation constant: α = √(πD × RL × fc / P)
Linear pipe resistance: RL calculated from pipe cross-section As = π/4 × [D² − (D − 2t)²]

### ISO 15589-2 Attenuation (remote protection from FLET/PLET bank anodes)
ΔEMe + ΔEA = L² × ρMe × i × fc × D / [4 × d × (D − d)] + Ra × i × π × fc × L / 2   (Eq. 17)
Used to verify full line length is protected from structure-mounted anodes.

### Bracelet Anode Spacing Verification
Maximum spacing: L = √[2 × ΔEMe × d × (D − d) / (ρMe × D × fc × i × k)]   (Eq. 18–19)

## CP Protection Philosophy
- WI and GI risers/flowlines: Protected primarily by FPSO ICCP hull system + PLET anodes
  (no anodes along steel lazy-wave riser lengths where possible)
- Production flowlines: Protected by FLET bank anodes; bracelet anodes added only where
  attenuation calculations show remote protection is insufficient
- Flowline jumpers: No own CP; protected by anodes on connected structures (electrical continuity required)
- Piping on structures: No anodes on piping — anodes only on seawater-exposed structural parts
- Electrical continuity requirement: ≤0.2 Ω
- Walking mitigation system (PCM): Separate design with own CP system

## Python cfg dict

```python
cfg = {
    "inputs": {
        "calculation_type": "DNVGL_RP_B401_2017",
        "standard": "DNVGL-RP-B401:2017",
        "co_standard_pipeline": "DNVGL-RP-F103:2016",
        "co_standard_iso": "ISO-15589-2:2012",
        "design_data": {
            "design_life": 25,         # years (primary)
            "wet_storage_years": 2,    # additional for stored items
            "water_depth_m": [1710, 1900],   # min, max
        },
        "environment": {
            "resistivity_seabed_ohm_m": 0.31,    # at 3.9°C
            "resistivity_surface_ohm_m": 0.18,   # at 27.3°C
            "resistivity_sediment_ohm_m": 1.00,
        },
        "protection": {
            "min_potential_aerobic_V": -0.800,   # vs Ag/AgCl
            "min_potential_anaerobic_V": -0.900,
        },
        "current_density_flowlines": {
            # Non-buried (A/m²) by fluid temperature band
            "non_buried": {
                "le25C": 0.050, "25_50C": 0.060,
                "50_80C": 0.075, "80_120C": 0.100,
            },
            # Buried (A/m²) by fluid temperature band
            "buried": {
                "le25C": 0.020, "25_50C": 0.030,
                "50_80C": 0.040, "80_120C": 0.060,
            },
            "temp_correction_per_degC_above_25": 0.001,   # A/m²/°C
        },
        "current_density_structures": {
            "exposed_gt300m": {"initial": 0.220, "mean": 0.110, "final": 0.170},
            "buried_gt300m":  {"initial": 0.020, "mean": 0.020, "final": 0.020},
        },
        "safety_factor_linepipes": 1.1,
        "safety_factor_structures": "10% surface area addition",
        "coating_breakdown_flowlines": {
            # a and b constants (DNV formula)
            "3LPP": {"a": 0.001, "b": 0.00003,
                     "mean_25yr": 0.001375, "final_25yr": 0.00175,
                     "mean_27yr": 0.001405, "final_27yr": 0.00181},
            "5LPP": {"a": 0.0003, "b": 0.00001,
                     "mean_25yr": 0.000425, "final_25yr": 0.00055,
                     "mean_27yr": 0.000435, "final_27yr": 0.00057},
        },
        "coating_breakdown_structures_25yr": {
            "bare":         {"initial": 1.0,    "mean": 1.0,     "final": 1.0},
            "cat_I":        {"initial": 0.10,   "mean": 0.725,   "final": 1.0},
            "cat_II":       {"initial": 0.05,   "mean": 0.2375,  "final": 0.425},
            "cat_III":      {"initial": 0.02,   "mean": 0.12,    "final": 0.22},
            "3LPP":         {"initial": 0.001,  "mean": 0.001375,"final": 0.00175},
            "insulation_5LPP": {"initial": 0.0003,"mean": 0.000425,"final": 0.00055},
        },
        "anode": {
            "alloy": "Al_Zn_In_indium_activated",
            "properties_by_condition": {
                "seawater_30C":   {"capacity_Ah_kg": 2000, "ccp_V": -1.05},
                "seawater_60C":   {"capacity_Ah_kg": 1500, "ccp_V": -1.05},
                "self_buried_30C":{"capacity_Ah_kg": 1500, "ccp_V": -1.00},
                "self_buried_60C":{"capacity_Ah_kg": 680,  "ccp_V": -1.00},
                "sediment":       {"capacity_Ah_kg": 1500, "ccp_V": -0.95},
            },
            "utilisation": {
                "long_stand_off":      0.90,
                "short_stand_off":     0.85,
                "short_flush_mounted": 0.80,
                "long_flush_mounted":  0.85,
                "bracelet_ge50mm":     0.80,
                "bracelet_lt50mm":     0.70,
            },
            # Final length reduction for long/short slender stand-off
            "end_life_length_reduction": 0.10,
        },
        "electrical_continuity_max_ohm": 0.2,
        "attenuation": {
            "methods": ["Gibson_non_homogeneous", "ISO_15589_2_remote"],
            "bracelet_spacing_formula": "DNVGL-RP-F103_Eq19",
        },
    }
}
# Run: CathodicProtection().router(cfg)
# This file is the design basis (S5); detailed calculations per structure type
# are in calc-003 (flowlines), calc-004 (RBS), calc-005 (foundations), calc-006 (PCM).
```

## Gaps Found
- This is a design-basis document only; it does not contain computed results. Computed anode counts
  and masses are in the separate calculation reports (flowlines, riser base structures, foundation,
  walking mitigation — calc-003 through calc-006).
- For WI and GI risers the primary CP is provided by FPSO ICCP hull — the FPSO contractor is
  responsible for ICCP design and supply. This design basis only specifies the current drain values
  to be communicated to the FPSO contractor; it does not compute ICCP parameters.
- The company technical specification (client GP documents) mandates 1.1 safety factor on
  linepipe current demand; this overrides the DNV default of no explicit current safety factor.
- Where internal fluid temperature exceeds 60°C, anode electrochemical capacity must be reduced
  to 1500 A·h/kg (seawater) or 680 A·h/kg (buried/self-buried). For production flowlines at 107°C
  this is a significant reduction. Implementation must apply temperature-adjusted capacity.
- Walking mitigation / in-line anchor CP systems are called out as either standalone or integrated
  with connected linepipe CP — specific design approach determined case-by-case. See calc-006.
