---
standard: DNV-RP-B401:2005
edition: "2005"
structure_type: single_leg_hybrid_riser
source_type: abstracted_client_calculation
discipline: cathodic_protection
---

# Single Leg Hybrid Riser (SLHR) — DNV-RP-B401:2005 Sacrificial Anode Design

## Source
Standards: DNV-RP-B401:2005 (Cathodic Protection Design), DNV-RP-F103:2003
(Cathodic Protection of Submarine Pipelines by Galvanic Anodes)
Structure type: 9 SLHRs — buoyancy tank, URA, LRA, rigid line pipe, foundation (pile + ballast box)
Design life: 22 years

## Riser Types Covered
| ID | Type | OD Upper/Lower (m) | WT Upper/Lower (mm) | MDT (°C) | Coating (Line Pipe) |
|----|------|-------------------|--------------------|-----------|--------------------|
| 12" Prod-A | 12" Production | 0.329 / 0.321 | 22.0 / 18.0 | 80 | Insulating |
| 12" Prod-B | 12" Production | 0.329 / 0.321 | 22.0 / 18.0 | 80 | Insulating |
| 10" Prod | 10" Production | 0.278 / 0.271 | 22.0 / 18.0 | 80 | Insulating |
| 12" Svc-A | 12" Service | 0.324 / 0.324 | 23.2 / 19.6 | 45 | 3LPP |
| 12" Svc-B | 12" Service | 0.324 / 0.324 | 23.2 / 19.6 | 45 | 3LPP |
| 10" Svc | 10" Service | 0.273 / 0.273 | 23.8 / 19.6 | 45 | 3LPP |
| 8" GL | 8" Gas Lift | 0.219 / 0.219 | 23.9 / 18.9 | 65 | Insulating |
| 10" GI | 10" Gas Injection | 0.273 / 0.273 | 24.9 / 21.7 | 65 | 3LPP |
| 14" WI | 14" Water Injection | 0.356 / 0.356 | 26.6 / 21.2 | 55 | 3LPP |

## Surface Areas (representative riser — 12" Production)
| Component | Surface Area (m²) | Notes |
|-----------|------------------|-------|
| Buoyancy Tank | 1021 | Common to most 12" and 10" risers |
| URA (incl. gutter + pullhead) | 460.3 | 86% coated / 14% insulated |
| Line Pipe (external, incl. 5% addition) | 1915 | Based on riser total length + 30 m tolerance |
| LRA (total external) | 111.5 | 57% coated / 43% insulated |
| Pile above mudline | 64.7 | 76.2% coated / 23.8% uncoated |
| Pile total buried external | 333.6 | Full burial |
| Buried exposed (mudline to -2 m): Pile | 15.3 | Corrosive zone |
| Buried exposed (mudline to -2 m): Ballast Box | 137.4 | Corrosive zone |
| Ballast Box | 602.5 | 97.6% coated / 2.4% uncoated (internal cone bare) |

Note: 12" Service buoyancy tank = 1077 m², 14" WI buoyancy tank = 1167 m², 8" GL buoyancy tank = 875 m²

## Environment
| Parameter | Value | Unit | Notes |
|-----------|-------|------|-------|
| Surface seawater temperature | 22 | °C | Tropical region |
| Climatic region | Tropical | — | |
| Resistivity (buoyancy tank + URA) | 0.22 | Ω·m | |
| Resistivity (line pipe, LRA, foundation) | 0.32 | Ω·m | |

## Current Density (Table 4.9)
| SLHR Structural Element | Initial (mA/m²) | Mean (mA/m²) | Final (mA/m²) |
|------------------------|----------------|-------------|--------------|
| All structures above mudline | 440 | 220 | 220 |
| Buried section (mudline and below) | 20 | 20 | 20 |
| Line Pipe — Production risers (MDT 80°C) | 470 | 250 | 250 |
| Line Pipe — Service risers (MDT 45°C) | 440 | 220 | 220 |
| Line Pipe — Gas Lift/Injection risers (MDT 65°C) | 455 | 235 | 235 |
| Line Pipe — Water Injection riser (MDT 55°C) | 445 | 225 | 225 |

Note: Current densities for line pipe increased by 1 mA/m² per °C above 50°C (project requirement).

## Coating Breakdown Factors (Table 4.10, 22-year design life)
| Component | Coating Type | Initial | Mean | Final |
|-----------|-------------|---------|------|-------|
| All structures above mudline | DNV Category III (min.) | 0.02 | 0.11 | 0.20 |
| Riser foundation below mudline | Bare metal | 1.00 | 1.00 | 1.00 |
| Line Pipe | 3LPP | 0.0050 | 0.0072 | 0.0094 |
| Line Pipe | Insulating | 0.0020 | 0.0031 | 0.0042 |

Note: Breakdown factors for line pipe coatings include field joint (FJ) contribution.

## Anode Specification (Table 4.11, Table 5.9)
| Parameter | Value | Unit | Notes |
|-----------|-------|------|-------|
| Anode code | AFA1190 | — | Long slender stand-off |
| Length | 1530 | mm | Initial full length |
| Width | 184 | mm | |
| Depth (height) | 159 | mm | |
| Net mass | 119.0 | kg | |
| Gross mass | 126.0 | kg | Includes 150 mm core extension each end |
| Electrochemical capacity | 2000 | A·h/kg | Per DNV-RP-B401 Table 10-6 |
| Closed circuit potential EA° | −1.05 | V vs Ag/AgCl | Per DNV-RP-B401 Table 10-6 |
| Density | 2750 | kg/m³ | Al alloy |
| Utilisation factor u | 0.90 | — | Long slender stand-off; DNV-RP-B401 Table 10-8 |
| Length final (−10% reduction) | 1377 | mm | Per DNV-RP-B401 §7.9.4 |

### Anode Resistance and Current Output
| Condition | Resistivity (Ω·m) | L (mm) | r (mm) | M (kg) | Ra (Ω) | Ia (A) |
|-----------|------------------|--------|--------|--------|--------|--------|
| Initial | 0.22 | 1530 | 95 | 119 | 0.072 | 3.45 |
| Final | 0.22 | 1377 | 32 | 11.9 | 0.106 | 2.36 |
| Initial | 0.32 | 1530 | 95 | 119 | 0.105 | 2.37 |
| Final | 0.32 | 1377 | 32 | 11.9 | 0.153 | 1.64 |

Note: Stand-off distance 150–300 mm from structure; resistance multiplied by factor 1.3 per Section 5.3.
Driving voltage ΔE° = EC° − EA° = −0.80 − (−1.05) = 0.25 V.

## Protection Potential
| Parameter | Value | Unit |
|-----------|-------|------|
| Min protection potential | −0.800 | V vs Ag/AgCl |
| Global protection potential E'C | −0.95 | V vs Ag/AgCl |
| Metallic voltage drop ΔEMe | 0.15 | V |

## Attenuation Analysis (DNV-RP-F103 §5.6.7)
Protected length formula used to verify line pipe is fully protected from adjacent structure anodes:

PL = √[ΔEMe × WT × (D − WT) / (ρMe × D × fcf × icm)]

with ρMe = 2.0×10⁻⁷ Ω·m (steel resistivity)

| Riser | Est. Total Pipe Length (m) | Upper PL (m) | Lower PL (m) | Result |
|-------|--------------------------|-------------|-------------|--------|
| 12" Production | 1784 | 3829 | 3484 | Line fully protected |
| 10" Production | 1834 | 3804 | 3464 | Line fully protected |
| 12" Service | 1834 | 2827 | 2614 | Line fully protected |
| 10" Service | 1786 | 2839 | 2598 | Line fully protected |
| 8" Gas Lift | 1842 | 4022 | 3622 | Line fully protected |
| 10" Gas Injection | 1786 | 2772 | 2604 | Line fully protected |
| 14" Water Injection | 1830 | 2954 | 2659 | Line fully protected |

## Calculation Results

### Current Demand per Structural Unit (12" Production Riser — Tables 5.1)
| Structural Unit | Initial (A) | Mean (A) | Final (A) | Net Mass (kg) |
|-----------------|------------|---------|----------|--------------|
| Buoyancy Tank | 8.99 | 24.70 | 44.92 | 2645 |
| URA | 3.54 | 9.62 | 17.47 | 1030 |
| Line Pipe | 1.80 | 1.48 | 2.01 | 159 |
| LRA | 0.60 | 1.57 | 2.84 | 168 |
| Pile external (above mudline) | 7.21 | 4.58 | 5.56 | 490 |
| Buried exposed (mudline to −2 m) | 3.05 | 3.05 | 3.05 | 327 |
| Ballast Box | 11.46 | 17.38 | 29.02 | 1860 |
| Total buried external surface (drain) | 5.00 | 5.00 | 5.00 | 535 |

### Current Demand per Structural Unit (14" Water Injection Riser — Table 5.7)
| Structural Unit | Initial (A) | Mean (A) | Final (A) | Net Mass (kg) |
|-----------------|------------|---------|----------|--------------|
| Buoyancy Tank | 10.27 | 28.24 | 51.34 | 3023 |
| URA | 4.04 | 11.11 | 20.20 | 1189 |
| Line Pipe | 4.78 | 3.48 | 4.55 | 373 |
| LRA | 1.00 | 2.74 | 4.99 | 294 |
| Pile external | 7.21 | 4.58 | 5.56 | 490 |
| Buried exposed | 3.05 | 3.05 | 3.05 | 327 |
| Ballast Box | 11.46 | 17.38 | 29.02 | 1860 |
| Total buried external | 5.00 | 5.00 | 5.00 | 535 |

### Anode Quantities per Riser (Summary — Appendix A1.0)
| Riser Type | Buoyancy Tank | URA | LRA | Foundation | Total (kg gross) |
|------------|--------------|-----|-----|------------|-----------------|
| 12" Production (each, ×2) | 24 | 10 | 3 | 28 | 8190 |
| 10" Production | 24 | 10 | 2 | 28 | 8064 |
| 12" Service (each, ×2) | 25 | 12 | 4 | 28 | 8694 |
| 10" Service | 24 | 11 | 4 | 28 | 8442 |
| 8" Gas Lift | 21 | 10 | 2 | 28 | 7686 |
| 10" Gas Injection | 24 | 12 | 4 | 28 | 8568 |
| 14" Water Injection | 27 | 12 | 5 | 28 | 9072 |

### Totals (9 SLHRs, all anodes AFA1190, 126 kg gross)
| Zone | Count (design) | Count (+30% spare) | Total gross mass (kg) |
|------|---------------|-------------------|----------------------|
| Buoyancy Tank | 218 | 283 | 35708 |
| URA | 99 | 129 | 16216 |
| LRA | 31 | 40 | 5078 |
| Foundation | 252 | 328 | 41278 |
| **Grand total** | **600** | **780** | **98280** |

Note: 30% spare is a procurement recommendation from the SLHR Spares Philosophy document, not a code requirement.

## Python cfg dict

```python
cfg = {
    "inputs": {
        "calculation_type": "DNV_RP_B401_2005",
        "standard": "DNV-RP-B401:2005",
        "co_standard": "DNV-RP-F103:2003",
        "design_data": {
            "design_life": 22,         # years
            "structure_type": "SLHR",
        },
        "environment": {
            "temperature": 22,         # °C (surface seawater)
            "climate_region": "Tropical",
            "resistivity_buoyancy_ura": 0.22,    # Ω·m
            "resistivity_lra_foundation": 0.32,  # Ω·m
        },
        "current_density": {
            # All structures above mudline (mA/m²)
            "structures_above_mudline": {
                "initial": 0.440, "mean": 0.220, "final": 0.220
            },
            "buried": 0.020,
            # Line pipe — by MDT (mA/m²)
            "line_pipe_production_mdt80": {
                "initial": 0.470, "mean": 0.250, "final": 0.250
            },
            "line_pipe_service_mdt45": {
                "initial": 0.440, "mean": 0.220, "final": 0.220
            },
            "line_pipe_gaslift_mdt65": {
                "initial": 0.455, "mean": 0.235, "final": 0.235
            },
            "line_pipe_wi_mdt55": {
                "initial": 0.445, "mean": 0.225, "final": 0.225
            },
        },
        "coating_breakdown": {
            # 22-year design life
            "structures_above_mudline_cat3": {
                "initial": 0.02, "mean": 0.11, "final": 0.20
            },
            "foundation_bare": {
                "initial": 1.00, "mean": 1.00, "final": 1.00
            },
            "line_pipe_3lpp": {
                "initial": 0.0050, "mean": 0.0072, "final": 0.0094
            },
            "line_pipe_insulating": {
                "initial": 0.0020, "mean": 0.0031, "final": 0.0042
            },
        },
        "anode": {
            "code": "AFA1190",
            "type": "long_slender_stand_off",
            "length": 1.530,           # m (initial)
            "length_final": 1.377,     # m (−10% per DNV-RP-B401 §7.9.4)
            "width": 0.184,            # m
            "depth": 0.159,            # m
            "net_mass": 119.0,         # kg
            "gross_mass": 126.0,       # kg
            "material": "Al_Zn_In",
            "density": 2750,           # kg/m³
            "current_capacity": 2000,  # A·h/kg
            "utilisation_factor": 0.90,
            "closed_circuit_potential": -1.05,  # V vs Ag/AgCl
            "stand_off_resistance_factor": 1.3, # per §5.3 (150–300 mm stand-off)
        },
        "protection": {
            "min_potential": -0.800,   # V vs Ag/AgCl
            "global_protection_potential": -0.950,
            "driving_voltage_delta_E": 0.250,   # V
        },
        "attenuation": {
            "method": "DNV-RP-F103_5.6.7",
            "steel_resistivity": 2.0e-7,  # Ω·m
            "metallic_voltage_drop": 0.15, # V
        },
        # Reference results — 12" Production Riser
        "expected_results_12in_prod": {
            "buoyancy_tank": {
                "current_initial_A": 8.99, "current_mean_A": 24.70,
                "current_final_A": 44.92, "net_mass_kg": 2645,
                "anode_count": 24,
            },
            "ura": {
                "current_initial_A": 3.54, "current_mean_A": 9.62,
                "current_final_A": 17.47, "net_mass_kg": 1030,
                "anode_count": 10,
            },
            "lra": {
                "current_initial_A": 0.60, "current_mean_A": 1.57,
                "current_final_A": 2.84, "net_mass_kg": 168,
                "anode_count": 3,
            },
            "foundation": {
                "anode_count": 28,
            },
        },
        # Fleet totals (9 SLHRs, design quantities, no spare)
        "fleet_totals_design": {
            "buoyancy_count": 218, "ura_count": 99,
            "lra_count": 31, "foundation_count": 252,
            "total_count": 600, "total_gross_mass_kg": 75600,
        },
        # Including 30% procurement spare
        "fleet_totals_with_spare": {
            "total_count": 780, "total_gross_mass_kg": 98280,
        },
    }
}
# Run: CathodicProtection().router(cfg)
```

## Gaps Found
- Foundation buried-section current drain of 5 A was applied as a flat value per DNV-RP-B401 §6.9.3
  (equivalent to well-casing recommendation for ~2500 m² external surface); actual buried pile area
  is 333.6 m² so this is conservative. Implementation should allow a configurable value.
- The 30% spare contingency (fleet total 780 vs. 600 design) comes from a separate spares philosophy
  document and is NOT part of the code-based CP calculation; it should be tracked separately.
- Flexible riser jumper anodes (21 total, from subcontractor calculation) are included in the source
  executive summary but explicitly excluded from this SLHR CP design calculation. They are NOT
  counted in the fleet totals above.
- The source uses `AFA1190` anode from Aberdeen Foundries; if an alternative supplier is used,
  all resistance and current-output values must be recalculated.
- Line pipe anodes are physically installed on URA (upper pipe) and LRA (lower pipe); no anodes
  are installed directly on the rigid riser line pipe itself.
