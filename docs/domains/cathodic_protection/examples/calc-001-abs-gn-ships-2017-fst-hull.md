---
standard: ABS-CATHODIC-PROTECTION-SHIPS
edition: "2017"
structure_type: floating_storage_terminal_hull
source_type: abstracted_client_calculation
discipline: cathodic_protection
---

# Floating Storage Terminal Hull — ABS Sacrificial Anode Design

> The narrative tables retain the abstracted source/legacy calculation record. The
> executable block uses the rebuilt route: coating entries are percentages, the
> 1,350 mA/m2 bare-equivalent density is an explicit project value outside Table 3.
> Owner decision 2026-10-01 accepts the route's three recorded interpretations;
> the result is `client-use-with-eor-check`.

## Source
Standard: ABS Guidance Notes on Cathodic Protection of Ships (December 2017) / ABS Guidance Notes on Cathodic Protection of Offshore Structures (2018)
Structure type: Floating Storage Terminal (FST) hull, tidal and submerged zones
Design life: 5 years (primary); sensitivity runs at 15 and 25 years also computed

## Notes on E1/E2 source pair
E1 is the `correct_final_current_demand_check` revision; E2 is the `vs. programming` revision.
Both workbooks are identical in structure and engineering values — E2 was generated to
validate the E1 calculation against a digital implementation. Both use ABS (not DNV-RP-F103).
The file cover sheet refers to DNV-RP-F103 only in the context of the submarine pipeline
standard; the actual FST hull calculations follow ABS methodology.

## Input Parameters
| Parameter | Symbol | Value | Unit | Notes |
|-----------|--------|-------|------|-------|
| Design life (primary) | tf | 5 | years | Sensitivity at 15 and 25 years also run |
| Coating coverage | — | 100 | % | All wetted area coated |
| Coating breakdown assumption | — | Deterioration | — | NOT disbonding (coating chafing only) |
| Average wetted surface area | Ac | 10 778 | m² | Average of min/max draft from stability analysis |
| Effective coated area | — | 10 778 | m² | Equal to wetted area (100% coating) |
| Steel corrosion potential | Ecorr | −0.800 | V vs Ag/AgCl | Min protection potential mild steel |
| Max protection potential | — | −1.100 | V vs Ag/AgCl | Hydrogen evolution limit |
| Safety factor | — | 1.0 | — | ABS methodology inherent |

## Environment
| Parameter | Value | Unit | Notes |
|-----------|-------|------|-------|
| Seawater temperature | 14 | °C | Coastal inlet |
| Average salinity (design) | 25 | ppt | Used for resistivity selection |
| Seawater resistivity (design) | 0.2547 | Ω·m | At 25 ppt salinity |
| Resistivity at 5 m depth | 0.5875 | Ω·m | Used in sensitivity study |
| Resistivity at 0.32 Ω·m | 0.32 | Ω·m | Sensitivity case |

### Salinity vs Depth Profile
| Depth (m) | Salinity (ppt) | Resistivity (Ω·m) |
|-----------|---------------|-------------------|
| 0 | 2 | 4.577 |
| 1 | 3 | 4.079 |
| 2 | 6 | 2.582 |
| 3 | 7 | 2.084 |
| 4 | 9 | 1.086 |
| 5 | 10 | 0.5875 |
| 10 | 25 | 0.2547 |
| 15 | 28 | 0.2324 |
| 25 | 28 | 0.2324 |

## Coating Breakdown Factors (ABS Table 4, Section 2.4.4)
| Durability | Initial fc (%) | Initial duration (yr) | fc per year (%/yr) |
|------------|---------------|----------------------|-------------------|
| Low | 2.0 | 2.0 | 3.0 |
| High | 1.0 | 2.0 | 1.0 |
| Bare steel | N/A | N/A | 0.0 |

### Derived Mean and Final Factors (5-year design life, High Durability)
| Stage | Value |
|-------|-------|
| Mean fcm | 1.0305 |
| Final fcf | 1.0510 |

## Current Density (ABS Table 5, Section 2.4.4)
| Surface condition | ici (mA/m²) | icm (mA/m²) | icf (mA/m²) |
|------------------|------------|------------|------------|
| Coated steel (tidal, V≤1 kn) | 13.5 | — | — |
| Bare steel | 200.0 | 200.0 | 200.0 |

### Derived Design Current Densities (5-yr, High Durability Coating)
| Stage | Current Density (mA/m²) |
|-------|------------------------|
| Initial ici | 13.5 |
| Mean icm | 13.844 |
| Final icf | 14.189 |

## Anode Specification
| Parameter | Option A | Option B | Unit |
|-----------|----------|----------|------|
| Anode shape | Box (flush) | Box (flush) | — |
| Anode material | Al alloy | Al alloy | — |
| Mean length | 0.65 | 1.00 | m |
| Width | 0.125 | 0.125 | m |
| Height | 0.13 | 0.07 | m |
| Net weight | 29.0 | 18.0 | kg |
| Gross weight | 30.0 | 19.7 | kg |
| Gross/Net ratio | 1.0345 | 1.0944 | — |
| Current capacity Q | 2500 | 2500 | A·h/kg |
| Utilisation factor u | 0.825 | 0.825 | — |
| Anode potential | −1.09 | −1.09 | V vs Ag/AgCl |
| L/W ratio | 5.2 | 8.0 | — |

### Anode Resistance (Long Flush Mount formula, ρ = 0.2547 Ω·m)
| Condition | Ra (Ω) |
|-----------|--------|
| Initial (L=0.65 m) | 0.3286 |
| Final (Ldepleted = 0.5964 m) | 0.3531 |

### Depleted Anode Geometry (post-utilisation)
| Parameter | Value | Unit |
|-----------|-------|------|
| Depleted mass Wfinal | 5.25 | kg |
| Depleted length Lfinal | 0.5964 | m |
| L/W at depletion | 4.771 | — |

## Calculation Results (from source document)
### Current Demand (5-yr, High Durability, Area = 10 778 m²)
| Stage | Current (A) |
|-------|------------|
| Initial Ici | 145.5 |
| Mean Icm | 149.2 |
| Final Icf | 152.9 |

### Anode Mass Requirements (5-yr, High Durability, Volume-based)
| Parameter | Value | Unit |
|-----------|-------|------|
| Total net anode mass | 3 169 | kg |
| Number of anodes (mass check) | 109 | # |
| Updated count (initial current check) | 165 | # |
| Updated count (final current check) | 187 | # |
| Governing anode count | 187 | # |
| Total gross mass | ~5.6 | MT |

**Historical-source note:** the values above preserve the retired legacy/source arithmetic,
not the rebuilt route. The executable block below supplies explicit project coating and
bare-current inputs, depleted geometry, and layout evidence. Its report is the controlling
output and carries `client-use-with-eor-check` under owner decision 2026-10-01.

### Individual Anode Current Output (initial geometry, ρ = 0.2547 Ω·m)
| Stage | Ia per anode (A) |
|-------|-----------------|
| Initial | 0.882 |
| Final (depleted) | 0.821 |

### Summary Table — Sensitivity Cases (Gross anode mass, MT)
| Design Life (yr) | Low Durability (MT) | High Durability (MT) |
|------------------|---------------------|----------------------|
| 5 | 6.04 | 5.59 |
| 15 | 12.12 | 10.36 |
| 25 | 24.40 | 18.24 |

## Python cfg dict

```python
from digitalmodel.infrastructure.base_solvers.hydrodynamics.cathodic_protection import CathodicProtection

cfg = {
    "inputs": {
        # Router key. The source standard is the ABS Guidance Notes on Cathodic Protection
        # of Ships, December 2017. The router key "ABS_gn_ships_2018" is misnamed for that
        # edition; it is the only ABS ships route and is not renamed here.
        "calculation_type": "ABS_gn_ships_2018",
        "design_data": {
            "design_life": 5,                    # years (primary case)
            "seawater_max_temperature": 14,      # deg C
            "coating_assumption": "deterioration",   # not disbonding (documentation only)
        },
        "environment": {
            "seawater": {"resistivity": {"input": 0.2547}},   # ohm.m at 25 ppt
            "salinity_ppt": 25,
        },
        "structure": {
            "steel_total_area": 10778.0,         # m2, average wetted area
            "area_coverage": 100.0,              # % coated
            "coating_initial_breakdown_factor": 1.0,    # % (High durability, ABS Table 4)
            "coating_initial_breakdown_duration": 2.0,  # years
            "coating_breakdown_factor_max": 2.0,
            # Source-derived factors: fcm(5 yr) = 1.0305, fcf(5 yr) = 1.0510
        },
        "design_current": {
            "dynamic_bare_steel_mA_m2": 1350.0,  # project value; outside Table 3
            "static_bare_steel_mA_m2": 1350.0,
            "dynamic_time_fraction": 0.5,
            "coated_steel_mA_m2": 13.5,          # project initial coated density
            "uncoated_steel_mA_m2": 200.0,
        },
        "anode": {
            "material": "aluminium",             # Al alloy, Option A box (flush) anode
            "protection_potential": 0.8,         # V (magnitude, vs Ag/AgCl)
            "closed_circuit_anode_potential": -1.09,   # V vs Ag/AgCl
            "anode_Utilisation_factor": 0.825,
            "anode_density": 2750.0,
            "physical_properties": {
                "net_weight": 29.0, "gross_weight": 30.0,
                "core_cross_section_m2": 0.01625,
            },
            "geometry": {"type": "long_flush", "length_m": 0.65, "width_m": 0.125},
        },
        "layout": {
            "actual_max_spacing_m": 8.0, "selected_locations": 230,
            "anodes_per_location": 2, "high_current_or_low_resistivity": False,
            "mechanical_damage_risk": False, "uniform_distribution_confirmed": True,
            "bilge_damage_avoided": True, "bilge_keel_fitted": False,
        },
    }
}

result = CathodicProtection().router(cfg)["cathodic_protection"]
demand = result["current_demand_A"]["totals"]
print("Ici / Icm / Icf (A): {:.1f} / {:.1f} / {:.1f}".format(
    demand["initial"], demand["mean"], demand["final"]))
print("Anode capacity (Ah/kg):", result["anode_current_capacity"])
print("Total net anode mass (kg): {:.0f}".format(result["anode_requirements"]["total_mass_kg"]))
print("Anode count (mass basis): {:.1f}".format(result["anode_requirements"]["anode_count"]))
print("Ra initial / final (ohm): {:.4f} / {:.4f}".format(
    result["anode_performance"]["resistance_ohm"]["initial"],
    result["anode_performance"]["resistance_ohm"]["final"]))
# Source primary results (5 yr, High durability) are tabulated above:
#   Ici 145.5 A, Icm 149.2 A, Icf 152.9 A, 187 anodes (final current governs), ~5.6 MT gross.
```

## Gaps Found
- Source uses ABS Cathodic Protection of Ships 2017, not DNV-RP-F103 or DNV-RP-B401.
  ABS Table 4 gives coating percentages but no time-development equation; the source's
  multiplicative model is historical project arithmetic, not an ABS-prescribed formula.
- ABS Table 5 current densities apply to hull/tidal conditions; DNV-RP-F103 tables are for submarine
  pipelines — the structures are conceptually different.
- Source also computed a sensitivity for disbonding assumption (vs. deterioration) — disbonding
  drives gross mass to 10.8 MT at 5 yr; this disbonding path is NOT the adopted design.
- The E2 "vs. programming" workbook is structurally identical to E1; no divergence in computed values
  was found. It confirms the E1 calculation is suitable as the reference.
