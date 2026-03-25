---
standard: ABS-CATHODIC-PROTECTION-SHIPS
edition: "2017"
structure_type: floating_storage_terminal_hull
source_type: abstracted_client_calculation
discipline: cathodic_protection
---

# Floating Storage Terminal Hull — ABS Sacrificial Anode Design

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

### Individual Anode Current Output (initial geometry, ρ = 0.2547 Ω·m)
| Stage | Ia per anode (A) |
|-------|-----------------|
| Initial | 0.882 |
| Final (depleted) | 0.821 |

### Summary Table — Sensitivity Cases (Gross anode mass, MT)
| Design Life | Low Durability | High Durability |
|-------------|---------------|-----------------|
| 5 yr | 6.04 | 5.59 |
| 15 yr | 12.12 | 10.36 |
| 25 yr | 24.40 | 18.24 |

## Python cfg dict

```python
cfg = {
    "inputs": {
        "calculation_type": "ABS_CP_SHIPS_2017",
        # NOTE: Source uses ABS Cathodic Protection of Ships (Dec 2017), NOT DNV-RP-F103.
        # Closest implemented route is DNV_RP_F103_2010 for pipeline section geometry;
        # hull/structure route would require ABS-specific breakdown factor formula.
        "design_data": {
            "design_life": 5,          # years (primary case)
            "coating_assumption": "deterioration",   # not disbonding
        },
        "structure": {
            "surface_area": 10778,     # m², average wetted area
            "coating_coverage": 1.0,   # 100%
            "coating_durability": "high",
        },
        "environment": {
            "temperature": 14,         # °C
            "salinity": 25,            # ppt
            "seawater_resistivity": 0.2547,   # Ω·m
        },
        "coating_breakdown": {
            "fc_initial": 0.010,       # 1.0% (High durability)
            "fc_initial_duration": 2,  # years
            "fc_per_year": 0.010,      # 1.0%/yr (High durability)
            # Derived: fcm(5yr)=1.0305, fcf(5yr)=1.0510
        },
        "current_density": {
            "initial": 0.0135,         # A/m² (coated steel, tidal, V≤1 kn)
            "bare_steel": 0.200,       # A/m²
        },
        "anode": {
            "material": "Al_alloy",
            "type": "long_flush_mount",
            "length": 0.65,            # m
            "width": 0.125,            # m
            "height": 0.13,            # m
            "net_weight": 29.0,        # kg
            "gross_weight": 30.0,      # kg
            "current_capacity": 2500,  # A·h/kg
            "utilisation_factor": 0.825,
            "closed_circuit_potential": -1.09,   # V vs Ag/AgCl
        },
        "protection": {
            "min_potential": -0.800,   # V vs Ag/AgCl
            "max_potential": -1.100,   # V vs Ag/AgCl
        },
    }
}
# Run: CathodicProtection().router(cfg)
# Expected primary results (5-yr, High Durability):
#   Initial current demand: 145.5 A
#   Mean current demand:    149.2 A
#   Final current demand:   152.9 A
#   Required anode count:   ~187 (final current governs)
#   Total gross mass:       ~5.6 MT
```

## Gaps Found
- Source uses ABS Cathodic Protection of Ships 2017, not DNV-RP-F103 or DNV-RP-B401. The
  ABS breakdown factor formula is a multiplicative annual-rate model (fc = fc_initial × (1 + fc_per_year)^t)
  rather than the linear DNV formula. Code must implement ABS Section 2.4.4 Table 4 formula to
  replicate results exactly.
- ABS Table 5 current densities apply to hull/tidal conditions; DNV-RP-F103 tables are for submarine
  pipelines — the structures are conceptually different.
- Source also computed a sensitivity for disbonding assumption (vs. deterioration) — disbonding
  drives gross mass to 10.8 MT at 5 yr; this disbonding path is NOT the adopted design.
- The E2 "vs. programming" workbook is structurally identical to E1; no divergence in computed values
  was found. It confirms the E1 calculation is suitable as the reference.
