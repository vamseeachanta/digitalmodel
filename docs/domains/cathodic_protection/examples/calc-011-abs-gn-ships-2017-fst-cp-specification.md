---
standard: ABS-CATHODIC-PROTECTION-SHIPS
edition: "2017"
structure_type: floating_storage_terminal_hull
source_type: abstracted_client_calculation
discipline: cathodic_protection
---

# Floating Storage Terminal Hull — ABS GN Ships 2017 Cathodic Protection Specification

## Source
Standard: ABS, Guidance Notes on Cathodic Protection of Ships, December 2017 (primary);
           DNV-RP-B401, October 2010 amended April 2011 (used for resistivity chart, Annex A
           Figure 10-1 only)
Structure type: Floating Storage Terminal (FST) hull — external hull, submerged zone only
                (Internal tanks and foundation structures not in scope)
Document type: CP specification — anode material, manufacture, installation, and sizing
Document reference: CP-CALC-11
Design life: 5 years
Classification: ABS (American Bureau of Shipping)

## Notes on E5 source
This document is a CP specification covering the external hull sacrificial anode system for
two FST hulls (referred to as FST Hull 1 and FST Hull 2 in this abstracted document) at a
coastal LNG facility. The vessels are sister LNG Carrier hulls converted to FSTs by
modification of bow and stern sections. Each FST is permanently moored via four mooring
jacket structures.

The primary calculation is in Section 4.4 (anode calculations, ABS method) and Section 4.5
(anode dimensions, weight, arrangement, and quantity). The design resistivity is derived
from DNV-RP-B401 (Oct 2010 amended Apr 2011) Annex A Figure 10-1 (seawater resistivity vs
temperature for 30–40 ppt salinity) — this cross-standard usage is noted in Gaps Found.

Appendix D of the source document is titled "CTR2-CP Calculation RB1" and contains the
same ABS calculation spreadsheet (4 pages). This is the same calculation as E1/E2 Excel
workbooks already abstracted in calc-001-abs-gn-ships-2017-fst-hull.md. The Appendix D
values are identical to Section 4.4 results. Do not re-abstract Appendix D; see calc-001
for the full E1/E2 cross-reference.

Key difference from calc-001: calc-001 uses psw = 0.2547 ohm.m (25 ppt, 5-yr design),
while this specification uses psw = 0.325 ohm.m (24.5 ppt at 14°C max temperature per
DNV B401 Fig 10-1). The surface area used here is 10 778 m² (average draft), which matches
calc-001. The coating durability in this document is "High" (1%/yr initial, 1%/yr annual).

## Input Parameters
| Parameter | Symbol | Value | Unit | Notes |
|-----------|--------|-------|------|-------|
| Design life | tf | 5 | yr | Primary design case |
| Max seawater temperature | T_max | 14 | °C | From site survey; used for resistivity |
| Min seawater temperature | T_min | 8 | °C | From site survey |
| Average seawater temperature | T_avg | 11 | °C | — |
| Min salinity | S_min | 15 | ppt | From site depth profile |
| Max salinity | S_max | 30 | ppt | From site depth profile |
| Average salinity | S_avg | 24.5 | ppt | Design salinity |
| Salinity as fraction | — | 2.45 | % | ppt / 10 |
| Seawater resistivity | psw | 0.325 | ohm.m | At 24.5 ppt, 14°C per DNV-RP-B401 Fig 10-1 |
| Anode material density | rho_a | 2750 | kg/m³ | Aluminium alloy (range 2640–2810 kg/m³) |

## Structure Data
| Parameter | Symbol | Value | Unit | Notes |
|-----------|--------|-------|------|-------|
| Minimum draft | — | 4.8 | m | Light load (light load calcs) |
| Maximum draft (scantling) | — | 11.532 | m | Full load condition |
| Average draft | — | 8.166 | m | Calculated |
| Average submerged wetted area | Ac | 10 778 | m² | From hydrostatics at average draft |

Note: Anodes are predominantly on the bottom shell. Water depth at hull ranges from
approximately 4.79 m (minimum draft) to 11.53 m (full draft). Temperature and salinity
read at 5 m and 10 m depths from site survey data.

## Environment — Site Survey Data Summary
| Station | Depth (m) | Temperature (°C) | Salinity (ppt) |
|---------|-----------|-----------------|----------------|
| Station A | 5 | 14 | 15 |
| Station A | 10 | 10 | 30 |
| Station B | 5 | 12 | 24 |
| Station B | 10 | 8 | 29 |
| Min | — | 8 | 15 |
| Max | — | 14 | 30 |
| Average | — | 11 | 24.5 |

## Resistivity Derivation
Method: DNV-RP-B401 (Oct 2010 amended Apr 2011), Annex A, Figure 10-1
        "Seawater resistivity as a function of temperature for salinity 30 to 40 ppt"
At 24.5 ppt salinity and 14°C maximum temperature: psw = 0.325 ohm.m
This is the design resistivity used in all anode calculations.

## Coating Breakdown Data (ABS GN Ships 2017, Section 2, Table 4)
| Parameter | Value | Unit | Notes |
|-----------|-------|------|-------|
| Coating system durability | High | — | Selected |
| Initial coating breakdown factor (fci) | 1 | % | ABS Table 4, High durability |
| Initial breakdown duration (di) | 2 | yr | ABS Table 4 |
| Yearly coating breakdown factor (fc) | 1 | %/yr | ABS Table 4, High durability |
| Annual multiplier (fca = 1 + fc) | 1.01 | 1/yr | — |
| Mean coating breakdown factor (fcm) | 1.53 | % | avg(fca, fcf) |
| Final coating breakdown factor (fcf) | 2.05 | % | (1 + fci/100)^di × (1+fc/100)^(tf-di) |

Note: fcf formula: (1 + 0.01/100)^2 × (1 + 0.01)^(5-2) = 1.0001^2 × 1.01^3 ≈ 2.05%

## Current Density (ABS GN Ships 2017, Section 2, Table 3 and Table 5)
| Parameter | Value | Unit | Notes |
|-----------|-------|------|-------|
| Design current density for coated steel, ici (avg Table 7, 20) | 13.5 | mA/m² | ABS §2, Table 3 |
| Design current density for uncoated steel, icdu (avg 150, 250) | 200 | mA/m² | ABS §2, Table 3 |
| Design current density (icdcd) assuming deterioration | 27.7 | mA/m² | icdc × fcf |
| Design current density assuming disbonding | 210 | mA/m² | fcfl × icdu |
| Assumption | Deterioration (not disbonding) | — | — |
| Mean current density (icm) | 20.6 | mA/m² | avg(ici, icf) |

## Calculation Results (from source — Section 4.4 / Appendix D, 5-yr, 0.325 ohm.m)

### Current Demand for Structure
| Stage | Formula | Value | Unit |
|-------|---------|-------|------|
| Initial current demand (Ici) | Ac × ici | 146 | A |
| Mean current demand (Icm) | Ac × icm | 222 | A |
| Final current demand (Icf) | Ac × icf | 298 | A |

### Anode Weight Calculation
| Parameter | Value | Unit | Reference |
|-----------|-------|------|-----------|
| Anode current capacity, Q | 2 500 | A·h/kg | ABS §2, Table 4 |
| Anode utilisation factor (u) | 0.825 | — | ABS §2, §7.3 |
| Total net anode weight required (NWm, based on Icm) | 4 713 | kg | Icm × tf × 8760 / (Q × u) |
| Total net anode weight required | 4.7 | mt | — |

### Number of Anode Locations
| Parameter | Value | Unit | Reference |
|-----------|-------|------|-----------|
| Number of locations (ABS max spacing 6–8 m) | 230 | — | ABS §3, §5.2 |
| Anode net weight required per location | 20.5 | kg | NWm / No.Loc |
| Quantity provided per location (initial) | 1 | qty/loc | — |

### Anode Provided (Selected Anode)
| Parameter | Symbol | Value | Unit | Notes |
|-----------|--------|-------|------|-------|
| Anode length | L | 0.65 | m | — |
| Anode width | W | 0.125 | m | — |
| Anode height | H | 0.13 | m | — |
| Anode volume | V = L×W×H | 0.0106 | m³ | — |
| Anode net weight each | wN | 29 | kg | V × rho_a |
| Insert weight estimate per anode | Wi | 2.26 | kg | — |
| Anode gross weight each | wG | 31.3 | kg | — |
| Gross to net ratio | wG/wN | 1.08 | — | — |

### Anode Life Check (ABS §2, §7.4)
| Parameter | Value | Unit |
|-----------|-------|------|
| Anode consumption rate (E) | 3.5 | kg/(A·yr) |
| Anode average current output during lifetime (Is) | 0.9649 | A |
| Anode life (Tanode) | 7.1 | yr |
| Design life | 5 | yr |
| Check (Tanode > tf) | Pass | — |

### Anode Weight at End of Life (ABS §2, §7.5)
| Parameter | Value | Unit |
|-----------|-------|------|
| Anode net weight initial (wNi) | 29 | kg |
| Anode utilisation factor | 0.825 | — |
| Anode net weight final (wNf = wNi × (1-u)) | 5.1 | kg |

### Anode End of Life Shape — Slender Anode (ABS §2, §7.5.2a)
| Parameter | Value | Unit |
|-----------|-------|------|
| Anode length initial (Li) | 0.65 | m |
| Anode length final (Lf = Li - 0.1×u×Li) | 0.60 | m |
| Anode core cross-sectional area (Xcore) | 0.0163 | m² |
| Anode cross-sectional area final (Xfinal) | 0.026 | m² |
| End-of-life radius (rfinal, long flush mounted §7.5.2b) | 0.1286 | m |

### Anode Current Capacity Check — Initial (ABS §2, §6.2)
| Parameter | Value | Unit | Notes |
|-----------|-------|------|-------|
| Mild steel protection potential | -0.80 | V | ABS §2, Table 1 |
| Maximum negative potential | -1.10 | V | ABS §2, Table 1 |
| Potential difference ΔE | 0.30 | V | Vs - Va |
| Length / Width (L/W) | 5.2 | — | — |
| Arithmetic mean of length and width (Si) | 0.40 | m | avg(Li, Wi) |
| Anode resistance Rai (long flush L ≥ 4W) | 0.419 | ohm | Rai = psw / (2 × Si) |
| Individual anode current output initial (Iaoi) | 0.72 | A | ΔE / Rai |
| Quantity per location for initial demand | 1 | qty/loc | — |
| Total anodes (initial) | 230 | — | — |
| Total anode current output initial (Icoi) | 165 | A | qty × Iaoi |
| Initial current demand (Ici) | 146 | A | — |
| Check (Icoi ≥ Ici) | Pass | — | — |

### Anode Current Capacity Check — Final (ABS §2, §6.2)
| Parameter | Value | Unit | Notes |
|-----------|-------|------|-------|
| Arithmetic mean of length and width final (Sf) | 0.4268 | m | avg(Lf, 2×rfinal) |
| Anode resistance Raf (long flush L ≥ 4W) | 0.3807 | ohm | Raf = psw / (2 × Sf) |
| Individual anode current output final (Iaof) | 0.788 | A | ΔE / Raf |
| Quantity per location for final demand | 2 | qty/loc | — |
| Total anodes (final) | 460 | — | 230 loc × 2 qty |
| Total anode current output final (Icof) | 329 | A | qty × Iaof |
| Final current demand (Icf) | 298 | A | — |
| Check (Icof ≥ Icf) | Pass | — | — |

### Summary — Number of Anodes Selected
| Parameter | Value | Unit | Notes |
|-----------|-------|------|-------|
| Total net weight required | 4 713 | kg | — |
| Number of anode locations | 230 | — | ABS 6–8 m spacing |
| Governing qty per location | 2 | qty/loc | Final current demand governs |
| Total number of anodes | 460 | — | 230 × 2 |
| Total net weight of anodes | 13 362 | kg | 460 × 29 kg |
| Total net weight of anodes | 13.4 | mt | — |
| Total gross weight of anodes | 14 400 | kg | 460 × 30 kg |
| Total gross weight of anodes | 14.4 | mt | — |
| Anode life (calculated) | 7.1 | yr | > 5 yr design life: pass |

## Anode Bill of Materials (Section 4.5)
```
QTY    DESCRIPTION                                             TOTAL WEIGHT
 460   ALUMINIUM-ZINC INDIUM-CADMIUM ALLOY SACRIFICIAL          14.40 mt
       ANODE, SIZE: 650×(115+135)×130 mm, WEIGHT: 30.0 KG/PC
```

## Anode Installation Requirements
- Anode spacing: 6 to 8 m (ABS GN Ships 2017, Section 3, Subsection 5.2)
- Anodes uniformly distributed over underwater surface
- Bolt-on type with M15 STL weld stud × 35 mm with S.S. hex nut
- Anodes NOT welded to the hull
- Placed away from areas likely to sustain mechanical damage (docking blocks, fenders)

## Monitoring
- Method: Periodic diver inspection
- Monitoring requirements per ABS GN Ships 2017 guidelines

## Linkage to calc-001
Appendix D of the source document ("CTR2-CP Calculation RB1") contains the same ABS
aluminium anode spreadsheet. The basic design data (T=14°C, S=24.5 ppt, psw=0.325 ohm.m,
Ac=10 778 m², tf=5 yr) matches this section 4.4 exactly. calc-001 was abstracted from the
same underlying Excel workbook (E1/E2 pair). The only numeric difference between calc-001
and this document is seawater resistivity: calc-001 uses 0.2547 ohm.m (25 ppt), while
here the design resistivity is 0.325 ohm.m (24.5 ppt at 14°C from DNV B401 Fig 10-1).

## Python cfg dict

```python
cfg = {
    "inputs": {
        "calculation_type": "ABS_gn_ships_2018",
        # ABS_gn_ships_2018 is the existing route in cathodic_protection.py.
        # Source standard is ABS GN Ships December 2017; route name reflects 2018 offshore
        # publication which uses the same methodology.
        "design_data": {
            "design_life": 5,                    # years
            "seawater_max_temperature": 14,      # deg C (max observed)
        },
        "environment": {
            "seawater": {
                "resistivity": {"input": 0.325},   # ohm.m (24.5 ppt, 14 deg C)
            },
        },
        "structure": {
            "steel_total_area": 10778.0,           # m², average submerged wetted area
            "area_coverage": 100.0,                # % coated
            "coating_initial_breakdown_factor": 1.0,    # % (High durability, ABS Table 4)
            "coating_initial_breakdown_duration": 2.0,  # years
            "coating_yearly_breakdown_factor": 1.0,     # %/yr (High durability)
            "coating_breakdown_factor_max": 2.05,  # % final (5-yr, High durability)
        },
        "design_current": {
            "coated_steel_mA_m2": 13.5,            # mA/m² (ABS §2, Table 3)
            "uncoated_steel_mA_m2": 200.0,         # mA/m²
        },
        "anode": {
            "material": "aluminium",
            "protection_potential": 0.8,           # V (magnitude, Ag/AgCl)
            "closed_circuit_anode_potential": -1.09, # V vs Ag/AgCl
            "anode_Utilisation_factor": 0.825,
            "physical_properties": {
                "net_weight": 29.0,                # kg
            },
            "geometry": {
                "type": "long_flush",
                "length_m": 0.65,                  # m
                "width_m": 0.125,                  # m
            },
        },
    }
}
```

## Code Validation

```python
# Run: CathodicProtection().router(cfg)
# Route: ABS_gn_ships_2018 (existing)
#
# Expected results (5-yr, 0.325 ohm.m, 10 778 m², High durability coating):
#   Initial coating breakdown factor (fci):     1.0%
#   Final coating breakdown factor (fcf):       2.05%
#   Mean coating breakdown factor (fcm):        ~1.53%
#   Design current density ici:                13.5 mA/m²  (coated, tidal)
#   Mean current density icm:                  20.6 mA/m²
#   Final current density icf:                 27.7 mA/m²
#   Initial current demand (Ici):               146 A
#   Mean current demand (Icm):                  222 A
#   Final current demand (Icf):                 298 A
#   Total net anode mass required:            4 713 kg
#   Number of anode locations (6-8 m):          230
#   Anode life calculated:                      7.1 yr  (> 5 yr: pass)
#   Governing qty per location:                 2       (final current governs)
#   Total anode count:                          460
#   Total net anode mass:                    13 362 kg (13.4 mt)
#   Total gross anode mass:                  14 400 kg (14.4 mt)
#   Anode resistance (initial, Rai):            0.419 ohm
#   Individual anode output initial (Iaoi):     0.72 A
#   Total initial current output (230 anodes):  165 A  (> 146 A: pass)
#   Anode resistance (final, Raf):              0.3807 ohm
#   Individual anode output final (Iaof):       0.788 A
#   Total final current output (460 anodes):    329 A  (> 298 A: pass)
#
# NOTE: The cfg key names above must match the ABS_gn_ships_2018() method signature
# in cathodic_protection.py. Cross-check against test_abs_cathodic_protection_calcs.py.
```

## Gaps Found
- The design resistivity (0.325 ohm.m) is derived from DNV-RP-B401 (2010) Annex A Figure
  10-1, not from an ABS-specific resistivity table. This cross-standard usage must be handled
  in the router — the ABS route currently expects resistivity as a direct input, which is
  correct here.
- The anode current capacity (A·h/kg) for the specific aluminium-zinc-indium-cadmium alloy
  is 2 500 A·h/kg per ABS Section 7, Subsection 2, Table 4. The existing test uses an
  implicit value via the ABS table look-up; confirm cathodic_protection.py uses 2 500 A·h/kg
  for aluminium alloy (not 2 000 A·h/kg used in the DNV B401 example calc).
- The ABS coating breakdown model (multiplicative annual-rate per ABS §2, Table 4) is
  distinct from the DNV linear model. Confirm cathodic_protection.py implements the
  ABS multiplicative formula: fcf = (1 + fci/100)^di × (1 + fc/100)^(tf - di).
- Anode current capacity (Q = 2 500 A·h/kg) is referenced to ABS §2, Table 4, but the
  exact alloy composition (Al-Zn-In-Cd) is not in the ABS table — it matches the generic
  aluminium alloy capacity. Confirm with supplier datasheet if a non-standard value applies.
- The specification covers the external hull only. Mooring jacket CP is not in scope and
  would require a separate calculation under DNV-RP-B401 or DNV-RP-F103.
- The two FST hulls are sister vessels treated as identical. If hull geometry were to differ
  (e.g., different scantling drafts post-conversion), separate calculations per vessel would
  be required.
- Anode placement drawings (separate deliverables) contain the final anode distribution map;
  the 6–8 m spacing rule from ABS §3 §5.2 is applied but exact positions are not reproduced
  here.
- Appendix D (CTR2-CP Calculation RB1) is identical to the E1/E2 Excel workbooks abstracted
  in calc-001. No additional engineering values found in Appendix D beyond Section 4.4.
