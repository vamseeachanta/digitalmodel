---
standard: ABS-CATHODIC-PROTECTION-SHIPS
edition: "2017"
structure_type: floating_storage_terminal
source_type: abstracted_client_calculation
discipline: cathodic_protection
---

# Floating Storage Terminal Hull — ABS Ships 2017 Cathodic Protection Specification (Two-Unit Fleet)

## Source
Standard: ABS, Guidance Notes on Cathodic Protection of Ships, December 2017 (primary)
           DNV-RP-B401, October 2010 amended April 2011 (used for resistivity chart only)
Structure type: Two Floating Storage Terminal (FST) hulls — external hull only
                (Internal tanks, associated foundation structures not in scope)
Document type: CP specification — anode material, manufacture, installation, and sizing
Design life: 5 years
Classification: ABS (American Bureau of Shipping)

## Scope
Sacrificial anode cathodic protection for the submerged external hull of two FST vessels
(referred to as Terminal_1 and Terminal_2 in this abstracted document).
Both vessels are LNG Carrier hulls converted to FSTs by modification of bow and stern sections.
Each FST is permanently moored via four mooring jacket structures.

The two FSTs are sister vessels and are assumed to have identical hull geometry, wetted
surface area, and CP design. The anode specification applies identically to both.

## Design Basis
- CP system type: Sacrificial anode only (SACP); no ICCP in this specification scope
- Anode material: Aluminium alloy (preferred over zinc for environmental reasons and superior
  performance in brackish/low-resistivity water)
- Anode installation: Bolt-on type with stainless steel M15 studs, 35 mm long with S.S. hex
  nut; anodes NOT welded to hull
- Anode spacing: 6 to 8 m (ABS GN Ships 2017, Section 3, Subsection 5.2)
- Anodes uniformly distributed over underwater surface for good current distribution
- Anodes placed away from areas likely to sustain mechanical damage (docking blocks, fenders)

## Input Parameters
| Parameter | Symbol | Value | Unit | Notes |
|-----------|--------|-------|------|-------|
| Design life | tf | 5 | years | Primary design case |
| Max seawater temperature | T_max | 14 | °C | From site survey, max observed |
| Min seawater temperature | T_min | 8 | °C | From site survey |
| Average seawater temperature | T_avg | 11 | °C | Used for design |
| Min salinity | S_min | 15 | ppt | From site survey depth profile |
| Max salinity | S_max | 30 | ppt | From site survey depth profile |
| Average salinity | S_avg | 24.5 | ppt | Used for design |
| Salinity as fraction | — | 2.45 | % | ppt / 10 |
| Seawater resistivity | psw | 0.325 | ohm.m | At 24.5 ppt, 14°C per DNV-RP-B401 Fig 10-1 |
| Anode material density | rho_a | 2750 | kg/m³ | Aluminium alloy (range 2640–2810 kg/m³) |

## Structure Data
| Parameter | Symbol | Value | Unit | Notes |
|-----------|--------|-------|------|-------|
| Minimum draft | — | 4.8 | m | Light load condition |
| Maximum draft (scantling) | — | 11.532 | m | Full load condition |
| Average draft | — | 8.166 | m | Calculated |
| Average submerged wetted area | Ac | 10 778 | m² | From hydrostatics at average draft |

Note: Anodes are predominantly on the bottom shell. Water depth at hull ranges from
approximately 4.79 m (minimum draft) to 11.53 m (full draft). Temperature and salinity
are read at 5 m and 10 m depths from site survey data.

## Environment — Site Survey Data Summary
| Location | Depth (m) | Temperature (°C) | Salinity (ppt) |
|----------|-----------|-----------------|----------------|
| Station A | 5 | 14 | 15 |
| Station A | 10 | 10 | 30 |
| Station B | 5 | 12 | 24 |
| Station B | 10 | 8 | 29 |
| Min | — | 8 | 15 |
| Max | — | 14 | 30 |
| Average | — | 11 | 24.5 |

Salinity range 27–32 ppt at depth, dependable on season and depth.
Near-surface salinity can drop significantly in summer months.

## Resistivity Derivation
Method: DNV-RP-B401 (Oct 2010 amended Apr 2011), Annex A, Figure 10-1
        "Seawater resistivity as a function of temperature for salinity 30 to 40 ppt"
At 24.5 ppt salinity and 14°C maximum temperature: psw = 0.325 ohm.m
This is the design resistivity used in the anode calculations.

## Anode Specification
| Parameter | Value | Unit | Notes |
|-----------|-------|------|-------|
| Anode material | Aluminium-zinc-indium-cadmium alloy | — | Sacrificial, bolt-on |
| Anode size | 650 × (115+135) × 130 | mm | L × (W_core + W_total) × H |
| Anode gross weight per unit | 30.0 | kg | Per supplier datasheet |
| Anode net weight per unit | 29.0 | kg | Approx. (without insert) |
| Installation | Bolt-on, stainless steel studs | — | M15 STL weld stud × 35 mm, S.S. hex nut |
| Anode current capacity | — | A·h/kg | Per ABS GN Ships 2017 |
| Coating compatibility | No inert coating formation | — | Must function in wet/dry cycling |

## Anode Calculations (Section 4.4 — ABS GN Ships 2017)

### Calculation Methodology
The calculation follows ABS Guidance Notes on Cathodic Protection of Ships, 2017:
1. Coating breakdown factors computed (deterioration assumption, not disbonding)
2. Initial, mean, and final current demand calculated
3. Total anode weight computed
4. Anode quantity determined from ABS max spacing requirement (6–8 m)
5. Anode life checked per ABS Section 7, Subsection 2, Clause 7.4
6. Anode current capacity checked for initial geometry
7. Anode current capacity checked for final (depleted) geometry
8. Summary results: number of anode locations and total anode weight

### Basic Design Data
| Description | Symbol | Value | Unit |
|-------------|--------|-------|------|
| Max seawater temperature | T | 14 | °C |
| Design life | tf | 5 | yr |
| Average salinity | S | 24.5 | ppt |
| Salinity as fraction | — | 2.45 | % |
| Seawater resistivity | psw | 0.325 | ohm.m |
| Anode density | rho_a | 2750 | kg/m³ |

### Calculation Results Summary
| Description | Value | Unit | Notes |
|-------------|-------|------|-------|
| Total net anode weight required | 4 713 | kg | Per ABS §7, Sect 2, Cl 7.3 |
| Number of anode locations (ABS max 6–8 m spacing) | 230 | — | — |
| Quantity per location | 1 initially; 2 for final demand | qty/loc | — |
| Anode net weight per unit | 29 | kg | Selected anode |
| Total net anode weight (based on selection) | 6 681 | kg | 230 loc × 1 qty × 29 kg |
| Design life input | 5 | yr | Input |
| Anode life (calculated per ABS §7, §2, Cl 7.4) | 7.1 | yr | > 5 yr: pass |
| Anode qty per location for initial current demand | 1 | qty/loc | Check |
| Anode qty per location for final current demand | 2 | qty/loc | Check |
| Governing anode qty per location | 2 | qty/loc | Final demand governs |
| Total number of anodes | 460 | — | 230 loc × 2 qty |
| Total net weight of anodes | 13 362 | kg | 460 × 29 kg |
| Total net weight of anodes | 13.4 | mt | — |
| Total gross weight of anodes | 14 400 | kg | 460 × 30 kg |
| Total gross weight of anodes | 14.4 | mt | — |

### Final Anode Bill of Materials
```
QTY    DESCRIPTION                                          TOTAL WEIGHT
 460   ALUMINIUM-ZINC INDIUM-CADMIUM ALLOY SACRIFICIAL      14.40 mt
       ANODE, SIZE: 650×(115+135)×130 mm, 30.0 KG/PC
```

## Monitoring
- Method: Periodic diver inspection
- Monitoring requirements per ABS GN Ships 2017 guidelines

## Python cfg dict

```python
cfg = {
    "inputs": {
        "calculation_type": "ABS_CP_SHIPS_2017",
        "design_data": {
            "design_life": 5,                    # years
        },
        "structure": {
            "structure_type": "floating_storage_terminal",
            "num_terminals": 2,                  # Two identical FST hulls
            "draft_min": 4.8,                    # m
            "draft_max": 11.532,                 # m (scantling draft)
            "draft_avg": 8.166,                  # m
            "surface_area": 10778,               # m², average submerged wetted area
            "coating_type": "deterioration",     # not disbonding
            "coating_coverage": 1.0,             # 100% coated
        },
        "environment": {
            "temperature_max": 14,               # deg C
            "temperature_min": 8,                # deg C
            "temperature_avg": 11,               # deg C
            "salinity_min": 15,                  # ppt
            "salinity_max": 30,                  # ppt
            "salinity_avg": 24.5,                # ppt
            "seawater_resistivity": 0.325,       # ohm.m (at 24.5 ppt, 14 deg C)
        },
        "anode": {
            "material": "aluminium_zinc_indium_cadmium_alloy",
            "type": "bolt_on_flush_mount",
            "length": 0.650,                     # m
            "width_core": 0.115,                 # m (core section)
            "width_total": 0.135,                # m (total including shoulders)
            "height": 0.130,                     # m
            "gross_weight": 30.0,                # kg per unit
            "net_weight": 29.0,                  # kg per unit
            "density": 2750,                     # kg/m³
            "installation": "bolt_on_ss_studs",
            "stud_spec": "M15_STL_35mm_SS_hex_nut",
        },
        "layout": {
            "anode_spacing_max": 8,              # m (ABS §3, §5.2 max spacing)
            "anode_spacing_min": 6,              # m
            "number_of_locations": 230,          # from hull geometry + spacing rule
        },
    }
}
# Run: CathodicProtection().router(cfg)
#
# Expected results (5-yr design, 0.325 ohm.m, 10 778 m²):
#   Total net anode mass required:     4 713 kg
#   Anode locations (6-8 m spacing):    230
#   Anode life (calculated):            7.1 yr  (> 5 yr design life: pass)
#   Governing qty per location:         2       (final current demand governs)
#   Total anode count:                  460
#   Total net mass:                    13 362 kg (13.4 mt)
#   Total gross mass:                  14 400 kg (14.4 mt)
#   Anode type: Al-Zn-In-Cd alloy, 650×(115+135)×130 mm, 30.0 kg/pc
```

## Gaps Found
- The ABS GN Ships 2017 current density tables (for brackish water) are not explicitly
  tabulated in this specification; the source document references the ABS standard directly
  for current density values. cathodic_protection.py must implement ABS Table values for
  tidal/submerged hull zones in brackish water.
- The ABS coating breakdown model (multiplicative annual-rate per Table 4, Section 2.4.4)
  must be confirmed as distinct from DNV linear model; source applies ABS method.
- Anode current capacity (A·h/kg) for the specific aluminium-zinc-indium-cadmium alloy
  is not stated numerically in this specification — it is referenced to ABS guidelines.
  A value of 2500 A·h/kg (standard Al alloy per ABS) is expected but should be confirmed
  from ABS Table 3.
- The resistivity derivation uses DNV-RP-B401 (2010) Figure 10-1 for the resistivity look-up,
  not the ABS standard's own resistivity data. This cross-standard usage must be handled in
  the cathodic_protection.py router.
- This specification covers the external hull only. Mooring structure (jacket) CP is not in
  scope and would require a separate calculation under DNV-RP-B401 or DNV-RP-F103.
- The two FST hulls are sister vessels treated as identical; if hull geometry differs
  (e.g., different scantling drafts), the calculation would require separate runs per vessel.
- Anode placement drawings (referenced as separate deliverables) contain the final anode
  distribution map; the spacing rule (6–8 m) from ABS §3 §5.2 is applied but the exact
  anode positions are not reproduced in this abstracted document.
