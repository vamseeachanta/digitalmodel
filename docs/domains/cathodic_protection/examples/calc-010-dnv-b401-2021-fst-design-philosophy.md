---
standard: DNV-RP-B401
edition: "2021"
structure_type: floating_storage_terminal_hull
source_type: abstracted_client_calculation
discipline: cathodic_protection
---

# Floating Storage Terminal Hull — DNV-RP-B401 2021 CP Design Philosophy and SACP Sensitivity Study

## Source
Standard: DNV-RP-B401, Cathodic Protection Design, May 2021 (primary);
           ABS Guidance Notes on Cathodic Protection of Ships, December 2017 (co-reference);
           BS EN 16222 Cathodic Protection of Ship Hulls, October 2012 (informative);
           CSA Z662, 9th edition 2023 (regulatory context);
           CSA EXP276.2, 1st edition 2019 (regulatory context);
           NACE SP0108, NACE AMPP SP0176 June 2022 (informative)
Structure type: Floating Storage Terminal (FST) hull — tidal, splash, and submerged zones
Document type: Design philosophy technical note (CP system selection + preliminary SACP sizing)
Document reference: CP-CALC-10
Design life: 25 years (primary sensitivity); 40 years (secondary sensitivity)

## Notes on E4 source
This document is a preliminary technical note evaluating three CP system options for an FST
hull deployed at a coastal LNG facility in brackish water (seawater salinity 1–32 ppt
depending on depth and season). The document is not a final certified CP design — it provides
design selection rationale and sensitivity calculations.

Three candidate CP systems were assessed: (1) Sacrificial Anode CP (SACP), (2) Impressed
Current CP (ICCP), and (3) Hybrid SACP + ICCP. The recommendation is ICCP as the main
system, with SACP as supplementary for periods when ICCP is non-operational (vessel
construction, transit, power-off). Magnesium anodes are noted for initial polarisation
under high-resistivity (near-freshwater) conditions.

Appendix 1 contains an example SACP calculation (labelled FST Hull, Draft 15m, Seawater
Exposed) using DNV-RP-B401 §7.8–7.9 methodology. That calculation is the primary extraction
target and is captured in full below. The design code for the example calc is identified as
DNV-RP-B401 (references to §7.9.3, §7.9.4, §7.9.5, §7.8.3 appear explicitly in the source
table). The current density values used (200 mA/m² bare steel, initial/mean/final) match ABS
GN Ships 2017 rather than DNV-RP-B401 Table 10-1 — this is noted in Gaps Found.

Regulatory context: ABS Class (statutory), IMO Gas Carrier Code (via ABS), CSA Z662 (no
specific hull CP requirements; coating selection provisions applicable), CSA EXP276.2
§6.5.2.2 permits SACP, ICCP, or combination per operator preference.

## Input Parameters
| Parameter | Symbol | Value | Unit | Notes |
|-----------|--------|-------|------|-------|
| Seawater/buried max temperature | T | 10 | °C | Design value for SACP preliminary calc |
| Design life (primary) | tf | 40 | yr | Primary sensitivity; 25 yr also computed |
| Seawater resistivity (10 ppt equiv.) | psw | 0.5875 | ohm.m | Design case for example calc |
| Seawater resistivity (31 ppt equiv.) | psw | 0.21 | ohm.m | Normal seawater sensitivity |
| Seawater resistivity (1 ppt equiv.) | psw | 5.076 | ohm.m | Worst-case near-freshwater |
| Protection potential | Ec | -0.80 | V vs Ag/AgCl | Per ABS CP guidance note |
| Closed circuit anode potential | E°a | -1.09 | V vs Ag/AgCl | Alloy A3 per ABS CP guidance note |
| Anode capacity | epsilon | 2000 | A·h/kg | Aluminium alloy A3 |
| Anode density | rho_a | 2750 | kg/m³ | Aluminium alloy |
| Coating type | — | Glass Flake Reinforced Epoxy | — | High integrity marine coating |
| Steel coated area | — | 100 | % | All wetted surface coated |
| Hull surface area at 15 m draft | Ac | 13 446.04 | m² | Max submerged surface (conservative) |
| Design initial current density | ici | 200 | mA/m² | Per ABS CP guidance note (bare steel) |
| Design mean current density | icm | 200 | mA/m² | Per ABS CP guidance note |
| Design final current density | icf | 200 | mA/m² | Per ABS CP guidance note |
| Initial coating breakdown factor | fci | 0.02 | — | 2%; high integrity coating |
| Yearly coating breakdown factor | — | 0.005 | /yr | 0.5%/yr |
| Mean coating breakdown factor (40 yr) | fcm | 0.12 | — | Derived: fci + 0.5×tf×yearly |
| Final coating breakdown factor (40 yr) | fcf | 0.22 | — | Derived: fci + tf×yearly |
| Anode utilisation factor | u | 0.85 | — | Long flush-mounted anode |
| Anode mean length | — | 1.400 | m | Long flush-mounted type |
| Anode width | — | 0.150 | m | — |
| Anode height | — | 0.060 | m | — |
| Anode net weight | wN | 27.5 | kg | — |
| Anode gross weight | wG | 30.25 | kg | ~10% insert/core allowance |

## Environment — Seawater Resistivity vs Salinity
| Salinity (ppt) | Conductivity (mS/cm) | Resistivity (ohm.cm) | Resistivity (ohm.m) |
|----------------|---------------------|----------------------|---------------------|
| 0.1 | 0.21 | 4 762 | 47.62 |
| 1 | 1.97 | 507.6 | 5.076 |
| 10 | 17.02 | 58.75 | 0.5875 |
| 25 | 39.26 | 25.47 | 0.2547 |
| 31 | 47.62 | 21.0 | 0.21 |

Site salinity varies 1 to 32 ppt depending on depth and season. Lowest salinity period
occurs June–July (near-freshwater). FST vessel summer draft approximately 11.5 m.

## Hull Surface Area vs Draft (Table 4 from source)
| Draft (m) | Volume (m³) | Cp | Cb | WS Area (m²) |
|-----------|-------------|----|----|---------------|
| 1.0 | 6 530 | 0.767 | 0.746 | 7 105 |
| 5.0 | 37 364 | 0.845 | 0.835 | 9 213 |
| 10.0 | 78 922 | 0.887 | 0.882 | 11 361 |
| 13.0 | 104 628 | 0.903 | 0.899 | 12 616 |
| 14.0 | 113 282 | 0.908 | 0.904 | 13 032 |
| 15.0 | 121 969 | 0.912 | 0.908 | 13 446 |

Reference draft for preliminary CP calculation: 15 m (maximum surface, conservative).

## Coating Breakdown Factors (Appendix 1, Example Calc)
| Stage | CBF value | Effective current density (mA/m²) |
|-------|-----------|-----------------------------------|
| Initial | 0.02 (2%) | 4 |
| Mean | 0.12 (12%) | 24 |
| Final | 0.22 (22%) | 44 |

## Calculation Results (from source — Appendix 1, 40-yr, 0.5875 ohm.m)

### Required Current Demand
| Stage | Current (A) |
|-------|------------|
| Initial (Ici) | 53.784 |
| Mean (Icm) | 322.705 |
| Final (Icf) | 591.626 |

### Anode Mass and Count
| Parameter | Value | Unit | Reference |
|-----------|-------|------|-----------|
| Total required anode mass (mean current governs) | 66 515 | kg | DNV-RP-B401 §7.8.3 |
| Number of anodes based on mass (mean current) | 2 419 | — | — |
| Number of anodes based on initial current | 70 | — | — |
| Number of anodes based on final current | 911 | — | — |
| Governing anode count | 2 419 | — | Mass governs |
| Individual anode current capacity (Ca) | 46 750 | A·h | — |
| Total anode current capacity (Ca × N) | 113 088 250 | A·h | — |
| Required current capacity | 113 075 818 | A·h | — |
| Anode life check | 40 | yr | Verified |

### Anode Resistance and Initial Current Output (Lloyd's Formula)
| Parameter | Value | Unit |
|-----------|-------|------|
| Anode equivalent radius | 0.067 | m |
| Anode resistance Ra initial (Lloyd's formula) | 0.379 | ohm |
| Individual anode current output (initial) | 0.77 | A |
| Total initial current output (2419 anodes) | 1 850.6 | A |
| Initial current check vs demand (53.784 A) | Pass | — |

### Depleted Anode Geometry (per DNV-RP-B401)
| Parameter | Symbol | Value | Unit | DNV Section |
|-----------|--------|-------|------|-------------|
| Depleted anode mass | Wf | 4.13 | kg | §7.9.3 |
| Depleted anode volume | Vf | 0.00150 | m³ | §7.9.5 |
| Depleted anode length | Lf | 1.260 | m | §7.9.4 |
| Depleted anode radius (semi-cylindrical) | rf | 0.03 | m | §7.9.4 |
| Depleted anode resistance | Ra_final | 0.45 | ohm | — |
| Final anode current output per anode | — | 0.65 | A | — |
| Final total anode current output (2419) | — | 1 570.26 | A | — |
| Final current check vs demand (591.626 A) | Pass | — | — |

### Total Anode Summary
| Parameter | Value | Unit |
|-----------|-------|------|
| NUMBER OF ANODES SELECTED | 2 419 | — |
| Total net mass of anodes | 66 522.5 | kg | — |
| Total gross mass of anodes | 73 174.8 | kg | — |
| Total gross mass of anodes | 73.17 | MT | — |

## SACP Sensitivity Results (Table 5 from source)
| Design Life | Resistivity (ohm.m) | Salinity equiv. | Gross Mass (kg) | No. Anodes |
|-------------|---------------------|-----------------|-----------------|------------|
| 25 yr | 5.076 | 1 ppt | 157 027 | 5 191 |
| 25 yr | 0.5875 | 10 ppt | 31 460 | 1 040 |
| 25 yr | 0.21 | 31 ppt | 31 460 | 1 040 |
| 40 yr | 5.076 | 1 ppt | 238 219 | 7 875 |
| 40 yr | 0.5875 | 10 ppt | 73 175 | 2 419 |
| 40 yr | 0.21 | 31 ppt | 73 175 | 2 419 |

Key observation: At 1 ppt salinity and 40 yr, SACP requires 7 875 anodes — impractical on hull
surface. At 10 ppt and above, mass and count stabilise (resistivity no longer limiting factor).
This is a primary driver for ICCP system selection.

## ICCP System Parameters (from Electrical Design Philosophy, informative)
- 12 ICCP anodes distributed along hull (titanium elements coated with Platinum)
- Anodes encapsulated in chlorine-resistant resin carrier
- Power supply: 600 V, 3-phase; two separate supplies (aft 6 anodes, forward 6 anodes)
- 4 reference electrodes for hull potential measurement
- Automatically controlled to adapt to changing resistivity and tidal conditions
- Control unit: potential-controlled computerised power supply with alarm capability

## Python cfg dict

```python
cfg = {
    "inputs": {
        "calculation_type": "DNV_RP_B401_2021",
        # NOTE: DNV_RP_B401_2021 route is NOT yet implemented in cathodic_protection.py.
        # See Code Validation and Gaps Found sections.
        "design_data": {
            "design_life": 40,                   # years (primary sensitivity case)
            "design_life_secondary": 25,          # years (secondary sensitivity)
            "seawater_max_temperature": 10,       # deg C
        },
        "structure": {
            "structure_type": "floating_storage_terminal_hull",
            "surface_area": 13446.04,            # m², 15 m draft (maximum, conservative)
            "surface_area_avg_draft": 10778.0,   # m², average draft (approx.)
            "coating_type": "glass_flake_reinforced_epoxy",
            "coating_coverage": 1.0,             # 100% coated
        },
        "environment": {
            "seawater": {
                "resistivity": {"input": 0.5875},  # ohm.m (10 ppt, design case)
            },
            "seawater_resistivity_worst": 5.076,   # ohm.m (1 ppt, near-freshwater)
            "seawater_resistivity_normal": 0.21,   # ohm.m (31 ppt, full saline)
        },
        "protection": {
            "protection_potential": -0.80,         # V vs Ag/AgCl
            "closed_circuit_anode_potential": -1.09, # V vs Ag/AgCl (Al alloy A3)
        },
        "structure_coating": {
            "coating_initial_breakdown_factor": 2.0,    # % initial (fci = 0.02)
            "coating_initial_breakdown_duration": 2.0,  # years (assumed standard)
            "coating_yearly_breakdown_factor": 0.5,     # %/yr (0.005 per year)
            # Derived: fcm(40yr) = 0.12 (12%), fcf(40yr) = 0.22 (22%)
        },
        "design_current": {
            "coated_steel_mA_m2": 200.0,           # mA/m² bare steel basis (ABS table)
            "uncoated_steel_mA_m2": 200.0,         # mA/m²
            # Effective with coating: initial=4 mA/m², mean=24 mA/m², final=44 mA/m²
        },
        "anode": {
            "material": "aluminium_alloy_A3",
            "type": "long_flush_mount",
            "capacity": 2000,                      # A·h/kg
            "density": 2750,                       # kg/m³
            "anode_Utilisation_factor": 0.85,
            "closed_circuit_anode_potential": -1.09, # V vs Ag/AgCl
            "geometry": {
                "type": "long_flush",
                "length_m": 1.400,                 # m
                "width_m": 0.150,                  # m
            },
            "physical_properties": {
                "net_weight": 27.5,                # kg
                "gross_weight": 30.25,             # kg
            },
        },
    }
}
```

## Code Validation

```python
# Run: CathodicProtection().router(cfg)
#
# ROUTE NOT YET IMPLEMENTED: calculation_type "DNV_RP_B401_2021" does not exist in
# cathodic_protection.py. The router will raise a routing error or fall through to
# an unrecognised-type handler.
#
# Expected results once implemented (40-yr, 0.5875 ohm.m, 13 446.04 m²):
#   Effective initial current density:    4 mA/m²  (200 × 0.02)
#   Effective mean current density:      24 mA/m²  (200 × 0.12)
#   Effective final current density:     44 mA/m²  (200 × 0.22)
#   Initial current demand (Ici):        53.784 A
#   Mean current demand (Icm):          322.705 A
#   Final current demand (Icf):         591.626 A
#   Required anode mass:                66 515 kg  (mean current governs, §7.8.3)
#   Number of anodes (mass basis):       2 419
#   Anode resistance (Lloyd's, initial):  0.379 ohm
#   Individual anode output (initial):    0.77 A
#   Total initial current output:      1 850.6 A  (> 53.784 A: pass)
#   Depleted anode mass (§7.9.3):          4.13 kg
#   Depleted anode length (§7.9.4):        1.260 m
#   Depleted anode resistance:             0.45 ohm
#   Final anode output per anode:          0.65 A
#   Total final current output (2419):  1 570.26 A  (> 591.626 A: pass)
#   Total gross anode mass:             73 174.8 kg  (73.17 MT)
#
# Sensitivity (40-yr, 5.076 ohm.m / 1 ppt):
#   Gross anode mass: 238 219 kg, 7 875 anodes — impractical, ICCP preferred
```

## Gaps Found
- **DNV_RP_B401_2021 route not implemented**: cathodic_protection.py has no route for
  `DNV_RP_B401_2021`. This is the first B401-2021 calculation in the library. The route
  must be added before this cfg can be executed.
- **Current density table conflict**: The example calculation uses 200 mA/m² bare steel
  current density values from ABS GN Ships 2017, not DNV-RP-B401 2021 Table 10-1 values.
  The DNV_RP_B401_2021 route must clarify which standard's current density table applies
  for FST hull (ship hull) geometry, or accept user-supplied values.
- **Coating breakdown formula**: The source uses a linear annual-rate model
  (fcf = fci + tf × yearly_rate), consistent with DNV-style. ABS uses a multiplicative
  model. The B401-2021 route must confirm which formula is applied.
- **ICCP sizing not calculated**: Only described qualitatively (12 anodes, TRU rating,
  reference electrodes). ICCP sizing would require a separate module.
- **Hybrid (SACP + ICCP) sizing**: Magnesium anode quantity for initial polarisation not
  computed. Not supported by current cathodic_protection.py.
- **Environmental resistivity input**: The worst-case 1 ppt sensitivity (5.076 ohm.m)
  represents an environmental envelope, not the adopted design condition. The router should
  handle multiple resistivity sensitivity runs as a batch input.
- **DNV-RP-B401 2021 differences from 2011**: The 2021 edition introduced Category II PTFE
  coating systems, updated inspection requirements, and connection resistance as a mandatory
  design input. These 2021-specific features are not reflected in the example calculation
  (which appears to use 2011-equivalent methodology) and are not supported in
  cathodic_protection.py.
