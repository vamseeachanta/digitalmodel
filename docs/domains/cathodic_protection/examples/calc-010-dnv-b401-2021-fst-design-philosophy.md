---
standard: DNV-RP-B401
edition: "2021"
structure_type: floating_storage_terminal
source_type: abstracted_client_calculation
discipline: cathodic_protection
---

# Floating Storage Terminal Hull — DNV-RP-B401 2021 CP Design Philosophy and SACP Sensitivity Study

## Source
Standard: DNV-RP-B401, Cathodic Protection Design, May 2021 (primary);
           ABS Guidance Notes on Cathodic Protection of Ships, December 2017 (co-reference);
           BS EN 16222 Cathodic Protection of Ship Hulls, October 2012 (informative)
Structure type: Floating Storage Terminal (FST) hull — tidal, splash, and submerged zones
Document type: Design philosophy technical note (CP system selection + preliminary SACP sizing)
Design life: 25 years (primary sensitivity); 40 years (secondary sensitivity)

## Purpose
This document defines design consideration, selection criteria, and design philosophy for the
Cathodic Protection (CP) system for an FST hull. Three candidate CP methods were evaluated:

1. Sacrificial Anode CP system (SACP)
2. Impressed Current CP system (ICCP)
3. Hybrid combination of SACP and ICCP

Recommendation: ICCP as main system; SACP as supplementary for periods when ICCP is
non-operational (construction, transit, power-off scenarios). Magnesium anodes considered
for initial polarisation under high-resistivity (near-freshwater) conditions.

## Regulatory and Class Context
- Classification society: ABS (American Bureau of Shipping)
- Statutory: IMO Gas Carrier Code requirements covered by ABS Class
- Canadian regulatory: CSA Z662 (Oil and Gas Pipeline Systems) — no specific CP requirements
  for marine vessels, but coating selection provisions (Table 9) applicable
- CSA EXP276.2 clause 6.5.2.2: SACP, ICCP, or combination permitted per operator preference

## Design Zones
| Zone | Protection Method |
|------|-------------------|
| Atmospheric | Anti-corrosion coating only |
| Splash | Corrosion allowance + coating |
| Submerged | Corrosion allowance + coating + passive CP system |

## Input Parameters
| Parameter | Symbol | Value | Unit | Notes |
|-----------|--------|-------|------|-------|
| Seawater temperature | T | 10 | °C | Design value for SACP preliminary calc |
| Design life (primary) | tf | 40 | years | Sensitivity; 25 yr also computed |
| Seawater resistivity (10 ppt) | psw | 0.5875 | ohm.m | Equiv. 10 ppt salinity |
| Seawater resistivity (31 ppt) | psw | 0.21 | ohm.m | Equiv. 31 ppt salinity |
| Seawater resistivity (1 ppt) | psw | 5.076 | ohm.m | Equiv. 1 ppt salinity — worst case |
| Protection potential | Ec | -0.80 | V (Ag/AgCl) | Per ABS CP guidance note |
| Closed circuit anode potential | E°a | -1.09 | V (Ag/AgCl) | Alloy A3, per ABS CP guidance note |
| Anode capacity | epsilon | 2000 | A·h/kg | Aluminium alloy A3 |
| Anode density | rho_a | 2750 | kg/m³ | Aluminium alloy |
| Coating type | — | Glass Flake Reinforced Epoxy | — | High integrity marine coating |
| Steel coated area | — | 100 | % | All wetted surface coated |
| Hull surface area (15 m draft) | Ac | 13 446.04 | m² | Maximum submerged surface |
| Design initial current density | ici | 200 | mA/m² | Per ABS CP guidance note |
| Design mean current density | icm | 200 | mA/m² | Per ABS CP guidance note |
| Design final current density | icf | 200 | mA/m² | Per ABS CP guidance note |
| Initial coating breakdown factor | fci | 0.02 | — | 2%; high integrity coating per ABS |
| Yearly coating breakdown factor | — | 0.005 | /yr | 0.5%/yr |
| Anode utilisation factor | u | 0.85 | — | Long flush-mounted anode |
| Anode mean length | — | 1.400 | m | Long flush-mounted type |
| Anode width | — | 0.150 | m | — |
| Anode height | — | 0.060 | m | — |
| Anode net weight | wN | 27.5 | kg | — |
| Anode gross weight | wG | 30.25 | kg | ~10% insert/core allowance |

## Hull Surface Area vs Draft
Table: FST Vessel Submerged Wetted Surface Area (Cws coefficients, baseline draft = 0 m)

| Draft (m) | Volume (m³) | Cp | Cb | WS Area (m²) |
|-----------|-------------|----|----|---------------|
| 1.0 | 6 530 | 0.767 | 0.746 | 7 105 |
| 5.0 | 37 364 | 0.845 | 0.835 | 9 213 |
| 10.0 | 78 922 | 0.887 | 0.882 | 11 361 |
| 13.0 | 104 628 | 0.903 | 0.899 | 12 616 |
| 14.0 | 113 282 | 0.908 | 0.904 | 13 032 |
| 15.0 | 121 969 | 0.912 | 0.908 | 13 446 |

Reference draft for preliminary CP calculation: 15 m (maximum surface, conservative)

## Seawater Environment
Salinity at site varies 1 to 32 ppt depending on depth and season.
Lowest salinity period occurs June–July.
FST vessel summer draft approximately 11.5 m.

| Salinity (ppt) | Conductivity (mS/cm) | Resistivity (ohm.cm) |
|----------------|---------------------|----------------------|
| 0.1 | 0.21 | 4 762 |
| 1 | 1.97 | 507.6 |
| 10 | 17.02 | 58.75 |
| 25 | 39.26 | 25.47 |
| 31 | 47.62 | 21.0 |

Note: Low salinity (near-freshwater) periods render SACP impractical due to excessive anode
count and weight requirements. This is a primary driver for ICCP system selection.

## Coating Breakdown Factors (Derived)
Initial cbf: fci = 0.02
Yearly increment: 0.005/yr
Mean cbf (40-yr): fcm = fci + 0.5 * (tf - t_initial_period) * yearly_rate
                      = 0.02 + 0.5 * 40 * 0.005 = 0.12 (i.e. 12%)
Final cbf (40-yr): fcf = fci + tf * yearly_rate
                       = 0.02 + 40 * 0.005 = 0.22 (i.e. 22%)

| Stage | CBF value | Effective current density (mA/m²) |
|-------|-----------|-----------------------------------|
| Initial | 0.02 | 4 |
| Mean | 0.12 | 24 |
| Final | 0.22 | 44 |

## Current Demand (Example Calculation, 40-yr, 10 ppt salinity)
| Stage | Current Density (mA/m²) | Total Current (A) |
|-------|------------------------|-------------------|
| Initial | 4 | 53.784 |
| Mean | 24 | 322.705 |
| Final | 44 | 591.626 |

Mean current demand governs anode mass calculation per DNV-RP-B401 §7.8.3.

## Anode Mass Calculation (Based on Mean Current Demand)
| Parameter | Value | Unit | Reference |
|-----------|-------|------|-----------|
| Total required anode mass | 66 515 | kg | — |
| Number of anodes (mass basis) | 2 419 | — | — |
| Number of anodes (initial current check) | 70 | — | — |
| Number of anodes (final current check) | 911 | — | — |
| Governing anode count | 2 419 | — | Mass governs |
| Anode individual current capacity (Ca) | 46 750 | A·h | — |
| Total anode current capacity (Ca × N) | 113 088 250 | A·h | — |
| Required current capacity | 113 075 818 | A·h | — |
| Anode life check | 40 | yr | Verified |

## Anode Resistance (Lloyd's Formula — Long Flush Mount)
| Parameter | Value | Unit |
|-----------|-------|------|
| Anode radius (equivalent) | 0.067 | m |
| Anode resistance Ra (initial) | 0.379 | ohm |
| Individual anode current output (initial) | 0.77 | A |
| Total initial current output (2419 anodes) | 1 850.6 | A |
| Initial current output check vs demand | Pass | — |

## Depleted Anode Geometry (per DNV-RP-B401)
| Parameter | Symbol | Value | Unit | Section |
|-----------|--------|-------|------|---------|
| Depleted anode mass | Wf | 4.13 | kg | §7.9.3 |
| Depleted anode volume | Vf | 0.00150 | m³ | §7.9.5 |
| Depleted anode length | Lf | 1.260 | m | §7.9.4 |

## SACP Sensitivity Results
Table: Total gross anode weight (kg) and count by design life and seawater resistivity

| Design Life | Resistivity (ohm.m) | Salinity equiv. | Gross Mass (kg) | No. Anodes |
|-------------|---------------------|-----------------|-----------------|------------|
| 25 yr | 5.076 | 1 ppt | 157 027 | 5 191 |
| 25 yr | 0.5875 | 10 ppt | 31 460 | 1 040 |
| 25 yr | 0.21 | 31 ppt | 31 460 | 1 040 |
| 40 yr | 5.076 | 1 ppt | 238 219 | 7 875 |
| 40 yr | 0.5875 | 10 ppt | 73 175 | 2 419 |
| 40 yr | 0.21 | 31 ppt | 73 175 | 2 419 |

Key observation: At 1 ppt salinity and 40 yr, SACP is impractical (7 875 anodes on hull surface).
At 10 ppt and above, mass and count stabilise — resistivity is no longer the limiting factor.

## ICCP System Parameters (from Electrical Design Philosophy)
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
        "calculation_type": "DNV_RP_B401_2021_SACP",
        "design_data": {
            "design_life": 40,                   # years (primary sensitivity case)
            "design_life_secondary": 25,          # years (secondary sensitivity)
        },
        "structure": {
            "structure_type": "floating_storage_terminal",
            "surface_area": 13446.04,            # m², 15 m draft (maximum)
            "surface_area_avg_draft": 10778.0,   # m², average draft (approx.)
            "coating_type": "glass_flake_reinforced_epoxy",
            "coating_coverage": 1.0,             # 100% coated
        },
        "environment": {
            "temperature": 10,                   # deg C
            "salinity_design": 10,               # ppt (nominal design case)
            "salinity_worst": 1,                 # ppt (worst case — near freshwater)
            "seawater_resistivity": 0.5875,      # ohm.m (10 ppt case)
            "seawater_resistivity_worst": 5.076, # ohm.m (1 ppt case)
            "seawater_resistivity_normal": 0.21, # ohm.m (31 ppt case)
        },
        "protection": {
            "protection_potential": -0.80,       # V vs Ag/AgCl
            "closed_circuit_anode_potential": -1.09,  # V vs Ag/AgCl (Al alloy A3)
        },
        "coating_breakdown": {
            "fc_initial": 0.02,                  # 2% initial
            "fc_yearly_rate": 0.005,             # 0.5%/yr
            # Derived: fcm(40yr) = 0.12, fcf(40yr) = 0.22
        },
        "current_density": {
            "design_initial": 0.200,             # A/m² (bare steel basis per ABS)
            "design_mean": 0.200,                # A/m²
            "design_final": 0.200,               # A/m²
            # Effective (with coating): initial=4 mA/m², mean=24 mA/m², final=44 mA/m²
        },
        "anode": {
            "material": "aluminium_alloy_A3",
            "type": "long_flush_mount",
            "capacity": 2000,                    # A·h/kg
            "density": 2750,                     # kg/m³
            "utilisation_factor": 0.85,
            "closed_circuit_potential": -1.09,   # V vs Ag/AgCl
            "length": 1.400,                     # m
            "width": 0.150,                      # m
            "height": 0.060,                     # m
            "net_weight": 27.5,                  # kg
            "gross_weight": 30.25,               # kg
        },
    }
}
# Run: CathodicProtection().router(cfg)
#
# Expected results (40-yr, 10 ppt / 0.5875 ohm.m):
#   Initial current demand:       53.784 A
#   Mean current demand:         322.705 A
#   Final current demand:        591.626 A
#   Required anode mass:          66 515 kg (based on mean current)
#   Number of anodes (mass):       2 419
#   Individual anode Ra:           0.379 ohm
#   Individual anode output:       0.77 A (initial)
#   Total initial current output:  1 850.6 A (>demand: pass)
#   Anode life check:              40 yr (verified)
#   Depleted anode mass:           4.13 kg
#   Depleted anode length:         1.260 m
#
# Sensitivity result (40-yr, 1 ppt / 5.076 ohm.m):
#   Gross anode mass: 238 219 kg, 7 875 anodes — impractical, ICCP preferred
```

## Gaps Found
- This document is a design philosophy note, not a final certified CP design. The example
  calculation in Appendix 1 uses DNV-RP-B401 §7.8–7.9 methodology but applies ABS current
  density tables (200 mA/m² bare steel) rather than DNV-RP-B401 Table 10-1 values. The
  cathodic_protection.py router would need to resolve which standard's current density table
  to apply when both are referenced.
- The coating breakdown formula used is linear-annual (DNV-style), not the ABS multiplicative
  model. cathodic_protection.py must confirm which formula is active.
- ICCP sizing (TRU rating, cable sizing, reference electrode spacing) is not calculated in
  this document — only described qualitatively. ICCP sizing would require a separate
  calculation module.
- Hybrid SACP+ICCP sizing (Mg anode initial polarisation quantity) is not computed here.
- Water resistivity input is based on site salinity surveys. The sensitivity at 1 ppt
  salinity is an environmental envelope case, not the adopted design condition.
- Page 21 of source document was not accessible; however all key numerical data was captured
  from Appendix 1 (pages 19–20) which contain the full example calculation table.
