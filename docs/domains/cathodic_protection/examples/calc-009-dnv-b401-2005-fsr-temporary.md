---
standard: DNV-RP-B401:2005
edition: "2005"
structure_type: free_standing_riser_temporary
source_type: abstracted_client_calculation
discipline: cathodic_protection
---

# Free Standing Riser (FSR) — Temporary Service — DNV-RP-B401:2005 / NACE SP0176-2007

## Source
Standards: DNV RP-B401 (2005), NACE SP0176-2007, DNV RP-F103 (2003), ISO-15589-2
Structure type: Free-standing riser (FSR) — temporary installation: suction pile, LRA, rigid riser,
URA, buoyancy can, DQ air can
Design life: 6 months (3-month service + 6-month installed life)

## Design Philosophy
- Intent is to limit corrosion to acceptable levels, not prevent all corrosion, given short service life
- Riser joints and buoyancy cans: TSA (thermal sprayed aluminium) coating; no supplemental anodes
  on riser string (TSA provides corrosion protection over 6-month life)
- URA and LRA: no dedicated CP system (decision by client engineering — duty of service and
  equipment life does not require it for such short duration)
- Suction pile, buoyancy can, DQ air can: flush-mount hull anodes

## Input Parameters
| Parameter | Value | Unit | Reference |
|-----------|-------|------|-----------|
| Design life | 6 | months | Project design basis |
| Design temperature (fluid / outer pipe) | 150 | °F | Project design basis |
| Anode type | Flush mount hull | — | Off-the-shelf standard |
| Anode alloy | Al/Zn/In (Galvotec CW-3 or equal) | — | DNV-RP-B401 Table 10-5 |
| Anode CCP | −1050 | mV vs Ag/AgCl | DNV-RP-B401 Table 10-6 |
| Anode capacity | 909 | A·h/lb | ~2000 A·h/kg |
| Protection potential | −800 | mV vs Ag/AgCl | DNV-RP-B401 §5.4.1 |
| Utilisation factor (flush mount) | 0.85 | — | DNV-RP-B401 Table 10-8 |

## Environment
| Parameter | Value | Unit | Notes |
|-----------|-------|------|-------|
| Seawater resistivity (buoyancy cans) | 24 | ohm-cm | = 0.24 Ω·m |
| Seawater resistivity (suction pile) | 31 | ohm-cm | = 0.31 Ω·m |

## Anode Specification
| Parameter | Value | Unit |
|-----------|-------|------|
| Type | Flush mount hull | — |
| Net alloy weight | 29 | lb (= 13.15 kg) |
| Gross weight | 34 | lb (= 15.42 kg) |
| Length | 24 | in (= 610 mm) |
| Width | 5 | in (= 127 mm) |
| Height | 2.5 | in (= 63.5 mm) |
| Insert (flat bar) | 1.5 × 0.25 × 29 | in |

## Coating Systems
| Component | Coating System | DNV Category | Notes |
|-----------|---------------|-------------|-------|
| Suction pile (all external) | 2–3 coat epoxy, >350 µm DFT | Cat 3 | Plus sacrificial anodes |
| Buoyancy can | 2-coat epoxy + acrylic topcoat | Cat 1 | Original DFT 4.5 mils |
| DQ Air can barrel (TSA) | Thermally sprayed aluminium | — | |
| DQ Air can ends (external) | 2-coat epoxy, ≥8 mils DFT | — | Per DQ supplier |
| Riser joints | Thermally sprayed aluminium (TSA) | — | No supplemental anodes |
| LRA/URA | Various (marine epoxy, zinc primer, TSA) | — | No anodes installed |

## Surface Areas (imperial units as per source)
| Structure | 2/3 Coat Epoxy Cat 3 (ft²) | TSA Coated (ft²) | Uncoated (ft²) | 2 Coat Epoxy Cat 1 (ft²) |
|-----------|--------------------------|-----------------|---------------|--------------------------|
| Suction Pile — buried | 3950 | 0 | 0 | 0 |
| Suction Pile — seawater | 300 | 0 | 350 (chain etc.) | 0 |
| Buoyancy Can | 0 | 0 | 400 (chain etc.) | 4859 |
| DQ Air Can | 0 | 1600 | 60 (chain) + 350 | 0 |

Note: 10% safety factor applied to surface areas to account for minor appurtenances.

## Coating Breakdown Factors (DNV-RP-B401 Table 10-4, 0.5 yr design life)
| Coating Type | DNV Category | Initial | Mean | Final |
|-------------|-------------|---------|------|-------|
| 2-coat epoxy | Cat 2 | 0.050 | 0.052 | 0.054 |
| 2-coat epoxy | Cat 1 | 0.100 | 0.113 | 0.125 |

## Current Densities (DNV-RP-B401 Tables 10-1, 10-2)
| Location | Depth | Initial (mA/ft²) | Mean (mA/ft²) | Final (mA/ft²) |
|----------|-------|-----------------|--------------|---------------|
| Buoyancy Can / DQ Air Can | 100–300 m, Tropical | 13.0 | 6.5 | 8.4 |
| TSA surface | — | — | — | 0.9 |
| Suction Pile | >300 m, Tropical | 16.7 | 8.4 | 12.1 |
| Seabed (buried) | — | 1.9 | 1.9 | 1.9 |

## Calculation Results
| Component | Governing Stage | Anode Count | Notes |
|-----------|----------------|-------------|-------|
| Suction Pile | Mass / initial current | 12 | Located on unburied surfaces |
| Buoyancy Can | Initial (polarisation) | 17 | Governs at initial CBF 0.1 (Cat 1) |
| DQ Air Can | Mass / initial | 4 | Primary: minimise TSA current drain |
| LRA | N/A | 0 | No anodes — client decision |
| URA | N/A | 0 | No anodes — client decision |
| Riser joints | N/A | 0 | TSA sufficient over 6-month life |

Total anodes: 33 (12 suction pile + 17 buoyancy can + 4 DQ air can)
All anodes: 29 lb net flush-mount, Galvotec CW-3 or equal

**Reproduction note (this branch, `DNV_RP_B401_offshore` route, edition "2005", cfg below):**
the route represents five seawater-exposed zones (645.9 m² after the 10% area factor: suction
pile at >300 m, cans at 100–300 m) plus the buried suction-pile steel (403.7 m²) as a `buried`
zone; the TSA-coated air-can barrel is omitted, one resistivity (0.31 Ω·m) is used for all
zones and the seawater temperature is taken as 25 °C (tropical region per the source; the
value itself is not stated). The values come from DNV-RP-B401 Tables 10-1 / 10-2 / 10-4 and
Sec. 6.3 as of #2207: 0.180 / 0.090 / 0.130 A/m² for the suction pile and 0.140 / 0.070 /
0.090 A/m² for the cans — identical to the source's 16.7 / 8.4 / 12.1 and 13.0 / 6.5 / 8.4
mA/ft² — buried 0.020 A/m², Cat I f_ci / f_cm / f_cf = 0.100 / 0.1125 / 0.125 (source Cat 1
0.100 / 0.113 / 0.125) and Cat III 0.020 / 0.022 / 0.024 at 0.5 yr. The code returns total
initial / mean / final currents of 33.173 / 21.064 / 25.863 A, a net anode mass of 54.27 kg,
5 anodes on the mass basis, `governing_case` "initial" and `recommended_anode_count` 29
(count by final current 23), against the source's 33 anodes (12 + 17 + 4), which the source
also reports as governed by the initial polarisation current. The source gives no current
values to compare (Appendix 2 was not recoverable). Provenance flag for the 2005 edition:
`verified-2011-tables`. The tabulated source values are left as extracted.

## Python cfg dict

```python
from digitalmodel.infrastructure.base_solvers.hydrodynamics.cathodic_protection import CathodicProtection

# Router key mapping: DNV-RP-B401:2005 -> "DNV_RP_B401_offshore" with design_data.edition
# = "2005". Requires #2207 or later.
cfg = {
    "inputs": {
        "calculation_type": "DNV_RP_B401_offshore",
        "standard": "DNV-RP-B401:2005",
        "co_standards": ["NACE-SP0176-2007", "DNV-RP-F103:2003"],
        "design_data": {
            "design_life": 0.5,        # years, router design life
            "edition": "2005",
            "design_life_months": 6,   # 0.5 years
            "design_life_years": 0.5,
            "structure_type": "free_standing_riser_temporary",
            "design_temperature_F": 150,
        },
        "environment": {
            "resistivity_buoyancy_ohm_cm": 24,     # = 0.24 Ω·m
            "resistivity_suction_pile_ohm_cm": 31, # = 0.31 Ω·m
            "resistivity_buoyancy_ohm_m": 0.24,
            "resistivity_suction_pile_ohm_m": 0.31,
            "seawater_resistivity_ohm_m": 0.31,    # router key; single value, suction-pile (higher) case
            "seawater_temperature_C": 25.0,        # router key; not stated in source (tropical region per source, >20 degC)
        },
        # Router zones, converted from the source ft2 areas (x 0.0929) with the 10% area
        # safety factor applied. Cat 3 -> "III", Cat 1 -> "I", uncoated -> "bare"; depth_m
        # places the suction pile in the >300 m band and the cans in the 100-300 m band as
        # in the source. The TSA-coated DQ air can barrel (1600 ft2) has no equivalent in the
        # route and is omitted (see Reproduction note).
        "structure": {
            "zones": [
                {"zone": "suction_pile_seawater_coated", "base_zone": "submerged", "depth_m": 400,
                 "area_m2": 30.7, "coating_category": "III"},
                {"zone": "suction_pile_uncoated", "base_zone": "submerged", "depth_m": 400,
                 "area_m2": 35.8, "coating_category": "bare"},
                {"zone": "suction_pile_buried", "base_zone": "buried", "depth_m": 400,
                 "area_m2": 403.7, "coating_category": "bare"},
                {"zone": "buoyancy_can_coated", "base_zone": "submerged", "depth_m": 200,
                 "area_m2": 496.6, "coating_category": "I"},
                {"zone": "buoyancy_can_uncoated", "base_zone": "submerged", "depth_m": 200,
                 "area_m2": 40.9, "coating_category": "bare"},
                {"zone": "dq_aircan_uncoated", "base_zone": "submerged", "depth_m": 200,
                 "area_m2": 41.9, "coating_category": "bare"},
            ],
        },
        "coating_breakdown": {
            # 0.5 yr design life
            "epoxy_cat2_0pt5yr": {
                "initial": 0.050, "mean": 0.052, "final": 0.054
            },
            "epoxy_cat1_0pt5yr": {
                "initial": 0.100, "mean": 0.113, "final": 0.125
            },
        },
        "current_density_mA_per_ft2": {
            # Tropical; buoyancy/DQ air can (100–300 m)
            "buoyancy_dq": {
                "initial": 13.0, "mean": 6.5, "final": 8.4
            },
            "tsa": 0.9,
            # Suction pile (>300 m)
            "suction_pile": {
                "initial": 16.7, "mean": 8.4, "final": 12.1
            },
            "buried": 1.9,
        },
        "surface_area_ft2": {
            "suction_pile_buried": 3950,
            "suction_pile_seawater_coated": 300,
            "suction_pile_uncoated": 350,   # chain, other
            "buoyancy_can_coated_cat1": 4859,
            "buoyancy_can_uncoated": 400,   # chain, other
            "dq_aircan_tsa": 1600,
            "dq_aircan_uncoated": 60 + 350, # chain + ends
        },
        "anode": {
            "type": "flush_mounted",   # router token for the flush-mount hull anode
            "alloy": "Al_Zn_In",
            "material": "aluminium",   # router material token
            "net_weight_lb": 29,
            "gross_weight_lb": 34,
            "net_weight_kg": 13.15,
            "individual_anode_mass_kg": 13.15,   # router key
            "gross_weight_kg": 15.42,
            "length_in": 24,
            "width_in": 5,
            "height_in": 2.5,
            "length_m": 0.610,         # router key
            "radius_m": 0.0606,        # router key; r = cross-section perimeter / 2 pi (127 x 63.5 mm)
            "current_capacity_Ah_per_lb": 909,
            "current_capacity_Ah_per_kg": 2000,
            "closed_circuit_potential_mV": -1050,
            "utilisation_factor": 0.85,
            "utilization_factor": 0.85,          # router key
        },
        "protection": {
            "min_potential_mV": -800,     # vs Ag/AgCl
        },
        # Expected design results
        "expected_results": {
            "suction_pile_anodes": 12,
            "buoyancy_can_anodes": 17,   # governs at initial polarisation
            "dq_aircan_anodes": 4,
            "ura_anodes": 0,             # no CP by client decision
            "lra_anodes": 0,             # no CP by client decision
            "riser_joints_anodes": 0,    # TSA self-sufficient
            "total_anodes": 33,
        },
    }
}

result = CathodicProtection().router(cfg)["results"]
print("Total mean / final current (A): {:.3f} / {:.3f}".format(
    result["current_demand_A"]["total_mean_A"], result["current_demand_A"]["total_final_A"]))
print("Net anode mass (kg):", result["anode_requirements"]["total_mass_kg"])
print("Anode count, all zones (mass basis):", result["anode_requirements"]["anode_count"])
print("Anode count, current-output basis:",
      result["current_output_verification"]["recommended_anode_count"])
# Note: the source current-density inputs are in mA/ft2 (1 mA/ft2 = 10.764 mA/m2); the
# router uses its own SI tables, not the source values.
```

## Gaps Found
- Source uses imperial units throughout (mA/ft², lb, in, ohm-cm); implementation must convert to SI.
  Conversion: 1 mA/ft² = 10.764 mA/m²; 1 lb = 0.4536 kg; 1 in = 25.4 mm; 1 ohm-cm = 0.01 Ω·m.
- Appendix 2 (calculation sheets) contains scanned handwritten/tabular calculation pages with no
  extractable text; individual current demand values for each component were not recoverable from
  the PDF.  Only anode count results (12 / 17 / 4) are stated in the narrative sections.
- The buoyancy can is governed by initial polarisation (Cat 1 CBF 0.10) rather than mean or final
  current demand, because this is a temporary structure with no depolarisation from wave turbulence.
- LRA and URA were originally designed for CP but anodes were removed by client decision (e-mail
  correspondence included in source Appendix 3); the corrosion impact was judged acceptable
  over the short service life given coating and TSA protection.
- The DQ Air Can anodes serve primarily to protect the TSA coating from excessive galvanic drain
  by uncoated chain hardware — they do not protect bare steel per standard methodology.
