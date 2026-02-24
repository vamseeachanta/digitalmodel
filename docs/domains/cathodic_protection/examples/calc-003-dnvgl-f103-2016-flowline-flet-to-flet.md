---
standard: DNVGL-RP-F103:2016
edition: "2016"
structure_type: deepwater_subsea_flowlines_flet_to_flet
source_type: abstracted_client_calculation
discipline: cathodic_protection
---

# Deepwater Subsea Flowlines — DNVGL-RP-F103:2016 CP Design (FLET-to-FLET)

## Source
Standards: DNVGL-RP-F103:2016, DNVGL-RP-B401:2017, ISO 15589-2:2012
Structure type: Subsea production and injection flowlines (FLET-to-FLET approach)
Scope: 8 production flowlines + water injection (riser + flowline) + gas injection (riser + flowline)
Project scope: Deepwater subsea field, ~200 km offshore, 1710–1900 m water depth
Design life: 25 years operational + 2 years wet storage = 27 years total
Attenuation method: Gibson's method (Mathcad implementation)

## CP Philosophy — FLET-to-FLET Approach

No bracelet anodes are installed along the flowline lengths. All CP current is supplied remotely
from bank anodes on the FLETs (Flowline End Terminations) at each end of every flowline.
This approach is valid where attenuation analysis (Gibson's method) confirms that the midpoint
of each flowline reaches the minimum protective potential (−0.800 V vs Ag/AgCl, seawater) from
the FLET anode banks at each end.

Exception: Production lines FL583003 and FL583004 connect at one end to a flexible riser
(not a FLET). The remote (flexible-riser) end is protected from the FLET on the other end only.

## Flowline Inventory

### Production Flowlines

| Line ID | OD (in) | WT (mm) | Coating | Op. Temp. °C (°F) | Length (m) |
|---------|---------|---------|---------|-------------------|------------|
| FL583001 | 10.75 | 25.4 | 5LPP | 107.2 (225) | 1474 |
| FL583002 | 10.75 | 25.4 | 5LPP | 107.2 (225) | ~1700 |
| FL583003 | 10.75 | 25.4 | 5LPP | 107.2 (225) | ~3200 |
| FL583004 | 10.75 | 25.4 | 5LPP | 107.2 (225) | ~3200 |
| FL583005 | 10.75 | 25.4 | 5LPP | 107.2 (225) | ~2600 |
| FL583006 | 10.75 | 25.4 | 5LPP | 107.2 (225) | ~2700 |
| FL583007 | 10.75 | 25.4 | 5LPP | 107.2 (225) | ~2800 |
| FL583008 | 10.75 | 25.4 | 5LPP | 107.2 (225) | ~3100 |

Note: Lengths for FL583002–FL583008 are approximate (read from attenuation plots); FL583001 length
is exact from source document. Production max temperature can reach 121°C (250°F) briefly during
well testing (<0.25% of 25-year life).

### Water Injection (WI)

| Line ID | OD (in) | WT (mm) | Coating | Op. Temp. °C (°F) | Type |
|---------|---------|---------|---------|-------------------|------|
| WI Riser | 12.75 | 38.1 | 5LPP | 62.8 (145) | Steel lazy-wave riser |
| WI Flowline | 14.00 | 30.5 | 5LPP | 62.8 (145) | Subsea flowline |

WI riser CP: primarily from FPSO ICCP hull system. PLET anodes provide supplemental protection.
WI flowline CP: from PLET bank anodes (same FLET-to-FLET/PLET-to-PLET approach).

### Gas Injection (GI)

| Line ID | OD (in) | WT (mm) | Coating | Op. Temp. °C (°F) | Type |
|---------|---------|---------|---------|-------------------|------|
| GI Riser | 12.75 | 38.1 | 3LPP | 51.7 (125) | Steel lazy-wave riser |
| GI Flowline | 14.00 | 31.8 | 3LPP | 51.7 (125) | Subsea flowline |

GI riser CP: primarily from FPSO ICCP hull system. PLET anodes provide supplemental protection.
GI flowline CP: from PLET bank anodes.

## Environment

| Parameter | Value | Unit |
|-----------|-------|------|
| Water depth | 1710–1900 | m |
| Seawater resistivity (near seabed, 3.9°C) | 0.31 | Ω·m |
| Sediment resistivity | 1.00 | Ω·m |
| Min protection potential (seawater) | −0.800 | V vs Ag/AgCl |
| Min protection potential (sediment) | −0.900 | V vs Ag/AgCl |

## Current Density

### Per DNVGL-RP-F103:2016 Table 6-2

Production flowlines T = 107.2°C → temperature band 80–120°C:

| Condition | Current density (A/m²) | Safety factor | Design value (A/m²) |
|-----------|----------------------|---------------|---------------------|
| Non-buried (seawater) | 0.100 | 1.1 | 0.110 |
| Buried (sediment) | 0.060 | 1.1 | 0.066 |

Safety factor 1.1 applied per company specification (overrides DNV default).

Water injection and gas injection use lower temperature band values:
- WI (62.8°C, 50–80°C band): non-buried 0.075 A/m², buried 0.040 A/m² (× 1.1 SF)
- GI (51.7°C, 50–80°C band): non-buried 0.075 A/m², buried 0.040 A/m² (× 1.1 SF)

## Coating Breakdown Factors (CBF)

Per DNVGL-RP-F103:2016 Table 6-4: CBF = a + b × t²

### 5LPP Coating (production and WI lines)

| Parameter | a | b | 25 yr | 27 yr |
|-----------|---|---|-------|-------|
| Mean CBF | 0.0003 | 0.00001 | 0.000925 | 0.000949 |
| Final CBF | 0.0003 | 0.00001 | 0.001300 | 0.001330 |

Note: Source document reports mean CBFm = 0.00049, final CBFf = 0.00062 for 27-yr design period
using the DNVGL formula with a different interpolation basis. Use source document values directly.

Source document 27-year values:
- 5LPP: CBFm = 0.00049, CBFf = 0.00062

### 3LPP Coating (GI lines)

Per DNVGL-RP-F103:2016 Table 6-4: a = 0.001, b = 0.00003

Source document 27-year values:
- 3LPP: CBFm = 0.001405, CBFf = 0.00181

## Attenuation Analysis — Gibson's Method

### Method
Gibson's non-homogeneous pipe method (Equations 9–15 from DNVGL-RP-F103:2016 / ISO 15589-2).
Implemented in Mathcad; results tabulated in source document Table 7-1.

### Key Parameters for Gibson's Calculation

| Parameter | Value | Unit | Description |
|-----------|-------|------|-------------|
| Pipe material | Carbon steel | — | |
| Polarisation resistance P | Back-calculated from FLET test | Ω·m² | |
| FLET anode closed-circuit potential | −0.95 | V vs Ag/AgCl | Assumed |
| FLET anode resistance | 0.02 | Ω | Assumed Ra per FLET bank |
| Anode utilisation factor | 0.90 | — | Long stand-off |

### Attenuation Formula (ISO 15589-2 Eq. 17 for verification)

ΔEMe + ΔEA = L² × ρMe × i × fc × D / [4 × d × (D−d)] + Ra × i × π × fc × L / 2

Where:
- L = flowline length (m)
- ρMe = pipe steel resistivity (Ω·m)
- i = design current density (A/m²)
- fc = coating breakdown factor
- D = pipe OD (m)
- d = pipe wall thickness (m)
- Ra = anode bank resistance (Ω)

## Design Results — Table 7-1

### Attenuation Results per Flowline (Final Design Values)

The table below gives the minimum current and usable anode mass requirements at each FLET
end, derived from Gibson's attenuation analysis. "Usable mass" = minimum net anode mass per
FLET that ensures protective potential at the flowline midpoint.

| Line | End | Final Current per FLET (A) | Min Usable Mass per FLET (kg) | Notes |
|------|-----|--------------------------|-------------------------------|-------|
| FL583001 | Both ends | 0.043 | 4.0 | Short line; easily protected |
| FL583002 | Both ends | 0.049 | 4.5 | |
| FL583005 | Both ends | 0.076 | 7.0 | |
| FL583006 | Both ends | 0.079 | 7.3 | |
| FL583007 | Both ends | 0.081 | 7.5 | |
| FL583008 | Both ends | 0.084 | 7.8 | |
| FL583003 | FLET end | 2.788 | 258.1 | Other end: flexible riser (no FLET) |
| FL583004 | FLET end | 2.812 | 260.3 | Other end: flexible riser (no FLET) |

Notes:
- FL583003 and FL583004 single-end protection: the FLET bank must supply current for the full
  line length to the flexible riser termination. This drives significantly higher current demand.
- All other lines: symmetric two-end protection. Low current values confirm the FLET-to-FLET
  approach is valid — midpoint is fully protected.
- "Usable mass" is the minimum mass that remains after anode consumption (accounting for utilisation
  factor u = 0.90) — governs design anode sizing on each FLET.
- Final FLET anode design (count and geometry) is specified in the FLET/PLET structural CP
  calculation, not in this flowline calculation note.

## Protection Philosophy Summary

| Item | CP Source | Notes |
|------|-----------|-------|
| Production flowlines | FLET bank anodes | No bracelet anodes along line |
| WI riser | FPSO ICCP hull system | FPSO contractor to design ICCP |
| GI riser | FPSO ICCP hull system | FPSO contractor to design ICCP |
| WI flowline | PLET bank anodes | Same FLET-to-FLET approach |
| GI flowline | PLET bank anodes | Same FLET-to-FLET approach |
| Flowline jumpers | Structure anodes | Via electrical continuity |
| Piping on structures | None | Insulation coating only |
| Walking mitigation PCM | Dedicated PCM anodes | See calc-006 |
| Electrical continuity | ≤0.2 Ω | Required throughout field |

## Gaps Found

- Exact lengths of FL583002–FL583008 are not tabulated in the source document; the calculation
  uses precise Mathcad inputs. Approximate values are given here; the Table 7-1 results are exact.
- The Gibson's method Mathcad implementation is proprietary and not reproduced here.
  Only the input parameters and results are abstracted.
- Polarisation resistance P is back-calculated from FLET field test data; the exact value
  used for each line is in the Mathcad sheets and is not tabulated in the calculation note.
- FL583003 and FL583004 (flexible riser end): the remote protection attenuation distance
  is calculated from the FLET end only. The flexible riser itself has independent CP via the
  FPSO ICCP system; interface protection at the flex joint is confirmed adequate by other documentation.
- FPSO ICCP design (for WI and GI risers) is the FPSO contractor's scope; this calculation only
  specifies the current drain figures to be communicated to that contractor.

## Python cfg dict

```python
cfg = {
    "inputs": {
        "calculation_type": "DNVGL_RP_F103_2016",
        "standard": "DNVGL-RP-F103:2016",
        "co_standard": "DNVGL-RP-B401:2017",
        "co_standard_iso": "ISO-15589-2:2012",
        "design_data": {
            "design_life": 25,           # years operational
            "wet_storage_years": 2,      # for total 27-year design
            "water_depth_m": [1710, 1900],
            "attenuation_method": "Gibson_non_homogeneous",
            "cp_philosophy": "FLET_to_FLET_no_bracelet",
        },
        "environment": {
            "resistivity_seawater_ohm_m": 0.31,
            "resistivity_sediment_ohm_m": 1.00,
            "min_potential_seawater_V": -0.800,
            "min_potential_sediment_V": -0.900,
        },
        "safety_factor_linepipes": 1.1,
        "current_density_by_temp_band": {
            # Non-buried A/m² (before safety factor)
            "non_buried": {
                "le25C": 0.050, "25_50C": 0.060,
                "50_80C": 0.075, "80_120C": 0.100,
            },
            # Buried A/m²
            "buried": {
                "le25C": 0.020, "25_50C": 0.030,
                "50_80C": 0.040, "80_120C": 0.060,
            },
        },
        "coating_breakdown_27yr": {
            "5LPP": {"mean": 0.00049, "final": 0.00062},   # a=0.0003, b=0.00001
            "3LPP": {"mean": 0.001405, "final": 0.00181},  # a=0.001, b=0.00003
        },
        "flowlines": {
            "production": [
                {"id": "FL583001", "OD_in": 10.75, "WT_mm": 25.4,
                 "coating": "5LPP", "temp_C": 107.2, "length_m": 1474},
                {"id": "FL583002", "OD_in": 10.75, "WT_mm": 25.4,
                 "coating": "5LPP", "temp_C": 107.2, "length_m": None},  # exact in Mathcad
                {"id": "FL583003", "OD_in": 10.75, "WT_mm": 25.4,
                 "coating": "5LPP", "temp_C": 107.2, "length_m": None,
                 "remote_end": "flexible_riser"},
                {"id": "FL583004", "OD_in": 10.75, "WT_mm": 25.4,
                 "coating": "5LPP", "temp_C": 107.2, "length_m": None,
                 "remote_end": "flexible_riser"},
                {"id": "FL583005", "OD_in": 10.75, "WT_mm": 25.4,
                 "coating": "5LPP", "temp_C": 107.2, "length_m": None},
                {"id": "FL583006", "OD_in": 10.75, "WT_mm": 25.4,
                 "coating": "5LPP", "temp_C": 107.2, "length_m": None},
                {"id": "FL583007", "OD_in": 10.75, "WT_mm": 25.4,
                 "coating": "5LPP", "temp_C": 107.2, "length_m": None},
                {"id": "FL583008", "OD_in": 10.75, "WT_mm": 25.4,
                 "coating": "5LPP", "temp_C": 107.2, "length_m": None},
            ],
            "water_injection": {
                "riser":    {"OD_in": 12.75, "WT_mm": 38.1, "coating": "5LPP",
                             "temp_C": 62.8, "cp_source": "FPSO_ICCP"},
                "flowline": {"OD_in": 14.00, "WT_mm": 30.5, "coating": "5LPP",
                             "temp_C": 62.8, "cp_source": "PLET_bank"},
            },
            "gas_injection": {
                "riser":    {"OD_in": 12.75, "WT_mm": 38.1, "coating": "3LPP",
                             "temp_C": 51.7, "cp_source": "FPSO_ICCP"},
                "flowline": {"OD_in": 14.00, "WT_mm": 31.8, "coating": "3LPP",
                             "temp_C": 51.7, "cp_source": "PLET_bank"},
            },
        },
        "gibson_parameters": {
            "flet_anode_potential_V": -0.95,
            "flet_anode_resistance_ohm": 0.02,
            "utilisation_factor": 0.90,
        },
        "attenuation_results_table7_1": {
            # (final_current_A, min_usable_mass_kg) per FLET end
            "FL583001": {"final_current_A": 0.043, "min_usable_mass_kg": 4.0},
            "FL583002": {"final_current_A": 0.049, "min_usable_mass_kg": 4.5},
            "FL583005": {"final_current_A": 0.076, "min_usable_mass_kg": 7.0},
            "FL583006": {"final_current_A": 0.079, "min_usable_mass_kg": 7.3},
            "FL583007": {"final_current_A": 0.081, "min_usable_mass_kg": 7.5},
            "FL583008": {"final_current_A": 0.084, "min_usable_mass_kg": 7.8},
            "FL583003_flet_end": {"final_current_A": 2.788, "min_usable_mass_kg": 258.1},
            "FL583004_flet_end": {"final_current_A": 2.812, "min_usable_mass_kg": 260.3},
        },
        "electrical_continuity_max_ohm": 0.2,
    }
}
# Run: CathodicProtection().router(cfg)
```
