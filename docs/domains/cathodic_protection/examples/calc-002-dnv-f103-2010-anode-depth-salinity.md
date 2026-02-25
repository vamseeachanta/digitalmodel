---
standard: DNV-RP-F103:2010
edition: "2010"
structure_type: floating_storage_tanker_hull_depth_salinity_correction
source_type: abstracted_client_calculation
discipline: cathodic_protection
---

# Floating Storage Tanker — DNV-RP-F103:2010 Anode Depth and Salinity Correction

## Source
Standard: DNV-RP-F103:2010 (supporting calculation to main CP design)
Structure type: Floating Storage Tanker (FST) hull — bottom shell and side shell anodes
Scope: Determination of seawater resistivity at anode locations as a function of water depth
       and salinity profile. Used to select the governing resistivity for CP design.
Reference: Wood (2001) salinity-resistivity relationship;
           course_notes_Appendix_A_Cathodic_Protection_Design.pdf (conductance table)

## Purpose

The CP design current demand is proportional to seawater resistivity at the anode location.
For a floating structure the anode water depth varies between drafts and tidal range.
This spreadsheet determines:
1. The depth range for each anode group (bottom shell, side shell)
2. The salinity at each depth from a site-specific salinity profile
3. The resistivity at each depth using the Wood salinity-resistivity formula
4. The governing resistivity to use in the main CP calculation

## FST Draft Range

| Condition | Draft (m) |
|-----------|-----------|
| Minimum loading draft | 3.48 |
| Maximum loading draft | 11.00 |

## Anode Location Water Depths

### Bottom Shell Anodes

Mounted on the bottom shell of the hull — water depth = vessel draft.

| Condition | Water depth (m) |
|-----------|-----------------|
| Minimum draft (lightest) | 3.48 |
| Maximum draft (deepest) | 11.00 |

### Side Shell Anodes

Mounted on the side of the hull — water depth ranges from above waterline to mid-draught.

| Condition | Water depth (m) | Note |
|-----------|-----------------|------|
| Minimum depth | −2.0 | Above waterline (occasionally exposed) |
| Maximum depth | 5.52 | Below waterline |

## Salinity Profile (Site-Specific)

| Depth (m) | Salinity (ppt) | Resistivity (Ω·m) |
|-----------|----------------|-------------------|
| 0 | 2 | 4.577 |
| 1 | 3 | 4.079 |
| 2 | 6 | 2.582 |
| 3 | 7 | 2.084 |
| 4 | 9 | 1.086 |
| 5 | 10 | 0.5875 |
| 10 | 25 | 0.2547 |
| 15 | 28 | 0.2324 |
| 25 | 28 | 0.2324 |

Note: Salinity increases rapidly from surface to 10 m depth, reflecting a freshwater layer near
the surface at this location (likely nearshore / estuarine environment).

## Resistivity Formula — Wood (2001)

Tabulated data from Wood (2001) relating salinity to resistivity:

| Salinity (ppt) | Resistivity (Ω·cm) | Resistivity (Ω·m) |
|----------------|-------------------|-------------------|
| 0.1 | 4762 | 47.62 |
| 1 | 507.6 | 5.076 |
| 10 | 58.75 | 0.5875 |
| 25 | 25.47 | 0.2547 |
| 31 | 21.0 | 0.210 |

The site salinity profile uses this relationship to convert measured salinity to resistivity.

## Conductance Reference Table (Sheet3)

Full conductance (1/Ω·cm) by chlorinity (ppt) and temperature (°C), from reference course notes.
Used for cross-checking the resistivity values at depth.

Governing computed seawater resistance:
- **20.856 Ω·cm** = **0.209 Ω·m** (from conductance table at site conditions)

### Selected Values from Conductance Table

| Chlorinity (ppt) | 0°C | 5°C | 10°C | 15°C | 20°C | 30°C |
|------------------|-----|-----|------|------|------|------|
| 1 | 0.001839 | 0.002134 | 0.002439 | 0.002763 | 0.003091 | 0.003431 |
| 5 | 0.008327 | 0.009653 | 0.011019 | 0.012459 | 0.013939 | 0.015471 |
| 10 | 0.015852 | 0.018329 | 0.020906 | 0.023584 | 0.026367 | 0.029242 |
| 15 | 0.022993 | 0.026548 | 0.030231 | 0.034090 | 0.038065 | 0.042180 |
| 20 | 0.029885 | 0.034454 | 0.039167 | 0.044114 | 0.049248 | 0.054551 |
| 22 | 0.032556 | 0.037508 | 0.042614 | 0.047948 | 0.053556 | 0.059321 |

Seawater resistance (computed): 20.856 Ω·cm = 0.209 Ω·m

## Governing Resistivity for CP Design

### Bottom Shell Anodes

Depth range: 3.48 m (min draft) to 11.0 m (max draft)

At 3.48 m depth, salinity ≈ 7–9 ppt → resistivity ≈ 1.1–2.1 Ω·m (high — shallow, low salinity zone)
At 11.0 m depth, salinity ≈ 25+ ppt → resistivity ≈ 0.25 Ω·m (low — deeper, normal seawater)

Governing (worst case = highest resistivity): at minimum draft (~3.5 m), ρ ≈ 2.08 Ω·m

### Side Shell Anodes

Depth range: −2.0 m (above water) to 5.52 m below waterline

At 5.52 m depth, salinity ≈ 9–10 ppt → resistivity ≈ 0.59–1.09 Ω·m
Governing: at minimum immersion (~0 m, surface), ρ ≈ 4.58 Ω·m

### Design Resistivity

The main CP calculation (calc-001) uses the resistivity that governs anode resistance.
This supplementary calculation establishes the depth-dependent resistivity profile;
the main calculation selects the governing resistivity from this data.

For the deepest, fully immersed condition (design governing for anode resistance):
- At 11 m depth (max draft): ρ ≈ 0.25 Ω·m (close to open ocean seawater)
- Confirmation from conductance table: 0.209 Ω·m at full salinity / site conditions

## Relationship to Main CP Calculation

This spreadsheet is a supplementary input to the main FST hull CP calculation (calc-001).
Key outputs used in calc-001:
- Salinity profile at anode locations confirms which depth zone governs anode resistance
- Resistivity values feed into anode resistance formulas (Ra = ρ/(2πL) × [ln(4L/r) − 1] etc.)
- The surface zone (low salinity, high resistivity) increases anode resistance for side shell
  anodes when the vessel is in light-load condition — this must be captured in anode sizing

## Gaps Found

- The chart data in Sheet2 ("Side Shell Anodes - MinDepth", "Bottom Shell Anodes - Min Depth",
  "Side Shell Anodes - Max Depth", "Bottom Shell Anodes - Max Depth") references a depth vs
  resistivity plot; the chart itself is not reproduced here as it is a visual of the tabular
  data already captured.
- The Wood (2001) formula is cited by reference only; exact polynomial coefficients are not
  given in the spreadsheet (only the tabulated lookup values are provided).
- The conductance table reference (course_notes_Appendix_A) is a non-public training document;
  the table itself is reproduced as tabular data above.
- This calculation does not specify which resistivity was ultimately selected for use in calc-001.
  Cross-referencing calc-001 reveals ρ = 0.22 Ω·m was used as design seawater resistivity.

## Python cfg dict

```python
cfg = {
    "inputs": {
        "calculation_type": "depth_salinity_resistivity",
        "standard": "DNV-RP-F103:2010",
        "design_data": {
            "structure_type": "floating_storage_tanker_hull",
            "fst_draft_min_m": 3.48,
            "fst_draft_max_m": 11.00,
        },
        "anode_depth_ranges": {
            "bottom_shell": {
                "min_depth_m": 3.48,    # = minimum draft
                "max_depth_m": 11.00,   # = maximum draft
            },
            "side_shell": {
                "min_depth_m": -2.0,    # above waterline (splash zone)
                "max_depth_m": 5.52,    # below waterline
            },
        },
        "salinity_depth_profile": [
            # (depth_m, salinity_ppt, resistivity_ohm_m)
            (0,  2,  4.577),
            (1,  3,  4.079),
            (2,  6,  2.582),
            (3,  7,  2.084),
            (4,  9,  1.086),
            (5,  10, 0.5875),
            (10, 25, 0.2547),
            (15, 28, 0.2324),
            (25, 28, 0.2324),
        ],
        "wood_2001_salinity_resistivity": [
            # (salinity_ppt, resistivity_ohm_cm, resistivity_ohm_m)
            (0.1,  4762, 47.62),
            (1,    507.6, 5.076),
            (10,   58.75, 0.5875),
            (25,   25.47, 0.2547),
            (31,   21.0,  0.210),
        ],
        "conductance_table_ref": "course_notes_Appendix_A_Cathodic_Protection_Design",
        "computed_seawater_resistance": {
            "value_ohm_cm": 20.856,
            "value_ohm_m": 0.209,
        },
        "governing_resistivity_for_cp": {
            # Used in main CP calculation (calc-001)
            "design_resistivity_ohm_m": 0.22,
            "basis": "fully_immersed_normal_salinity",
        },
    }
}
# Run: CathodicProtection().router(cfg)
# This is a supplementary calculation; results feed into calc-001 (FST hull main CP calc).
```
