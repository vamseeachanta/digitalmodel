# Offshore Jacket Structure CP Design — DNV-RP-B401 (Placeholder)

**Standard:** DNV-RP-B401:2021
**Structure type:** Fixed offshore jacket — submerged zone, tropical seawater
**Status:** NOT YET IMPLEMENTED — see WRK-272

---

## Standard and Scope

DNV-RP-B401 (2021 Edition) — Cathodic Protection Design. This recommended practice
covers the design of cathodic protection systems for offshore structures including
fixed jackets, gravity-based structures (GBS), and associated subsea equipment.

The 2021 edition PDF is available at:
`acma-projects/B1522/ctr-2/cal/DNV-RP-B401-2021.pdf`

Key differences from DNV-RP-F103 (pipelines):
- B401 covers fixed structures; F103 covers submarine pipelines
- B401 uses higher current densities for submerged structural steel
- B401 coating breakdown model differs from F103 Annex 1
- B401 explicitly addresses impressed current CP (ICCP) as well as sacrificial anodes

---

## Design Inputs (Proposed)

| Parameter | Value | Unit |
|-----------|-------|------|
| Total submerged surface area | 20,000 | m² |
| Water depth (jacket leg base) | 80 | m |
| Climatic region | Tropical | — |
| Design life | 25 | years |
| Seawater temperature | 28 | °C |
| Seawater resistivity | 0.20 | Ω·m |
| Anode material | Al-Zn-In | — |
| Anode geometry | stand-off | — |
| Individual anode mass | 220 | kg |
| Utilisation factor | 0.80 | — |
| Coating type | epoxy | — |
| Protection potential criterion | −0.80 | V vs Ag/AgCl |

---

## Expected Calculation Steps (once WRK-272 is implemented)

### Step 1: Current Density Selection (B401 Table 10-1)

DNV-RP-B401:2021 Table 10-1 tabulates mean current densities (A/m²) for offshore
structural steel as a function of water depth and geographic region. For tropical
conditions at 80 m water depth, expected values are in the range 0.08–0.12 A/m².

### Step 2: Coating Breakdown Factors (B401 Section 10.3)

B401 provides coating breakdown factors as a function of coating type and water depth.
Unlike F103's linear Annex 1 formula, B401 specifies coating quality categories
(bare, light, medium, heavy) with distinct initial and final factors.

### Step 3: Current Demand

```
I_cm = A_c × f_cm × i_cm    (mean current demand)
I_cf = A_c × f_cf × i_cm    (final current demand)
```

Where i_cm is from B401 Table 10-1 for the applicable region/depth.

### Step 4: Anode Mass and Count

```
M_total = I_cm × t_f × 8760 / (acc × u)
N = ceil(M_total / m_a)
```

### Step 5: Anode Current Output Check

For stand-off anodes, DNV-RP-B401 specifies a resistance formula that accounts for
anode geometry and standoff distance from the structure.

---

## Anticipated Results Summary (approximate, pending implementation)

| Output | Estimate | Unit |
|--------|----------|------|
| Total submerged area | 20,000 | m² |
| Mean current density i_cm | ~0.09 | A/m² |
| Coating factor f_cm | ~0.05–0.15 | — |
| Mean current demand I_cm | ~90–270 | A |
| Anode mass required | ~20,000–60,000 | kg |
| Anode count (220 kg each) | ~90–275 | — |

These estimates are indicative only. Actual values depend on the exact B401:2021
table values to be implemented in WRK-272.

---

## Python Example (NOT YET IMPLEMENTED — see WRK-272)

```python
# NOT YET IMPLEMENTED — see WRK-272
# The DNV_RP_B401_2021 route is not available in this codebase yet.
# When implemented, the expected cfg structure will be:

from digitalmodel.infrastructure.common.cathodic_protection import CathodicProtection

cfg = {
    "inputs": {
        "calculation_type": "DNV_RP_B401_2021",   # NOT YET IMPLEMENTED — see WRK-272
        "design_data": {
            "design_life": 25.0,
        },
        "structure": {
            "surface_area_m2": 20000.0,
            "water_depth_m": 80.0,
            "geographic_region": "tropical",
            "coating_type": "epoxy",
            "coating_quality": "medium",
        },
        "environment": {
            "seawater_temperature_C": 28.0,
            "seawater_resistivity_ohm_m": 0.20,
        },
        "anode": {
            "material": "aluminium",
            "utilization_factor": 0.80,
            "individual_anode_mass_kg": 220.0,
            "geometry": "stand_off",
        },
    }
}

# cp = CathodicProtection()
# result = cp.router(cfg)   # Will raise NotImplementedError until WRK-272 is complete
```

---

## Gap Notes

1. **WRK-272 required:** The `DNV_RP_B401_2021` calculation type is not yet implemented
   in `cathodic_protection.py`. A separate WRK item (WRK-272) is required to add this
   route. Until then, calling `router()` with `calculation_type="DNV_RP_B401_2021"` will
   raise an `Exception`.

2. **B401 vs B401:2021:** The 2021 edition supersedes the 2017 and 2010 editions.
   The target implementation is B401:2021. Previous editions used different table values;
   the implementation must use the 2021 PDF tables.

3. **B401 source document:** `acma-projects/B1522/ctr-2/cal/DNV-RP-B401-2021.pdf` is
   the canonical reference for this implementation.

4. **Scope boundary:** This placeholder covers the jacket submerged zone. Splash zone
   and atmospheric zone protection (typically paint + impressed current) are out of scope
   for the galvanic anode route.

5. **Existing ABS offshore route:** For immediate use, the `ABS_gn_offshore_2018`
   route in this codebase provides similar fixed-structure CP calculations per the ABS
   standard. See `example-03-platform-abs-offshore-2018.md` for a worked example.
