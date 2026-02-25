# Submarine Pipeline CP Design — DNV-RP-F103:2010

**Standard:** DNV-RP-F103 (October 2010 Edition)
**Structure type:** Submarine pipeline — non-buried, 3LPE-coated
**Anode type:** Al-Zn-In bracelet anodes

---

## Standard and Scope

DNV-RP-F103 (October 2010) — Cathodic Protection of Submarine Pipelines by Galvanic Anodes.

This recommended practice covers the design of sacrificial anode systems for submarine
pipelines. It specifies:

- Current density requirements from Table 5-1 (function of burial condition and internal
  fluid temperature band)
- Coating breakdown factors from Annex 1 linear formula (Eq. 2 and Eq. 4)
- Anode mass and quantity determination (Eq. 5)
- Attenuation analysis (Eq. 11) for current distribution

---

## Design Inputs

| Parameter | Value | Unit |
|-----------|-------|------|
| Pipeline outer diameter (OD) | 323.9 | mm |
| Wall thickness (WT) | 12.7 | mm |
| Pipeline length | 10,000 | m |
| Burial condition | non_buried | — |
| Internal fluid temperature | 60 | °C |
| Coating type | 3LPE (3-layer FBE/PE) | — |
| Design life | 25 | years |
| Anode material | aluminium (Al-Zn-In) | — |
| Anode net mass | 300 | kg |
| Utilisation factor | 0.80 | — |
| Contingency factor | 1.10 | — |
| Pipe steel resistivity | 2.0 × 10⁻⁷ | Ω·m |
| Seawater resistivity | 0.25 | Ω·m |
| Min anode spacing | 5 | m |
| Max anode spacing | 500 | m |

---

## Calculation Steps

### Step 1: Pipeline Surface Area

The outer surface area is required for current demand calculation (Eq. 1):

```
A_c = π × D × L
A_c = π × 0.3239 × 10,000
A_c = 10,175.6 m²
```

Because the pipeline carries no additional weight-coating, the wetted area equals the
outer surface area.

### Step 2: Current Density from Table 5-1

DNV-RP-F103 (2010) Table 5-1 specifies the mean design current density as a function
of burial condition and internal fluid temperature band:

| Burial condition | ≤50 °C | >50–80 °C | >80–120 °C | >120 °C |
|-----------------|--------|-----------|------------|---------|
| non_buried      | 0.050  | **0.060** | 0.070      | 0.100   |
| buried          | 0.020  | 0.025     | 0.030      | 0.040   |

For T = 60 °C (band >50–80 °C) and non_buried:

```
i_cm = 0.060 A/m²
```

No Arrhenius temperature correction or coating-quality adjustment applies in
F103-2010; the table value is used directly.

### Step 3: Coating Breakdown Factors (Annex 1 Linear Formula)

For 3LPE coating, Annex 1 Table A.1 gives:

```
a = 0.001   (initial breakdown factor)
b = 0.00003 (degradation constant per year)
```

Applying linear formulae (Eq. 2 and Eq. 4) for t_f = 25 years:

```
f_ci = a = 0.001
f_cm = a + 0.5 × b × t_f = 0.001 + 0.5 × 0.00003 × 25 = 0.001 + 0.000375 = 0.001375
f_cf = a + b × t_f       = 0.001 + 0.00003 × 25       = 0.001 + 0.00075   = 0.001750
```

The mean factor f_cm = 0.001375 means that 0.14% of the pipe surface is effectively
bare steel at mid-life. The 3LPE is a high-performance coating system with low breakdown.

### Step 4: Current Demand (Eq. 1 and Eq. 3)

```
I_ci = A_c × f_ci × i_cm = 10175.6 × 0.001    × 0.060 = 0.611 A
I_cm = A_c × f_cm × i_cm = 10175.6 × 0.001375 × 0.060 = 0.839 A
I_cf = A_c × f_cf × i_cm = 10175.6 × 0.001750 × 0.060 = 1.068 A
```

The mean demand (0.839 A) is the basis for anode mass sizing.
The final demand (1.068 A) verifies end-of-life performance.

Total charge over 25 years (sizing basis for Eq. 5):

```
Q_total = I_cm × t_f × 8760
Q_total = 0.839 × 25 × 8760
Q_total = 183,847 Ah
```

### Step 5: Anode Mass Requirement (Eq. 5)

Aluminium anode electrochemical capacity:

```
acc = 2000 Ah/kg   (Al-Zn-In alloy at ~20 °C seawater)
```

Required total anode mass:

```
M_total = Q_total / (acc × u)
M_total = 183,847 / (2000 × 0.80)
M_total = 114.9 kg
```

With contingency factor 1.10 and individual anode mass 300 kg:

```
N_raw  = 114.9 / 300 × 1.10 = 0.421 → round up → 1 anode
```

A single 300 kg bracelet anode covers the entire 25-year charge requirement for this
short, well-coated pipeline at this current density.

### Step 6: Anode Spacing Check

For N = 1 anode on a 10,000 m pipeline:

```
spacing = L / (N - 1)   → not applicable (single anode)
```

With only one anode the concept of inter-anode spacing does not apply. In practice,
the designer would review whether the attenuation length (calculated below) supports
single-point protection for a 10 km line.

### Step 7: Longitudinal Resistance (Metallic Attenuation, Eq. 11)

DNV-RP-F103 (2010) Eq. 11 longitudinal resistance per unit length:

```
R_Me/m = ρ_Me / (π × d × (D − d))
       = 2.0×10⁻⁷ / (π × 0.0127 × (0.3239 − 0.0127))
       = 2.0×10⁻⁷ / (π × 0.0127 × 0.3112)
       = 2.0×10⁻⁷ / 0.01241
       = 1.61×10⁻⁵ Ω/m
```

Attenuation length (simplified):

```
La = sqrt((D × WT × ρ_coating) / (4 × ρ_steel × ln(D/d)))
```

Where ρ_coating is coating resistivity (Ω·m). For a high-quality 3LPE system, La
is typically in the range 200–500 m, confirming that a single anode at the pipe
mid-point may not deliver adequate protection at both ends of a 10 km section.
The code computes La = 272 m for the default coating resistivity value.

---

## Results Summary

| Output | Value | Unit |
|--------|-------|------|
| Pipeline outer surface area | 10,175.6 | m² |
| Current density i_cm (Table 5-1) | 0.060 | A/m² |
| Temperature band | >50–80 °C | — |
| Initial coating factor f_ci | 0.001000 | — |
| Mean coating factor f_cm | 0.001375 | — |
| Final coating factor f_cf | 0.001750 | — |
| Initial current demand I_ci | 0.611 | A |
| Mean current demand I_cm | 0.839 | A |
| Final current demand I_cf | 1.068 | A |
| Total charge Q_total | 183,848 | Ah |
| Anode mass required | 114.9 | kg |
| Anode count (300 kg each, 1.10 cont.) | 1 | — |
| Actual installed mass | 300 | kg |
| Attenuation length La | 272 | m |

---

## Python Example

```python
from digitalmodel.infrastructure.common.cathodic_protection import CathodicProtection

cfg = {
    "inputs": {
        "calculation_type": "DNV_RP_F103_2010",
        "design_data": {"design_life": 25.0},
        "pipeline": {
            "outer_diameter_m": 0.3239,
            "wall_thickness_m": 0.0127,
            "length_m": 10000.0,
            "burial_condition": "non_buried",
            "internal_fluid_temperature_C": 60.0,
            "coating_type": "3LPE",
            "resistivity_ohm_m": 0.2e-6,
        },
        "environment": {},
        "anode": {
            "material": "aluminium",
            "utilization_factor": 0.80,
            "individual_anode_mass_kg": 300.0,
            "contingency_factor": 1.10,
            "min_spacing_m": 5.0,
            "max_spacing_m": 500.0,
        },
    }
}

cp = CathodicProtection()
result = cp.router(cfg)
r = result["results"]

print("Pipeline surface area:   {:.1f} m²".format(
    r["pipeline_geometry_m"]["outer_surface_area_m2"]))
print("Current density i_cm:    {:.3f} A/m²".format(
    r["current_densities_mA_m2"]["mean_current_density_A_m2"]))
print("Temperature band:        {}".format(
    r["current_densities_mA_m2"]["temperature_band"]))
print("Coating factor f_cm:     {:.6f}".format(
    r["coating_breakdown_factors"]["mean_factor"]))
print("Mean current demand:     {:.3f} A".format(
    r["current_demand_A"]["mean_current_demand_A"]))
print("Final current demand:    {:.3f} A".format(
    r["current_demand_A"]["final_current_demand_A"]))
print("Total charge:            {:.0f} Ah".format(
    r["current_demand_A"]["total_charge_Ah"]))
print("Anode mass required:     {:.1f} kg".format(
    r["anode_requirements"]["total_anode_mass_kg"]))
print("Anode count:             {}".format(
    r["anode_requirements"]["anode_count"]))
print("Attenuation length:      {:.1f} m".format(
    r["attenuation_analysis"]["attenuation_length_m"]))
```

---

## Validation Notes

1. **3LPE coating performance:** With a = 0.001 and b = 0.00003, the 3LPE breakdown
   factors are an order of magnitude lower than FBE (a = 0.010, b = 0.0003). This
   directly reduces current demand for a given surface area.

2. **Current density band >50–80 °C:** The 60 °C internal fluid temperature falls in
   the second temperature band. For a pipeline with T ≤ 50 °C, i_cm would be 0.050 A/m²
   (17% lower). Accurate temperature specification is critical.

3. **Single anode result:** The mass calculation yields only one 300 kg anode for a
   10 km pipeline because the 3LPE coating is very effective. The attenuation analysis
   (La = 272 m) shows the single anode at mid-pipe may not protect both ends of the
   10 km line. The designer must verify adequate potential at the pipeline extremities,
   typically by increasing anode count or using a FLET-based CP approach.

4. **F103-2010 vs 2016:** The 2010 edition uses the linear coating breakdown model
   (Eq. 2/4) and Table 5-1 current densities. The 2016 edition uses a quadratic model
   (Table 6-4) and different current density table values; these are incompatible.
   This example uses the 2010 edition exclusively.

5. **No safety factor:** F103-2010 does not include a built-in safety factor k in
   the current demand formula. If a company specification requires a safety factor,
   it is applied externally via the `design_margin` parameter in the `design` dict.
