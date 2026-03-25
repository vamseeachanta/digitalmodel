# Worked Example 01 — Submarine Pipeline Cathodic Protection

**Standard:** DNV-RP-F103 (October 2010 Edition)
**Structure type:** Submarine pipeline (non-buried, FBE-coated)
**Anode type:** Aluminium bracelet anodes

---

## 1. Input Parameters

| Parameter | Symbol | Value | Unit |
|---|---|---|---|
| Pipeline outer diameter | D | 0.610 | m (24 inch) |
| Wall thickness | d | 0.0191 | m |
| Pipeline length | L | 5,000 | m |
| Burial condition | — | non_buried | — |
| Internal fluid temperature | T_f | 40 | °C |
| Coating type | — | FBE | — |
| Design life | t_f | 25 | years |
| Steel resistivity (CMn steel) | ρ_Me | 2.0 × 10⁻⁷ | Ω·m |
| Anode material | — | aluminium | — |
| Anode shape | — | bracelet | — |
| Net anode mass | m_a | 150 | kg |
| Utilisation factor | u | 0.80 | — |
| Contingency factor | k | 1.0 | — |
| Min anode spacing | s_min | 5 | m |
| Max anode spacing | s_max | 300 | m |

---

## 2. Step-by-Step Calculation Narrative

### Step 1 — Pipeline Surface Area

The exposed outer surface area of the pipeline is the bare-cylinder formula:

```
A_c = π × D × L
A_c = π × 0.610 × 5000
A_c = 9,582.0 m²
```

This is the area that requires cathodic protection. Because the pipeline has no
weight-coating in this example, the wetted surface area equals the outer surface area.

### Step 2 — Current Density from Table 5-1

DNV-RP-F103 (2010) Table 5-1 specifies the mean design current density as a
function of burial condition and internal fluid temperature band:

| Burial condition | ≤50 °C | >50–80 °C | >80–120 °C | >120 °C |
|---|---|---|---|---|
| non_buried | **0.050** A/m² | 0.060 A/m² | 0.070 A/m² | 0.100 A/m² |
| buried | 0.020 A/m² | 0.025 A/m² | 0.030 A/m² | 0.040 A/m² |

For T_f = 40 °C (band ≤50 °C) and non_buried:

```
i_cm = 0.050 A/m²
```

### Step 3 — Coating Breakdown Factors (Annex 1 Linear Formula)

For FBE coating, Annex 1 Table A.1 gives:

```
a = 0.010  (initial breakdown factor)
b = 0.0003 (degradation rate per year)
```

Applying the linear formulae (Eq. 2 and Eq. 4) for t_f = 25 years:

```
f_ci = a = 0.010
f_cm = a + 0.5 × b × t_f = 0.010 + 0.5 × 0.0003 × 25 = 0.010 + 0.00375 = 0.01375
f_cf = a + b × t_f       = 0.010 + 0.0003 × 25       = 0.010 + 0.0075   = 0.0175
```

The mean breakdown factor f_cm = 0.01375 represents that 1.375 % of the pipeline
surface is effectively bare steel at the design midpoint.

### Step 4 — Total Current Demand (Eq. 1 and Eq. 3)

```
I_cm = A_c × f_cm × i_cm  (Eq. 1 — mean current demand)
I_cm = 9582.0 × 0.01375 × 0.050
I_cm = 6.588 A

I_cf = A_c × f_cf × i_cm  (Eq. 3 — final current demand)
I_cf = 9582.0 × 0.0175 × 0.050
I_cf = 8.384 A
```

The mean current demand (6.6 A) is used for anode mass sizing.
The final demand (8.4 A) verifies the system can deliver adequate current at end-of-life.

### Step 5 — Anode Electrochemical Capacity

For aluminium alloy anodes at 20 °C seawater:

```
acc = 2000 Ah/kg
```

The capacity formula with temperature correction used in the code is:
```
acc = 2000 - 27 × (T - 20)
```
At T = 20 °C: acc = 2000 Ah/kg.

### Step 6 — Total Anode Mass Required

Total charge to be delivered over design life:

```
Q_total = I_cm × t_f × 8760   (hours per year)
Q_total = 6.588 × 25 × 8760
Q_total = 1,441,740 Ah
```

Required anode mass:

```
M_total = Q_total / (acc × u)
M_total = 1,441,740 / (2000 × 0.80)
M_total = 901.1 kg
```

### Step 7 — Anode Count

With individual anode net mass m_a = 150 kg and contingency factor k = 1.0:

```
N = ceil(M_total / m_a × k)
N = ceil(901.1 / 150 × 1.0)
N = ceil(6.007)
N = 7 anodes
```

Actual installed mass = 7 × 150 = 1,050 kg.

### Step 8 — Anode Spacing

For 7 anodes distributed end-to-end along 5,000 m:

```
spacing = L / (N - 1) = 5000 / 6 = 833.3 m
```

This exceeds the recommended maximum of 300 m, indicating that more anodes are
needed to meet the spacing criterion. In practice the anode spacing limit governs,
and additional anodes are placed at the required 300 m pitch (approximately 17
anodes for a 5 km pipeline).

---

## 3. Python Code Block

```python
from digitalmodel.infrastructure.common.cathodic_protection import CathodicProtection

cfg = {
    "inputs": {
        "calculation_type": "DNV_RP_F103_2010",
        "design_data": {"design_life": 25.0},
        "pipeline": {
            "outer_diameter_m": 0.610,
            "wall_thickness_m": 0.0191,
            "length_m": 5000.0,
            "burial_condition": "non_buried",
            "internal_fluid_temperature_C": 40.0,
            "coating_type": "FBE",
            "resistivity_ohm_m": 0.2e-6,
        },
        "environment": {},
        "anode": {
            "material": "aluminium",
            "utilization_factor": 0.80,
            "individual_anode_mass_kg": 150.0,
            "contingency_factor": 1.0,
            "min_spacing_m": 5.0,
            "max_spacing_m": 300.0,
        },
    }
}

cp = CathodicProtection()
result = cp.router(cfg)
r = result["results"]

print("Pipeline surface area:  {:.1f} m²".format(
    r["pipeline_geometry_m"]["outer_surface_area_m2"]))
print("Current density i_cm:   {:.3f} A/m²".format(
    r["current_densities_mA_m2"]["mean_current_density_A_m2"]))
print("Coating factor f_cm:    {:.5f}".format(
    r["coating_breakdown_factors"]["mean_factor"]))
print("Mean current demand:    {:.3f} A".format(
    r["current_demand_A"]["mean_current_demand_A"]))
print("Final current demand:   {:.3f} A".format(
    r["current_demand_A"]["final_current_demand_A"]))
print("Anode mass required:    {:.1f} kg".format(
    r["anode_requirements"]["total_anode_mass_kg"]))
print("Anode count:            {}".format(
    r["anode_requirements"]["anode_count"]))
print("Anode spacing:          {:.1f} m".format(
    r["anode_spacing_m"]["spacing_m"]))
print("Spacing valid:          {}".format(
    r["anode_spacing_m"]["spacing_valid"]))
```

---

## 4. Expected Output

| Output | Value | Unit |
|---|---|---|
| Pipeline outer surface area | 9,582.0 | m² |
| Mean current density (Table 5-1) | 0.050 | A/m² |
| Initial coating factor f_ci | 0.01000 | — |
| Mean coating factor f_cm | 0.01375 | — |
| Final coating factor f_cf | 0.01750 | — |
| Mean current demand I_cm | 6.588 | A |
| Final current demand I_cf | 8.384 | A |
| Total anode mass required | ~901 | kg |
| Anode count (mass-governed) | 7 | — |
| Anode spacing (7 anodes) | 833.3 | m |
| Spacing valid (≤300 m) | False | — |

---

## 5. Interpretation Notes

1. **Governing criterion:** The spacing validity check fails because 7 anodes spaced
   over 5 km yields 833 m intervals, exceeding the 300 m maximum. The designer should
   increase the anode count to meet the spacing constraint (approximately 17+ anodes).

2. **FBE coating quality:** FBE is a well-performing coating type (a=0.01, b=0.0003).
   Better coatings (e.g. 3LPE: a=0.001, b=0.00003) would reduce current demand
   significantly.

3. **Non-buried vs. buried:** A non-buried pipeline has a higher current density
   requirement (0.050 A/m² vs. 0.020 A/m² for buried) because the seawater exposure
   is greater and there is no natural shielding from the burial medium.

4. **Design life sensitivity:** Extending the design life to 40 years increases
   f_cm from 0.01375 to 0.016 and f_cf from 0.0175 to 0.022, raising current demand
   proportionally.
