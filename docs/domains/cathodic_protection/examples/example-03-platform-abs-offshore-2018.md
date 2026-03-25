# Worked Example 03 — Fixed Offshore Jacket Cathodic Protection

**Standard:** ABS Guidance Notes on Cathodic Protection of Offshore Structures (December 2018)
**Structure type:** Fixed jacket, submerged zone, tropical environment
**Anode type:** Aluminium flush anodes

---

## 1. Input Parameters

| Parameter | Symbol | Value | Unit |
|---|---|---|---|
| Surface area (submerged) | A_c | 2,000 | m² |
| Water depth | h | 50 | m |
| Climatic region | — | tropical | — |
| Zone | — | submerged | — |
| Design life | t_f | 25 | years |
| Seawater temperature | T | 25 | °C |
| Anode material | — | aluminium | — |
| Anode net mass | m_a | 100 | kg |
| Utilisation factor | u | 0.80 | — |

---

## 2. Step-by-Step Calculation Narrative

### Step 1 — Current Density Selection (ABS Table 1, p. 12)

ABS GN Offshore 2018 Section 2/11.7.2 Table 1 tabulates initial design current
densities for bare metal surfaces (mA/m²) by climatic region and water-depth band.

**Depth band for h = 50 m:** 50 m falls in the ">30–100 m" band.

| Region | 0–30 m | >30–100 m | >100–300 m | >300 m |
|---|---|---|---|---|
| Tropical | 150 | **120** | 140 | 180 |
| Sub-tropical | 170 | 140 | 160 | 200 |
| Temperate | 200 | 170 | 190 | 220 |
| Arctic | 250 | 200 | 220 | 220 |

For tropical, >30–100 m depth band:

```
i_ci = 120 mA/m²   (initial — Table 1)
i_cm = i_ci / 2  = 120 / 2 = 60 mA/m²   (Section 2/11.7.2)
i_cf = i_ci × 2/3 = 120 × 2/3 = 80 mA/m²  (Section 2/11.7.2)
```

### Step 2 — Coating Breakdown Factors (Section 2/11.7.4, p. 13)

The formula is:

```
f_c = α + β × t
```

For water depth > 30 m (deep zone):

```
α = 0.01        (initial breakdown fraction)
β_mean  = 0.004  (mean degradation rate)
β_final = 0.008  (final degradation rate)
```

Applying for t = 25 years:

```
f_ci = α            = 0.01
f_cm = α + 0.004×25 = 0.01 + 0.10 = 0.11
f_cf = α + 0.008×25 = 0.01 + 0.20 = 0.21
```

At the end of design life 21 % of the steel surface is effectively uncoated.

For reference, the shallow-water zone (≤30 m) uses faster degradation rates
(β_mean = 0.006, β_final = 0.012), reflecting wave action and UV exposure.

### Step 3 — Current Demand (Section 2/11.7.3, p. 13)

```
I_c = A_c × i_c (A/m²) × f_c
```

Converting current densities from mA/m² to A/m²:

```
I_ci = 2000 × (120/1000) × 0.01 = 2000 × 0.120 × 0.01 = 2.400 A
I_cm = 2000 × (60/1000)  × 0.11 = 2000 × 0.060 × 0.11 = 13.200 A
I_cf = 2000 × (80/1000)  × 0.21 = 2000 × 0.080 × 0.21 = 33.600 A
```

The mean current demand (13.2 A) is the sizing basis for anode mass.
The final demand (33.6 A) represents the worst-case condition at end-of-life.

### Step 4 — Anode Current Capacity

For aluminium alloy at T = 25 °C seawater:

```
acc = 2000 − 27 × (T − 20)
acc = 2000 − 27 × (25 − 20)
acc = 2000 − 135
acc = 1,865 Ah/kg
```

### Step 5 — Anode Mass Required

```
M = I_cm × t_f × 8760 / (acc × u)
M = 13.200 × 25 × 8760 / (1865 × 0.80)
M = 2,890,800 / 1,492
M = 1,937 kg
```

### Step 6 — Anode Count

With individual anode net mass m_a = 100 kg:

```
N = ceil(M / m_a) = ceil(1937 / 100) = ceil(19.37) = 20 anodes
```

Installed anode mass = 20 × 100 = 2,000 kg.

---

## 3. Python Code Block

```python
from digitalmodel.infrastructure.common.cathodic_protection import CathodicProtection

cfg = {
    "inputs": {
        "calculation_type": "ABS_gn_offshore_2018",
        "design_data": {
            "design_life": 25.0,
            "seawater_max_temperature": 25.0,
        },
        "structure": {
            "surface_area_m2": 2000.0,
            "water_depth_m": 50.0,
            "climatic_region": "tropical",
            "zone": "submerged",
        },
        "anode": {
            "material": "aluminium",
            "utilisation_factor": 0.80,
            "physical_properties": {"net_weight": 100.0},
        },
    }
}

cp = CathodicProtection()
result = cp.router(cfg)
r = result["results"]

print("Climatic region:       {}".format(r["climatic_region"]))
print("Water depth:           {:.0f} m".format(r["water_depth_m"]))
print("Design life:           {:.0f} years".format(r["design_life_years"]))
print()

cd = r["current_densities_mA_m2"]
print("i_ci (initial):        {:.1f} mA/m²".format(cd["initial"]))
print("i_cm (mean):           {:.1f} mA/m²".format(cd["mean"]))
print("i_cf (final):          {:.2f} mA/m²".format(cd["final"]))
print()

cb = r["coating_breakdown_factors"]
print("Coating depth zone:    {}".format(cb["depth_zone"]))
print("f_ci (initial):        {:.3f}".format(cb["initial"]))
print("f_cm (mean):           {:.3f}".format(cb["mean"]))
print("f_cf (final):          {:.3f}".format(cb["final"]))
print()

dem = r["current_demand_A"]
print("I_ci (initial demand): {:.3f} A".format(dem["initial"]))
print("I_cm (mean demand):    {:.3f} A".format(dem["mean"]))
print("I_cf (final demand):   {:.3f} A".format(dem["final"]))
print()

print("Anode capacity (acc):  {:.0f} Ah/kg".format(
    r["anode_current_capacity_Ah_kg"]))
print("Anode mass required:   {:.1f} kg".format(r["anode_mass_kg"]))
```

---

## 4. Expected Output

| Output | Value | Unit |
|---|---|---|
| Initial current density i_ci (Table 1) | 120.0 | mA/m² |
| Mean current density i_cm | 60.0 | mA/m² |
| Final current density i_cf | 80.0 | mA/m² |
| Initial coating factor f_ci | 0.01 | — |
| Mean coating factor f_cm | 0.11 | — |
| Final coating factor f_cf | 0.21 | — |
| Initial current demand I_ci | 2.400 | A |
| Mean current demand I_cm | 13.200 | A |
| Final current demand I_cf | 33.600 | A |
| Aluminium anode capacity (at 25 °C) | 1,865 | Ah/kg |
| Total anode mass required | ~1,937 | kg |

---

## 5. Interpretation Notes

1. **Current density progression:** The mean and final current densities (60 and
   80 mA/m²) are derived from the initial Table 1 value (120 mA/m²) using fixed
   ratios (1/2 and 2/3). The same initial density is not directly used for demand
   calculation; the coating factor scales how much of the surface is unprotected
   at each stage.

2. **Coating breakdown dominates at end-of-life:** The final current demand (33.6 A)
   is 2.5× the mean demand (13.2 A). This large ratio is driven by the coating factor
   rising from 0.11 to 0.21 and the current density rising from 60 to 80 mA/m².

3. **Temperature effect:** For tropical conditions with T = 25 °C, aluminium anode
   capacity reduces to 1,865 Ah/kg from the nominal 2,000 Ah/kg. In arctic conditions
   with T = 5 °C, capacity would be 2,000 − 27 × (5 − 20) = 2,405 Ah/kg, reducing
   the required anode mass by ~22%.

4. **Shallow zone comparison:** For a jacket leg in 20 m water depth (≤30 m band)
   in the same tropical region, i_ci = 150 mA/m² and the coating uses
   β_mean = 0.006, β_final = 0.012 (faster breakdown due to near-surface effects).
   This would increase mean coating factor to f_cm = 0.01 + 0.006 × 25 = 0.16
   and raise the mean current demand to 2000 × 75e-3 × 0.16 = 24 A.

5. **Saline mud alternative:** Steel piles embedded in seabed mud would use
   zone="saline_mud" with fixed densities i_ci=25, i_cm=i_cf=20 mA/m² and a
   coating factor of 1.0 (no coating benefit in mud contact).
