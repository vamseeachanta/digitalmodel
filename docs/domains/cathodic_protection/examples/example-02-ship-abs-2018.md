# Worked Example 02 — Ship Hull Cathodic Protection

**Standard:** ABS Guidance Notes on Cathodic Protection of Ships (December 2017)
**Structure type:** Ship hull submerged steel surface
**Anode type:** Long-flush aluminium anodes

---

## 1. Input Parameters

| Parameter | Symbol | Value | Unit |
|---|---|---|---|
| Total steel area | A_total | 4,500 | m² |
| Coated area coverage | — | 95 % | — |
| Design life | t_f | 5 | years |
| Seawater temperature | T | 20 | °C |
| Seawater resistivity | ρ | 0.25 | Ω·m |
| Anode geometry | — | long_flush | — |
| Anode length | L_a | 0.65 | m |
| Anode width | W_a | 0.125 | m |
| Anode material | — | aluminium | — |
| Anode utilisation factor | u | 0.825 | — |
| Net anode weight | m_a | 29 | kg |
| Coated current density | i_c | 13.5 | mA/m² |
| Uncoated current density | i_u | 200.0 | mA/m² |
| Initial coating breakdown | α | 2.0 | % |
| Initial breakdown duration | — | 2 | years |
| Yearly coating breakdown | β | 3.0 | % per year |
| Max coating breakdown factor | — | 2.0 | — |
| Protection potential | E_p | −0.80 | V vs Ag/AgCl |
| Anode closed-circuit potential | E_a | −1.09 | V vs Ag/AgCl |

---

## 2. Step-by-Step Calculation Narrative

### Step 1 — Surface Areas

```
coated_area   = 4500 × 0.95 = 4,275 m²
uncoated_area = 4500 × 0.05 = 225 m²
```

### Step 2 — Coating Breakdown Factors

The ABS GN Ships method uses a compound-growth model rather than a linear model.

Initial factor:

```
f_initial = 1 + α/100 = 1 + 0.02 = 1.02  (per year for 2 years)
f_yearly  = 1 + β/100 = 1 + 0.03 = 1.03  (per year thereafter)
```

Final factor after t_f = 5 years (2 initial + 3 remaining):

```
fcf_raw = f_initial^2 × f_yearly^3
        = 1.02² × 1.03³
        = 1.0404 × 1.092727
        = 1.1369  (rounded to 4 d.p.)
fcf = min(fcf_raw, max_factor) = min(1.1369, 2.0) = 1.1369
```

Mean factor:

```
fcm = (f_yearly + fcf) / 2 = (1.03 + 1.1369) / 2 = 1.0835
```

### Step 3 — Current Densities

The coated current densities scale with the breakdown factors:

```
i_c_initial = 13.5 mA/m²
i_c_mean    = 13.5 × fcm = 13.5 × 1.0835 = 14.63 mA/m²
i_c_final   = 13.5 × fcf = 13.5 × 1.1369 = 15.35 mA/m²
```

Disbonding current (for the uncoated fraction only):

```
i_disbond_mean  = 200 × (fcm − 1.0) × uncoated_area / 1000
               = 200 × 0.0835 × 225 / 1000
               = 3.758 A

i_disbond_final = 200 × (fcf − 1.0) × uncoated_area / 1000
                = 200 × 0.1369 × 225 / 1000
                = 6.161 A
```

### Step 4 — Total Current Demand

Coated steel contribution:

```
I_cm_coated = i_c_mean × coated_area / 1000
            = 14.63 × 4275 / 1000
            = 62.54 A

I_cf_coated = i_c_final × coated_area / 1000
            = 15.35 × 4275 / 1000
            = 65.62 A
```

Total mean current demand (coated + disbonding):

```
I_cm_total = I_cm_coated + i_disbond_mean
           = 62.54 + 3.76
           = 66.30 A
```

### Step 5 — Anode Current Capacity

For aluminium at T = 20 °C:

```
acc = 2000 − 27 × (20 − 20) = 2000 Ah/kg
```

### Step 6 — Total Anode Mass Required

```
M_total = I_cm_total × t_f × 8760 / (acc × u)
        = 66.30 × 5 × 8760 / (2000 × 0.825)
        = 2,903,940 / 1,650
        = 1,760 kg
```

### Step 7 — Anode Count

```
N = M_total / m_a = 1760 / 29 = 60.7 → 61 anodes
```

### Step 8 — Anode Resistance (Long-Flush)

For a flush-mounted anode, the Dwight formula gives:

```
s_mean = 0.5 × (L_a + W_a) = 0.5 × (0.65 + 0.125) = 0.3875 m

R_a = ρ / (2 × s_mean) = 0.25 / (2 × 0.3875) = 0.3226 Ω
```

### Step 9 — Current Output per Anode

Driving voltage:

```
ΔE = |E_a − E_p| = |−1.09 − (−0.80)| = 0.29 V
```

Current output per anode:

```
I_a = ΔE / R_a = 0.29 / 0.3226 = 0.899 A
```

Total current from 61 anodes:

```
I_total = 61 × 0.899 = 54.8 A
```

---

## 3. Python Code Block

```python
from digitalmodel.infrastructure.common.cathodic_protection import CathodicProtection

cfg = {
    "inputs": {
        "calculation_type": "ABS_gn_ships_2018",
        "design_data": {
            "design_life": 5,
            "seawater_max_temperature": 20,
        },
        "environment": {
            "seawater": {"resistivity": {"input": 0.25}},
        },
        "structure": {
            "steel_total_area": 4500.0,
            "area_coverage": 95.0,
            "coating_initial_breakdown_factor": 2.0,
            "coating_initial_breakdown_duration": 2.0,
            "coating_yearly_breakdown_factor": 3.0,
            "coating_breakdown_factor_max": 2.0,
        },
        "design_current": {
            "coated_steel_mA_m2": 13.5,
            "uncoated_steel_mA_m2": 200.0,
        },
        "anode": {
            "material": "aluminium",
            "protection_potential": 0.8,
            "closed_circuit_anode_potential": -1.09,
            "anode_Utilisation_factor": 0.825,
            "physical_properties": {"net_weight": 29.0},
            "geometry": {
                "type": "long_flush",
                "length_m": 0.65,
                "width_m": 0.125,
            },
        },
    }
}

cp = CathodicProtection()
cp.ABS_gn_ships_2018(cfg)

r = cfg["cathodic_protection"]

print("Coating breakdown final:  {:.4f}".format(
    r["coating_breakdown_factors"]["final_factor"]))
print("Coating breakdown mean:   {:.4f}".format(
    r["coating_breakdown_factors"]["mean_factor"]))
print("Mean current demand:      {:.2f} A".format(
    r["current_demand_A"]["totals"]["mean"]))
print("Final current demand:     {:.2f} A".format(
    r["current_demand_A"]["totals"]["final"]))
print("Total anode mass:         {:.1f} kg".format(
    r["anode_requirements"]["total_mass_kg"]))
print("Anode count:              {:.1f}".format(
    r["anode_requirements"]["anode_count"]))
print("Anode resistance (initial): {:.4f} Ω".format(
    r["anode_performance"]["resistance_ohm"]["initial"]))
print("Current output / anode:   {:.3f} A".format(
    r["anode_performance"]["current_output_A"]["initial_per_anode"]))
```

---

## 4. Expected Output

| Output | Value | Unit |
|---|---|---|
| Coating factor — initial | 1.0200 | — |
| Coating factor — mean | ~1.0835 | — |
| Coating factor — final | ~1.1369 | — |
| Mean current demand | ~66.3 | A |
| Final current demand | ~71.8 | A |
| Total anode mass | ~1,760 | kg |
| Anode count | ~61 | — |
| Anode resistance (initial) | ~0.323 | Ω |
| Driving voltage ΔE | ~0.29 | V |
| Current output per anode | ~0.90 | A |

---

## 5. Variant — 20-Year Design Life Comparison

> **Note:** For comparison, if the same hull were protected for 20 years instead of 5,
> the final breakdown factor is capped at max_factor = 2.0 because the uncapped
> compounding would exceed 2.0 for this coating specification.

With t_f = 20 years:

```
Initial years: 2 (same)
Remaining years: 18

fcf_raw = 1.02^2 × 1.03^18
        = 1.0404 × 1.7024
        ≈ 1.771   (below the 2.0 cap, so fcf = 1.771)

fcm = (1.03 + 1.771) / 2 = 1.401

I_cm_coated = 13.5 × 1.401 × 4275 / 1000 = 80.8 A
I_cm_total  ≈ 89 A

M_total = 89 × 20 × 8760 / (2000 × 0.825) ≈ 9,474 kg
N       ≈ 327 anodes
```

The 20-year case requires roughly 5.4× more anode mass than the 5-year case,
illustrating the strong sensitivity of CP system size to design life.

---

## 6. Interpretation Notes

1. **Mean current governs anode mass:** The ABS GN Ships formula sizes anode mass
   on the mean current demand, which represents the time-averaged protection
   requirement.

2. **Coating breakdown cap:** The maximum breakdown factor of 2.0 prevents
   unrealistic current demands for very long design lives with poor coatings.

3. **Anode current output vs. demand:** The initial total current output (54.8 A)
   is less than the mean demand (66.3 A). This is common in ship CP design
   because mean current is the sizing basis; instantaneous output is checked
   separately for initial and final conditions.

4. **Temperature effect on capacity:** At 30 °C instead of 20 °C, the aluminium
   anode capacity drops to 2000 − 27 × 10 = 1,730 Ah/kg, increasing required
   anode mass by ~15%.
