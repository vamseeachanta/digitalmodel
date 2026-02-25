# Offshore Vessel Hull CP Design — ABS GN Ships 2017

**Standard:** ABS Guidance Notes on Cathodic Protection of Ships (December 2017)
**Structure type:** Floating Storage Terminal (FST) hull — submerged zone
**Anode type:** Long-flush aluminium anodes

---

## Standard and Scope

ABS Guidance Notes on Cathodic Protection of Ships (December 2017) covers the design
of sacrificial anode cathodic protection systems for ship and vessel hulls. The method
implements:

- Section 2.4.4 Table 4: Coating breakdown factors (compound-growth model)
- Section 2.4.4 Table 5: Design current densities for coated and bare steel
- Anode mass formula: M = I_cm × t_f × 8760 / (acc × u)
- Anode resistance for long-flush geometry: R = ρ / (2 × s_mean)

---

## Design Inputs

| Parameter | Value | Unit |
|-----------|-------|------|
| Average wetted surface area | 10,778 | m² |
| Coating coverage | 100 | % |
| Design life | 5 | years |
| Seawater temperature | 14 | °C |
| Seawater resistivity | 0.325 | Ω·m |
| Coating durability | High | — |
| Initial coating breakdown rate | 1.0 | %/yr |
| Initial breakdown duration | 2.0 | years |
| Annual coating breakdown rate | 1.0 | %/yr |
| Max coating breakdown factor | 2.0 | — |
| Coated steel current density | 13.5 | mA/m² |
| Bare steel current density | 200.0 | mA/m² |
| Anode material | Al alloy | — |
| Anode geometry | long_flush | — |
| Anode mean length | 1.00 | m |
| Anode width | 0.125 | m |
| Anode net weight | 29.0 | kg |
| Utilisation factor | 0.825 | — |
| Anode potential | −1.09 | V vs Ag/AgCl |
| Min protection potential | −0.800 | V vs Ag/AgCl |

---

## Calculation Steps

### Step 1: Coating Breakdown Factors (ABS Section 2.4.4 Table 4)

The ABS method uses a compound-growth model (not the linear DNV formula):

```
f_initial = 1 + α/100 = 1 + 1.0/100 = 1.010   (per year for 2 years)
f_yearly  = 1 + β/100 = 1 + 1.0/100 = 1.010   (per year thereafter)
```

Final factor after t_f = 5 years (2 initial + 3 remaining):

```
fcf_raw = f_initial^2 × f_yearly^3
        = 1.010^2 × 1.010^3
        = 1.010^5
        = 1.051010
fcf = min(fcf_raw, 2.0) = 1.051010   (well below cap)
```

Mean factor (average of yearly factor at start of design life and final factor):

```
fcm = (f_yearly + fcf) / 2
    = (1.010 + 1.051010) / 2
    = 1.030505
```

### Step 2: Current Demand

With 100% coating coverage, there is no bare-steel disbonding contribution.
All current demand comes from the coated area:

```
A_coated = 10,778 × 1.00 = 10,778 m²

I_cm = (i_coated × A_coated × fcm) / 1000
     = (13.5 × 10,778 × 1.030505) / 1000
     = 149.94 A
```

### Step 3: Anode Current Capacity

For aluminium alloy at T = 14 °C seawater:

```
acc = 2000 − 27 × (T − 20)
    = 2000 − 27 × (14 − 20)
    = 2000 + 162
    = 2162 Ah/kg
```

### Step 4: Total Anode Mass Required

```
M_total = I_cm × t_f × 8760 / (acc × u)
        = 149.94 × 5 × 8760 / (2162 × 0.825)
        = 6,562,392 / 1,783.65
        = 3,679 kg
```

### Step 5: Anode Count

```
N = M_total / m_a = 3,679 / 29.0 = 126.9 → 127 anodes
```

### Step 6: Anode Resistance (Long-Flush Formula)

For a long-flush anode with ρ = 0.325 Ω·m, L = 1.00 m, W = 0.125 m:

```
s_mean = 0.5 × (L + W) = 0.5 × (1.00 + 0.125) = 0.5625 m

R_a = ρ / (2 × s_mean) = 0.325 / (2 × 0.5625) = 0.2889 Ω
```

### Step 7: Individual Anode Current Output

Driving voltage:

```
ΔE = |E_a − E_p| = |−1.09 − (−0.800)| = 0.290 V
```

Current output per anode (initial condition):

```
I_a = ΔE / R_a = 0.290 / 0.2889 = 1.004 A
```

Total initial current from 127 anodes:

```
I_total = 127 × 1.004 = 127.5 A
```

---

## Results Summary

| Output | Value | Unit |
|--------|-------|------|
| Wetted surface area | 10,778 | m² |
| Coating factor — initial (f_initial) | 1.010 | — |
| Coating factor — mean (fcm) | 1.031 | — |
| Coating factor — final (fcf) | 1.051 | — |
| Aluminium anode capacity (14 °C) | 2,162 | Ah/kg |
| Mean current demand I_cm | 149.9 | A |
| Total anode mass required | 3,679 | kg |
| Anode count (5-year design) | 127 | — |
| Anode resistance (long-flush, 1.00 m) | 0.289 | Ω |
| Driving voltage ΔE | 0.290 | V |
| Current output per anode | ~1.00 | A |

---

## Python Example

```python
from digitalmodel.infrastructure.common.cathodic_protection import CathodicProtection

cfg = {
    "inputs": {
        "calculation_type": "ABS_gn_ships_2018",
        "design_data": {
            "design_life": 5,
            "seawater_max_temperature": 14,
        },
        "environment": {
            "seawater": {"resistivity": {"input": 0.325}},
        },
        "structure": {
            "steel_total_area": 10778.0,
            "area_coverage": 100.0,
            "coating_initial_breakdown_factor": 1.0,
            "coating_initial_breakdown_duration": 2.0,
            "coating_yearly_breakdown_factor": 1.0,
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
                "length_m": 1.00,
                "width_m": 0.125,
            },
        },
    }
}

cp = CathodicProtection()
result = cp.router(cfg)
r = result["cathodic_protection"]

print("Anode capacity (acc):      {:.0f} Ah/kg".format(
    r["anode_current_capacity"]))
print("Coating factor (final):    {:.6f}".format(
    r["coating_breakdown_factors"]["final_factor"]))
print("Coating factor (mean):     {:.6f}".format(
    r["coating_breakdown_factors"]["mean_factor"]))
print("Mean current demand:       {:.2f} A".format(
    r["current_demand_A"]["totals"]["mean"]))
print("Total anode mass:          {:.1f} kg".format(
    r["anode_requirements"]["total_mass_kg"]))
print("Anode count:               {:.1f}".format(
    r["anode_requirements"]["anode_count"]))
print("Anode resistance (initial): {:.4f} Ohm".format(
    r["anode_performance"]["resistance_ohm"]["initial"]))
print("Current output per anode:  {:.3f} A".format(
    r["anode_performance"]["current_output_A"]["initial_per_anode"]))
```

---

## Sensitivity — 5-Year vs 25-Year Design Life

This section compares key outputs for the same hull protected for 5 years (primary
design) and 25 years.

**High-durability coating (α = 1.0%/yr, β = 1.0%/yr, cap = 2.0):**

| Output | 5-year | 25-year | Unit |
|--------|--------|---------|------|
| Final coating factor fcf | 1.051 | 1.282 | — |
| Mean coating factor fcm | 1.031 | 1.146 | — |
| Acc (Al, 14 °C) | 2,162 | 2,162 | Ah/kg |
| Mean current demand I_cm | 149.9 | 166.8 | A |
| Total anode mass required | 3,679 | 20,477 | kg |
| Anode count (29 kg each) | 127 | 707 | — |

The 25-year design requires approximately 5.6× more anodes despite the mean current
demand being only 11% higher. The large difference is driven almost entirely by the
longer duration (25 vs 5 years) in the anode mass formula M = I × t × 8760 / (acc × u).

**25-year Python configuration change** — set `"design_life": 25` in `design_data`.
All other parameters remain identical.

---

## Validation Notes

1. **High-durability vs low-durability:** For the same 10,778 m² hull, switching from
   high-durability (α=β=1%/yr) to low-durability coating (α=β=3%/yr, cap=2.0) at
   5-year design life would increase the final factor to ~1.137 and raise anode count
   from 127 to approximately 160.

2. **Temperature effect on anode capacity:** At 14 °C, aluminium capacity is 2,162 Ah/kg
   (higher than the nominal 2,000 Ah/kg at 20 °C). In tropical conditions (T = 30 °C),
   capacity drops to 1,730 Ah/kg — increasing anode mass by 25%.

3. **Resistivity effect on resistance:** The design uses ρ = 0.325 Ω·m (coastal inlet
   salinity ~24.5 ppt at 14 °C). In higher-salinity open ocean (ρ = 0.25 Ω·m), anode
   resistance would be proportionally lower, improving current delivery per anode.

4. **ABS vs DNV coating model:** ABS uses a compound-growth model (fcf = f_i^2 × f_y^3
   for 5 years), whereas DNV-RP-F103 uses a linear model (fcf = a + b×t). The models
   give different numerical results even for similar input parameters; they should not
   be mixed.
