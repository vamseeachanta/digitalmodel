# Fixed Offshore Jacket CP Design — DNV-RP-B401 (`DNV_RP_B401_offshore` route)

**Standard:** DNV-RP-B401 Cathodic Protection Design (the route on this branch carries the
May 2021 tables in `cp_DNV_RP_B401_2021.py`)
**Structure type:** Fixed jacket, submerged zone, temperate seawater, 0–30 m
**Anode type:** Aluminium stand-off anodes

---

## Standard and Scope

DNV-RP-B401 covers galvanic and impressed-current CP of offshore structures: fixed jackets,
gravity-based structures, subsea equipment. The `DNV_RP_B401_offshore` route implements the
galvanic design chain for one or more zones:

- mean design current density by zone and seawater temperature (Table 3-1 in the 2021 module)
- coating breakdown f_c(t) = f_ci + k × t by coating category (Sec. 3.4.6), clamped at 1.0
- mean and final current demand per zone, I = A × i_mean × f_c
- anode mass M = I_mean × t_f × 8760 / (ε × u) and count N = ceil(M / m_a) (Sec. 4.3)
- anode resistance by the Dwight formula (Sec. 4.9) and a current-output check against the
  final demand

The edition is selected with `design_data.edition` on the #2207 branch; on `main` the router
ignores that key and always uses the 2021 module. #2207 replaces the tables below with cited,
edition-keyed tables, so the numbers in this example will change when it lands.

---

## Design Inputs

| Parameter | Value | Unit |
|-----------|-------|------|
| Submerged surface area (one zone) | 12,000 | m² |
| Water depth band | 0–30 | m |
| Climatic region / seawater temperature | temperate / 10 | °C |
| Seawater resistivity | 0.30 | Ω·m |
| Coating category | III | — |
| Design life | 25 | years |
| Anode material | aluminium (Al-Zn-In) | — |
| Anode geometry | stand-off | — |
| Anode length / equivalent radius | 1.5 / 0.08 | m |
| Individual anode net mass | 200 | kg |
| Utilisation factor | 0.90 | — |

---

## Calculation Steps (as the route computes them on this branch)

### Step 1: Current density (Table 3-1, submerged, 10 °C → ">7–12 °C" band, coated)

```
i_mean = 0.050 A/m²
```

### Step 2: Coating breakdown (Category III: f_ci = 0.25, k = 0.050 per year)

```
f_ci = 0.25
f_cm = 0.25 + 0.050 × 25 / 2 = 0.875
f_cf = min(0.25 + 0.050 × 25, 1.0) = 1.0
```

### Step 3: Current demand

```
I_mean  = 12,000 × 0.050 × 0.875 = 525.0 A
I_final = 12,000 × 0.050 × 1.0   = 600.0 A
```

### Step 4: Anode mass and count (ε = 2000 Ah/kg, u = 0.90, m_a = 200 kg)

```
M = 525.0 × 25 × 8760 / (2000 × 0.90) = 63,875 kg
N = ceil(63,875 / 200) = 320 anodes
```

### Step 5: Anode resistance (Dwight, L = 1.5 m, r = 0.08 m, ρ = 0.30 Ω·m)

```
R_a = ρ / (2πL) × (ln(4L / r) − 1) = 0.30 / (2π × 1.5) × (ln(75) − 1) = 0.1056 Ω
```

### Step 6: Current-output check (E_structure −0.80 V, E_anode −1.05 V)

```
ΔE       = 0.25 V
I_anode  = 0.25 / 0.1056 = 2.367 A
I_total  = 320 × 2.367 = 757.6 A  ≥  I_final = 600.0 A  → adequate
count by current = ceil(600.0 / 2.367) = 254  → recommended count = max(320, 254) = 320
```

---

## Results Summary (code output on this branch)

| Output | Value | Unit |
|--------|-------|------|
| Mean current density i_mean | 0.050 | A/m² |
| Coating factor f_ci / f_cm / f_cf | 0.25 / 0.875 / 1.0 | — |
| Mean current demand I_mean | 525.0 | A |
| Final current demand I_final | 600.0 | A |
| Total anode mass required | 63,875 | kg |
| Anode count (mass basis) | 320 | — |
| Anode resistance R_a | 0.1056 | Ω |
| Driving voltage ΔE | 0.25 | V |
| Current output per anode | 2.367 | A |
| Total anode current output | 757.6 | A |
| Current-output check | adequate | — |
| Recommended anode count | 320 | — |

---

## Python Example

```python
from digitalmodel.infrastructure.base_solvers.hydrodynamics.cathodic_protection import CathodicProtection

cfg = {
    "inputs": {
        "calculation_type": "DNV_RP_B401_offshore",
        "design_data": {
            "design_life": 25.0,
            "edition": "2021",      # honoured on the #2207 branch; ignored on main
        },
        "structure": {
            "zones": [
                {"zone": "submerged", "area_m2": 12000.0, "coating_category": "III"},
            ],
        },
        "environment": {
            "seawater_temperature_C": 10.0,      # temperate, 0-30 m
            "seawater_resistivity_ohm_m": 0.30,
        },
        "anode": {
            "type": "stand_off",
            "length_m": 1.5,
            "radius_m": 0.08,
            "material": "aluminium",
            "utilization_factor": 0.90,
            "individual_anode_mass_kg": 200.0,
        },
    }
}

r = CathodicProtection().router(cfg)["results"]
zone = r["current_demand_A"]["submerged"]
print("i_mean (A/m2):            {:.3f}".format(zone["i_mean_A_m2"]))
print("f_cm / f_cf:              {:.3f} / {:.3f}".format(zone["f_cm"], zone["f_cf"]))
print("I_mean / I_final (A):     {:.1f} / {:.1f}".format(
    r["current_demand_A"]["total_mean_A"], r["current_demand_A"]["total_final_A"]))
print("Anode mass (kg):          {:.0f}".format(r["anode_requirements"]["total_mass_kg"]))
print("Anode count:              {}".format(r["anode_requirements"]["anode_count"]))
print("Anode resistance (ohm):   {:.4f}".format(r["anode_resistance_ohm"]))
v = r["current_output_verification"]
print("Output per anode (A):     {:.3f}".format(v["anode_current_output_per_anode_A"]))
print("Total output (A):         {:.1f}  adequate={}".format(
    v["total_anode_current_output_A"], v["adequate"]))
```

---

## Notes

1. **Category III breakdown on this branch** reaches f_cf = 1.0 (bare steel) within the
   25-year life, which drives the 600 A final demand. The coating and current-density tables
   used here are those in `cp_DNV_RP_B401_2021.py`; #2207 replaces them with cited,
   edition-keyed tables and these numbers will change accordingly.
2. **Multi-zone models:** add further entries to `structure.zones` with distinct `zone` IDs and
   `base_zone` set to `submerged`, `splash` or `atmospheric` when one physical zone carries
   several coating categories.
3. **ABS comparison:** the same jacket under the ABS GN Offshore 2018 method is in
   `example-03-platform-abs-offshore-2018.md`; the two methods use different current-density
   and coating-breakdown tables and should not be mixed.
