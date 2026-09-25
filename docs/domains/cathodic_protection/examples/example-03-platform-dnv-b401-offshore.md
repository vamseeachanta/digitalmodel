# Fixed Offshore Jacket CP Design — DNV-RP-B401 (`DNV_RP_B401_offshore` route)

**Standard:** DNV-RP-B401 Cathodic Protection Design, 2021 edition tables (Tables 10-1 / 10-2 /
10-4 / 10-6 as held in `b401_tables.py`, #2207)
**Structure type:** Fixed jacket, submerged zone, temperate seawater, 0–30 m
**Anode type:** Aluminium stand-off anodes

---

## Standard and Scope

DNV-RP-B401 covers galvanic and impressed-current CP of offshore structures: fixed jackets,
gravity-based structures, subsea equipment. The `DNV_RP_B401_offshore` route implements the
Sec. 7 galvanic design loop for one or more zones:

- initial / mean / final design current densities for bare steel by climatic region and depth
  band (Tables 10-1 and 10-2); buried surfaces at 0.020 A/m² (Sec. 6.3)
- coating breakdown f_c(t) = a + b × t by paint category and depth band (Table 10-4), clamped
  at 1.0
- initial, mean and final current demand per zone, I = A × i × f_c
- anode mass M = I_mean × t_f × 8760 / (ε × u) and count N = ceil(M / m_a) (Table 10-6
  capacity, Table 10-8 utilisation)
- anode resistance by the Dwight formula (Table 10-7) and initial and final current-output
  checks; `governing_case` names the criterion that sets `recommended_anode_count`

The edition is selected with `design_data.edition` (`"2005"`, `"2010"`, `"2017"`, `"2021"`;
requires #2207 or later). The result carries `standard`, `edition`, `provenance` and the
`citations` of every table value used.

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

### Step 1: Current densities (Tables 10-1 / 10-2, temperate, 0–30 m)

```
i_initial = 0.200 A/m²
i_mean    = 0.100 A/m²
i_final   = 0.130 A/m²
```

### Step 2: Coating breakdown (Table 10-4, Category III, 0–30 m: a = 0.02, b = 0.012 per year)

```
f_ci = 0.02
f_cm = 0.02 + 0.012 × 25 / 2 = 0.17
f_cf = 0.02 + 0.012 × 25     = 0.32
```

### Step 3: Current demand

```
I_initial = 12,000 × 0.200 × 0.02 =  48.0 A
I_mean    = 12,000 × 0.100 × 0.17 = 204.0 A
I_final   = 12,000 × 0.130 × 0.32 = 499.2 A
```

### Step 4: Anode mass and count (ε = 2000 Ah/kg, u = 0.90, m_a = 200 kg)

```
M = 204.0 × 25 × 8760 / (2000 × 0.90) = 24,820 kg
N = ceil(24,820 / 200) = 125 anodes   (mass basis)
```

### Step 5: Anode resistance (Dwight, L = 1.5 m, r = 0.08 m, ρ = 0.30 Ω·m)

```
R_a = ρ / (2πL) × (ln(4L / r) − 1) = 0.30 / (2π × 1.5) × (ln(75) − 1) = 0.1056 Ω
```

### Step 6: Current-output checks (E_structure −0.80 V, E_anode −1.05 V)

```
ΔE       = 0.25 V
I_anode  = 0.25 / 0.1056 = 2.367 A
125 anodes: I_total = 125 × 2.367 = 295.9 A
   initial check: 295.9 A ≥  48.0 A  → pass   (count by initial current = 21)
   final check:   295.9 A <  499.2 A → fail   (count by final current  = 211)
governing_case = "final";  recommended_anode_count = max(125, 21, 211) = 211
```

The mass-based count is not adequate at end of life: the final current demand governs and
211 anodes (42,200 kg installed) are required.

---

## Results Summary (code output on this branch)

| Output | Value | Unit |
|--------|-------|------|
| Current density i_initial / i_mean / i_final | 0.200 / 0.100 / 0.130 | A/m² |
| Coating factor f_ci / f_cm / f_cf | 0.02 / 0.17 / 0.32 | — |
| Initial current demand I_initial | 48.0 | A |
| Mean current demand I_mean | 204.0 | A |
| Final current demand I_final | 499.2 | A |
| Total anode mass required (mass basis) | 24,820 | kg |
| Anode count (mass basis) | 125 | — |
| Anode resistance R_a | 0.1056 | Ω |
| Driving voltage ΔE | 0.25 | V |
| Current output per anode | 2.367 | A |
| Total output with 125 anodes | 295.9 | A |
| Count by initial / final current | 21 / 211 | — |
| Governing case | final | — |
| Recommended anode count | 211 | — |
| Provenance (2021 edition) | inherited-2011-unverified | — |

Citations returned: DNV-RP-B401 Table 10-1, Table 10-2, Table 10-4, Table 10-6 and Sec. 5
(protection potential criteria).

---

## Python Example

```python
from digitalmodel.infrastructure.base_solvers.hydrodynamics.cathodic_protection import CathodicProtection

cfg = {
    "inputs": {
        "calculation_type": "DNV_RP_B401_offshore",
        "design_data": {
            "design_life": 25.0,
            "edition": "2021",      # requires #2207 or later
        },
        "structure": {
            "zones": [
                # depth_m selects the Table 10-1 / 10-2 depth band (default 0 -> "0-30")
                {"zone": "submerged", "area_m2": 12000.0, "coating_category": "III", "depth_m": 20},
            ],
        },
        "environment": {
            "seawater_temperature_C": 10.0,      # temperate band
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
print("Standard / edition / provenance:", r["standard"], r["edition"], r["provenance"])
print("i initial/mean/final (A/m2): {:.3f} / {:.3f} / {:.3f}".format(
    zone["i_initial_A_m2"], zone["i_mean_A_m2"], zone["i_final_A_m2"]))
print("f_ci / f_cm / f_cf:          {:.3f} / {:.3f} / {:.3f}".format(
    zone["f_ci"], zone["f_cm"], zone["f_cf"]))
print("I initial/mean/final (A):    {:.1f} / {:.1f} / {:.1f}".format(
    r["current_demand_A"]["total_initial_A"], r["current_demand_A"]["total_mean_A"],
    r["current_demand_A"]["total_final_A"]))
print("Anode mass (kg):             {:.0f}".format(r["anode_requirements"]["total_mass_kg"]))
print("Anode count (mass basis):    {}".format(r["anode_requirements"]["anode_count"]))
print("Anode resistance (ohm):      {:.4f}".format(r["anode_resistance_ohm"]))
v = r["current_output_verification"]
print("Output per anode (A):        {:.3f}".format(v["anode_current_output_per_anode_A"]))
print("Governing case:              {}".format(v["governing_case"]))
print("Recommended anode count:     {}".format(v["recommended_anode_count"]))
print("Citations:", r["citations"])
```

---

## Notes

1. **Final current governs.** With Category III paint in the 0–30 m band the breakdown factor
   reaches 0.32 at 25 years, so the end-of-life demand (499.2 A) exceeds what the mass-based
   count can deliver; `recommended_anode_count` (211) is the number to carry forward.
2. **Provenance.** The 2017 and 2021 editions reuse the 2010/2011 table numbers
   (`inherited-2011-unverified`); the 2005 and 2010 editions are flagged `verified-2011-tables`.
3. **Multi-zone models:** add further entries to `structure.zones` with distinct `zone` IDs and
   `base_zone` set to `submerged`, `buried`, `splash` or `atmospheric` when one physical zone
   carries several coating categories; `depth_m` sets each zone's depth band.
4. **ABS comparison:** the same structure type under the ABS GN Offshore 2018 method is in
   `example-03-platform-abs-offshore-2018.md`; the two methods use different current-density
   and coating-breakdown tables and should not be mixed.
