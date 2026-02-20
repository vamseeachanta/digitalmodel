# Wall Thickness Implementations (Step-by-Step)

This document summarizes the implemented wall thickness calculations in code, organized by standard. Each section lists the steps, equations (LaTeX), and the code reference where the logic lives.

**Related references:**
- Latest-only extracts: `docs/domains/pipecapacity/WALL_THICKNESS_LATEST_ONLY.md`
- Latest-only LaTeX: `docs/domains/pipecapacity/WALL_THICKNESS_LATEST_ONLY_LATEX.md`
- Latest-to-older extracts: `docs/domains/pipecapacity/WALL_THICKNESS_LATEST_TO_OLDER.md`
- OCR inventory: `docs/domains/pipecapacity/WALL_THICKNESS_CODES_ONLY.md`

## Common Symbols

- `D`: nominal outer diameter
- `t`: wall thickness
- `D_i`: nominal inner diameter
- `P_i`: internal pressure
- `P_o`: external pressure
- `SMYS`: specified minimum yield strength
- `SMUS`: specified minimum ultimate tensile strength
- `E`: Young's modulus
- `nu`: Poisson's ratio
- `Fd`, `k`: design factors
- `F_p`: propagation factor
- `gamma_m`, `gamma_sc`: material and safety class factors
- `alpha_u`, `alpha_fab`: strength and fabrication factors
- `T_derate`: temperature derating factor

Code references use file paths and function names under `src/digitalmodel/modules/pipe_capacity/custom/PipeCapacity.py`.

---

## ASME B31.* (Modified Barlow)

**Code reference:** `src/digitalmodel/modules/pipe_capacity/custom/PipeCapacity.py`  
Functions: `evaluate_burst_pressure_modified_burlow_equation`, `evaluate_burst_minimum_thickness_modified_burlow_equation`

**Steps (burst pressure):**
1. Compute allowable hoop stress:
2. Select thin/thick wall formula based on `D/t` transition ratio.

**Equations:**
```tex
sigma_{allow} = SMYS \cdot F_d \cdot W \cdot T_{derate}
```
```tex
P = \frac{2 t \, \sigma_{allow}}{D} + P_o \quad \text{if } D/t \ge D/t_{transition}
```
```tex
P = \frac{2 t \, \sigma_{allow}}{D - t} + P_o \quad \text{if } D/t < D/t_{transition}
```

**Steps (minimum thickness):**
1. Compute `sigma_allow` as above.
2. Use thin/thick wall formula based on `D/t` transition ratio.

**Equations:**
```tex
t_{min} = \frac{(P_i - P_o) D}{2 \, \sigma_{allow}} \quad \text{if } D/t \ge D/t_{transition}
```
```tex
t_{min} = \frac{(P_i - P_o) D}{2 \, \sigma_{allow} + (P_i - P_o)} \quad \text{if } D/t < D/t_{transition}
```

**Variables (ASME B31.*):**

| Symbol | Meaning | Code Source |
| --- | --- | --- |
| `D` | Nominal OD | `cfg[pipe_flag]["Geometry"]["Nominal_OD"]` |
| `t` | Wall thickness | `thickness` argument |
| `P_i` | Internal pressure | `cfg["Design"][i]["InternalPressure"][pipe_flag]` |
| `P_o` | External pressure | `cfg["Design"][i]["ExternalPressure"][pipe_flag]` |
| `SMYS` | Yield strength | `cfg[pipe_flag]["Material"]["SMYS"]` |
| `F_d` | Design factor | `cfg["DesignFactors"][spec_code][load_condition]` |
| `W` | Weld factor | `cfg[pipe_flag]["Material"]["WeldFactor"]["Seamless"]` |
| `T_derate` | Temperature derating | `cfg["Design"][i]["Material"]["temperature_derating"][pipe_flag][spec_code]` |

---

## API STD 2RD-2013 (Internal Pressure, Collapse)

**Code reference:** `src/digitalmodel/modules/pipe_capacity/custom/PipeCapacity.py`  
Functions: `evaluate_burst_pressure_API_STD_2RD`, `evaluate_burst_minimum_thickness_API_STD_2RD`,  
`evaluate_collapse_pressure_API_STD_2RD`, `evaluate_collapse_minimum_thickness_API_STD_2RD`

**Steps (burst pressure):**
1. Compute burst pressure using log form.

**Equations:**
```tex
P = F_d \cdot k \cdot (SMYS + SMUS) \cdot \ln\left(\frac{D}{D - 2t}\right)
```

**Steps (minimum thickness):**
1. Compute factored strength ratio.
2. Solve for `t`.

**Equations:**
```tex
B = \frac{P_i - P_o}{F_d \cdot k \cdot (SMYS + SMUS)}
```
```tex
t_{min} = \frac{1}{2}\left(D - \frac{D}{e^B}\right)
```

**Steps (collapse pressure):**
1. Compute elastic and plastic collapse pressures.
2. Combine to characteristic collapse and apply `F_d`.

**Equations:**
```tex
P_y = 2 \, SMYS \cdot \frac{t}{D}, \quad
P_{el} = \frac{2 E (t/D)^3}{1 - nu^2}
```
```tex
P_c = \frac{P_y P_{el}}{\sqrt{P_y^2 + P_{el}^2}}, \quad
P_e = F_d \cdot P_c
```

**Note:** `evaluate_collapse_minimum_thickness_API_STD_2RD` is a placeholder (returns `100`).

**Variables (API STD 2RD-2013):**

| Symbol | Meaning | Code Source |
| --- | --- | --- |
| `D` | Nominal OD | `cfg[pipe_flag]["Geometry"]["Nominal_OD"]` |
| `t` | Wall thickness | `thickness` argument |
| `P_i` | Internal pressure | `cfg["Design"][i]["InternalPressure"][pipe_flag]` |
| `P_o` | External pressure | `cfg["Design"][i]["ExternalPressure"][pipe_flag]` |
| `SMYS` | Yield strength | `cfg[pipe_flag]["Material"]["SMYS"]` |
| `SMUS` | Tensile strength | `cfg[pipe_flag]["Material"]["SMUS"]` |
| `E` | Young's modulus | `cfg["Material"][material]["E"]` |
| `nu` | Poisson's ratio | `cfg["Material"][material]["Poissionsratio"]` |
| `F_d` | Design factor | `cfg["DesignFactors"][spec_code][load_condition]["Fd"]` |
| `k` | Burst factor | `cfg["DesignFactors"][spec_code][load_condition]["k"]["API 5L"]` |

---

## API RP 1111-2009 (Internal Pressure, Propagation)

**Code reference:** `src/digitalmodel/modules/pipe_capacity/custom/PipeCapacity.py`  
Functions: `evaluate_burst_pressure_API_RP_1111`, `evaluate_burst_minimum_thickness_API_RP_1111`,  
`evaluate_collapse_propagation_pressure_API_RP_1111`, `evaluate_collapse_propagation_minimum_thickness_API_RP_1111`

**Steps (burst pressure):**
1. Check `D/t` transition ratio.
2. Use log or thin-wall form accordingly.

**Equations:**
```tex
P = F_d \cdot 0.45 \cdot (SMYS + SMUS) \cdot \ln\left(\frac{D}{D - 2t}\right)
```
```tex
P = F_d \cdot 0.90 \cdot (SMYS + SMUS) \cdot \frac{t}{D - t}
```

**Steps (minimum thickness):**
1. Compute `B` using 0.45 or 0.90 factor.
2. Solve for `t`.

**Equations:**
```tex
B = \frac{P_i - P_o}{0.45 \cdot F_d \cdot (SMYS + SMUS)}
```
```tex
t_{min} = \frac{1}{2}\left(D - \frac{D}{e^B}\right)
```
```tex
B = \frac{P_i - P_o}{0.90 \cdot F_d \cdot (SMYS + SMUS)}
```
```tex
t_{min} = \frac{B D}{1 + B}
```

**Steps (propagation pressure):**
1. Compute propagation pressure `P_p`.
2. Apply factor `F_p`.

**Equations:**
```tex
P_p = 24 \cdot SMYS \cdot \left(\frac{t}{D}\right)^{2.4}
```
```tex
P = F_p \cdot P_p
```

**Steps (propagation minimum thickness):**
1. Convert external pressure to `P_p` using `F_p`.
2. Solve for `t`.

**Equations:**
```tex
P_p = \frac{P_o}{F_p}, \quad
t_{min} = D \left(\frac{P_p}{24 \cdot SMYS}\right)^{1/2.4}
```

**Variables (API RP 1111-2009):**

| Symbol | Meaning | Code Source |
| --- | --- | --- |
| `D` | Nominal OD | `cfg[pipe_flag]["Geometry"]["Nominal_OD"]` |
| `t` | Wall thickness | `thickness` argument |
| `P_i` | Internal pressure | `cfg["Design"][i]["InternalPressure"][pipe_flag]` |
| `P_o` | External pressure | `cfg["Design"][i]["ExternalPressure"][pipe_flag]` |
| `SMYS` | Yield strength | `cfg[pipe_flag]["Material"]["SMYS"]` |
| `SMUS` | Tensile strength | `cfg[pipe_flag]["Material"]["SMUS"]` |
| `F_d` | Design factor | `cfg["DesignFactors"][spec_code][load_condition]["Fd"]` |
| `F_p` | Propagation factor | `cfg["DesignFactors"][spec_code][load_condition]["Fp"]` |

---

## API RP 16Q-2017 (Von Mises Iteration)

**Code reference:** `src/digitalmodel/modules/pipe_capacity/custom/PipeCapacity.py`  
Classes: `API_RP_16Q`, `VonMises_Pipe`

**Steps (stress evaluation):**
1. Compute stresses:
   - Radial
   - Hoop
   - Axial (addition/subtraction)
2. Compute Von Mises stress and take the maximum case.

**Equations:**
```tex
sigma_r = -\frac{P_o D + P_i D_i}{D + D_i}
```
```tex
sigma_h = \frac{(P_i - P_o) D}{2 t} - P_i
```
```tex
sigma_a = \frac{T}{A} \pm \frac{M}{2 I} (D - t)
```
```tex
sigma_{vm} = \sqrt{\frac{1}{2}\max\left(
(\sigma_r-\sigma_h)^2 + (\sigma_h-\sigma_{a+})^2 + (\sigma_{a+}-\sigma_r)^2,
(\sigma_r-\sigma_h)^2 + (\sigma_h-\sigma_{a-})^2 + (\sigma_{a-}-\sigma_r)^2
\right)}
```

**Steps (burst minimum thickness):**
1. Sweep `t` over a range.
2. Interpolate where `sigma_vm = allowable_ratio * SMYS`.

**Steps (burst pressure):**
1. Sweep `P_i` over a range.
2. Interpolate where `sigma_vm = allowable_ratio * SMYS`.

**Variables (API RP 16Q-2017):**

| Symbol | Meaning | Code Source |
| --- | --- | --- |
| `D` | Nominal OD | `cfg[pipe_flag]["Geometry"]["Nominal_OD"]` |
| `D_i` | Nominal ID | `cfg[pipe_flag]["Geometry"]["Nominal_ID"]` |
| `t` | Wall thickness | `cfg[pipe_flag]["Geometry"]["Design_WT"]` |
| `P_i` | Internal pressure | `cfg["Design"][i]["InternalPressure"][pipe_flag]` |
| `P_o` | External pressure | `cfg["Design"][i]["ExternalPressure"][pipe_flag]` |
| `T` | Axial force (effective) | `loads["EffectiveTension"]` or `loads["AxialForce"]` |
| `M` | Bending moment | `loads["BendingMoment"]` |
| `A` | Section area | `section_properties["A"]` |
| `I` | Section inertia | `section_properties["I"]` |
| `SMYS` | Yield strength | `section_properties["SMYS"]` |
| `allowable_ratio` | Allowable to yield | `loads["allowable_stress_to_yield_ratio"]` |

---

## API TR 5C3-2018 (Collapse)

**Code reference:** `src/digitalmodel/modules/pipe_capacity/custom/PipeCapacity.py`  
Class: `API_TR_5C3`

**Steps (collapse pressure):**
1. Compute `D/t`.
2. Use the API TR 5C3 closed form.

**Equation:**
```tex
P_e = \frac{46.95 \times 10^6}{(D/t) (D/t - 1)^2}
```

**Steps (minimum thickness):**
1. Sample `D/t` ratios to build a curve of `P_e` vs `D/t`.
2. Interpolate to match `P_o`.
3. Convert `D/t` back to `t`.

**Variables (API TR 5C3-2018):**

| Symbol | Meaning | Code Source |
| --- | --- | --- |
| `D` | Nominal OD | `cfg[pipe_flag]["Geometry"]["Nominal_OD"]` |
| `t` | Wall thickness | `cfg[pipe_flag]["Geometry"]["Design_WT"]` |
| `P_o` | External pressure | `cfg["Design"][i]["ExternalPressure"][pipe_flag]` |

---

## 30 CFR Part 250 (Internal Pressure)

**Code reference:** `src/digitalmodel/modules/pipe_capacity/custom/PipeCapacity.py`  
Class: `CFR_30_Part_250`

**Steps:**
1. Compute allowable hoop stress.
2. Compute minimum thickness or maximum pressure.

**Equations:**
```tex
sigma_{allow} = SMYS \cdot F_d \cdot W \cdot T_{derate}
```
```tex
t_{min} = \frac{(P_i - P_o) D}{2 \, \sigma_{allow}}
```
```tex
P = \frac{2 t \, \sigma_{allow}}{D} + P_o
```

**Variables (30 CFR Part 250):**

| Symbol | Meaning | Code Source |
| --- | --- | --- |
| `D` | Nominal OD | `cfg[pipe_flag]["Geometry"]["Nominal_OD"]` |
| `t` | Wall thickness | `cfg[pipe_flag]["Geometry"]["Design_WT"]` |
| `P_i` | Internal pressure | `cfg["Design"][i]["InternalPressure"][pipe_flag]` |
| `P_o` | External pressure | `cfg["Design"][i]["ExternalPressure"][pipe_flag]` |
| `SMYS` | Yield strength | `cfg[pipe_flag]["Material"]["SMYS"]` |
| `F_d` | Design factor | `cfg["DesignFactors"][spec_code][load_condition]["Fd"]` |
| `W` | Weld factor | `cfg[pipe_flag]["Material"]["WeldFactor"]["Seamless"]` |
| `T_derate` | Temperature derating | `cfg["Design"][i]["Material"]["temperature_derating"][pipe_flag][spec_code]` |

---

## DNV F101 / F201 (Burst, Collapse, Propagation)

**Code reference:** `src/digitalmodel/modules/pipe_capacity/custom/PipeCapacity.py`  
Class: `DNVWallThickness`

**Steps (material strength):**
1. Derate strengths using `alpha_u` and temperature derating.
2. Compute containment strength.

**Equations:**
```tex
f_y = SMYS \cdot alpha_u \cdot T_{derate}
```
```tex
f_u = SMUS \cdot alpha_u \cdot T_{derate}
```
```tex
f_{cb} = \min(f_y, f_u / 1.15)
```

**Steps (burst pressure):**
1. Compute allowable containment pressure.
2. Add external pressure.

**Equations:**
```tex
P_{allow} = \frac{2}{\sqrt{3}} \cdot \frac{2 t}{D - t} \cdot \frac{f_{cb}}{gamma_m \, gamma_{sc}}
```
```tex
P = P_{allow} + P_o
```

**Steps (minimum thickness):**
1. Compute `P_i - P_o`.
2. Solve for `t`.

**Equation:**
```tex
t_{min} = \frac{(P_i - P_o) D}{(P_i - P_o) + 2 \cdot \frac{2}{\sqrt{3}} \cdot \frac{f_{cb}}{gamma_m \, gamma_{sc}}}
```

**Steps (collapse pressure):**
1. Compute elastic and plastic collapse pressures.
2. Solve cubic for `P_c`.
3. Apply safety factors and add minimum internal pressure.

**Equations:**
```tex
P_{el} = \frac{2 E (t/D)^3}{1 - nu^2}
```
```tex
P_p = 2 f_y \, alpha_{fab} \cdot \frac{t}{D}
```
```tex
P = \frac{P_c}{gamma_m \, gamma_{sc}} + P_{i,min}
```

**Steps (propagation):**
1. Compute propagation pressure.
2. Apply safety factors and add minimum internal pressure.

**Equations:**
```tex
P_{prop} = 35 \cdot f_y \cdot alpha_{fab} \cdot \left(\frac{t}{D}\right)^{2.5}
```
```tex
P = \frac{P_{prop}}{gamma_m \, gamma_{sc}} + P_{i,min}
```
```tex
t_{min} = D \left(\frac{(P_o - P_{i,min}) \, gamma_m \, gamma_{sc}}{35 \cdot f_y \cdot alpha_{fab}}\right)^{1/2.5}
```

**Variables (DNV F101/F201):**

| Symbol | Meaning | Code Source |
| --- | --- | --- |
| `D` | Nominal OD | `cfg[pipe_flag]["Geometry"]["Nominal_OD"]` |
| `t` | Wall thickness | `cfg[pipe_flag]["Geometry"]["Design_WT"]` |
| `P_i` | Internal pressure | `cfg["Design"][i]["InternalPressure"][pipe_flag]` |
| `P_o` | External pressure | `cfg["Design"][i]["ExternalPressure"][pipe_flag]` |
| `P_{i,min}` | Minimum internal pressure | `cfg["Design"][i]["MinimumInternalPressure"]` |
| `SMYS` | Yield strength | `cfg[pipe_flag]["Material"]["SMYS"]` |
| `SMUS` | Tensile strength | `cfg[pipe_flag]["Material"]["SMUS"]` |
| `E` | Young's modulus | `cfg[pipe_flag]["Material"]["E"]` |
| `nu` | Poisson's ratio | `cfg[pipe_flag]["Material"]["Poissionsratio"]` |
| `gamma_m` | Material factor | `DesignFactors` (`internal_pressure`/`external_pressure`) |
| `gamma_{sc}` | Safety class factor | `DesignFactors` (`internal_pressure`/`external_pressure`) |
| `alpha_u` | Strength factor | `DesignFactors` (`internal_pressure`) |
| `alpha_{fab}` | Fabrication factor | `DesignFactors` (`external_pressure`/`collapse_propagation`) |
