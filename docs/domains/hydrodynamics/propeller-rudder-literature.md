# Propeller-Rudder Hydrodynamic Interaction — Literature Survey

> **WRK-1148** | Parent: WRK-1147 | Created: 2026-03-12
> Structured reference for WRK-1149 (method selection).

---

## Parameter Definitions (canonical cross-source table)

| Symbol | Definition | Units | Notes |
|--------|-----------|-------|-------|
| `J` | Advance ratio = V_A / (n·D) | — | V_A = advance speed; n = rev/s; D = diameter |
| `K_T` | Thrust coefficient = T / (ρ·n²·D⁴) | — | Non-dimensional thrust |
| `K_Q` | Torque coefficient = Q / (ρ·n²·D⁵) | — | Non-dimensional torque |
| `η₀` | Open-water efficiency = J·K_T / (2π·K_Q) | — | Propeller efficiency in undisturbed flow |
| `t` | Thrust deduction fraction | — | Accounts for hull suction behind propeller; typical 0.0–0.2 |
| `w` | Wake fraction | — | Velocity deficit at propeller plane; typical 0.0–0.4 |
| `C_T` | Thrust loading coefficient = T / (½ρV²A_disc) | — | Also written `C_th`; used in slipstream velocity calc |
| `V_A` | Advance velocity at propeller plane | m/s | `V_A = V_s(1 - w)` |
| `V_s` | Ship speed | m/s | — |
| `Va` | Axial slipstream velocity downstream of disc | m/s | `Va = V_A(1 + a)` |
| `Vt` | Tangential slipstream velocity at rudder plane | m/s | From propeller rotation |
| `a` | Axial induction factor | — | `a = (√(1+C_T) - 1)/2` (actuator-disk) |
| `C_L` | Rudder lift coefficient | — | Lift force / (½ρV²A_rudder) |
| `α` | Rudder geometric angle of attack | deg | Drift + rudder deflection angle |
| `δ` | Rudder deflection angle | deg | Commanded helm angle |
| `A_R` | Rudder aspect ratio = span²/area | — | Affects lift slope |
| `C^(rp)` | Rudder-propeller interaction coefficient | — | Range 0.0–1.0; scales propeller-induced rudder forces |
| `D` | Propeller diameter | m | — |
| `n` | Propeller rotational speed | rev/s (rps) | — |
| `ρ` | Water density | kg/m³ | ~1025 kg/m³ seawater |

---

## 1. Actuator-Disk / Momentum Theory

**Authors:** Rankine (1865), Froude (1889); textbook treatments in Carlton (2007), Breslin & Andersen (1994)
**Year:** Classical; standard reference formulation

### Key Contribution

Actuator-disk theory (Rankine-Froude momentum theory) treats the propeller as a thin disk
across which pressure jumps discontinuously. It provides closed-form expressions for the
slipstream velocity field downstream — the primary input to rudder force models.

### Key Formulae

**Axial induction factor:**
```
a = (sqrt(1 + C_T) - 1) / 2
```

**Axial velocity immediately behind the disc (Va at disc):**
```
Va_disc = V_A * (1 + a)
```

**Far-field axial velocity in fully-contracted slipstream:**
```
Va_ff = V_A * (1 + 2*a)
```

**Thrust loading coefficient:**
```
C_T = T / (0.5 * rho * V_A^2 * pi/4 * D^2)
```

**Relation to thrust coefficient K_T:**
```
C_T = 8 * K_T / (pi * J^2)
```

**Axial velocity at rudder plane** (located x/R behind disc, partially contracted):
```
Va_R = V_A * [1 + a * (1 + x / sqrt(x^2 + R^2))]
```
where x is axial distance from disc centre to rudder mid-span, R = D/2.

**Tangential velocity at rudder plane** (rotation-induced, simplified):
```
Vt_R ≈ a' * omega * r
```
where `a'` is tangential induction factor and `omega * r` is local blade speed at radius `r`.

### Applicability Range

- Applies to lightly-loaded to moderately-loaded propellers: `C_T < 2.5` (J > 0.3 approx.)
- Breaks down at high loading (bollard pull, J → 0) where slipstream contraction ratio diverges
- Ship type: all single-screw ships
- Provides the upstream boundary condition for rudder-in-slipstream models

---

## 2. Holtrop & Mennen (1982, 1984)

**Authors:** J. Holtrop, G.G.J. Mennen
**Year:** 1982 (Int. Shipbuild. Prog. 29), 1984 (Int. Shipbuild. Prog. 31)
**Key reference numbers in McTaggart (2005):** [13, 14]

### Key Contribution

ITTC-widely-used semi-empirical regression method for ship resistance and propulsive
coefficients based on statistical analysis of model test data from a large ship database.
Provides design-level estimates of wake fraction `w`, thrust deduction `t`, and relative
rotative efficiency `η_R` from hull form parameters.

### Key Formulae

**Wake fraction for single-screw ships (Holtrop 1984):**
```
w = C_9 * C_20 * (L_wl/T_A) * (0.050776 + 0.93405*C_11*(C_VD/(1-C_VP)))
    + 0.27915*C_20*sqrt(B/(L_wl*(1-C_VP))) + C_9*C_20
```
where `C_9`, `C_11`, `C_20` are hull-form correction factors, `C_VP` is vertical prismatic
coefficient, `C_VD` is vertical distribution factor.

**Simplified wake fraction (design estimate):**
```
w ≈ 0.3095*C_B + 10*C_V*C_B - 0.23*D/sqrt(B*T)
```
where `C_V` is viscous resistance coefficient, `C_B` is block coefficient.

**Thrust deduction fraction:**
```
t = 0.25014*(B/L)^0.28956 * (sqrt(B*T)/D)^0.2624 / (1 - C_P + 0.0225*lcb)^0.01762
    + 0.0015*C_stern
```

**Relative rotative efficiency:**
```
η_R = 0.9922 - 0.05908*A_E/A_0 + 0.07424*(C_P - 0.0225*lcb)
```

### Applicability Range

- J range: 0.3 to 1.1 (design service conditions)
- Froude number: 0.10–0.45
- Ship types: tankers, bulk carriers, containerships, ferries, naval vessels
- Block coefficient: 0.55 ≤ C_B ≤ 0.85
- Accuracy: ±5% on t and w for ships within the regression database
- Not applicable: high-speed planing hulls, sailing yachts, unconventional sterns

---

## 3. Söding (1998) / Brix (1993)

**Authors:** H. Söding (1998 formulation); J. Brix, "Manoeuvring Technical Manual," Seehafen Verlag, Hamburg, 1993
**Referenced as:** Söding [18] in McTaggart (2005); Brix (1993) primary source

### Key Contribution

Söding developed practical closed-form expressions for the additional rudder forces
arising from the propeller slipstream. The approach augments standard rudder lift/drag
with propeller-induced velocity terms, introducing the interaction coefficient `C^(rp)`.
This is the method implemented in DRDC ShipMo3D and is widely used in maneuvering
simulation programs.

### Key Formulae (as extracted from McTaggart 2005, Section 9)

**Propeller thrust force:**
```
F_prop = (1 - t) * rho * n^2 * D^4 * K_T(J)
```

**Advance ratio at propeller:**
```
J = U * (1 - w) / (n * D)
```

**Thrust loading coefficient:**
```
C_th = F_prop / (0.5 * rho * U^2 * (1 - w)^2 * pi/4 * D^2)
```

**Rudder lift augmented by propeller slipstream:**
```
F_lift^(rp) = C^(rp) * F_prop * (1 + 1/sqrt(1 + C_th)) * sin(delta)
```

**Rudder drag augmented by propeller slipstream:**
```
F_drag^(rp) = -C^(rp) * F_prop * (1 + 1/sqrt(1 + C_th)) * (1 - cos(delta))
```

where `C^(rp)` is the rudder-propeller interaction coefficient.

**Rudder-propeller interaction forces in ship-fixed axes:**
```
F_1^(rp,S) = F_drag^(rp)                          (surge)
F_2^(rp,S) = -F_lift^(rp) * sin(Gamma)            (sway)
F_3^(rp,S) =  F_lift^(rp) * cos(Gamma)            (heave)
```
where `Gamma` is rudder dihedral angle.

**Interaction coefficient selection:**
```
C^(rp) ≈ 1.0   if rudder is fully within propeller slipstream (rudder span ≤ 2*D)
C^(rp) ≈ 0.0   if rudder is clearly outside slipstream
C^(rp) ≈ 0.9   for typical single-screw tanker (Esso Osaka: L=325m, C_B=0.831)
```

### Applicability Range

- J range: 0.0 to 1.1 (full maneuvering envelope including low-speed turning)
- Ship type: single-screw ships; twin-screw requires careful coefficient selection
- Effect becomes negligible when rudder span > 2·D
- Validated against turning circle data for Esso Osaka tanker (full-scale trials)
- Uncertainty in `C^(rp)` is the principal source of error in maneuvering prediction

---

## 4. Molland & Turnock (2007)

**Authors:** A.F. Molland, S.R. Turnock
**Title:** "Marine Rudders and Control Surfaces: Principles, Data, Design and Applications"
**Publisher:** Butterworth-Heinemann, Oxford, 2007

### Key Contribution

Comprehensive treatment of rudder hydrodynamics including systematic wind tunnel and
towing tank data for rudders operating in propeller slipstreams. Provides tabulated
C_L versus angle-of-attack data for a range of rudder geometries, aspect ratios, and
propeller loadings. First systematic quantification of rudder lift augmentation in
propeller wake.

### Key Formulae

**Lift coefficient in free stream:**
```
C_L = (dC_L/dalpha) * alpha
dC_L/dalpha ≈ 2*pi*A_R / (A_R + 2)   (thin-airfoil + aspect ratio correction)
```

**Lift augmentation ratio in propeller slipstream:**
```
C_L^(slipstream) / C_L^(freestream) ≈ (Va_R / V_s)^2
```
where Va_R is the effective axial velocity at the rudder plane.

**Effective inflow angle to rudder in slipstream:**
```
alpha_eff = atan(V_T / Va_R)
```
where `V_T` is transverse component of ship velocity at rudder.

**Effective rudder speed in propeller wake:**
```
V_eff = sqrt(Va_R^2 + Vt_R^2)
```

**Rudder force scaling by propeller loading (Molland & Turnock data):**
```
C_L = f(alpha, Va_R/V_s, AR, rudder_type)
```
Tabulated for NACA 0012/0015/0020 sections, AR = 1.0–3.0, J = 0.2–1.1.

### Applicability Range

- J range: 0.2 to 1.1 (tested in cavitation tunnel and towing tank)
- Rudder aspect ratio: 1.0–3.0
- Ship types: single-screw merchant ships and naval vessels
- Validated against: experimental data from Southampton towing tank
- Key finding: C_L can be 2–4× higher in propeller slipstream vs free stream at same angle

---

## 5. McTaggart — ShipMo3D Maneuvering Library

**Authors:** Kevin McTaggart
**Title:** "Simulation of Hydrodynamic Forces and Motions for a Freely Maneuvering Ship in a Seaway"
**Report:** DRDC Atlantic TM 2005-071, Defence R&D Canada — Atlantic, December 2005
**Repository file:** `digitalmodel/docs/domains/ship-design/maneuvering_ship.pdf`

### Key Contribution

Extends the DRDC ShipMo3D seakeeping library to freely maneuvering ships in calm water
and in waves. Introduces propeller thrust, rudder deflection, and rudder-propeller
interaction force components. Implements Söding's slipstream model. Validated against
full-scale turning circle trials for the Esso Osaka tanker.

### Key Formulae (from Sections 7–9)

**Propeller thrust (Eq. 59):**
```
F_prop = (1 - t_prop) * rho * n_prop^2 * D_prop^4 * K_T(J_prop)
```

**Advance ratio (Eq. 60):**
```
J_prop = U * (1 - w_prop) / (n_prop * D_prop)
```

**K_T polynomial for Esso Osaka (Eq. 79, scaled from van Manen & van Oossanen):**
```
K_T = 0.394 - 0.197*J - 0.148*J^2
```

**Rudder-propeller interaction lift (Eq. 66):**
```
F_lift^(rp) = C^(rp) * F_prop * (1 + 1/sqrt(1 + C_th)) * sin(delta_rudder)
```

**Thrust loading coefficient (Eq. 68):**
```
C_th = F_prop / (0.5 * rho * U^2 * (1 - w_prop)^2 * pi/4 * D_prop^2)
```

### Validation Data (Esso Osaka)

| Parameter | Value |
|-----------|-------|
| Ship length L | 325 m |
| Block coefficient C_B | 0.831 |
| Propeller diameter | 9.1 m |
| Wake fraction w_prop | 0.352 |
| Thrust deduction t_prop | 0.20 |
| Rudder-propeller coefficient C^(rp) | 0.9 |
| Max rudder deflection | 35° |

### Applicability Range

- J range: 0.0–1.1 (full maneuvering range)
- Applies to: single-screw ships with rudder directly aft of propeller
- Typical parameter ranges: `0.0 ≤ t ≤ 0.2`, `0.0 ≤ w ≤ 0.4`
- Turning circle predictions agree within ~10% of Esso Osaka sea trials
- Hull maneuvering coefficients from Inoue et al. regression (intended for rectangular
  lateral profiles; use caution for frigates with significant draft variation)

---

## 6. Propeller-Hull Interaction PDF (repo)

**Authors:** Reza Mehdipour
**Title:** "Simulating propeller and Propeller-Hull Interaction in OpenFOAM"
**Organization:** KTH Royal Institute of Technology / Chalmers University, 2013
**Repository file:** `digitalmodel/docs/domains/openfoam/naval_architecture/propeller_hull_interaction.pdf`

### Key Contribution

RANS validation study comparing MRF (steady-state) and AMI (sliding mesh, transient)
techniques in OpenFOAM for a 4-bladed fixed-pitch propeller in open water and
self-propulsion conditions. Provides numerical K_T and K_Q values for J = 0.5–1.0.
Self-propulsion test includes rudder geometry (Figure 4.3, Table 4.7).

### Key Formulae (from Section 2.10)

**Open-water coefficients:**
```
K_T = T / (rho * n^2 * D^4)
K_Q = Q / (rho * n^2 * D^5)
eta_0 = J * K_T / (2*pi * K_Q) = P_T / P_D
```

**STREAMLINE reference ship (7000 DWT tanker, full scale):**

| Parameter | Value |
|-----------|-------|
| L_PP | 94.0 m |
| Beam B | 15.4 m |
| Draft T | 6.005 m |
| Block coefficient C_B | 0.762 |
| Propeller diameter D_P | 3.850 m |
| Number of blades | 4 (fixed pitch) |
| P/D at 0.7R | 1.0 |
| EAR | 0.58 |
| Design speed | 14 knots (J = 0.629) |

**Self-propulsion coefficients at 9–16 knots (Table 4.3):**

| V_s [kn] | t | w | η_H | J | η₀ |
|----------|---|---|-----|---|-----|
| 9  | 0.186 | 0.257 | 1.095 | 0.693 | 0.649 |
| 12 | 0.200 | 0.257 | 1.077 | 0.677 | 0.640 |
| 14 | 0.195 | 0.261 | 1.090 | 0.629 | 0.611 |
| 16 | 0.194 | 0.261 | 1.090 | 0.588 | 0.584 |

**Experimental open-water values at J = 0.629:**
```
K_T (exp) = 0.246
10*K_Q (exp) = 0.420
eta_0 (exp) = 0.600
```

**MRF numerical vs experimental (k-ε, Table 5.2):**
```
K_T (MRF) = 0.267   (ΔK_T = 8.5%)
10*K_Q (MRF) = 0.481  (ΔK_Q = 14.5%)
eta_0 (MRF) = 0.555  (Δη = 7.5%)
```

**SST k-ω gives best efficiency prediction** (Δη = 1.0% at J = 0.629).

### Applicability Range

- J range validated: 0.5 to 1.0 (open water), 0.629 (self-propulsion)
- Propeller type: 4-bladed fixed-pitch right-hand screw
- Ship speed: 9–16 knots full scale
- RANS turbulence models: k-ε, RNG k-ε, SST k-ω (SST best for efficiency)
- MRF: suitable for time-averaged propulsive coefficients (faster, ~15% less time than AMI)
- AMI: required for unsteady blade-pass loading and hull pressure fluctuations

---

## 7. ITTC Procedures

**Organization:** International Towing Tank Conference
**Key procedures:**
- ITTC 7.5-02-03-02.1 (2014): "Testing and Extrapolation Methods — Propulsion — Open Water Test"
- ITTC 7.5-02-06-01 (2017): "Guidelines for Rudder and Nozzle Interaction with Propeller"
- ITTC 7.5-02-06-02 (2017): "Propulsion/Seakeeping/Manoeuvring Interaction"

### Key Contribution

Standardises the non-dimensional coefficient system (K_T, K_Q, J) and extrapolation
procedures from model scale to full scale. Defines correlation allowances for wake
fraction and thrust deduction. Provides standard test procedures that underpin all
experimental data cited in other sources.

### Key Formulae

**Open-water test standardisation (per ITTC 7.5-02-03-02.1):**
```
J = V_A / (n * D)              (advance ratio, tested at constant n, varying V_A)
K_T = T / (rho * n^2 * D^4)   (thrust coefficient)
K_Q = Q / (rho * n^2 * D^5)   (torque coefficient)
```

**Scale correction for wake fraction (ITTC 1978 method, simplified form):**

The ITTC 1978 performance prediction method prescribes a wake-fraction correction
from model scale (M) to full scale (S). The exact procedure involves form-factor
decomposition of the resistance and is defined in ITTC Procedure 7.5-02-03-01.4
(§3.3). The core principle is:
```
w_S = (t + 0.04) + (w_M - t - 0.04) * (1 + k) * C_F_S / ((1 + k) * C_F_M)
```
where:
- `w_M` = model-scale wake fraction (from self-propulsion test)
- `t` = thrust deduction fraction
- `k` = form factor (from Prohaska or ITTC resistance test)
- `C_F_M`, `C_F_S` = frictional resistance coefficients at model and ship Re (ITTC 1957 line)

> **Note:** Multiple variants exist in the literature. The above follows the ITTC 1978
> method as reproduced in Carlton (2007, §8.11). For exact implementation, consult the
> primary ITTC procedure document rather than secondary sources.

**Thrust deduction relation to resistance:**
```
t = 1 - R_T / T    (from self-propulsion test; tow force = 0 at model self-propulsion point)
```

**Hull efficiency:**
```
η_H = (1 - t) / (1 - w)
```

**Overall propulsive efficiency:**
```
η_D = η_0 * η_H * η_R
```

### Applicability Range

- J range: First quadrant (J = 0 to J at zero thrust), i.e., forward motion
- All ship types: single-screw, twin-screw, azimuthing, CPP
- Required for full-scale extrapolation of all towing tank data
- Defines the K_T and J parameters used consistently across all sources in this survey

---

## 8. RANS/CFD Validation Studies (2010–2024)

### 8.1 Felli, Camussi & Di Felice (2011)

**Authors:** M. Felli, R. Camussi, F. Di Felice
**Title:** "Mechanisms of evolution of the propeller wake in the transition and far fields"
**Journal:** Journal of Fluid Mechanics, vol. 682, pp. 5–53
**Key contribution:** PIV measurements of propeller slipstream structure at J = 0.4–0.9.
Shows helical vortex instability and slipstream contraction.

**Key formulae / correlations (from PIV data):**
```
Slipstream contraction ratio at 1D behind disc: r_wake / r_prop ≈ 0.87-0.92
Fully contracted at ~3D downstream
Tip vortex pitch / D ≈ J * (1 - w) at design condition
```

**Applicability range:** J = 0.4–0.9; single propeller in open water; lightly-loaded
conditions (J > 0.8) contract more slowly. Data applicable for calibrating actuator-disk
contraction assumptions.

### 8.2 Shen, Gu & Wan (2015)

**Authors:** Z. Shen, M. Gu, D. Wan
**Title:** "Numerical prediction of propeller-rudder interaction"
**Journal:** Ocean Engineering, vol. 108, pp. 392–407
**Key contribution:** RANS simulation of propeller-rudder system; validates rudder C_L
augmentation against experimental data. SST k-ω turbulence model gives best agreement
for rudder-in-slipstream configurations.

**Key formulae (reported):**
```
C_L augmentation: +40 to +80% vs isolated rudder, depending on J and delta
Peak C_L shifts to lower AoA in slipstream (effective inflow angle increased by Vt_R)
```

**Applicability:** J = 0.4–0.9; rudder δ = 0°–30°; single-screw configuration.

### 8.3 Greco, Muscari, Testa & Di Mascio (2014)

**Authors:** L. Greco, R. Muscari, C. Testa, A. Di Mascio
**Title:** "Studies on propeller-rudder interaction by an unsteady vortex lattice method"
**Journal:** Journal of Marine Science and Technology, vol. 19, pp. 366–379
**Key contribution:** BEM-based method for unsteady propeller-rudder forces. Captures
blade-frequency excitation on rudder.

**Key formulae / correlations:**
```
Unsteady rudder force amplitude / steady force ≈ 0.05-0.15 at design J
Dominant frequency: f = Z * n   (Z = blade count, n = rev/s)
Unsteady side force on rudder: F_y(t) = F_y_mean + ΔF_y * sin(2π * Z * n * t + φ)
```

**Applicability range:** J = 0.4–0.8; single-screw with semi-balanced rudder; relevant
for structural fatigue of rudder stock at high propeller loading. BEM method is inviscid —
does not capture viscous separation at high δ.

### 8.4 Muscari, Di Mascio & Verzicco (2013)

**Authors:** R. Muscari, A. Di Mascio, R. Verzicco
**Title:** "Modeling of vortex dynamics in the wake of a marine propeller"
**Journal:** Computers & Fluids, vol. 73, pp. 65–79
**Key contribution:** LES/DES study of propeller wake topology. Provides benchmark for
RANS models.

**Key formulae / correlations:**
```
Axial velocity deficit at x/D = 1:
  k-ε:     Va / V_A ≈ 1.25  (over-predicts acceleration)
  SST k-ω: Va / V_A ≈ 1.18  (matches LES within 5%)
  LES:     Va / V_A ≈ 1.16  (reference)
```

**Applicability range:** J = 0.5–0.8; single 4-bladed propeller in open water.
Primary value: establishes SST k-ω as the preferred RANS turbulence model for
slipstream prediction.

### 8.5 Yilmaz, Aktas, Can, Yazir & Atlar (2020)

**Authors:** N. Yilmaz, B. Aktas, U. Can, D. Yazir, M. Atlar
**Title:** "A comparative study on tip vortex and propeller performance"
**Journal:** Applied Ocean Research, vol. 98
**Key contribution:** Experimental + RANS comparison for propeller at J = 0.3–0.9.
Demonstrates SST k-ω superiority over standard k-ε for slipstream prediction.

**Key formulae / correlations:**
```
Va_R / V_A at rudder plane (x = 0.5D behind disc):
  J = 0.5: Va_R / V_A ≈ 1.35  (high loading)
  J = 0.7: Va_R / V_A ≈ 1.20
  J = 0.9: Va_R / V_A ≈ 1.10  (light loading)
K_T error: SST k-ω ≈ 3%, k-ε ≈ 8% (vs experiment)
```

**Applicability range:** J = 0.3–0.9; 4-bladed fixed-pitch propeller; experimental
data from cavitation tunnel + open water. Confirms SST k-ω for RANS propeller work.

### 8.6 RANS applicability summary

| Method | J range | Ship type | Recommended turbulence model | Cost vs actuator-disk |
|--------|---------|-----------|------------------------------|-----------------------|
| RANS-MRF | 0.3–1.1 | All | SST k-ω | 100× |
| RANS-AMI | 0.3–1.1 | All | SST k-ω | 500× |
| BEM | 0.3–1.1 | All | — (inviscid) | 10× |
| Actuator-disk | 0.0–1.1 | All | — (analytical) | 1× |
| Söding model | 0.0–1.1 | Single-screw | — (semi-empirical) | 1× |

---

## Applicability Summary Table

| Method | J range | Ship type | Accuracy | Data required | Rudder forces? |
|--------|---------|-----------|----------|---------------|----------------|
| Actuator-disk (Rankine-Froude) | 0.3–1.1 | All | ±10% on Va | None (analytic) | Indirect via Va |
| Holtrop & Mennen 1982/84 | 0.3–1.1 | Conventional hulls, C_B 0.55–0.85 | ±5% on t, w | Hull form | No (resistance/propulsion only) |
| Söding / ShipMo3D | 0.0–1.1 | Single-screw | ±10–20% | K_T curve, t, w | Yes — via C^(rp) |
| Molland & Turnock | 0.2–1.1 | Single-screw | ±5–10% | Rudder geometry, Va | Yes — systematic data |
| ITTC procedures | 0.0–1.1 | All | Reference standard | Model test data | Indirect |
| RANS-MRF (steady) | 0.3–1.1 | All | ±5–15% | CAD geometry, mesh | Yes (time-averaged) |
| RANS-AMI (transient) | 0.3–1.1 | All | ±3–10% | CAD geometry, mesh | Yes (unsteady) |

**Recommended for WRK-1149 method selection:**
- Semi-empirical maneuvering simulation → Söding model (Sections 3, 5)
- Design-phase propulsive coefficients → Holtrop & Mennen (Section 2)
- CFD validation → RANS-SST k-ω with AMI (Sections 6, 8)
- Slipstream input to rudder → Actuator-disk Va/Vt formulae (Section 1)

---

## References

1. **Rankine, W.J.M. (1865).** "On the mechanical principles of the action of propellers."
   Trans. Inst. Naval Architects, vol. 6.

2. **Froude, R.E. (1889).** "On the part played in propulsion by differences of fluid pressure."
   Trans. Inst. Naval Architects, vol. 30.

3. **Holtrop, J. & Mennen, G.G.J. (1982).** "An approximate power prediction method."
   International Shipbuilding Progress, vol. 29, pp. 166–170.

4. **Holtrop, J. (1984).** "A statistical re-analysis of resistance and propulsion data."
   International Shipbuilding Progress, vol. 31, pp. 272–276.

5. **Brix, J. (1993).** *Manoeuvring Technical Manual.* Seehafen Verlag, Hamburg.

6. **Söding, H. (1998).** "Limits of potential theory in rudder flow predictions."
   Ship Technology Research, vol. 45, pp. 141–155.

7. **Molland, A.F. & Turnock, S.R. (2007).** *Marine Rudders and Control Surfaces:
   Principles, Data, Design and Applications.* Butterworth-Heinemann, Oxford.

8. **Carlton, J.S. (2007).** *Marine Propellers and Propulsion.* 2nd ed.
   Butterworth-Heinemann, Oxford.

9. **Breslin, J.P. & Andersen, P. (1994).** *Hydrodynamics of Ship Propellers.*
   Cambridge University Press.

10. **McTaggart, K. (2005).** "Simulation of Hydrodynamic Forces and Motions for a
    Freely Maneuvering Ship in a Seaway." DRDC Atlantic TM 2005-071, Defence R&D
    Canada — Atlantic. *(Repo: digitalmodel/docs/domains/ship-design/maneuvering_ship.pdf)*

11. **Mehdipour, R. (2013).** "Simulating propeller and Propeller-Hull Interaction in
    OpenFOAM." MSc thesis, KTH / Chalmers University of Technology.
    *(Repo: digitalmodel/docs/domains/openfoam/naval_architecture/propeller_hull_interaction.pdf)*

12. **Felli, M., Camussi, R. & Di Felice, F. (2011).** "Mechanisms of evolution of the
    propeller wake in the transition and far fields." Journal of Fluid Mechanics, vol. 682,
    pp. 5–53.

13. **Greco, L., Muscari, R., Testa, C. & Di Mascio, A. (2014).** "Studies on
    propeller-rudder interaction by an unsteady vortex lattice method." Journal of Marine
    Science and Technology, vol. 19, pp. 366–379.

14. **Muscari, R., Di Mascio, A. & Verzicco, R. (2013).** "Modeling of vortex dynamics in
    the wake of a marine propeller." Computers & Fluids, vol. 73, pp. 65–79.

15. **Shen, Z., Gu, M. & Wan, D. (2015).** "Numerical prediction of propeller-rudder
    interaction." Ocean Engineering, vol. 108, pp. 392–407.

16. **Yilmaz, N., Aktas, B., Can, U., Yazir, D. & Atlar, M. (2020).** "A comparative study
    on tip vortex and propeller performance characteristics in the case of two
    propellers." Applied Ocean Research, vol. 98.

17. **ITTC (2014).** Procedure 7.5-02-03-02.1: "Testing and Extrapolation Methods —
    Propulsion — Open Water Test." 27th ITTC.

18. **ITTC (2017).** Procedure 7.5-02-06-01: "Guidelines for Rudder and Nozzle
    Interaction with Propeller." 28th ITTC.

19. **ITTC (1978/2017).** Procedure 7.5-02-03-01.4: "1978 ITTC Performance Prediction
    Method." Revised 28th ITTC 2017. Defines wake-fraction and thrust-deduction
    scale-correction from model to full scale.
