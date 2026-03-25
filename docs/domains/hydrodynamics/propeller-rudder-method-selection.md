# Propeller-Rudder Interaction — Method Assessment and Selection

> **WRK-1149** | Parent: WRK-1147 | Input: WRK-1148 (literature survey)
> Selects the primary calculation method for a fast-running Python model.

---

## 1. Evaluation Criteria

| # | Criterion | Weight | Description |
|---|-----------|--------|-------------|
| 1 | **Accuracy** | High | Agreement with experimental / RANS data across J range |
| 2 | **Input requirements** | Medium | Ship/propeller/rudder parameters needed |
| 3 | **RPM / J range** | High | Valid from n=0 (engine-off) through full drive-off |
| 4 | **Computational cost** | High | Must run in seconds, not hours — Python loop-friendly |
| 5 | **Implementation complexity** | Medium | Codeable without proprietary tools or licensed datasets |

---

## 2. Candidate Methods

### 2.1 Actuator-Disk + Rankine-Froude (AD)

Treats the propeller as a thin disk with a pressure jump. Provides closed-form
slipstream velocity at the rudder plane.

**Key equations:**
- Axial induction: `a = (sqrt(1 + C_T) - 1) / 2`
- Velocity at rudder: `Va_R = V_A * [1 + a * (1 + x / sqrt(x^2 + R^2))]`

**Strengths:** Analytic, zero external data, runs in microseconds.
**Weaknesses:** No direct rudder force output — must be paired with a rudder model.
Valid J = 0.3–1.1; overpredicts slipstream velocity at high loading (C_T > 2.5, J < 0.3).

### 2.2 Holtrop & Mennen (HM)

Semi-empirical regression for wake fraction `w`, thrust deduction `t`, and relative
rotative efficiency `eta_R` from hull form parameters.

**Strengths:** Widely validated (ITTC standard), good accuracy (±5% on t, w),
minimal input (hull form coefficients).
**Weaknesses:** Provides propulsive coefficients only — no rudder forces.
Valid only for design-speed operation (J = 0.3–1.1), not low-speed maneuvering.
Not a rudder interaction model.

### 2.3 Söding/Brix Slipstream-in-Rudder (SB)

Augments rudder lift/drag with propeller slipstream via the interaction coefficient
`C^(rp)`. Implemented in DRDC ShipMo3D. Validated against Esso Osaka turning trials.

**Key equations:**
- `F_lift^(rp) = C^(rp) * F_prop * (1 + 1/sqrt(1 + C_th)) * sin(delta)`
- `F_drag^(rp) = -C^(rp) * F_prop * (1 + 1/sqrt(1 + C_th)) * (1 - cos(delta))`

**Strengths:** Full J range 0.0–1.1, direct rudder force output in ship axes,
proven in maneuvering simulators, few inputs (K_T curve, t, w, C^(rp)),
single-expression evaluation (microseconds).
**Weaknesses:** C^(rp) is semi-empirical (0.0–1.0); uncertainty ~±10–20%.
Less accurate for twin-screw or rudders outside slipstream.

### 2.4 Molland & Turnock Empirical Corrections (MT)

Tabulated C_L vs angle-of-attack for rudders in propeller slipstreams, from
systematic wind tunnel and towing tank tests.

**Key equations:**
- `C_L^(slipstream) / C_L^(freestream) ~ (Va_R / V_s)^2`
- Tabulated for NACA 0012/0015/0020, AR = 1.0–3.0, J = 0.2–1.1

**Strengths:** Systematic experimental data, ±5–10% accuracy, includes rudder
geometry effects (section type, aspect ratio).
**Weaknesses:** Requires interpolation into published tables (or digitized curves).
Valid only for J = 0.2–1.1 — no low-speed / bollard data. Rudder C_L only, not
full ship-axis forces.

### 2.5 Hybrid: AD Slipstream + Söding Rudder Forces (Hybrid)

Combines actuator-disk velocity field (Section 2.1) with Söding's interaction
force model (Section 2.3). Uses AD to compute Va_R and Vt_R at the rudder plane,
then feeds these into a rudder force model with C^(rp) calibration.

**Strengths:** Physics-based velocity field + empirical rudder forces.
Full J range. More accurate Va_R than Söding's simplified form at intermediate J.
**Weaknesses:** Two-model coupling adds implementation complexity.
Marginal accuracy improvement over pure Söding for maneuvering applications.

---

## 3. Comparison Matrix

| Criterion | AD | HM | SB (Söding) | MT | Hybrid |
|-----------|----|----|-------------|----|----|
| **Accuracy** (agreement with exp.) | ±10% Va | ±5% t,w | ±10–20% forces | ±5–10% C_L | ±10–15% forces |
| **Input requirements** | T, D, V_A | Hull form | K_T, t, w, C^(rp) | Va_R, AR, section | K_T, t, w, C^(rp), x |
| **J range validity** | 0.3–1.1 (degrades <0.3) | 0.3–1.1 | **0.0–1.1** | 0.2–1.1 | **0.0–1.1** |
| **Computational cost** | ~μs | ~μs | ~μs | ~ms (interpolation) | ~μs |
| **Implementation complexity** | Low (3 eqns) | Low (regression) | **Low (5 eqns)** | Medium (table lookup) | Medium (7+ eqns) |
| **Rudder forces?** | No (Va only) | No | **Yes — ship axes** | Partial (C_L only) | Yes — ship axes |

### Scoring (1–5, higher = better)

| Criterion | Weight | AD | HM | SB | MT | Hybrid |
|-----------|--------|----|----|----|----|--------|
| Accuracy | 3 | 3 | 4 | 3 | 4 | 3 |
| Input requirements | 2 | 5 | 4 | 4 | 3 | 3 |
| J range | 3 | 2 | 2 | **5** | 3 | **5** |
| Computational cost | 3 | 5 | 5 | **5** | 4 | 5 |
| Implementation complexity | 2 | 5 | 5 | **5** | 3 | 3 |
| **Weighted total** | | **50** | **51** | **57** | **45** | **51** |

---

## 4. Primary Method Selected: Söding/Brix (SB)

### Rationale

1. **Full J range (0.0–1.1):** Only SB and Hybrid cover the complete maneuvering
   envelope including drive-off startup (J → 0) and engine-off (n = 0). This is
   critical for the parent feature WRK-1147 which requires drive-off simulation.

2. **Direct rudder force output in ship axes:** SB produces surge, sway, and yaw
   forces directly — no post-processing or coordinate transforms needed. AD and HM
   do not provide rudder forces at all.

3. **Minimal inputs:** Requires only K_T(J) polynomial, thrust deduction t, wake
   fraction w, and interaction coefficient C^(rp). All available from Holtrop-Mennen
   regression or a single model test.

4. **Proven in maneuvering simulators:** Validated against Esso Osaka full-scale
   turning trials (McTaggart 2005). Widely used in DRDC ShipMo3D and commercial
   maneuvering programs.

5. **Trivial to implement:** Five closed-form expressions, no table lookups, no
   iterative solvers. Evaluates in microseconds — suitable for time-domain simulation
   loops at ~100 Hz.

6. **Hybrid adds complexity without proportional accuracy gain:** The Hybrid scores
   only 51 vs SB's 57. The additional AD velocity field calculation improves Va_R
   accuracy at intermediate J, but this is already where SB is most accurate.
   The marginal gain does not justify the coupling complexity.

### Selected equations (implementation reference)

```
J       = V_s * (1 - w) / (n * D)
K_T     = f(J)                          # polynomial from open-water test or regression
F_prop  = (1 - t) * rho * n^2 * D^4 * K_T
C_th    = F_prop / (0.5 * rho * (V_s*(1-w))^2 * pi/4 * D^2)

F_lift  = C_rp * F_prop * (1 + 1/sqrt(1 + C_th)) * sin(delta)
F_drag  = -C_rp * F_prop * (1 + 1/sqrt(1 + C_th)) * (1 - cos(delta))

F_surge = F_drag
F_sway  = -F_lift * cos(Gamma)          # Gamma=0 for vertical rudder
F_yaw   = -F_lift * cos(Gamma) * x_R    # x_R = unsigned CG-to-rudder distance (positive)
# Sign convention: x forward, y to port. x_R > 0 always (rudder is aft of CG).
```

---

## 5. Fallback Method: Actuator-Disk (AD) + Flat-Plate Rudder

### When to use

| Condition | Primary (SB) | Fallback (AD) |
|-----------|-------------|---------------|
| Normal operation (J = 0.3–1.1) | Use | — |
| Low-speed maneuvering (J = 0.0–0.3) | Use | — |
| Engine-off (n = 0, T = 0) | Returns zero (correct) — misses ship-speed rudder force | **Use flat-plate for ship-speed component** |
| No K_T data available | Cannot use | Use with estimated C_T from power |
| Twin-screw (rudder outside slipstream) | C^(rp) → 0 | Use for each prop independently |

### Engine-off handling

When n = 0 (engine shutdown), F_prop = 0 and SB forces vanish. The rudder still
experiences hydrodynamic forces from ship forward speed. In this regime:

1. Compute rudder inflow from ship speed: `V_R = V_s * (1 - w_rudder)`
2. Use flat-plate / NACA lift curve: `C_L = (2*pi*AR/(AR+2)) * alpha`
3. Rudder force: `F_R = 0.5 * rho * V_R^2 * A_rudder * C_L`

This is a degenerate case of the AD model with C_T = 0 (no propeller acceleration).
Note: SB returns exactly zero propeller-augmented force when n = 0, T = 0 (correct
behaviour, not degradation). The flat-plate model captures the residual rudder force
from ship forward speed that SB was never designed to model.

### Free-wheeling propeller

When the engine is declutched but the propeller windmills, use 4-quadrant data
(WRK-1280) to obtain K_T at the windmilling J. Then feed this into the SB model
normally — the SB equations remain valid as long as K_T is correctly signed.

**Guard condition:** If `1 + C_th < 0` (braking quadrant where the propeller absorbs
more energy than it transmits), the `sqrt(1 + C_th)` term becomes undefined. In this
regime, fall back to the engine-off flat-plate model above rather than extrapolating
the SB interaction term — Söding's model was not derived for 2nd/4th quadrant operation.

---

## 6. Known Limitations and Validation Gaps

### Limitations of the Söding model

| Limitation | Impact | Mitigation |
|------------|--------|------------|
| C^(rp) is semi-empirical (0.0–1.0) | ±10–20% force uncertainty | Calibrate against model test or CFD for specific hull |
| No unsteady blade-frequency forces | Missing fatigue-relevant fluctuations | Use BEM (Greco et al.) if fatigue assessment needed |
| Single-screw assumption | Not directly applicable to twin-screw | Set C^(rp) per rudder based on slipstream overlap |
| Tangential velocity (Vt) neglected | Underestimates effective AoA at high loading | Accept for maneuvering; add AD-derived Vt for detailed work |
| No rudder stall prediction | Overpredicts force beyond stall angle (~30–35°) | Clamp delta to ±35° or add empirical stall correction |

### Validation gaps

| Gap | Priority | Path to resolution |
|-----|----------|--------------------|
| No validation data for J < 0.2 (near-bollard) | Medium | CFD study or captive model test at low advance ratio |
| C^(rp) sensitivity to rudder position (x/D) | High | Parametric RANS sweep at x/D = 0.3–0.8 |
| Twin-screw interaction | Low (out of scope) | Separate study if twin-screw vessels enter scope |
| Rudder stall angle in slipstream | Medium | Compare Molland & Turnock data at high delta |
| Scale effects on C^(rp) | Low | ITTC 1978 wake correction applies to w; C^(rp) assumed scale-independent |

### Recommended calibration approach

For a specific vessel, calibrate C^(rp) by matching the SB model's turning circle
diameter against full-scale trial data or RANS simulation:

1. Run SB with C^(rp) = 0.9 (Esso Osaka default)
2. Compare predicted tactical diameter with trial data
3. Adjust C^(rp) in steps of 0.05 until agreement within 10%
4. Document calibrated C^(rp) in vessel data file

---

## 7. Quantitative Comparison (Esso Osaka, δ=20°, n=1.5 rps)

J-sweep results from `propeller_rudder.py` implementation:

| J | V_s [m/s] | SB sway [kN] | AD sway [kN] | Ratio SB/AD |
|---|-----------|-------------|-------------|-------------|
| 0.10 | 2.1 | -1,617 | -4,417 | 0.37 |
| 0.20 | 4.2 | -1,671 | -4,425 | 0.38 |
| 0.30 | 6.3 | -1,688 | -4,490 | 0.38 |
| 0.50 | 10.5 | -1,578 | -4,799 | 0.33 |
| 0.70 | 14.7 | -1,253 | -5,367 | 0.23 |
| 1.00 | 21.1 | -373 | -6,751 | 0.06 |

### Key finding: the two methods model different quantities

- **Söding/Brix** computes the propeller-augmented *interaction increment* —
  the additional rudder force caused by the propeller slipstream. Correctly
  diminishes toward zero as propeller loading decreases (high J).
- **AD + flat-plate** computes the *total rudder force* in the slipstream —
  amplified velocity applied to the full rudder area. Grows with V² as
  ship speed increases.

These are **complementary, not competing**:

| Aspect | Söding | AD + Flat-Plate |
|--------|--------|-----------------|
| What it computes | Interaction increment | Total rudder force |
| Validated against | Turning circle trials | Slipstream PIV data |
| At n = 0 | Zero (correct) | Ship-speed rudder force |
| Use in maneuvering | Additive to hull forces | Baseline rudder model |

---

## 8. Decision Summary: Layered Architecture

The total rudder force in a maneuvering simulation is:

```
F_rudder_total = F_rudder_baseline (AD flat-plate) + F_rudder_interaction (Söding)
```

This is consistent with McTaggart (2005), where hull force equations sum:
hull derivatives + propeller thrust + rudder baseline + rudder-propeller interaction.

| Decision | Value |
|----------|-------|
| **Interaction model** | Söding/Brix — propeller-augmented rudder force increment |
| **Baseline rudder model** | AD + flat-plate — total rudder force from ship-speed inflow |
| **Layered total** | F_total = AD_baseline + Söding_interaction |
| **Engine-off** | Only AD baseline survives (Söding correctly returns zero) |
| **Braking quadrant** | Guard clamp on C_th < -1; fall back to AD baseline only |
| **J range covered** | 0.0–1.1 (both models) + engine-off (AD only) |
| **Key inputs** | K_T(J), t, w, C^(rp), D, rudder area/AR/x_R |
| **Expected accuracy** | ±10–20% on interaction; ±5–10% on baseline (single-screw) |
| **Implementation** | `propeller_rudder.py` — 150 lines, 36 tests pass |
| **Next WRK** | WRK-1150 (integration into maneuvering model) |

---

## References

See [propeller-rudder-literature.md](propeller-rudder-literature.md) (WRK-1148) for
full formulae, applicability ranges, and source citations.

Implementation: `src/digitalmodel/hydrodynamics/propeller_rudder.py`
Tests: `tests/test_propeller_rudder_interaction.py` (36 pass)
