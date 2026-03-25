# Parachute Aerodynamics — Reference Document

> WRK-5082 | Compiled: 2026-03-16

## 1. Fundamental Drag Equation

The aerodynamic drag force on a parachute:

```
F = 0.5 × ρ × V² × Cd × S₀ × Cx
```

Where:
- `F` = drag force (lbs in Imperial, N in SI)
- `ρ` = air density (0.002378 slug/ft³ = 1.225 kg/m³ at sea level standard)
- `V` = velocity (ft/s or m/s)
- `Cd` = drag coefficient (dimensionless)
- `S₀` = nominal canopy area = π × (D/2)² (ft² or m²)
- `Cx` = opening shock factor (dimensionless, 1.0 for steady-state)

**Sources**:
- Knacke, T.W., *Parachute Recovery Systems Design Manual*, NWC TP-6575, 1992. [Full PDF (DTIC ADA247666)](https://apps.dtic.mil/sti/tr/pdf/ADA247666.pdf)
- Ewing, Bixby & Knacke, *Recovery Systems Design Guide*, AFFDL-TR-78-151, 1978.
- Pflanz method: F_peak = q₁ × Cd × S₀ × Cx × X₁ (where X₁ = finite mass reduction factor ≤ 1.0)

---

## 2. Drag Coefficient (Cd) by Parachute Type

| Parachute Type | Cd (ref: S₀) | Notes |
|---|---|---|
| Flat circular (parasheet) | 0.75–0.90 | Simplest design; model rocket standard |
| Hemispherical | 1.2–1.5 | Open-end facing flow: ~1.42 |
| Conical | 0.8–1.0 | Better stability than flat |
| Ribbon | 0.5–0.7 | Low opening shock; high-speed use |
| Cross (cruciform) | 0.6–0.85 | Good high-speed stability; hard opening |
| Ellipsoidal | 1.5 | High drag per unit area |
| Ringslot | 0.6–0.7 | Moderate opening shock |
| Guide surface | 0.3–0.4 | Drogue applications |
| **Stroud drag racing** | **1.2–1.5** | Proprietary design; softer opening |

**Sources**:
- [Nakka Rocketry Appendix K](https://nakka-rocketry.net/Appendix-K.htm)
- [NASA NTRS — Drag coefficients for flat circular parachutes](https://ntrs.nasa.gov/api/citations/19710023919/downloads/19710023919.pdf)
- [CFD Analysis of Drag Coefficient (ResearchGate)](https://www.researchgate.net/publication/267497133)

---

## 3. Opening Shock Factor (Cx)

The opening shock factor multiplies the steady-state drag to account for the transient peak force during canopy inflation.

| Parachute Type | Cx Range | Notes |
|---|---|---|
| Flat circular | 1.5–1.8 | Hard opening |
| Hemispherical | 1.4–1.7 | Moderate |
| Cross (cruciform) | 1.0–1.2 | Low opening shock |
| Ellipsoidal | 1.4–1.6 | Moderate |
| Ribbon | 1.0–1.1 | Designed for low shock |
| **Stroud design** | **1.2–1.5** | Shroud lines tension before canopy opens → softer |

**Key insight (Bob Stroud)**: "The shroud lines' slack is taken out before the canopy opens, which causes it to open up softer." Cross-form designs open harder because "the canopy opens before the slack in the shroud lines are out."

**Sources**:
- [Nakka Rocketry — Opening Force Coefficients](https://nakka-rocketry.net/Appendix-K.htm)
- [Dragstuff — Bob Stroud Q&A](https://www.dragstuff.com/techarticles/stroud-parachute-qa.html)

---

## 4. Parachute Deployment Dynamics

### Yank Load Analysis (Bonneville/Land Racing Example)

From Dr. Mayf's analysis for a 400 MPH streamliner:

| Parameter | Value |
|---|---|
| Vehicle speed | 400 mph (586.8 ft/s) |
| Chute diameter | 4 ft |
| Canopy area | 12.56 ft² |
| Cd | 1.20 |
| Air density (Bonneville) | 0.00186 slug/ft³ |
| Tow line length | 80 ft |
| Canopy opening time | 30–480 ms |
| Aero yank rate | 321,658 lbs/s |
| Inertial yank rate | 9,861 lbs/s |
| **Total yank** | **~330,000 lbs/s** |

**Total force equation**: FT = FD + FN (aerodynamic + inertial)

**Source**: [LandRacing.com — Parachute Yank Loads](https://www.landracing.com/index.php/8-dr-mayfs-analysis/dr-mayfs-analyses/19-parachute-yank-loads)

### Deceleration at Deployment

For a 3,600 lb car at 250 MPH with single 12 ft chute (Cd=1.5, no Cx):
- Steady-state drag: ~27,000 lbs
- Deceleration: ~7.5 g (instantaneous, without Cx)
- With Cx=1.5: ~40,000 lbs → ~11 g peak

**ChatGPT cross-validation**: 25,000–30,000 lbs steady-state at 250 MPH.

---

## 5. NHRA/SFI Parachute Requirements

| Speed Class | Requirement |
|---|---|
| Under 150 mph (¼ mile) | No parachute required |
| 150+ mph (¼ mile) | Single parachute mandatory |
| 200+ mph (¼ mile) | **Dual parachutes** mandatory |
| 125+ mph (⅛ mile) | Single parachute mandatory |

### Mounting Requirements (NHRA)
- Mount at **crankshaft centerline height**
- **Centered left-to-right** on chassis
- Pack angled **45° upward** from horizontal
- Must extend behind and above spoiler
- Pull point at vehicle center of gravity
- Floor must be flat; max 6" extension beyond pack

### SFI Specifications
- **SFI 9.1**: Parachute specification (correct spec — SFI 35.1 does not exist)
- **SFI 25.1**: Roll cage/chassis tube specification

**Sources**:
- [DSPORT — Drag Racing Parachutes 101](https://dsportmag.com/the-tech/education/quick-tech-drag-racing-parachutes-101/)
- [State of Speed — Safety Equipment](https://stateofspeed.com/2019/09/04/drag-racing-dragstrip-pass-3/)
- [NHRA Parachute Tether Spec V2](https://www.nhraracer.com/Files/Tech/NHRA_Parachute_Tether_Specification_V2.pdf)

---

## 6. Stroud Safety Product Data

| Model | Price Range | Notes |
|---|---|---|
| 400 | $429–$849 | Entry-level |
| 410 | $469–$879 | Mid-range |
| 420 | $529–$899 | Mid-range |
| 430 | $549–$939 | Standard — sizes: 24, 26, 28, 30, 32 |
| **450** | **$629–$999** | **Pro-level — probable GT1R match** |
| 470 (Funny Car) | $659–$999 | Top-level |

All models: available in Spring Pilot, Spring Launcher, or Air Launcher deployment.
Material: proprietary UV/heat/water-resistant ripstop fabric, 100% USA made.

Stroud sizing chart (from WRK-5082 design basis):
- 3,200–4,000 lb vehicle → **Model 430 Std. 32** (single, up to ~200 mph)
- 3,500–4,000 lb, 280–320 mph → **Model 450 / 470** (dual)

**Confirmed**: T1 Race Development bundles **Stroud 430** with the GT1R kit (+$541 add-on).
Available canopy sizes: 24", 26", 28", 30", 32" diameter. Sizes 24"–30" require qty 2 (dual chute); 32" is single-chute.

**Note on Cd**: Stroud does not publish Cd values. Based on the modified circular/conical
design with deployment bag, engineering estimate is Cd ≈ 0.70–0.80 (ref: S₀). The analysis
uses Cd = 1.4 (ref: projected area) which is conservative.

**Source**: [Stroud Safety — Parachutes](https://stroudsafety.com/parachutes/)

---

## 7. Canonical References

1. **Knacke, T.W.** *Parachute Recovery Systems Design Manual*, Para Publishing, 1992.
   [Amazon](https://www.amazon.com/Parachute-Recovery-Systems-Design-Manual/dp/0915516853) |
   [Internet Archive](https://archive.org/details/parachuterecover0000knac)

2. **Ewing, Bixby & Knacke.** *Recovery Systems Design Guide*, AFFDL-TR-78-151, 1978.
   [DTIC](https://apps.dtic.mil/sti/tr/pdf/ADA070251.pdf)

3. **NASA TN D-6458.** *Drag coefficients for partially inflated flat circular parachutes*, 1971.
   [NASA NTRS](https://ntrs.nasa.gov/api/citations/19710023919/downloads/19710023919.pdf)

4. **MAE 6530 — Recovery Systems: Parachutes 101** (Utah State University lecture notes).
   [PDF](http://mae-nas.eng.usu.edu/MAE_6530_Web/New_Course/launch_design/Section3.5.pdf)
