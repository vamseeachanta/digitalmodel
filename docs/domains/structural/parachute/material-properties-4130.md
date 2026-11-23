# AISI 4130 Chromoly Steel — Material Reference

> WRK-5082 | Compiled: 2026-03-16

## 1. Chemical Composition (ASTM A519 / AMS 6370)

| Element | Weight % |
|---|---|
| Carbon (C) | 0.28–0.33 |
| Chromium (Cr) | 0.80–1.10 |
| Molybdenum (Mo) | 0.15–0.25 |
| Manganese (Mn) | 0.40–0.60 |
| Silicon (Si) | 0.15–0.35 |
| Phosphorus (P) | ≤ 0.035 |
| Sulfur (S) | ≤ 0.040 |
| Iron (Fe) | Balance |

---

## 2. Mechanical Properties by Heat Treatment

### Normalized at 870°C (1600°F) — **Used in WRK-5082 analysis**

| Property | SI | Imperial |
|---|---|---|
| Ultimate Tensile Strength (Fu) | 670 MPa | **97,200 psi** |
| Yield Strength (Fy) | 435 MPa | **63,100 psi** |
| Elastic Modulus (E) | 205 GPa | **29,700 ksi** |
| Shear Modulus (G) | 80 GPa | 11,600 ksi |
| Poisson's Ratio (ν) | 0.29 | 0.29 |
| Density (ρ) | 7,850 kg/m³ | 0.284 lb/in³ |
| Elongation at Break | 25.5% | — |
| Reduction of Area | 60% | — |
| Hardness | 92 HRB | 217 HB |

### Annealed

| Property | SI | Imperial |
|---|---|---|
| Ultimate Tensile Strength | 560 MPa | 81,200 psi |
| Yield Strength | 460 MPa | 66,700 psi |
| Elongation at Break | 21.5% | — |
| Hardness | 95 HRB | 217 HB |

### Quenched & Tempered (855°C water quench, 480°C temper)

| Property | SI | Imperial |
|---|---|---|
| Ultimate Tensile Strength | 1,030 MPa | 149,000 psi |
| Yield Strength | 910 MPa | 132,000 psi |
| Elongation | 15.5% | — |
| Hardness | 30 HRC | 302 HB |

**Source**: [MatWeb — AISI 4130 Normalized](https://www.matweb.com/search/DataSheet.aspx?MatGUID=e1ccebe90cf94502b35c2a4745f63593)

---

## 3. Applicable Standards

| Standard | Title |
|---|---|
| ASTM A519 | Seamless Carbon and Alloy Steel Mechanical Tubing |
| AMS 6370 | Steel Tubing, Seamless, 0.28-0.33C, 0.80-1.10Cr, 0.15-0.25Mo |
| AMS 6345 | Steel Bars, Forgings, Tubing — Normalized |
| MIL-T-6736 | Steel, Tubes, Seamless and Welded (Aerospace) |
| MIL-S-6758 | Steel, Chromium-Molybdenum (4130) |
| SAE J404 | Chemical Compositions of SAE Alloy Steels |

---

## 4. Allowable Stress Criteria

### ASD (Allowable Stress Design) per AISC

| Stress Type | Allowable | Factor |
|---|---|---|
| Tension | 0.6 × Fy | **37,800 psi** (normalized 4130) |
| Bending (major axis) | 0.66 × Fy | 41,600 psi |
| Bending (minor axis) | 0.75 × Fy | 47,300 psi |
| Shear | 0.4 × Fy | 25,200 psi |

**Note**: AISC ASD allowables apply to individual stress components, not directly to von Mises combined stress. For combined stress checks, compare σ_VM to Fy with appropriate safety factor (typically 1.5–1.67 for structural steel).

### Von Mises Combined Stress Check

```
σ_VM = √(σ² + 3τ²)
```

Where σ = σ_axial + σ_bending (worst case, same sign) and τ = shear stress.

**Allowable (ASD basis)**: σ_VM ≤ 0.6 × Fy = 37,800 psi
**Unity ratio**: U = σ_VM / (0.6 × Fy) → must be ≤ 1.0

**Sources**:
- [AISC ASD Allowable Stress Checks (Eng-Tips)](https://www.eng-tips.com/threads/aisc-asd-allowable-stress-checks-for-von-mises.453786/)
- [Von Mises vs Yield (Eng-Tips)](https://www.eng-tips.com/threads/von-mises-vs-yield-stress-aisc.459841/)
- [AISC Spec — ASD and Plastic Design](https://www.aisc.org/globalassets/aisc/manual/15th-ed-ref-list/specification-for-structural-steel-buildings-allowable-stress-design-and-plastic-design.pdf)

---

## 5. Weld Properties (TIG on 4130)

| Parameter | Value | Notes |
|---|---|---|
| Filler wire | ER70S-2 or ER80S-D2 | ER80S-D2 for strength match |
| Electrode classification | E70 (70 ksi tensile) | Standard for fillet welds |
| Weld allowable shear | 0.3 × 70 ksi = **21,000 psi** | Per AWS D1.1 |
| Effective throat | 0.707 × leg size | 45° fillet weld geometry |
| Post-weld treatment | Stress relieve at 595°C (1100°F) | Recommended for Q&T condition |

**Note**: TIG welding 4130 in the normalized condition does NOT require post-weld heat treatment for most racing applications. The HAZ retains acceptable properties.

---

## 6. Common Tube Sizes in Motorsport

| Application | OD (in) | Wall (in) | A (in²) | I (in⁴) |
|---|---|---|---|---|
| Roll cage main hoop | 1.750 | 0.120 | 0.6145 | 0.1895 |
| Roll cage diagonal | 1.625 | 0.120 | 0.5672 | 0.1500 |
| Parachute frame (est.) | 1.500 | 0.120 | 0.5202 | 0.1248 |
| Support bracing | 1.375 | 0.120 | 0.4733 | 0.0956 |
| Gussets/tabs | 1.250 | 0.120 | 0.4264 | 0.0720 |
| Main hoop (SFI 25.1) | 1.750 | 0.134 | 0.6813 | 0.2086 |

### SFI 25.1 Minimum Tube Sizes (4130 Chromoly)

| Component | 4130 Chromoly | Mild Steel Alt. |
|---|---|---|
| Main hoop, A-pillar, door bars | 1-5/8" × 0.083" | 1-5/8" × 0.118" |
| Firewall/lower dash bar | 1-1/4" × 0.058" | 1-1/4" × 0.118" |
| Rocker/sill bar (rect) | 2" × 2" × 0.058" | 2" × 2" × 0.058" |

**Material requirement**: All structural tubing must be normalized 4130 (4130N)
per MIL-T-6736B or equivalent. 100% TIG welded.

---

## 7. Connection Design Reference Values

### Grade 8 Bolts (SAE J429)
| Property | Value |
|---|---|
| Proof strength | 120 ksi |
| Tensile strength | 150 ksi |
| Shear strength (0.6 × Fu) | **90 ksi** |
| Allowable shear (ASD, 0.6 × 90) | **54,000 psi** |

### Bolt Shear Areas

| Diameter | Single Shear Area | Allowable (Grade 8) |
|---|---|---|
| 5/16" | 0.0767 in² | 4,142 lbs |
| 3/8" | 0.1104 in² | 5,963 lbs |
| 7/16" | 0.1503 in² | 8,117 lbs |
| 1/2" | 0.1963 in² | 10,603 lbs |

### Pin Connections
- Double shear capacity = 2 × single shear area × Fv_allowable
- 1/2" pin, double shear, Grade 8: 2 × 10,603 = **21,206 lbs**

**Sources**:
- [AZoM — AISI 4130 Alloy Steel](https://www.azom.com/article.aspx?ArticleID=6742)
- [Modulus Metal — AISI 4130 Normalized](https://www.modulusmetal.com/aisi-4130-low-alloy-steel-normalized-mechanical-properties/)
- [Thomas — All About 4130 Steel](https://www.thomasnet.com/articles/metals-metal-products/all-about-4130-steel-properties-strength-and-uses/)
