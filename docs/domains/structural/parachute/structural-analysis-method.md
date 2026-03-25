# Structural Analysis Methodology — Reference Document

> WRK-5082 | Compiled: 2026-03-16

## 1. Direct Stiffness Method — 2D Frame Element

### 1.1 Degrees of Freedom

Each node has 3 DOFs: axial displacement (u), transverse displacement (v), rotation (θ).
Each element has 6 DOFs total: [u₁, v₁, θ₁, u₂, v₂, θ₂].

### 1.2 Local Stiffness Matrix (6×6)

For a 2D frame element with length L, Young's modulus E, cross-sectional area A,
and second moment of area I:

```
        [ EA/L      0          0       -EA/L     0          0      ]
        [  0     12EI/L³    6EI/L²      0    -12EI/L³    6EI/L²   ]
K_L =   [  0      6EI/L²    4EI/L       0     -6EI/L²    2EI/L    ]
        [-EA/L     0          0        EA/L     0          0       ]
        [  0    -12EI/L³   -6EI/L²      0     12EI/L³   -6EI/L²   ]
        [  0      6EI/L²    2EI/L       0     -6EI/L²    4EI/L    ]
```

Combines:
- **Axial** (bar element): EA/L terms in rows/cols 0, 3
- **Bending** (Euler-Bernoulli beam): EI terms in rows/cols 1, 2, 4, 5

### 1.3 Transformation Matrix

For element inclined at angle α from the global X-axis (c = cos α, s = sin α):

```
        [ c   s   0   0   0   0 ]
        [-s   c   0   0   0   0 ]
T =     [ 0   0   1   0   0   0 ]
        [ 0   0   0   c   s   0 ]
        [ 0   0   0  -s   c   0 ]
        [ 0   0   0   0   0   1 ]
```

Global stiffness: **K_G = Tᵀ × K_L × T**

### 1.4 Assembly and Solution

1. Build element stiffness matrices K_G for each member
2. Assemble into global stiffness matrix K by DOF mapping
3. Form global load vector F from applied forces/moments
4. Apply boundary conditions (zero rows/cols for fixed DOFs)
5. Solve K × u = F for displacement vector u
6. Compute reactions: R = K_original × u − F
7. Extract member-end forces: f_local = K_L × (T × u_elem)

### 1.5 Equilibrium Verification

Sum of all reaction forces must equal sum of all applied forces:
- ΣR_x = ΣF_x (horizontal equilibrium)
- ΣR_y = ΣF_y (vertical equilibrium)
- ΣM = 0 (moment equilibrium about any point)

Tolerance: 0.1% of applied load.

**Sources**:
- [Duke CEE 421 — Frame Element Stiffness Matrices](https://people.duke.edu/~hpgavin/cee421/frame-element.pdf)
- [Wikipedia — Direct Stiffness Method](https://en.wikipedia.org/wiki/Direct_stiffness_method)
- Przemieniecki, J.S. *Theory of Matrix Structural Analysis*, Dover, 1985
- McGuire, Gallagher & Ziemian. *Matrix Structural Analysis*, 2nd Ed., Wiley, 2000
- [Colin Caprani — Matrix Stiffness Method](http://www.colincaprani.com/files/notes/SAIV/Matrix%20Stiffness%20Method%200910.pdf)

---

## 2. Von Mises Yield Criterion

### 2.1 General 3D Stress State

```
σ_VM = √[ 0.5 × ((σ₁−σ₂)² + (σ₂−σ₃)² + (σ₃−σ₁)²) + 3(τ₁₂² + τ₂₃² + τ₃₁²) ]
```

### 2.2 Tube Under Combined Axial + Bending + Shear

For a tubular member with:
- σ_axial = N / A (axial force / cross-section area)
- σ_bending = M × c / I (bending moment × distance to neutral axis / moment of inertia)
- τ = 2V / A (average shear stress, thin-walled tube)

Combined normal stress at extreme fiber (worst case):
```
σ_total = σ_axial + σ_bending    (when same sign)
```

Von Mises combined stress:
```
σ_VM = √(σ_total² + 3τ²)
```

### 2.3 Yield Check

Material yields when σ_VM ≥ Fy (yield strength).

**ASD check**: σ_VM ≤ 0.6 × Fy (safety factor ≈ 1.67)

**Unity ratio**: U = σ_VM / (0.6 × Fy) — must be ≤ 1.0

**Sources**:
- [Von Mises Yield Criterion (Wikipedia)](https://en.wikipedia.org/wiki/Von_Mises_yield_criterion)
- [SDC Verifier — Von Mises in FEA](https://sdcverifier.com/articles/what-is-von-mises-stress/)
- [SimScale — Von Mises Stress](https://www.simscale.com/docs/simwiki/fea-finite-element-analysis/what-is-von-mises-stress/)

---

## 3. ASME Combined Stress

### B31-type Stress Intensity

For piping/tubular members (ASME B31.3, Section VIII Div. 2):

```
S_I = max(|σ₁ − σ₂|, |σ₂ − σ₃|, |σ₃ − σ₁|)
```

Where σ₁, σ₂, σ₃ are principal stresses. For thin-walled tubes:

```
σ₁, σ₂ = (σ_x + σ_y)/2 ± √[((σ_x − σ_y)/2)² + τ²]
```

**Allowable**: S_I ≤ S_allowable (per ASME code case)

**Note**: For racing chassis applications, von Mises with ASD factors is more commonly used than ASME code stress intensity.

---

## 4. Tube Section Properties

For a circular hollow section with outer diameter D and wall thickness t:

| Property | Formula |
|---|---|
| Inner diameter | d = D − 2t |
| Cross-section area | A = π/4 × (D² − d²) |
| Moment of inertia | I = π/64 × (D⁴ − d⁴) |
| Section modulus | S = I / (D/2) |
| Radius of gyration | r = √(I/A) |

---

## 5. Connection Design Methods

### 5.1 Bolt Shear Check

```
τ_bolt = V / (n × A_bolt)
Unity = τ_bolt / F_v_allowable
```

Where:
- V = total shear force on connection
- n = number of bolts
- A_bolt = π/4 × d²
- F_v_allowable = 0.6 × F_u_bolt (Grade 8: 54,000 psi)

### 5.2 Fillet Weld Throat Check

```
f_weld = F / (L_weld × a)
Unity = f_weld / F_w_allowable
```

Where:
- F = force on weld
- L_weld = total weld length
- a = effective throat = 0.707 × leg size
- F_w_allowable = 0.3 × F_EXX (E70: 21,000 psi)

### 5.3 Pin Shear Check

```
τ_pin = V / (n_planes × A_pin)
Unity = τ_pin / F_v_allowable
```

Where n_planes = 1 (single shear) or 2 (double shear).

**Sources**:
- AISC *Steel Construction Manual*, 15th Edition
- AWS D1.1 *Structural Welding Code — Steel*
- [AISC ASD Spec](https://www.aisc.org/globalassets/aisc/manual/15th-ed-ref-list/specification-for-structural-steel-buildings-allowable-stress-design-and-plastic-design.pdf)

---

## 6. Textbook References

| Ref | Author | Title | Use |
|---|---|---|---|
| [1] | Przemieniecki, J.S. | *Theory of Matrix Structural Analysis*, Dover, 1985 | Stiffness method theory |
| [2] | McGuire, Gallagher, Ziemian | *Matrix Structural Analysis*, 2nd Ed., Wiley, 2000. [**Free PDF**](https://digitalcommons.bucknell.edu/books/7/) | Frame element derivation |
| [3] | Hibbeler, R.C. | *Structural Analysis*, 10th Ed., Pearson, 2017 | General structural analysis |
| [4] | Kassimali, A. | *Matrix Analysis of Structures*, 3rd Ed., Cengage, 2021 | Assembly procedure |
| [5] | Knacke, T.W. | *Parachute Recovery Systems Design Manual*, Para Publishing, 1992 | Parachute aerodynamics |
| [6] | Ewing, Bixby, Knacke | *Recovery Systems Design Guide* (AFFDL-TR-78-151), 1978 | Cd and Cx data |
