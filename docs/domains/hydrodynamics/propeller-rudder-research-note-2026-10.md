> Source: [2026-10-11 research note on issue 143](https://github.com/vamseeachanta/digitalmodel/issues/143#issuecomment-6115228312).

# Propeller-rudder interaction — 2026-10 research note

This is a transcription and structuring of the 2026-10-11 research note on [issue 143](https://github.com/vamseeachanta/digitalmodel/issues/143), recording only what the note reports was read on pages opened that day. Its single factual source is the issue comment linked above. The underlying papers have not been independently opened for this transcription. This reference contains no implementation and does not replace [propeller-rudder-literature.md](propeller-rudder-literature.md) or [propeller-rudder-method-selection.md](propeller-rudder-method-selection.md).

Source IDs R1–R9 and L1–L11 are assigned by this transcription, in the order of the note’s source lists. Read entries retain the limited parts read; located entries remain located, not read.

## 1. Bottom line

1. The modular manoeuvring-model formulation (momentum-theory slipstream velocity, an empirical rudder normal-force coefficient, and four hull-rudder interaction coefficients) is the only approach found with a complete, openly reproducible coefficient set for a benchmark hull (KVLCC2). It suits a fast open implementation. The equations and the coefficient table were read in an open repository copy of Liu, Quadvlieg and Hekkenberg (2016), which reproduces them with attribution to Yasukawa and Yoshimura; the primary paper itself could not be opened (publisher redirect).
2. That formulation is a first-quadrant, moderate-rudder-angle model. The page read states that it neglects the rudder tangential force and that this is valid for rudder angles between -35 and 35 degrees. Its slipstream term contains thrust coefficient divided by advance ratio squared, so the engine-off, start-up and bollard regimes in this issue's scope (n = 0, J tending to zero or infinity) fall outside the form as written and need a separate treatment. The Southampton wind-tunnel programme (bollard and four-quadrant reports) is the published data for those regimes; it was located but could not be opened.
3. Higher-fidelity methods are documented as calibration tools, not as simulation kernels, with one exception: Molland and Turnock (1996) present a lifting-line rudder with a blade-element-momentum propeller as "rapid and compact" enough for a manoeuvring simulation. No source read supports a Holtrop-Mennen rudder correction; that acceptance item has no located reference.

## 2. Approaches and the papers that define them

| Approach | What it computes | Defining or describing source | Status |
|---|---|---|---|
| Modular manoeuvring model with momentum-theory slipstream | Rudder forces from a normal force, an effective inflow angle, and an inflow speed raised by propeller loading | Yasukawa and Yoshimura, "Introduction of MMG standard method for ship maneuvering predictions", J Mar Sci Technol 20(1), 37-52, 2015 | Primary not opened; equations read as reproduced in Liu, Quadvlieg and Hekkenberg, Ocean Engineering 2016 (doi 10.1016/j.oceaneng.2016.07.064, repository manuscript) |
| Empirical rudder normal-force coefficient | Coefficient from aspect ratio and effective angle | Fujii 1960; Fujii and Tsuda 1961, 1962 (in Japanese), as cited by Liu et al. | Formula read in Liu et al. |
| Potential-flow rudder with circumferentially averaged slipstream | Slipstream axial, swirl and radial velocity from thrust and torque, contraction with distance, then rudder forces by a panel method | Soding, "Limits of Potential Theory in Rudder Flow Predictions", 22nd Symposium on Naval Hydrodynamics, pp. 622-637; contraction relation attributed there to Soding (1981), "Forces on rudders behind a maneuvering ship" | One page read (section 3.2); equations are images on that page and were not recoverable |
| Lifting line plus blade-element-momentum propeller | Spanwise rudder loading with propeller-modified inflow speed and incidence | Molland and Turnock, "A compact computational method for predicting forces on a rudder in a propeller slipstream", RINA Spring Meeting, 1996, Paper 2 | Abstract record read |
| Lifting-surface panel with interaction velocity field | Rudder and propeller modelled separately, coupled iteratively through circumferentially averaged induced velocities | Turnock, Wellicome and Molland, Propellers/Shafting '94, SNAME, Paper 18 | Abstract record read |
| Lifting line (propeller) plus panel method (rudder), two-way | Rudder forces for design use | Carstensen, PRADS 2019 | Abstract record read |
| Lifting surface with propeller-induced velocities | Rudder pressure, lift and drag in the slipstream | Le Thuy Hang, Polish Maritime Research 8(2), 2001 | Abstract page read |
| Rudder-plus-propeller database, four quadrants | Combined unit treated as one force generator; tested in a simulator | Molland, Turnock and Wilson, MARSIM '96, p. 425 | Abstract record read |
| Viscous CFD with body-force or resolved propeller | Force split between hull, propeller and rudder; response surfaces | Bulten, smp'24 (doi 10.15480/882.9324) read; Phillips, Turnock and Furlong 2009 and Badoe, Phillips and Turnock located only | One read, others located |

## 3. Equations and coefficients (as printed in Liu, Quadvlieg and Hekkenberg 2016, attributed there to Yasukawa and Yoshimura unless stated)

Plain-text transcription; equation numbers are those of Liu et al.

### Rudder normal force (eq. 1)

As printed in Liu, Quadvlieg and Hekkenberg 2016 [R1], attributed there to Yasukawa and Yoshimura; primary not opened.

```
F_N = 0.5 rho A_R V_R^2 C_N
```

### Normal-force coefficient (eq. 2, Fujii; Fujii and Tsuda)

As printed in Liu, Quadvlieg and Hekkenberg 2016 [R1], attributed there to Fujii; Fujii and Tsuda; primary not opened.

```
C_N = 6.13 sin(alpha_R) Lambda / (Lambda + 2.25), with Lambda the geometric aspect ratio
```

### Rudder forces on the ship (eq. 13)

As printed in Liu, Quadvlieg and Hekkenberg 2016 [R1], attributed there to Yasukawa and Yoshimura; primary not opened.

```
X_R = -(1 - t_R) F_N sin(delta); Y_R = -(1 + a_H) F_N cos(delta); N_R = -(x_R + a_H x_H) F_N cos(delta)
```

### Effective rudder angle (eq. 14)

As printed in Liu, Quadvlieg and Hekkenberg 2016 [R1], attributed there to Yasukawa and Yoshimura; primary not opened.

```
alpha_R = delta - arctan(v_R / u_R)
```

### Longitudinal inflow (eq. 15)

As printed in Liu, Quadvlieg and Hekkenberg 2016 [R1], attributed there to Yasukawa and Yoshimura; primary not opened.

```
u_R = u (1 - w_R) sqrt( eta [ 1 + kappa ( sqrt(1 + 8 K_T / (pi J^2)) - 1 ) ]^2 + (1 - eta) ), where w_R is the wake factor at the rudder, kappa "an experimental constant" and eta the ratio of propeller diameter to rudder span
```

### Lateral inflow (eq. 16)

As printed in Liu, Quadvlieg and Hekkenberg 2016 [R1], attributed there to Yasukawa and Yoshimura; primary not opened.

```
v_R = V gamma_R (beta - l'_R r'), gamma_R the flow-straightening factor, different to port and starboard
```

### Propeller wake in manoeuvring (eqs. 11, 12)

As printed in Liu, Quadvlieg and Hekkenberg 2016 [R1], attributed there to Yasukawa and Yoshimura; primary not opened.

```
beta_P = beta - x'_P r'; (1 - w_P) / (1 - w_P0) = 1 + [1 - exp(-C_1 |beta_P|)] (C_2 - 1)
```

Interaction coefficients defined: t_R (steering resistance deduction), a_H (rudder force increase factor), x_H (longitudinal position of the additional lateral force), gamma_R, l'_R, kappa, and a wake ratio at the rudder.

### KVLCC2 model-scale values as reproduced in the secondary paper; primary not opened

Table 2 of Liu, Quadvlieg and Hekkenberg 2016 [R1] (source stated as Yasukawa and Yoshimura). k0, k1 and k2 are the thrust polynomial coefficients. A blank unit or condition records that the note gives none for that entry.

| Symbol or label as printed | Value | Unit | Condition as printed |
|---|---|---|---|
| Lpp | 7.0 | m |  |
| propeller diameter | 0.216 | m |  |
| x'_P | -0.48 |  |  |
| k0 | 0.2931 |  |  |
| k1 | -0.2753 |  |  |
| k2 | -0.1385 |  |  |
| t_P | 0.220 |  |  |
| w_P0 | 0.40 |  |  |
| rudder area | 0.0539 | m2 |  |
| aspect ratio | 1.827 |  |  |
| x'_R | -0.50 |  |  |
| t_R | 0.387 |  |  |
| a_H | 0.312 |  |  |
| x'_H | -0.464 |  |  |
| C_1 | 2.0 |  |  |
| C_2 | 1.6 |  | beta_P positive |
| C_2 | 1.1 |  | negative |
| gamma_R | 0.395 |  | beta_R negative |
| gamma_R | 0.640 |  | positive |
| l'_R | -0.710 |  |  |

### To be confirmed against the primary paper before use

> Two further constants printed as 1.09 and 0.50 whose symbol labels were lost in text extraction (to be confirmed against the primary paper before use). No symbol is assigned to either constant. They must not be used in any calculation until confirmed.

Soding's slipstream model (section 3.2 of the lecture, prose only): slipstream averaged circumferentially; far-downstream mean axial velocity from thrust by axial momentum; thrust-to-torque ratio assumed independent of radius; swirl from torque; far-field radius from continuity; contraction as a function of distance in propeller radii; a radial thrust distribution extending slightly beyond the propeller radius to represent turbulent mixing. He states that details of the radial distribution "hardly influence the rudder force" and that the larger error sources are turbulent mixing, flow acceleration by the hull, and flow curvature behind a drifting or turning hull.

## 4. Stated ranges of validity

| Source | Statement |
|---|---|
| Liu et al. 2016 | Tangential rudder force neglected; valid for rudder angles of -35 to 35 degrees; "further study is needed" for large angles |
| Liu et al. 2016 | Slipstream assumed only to extend stall (from 15 to 20 degrees to 30 to 35 degrees in the Nienhuis 1987 tests at advance ratios 0.0, 0.2, 0.4 and pitch ratios 0.8 to 1.2) without changing the normal-force slope; further research suggested |
| Liu et al. 2016 | Validation against free-running tests of KVLCC2 at scale 45.7; port-starboard asymmetry underestimated; one turning index differs by 4.56 % |
| Soding lecture | Circumferential averaging suits time-averaged manoeuvring forces, not vibration; rudder stock moment errors of about 3 to 8 % of chord in the propeller-rudder comparison |
| Molland and Turnock 1996 (abstract) | "Satisfactory predictions of the form of the spanwise loadings" over the thrust loadings and separations tested; no numerical limits in the abstract |
| Bulten 2024 | A CFD-fitted response-surface model needs "quite some simulation capacity"; virtual-disk propeller models may be compromised when drift pre-rotates the inflow |
| Search snippet only, unverified | A later Springer paper is reported to state that the standard formulation applies for drift angles below about 45 degrees |

Observation by the research note's author, not from a source: eq. 15 contains K_T / J^2, undefined at zero ship speed, and the inner square root becomes imaginary when K_T is sufficiently negative (windmilling or locked propeller). The engine-off and start-up regimes in this issue therefore need explicit branches.

## 5. Open benchmark hulls and test cases

- KVLCC2: complete coefficient set and propeller polynomial as above; free-running tests by a Dutch model basin (Lee et al. 2007; Quadvlieg and Brouwer 2011, as cited by Liu et al.).
- SIMMAN workshop series: KVLCC2 tanker, KCS container ship and ONRT surface combatant, compared with towing-tank data; sessions on captive tests and on trajectories (organiser's event page read). Data-access terms were not shown on that page; search snippets say registration is required and that earlier workshops released captive and free-running data including the 5415M hull (unverified).
- Southampton wind-tunnel programme: Wageningen B4.40-type propeller with all-movable and skeg rudders, including a bollard-condition report and a four-quadrant low-speed report (search-result descriptions only; repository refused access).
- Nienhuis 1987 rudder-behind-propeller tests, as summarised by Liu et al.

## 6. Assessment for a fast open implementation

| Criterion | Modular model (section 3) | Lifting line plus blade-element momentum | Panel or lifting surface | Viscous CFD |
|---|---|---|---|---|
| Cost per evaluation | Algebraic | Low (authors' claim) | Moderate, iterative | High |
| Inputs | Open-water thrust curve, wake and thrust deduction, six to eight interaction coefficients | Rudder and propeller geometry, thrust loading, separation | Full geometry | Full geometry and mesh |
| Open data to test against | KVLCC2 complete | Southampton reports (not opened) | Same | SIMMAN cases |
| Covers n = 0, bollard, astern | No, as written | Unverified | Unverified | Yes in principle |

Recommendation: primary method the modular model, verified first by reproducing the KVLCC2 numbers above; fallback outside its range a tabulated rudder-plus-propeller characteristic in thrust-loading form built from the Southampton bollard and four-quadrant reports once they are obtained; lifting line with blade-element momentum as the next step if spanwise loading or geometry sensitivity is needed. Data needed before coding: the primary MMG paper (to confirm symbols and the two unlabeled constants), a four-quadrant open-water curve for the chosen propeller, and the Southampton reports.

## 7. Sources

### Read (9)

- [R1] Liu, Quadvlieg and Hekkenberg 2016 (repository manuscript; sections 1, 4.2, 5.4 to 5.6 and Table 2)
- [R2] Soding Weinblum lecture (one page)
- [R3] Molland and Turnock 1996 (abstract record)
- [R4] Molland, Turnock and Wilson 1996 (abstract record)
- [R5] Turnock, Wellicome and Molland 1994 (abstract record)
- [R6] Carstensen 2019 (abstract record)
- [R7] Bulten 2024 (sections 4.5, 6, 7)
- [R8] Le Thuy Hang 2001 (abstract page)
- [R9] SIMMAN 2020 event page

### Located, not read (16)

- [L1] Yasukawa and Yoshimura 2015 (primary)
- [L2] Southampton Ship Science Reports 46, 48, 49, 52, 64 and 90 and the related RINA paper
- [L3] Phillips, Turnock and Furlong 2009
- [L4] Badoe, Phillips and Turnock
- [L5] a 2012 actuator-disk induction method and a 2024 review of rudder sub-models in International Shipbuilding Progress
- [L6] a Flanders Hydraulics four-quadrant rudder model for KCS (file opened, text not extractable)
- [L7] a 2024 model-basin four-quadrant rudder model
- [L8] Ito, Takashina and Yasukawa 2025
- [L9] a 2026 open-access four-quadrant paper
- [L10] Brix, Manoeuvring Technical Manual
- [L11] Soding 1981

Enumeration note: the printed count is 16; the top-level semicolon-separated groups number 11. Compound groups are retained as printed, without expansion or reconciliation.

## 8. Could not verify

- The primary MMG paper's own wording, symbols and stated limits.
- Any Holtrop-Mennen rudder correction; any closed-form Soding or Brix slipstream formula (only prose was readable).
- Content and data tables of the Southampton reports.
- Availability and licence of SIMMAN captive and free-running data.
- Accuracy figures for the lifting-line, panel and CFD methods beyond the qualitative abstract statements.

## Relation to the existing documents (differences listed, not resolved)

The following records comparable statements without deciding which is right. The existing documents and code are unchanged and have not been re-verified.

| Topic | Existing document statement and location | Research note record and status |
|---|---|---|
| Primary method | `propeller-rudder-method-selection.md`, “4. Primary Method Selected: Söding/Brix (SB)” selects Söding/Brix. | Section 6 recommends the modular model and KVLCC2 reproduction; primary MMG paper not opened [L1]. |
| Slipstream relation | `propeller-rudder-literature.md`, “1. Actuator-Disk / Momentum Theory” prints a distance-dependent axial velocity relation. | Section 3 reproduces longitudinal inflow (eq. 15) from Liu et al. [R1], attributed to Yasukawa and Yoshimura; primary not opened. |
| Söding/Brix formula recovery | `propeller-rudder-literature.md`, “3. Söding (1998) / Brix (1993)” prints closed-form interaction lift and drag relations. | Sections 3 and 8 record prose only for Soding and no verified closed-form Soding or Brix slipstream formula; equations not recoverable [R2], [L10], [L11]. |
| Normal-force relation | `propeller-rudder-literature.md`, “4. Molland & Turnock (2007)” prints a free-stream lift slope with an aspect-ratio correction. | Section 3 reproduces a normal-force coefficient (eq. 2), attributed to Fujii; Fujii and Tsuda, read in Liu et al. [R1]; primary not opened. |
| Operating range | `propeller-rudder-method-selection.md`, “4. Primary Method Selected: Söding/Brix (SB)”, “Rationale” claims coverage of drive-off startup and engine-off. | Sections 1 and 6 state that the modular model does not cover engine-off, start-up and bollard as written; separate treatment needed [R1]. The methods differ; no equivalence is asserted. |
| Experimental coverage | `propeller-rudder-method-selection.md`, “2.4 Molland & Turnock Empirical Corrections (MT)” states no low-speed / bollard data. | Sections 1 and 5 locate Southampton bollard and four-quadrant reports; reports not opened [L2]. |
| Accuracy evidence | `propeller-rudder-method-selection.md`, “3. Comparison Matrix” gives numerical accuracy estimates for candidate methods. | Section 8 could not verify accuracy figures for lifting-line, panel and CFD methods beyond qualitative abstract statements. |
| Holtrop-Mennen correction | `propeller-rudder-method-selection.md`, “2.2 Holtrop & Mennen (HM)” states that it is not a rudder interaction model. | Sections 1 and 8 state that no source read supports a Holtrop-Mennen rudder correction; that acceptance item has no located reference. |
