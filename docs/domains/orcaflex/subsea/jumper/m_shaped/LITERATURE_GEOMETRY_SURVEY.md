# M-Shaped Rigid Subsea Jumper — Published Geometry Survey

> Literature sweep performed 2026-06-12 (AI-assisted web research, hand-curated).
> Purpose: source geometries with published dimensions sufficient to build
> OrcaFlex model-library entries, plus measured/computed dynamic responses to
> serve as validation targets. Tracking issue:
> [#722](https://github.com/vamseeachanta/digitalmodel/issues/722).
>
> **Rule applied:** dimensions below are only those explicitly stated in the
> cited source unless marked *(inferred)*. Do not build specs from inferred
> values without recording the assumption in the spec itself.

## Summary comparison

| # | Case | Bottom span / total | Inner leg height | OD | WT | Scale | Context | Geometry completeness |
|---|------|--------------------|------------------|-----|-----|-------|---------|----------------------|
| 1 | ExxonMobil VIV model test (Wang/Zheng/Igeh) | H2 = 4.327 m / 13.96 m total | 2.32 m | 60.5 mm | 2.77 mm | model of 10″ FS jumper | towing tank, 0.05–1.24 m/s | **Full** (corner radii not published) |
| 2 | Zhu et al. 2022 | L4 = 10 m | 6 m | 270 mm | 30 mm | full-scale (numerical) | gas–oil production, 6 MPa | **Full** (verify BCs) |
| 3 | Li et al. JMSE 2024 | L4 = 2 m / 3.744 m total | 1.0 m | 60 mm | 6 mm | lab | air–water + 0.5–1.0 m/s current | **Full** (segment map via Fig. 1) |
| 4 | Li et al. Appl. Sci. 2023 | – / 3.6 m total | n/p | 52 mm (?) | 4 mm (?) | lab | air–water slug rig | Partial (no segment lengths; OD/WT inconsistent) |
| 5 | Yurishchev et al. 2023/2024 | 70D = 3.5 m | 40D / 70D | ID 50 mm | n/p (Perspex) | lab + scaled family | gas start-up purge | Flow path full; **no structural data** |
| 6 | Holmes & Constantinides 2010 | ~30 m *(inferred)* | ~5–7 m *(inferred)* | 167 mm (+600 mm buoy) | n/p | full-scale (CFD) | seabed current 0.1–0.5 m/s | Partial (figure-scaled) |

**Build order recommendation:** Case 1 → Case 2 → Case 3 (the only three with
explicit dimension chains). Cases 4–6 are validation companions / load-case
references only.

---

## Case 1 — ExxonMobil M-jumper VIV model test (industry benchmark)

**Sources**

- Wang, H.; Huang, J.; Lee, S.; Gioielli, P.; Kan, W.; Spencer, D.; Islam, M.,
  "VIV Response of a Subsea Jumper in Uniform Current," OMAE2013, Nantes,
  V04BT04A043. <https://asmedigitalcollection.asme.org/OMAE/proceedings/OMAE2013/55379/V04BT04A043/270608>
- Zheng, H.N.; Slocum, S.T.; Huang, J.Z.; Srivastava, V.; Lee, S.; Wang, H.H.,
  "Numerical Analysis of Experimental Data of Subsea Jumper Vortex Induced
  Vibrations," OMAE2015, St. John's.
- Igeh, L.A., *VIV Fatigue Investigation for Subsea Planar Rigid Spools and
  Jumpers*, MSc thesis, University of Stavanger, 2017 (CC-BY). **Complete
  dimension tables (Tables 1–4, pp. 54–58).**
  <https://core.ac.uk/download/249952931.pdf>
- Companion analyses: Liu/Igeh/Wu/Ong, *JOMAE* 142(1):011602, 2020
  <https://asmedigitalcollection.asme.org/offshoremechanics/article-abstract/142/1/011602/955900>;
  Qu et al., *Ocean Eng.* 243 (2022).

**Configuration** (model scale, planar M; 7 welded segments, ends fully
clamped — force dynamometers at both ends in test). Segment chain, Igeh
Table 1 (explicit):

| Segment | V1 | H1 | V2 | H2 | V3 | H3 | V4 |
|---------|------|------|------|------|------|------|------|
| Length (m) | 1.495 | 1.000 | 2.323 | 4.327 | 2.326 | 1.000 | 1.495 |

Total length 13.96 m. V = vertical, H = horizontal: outer legs V1/V4, short
shoulders H1/H3, tall inner legs V2/V3, long bottom span H2 — classic M.

**Cross-section:** OD 0.0605 m, ID 0.055 m, WT 2.77 mm, aluminium
(ρ = 2700 kg/m³, E = 6.90e10 Pa, EI = 1.44e4 N·m², GJ = 1.08e4 N·m²).

**Mass/contents:** internal fluid SG 1.4 plus distributed lead → mass ratio
2.33; equivalent internal-fluid density used in the VIVANA FEM:
2328.45 kg/m³. Drag diameter 0.0605 m, Cd = 1.1, Ca = 1.0.

**Test context:** 200 m towing tank; tow speeds 0.05–1.241 m/s at
10°/45°/90° to the jumper plane; bare and straked variants; 13 accelerometers
+ 3 strain gauges. Base case is a full-scale 10″ OD M-shaped production
jumper (scale ≈ 1:4.5 if 10.75″ OD — *(inferred; only "10-inch" stated)*).

**Validation targets:** measured natural frequencies 0.863 / 2.149 / 2.194 /
2.542 Hz (modes 1–4); mode 3 first excited at U ≈ 0.18 m/s; VIVANA +
DNV-RP-F105 response and fatigue comparisons in the Igeh/Liu papers.

**Gaps:** corner bend radii not published (FEM used sharp corners); full-scale
base-case dimensions beyond "10-inch OD" not published.

---

## Case 2 — Full-scale numerical M-jumper, gas–oil slug FIV (Zhu et al. 2022)

**Source:** Zhu, H.; Hu, Y.; Tang, T.; Ji, C.; Zhou, T., "Evolution of
Gas-Liquid Two-Phase Flow in an M-Shaped Jumper and the Resultant
Flow-Induced Vibration Response," *Processes* 10(10):2133, 2022 (open
access). <https://www.mdpi.com/2227-9717/10/10/2133>

**Configuration** (full-scale, planar M, 6 bends; Table 1 of paper, explicit):
inlet/outlet verticals L1 = 4 m; top horizontals L2 = 3 m; inner verticals
L3 = 6 m each; bottom horizontal L4 = 10 m; bend radius R/D = 2
(D = 0.27 m → R = 0.54 m).

**Cross-section:** OD 0.27 m, ID 0.21 m, WT 30 mm; E = 2.068e11 Pa,
ν = 0.303 (steel). Near-identical pipe size to the Ballymore 10.75″
(0.273 m) jumpers — plausible full-scale realization of Case 1's 10″ base
case.

**Contents:** natural gas–oil multiphase at 6 MPa back pressure (subsea
production conditions). Dry/wet weights not tabulated — gap.

**Validation targets:** fundamental out-of-plane frequency ≈ 1.464 Hz; FIV
amplitudes O(10⁻³ D); response locked to 2.5 Hz inflow slug frequency; max
structural stress at Bend 5, max pressure stress at Bend 3.

**Gaps:** end boundary conditions and external-flow data to be verified
against the open-access paper before model freeze; no weight table.

---

## Case 3 — Lab-scale M-jumper, combined internal + external flow (Li et al. 2024)

**Source:** Li, G.; Li, W.; Lin, S.; Han, F.; Zhou, X. (Dalian Maritime
Univ.), "Dynamic Response Analysis of a Subsea Rigid M-Shaped Jumper under
Combined Internal and External Flows," *J. Mar. Sci. Eng.* 12(8):1261, 2024
(open access). <https://www.mdpi.com/2077-1312/12/8/1261>

**Configuration:** vertical overhang connected to equipment at both ends, no
other supports; left–right symmetric; 8 bends, 3 horizontal + 4 vertical
segments. Table 1 (verbatim): total length 3744 mm; L1 = 800, L2 = 800,
L3 = 1000, L4 = 2000 mm; bend radius 72 mm. **Mapping of L1–L4 to specific
segments is defined by the paper's Figure 1 — consult the figure when
building the centerline.**

**Cross-section:** OD 60 mm, ID 48 mm, WT 6 mm; steel ρ = 7850 kg/m³,
E = 2.1e11 Pa, ν = 0.3; structural damping 1.2 % (Rayleigh a = 1.160,
b = 4.368e-5).

**Contents/flows:** internal air–water stratified flow 0.02–0.90 m/s (water
fractions 0.3–0.7); external uniform current 0.50 / 1.00 m/s normal to the
jumper plane (Re 30k / 60k).

**Validation targets:** 13 modes < 100 Hz; mode 1 = 8.520 Hz (out-of-plane),
mode 2 = 13.825 Hz (in-plane), mode 3 = 17.473 Hz, mode 4 = 20.628 Hz, …
mode 13 = 78.933 Hz. Displacement dominated by external flow,
vibration/stress by internal flow; fatigue at worst bend; superposition of
separate responses ≈ combined response.

---

## Case 4 — Lab M-jumper slug FIV experiment (Li, Li, Yin, Ong 2023) — *partial*

**Source:** Li, W.; Li, J.; Yin, G.; Ong, M.C., "Experimental and Numerical
Study on the Slug Characteristics and Flow-Induced Vibration of a Subsea
Rigid M-Shaped Jumper," *Appl. Sci.* 13(13):7504, 2023 (open access).
<https://www.mdpi.com/2076-3417/13/13/7504>

2 ascending + 3 horizontal + 2 descending portions, 6 elbows, curvature
radius 1.5 D; total length 3.6 m; both ends fixed. Cross-section stated as
"diameter of 52 mm, an inner diameter of 48 mm, and a wall thickness of
4 mm" — **internally inconsistent** (48 + 2×4 = 56 ≠ 52). The same group's
2024 rig (Case 3) uses OD 60 / ID 48 / WT 6; ID 48 mm is the consistent
number. Steel, ρ = 7850 kg/m³, E = 2.06e5 MPa, ν = 0.3.

Natural frequencies — empty: 7.44 / 10.39 / 12.58 / 16.68 Hz; water-filled:
5.89 / 7.56 / 9.42 / 13.67 Hz. Resonance when slug frequency ≈ natural
frequency; CFD slug velocity within 10 % of test.

**Do not build a spec from this case** (segment lengths unpublished, OD/WT
contradiction). Use as a slug-FIV validation companion to Case 3.

---

## Case 5 — Tel Aviv Univ. gas-purge benchmark (Yurishchev et al.) — *load-case reference*

**Sources:**

- Yurishchev, A.; Brauner, N.; Ullmann, A., "Transient gas-liquid flow
  phenomena in M-shaped jumper of subsea gas production systems during
  start-up operation," *Int. J. Multiphase Flow* (2023).
  Preprint: <https://arxiv.org/abs/2306.08334>
- Yurishchev, A. et al., "Modeling of high-pressure transient gas-liquid flow
  in M-shaped jumpers of subsea gas production systems," *Int. J. Multiphase
  Flow* (2024). Preprint: <https://arxiv.org/abs/2403.14463>

Perspex rig, ID D = 50 mm; elbows R = 3D (150 mm); lower horizontal 70D
(3.5 m); downcomer 40D (2.0 m); riser 70D (3.5 m). The 2024 paper scales the
identical geometry by D (51–200 mm) for methane at 1–5+ atm with
water/MEG50/MEG100 (properties tabulated). Outputs: critical gas purge
velocity, transient pressure peaks, elbow impact forces, dominant
force/pressure frequencies; OpenFOAM VOF validation + mechanistic model.

**No structural data** (rigid rig) — use as an internal slug/purge load-case
benchmark, not a structural spec.

---

## Case 6 — Full-scale serpentine jumper CFD VIV (Holmes & Constantinides 2010) — *partial*

**Source:** Holmes, S.; Constantinides, Y., "Vortex Induced Vibration
Analysis of a Complex Subsea Jumper," OMAE2010-20520, Shanghai, pp. 671–678.
<https://asmedigitalcollection.asme.org/OMAE/proceedings-abstract/OMAE2010/49149/671/349847>

7 straight welded sections forming a planar serpentine; span "slightly more
than 30 m"; lowermost horizontal ≈ 3 m above seabed; end legs ≈ 5–7 m tall
*(scaled from Figure 2 — inferred)*; mid-span buoyancy module with helical
strakes. Jumper OD 0.167 m, steel, effective density 7849 kg/m³ (≈ 3×
displaced mass incl. contents); buoyancy section OD 0.6 m, syntactic foam
509 kg/m³. **Wall thickness and bend radii not published.**

Validation targets: 20 in-air modes tabulated, 0.366 Hz (mode 1) to
15.77 Hz (mode 20) with generalized masses; added mass (Ca = 1) shifts range
to 0.27–13 Hz; as-installed response < 1 % D (STD); bare-jumper mode-2
lock-in at 0.5 m/s near 0.69 Hz. Currents 0.1–0.5 m/s normal to plane;
AcuSolve DES fully coupled FSI.

---

## Adjacent (not M-shaped, noted for library context)

Deka, D.; Cerkovnik, M.; Panicker, N.; Achanta, V., "Subsea Jumpers
Vibration Assessment," OMAE2013-11011
(<https://www.academia.edu/53808967/Subsea_Jumpers_Vibration_Assessment>) —
multi-planar jumper, OD 508 mm, WT 11 mm, modes 0.35/0.48/0.69/1.09 Hz,
SHEAR7 vs DNV-RP-F105 (0.11 vs 0.78 A/D), slugging fatigue lives 23/35 days
unfactored, TMD mitigation ×8.
