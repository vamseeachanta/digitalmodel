# OrcaFlex Modular Example Model Cards

Per-example engineering descriptions for representative OrcaFlex modular examples.
Each card covers structure type, key objects, analysis intent, environment, notable
features, and expected results. See [PARAMETER_REFERENCE.md](PARAMETER_REFERENCE.md)
for parameter-level detail across all 62 examples.

**49 categories | 62 variants | 14 letter groups (A–M, Z)**

---
## A01 — Flexible Riser

### A01 Catenary Riser

| Field | Value |
|---|---|
| **Structure type** | Single flexible riser, simple catenary hang-off from FPSO |
| **Water depth** | 200 m |
| **Analysis intent** | Static shape + dynamic effective tension and curvature under waves |

**Key objects**
- *Line type* — `10in Flexible Pipe`: OD = 355.6 mm, EI = 125 kN·m², EA = 711 kN, MBL = 4000 kN
- *Vessel* — FPSO at X = −35 m, Y = 0; drives hang-off end
- *Seabed* — Flat elastic; bottom end at seabed

**Environment** — Dean stream wave; Hs / Tp from `hs`/`tp` parameters; wave direction 180°.
**Lines** — 3 sections (90 + 20 + 20 m = 130 m); Catenary statics → Full statics.
**Notable features** — Simplest riser configuration; baseline for sensitivity studies;
`--static` flag in run script limits to static position only.
**Expected results** — Top tension ~ 80–120 kN; static sag depth well within 200 m WD.
**Run script** — `modular/A01/A01 Catenary riser/run_orcaflex.py`

---
### A01 Lazy Wave Riser

| Field | Value |
|---|---|
| **Structure type** | Flexible riser with distributed buoyancy arch section |
| **Analysis intent** | Compare distributed vs discrete buoyancy; verify arch uplift force |

**Key objects**
- *Line type (bare)* — `10in Flexible Pipe`: OD = 355.6 mm (same as catenary)
- *Line type (buoyed)* — `10in Flexible Pipe + Floats`: effective OD = 807 mm (floats as enlarged pipe)
- *Vessel* — FPSO at X = −35 m; same as catenary variant

**Environment** — Same Dean stream wave as catenary; `hs` / `tp` parametric.
**Lines** — 5 sections (50 + 30 + 50 + 40 + 20 m = 190 m); arch span across middle three sections.
**Notable features** — Distributed buoyancy (enlarged OD) vs discrete buoy approaches compared;
arch apex depth is the primary shape check; lower TDP tension than catenary.
**Expected results** — Arch apex near mid-water; buoyancy net uplift > 0 kN.
**Run script** — `modular/A01/A01 Lazy wave riser/run_orcaflex.py`

---
## B01 / B06 — Drilling & BOP Operations

### B01 Drilling Riser

| Field | Value |
|---|---|
| **Structure type** | Deepwater drilling riser with choke/kill lines and moonpool constraint |
| **Water depth** | 1020 m |
| **Analysis intent** | Dynamic stress and stroke in drilling riser stack under ocean current |

**Key objects**
- *5 line types* — `24" RISER` (main casing), `Slip Joint` (telescoping), `Drill Pipe`,
  `Choke & Kill` (auxiliary), `Moonpool Edge` (constraint guidance)
- *11 lines* — main riser + inner drill string + auxiliary lines + moonpool guides
- *Vessel* — Drillship at surface; all lines connect to vessel moonpool frame

**Environment** — Depth-varying current profile; no waves (current-only loading).
**Notable features** — Telescoping slip joint via two overlapping prescribed-constraint sections;
moonpool edge lines guide riser through hull opening; 5-component stack = multi-annulus architecture.
**Expected results** — Top tension within stroke allowable; auxiliary line tensions balanced.

### B06 Running BOP

**Structure:** BOP lowering on drill string | **WD:** 234 m | **Intent:** Dynamic loads during running.
**Environment** — JONSWAP Hs = 3 m, Tz = 5 s; `WaveTimeOrigin = −1935 s` burn-in to statistical stationarity.
**Notable features** — Long burn-in is critical for JONSWAP stationarity; reference example for
wave-time-origin methodology in batch direction sweeps.

---
## C06 — CALM Buoy

| Field | Value |
|---|---|
| **Structure type** | Catenary Anchor Leg Mooring buoy with loading hose and turret weathervaning |
| **Water depth** | 80 m |
| **Analysis intent** | Full system dynamics under combined wind, wave, current; weathervaning response |

**Key objects**
- *CALM buoy* — Central buoy body with turret; weathervanes freely under current
- *Mooring chains* — Catenary legs from buoy to seabed anchors
- *Loading hose* — Flexible hose connecting buoy to tanker
- *Constraints* — Turret articulation joint (zero yaw moment transfer)

**Environment** — 96 user-specified wave components (full frequency-spectrum irregular sea);
NPD wind spectrum at 12 m/s; 3-zone rotating current (surface / mid-water / seabed).
**Notable features** — User-specified components give exact sea-state control for code-check
calibration; NPD stochastic turbulence (vs constant wind elsewhere); 3-zone current rotation
captures stratified flow typical of CALM buoy exposed sites.
**Expected results** — Mooring peaks < MBL; hose curvature within allowable bend radius;
buoy offset bounded by chain catenary geometry.

---
## D02 — Pull-in Analysis

| Field | Value |
|---|---|
| **Structure type** | Subsea pipeline pull-in through guided installation route |
| **Analysis intent** | Quasi-static load distribution during pull-in operation |

**Key objects**
- *Pipe line* — Primary pipeline being pulled
- *Gateway 1, Gateway 2* — Constraint nodes defining the guided route
- *Seabed* — Flat elastic; stiffness = 500 kN/m/m

**Environment** — Zero-wave (Airy H = 0); no current. Installation is quasi-static; waves
suppressed to isolate structural contact loads.
**Lines** — Prescribed statics (tracked route) → Full statics; path defined by gateway positions.
**Notable features** — Seabed stiffness dominates contact/friction response; Prescribed initialises
pulled geometry then Full statics solves final state; represents D-series installation with no vessel
or wave loading needed.
**Expected results** — Contact loads at gateways within allowable; no free-span buckling.

---
## E01 — S-lay Pipelay

| Field | Value |
|---|---|
| **Structure type** | S-lay pipeline installation with articulated stinger |
| **Water depth** | 100 m |
| **Analysis intent** | Overbend and sagbend strain/curvature during pipe laying |

**Key objects**
- *Line type* — `250 mm OD × 15 mm WT` steel pipe
- *4-section line* — 120 + 205 + 50 + 220 m = 595 m total (stinger segments + free catenary)
- *Vessel* — Lay barge with stinger attachment points

**Environment** — JONSWAP Hs = 1 m, Tz = 4 s, wave direction = 135° (quartering seas).
**Notable features** — Quartering sea (135°) drives stinger lateral loads not seen in head-seas;
overbend (stinger) and sagbend (free catenary) are the critical curvature regions; compare E08
(PipelayConfig) for parameterised lay table sweeps.
**Expected results** — Overbend strain < 0.2%; sagbend curvature within DNV allowable; top tension
positive throughout storm record.

---
## F01 — Subsea Lowering Operations

Three variants cover progressively complex lowering physics:

| Variant | Structure | Hs | Key object count | Key feature |
|---|---|---|---|---|
| Lowered cone | Single rigid buoy on crane wire | 0 (quiescent) | Vessel + buoy + winch; no lines | Baseline added-mass; winch-only vertical drive |
| Manifold block | 3×3×3 m cubic rigid frame | 1.5 m ISSC | 30 Frame members + 1 Crane Wire | Distributed Morison loading on frame skeleton |
| Trapped water | Spreader bar with trapped mass | JONSWAP, beam seas | 4 Crane Wires + trapped-water buoy | Variable added mass as free surface crossed; 3 stages |

**Lowered cone** — Hs = 0; current = 0; pure inertia and gravity. Establishes baseline Cd/Cm
coefficients without hydrodynamic complexity.

**Manifold block** — ISSC Hs = 1.5 m (contrast with JONSWAP elsewhere); 30 Frame members
model structural stiffness with correct distributed drag; cubic Shapes define hydrodynamic area.

**Trapped water** — JONSWAP dir = 90° (beam seas), current = 1 m/s; 3 simulation stages: free-
hanging → splash zone → submerged. Trapped water mass varies as free surface moves through structure;
spreader bar via 4 lines distributes crane load.

---
## G–J — Deployment & Operational Groups (Summary)

| Group | Example | Structure type | Key feature |
|---|---|---|---|
| **G04** | Anchor-last deployment | Single-point mooring buoy | 500 m WD; 1 wire catenary (350 + 100 m); calm sea; seabed friction 0.5; anchor-last sequence |
| **H01** | Chinese lantern | CALM buoy + 12 chains + 2 risers | Shallow 28 m WD; Dean stream Hs = 8 m; risers with / without torsion comparison; clump weights |
| **I01** | Seismic streamer array | Towed dual-array seismic spread | Ship + 8 lines (streamers + lead-ins + gun umbilical); diverter ballast; 100 m WD; 2 m / 6 s seas |
| **J01** | Deployment with submarine | Sub-deployed sensor mooring | Submarine vessel; 2 sensor lines + 1 baseline wire; 250 m WD; calm; 5-stage deployment |

---
## K01 — Spar FOWT

| Field | Value |
|---|---|
| **Structure type** | Spar-type Floating Offshore Wind Turbine with delta mooring |
| **Water depth** | 320 m |
| **Analysis intent** | Coupled aero-hydrodynamic response; mooring pre-tension; fatigue motions |

**Key objects**
- *3 line types* — `Tower` (spar column), `Mooring` (875 m catenary/leg), `Delta` (30 m legs)
- *Mooring* — 3 × (2 × 30 m delta legs + 1 × 875 m catenary) = 9 lines total
- *Nacelle* — 240 t top mass; NREL 5 MW reference turbine
- *Turbine* — OrcaFlex turbine object with blade aerofoil tables; `GenPower.py` UDR for generator control
- *Stages* — 250 s ramp + 250 s analysis

**Environment** — JONSWAP (Hs / Tp from parameters); stochastic turbine loading via Python UDR.
**Notable features** — Delta mooring prevents spar yaw (splits catenary into 2-leg fan); external
`GenPower.py` UDR applies generator torque in real-time; 2-stage simulation: ramp then record;
`ORCAFLEX_API_PATH` env var required to locate OrcFxAPI.
**Expected results** — Surge offset < 5% WD; all mooring legs > 50 kN; tower top < 5% hub height.
**Run script** — `modular/K01/K01 5MW spar FOWT/run_orcaflex.py`

---
## L01 / L03 — Vessels

### L01 Default Vessel

| Field | Value |
|---|---|
| **Structure type** | Generic offshore vessel, RAO-driven passive motion |
| **Water depth** | 100 m |
| **Analysis intent** | Baseline 6-DOF motion response; no lines attached |

**Key objects** — Single vessel: 103 m length, 9018 t displacement; full 6-DOF RAO tables;
no lines or moorings.
**Environment** — Dean stream wave; `hs` / `tp` / `wave_direction` parametric.
**Notable features** — RAO tables drive vessel directly (passive model — no mooring restoring
forces); building block for A-series riser examples that attach lines to this vessel type.
**Expected results** — Heave RAO peak > 1.0 m/m at natural period; draft within ± 5% of spec.
**Run script** — `modular/L01/L01 Default vessel/run_orcaflex.py`

### L03 Semi-submersible

| Field | Value |
|---|---|
| **Structure type** | Column-stabilised semi-sub with catenary moorings and rigid structural frame |
| **Water depth** | 200 m |
| **Analysis intent** | Semi-sub motions and mooring tensions under JONSWAP irregular seas |

**Key objects**
- *3 mooring lines* — Catenary chains from pontoon fairleads to seabed
- *15 structural lines* — Upper/lower pontoons + cross-braces; `Infinity` bending stiffness = rigid
- *Vessel* — Semi-sub hull with heave plates

**Environment** — JONSWAP; 200 wave components; 200 m WD.
**Notable features** — Rigid structural lines (Infinity EI) model frame topology without separate
Frame objects; 18 total lines (3 functional + 15 structural); high component count useful for
batch-mode performance benchmarking.
**Expected results** — Heave and pitch within operability limits; mooring symmetric under head seas.

---
## M01 — Subsea Pipeline

### M01 Lateral Buckling

| Field | Value |
|---|---|
| **Structure type** | Thermally-loaded subsea pipeline on profiled seabed |
| **Analysis intent** | Lateral buckle initiation and post-buckle shape under thermal expansion |

**Key objects**
- *Profile seabed* — 60-point cubic spline (50 m spacing = 3000 m seabed profile)
- *Pipeline* — 1000 m; `IncludeTorsion = Yes`; thermal expansion table drives axial force
- *Current* — Power-law depth-varying profile
- *External contents file* — `contents.txt` defines temperature/pressure gradient along pipe

**Environment** — Hs = 0 (current-only, operational loading); power-law current.
**Notable features** — Out-of-flatness seabed triggers buckle initiation; `IncludeTorsion = Yes`
captures torsion-bending coupling; external contents file models thermal gradient along length.
**Expected results** — Buckle forms at seabed high-point; post-buckle lateral displacement measurable;
axial feed-in symmetric about buckle crown.

### M01 Pipeline Walking

**Structure:** Long pipeline (3000 m) susceptible to ratcheting axial walk | **Intent:** Cumulative
axial displacement over multiple thermal cycles.

Same seabed profile and thermal loading as lateral buckling variant. Key differences:
- **Pipe length** — 3000 m vs 1000 m; 5 m segment discretisation
- **End B** — Free (no anchor); allows axial ratcheting to accumulate

**Expected results** — Net walk per thermal cycle > 0; walk rate ∝ seabed friction × ΔT.

---
## Z02 — In-air Edge Case (Ropeway / Line Contact)

| Field | Value |
|---|---|
| **Structure type** | Above-water ropeway — line-on-line slide contact mechanics |
| **Analysis intent** | Contact forces and sag in a non-marine (in-air) line system |

**Key objects**
- *10 lines* — Ropeway carriers + guide tube + frame supports
- *Water density* — 0.00128 t/m³ (air density; not seawater)
- *SeaHidden* — `Yes` (sea surface suppressed in 3-D view)
- *Line contact* — `Line on line slide` contact model between carrier and guide tube

**Environment** — No waves; no current. Gravity + air drag only.
**Notable features** — Only example with air density (validates in-air cable mechanics where
buoyancy = zero); `SeaHidden: Yes` suppresses sea rendering; line-on-line slide applies to
umbilical rollers, crane sheaves, and pipe-in-pipe systems.
**Expected results** — Carrier sag by gravity only; contact forces ∝ line weight × angle;
zero buoyancy contribution confirms correct density assignment.

---
## All Letter Groups — Coverage Reference

| Group | Categories | Example name(s) | Model class | Detail |
|---|---|---|---|---|
| **A** | A01–A06 | Catenary riser, Lazy wave, Steel catenary, Top-tensioned, FPS units | Flexible / rigid risers | A01 above |
| **B** | B01, B06 | Drilling riser, Running BOP | Drilling / BOP operations | B01+B06 above |
| **C** | C03–C10 | CALM buoy, lazy-S riser, multiple statics, direction sweep | Moorings & parametric | C06 above |
| **D** | D02–D04 | Pull-in, A&R, rigid spool | Installation operations | D02 above |
| **E** | E01–E08 | S-lay, J-lay, reel-lay, lay table sweep | Pipelay installation | E01 above |
| **F** | F01–F07 | Lowered cone, manifold, trapped water, crane+rigging | Subsea lowering | F01 variants above |
| **G** | G04 | Anchor-last deployment | Mooring deployment | Summary above |
| **H** | H01–H03 | Chinese lantern, VIV suppression, helical strake | Vortex / contact | Summary above |
| **I** | I01 | Seismic streamer array | Towed systems | Summary above |
| **J** | J01 | Deployment with submarine | Submarine / AUV | Summary above |
| **K** | K01–K06 | Spar FOWT, TLP, semi-FOWT variants | Floating wind | K01 above |
| **L** | L01–L05 | Default vessel, semi-sub, FPSO, drillship | Vessel types | L01+L03 above |
| **M** | M01 | Lateral buckling, pipeline walking | Pipelines on seabed | M01 variants above |
| **Z** | Z02, Z09 | In-air ropeway, sub-sea to surface extreme | Edge cases | Z02 above |

---
## Cross-References

| Document | Purpose |
|---|---|
| [PARAMETER_REFERENCE.md](PARAMETER_REFERENCE.md) | All `inputs/parameters.yml` keys, units, OrcFxAPI paths, observed values |
| `modular/A01/A01 Catenary riser/run_orcaflex.py` | Run script: static + dynamic tension / curvature |
| `modular/A01/A01 Lazy wave riser/run_orcaflex.py` | Run script: arch apex depth, buoyancy check |
| `modular/K01/K01 5MW spar FOWT/run_orcaflex.py` | Run script: spar offset, fairlead tensions |
| `modular/L01/L01 Default vessel/run_orcaflex.py` | Run script: 6-DOF time histories |
| `modular/C10/C10 Multiple statics/run_orcaflex.py` | Run script: wave direction sweep CSV |
| `modular/E08/PipelayConfig/run_orcaflex.py` | Run script: lay table sweep, overbend/sagbend CSV |
| `qa/orcaflex_example_qa.py` | Physics-based QA checks for R01–R06 examples |
| `../../../src/digitalmodel/orcaflex/reporting/` | HTML report generator (WRK-327) |
