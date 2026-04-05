# GTM Demo 3: Deepwater Mudmat Installation — Detailed Requirements

## Issue: #1872

## 1. PURPOSE

Demonstrate ACE Engineer's deepwater installation analysis capability by
running a parametric comparison of 2 vessels × 3 mudmat sizes × 6 water depths.
The hero deliverable is a Go/No-Go heatmap that instantly answers:
"Can my vessel install this structure at this water depth?"

## 2. PARAMETER MATRIX (36 cases)

| Dimension       | Values                                          | Count |
|-----------------|------------------------------------------------|-------|
| Vessels         | Large CSV (5000te), Medium CSV (2500te)        | 2     |
| Water Depths    | 500, 1000, 1500, 2000, 2500, 3000 m           | 6     |
| Mudmat Sizes    | 50te (6×6m), 100te (8×10m), 200te (12×14m)    | 3     |
| **Total**       |                                                | **36**|

## 3. ENGINEERING METHODOLOGY — 5 INSTALLATION PHASES

Each of the 36 cases evaluates ALL 5 phases. A case is GO only if ALL phases pass.

### Phase 1: Lift-off
- **Calculation**: Hook load = mass_air × g × DAF_lift
  - DAF_lift = 1.10 (sheltered waters, DNV-ST-N001)
- **Check**: Hook load ≤ crane SWL at operating radius
- **Crane capacity**: Interpolate from crane_capacity_curve in vessel data
- **Utilisation**: hook_load / crane_SWL_at_radius

### Phase 2: In-air transit
- **Calculation**: Tilt angle from CoG offset + dynamic loads
  - Tilt check: Is CoG directly below hook? (sling geometry check)
- **Check**: Tilt ≤ 5° (typical acceptance criterion)
- **Note**: Simplified — assume 4-point lift with spreader beam per rigging data

### Phase 3: Splash zone (CRITICAL — most cases fail here)
- **Slamming force**: F_slam = 0.5 × ρ × C_s × A_p × v_rel²
  - v_rel = v_lowering + v_wave (DNV-RP-H103 §4.3.4)
  - v_lowering = hoist_speed from vessel data (convert m/min → m/s)
  - v_wave = π × Hs / Tp (simplified wave particle velocity at surface)
  - C_s = from mudmat data (5.0 for flat bottom)
  - A_p = bottom projected area from mudmat data
- **Varying buoyancy**: F_var = ρ × g × A_wp × (Hs/2)
- **Max hook load in splash**: W_air + F_slam + F_var + F_drag
  - F_drag = 0.5 × ρ × C_D × A_side × v_lowering²
- **DAF_splash**: Typical 1.3 (DNV-RP-H103 guidance)
- **Check**: Max hook load × DAF_splash ≤ crane SWL

### Phase 4: Lowering through water column
- **Cable tension**: T = W_sub + W_cable_sub(depth)
  - W_sub = (mass_air - ρ_w × V_displaced) × g
  - W_cable_sub = cable unit weight × depth
  - Cable unit weight: estimate from wire diameter → π/4 × d² × (ρ_steel - ρ_w) × g
- **Snap load risk**: Check if cable tension > 0 during heave cycle
  - Heave amplitude at crane tip: Hs × heave_RAO / 2
  - Dynamic cable load: (mass_structure + added_mass) × ω² × heave_amp
  - ω = 2π / Tp
- **Check**: Max cable tension ≤ 0.85 × wire MBL (safety factor)

### Phase 5: Landing
- **Bearing pressure**: p = (W_sub) / A_base
  - A_base = mudmat bottom projected area
- **Check**: p ≤ 50 kPa (typical soft clay seabed allowable)
- **Note**: This phase is almost never governing for mudmats

## 4. GO/NO-GO LOGIC

```
case_result = "GO" if ALL phases pass
           = "MARGINAL" if max utilisation in [0.85, 1.00]
           = "NO_GO" if any phase fails
```

Governing phase = the phase with highest utilisation ratio.

## 5. CHARTS (5 interactive Plotly figures)

### Chart 1: Go/No-Go Heatmap (HERO CHART)
- X-axis: Water depth (500–3000m)
- Y-axis: Mudmat size (50te, 100te, 200te)
- Two subplots side-by-side: Large CSV | Medium CSV
- Cell color: Green=GO, Yellow=MARGINAL, Red=NO_GO
- Cell annotation: Governing phase name + utilisation %

### Chart 2: Crane Utilisation vs Water Depth
- X-axis: Water depth
- Y-axis: Crane utilisation (%)
- Lines: One per mudmat size
- Two traces groups: Large CSV (solid) vs Medium CSV (dashed)
- Red dashed line at 100% (capacity limit)

### Chart 3: DAF vs Water Depth
- X-axis: Water depth
- Y-axis: DAF (dimensionless)
- Two phases shown: Splash zone DAF + Lowering DAF
- Parametric by mudmat size
- Shows that DAF increases with depth for lowering phase

### Chart 4: Max Hs Limit vs Structure Weight
- X-axis: Structure weight (te)
- Y-axis: Max allowable Hs (m)
- One curve per vessel
- Shows the operability envelope — above the curve, no-go

### Chart 5: Vessel Head-to-Head Comparison
- Grouped bar chart
- X-axis: Water depth
- Groups: Large CSV vs Medium CSV
- Y-axis: Number of structures installable (0, 1, 2, or 3)
- Or: Max structure weight installable at each depth

## 6. EXISTING MODULE USAGE

### Direct reuse from marine_ops/installation/:
| Module              | What to use                                           |
|---------------------|-------------------------------------------------------|
| models.py           | Structure, Vessel, CraneCurve, InstallationCriteria   |
| splash_zone.py      | slamming_force(), varying_buoyancy_force()             |
| crane_tip_motion.py | crane_tip_raos() if we have full RAOs                 |
| operability.py      | hs_limit_for_criterion(), compute_operability()        |
| vessel_screening.py | screen_vessels() for multi-vessel comparison           |

### New code needed in the demo script:
1. **JSON data loaders** — parse csv_hlv_vessels.json and mudmat_structures.json
2. **Phase calculators** — 5 functions, one per installation phase
3. **Parametric runner** — iterate 36 cases, collect results
4. **Chart builders** — 5 Plotly chart functions
5. **Results serialiser** — dump to demo_03_mudmat_installation_results.json

### IMPORTANT DECISION: Use existing modules or self-contained?
The existing installation modules (models.py, splash_zone.py etc.) use
dataclass-based APIs with full RAO data. The vessel JSON has simplified RAOs
(peak values only, not frequency-dependent).

**RECOMMENDED APPROACH**: Self-contained calculations in the demo script
(like Demo 2 does), because:
1. The simplified RAO data doesn't fit the full-RAO API
2. Demo scripts should be readable stand-alone for prospects
3. Can reference module patterns without requiring exact API compatibility
4. Later, upgrade data to full RAOs and wire to modules

## 7. OUTPUT FILES

- `output/demo_03_mudmat_installation_report.html` — branded report
- `results/demo_03_mudmat_installation_results.json` — full cached results
- Updates to `results/vessel_comparison_matrix.json`
- Updates to `results/structure_comparison_matrix.json`

## 8. SCRIPT STRUCTURE (following Demo 2 pattern)

```
demo_03_deepwater_mudmat_installation.py (~800-1000 lines)
├── Imports (numpy, pandas, plotly, report_template)
├── Constants (densities, safety factors, criteria)
├── Data Loaders (read vessel + mudmat JSON)
├── Phase Calculators (5 functions)
│   ├── calc_liftoff(vessel, structure)
│   ├── calc_in_air(vessel, structure)
│   ├── calc_splash_zone(vessel, structure, Hs, Tp)
│   ├── calc_lowering(vessel, structure, depth)
│   └── calc_landing(structure)
├── Parametric Runner (iterate matrix, collect results)
├── Chart Builders (5 functions, each returns go.Figure)
├── Report Assembly (GTMReportBuilder)
├── Results Export (JSON dump)
└── main() with --from-cache support
```

## 9. HAND-HOLD CHECKPOINTS

Before building, you should review and confirm:

[ ] Phase 3 (splash zone) methodology — is the simplified wave velocity OK?
[ ] Phase 4 (lowering) — cable weight formula and snap load check
[ ] Go/No-Go thresholds — utilisation ratios for MARGINAL vs NO_GO
[ ] Chart 1 layout — two side-by-side heatmaps or single with vessel selector?
[ ] Bearing pressure limit — 50 kPa for soft clay, or parametric soil?
[ ] DAF values — 1.10 for lift-off, 1.30 for splash zone, what for lowering?
[ ] Should we use Hs = 2.0m as the reference sea state for all cases?
    Or sweep Hs too? (Would change matrix from 36 to 36×N cases)
