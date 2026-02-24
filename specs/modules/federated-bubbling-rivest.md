# Post-Processing Plan: 900kN and 1250kN Pipeline Installation Models

## Overview
Post-process OrcaFlex simulation results for the newly created 900kN and 1250kN tension models, extracting standard quantities and generating reports.

## Input Files
**Current Location:** `docs/modules/orcaflex/pipeline/installation/floating/24in_pipeline/monolithic/runs/sim/`

| Tension | Simulation Files |
|---------|------------------|
| 900 kN | `900kN_env_001yr_090deg.sim`, `900kN_env_001yr_135deg.sim`, `900kN_env_95NE_090deg.sim`, `900kN_env_95NE_135deg.sim` |
| 1250 kN | `1250kN_env_001yr_090deg.sim`, `1250kN_env_001yr_135deg.sim`, `1250kN_env_95NE_090deg.sim`, `1250kN_env_95NE_135deg.sim` |

## Quantities to Extract

### Static Analysis
- End A Effective Tension (kN)
- End B Effective Tension (kN)

### Dynamic Analysis (LatestWave Period)
- End A Effective Tension - Min/Max (kN)
- End B Effective Tension - Min/Max (kN)
- Global Y Position - Min/Max (m)
- Direct Tensile Strain - Max (%)
- Max Bending Stress (MPa)

### Calculated Metrics
- Bending Utilization (vs X65 yield: 448.2 MPa)
- Combined Utilization (bend + tensile stress)
- Pass/Fail Status

### Arc-Length Distributed Values (NEW)
Export RangeGraph data along pipeline length for each simulation:
- **Arc Length** (m) - X-axis for all distributions
- **Effective Tension** (kN) - Min/Max/Mean along length
- **Global Y Position** (m) - Min/Max along length
- **Direct Tensile Strain** (%) - Min/Max along length
- **Max Bending Stress** (MPa) - Min/Max along length
- **Direct Tensile Stress** (MPa) - Min/Max along length

Output format: CSV files per simulation with columns for arc length and each quantity

## Implementation Steps

1. **Fix sim file location** (Line 11-12 in generate_html_report.py)
   - Current: `runs_dir.glob('*.sim')` looks in `runs/`
   - New sims are in `runs/sim/` subdirectory
   - Change to: `(runs_dir / 'sim').glob('*.sim')` for explicit path

2. **Add RangeGraph extraction** (add after line 31)
   - Extract arc length: `rg.X` property
   - Extract Min/Max/Mean: `rg.Min`, `rg.Max`, `rg.Mean`
   - Add Effective Tension range graph
   - Save per-simulation CSV with all distributions to `postproc/rangegraphs/`

3. **Add arc-length chart generation functions**
   - `create_rangegraph_chart(df, var, title, y_label, xlim=None)` - base chart function
   - `create_comparison_page(data, group_by, group_value)` - generates HTML page
   - Support full-length and critical zone (0-300m) views

4. **Generate grouped chart pages**
   - Loop through load conditions → generate `charts_env_*.html`
   - Loop through tensions → generate `charts_tension_*.html`
   - Each page contains 5 chart types × 2 views (full + critical zone)

5. **Run Post-Processing**
   ```bash
   cd docs/modules/orcaflex/pipeline/installation/floating/24in_pipeline/monolithic/postproc
   uv run python generate_html_report.py
   ```

## Output Files

### Summary Files
- `results_summary_latestwave.csv` - 20 rows (5 tensions × 4 environments)
- `results_interactive.html` - Interactive charts with all 5 tension levels

### Arc-Length Distribution Files (NEW)
One CSV per simulation in `postproc/rangegraphs/`:
- `900kN_env_001yr_090deg_rangegraph.csv`
- ... (20 files total)

Columns: `ArcLength_m, Tension_Min_kN, Tension_Max_kN, Tension_Mean_kN, Y_Min_m, Y_Max_m, Strain_Min_pct, Strain_Max_pct, BendStress_Min_MPa, BendStress_Max_MPa, TensStress_Min_MPa, TensStress_Max_MPa`

### Interactive Charts (NEW)

**Chart Groupings:**

1. **By Loading Condition** (4 HTML pages)
   - `charts_env_001yr_090deg.html` - All 5 tensions at 1yr 090°
   - `charts_env_001yr_135deg.html` - All 5 tensions at 1yr 135°
   - `charts_env_95NE_090deg.html` - All 5 tensions at 95%NE 090°
   - `charts_env_95NE_135deg.html` - All 5 tensions at 95%NE 135°

2. **By Tension Level** (5 HTML pages)
   - `charts_tension_900kN.html` - All 4 load cases at 900 kN
   - `charts_tension_1250kN.html` - All 4 load cases at 1250 kN
   - `charts_tension_1500kN.html` - All 4 load cases at 1500 kN
   - `charts_tension_2000kN.html` - All 4 load cases at 2000 kN
   - `charts_tension_2500kN.html` - All 4 load cases at 2500 kN

3. **Critical Zone Focus** (0-300m arc length)
   - Separate plots zoomed to 0-300m where stress response is critical
   - Included as subplots in each HTML page

**Chart Types per Page:**
- Effective Tension vs Arc Length (Min/Max envelope)
- Bending Stress vs Arc Length (Max)
- Tensile Strain vs Arc Length (Max)
- Global Y Position vs Arc Length (Min/Max)
- Combined Utilization vs Arc Length

**Chart Features:**
- Full length view (0 to end) + Critical zone view (0-300m) side-by-side
- Color-coded lines by tension or load case
- Interactive Plotly with zoom, pan, legend toggle
- Horizontal reference lines (X65 yield, unity utilization)

## Verification
- Confirm script finds all 20 .sim files
- Verify summary CSV contains 20 rows
- Verify 20 rangegraph CSVs are created in `postproc/rangegraphs/`
- Verify 4 load condition chart pages created (`charts_env_*.html`)
- Verify 5 tension level chart pages created (`charts_tension_*.html`)
- Check all charts show critical zone (0-300m) views
- Check HTML report shows all 5 tension levels
- Validate utilization values are reasonable (< 1.0 for pass)

## Critical Files
- `postproc/generate_html_report.py` - Main script (modify)
- `postproc/rangegraphs/` - Output directory for arc-length CSVs (create)
- `runs/sim/*.sim` - Input simulation files (20 total)
