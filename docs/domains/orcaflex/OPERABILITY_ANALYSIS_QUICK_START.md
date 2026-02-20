# CALM Buoy Operability Analysis - Quick Start Guide

## 🚀 5-Minute Quick Start

This guide will get you running a complete operability analysis in 5 minutes using the North Sea example.

---

## Prerequisites

- Python 3.8+ with required packages (plotly, pandas, numpy, pyyaml)
- OrcaFlex (for running simulations - optional for testing)
- Git repository cloned

---

## Step 1: Generate Operability Project (1 minute)

Generate a CALM Buoy project with 12 operability load cases using 1-year return period data:

```bash
cd D:/workspace-hub/digitalmodel

python scripts/generate_calm_buoy_project.py \
  --config examples/modules/calm_buoy/north_sea_calm_project_human.yml \
  --output projects/QUICKSTART_OPERABILITY \
  --operability-directions 12 \
  --operability-return-period design_1yr \
  --validate
```

**What happens:**
- ✅ Creates project directory structure
- ✅ Generates 12 load cases (0°, 30°, 60°, ..., 330°)
- ✅ Each load case uses 1-year return period (Hs=2.5m, Tp=7.5s)
- ✅ Environment modules populated automatically
- ✅ Validation runs (100% pass expected)

**Expected output:**
```
🎯 Generating operability analysis load cases...
   Directions: 12 (30° spacing)
   Return period: design_1yr
✅ Generated 12 operability load cases

✅ PROJECT GENERATION COMPLETE
Project location: projects/QUICKSTART_OPERABILITY
```

---

## Step 2: Review Generated Files (1 minute)

Check what was created:

```bash
cd projects/QUICKSTART_OPERABILITY
ls -la
```

**Directory structure:**
```
QUICKSTART_OPERABILITY/
├── project_config.yml          # Project configuration
├── README.md                   # Project documentation
├── orcaflex/                   # OrcaFlex model files
│   ├── NSE_CALM_001_calm_buoy.yml
│   └── modules/                # 20 modular files
├── reports/
│   └── validation/             # Validation reports
└── data/                       # Project data
```

**Review load cases in configuration:**

```bash
# View generated load cases
grep -A 5 "operability_" project_config.yml | head -20
```

You should see 12 load cases with the 1-year return period conditions applied.

---

## Step 3: Generate Operability Report with Sample Data (2 minutes)

Since we don't have OrcaFlex simulation results yet, use sample data to test the analysis:

```bash
python scripts/analyze_operability_results.py \
  --project projects/QUICKSTART_OPERABILITY \
  --use-sample-data \
  --sample-directions 12 \
  --operability-limit 2.5
```

**What happens:**
- ✅ Creates realistic sample tension data
- ✅ Generates interactive Plotly operability envelope
- ✅ Calculates weather downtime statistics
- ✅ Produces comprehensive HTML report

**Expected output:**
```
📊 Quick Summary:
   Maximum tension: 4639.7 kN @ 90°
   Minimum tension: 3384.9 kN @ 180°
   Mean tension: 3979.4 kN
   Intact limit: 4055.6 kN
   Max utilization: 1.144 ❌

✅ Comprehensive report saved: reports/operability_report_YYYYMMDD_HHMMSS.html
```

---

## Step 4: View Interactive Report (1 minute)

Open the generated HTML report in your browser:

**Windows:**
```bash
start projects/QUICKSTART_OPERABILITY/reports/operability_report_*.html
```

**Linux/Mac:**
```bash
open projects/QUICKSTART_OPERABILITY/reports/operability_report_*.html
```

**What you'll see:**
1. **Project Information Card** - Project metadata
2. **Design Criteria Card** - MBL, safety factors
3. **Interactive Operability Envelope** - Polar plot with:
   - Maximum tension vs. heading (360°)
   - Mean tension line
   - Intact limit line (MBL/SF)
   - Hover tooltips with exact values
4. **Critical Headings Table** - Top 10 worst-case scenarios
5. **Weather Downtime Analysis** - Annual operational statistics

---

## 🎯 What You've Achieved

In 5 minutes, you've:

✅ Generated a complete CALM Buoy project with operability analysis
✅ Created 12 load cases with 1-year return period data
✅ Generated an interactive operability report with Plotly visualizations
✅ Identified critical headings and utilization ratios
✅ Calculated weather downtime statistics

---

## Next Steps: Running Real OrcaFlex Simulations

### Option A: Manual OrcaFlex Execution

1. **Open OrcaFlex:**
   ```
   OrcaFlex projects/QUICKSTART_OPERABILITY/orcaflex/NSE_CALM_001_calm_buoy.yml
   ```

2. **Run Batch Analysis:**
   - File → Batch Script → Run
   - Select all 12 load cases
   - Run simulations (1 hour each × 12 = 12 hours)

3. **Export Results:**
   - Extract maximum mooring line tensions for each load case
   - Save to: `projects/QUICKSTART_OPERABILITY/results/operability_results.csv`

   **Required CSV format:**
   ```csv
   heading,max_tension,mean_tension,std_tension
   0,3500.5,2800.2,250.3
   30,3800.1,3000.5,260.1
   60,4200.8,3300.2,280.5
   ...
   ```

4. **Run Analysis with Real Data:**
   ```bash
   python scripts/analyze_operability_results.py \
     --project projects/QUICKSTART_OPERABILITY \
     --results projects/QUICKSTART_OPERABILITY/results/operability_results.csv \
     --wave-scatter data/raw/north_sea_wave_scatter.csv \
     --operability-limit 2.5
   ```

### Option B: Python API Integration (Future Enhancement)

```python
# Future feature - automated OrcaFlex execution
from digitalmodel.orcaflex.batch_runner import OrcaFlexBatchRunner

runner = OrcaFlexBatchRunner(
    project_dir="projects/QUICKSTART_OPERABILITY",
    orcaflex_path="C:/Program Files/Orcina/OrcaFlex/OrcaFlex.exe"
)

# Run all load cases
results = runner.run_all_load_cases(parallel=True, max_workers=4)

# Export results automatically
results.to_csv("projects/QUICKSTART_OPERABILITY/results/operability_results.csv")
```

---

## Advanced Usage

### Generate More Directions

**24 directions (15° spacing):**
```bash
python scripts/generate_calm_buoy_project.py \
  --config examples/modules/calm_buoy/north_sea_calm_project_human.yml \
  --operability-directions 24 \
  --operability-return-period design_1yr
```

**36 directions (10° spacing):**
```bash
python scripts/generate_calm_buoy_project.py \
  --operability-directions 36
```

### Use Different Return Periods

**10-year return period (design verification):**
```bash
python scripts/generate_calm_buoy_project.py \
  --operability-directions 12 \
  --operability-return-period design_10yr
```

**100-year return period (survival):**
```bash
python scripts/generate_calm_buoy_project.py \
  --operability-directions 12 \
  --operability-return-period extreme
```

### Compare Multiple Return Periods

Run operability analysis for multiple return periods and compare:

```bash
# Generate 1-year analysis
python scripts/generate_calm_buoy_project.py \
  --output projects/OPR_1YEAR \
  --operability-return-period design_1yr

# Generate 10-year analysis
python scripts/generate_calm_buoy_project.py \
  --output projects/OPR_10YEAR \
  --operability-return-period design_10yr

# Generate 100-year analysis
python scripts/generate_calm_buoy_project.py \
  --output projects/OPR_100YEAR \
  --operability-return-period extreme

# Compare results
python scripts/compare_operability_results.py \
  --projects projects/OPR_*YEAR \
  --output reports/return_period_comparison.html
```

---

## Troubleshooting

### Issue: "Plotly not available"

**Solution:**
```bash
pip install plotly pandas numpy pyyaml
```

### Issue: "Results file not found"

This is expected if you haven't run OrcaFlex simulations yet. Use `--use-sample-data` flag:

```bash
python scripts/analyze_operability_results.py \
  --project [path] \
  --use-sample-data
```

### Issue: "Validation warnings"

Review warnings in validation report. Most warnings are informational and don't prevent generation:

```bash
cat projects/QUICKSTART_OPERABILITY/reports/validation/validation_*.md
```

### Issue: "Module not found: assetutilities"

This is a non-critical warning and can be ignored. All operability features work correctly.

---

## Understanding the Results

### Operability Envelope Interpretation

**Polar Plot:**
- **Radial axis**: Mooring line tension (kN)
- **Angular axis**: Wave heading (0-360°)
- **Blue line**: Maximum tension for each heading
- **Green dashed line**: Mean tension
- **Orange dashed line**: Intact limit (MBL/SF_intact)
- **Red dotted line**: Damaged limit (MBL/SF_damaged)

**Key Insights:**
- Headings where max tension **exceeds intact limit** → ❌ Design fails
- Headings where max tension is **90-100% of limit** → ⚠️ Warning
- Headings where max tension is **< 90% of limit** → ✅ Safe

### Critical Headings Table

Shows top 10 worst-case headings with:
- **Max Tension**: Peak tension during simulation
- **Mean Tension**: Average tension
- **Utilization Ratio**: max_tension / (MBL/SF)
  - > 1.0 → ❌ FAIL (exceeds design)
  - 0.9-1.0 → ⚠️ WARNING (close to limit)
  - < 0.9 → ✅ PASS (acceptable)

### Weather Downtime Analysis

Based on wave scatter diagram:
- **Operational %**: Percentage of time Hs < operability limit
- **Downtime %**: Percentage of time Hs > operability limit
- **Hours/Year**: Actual operational hours annually
- **Days/Year**: Downtime in days

**Example Interpretation:**
- Operability limit: Hs = 2.5m (1-year return period)
- Operational: 88% (7708 hours/year)
- Downtime: 12% (1052 hours/year = 44 days/year)

This means offloading operations can occur 88% of the year, with 44 days lost to weather.

---

## Tips & Best Practices

### 1. Start with 12 Directions

For initial studies, 12 directions (30° spacing) provides good coverage with reasonable simulation time:
- **12 directions**: ~12 hours simulation time
- **24 directions**: ~24 hours simulation time
- **36 directions**: ~36 hours simulation time

Use finer spacing (24-36) for final design or critical projects.

### 2. Use 1-Year Return Period for Operability

Per API RP 2SK, operability analysis should use 1-year return period:
- **1-year**: Operability limit, intact mooring design
- **10-year**: Design verification, damaged condition
- **100-year**: Ultimate limit state, survival

### 3. Review Critical Headings First

Focus optimization on the worst headings (typically 60-120° for CALM buoys):
- Beam seas usually give highest tensions
- Head/stern seas usually give lowest tensions

### 4. Validate Against Design Basis

Always check:
- Utilization ratios < 1.0 for all headings
- Weather downtime acceptable for operations
- Critical headings identified and mitigated

### 5. Document Assumptions

Include in project documentation:
- Return period selection rationale
- Wave scatter diagram source
- Operability limit justification
- Critical design decisions

---

## Example Real-World Workflow

### Project: North Sea CALM Buoy - Operability Assessment

**Objective:** Determine weather downtime for 300,000 DWT VLCC offloading

**Step 1:** Configure project with North Sea 1-year data
```bash
# Edit: examples/modules/calm_buoy/north_sea_calm_project_human.yml
# Set: Operability limit Hs = 2.5m (1-year return period)
```

**Step 2:** Generate 24-direction operability analysis
```bash
python scripts/generate_calm_buoy_project.py \
  --config examples/modules/calm_buoy/north_sea_calm_project_human.yml \
  --output projects/NORTH_SEA_OPERABILITY \
  --operability-directions 24 \
  --operability-return-period design_1yr
```

**Step 3:** Run OrcaFlex simulations overnight
```
- 24 load cases × 1 hour each = 24 hours
- Use OrcaFlex batch script
- Export tensions to CSV
```

**Step 4:** Generate operability report
```bash
python scripts/analyze_operability_results.py \
  --project projects/NORTH_SEA_OPERABILITY \
  --results projects/NORTH_SEA_OPERABILITY/results/operability_results.csv \
  --wave-scatter data/raw/north_sea_wave_scatter.csv \
  --operability-limit 2.5
```

**Step 5:** Review results
- Open HTML report
- Check critical headings (expect 60-120° beam seas)
- Verify all utilization ratios < 1.0
- Note weather downtime (expect ~10-15% for North Sea)

**Step 6:** Client deliverable
- Include operability envelope plot in report
- Document weather downtime in operations manual
- Provide critical heading restrictions

---

## Support & Documentation

### Full Documentation:
- **Implementation Guide**: `docs/domains/orcaflex/OPERABILITY_ANALYSIS_IMPLEMENTATION.md`
- **Proposal (Background)**: `docs/domains/orcaflex/OPERABILITY_ANALYSIS_PROPOSAL.md`
- **Metocean Data Sources**: `docs/domains/orcaflex/NORTH_SEA_METOCEAN_DATA_SOURCES.md`

### Example Files:
- **North Sea Example**: `examples/modules/calm_buoy/north_sea_calm_project_human.yml`
- **Template**: `templates/calm_buoy/project_template_human_friendly.yml`

### Scripts:
- **Generator**: `scripts/generate_calm_buoy_project.py`
- **Analyzer**: `scripts/analyze_operability_results.py`
- **Key Mapper**: `scripts/yaml_key_mapper.py`

### Source Code:
- **Analyzer Class**: `src/digitalmodel/modules/orcaflex/operability_analysis.py`

---

## Next Features (Coming Soon)

1. **OrcaFlex Python API Integration** - Automated simulation execution
2. **Damaged Condition Analysis** - 1-line out scenarios
3. **Fatigue Analysis** - Long-term tension statistics
4. **Return Period Comparison Tool** - Side-by-side analysis
5. **Real-Time Monitoring Dashboard** - Live simulation progress

---

**Ready to start? Run the 5-minute quick start above!** 🚀

**Questions or issues?** Open an issue on GitHub or check the full documentation.

---

**Last Updated**: 2025-11-11
**Status**: ✅ Production Ready
**Version**: 1.0.0
