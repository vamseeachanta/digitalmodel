# Cumulative Damage Analysis - Fatigue Engineer's Practical Guide

## Quick Start for Fatigue Engineers

This guide provides practical, hands-on instructions for running fatigue analysis on offshore structures using the cumulative damage module.

## 1. Pre-Analysis Checklist

Before starting, ensure you have:

- [ ] **Rainflow counting results** (stress ranges and cycle counts)
- [ ] **Sea state occurrence data** (probability distribution)
- [ ] **S-N curve parameters** (already applied in damage calculation)
- [ ] **Component geometry** (strut numbers, locations)
- [ ] **Python environment** with required packages

## 2. Step-by-Step Execution

### Step 1: Prepare Your Data

#### 1.1 Organize Damage Rate Files

Place your damage rate CSV files in the correct structure:
```
sample_damage_results/
├── {config}_FC001_Strut1_loc02_damage_rate.csv
├── {config}_FC001_Strut1_loc03_damage_rate.csv
└── ... (all FC/Strut/Location combinations)
```

**File Format Requirements:**
```csv
stress_range_mpa,cycles_annual,stress_corrected_mpa,cycles_to_failure,damage_per_bin,damage_rate_per_year
25.5,14132800,51.0,8482539,0.0016,0.0016
30.2,8234100,60.4,5434403,0.0015,0.0015
...
```

#### 1.2 Prepare Sea State Occurrence File

Update `input/fatigue_seastates_production.csv`:
```csv
Row,Wind Speed (m/s),Wind Dir (deg),Hs (m),Tp (s),Wave Dir (deg),Occurrence (%)
FC001,5,180,0.15,1.7,150,7.76
FC002,3,180,0.09,1.0,150,6.977
...
```

**Important:** Ensure occurrence percentages sum to ~100% (±0.5%)

### Step 2: Configure the Analysis

#### 2.1 Basic Configuration

Edit `cumulative_damage_config.yml`:

```yaml
# Essential settings for engineers
processing:
  configurations:
    mode: "all"  # or "specific" with list
  struts:
    range: [1, 8]  # Your structure's strut numbers
  locations:
    mode: "all"  # Process all locations found
    
output:
  individual_files:
    enabled: true  # Keep for detailed review
  summary:
    enabled: true  # Master summary file
  plots:
    enabled: true  # Visual inspection
    
execution:
  parallel_processing: true  # Speed up analysis
  max_workers: 4  # Adjust based on CPU cores
```

#### 2.2 Quality Control Settings

```yaml
quality_checks:
  enabled: true
  checks:
    - name: "damage_rate_bounds"
      min_value: 0.0
      max_value: 0.1  # Typical max annual damage
    - name: "fatigue_life_bounds"
      min_years: 5.0   # Minimum expected life
      max_years: 500.0 # Maximum reasonable life
```

### Step 3: Run the Analysis

#### 3.1 Command Line Execution

```bash
# Navigate to analysis directory
cd specs/modules/fatigue-analysis/cummulative_damage

# Run with default configuration
python cumulative_damage_analysis.py

# Or with specific options
python cumulative_damage_analysis.py --config my_config.yml --log-level INFO
```

#### 3.2 Using the Runner Script

```bash
# Simple execution
python run_analysis.py
```

### Step 4: Monitor Progress

Watch the console output:
```
2025-09-25 10:00:00 - INFO - Loading fatigue conditions from input/...
2025-09-25 10:00:01 - INFO - Found 112 unique combinations
2025-09-25 10:00:02 - INFO - Processing: Config1 - Strut1 - loc02
2025-09-25 10:00:03 - INFO - Saved individual result to output/...
```

Check the log file for details:
```bash
tail -f cumulative_damage_analysis.log
```

## 3. Understanding the Results

### 3.1 Summary File Structure

`output/fatigue_life_summary.csv`:
```csv
configuration,strut,location,total_damage_rate,fatigue_life_years,critical_fc,critical_fc_contribution
Platform_A,1,loc02,0.0234,42.7,FC008,35.2
Platform_A,1,loc03,0.0187,53.5,FC008,31.8
```

**Key Columns:**
- **total_damage_rate**: Annual damage accumulation (should be < 0.1)
- **fatigue_life_years**: Estimated life (1/damage_rate)
- **critical_fc**: Most damaging sea state
- **critical_fc_contribution**: % of total damage from critical FC

### 3.2 Individual Result Files

Each file contains detailed breakdown:
```csv
fatigue_condition,occurrence_percent,damage_rate_annual,weighted_damage_rate,contribution_percent
FC001,7.76,0.0125,0.00097,41.5
FC002,6.977,0.0089,0.00062,26.5
...
TOTAL,100.0,,0.00234,100.0
FATIGUE_LIFE_YEARS,,,,42.7
```

### 3.3 Interpreting Plots

#### Damage Contribution Bar Chart
- **Height**: Weighted damage rate per sea state
- **Red Line**: Cumulative damage accumulation
- **Focus on**: Tall bars (high damage contributors)

#### Fatigue Life Heatmap
- **Color Scale**: Green (long life) to Red (short life)
- **Pattern Recognition**: Identify problem areas
- **Clusters**: Similar behavior regions

## 4. Engineering Decisions

### 4.1 Critical Component Identification

Components requiring immediate attention:
```python
# In analysis_report.md, look for:
- Fatigue life < 20 years
- Total damage rate > 0.05
- Single FC contribution > 50%
```

### 4.2 Inspection Planning

Based on fatigue life (T_f):
```
Inspection Interval = T_f / FDF

Where FDF (Fatigue Design Factor):
- 10 for critical, non-inspectable
- 5 for critical, inspectable  
- 3 for non-critical
- 2 for redundant members
```

### 4.3 Mitigation Strategies

For components with short fatigue life:

1. **Geometry Optimization**
   - Reduce stress concentrations
   - Increase section thickness
   
2. **Load Reduction**
   - Operational restrictions in severe sea states
   - Damping systems
   
3. **Material Enhancement**
   - Post-weld treatment
   - Cathodic protection optimization

## 5. Common Scenarios

### Scenario 1: Analyzing Specific Components

```yaml
# In config file:
processing:
  configurations:
    mode: "specific"
    list: ["Platform_A", "Platform_B"]
  struts:
    mode: "specific"
    list: [1, 2, 7, 8]  # Critical struts only
```

### Scenario 2: Comparing Design Alternatives

```bash
# Run multiple configs
python cumulative_damage_analysis.py --config design_v1.yml
python cumulative_damage_analysis.py --config design_v2.yml

# Compare results
python compare_results.py output_v1/summary.csv output_v2/summary.csv
```

### Scenario 3: Sensitivity Analysis

```yaml
# Adjust occurrence probabilities by ±10%
fatigue_conditions:
  sensitivity:
    enabled: true
    variation: 0.10  # 10% variation
    runs: 100        # Monte Carlo runs
```

## 6. Troubleshooting Guide

### Issue: "Infinite fatigue life"
**Cause**: Zero damage rates in input files
**Solution**: Check stress calculations and S-N curve application

### Issue: "Occurrence sum ≠ 100%"
**Cause**: Incomplete or incorrect sea state data
**Solution**: Normalize probabilities or check missing states

### Issue: "Memory error with large dataset"
**Solution**: 
```yaml
execution:
  parallel_processing: false  # Or reduce workers
  batch_size: 50  # Process in smaller batches
```

## 7. Best Practices

### Data Validation
1. Always verify input file formats before running
2. Check for outliers in damage rates
3. Validate sea state occurrence against metocean data

### Result Verification
1. Cross-check critical components with FEA hot-spots
2. Compare with similar structures/platforms
3. Validate against inspection findings

### Documentation
1. Document all assumptions in analysis
2. Keep configuration files under version control
3. Archive results with input data

## 8. Integration with Engineering Workflow

### Upstream Integration
- **FEA Results** → Stress extraction → Rainflow counting → **This module**
- **Metocean Data** → Statistical analysis → Occurrence table → **This module**

### Downstream Applications
- **This module** → Inspection planning system
- **This module** → Risk-based maintenance
- **This module** → Life extension studies

## 9. Reporting Template

### For Technical Reports

```markdown
## Fatigue Analysis Results - [Project Name]

### Analysis Parameters
- Configuration: [Config name]
- Analysis Date: [Date]
- Number of Components: [Count]
- Sea States Considered: [FC count]

### Key Findings
1. Critical Components:
   - [Component]: [Life] years
   - Governing Sea State: [FC]
   
2. Statistical Summary:
   - Mean Fatigue Life: [X] years
   - Minimum Life: [X] years
   - Components < 25 years: [Count]

### Recommendations
1. Immediate inspection: [List]
2. Enhanced monitoring: [List]
3. Design modifications: [List]
```

## 10. Quick Reference Commands

```bash
# Standard analysis
python cumulative_damage_analysis.py

# Debug mode
python cumulative_damage_analysis.py --log-level DEBUG

# Specific configuration
python cumulative_damage_analysis.py --config production.yml

# Quick test (first 10 files)
python cumulative_damage_analysis.py --test-mode --limit 10

# Generate report only (from existing results)
python generate_report.py output/fatigue_life_summary.csv
```

## Support Resources

- **Technical Issues**: fatigue-team@company.com
- **Module Documentation**: `/docs/README.md`
- **Example Datasets**: `/examples/`
- **Validation Cases**: `/tests/benchmarks/`