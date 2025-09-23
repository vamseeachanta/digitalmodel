# Sample Data Inputs and Outputs Location Guide

## 📁 Input Data Locations

### 1. Sample Time Series Data
**Location**: `sample_data/`

Structure:
```
sample_data/
├── fsts_l015/                              # Configuration 1: FSTs Light (15% loaded)
│   ├── wind_000deg/                        # Wind reference (10 m/s baseline)
│   │   └── Strut[1-8].csv                  # 8 strut files with tension time series
│   └── wave_000deg_Hs050cm_Tp270cs/        # Wave reference (Hs=0.5m baseline)
│       └── Strut[1-8].csv                  # 8 strut files with tension time series
├── fsts_l095/                              # Configuration 2: FSTs Full (95% loaded)
│   ├── wind_000deg/
│   │   └── Strut[1-8].csv
│   └── wave_000deg_Hs050cm_Tp270cs/
│       └── Strut[1-8].csv
├── fsts_l015_125km3_l100_pb/               # Configuration 3: FSTs Light + LNGC Full
│   ├── wind_000deg/
│   │   └── Strut[1-8].csv
│   └── wave_000deg_Hs050cm_Tp270cs/
│       └── Strut[1-8].csv
└── fsts_l095_125km3_l000_pb/               # Configuration 4: FSTs Full + LNGC Light
    ├── wind_000deg/
    │   └── Strut[1-8].csv
    └── wave_000deg_Hs050cm_Tp270cs/
        └── Strut[1-8].csv
```

**Total Files**: 64 CSV files (4 configs × 2 references × 8 struts)

### 2. Configuration Files
**Location**: `input/`

Files:
- `fatigue_conditions.csv` - 10 environmental conditions with wind/wave parameters
- `configuration_weights.csv` - Operational time distribution for 4 vessel configurations
- `tension_range_to_stress_range_function.csv` - Tension-to-stress conversion table

### 3. Sample Data Characteristics
- **Time Steps**: 1000 points (0-99.9 seconds at 0.1s intervals)
- **Wind Tension**: Base ~500 kN with sinusoidal variation
- **Wave Tension**: Base ~200 kN with higher frequency oscillation
- **Column Format**: `Time`, `Effective Tension at Vessel End`

## 📊 Output Data Locations

### 1. Analysis Results
**Location**: `output/`

Files:
- `fatigue_analysis_results.csv` - Detailed results for each strut/condition/configuration
  - Columns: config, condition_id, strut, tension_ranges, damage, fatigue_life_years, etc.
- `integrated_fatigue_results.csv` - Duplicate of above from integrated processor
- `configuration_summary.json` - Summary by configuration with critical struts and minimum life

### 2. Visualizations
**Location**: `output/visualizations/`

Generated plots:
- `sn_curve.png` - S-N curve with stress range data points
- `damage_histogram_fsts_l015.png` - Damage distribution for Config 1
- `damage_histogram_fsts_l095.png` - Damage distribution for Config 2
- `damage_histogram_fsts_l015_125km3_l100_pb.png` - Damage distribution for Config 3
- `damage_histogram_fsts_l095_125km3_l000_pb.png` - Damage distribution for Config 4
- `fatigue_dashboard.png` - Combined multi-panel dashboard view

### 3. Reports
**Location**: `output/`

- `sample_analysis_report.md` - Comprehensive analysis report with methodology and results

### 4. Performance Metrics (when using parallel processing)
**Location**: `output/`

- `parallel_performance.json` - Processing metrics, timing, and efficiency statistics

## 🔧 How to Access the Data

### View Input Data
```bash
# Check fatigue conditions
cat input/fatigue_conditions.csv

# Check configuration weights
cat input/configuration_weights.csv

# View sample time series (first 10 lines)
head -10 sample_data/fsts_l015/wind_000deg/Strut1.csv
```

### View Output Results
```bash
# Check analysis results
head output/fatigue_analysis_results.csv

# View configuration summary
python -m json.tool output/configuration_summary.json

# Open visualizations (Windows)
start output/visualizations/fatigue_dashboard.png
```

### Python Access
```python
import pandas as pd
import json

# Load input data
conditions = pd.read_csv('input/fatigue_conditions.csv')
weights = pd.read_csv('input/configuration_weights.csv')
sample_ts = pd.read_csv('sample_data/fsts_l015/wind_000deg/Strut1.csv')

# Load output results
results = pd.read_csv('output/fatigue_analysis_results.csv')
with open('output/configuration_summary.json', 'r') as f:
    summary = json.load(f)

# Access specific configuration results
fsts_l015_results = results[results['config'] == 'fsts_l015']
```

## 📝 Data Flow Summary

```
INPUT                          PROCESSING                      OUTPUT
=====                          ==========                      ======
sample_data/                   Reference Seastate              output/
  └── [64 CSV files]    →      Processor                  →    ├── fatigue_analysis_results.csv
                               ├── Load scaling                 ├── configuration_summary.json
input/                         ├── Rainflow counting            └── visualizations/
  ├── fatigue_conditions.csv  ├── Damage calculation               ├── sn_curve.png
  ├── config_weights.csv      └── Visualization                    ├── damage_histogram_*.png
  └── tension_stress.csv                                           └── fatigue_dashboard.png
```

## 🎯 Quick Commands

### Run Analysis
```bash
# With sample data
python -m digitalmodel.modules.fatigue_analysis --sample --timesteps 1000

# With parallel processing
python -m digitalmodel.modules.fatigue_analysis --sample --parallel 4
```

### Generate Visualizations
```python
from digitalmodel.modules.fatigue_analysis.visualizer import generate_all_visualizations
generate_all_visualizations('output', 'output/visualizations')
```

---
*All paths are relative to the repository root: D:/github/digitalmodel/*