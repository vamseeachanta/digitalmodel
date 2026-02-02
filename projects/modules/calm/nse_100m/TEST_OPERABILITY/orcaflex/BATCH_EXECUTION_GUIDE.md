# Batch Execution Guide - 1-Year Operability Analysis

**Project**: NSE_CALM_001
**Script**: `run_1year_operability.py`
**Date**: 2025-11-11

---

## Overview

This guide explains how to run batch operability analysis for all 12 directions using the new modular structure.

## Quick Start

### Step 1: Verify OrcaFlex Installation

```bash
python -c "import OrcFxAPI; print(f'OrcaFlex version: {OrcFxAPI.Version()}')"
```

If this fails, update the Python path in `run_1year_operability.py`:

```python
# Line 8-9: Uncomment and update for your installation
sys.path.append(r"C:\Program Files\Orcina\OrcaFlex\11.4\Python")
```

### Step 2: Run Batch Analysis

```bash
cd projects/TEST_OPERABILITY/orcaflex
python run_1year_operability.py
```

This will:
- Find all 12 × 1-year simple models
- Run statics and dynamics for each
- Save results to `../results/1year_operability/`
- Generate JSON report with timing statistics

---

## Models Included

The script runs these 12 analysis models:

| # | Model File | Direction | Wave Hs | Current | Wind |
|---|------------|-----------|---------|---------|------|
| 1 | NSE_CALM_001_000deg_1yr_simple.yml | 0° | 2.5m | 1.4 m/s | 20 m/s |
| 2 | NSE_CALM_001_030deg_1yr_simple.yml | 30° | 2.5m | 1.4 m/s | 20 m/s |
| 3 | NSE_CALM_001_060deg_1yr_simple.yml | 60° | 2.5m | 1.4 m/s | 20 m/s |
| 4 | NSE_CALM_001_090deg_1yr_simple.yml | 90° | 2.5m | 1.4 m/s | 20 m/s |
| 5 | NSE_CALM_001_120deg_1yr_simple.yml | 120° | 2.5m | 1.4 m/s | 20 m/s |
| 6 | NSE_CALM_001_150deg_1yr_simple.yml | 150° | 2.5m | 1.4 m/s | 20 m/s |
| 7 | NSE_CALM_001_180deg_1yr_simple.yml | 180° | 2.5m | 1.4 m/s | 20 m/s |
| 8 | NSE_CALM_001_210deg_1yr_simple.yml | 210° | 2.5m | 1.4 m/s | 20 m/s |
| 9 | NSE_CALM_001_240deg_1yr_simple.yml | 240° | 2.5m | 1.4 m/s | 20 m/s |
| 10 | NSE_CALM_001_270deg_1yr_simple.yml | 270° | 2.5m | 1.4 m/s | 20 m/s |
| 11 | NSE_CALM_001_300deg_1yr_simple.yml | 300° | 2.5m | 1.4 m/s | 20 m/s |
| 12 | NSE_CALM_001_330deg_1yr_simple.yml | 330° | 2.5m | 1.4 m/s | 20 m/s |

**Model Type**: Simple (single 6DOF vessel)
**Return Period**: 1-year (operability conditions, API RP 2SK)

---

## Output

### Results Directory Structure

```
projects/TEST_OPERABILITY/results/1year_operability/
├── NSE_CALM_001_000deg_1yr_simple.sim
├── NSE_CALM_001_030deg_1yr_simple.sim
├── ... (12 .sim files)
├── NSE_CALM_001_330deg_1yr_simple.sim
└── batch_results_YYYYMMDD_HHMMSS.json
```

### JSON Report

The batch execution generates a JSON report with:
- Timestamp and elapsed time
- Success/failure counts
- Per-model results with timing
- Error messages (if any)

Example:

```json
{
  "timestamp": "2025-11-11T14:30:00",
  "elapsed_seconds": 720.5,
  "total_models": 12,
  "success_count": 12,
  "failed_count": 0,
  "results": [
    {
      "model": "NSE_CALM_001_000deg_1yr_simple.yml",
      "status": "success",
      "statics_time": 2.1,
      "dynamics_time": 58.3,
      "total_time": 60.4
    },
    ...
  ]
}
```

---

## Known Issues & Workarounds

### Issue 1: OrcaFlex Cannot Load Models with Relative Paths

**Symptom**:
```
Error reading file: The file does not appear to be a valid OrcaFlex data file.
Invalid leading UTF-8 octet at Line 1, Column 1
```

**Cause**: OrcaFlex may have trouble resolving relative paths in `includefile` statements when models are loaded from Python.

**Workaround Options**:

#### Option A: Load Models in OrcaFlex GUI First

1. Open OrcaFlex
2. File → Load → Select an analysis model (e.g., `NSE_CALM_001_000deg_1yr_simple.yml`)
3. Verify it loads correctly in GUI
4. If it loads in GUI but not in Python, this confirms path resolution issue

#### Option B: Use Absolute Paths

Create a script to temporarily convert relative to absolute paths:

```python
import re
from pathlib import Path

def fix_paths_for_batch(model_file: Path, base_dir: Path):
    """Replace relative paths with absolute paths."""
    content = model_file.read_text()
    content = re.sub(
        r'includefile: \.\./base_files/',
        f'includefile: {str(base_dir / "base_files")}/'.replace('\\', '/'),
        content
    )
    temp_file = model_file.parent / f"_temp_{model_file.name}"
    temp_file.write_text(content)
    return temp_file
```

#### Option C: Run from OrcaFlex Working Directory

```python
import os
os.chdir('D:/workspace-hub/digitalmodel/projects/TEST_OPERABILITY/orcaflex/analysis_models')
model = OrcFxAPI.Model('NSE_CALM_001_000deg_1yr_simple.yml')
```

#### Option D: Use OrcaFlex Batch Processing

Use OrcaFlex's built-in batch mode instead of Python:

```bash
# Create batch file list
echo "NSE_CALM_001_000deg_1yr_simple.yml" > batch_list.txt
echo "NSE_CALM_001_030deg_1yr_simple.yml" >> batch_list.txt
# ... (all 12 models)

# Run OrcaFlex batch (adjust path to your OrcaFlex installation)
"C:\Program Files\Orcina\OrcaFlex\11.4\OrcaFlex.exe" /b batch_list.txt
```

### Issue 2: Unicode Display on Windows Console

The script has been updated to avoid Unicode emojis. If you still see encoding errors, run:

```bash
# Set console to UTF-8
chcp 65001
python run_1year_operability.py
```

---

## Performance Expectations

### Simple Model (Single Vessel)

**Typical timing per model**:
- Statics: 1-3 seconds
- Dynamics: 30-90 seconds (depends on simulation duration)
- **Total per model**: ~60 seconds

**Full batch (12 models)**:
- Serial execution: ~12 minutes
- **Recommended**: Run overnight for complex models

### Hardware Recommendations

| Component | Minimum | Recommended |
|-----------|---------|-------------|
| CPU | 4 cores | 8+ cores |
| RAM | 8 GB | 16+ GB |
| Storage | HDD | SSD (faster I/O) |

---

## Alternative: Run Specific Directions

To run only specific directions, modify the script:

```python
# In main() function, replace:
model_pattern = "NSE_CALM_001_*deg_1yr_simple.yml"

# With specific directions:
directions = [0, 90, 180]  # Only these directions
models = [models_dir / f"NSE_CALM_001_{d:03d}deg_1yr_simple.yml" for d in directions]
```

---

## Post-Processing

After batch execution completes, post-process results:

### Extract Maximum Motions

```python
import OrcFxAPI

for direction in [0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330]:
    sim_file = f"NSE_CALM_001_{direction:03d}deg_1yr_simple.sim"
    model = OrcFxAPI.Model()
    model.LoadSimulation(sim_file)

    # Get vessel object
    vessel = model['Vessel1']

    # Extract maximum motions
    max_surge = vessel.TimeHistory('X', OrcFxAPI.oeMax)
    max_heave = vessel.TimeHistory('Z', OrcFxAPI.oeMax)
    max_roll = vessel.TimeHistory('Rotation 1', OrcFxAPI.oeMax)

    print(f"{direction}°: Surge={max_surge:.2f}m, Heave={max_heave:.2f}m, Roll={max_roll:.2f}°")
```

### Generate Polar Plot

```python
import matplotlib.pyplot as plt
import numpy as np

# After extracting motions for all directions
directions_rad = np.deg2rad(directions)
fig = plt.figure(figsize=(10, 10))
ax = fig.add_subplot(111, projection='polar')
ax.plot(directions_rad, surge_values, 'o-', label='Surge')
ax.plot(directions_rad, heave_values, 's-', label='Heave')
ax.legend()
plt.title('CALM Buoy Motions vs Direction (1-Year)')
plt.savefig('operability_polar.png')
```

---

## Advanced: Parallel Execution

For faster batch processing, use the universal batch processor:

```python
from digitalmodel.orcaflex.universal.batch_processor import BatchProcessor

processor = BatchProcessor(max_workers=4)  # Adjust based on CPU cores
results = processor.process_batch(
    models=list(models_dir.glob("NSE_CALM_001_*deg_1yr_simple.yml")),
    output_directory=results_dir
)
```

**Note**: Parallel execution requires sufficient RAM (2-4 GB per worker).

---

## Troubleshooting

### Problem: "No module named OrcFxAPI"

**Solution**: Install OrcaFlex and add Python API to path

### Problem: Models take too long

**Solution**:
1. Check simulation duration in `_01c_dynamics.yml`
2. Reduce output frequency if not needed
3. Use simple model type (not discretised)

### Problem: "File not found" errors

**Solution**: Verify directory structure:

```
orcaflex/
├── analysis_models/     ← Models here
├── base_files/          ← Base modules here
│   └── env/             ← Environment files here
└── run_1year_operability.py  ← Script here
```

### Problem: Statics don't converge

**Solution**:
1. Check mooring line pretensions in `_07_lines.yml`
2. Verify vessel displacement matches buoyancy
3. Review static analysis settings in `_01b_statics.yml`

---

## Next Steps

After successful 1-year analysis:

1. **10-Year Analysis**: Modify script pattern to `*_10yr_simple.yml`
2. **100-Year Analysis**: Modify script pattern to `*_100yr_simple.yml`
3. **Discretised Models**: Change to `*_discretised.yml` for detailed analysis
4. **Critical Cases**: Re-run worst directions with discretised models

---

## References

- **OrcaFlex Manual**: Batch Processing (Section 5.8)
- **API RP 2SK 2005**: Design and Analysis of Stationkeeping Systems
- **Project Documentation**: See `STRUCTURE_SUMMARY.md`

---

*Last updated: 2025-11-11*
*Script: run_1year_operability.py*
*Status: Ready for testing with OrcaFlex installation*
