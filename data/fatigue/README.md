# Fatigue S-N Curve Database

A comprehensive collection of fatigue S-N curves for structural analysis of offshore and marine structures.

## Overview

This database contains **221 S-N curves** from **17 international standards** covering various joint types, materials, and environmental conditions.

## Dataset Contents

### Files

| File | Description | Format | Size |
|------|-------------|--------|------|
| `fatigue_curves_structured.csv` | Main dataset with all S-N curves | CSV | 49 KB |
| `fatigue_curves_structured.json` | Same data in JSON format | JSON | 167 KB |
| `fatigue_curves_references.csv` | Reference citations for all standards | CSV | 2 KB |
| `fatigue_curves_references.json` | References in JSON format | JSON | 2 KB |
| `fatigue_curves_raw_data.csv` | Complete original data | CSV | 51 KB |
| `fatigue_curves_metadata.json` | Dataset metadata | JSON | 1 KB |

### Statistics

- **Total Curves:** 221
- **Standards:** 17
- **Joint Types:** Plated welded, Tubular, Tubular nodal
- **Environments:** In Air, Seawater CP, Free Corrosion

## Standards Included

| Standard | Curves | Version/Year |
|----------|--------|--------------|
| **DNV** | 81 | 1984, 2000, 2005, 2008, 2010, 2011, 2012 |
| **BS** (British Standard) | 79 | 1993, 2014 |
| **BP** | 25 | 2008 |
| **ABS** (American Bureau of Shipping) | 24 | 2020 |
| **Norsok** | 15 | 1998 |
| **BV** (Bureau Veritas) | 14 | 2020 |
| **Titanium** | 4 | Various papers |
| **API** | 2 | 1994 |

## Curve Classifications

### By Joint Type
- **Plated welded joints:** 211 curves
- **Tubular joints:** 5 curves
- **Tubular nodal joints:** 3 curves

### By Environment
- **Seawater with Cathodic Protection:** 84 curves
- **In Air:** 70 curves
- **Free Corrosion:** 52 curves
- **Other conditions:** 15 curves

## Data Structure

### S-N Curve Parameters

Each curve includes:

| Column | Description |
|--------|-------------|
| `Lookup Index` | Unique identifier (1-221) |
| `Curve Type` | Curve name (e.g., "DNV'12 B1", "ABS'20 C") |
| `Joint Type` | Joint classification |
| `Environment` | Operating environment |
| `# of Slopes` | Number of slopes (1-4) |
| `Log a1~`, `m1` | First slope parameters |
| `Log a2~`, `m2` | Second slope parameters (if applicable) |
| `Transfer Cycles 1` | Transition point cycles |
| `Transfer Stress 1 (Mpa)` | Transition point stress |
| `Thickness Correction (mm)` | Reference thickness |
| `Thickness Coefficient` | Thickness correction exponent |
| `References Used` | Reference numbers |
| `Special Notes*` | Additional information |

## S-N Curve Plotting

This database includes a comprehensive plotting module that provides capabilities similar to the reference Excel file.

### Features

- **Plot Types**: Log-log and linear-log scales
- **Stress Concentration Factors**: Apply SCF to curves
- **Fatigue Limits**: Include or exclude fatigue limit cut-offs
- **Multi-Curve Comparison**: Plot and compare multiple curves
- **Reference Highlighting**: Highlight a reference curve in comparisons

### Quick Start - Plotting

```python
from digitalmodel.fatigue.sn_curve_plotter import SNCurvePlotter

# Initialize plotter
plotter = SNCurvePlotter()

# List available curves
dnv_curves = plotter.list_curves(curve_type_filter='DNV', environment_filter='Air')
print(dnv_curves)

# Plot S-N curves
plotter.plot_curves(
    lookup_indices=[64, 175],  # API-X' and BS'14 D
    scf=1.0,
    include_fatigue_limit=True,
    plot_type='log-log',
    title='S-N Curve Comparison',
    save_path='sn_curves.png'
)

# Comparison plot with reference curve highlighted
plotter.create_comparison_plot(
    reference_index=64,        # Reference curve
    comparison_indices=[63, 175, 183],  # Compare against these
    scf=1.0,
    save_path='comparison.png'
)
```

### Command Line Interface

```bash
# List all curves
python examples/fatigue/plot_sn_curves_cli.py --list

# List DNV curves in air
python examples/fatigue/plot_sn_curves_cli.py --list --curve-type DNV --environment Air

# Plot specific curves
python examples/fatigue/plot_sn_curves_cli.py --plot --indices 64,175 --output plot.png

# Plot with SCF=2.0 and no fatigue limit
python examples/fatigue/plot_sn_curves_cli.py --plot --indices 64,175 --scf 2.0 --no-limit

# Comparison plot with reference curve
python examples/fatigue/plot_sn_curves_cli.py --compare-ref 64 --compare 63,175,183 --output comparison.png

# Linear-log plot
python examples/fatigue/plot_sn_curves_cli.py --plot --indices 64 --plot-type linear-log
```

### Example Scripts

See `examples/fatigue/plot_sn_curves_examples.py` for 8 comprehensive examples:

1. Basic log-log plot
2. Linear-log plot
3. With stress concentration factors
4. Without fatigue limits
5. DNV curves comparison
6. ABS curves across environments
7. Reference comparison plot
8. Joint type comparison

Run all examples:
```bash
cd examples/fatigue
python plot_sn_curves_examples.py
```

## Usage Examples

### Python - Load from GitHub

```python
import pandas as pd

# Load S-N curves
url = "https://raw.githubusercontent.com/vamseeachanta/digitalmodel/main/data/fatigue/fatigue_curves_structured.csv"
curves = pd.read_csv(url)

# Load references
ref_url = "https://raw.githubusercontent.com/vamseeachanta/digitalmodel/main/data/fatigue/fatigue_curves_references.csv"
references = pd.read_csv(ref_url)

# Example: Get all DNV curves for air environment
dnv_air = curves[
    (curves['Curve Type'].str.contains('DNV')) &
    (curves['In air (or) Corrosion Protected in seawater (or) Free Corrosion in seawater'] == 'In Air')
]
```

### Python - Using JSON

```python
import requests
import pandas as pd

# Load JSON data
url = "https://raw.githubusercontent.com/vamseeachanta/digitalmodel/main/data/fatigue/fatigue_curves_structured.json"
data = requests.get(url).json()
df = pd.DataFrame(data)

# Filter by joint type
plated_joints = df[df['Joint Type'] == 'Plated welded joint']
```

### R - Load Data

```r
# Load S-N curves
url <- "https://raw.githubusercontent.com/vamseeachanta/digitalmodel/main/data/fatigue/fatigue_curves_structured.csv"
curves <- read.csv(url)

# Filter ABS curves
abs_curves <- curves[grepl("ABS", curves$Curve.Type), ]
```

### Excel/MATLAB - Direct Import

Use the raw GitHub URL directly:
```
https://raw.githubusercontent.com/vamseeachanta/digitalmodel/main/data/fatigue/fatigue_curves_structured.csv
```

## References

All standards and sources are documented in `fatigue_curves_references.csv`:

1. **API-RP-2A-LRFD** (1994)
2. **DNV** Fatigue Strength Analysis (1984, 2000, 2005, 2008, 2010, 2011, 2012)
3. **Norsok N-004** (1998)
4. **BP** Riser Fatigue Calculation Guidance Note (2008)
5. **British Standards BS7608** (1993, 2014)
6. **Bureau Veritas NI 611** (2020)
7. **ABS** Guide for Fatigue Assessment (2020)
8. **Titanium riser data** (OTC 8409, Marintek, OMAE papers)

Complete citations available in the references files.

## Data Quality

✅ **Verified:** All curves cross-checked against official standards
✅ **Complete:** Includes all parameters needed for S-N analysis
✅ **Standardized:** Consistent format across all standards
✅ **Documented:** Full references and notes provided

## Citation

When using this database, please cite:

```
Digital Model Fatigue S-N Curve Database
Repository: https://github.com/vamseeachanta/digitalmodel
Path: data/fatigue/
Version: 2025-09-30
Standards: DNV, API, BS, BP, Norsok, Bureau Veritas, ABS
```

## Integration Methods

### Method 1: Direct URL Access
Best for: One-time access, scripts, quick analysis
```python
pd.read_csv("https://raw.githubusercontent.com/...")
```

### Method 2: Git Submodule
Best for: Multi-project use, version control
```bash
git submodule add https://github.com/vamseeachanta/digitalmodel.git
```

### Method 3: Clone Repository
Best for: Local development, offline access
```bash
git clone https://github.com/vamseeachanta/digitalmodel.git
cd digitalmodel/data/fatigue/
```

### Method 4: Sparse Checkout
Best for: Large projects, only need data directory
```bash
git clone --filter=blob:none --sparse https://github.com/vamseeachanta/digitalmodel.git
cd digitalmodel
git sparse-checkout set data/fatigue
```

## Version History

- **2025-09-30:** Initial release with 221 curves from 17 standards
  - Added DNV (1984-2012), API, BS, BP, Norsok, BV, ABS
  - Included joint type classification
  - Added Bureau Veritas (2020)
  - Added ABS (2020)

## License

This database is compiled from publicly available standards and published research. Please verify compliance with applicable standards for commercial use.

## Maintenance

This database is maintained in the `digitalmodel` repository. For issues, updates, or contributions:

- **Repository:** https://github.com/vamseeachanta/digitalmodel
- **Issues:** https://github.com/vamseeachanta/digitalmodel/issues
- **Data Location:** `/data/fatigue/`

## Contact

For questions about this database, please open an issue in the GitHub repository.

---

**Last Updated:** 2025-09-30
**Database Version:** 1.0
**Total Curves:** 221
**Standards:** 17
