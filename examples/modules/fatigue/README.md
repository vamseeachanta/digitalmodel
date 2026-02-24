# Fatigue Analysis Module Examples

## Overview

Examples for fatigue life assessment of offshore structures, mooring lines, and components.

## Files

### Python Scripts

**`fatigue_analysis_examples.py`** - Comprehensive fatigue analysis examples
- S-N curve generation and plotting
- Rainflow cycle counting
- Cumulative damage (Miner's rule)
- Fatigue life calculations
- Multiple material standards

### Advanced Examples

**`advanced_examples/`** - Advanced fatigue analysis workflows
- `complete_fatigue_analysis.py` - Full fatigue assessment workflow
- `plot_sn_curves_cli.py` - Command-line S-N curve plotting
- `plot_sn_curves_examples.py` - S-N curve visualization examples

### Input Files

**`../input_files/fatigue_analysis/`** - Sample input configurations
- `offshore_structure_fatigue.yml` - Offshore structure fatigue assessment config

## Usage

### Basic Fatigue Analysis

```bash
python examples/modules/fatigue/fatigue_analysis_examples.py
```

### Advanced Complete Analysis

```bash
python examples/modules/fatigue/advanced_examples/complete_fatigue_analysis.py
```

### Plot S-N Curves

```bash
python examples/modules/fatigue/advanced_examples/plot_sn_curves_cli.py --material steel --environment seawater
```

## Features

- ✅ S-N curve database (DNV, BS, AWS)
- ✅ Rainflow cycle counting
- ✅ Palmgren-Miner cumulative damage
- ✅ Stress concentration factors
- ✅ Mean stress corrections
- ✅ Multi-axial fatigue
- ✅ Variable amplitude loading

## Example Workflow

```python
from assetutilities.fatigue import FatigueAnalyzer

# Load stress history
stress_time_series = load_stress_data('mooring_line_tension.csv')

# Perform rainflow counting
cycles = rainflow_count(stress_time_series)

# Calculate cumulative damage
analyzer = FatigueAnalyzer(
    material='steel',
    environment='seawater',
    sn_curve='DNV-C203'
)

damage = analyzer.calculate_damage(cycles)
life_years = analyzer.fatigue_life(damage)

print(f"Fatigue life: {life_years:.1f} years")
```

## Standards Referenced

- DNV-RP-C203 (Fatigue design)
- BS 7608 (Fatigue design of steel structures)
- AWS D1.1 (Structural welding code)
- API RP 2A-WSD (Fixed platforms)

## Related Modules

- `../stress/` - Stress analysis
- `../mooring/` - Mooring line design
- `../fpso/` - FPSO fatigue assessment
