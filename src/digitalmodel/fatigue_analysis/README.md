# Fatigue Analysis Module

## Overview

The Fatigue Analysis Module provides comprehensive fatigue assessment capabilities for offshore structures, with a focus on strut foundations using reference seastate scaling methodology. It implements industry-standard algorithms including ASTM E1049 rainflow counting and S-N curve-based damage calculations using Miner's rule.

## Key Features

- **Reference Seastate Scaling**: Scale reference wind (10 m/s) and wave (Hs=0.5m) conditions to actual environmental conditions
- **Rainflow Counting**: ASTM E1049-compliant cycle counting algorithm
- **S-N Curve Analysis**: Multiple curve options (ABS, DNV) with bi-linear support
- **Configuration Weighting**: Support for multiple vessel configurations with operational time distribution
- **Tension-to-Stress Conversion**: Flexible conversion using lookup tables or formulas
- **Comprehensive Visualization**: Automated dashboard generation with damage histograms, S-N curves, and configuration comparisons
- **CLI Interface**: Full-featured command-line interface for batch processing

## Installation

The module is part of the digitalmodel package. Ensure you have the required dependencies:

```bash
uv add numpy pandas matplotlib seaborn
```

## Quick Start

### Command Line Interface

```bash
# Run with sample data
python -m digitalmodel.fatigue_analysis --sample

# Run with production data
python -m digitalmodel.fatigue_analysis --input-directory /path/to/data

# Process specific configurations
python -m digitalmodel.fatigue_analysis --configs fsts_l015,fsts_l095

# Use different S-N curve
python -m digitalmodel.fatigue_analysis --sn-curve DNV_D_AIR --scf 1.2

# Preview without processing
python -m digitalmodel.fatigue_analysis --dry-run
```

### Python API

```python
from digitalmodel.fatigue_analysis import ReferenceSeaStateProcessor

# Initialize processor
processor = ReferenceSeaStateProcessor(
    data_path="/path/to/data",
    output_path="output",
    scf=1.0,
    design_life_years=20.0
)

# Load fatigue conditions
conditions = processor.load_fatigue_conditions("conditions.csv")

# Run analysis
processor.run_analysis(
    conditions=conditions,
    strut_nums=[1, 2, 3, 4, 5, 6, 7, 8],
    config_names=['fsts_l015', 'fsts_l095']
)
```

### Visualization

```python
from digitalmodel.fatigue_analysis import FatigueVisualizer

# Create visualizer
visualizer = FatigueVisualizer("output/visualizations")

# Generate S-N curve plot
sn_params = {
    'name': 'ABS E in Air',
    'log_a1': 12.018,
    'm1': 3.0,
    'log_a2': 11.170,
    'm2': 5.0,
    'threshold': 1e6
}
visualizer.plot_sn_curve(sn_params)

# Generate all standard visualizations
from digitalmodel.fatigue_analysis import generate_all_visualizations
generate_all_visualizations("output", "output/visualizations")
```

## Module Components

### 1. Rainflow Counter (`rainflow_counter.py`)

Implements ASTM E1049 rainflow counting algorithm for identifying stress/load cycles.

```python
from digitalmodel.fatigue_analysis import RainflowCounter

counter = RainflowCounter(gate_value=5.0)  # Filter small cycles
ranges, counts = counter.count_cycles(time_series_data)
```

### 2. Fatigue Damage Calculator (`fatigue_damage_calculator.py`)

Calculates fatigue damage using S-N curves and Miner's linear damage accumulation rule.

```python
from digitalmodel.fatigue_analysis import FatigueDamageCalculator, SNCurveParameters

# Define S-N curve
sn_curve = SNCurveParameters(
    name="ABS E in Air",
    log_a1=12.018,
    m1=3.0,
    log_a2=11.170,
    m2=5.0,
    threshold=1e6
)

# Calculate damage
calculator = FatigueDamageCalculator(sn_curve=sn_curve, scf=1.0)
damage = calculator.calculate_damage(stress_ranges, cycle_counts)
```

### 3. Reference Seastate Processor (`reference_seastate_processor.py`)

Main processing pipeline that orchestrates the complete fatigue analysis.

Key capabilities:
- Automatic data path detection (production vs sample)
- Configuration management with operational weights
- Reference seastate selection based on wind/wave parameters
- Batch processing for multiple struts and conditions
- Results aggregation and reporting

### 4. Visualizer (`visualizer.py`)

Comprehensive visualization suite for analysis results.

Generated plots include:
- S-N curves with data points overlay
- Damage distribution histograms
- Configuration comparison charts
- Fatigue life heatmaps
- Integrated analysis dashboard

## Data Structure

### Input Data Organization

```
data_path/
├── fsts_l015/                      # Configuration 1
│   ├── wind_000deg/                # Wind reference (10 m/s)
│   │   └── Strut[1-8].csv         # Strut tension data
│   └── wave_000deg_Hs050cm_Tp270cs/  # Wave reference (Hs=0.5m)
│       └── Strut[1-8].csv
├── fsts_l095/                      # Configuration 2
├── fsts_l015_125km3_l100_pb/      # Configuration 3 (Port Berthing)
└── fsts_l095_125km3_l000_pb/      # Configuration 4 (Port Berthing)
```

### Required CSV Formats

#### Fatigue Conditions (`fatigue_conditions.csv`)
```csv
Row,Wind Speed (m/s),Wind Dir (°),Hs (m),Tp (s),Wave Dir (°),Occurrence (%)
1,5.0,0,0.15,1.93,0,7.76
2,10.0,45,0.25,2.70,45,5.50
```

#### Configuration Weights (`configuration_weights.csv`)
```csv
Configuration,Weight (%)
fsts_l015,46.25
fsts_l095,46.25
fsts_l015_125km3_l100_pb,3.75
fsts_l095_125km3_l000_pb,3.75
```

#### Tension-to-Stress Conversion (`tension_range_to_stress_range_function.csv`)
```csv
Tension Range (kN),Stress Range (Mpa)
0,0
500,125
1000,250
2000,500
```

## Vessel Configurations

The module supports 4 default vessel configurations:

1. **fsts_l015**: FSTs Light (15% loaded) - 46.25% operational time
2. **fsts_l095**: FSTs Full (95% loaded) - 46.25% operational time
3. **fsts_l015_125km3_l100_pb**: FSTs Light + LNGC Full (Port Berthing) - 3.75%
4. **fsts_l095_125km3_l000_pb**: FSTs Full + LNGC Light (Port Berthing) - 3.75%

## S-N Curves Available

- **ABS_E_AIR**: ABS E curve in air environment
- **ABS_F_AIR**: ABS F curve in air environment
- **DNV_D_AIR**: DNV D curve in air environment
- **DNV_C_SEAWATER**: DNV C curve in seawater with cathodic protection

## Scaling Methodology

### Wind Scaling
```
Scaled_Wind_Tension = Reference_Wind_Tension × (V/10)²
```
Where V is the actual wind speed in m/s.

### Wave Scaling
```
Scaled_Wave_Tension = Reference_Wave_Tension × (Hs/0.5)
```
Where Hs is the actual significant wave height in meters.

### Combined Loading
```
Effective_Tension = Scaled_Wind_Tension + Scaled_Wave_Tension
```

## Output Files

### Analysis Results

- `fatigue_analysis_results.csv`: Detailed results for each strut/condition/configuration
- `analysis_summary.json`: Configuration summaries and overall fatigue life
- `fatigue_analysis_report.md`: Markdown-formatted report

### Visualizations

- `sn_curve.png`: S-N curve with analysis points
- `config_comparison.png`: Multi-panel configuration comparison
- `damage_histogram_*.png`: Damage distribution for each configuration
- `fatigue_dashboard.png`: Comprehensive analysis dashboard

## Performance Considerations

- **Sample Mode**: Use `--sample --timesteps 1000` for quick testing
- **Parallel Processing**: Use `--parallel N` to process multiple struts concurrently
- **Memory Usage**: Large datasets may require chunked processing
- **Typical Runtime**: 
  - Sample data (1000 timesteps): < 1 second per configuration
  - Full production (200,000 timesteps): ~1 minute per configuration

## Testing

Run the test suite:

```bash
# Run all fatigue analysis tests
pytest tests/modules/fatigue_analysis/ -v

# Run specific test file
pytest tests/modules/fatigue_analysis/test_rainflow_counter.py -v

# With coverage
pytest tests/modules/fatigue_analysis/ --cov=digitalmodel.fatigue_analysis
```

## Known Issues

- Fatigue life calculations may show very low values with uncalibrated data
- Annual damage scaling requires verification with production data
- Some test expectations need adjustment for actual algorithm behavior

## References

- ASTM E1049-85: Standard Practices for Cycle Counting in Fatigue Analysis
- DNV-RP-C203: Fatigue Design of Offshore Steel Structures
- ABS Guide for Fatigue Assessment of Offshore Structures

## Contributing

When contributing to this module:

1. Follow the existing code style and patterns
2. Add unit tests for new functionality
3. Update documentation for API changes
4. Run the test suite before submitting changes
5. Generate sample visualizations to verify output

## License

See the main repository LICENSE file for details.

## Support

For issues or questions:
- Create an issue in the repository
- Refer to the specification documents in `specs/modules/fatigue-analysis/`
- Check the task summary for current development status