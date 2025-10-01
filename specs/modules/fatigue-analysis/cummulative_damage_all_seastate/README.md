# Cumulative Fatigue Damage Analysis Module

## Overview
This module calculates cumulative fatigue damage and fatigue life by combining stress rainflow damage rates with sea state occurrence probabilities using Miner's rule.

## Features
- **Automated Processing**: Processes all available configurations, struts, and locations
- **Miner's Rule Implementation**: Industry-standard damage accumulation methodology  
- **Comprehensive Visualization**: Multiple plot types for damage analysis
- **Quality Checks**: Built-in validation for damage rates and fatigue life
- **Parallel Processing**: Efficient computation for large datasets
- **Detailed Reporting**: Markdown reports with executive summary and recommendations

## Quick Start

### 1. Prerequisites
```bash
pip install pandas numpy matplotlib seaborn pyyaml
```

### 2. Run Analysis
```bash
# Using the runner script
python run_analysis.py

# Or directly with options
python cumulative_damage_analysis.py --config cumulative_damage_config.yml --log-level INFO
```

## File Structure

```
cummulative_damage/
├── cumulative_damage_config.yml    # Configuration file
├── cumulative_damage_analysis.py   # Main analysis module
├── run_analysis.py                  # Quick runner script
├── input/                          
│   └── fatigue_seastates_production.csv  # FC occurrence probabilities
├── sample_damage_results/          # Input damage rate files
│   └── {config}_FC{###}_Strut{#}_{location}_damage_rate.csv
└── output/                         # Generated results
    ├── individual_results/         # Per-component results
    ├── plots/                      # Visualizations
    ├── fatigue_life_summary.csv    # Summary table
    └── analysis_report.md          # Analysis report
```

## Input Data Format

### Damage Rate Files
CSV files with columns:
- `stress_range_mpa`: Stress range in MPa
- `cycles_annual`: Annual cycles
- `stress_corrected_mpa`: Corrected stress
- `cycles_to_failure`: Cycles to failure
- `damage_per_bin`: Damage per stress bin
- `damage_rate_per_year`: Annual damage rate

### Fatigue Conditions File
CSV file with columns:
- `Row`: Fatigue condition ID (FC001, FC002, etc.)
- `Wind Speed (m/s)`: Wind speed
- `Wind Dir (deg)`: Wind direction  
- `Hs (m)`: Significant wave height
- `Tp (s)`: Peak period
- `Wave Dir (deg)`: Wave direction
- `Occurrence (%)`: Occurrence probability

## Output Description

### Individual Results
For each configuration/strut/location:
- Damage rate by fatigue condition
- Weighted damage rates (damage × occurrence)
- Contribution percentages
- Total damage rate and fatigue life

### Summary File
Consolidated results with:
- Configuration, strut, location identifiers
- Total damage rate (1/year)
- Fatigue life (years)
- Critical fatigue condition
- Critical contribution percentage

### Visualizations
1. **Damage Contribution Bar Charts**: Shows weighted damage by FC with cumulative line
2. **Percentage Breakdown Pie Charts**: Visual breakdown of damage contributions
3. **Fatigue Life Heatmaps**: Matrix view of fatigue life (struts vs locations)
4. **Comparison Bar Charts**: Grouped comparison across components
5. **Damage Trend Lines**: Trends across struts for each location

### Analysis Report
Markdown report containing:
- Executive summary with key statistics
- Methodology description
- Results overview by configuration
- Critical findings (shortest fatigue life components)
- Most influential fatigue conditions
- Engineering recommendations

## Configuration Options

Key configuration parameters in `cumulative_damage_config.yml`:

```yaml
processing:
  configurations:
    mode: "all"           # Process all or specific configs
  methodology:
    damage_combination: "miners_rule"
    
output:
  individual_files:
    enabled: true         # Save individual results
  summary:
    enabled: true         # Generate summary file
  plots:
    enabled: true         # Create visualizations
    
execution:
  parallel_processing: true
  max_workers: 4         # Number of parallel workers
  
quality_checks:
  enabled: true          # Perform validation checks
```

## Methodology

The analysis follows these steps:

1. **Load Fatigue Conditions**: Read sea state occurrence probabilities
2. **Process Each Component**: For each config/strut/location:
   - Load damage rates for all fatigue conditions
   - Apply occurrence weighting: `weighted_damage = damage_rate × occurrence`
   - Sum using Miner's rule: `total_damage = Σ(weighted_damages)`
   - Calculate fatigue life: `life = 1 / total_damage`
3. **Generate Outputs**: Create results files, plots, and reports

## Quality Checks

The module includes validation for:
- Damage rate bounds (0 to 1)
- Fatigue life range (1 to 1000 years)
- Occurrence probability sum (~100%)

## Performance

- **Parallel Processing**: Utilizes multiple CPU cores
- **Memory Efficient**: Processes files incrementally
- **Scalable**: Handles hundreds of file combinations

## Troubleshooting

### Common Issues

1. **Missing Files**: Check log for skipped combinations
2. **Memory Issues**: Reduce `max_workers` in config
3. **Plot Generation**: Ensure matplotlib backend is configured

### Logging

Detailed logs are saved to `cumulative_damage_analysis.log`

## Next Steps

After running the analysis:

1. Review `analysis_report.md` for key findings
2. Check critical components in summary CSV
3. Examine individual plots for detailed damage breakdown
4. Use results for maintenance planning and design optimization

## Support

For issues or questions, consult:
- Log file: `cumulative_damage_analysis.log`
- Configuration: `cumulative_damage_config.yml`
- User specification: `user_spec.md`