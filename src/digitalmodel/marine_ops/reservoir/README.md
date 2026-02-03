# Reservoir Engineering Analysis Module

This module provides comprehensive tools for reservoir engineering calculations, modeling, and analysis within the DigitalModel framework.

## Features

### Core Capabilities
- **Rock and Fluid Properties**: Comprehensive petrophysical property calculations
- **Reservoir Modeling**: Material balance, well test analysis, and reservoir simulation
- **Log Analysis**: Well log interpretation, facies analysis, and petrophysical calculations
- **Stratigraphic Analysis**: Multi-well correlation and visualization
- **Production Forecasting**: Decline curve analysis and EUR estimation
- **Reservoir Characterization**: Integrated workflows for reservoir description

### Module Structure

```
reservoir/
├── __init__.py              # Public API and imports
├── properties.py           # Rock and fluid properties
├── modeling.py             # Reservoir models and simulations
├── analysis.py             # Data analysis and visualization
├── legacy_migration.py     # Migration utilities from legacy code
└── README.md              # This file
```

## Quick Start

### Basic Reservoir Properties

```python
from digitalmodel.reservoir import create_sandstone_reservoir

# Create a sandstone reservoir
reservoir = create_sandstone_reservoir(
    porosity=0.18,
    permeability=75,
    water_saturation=0.35,
    area=320,  # acres
    thickness=60,  # ft
    pressure=2800,
    temperature=160
)

# Calculate reserves
ooip = reservoir.calculate_ooip()
pore_volume = reservoir.calculate_pore_volume()
print(f"OOIP: {ooip:,.0f} STB")
print(f"Pore Volume: {pore_volume:,.0f} acre-ft")
```

### Well Log Analysis

```python
from digitalmodel.reservoir import LogAnalysis, LogCurve

# Create log analysis
log_analysis = LogAnalysis('WELL-001')

# Add log curves
depths = np.arange(2000, 2100, 0.5)
gr_values = 50 + 10 * np.random.normal(0, 1, len(depths))

gr_curve = LogCurve('GR', depths, gr_values, 'API')
log_analysis.add_curve(gr_curve)

# Calculate properties
porosity = log_analysis.calculate_porosity_from_logs()
facies = log_analysis.calculate_facies_from_logs(n_clusters=4)
net_pay = log_analysis.calculate_net_pay()

print(f"Net thickness: {net_pay['net_thickness']:.1f} ft")
print(f"Average porosity: {net_pay['average_porosity']:.1%}")
```

### Material Balance Analysis

```python
from digitalmodel.reservoir import ReservoirModel, ProductionData
from datetime import datetime, timedelta

# Create reservoir model
model = ReservoirModel(reservoir, "Example Field")

# Add production data
production_data = []
start_date = datetime(2020, 1, 1)

for i in range(365):
    date = start_date + timedelta(days=i)
    oil_rate = 100 * np.exp(-i / 500)  # Declining production

    prod_data = ProductionData(
        well_id='PROD-001',
        date=date,
        oil_rate=oil_rate,
        gas_rate=oil_rate * 600
    )
    production_data.append(prod_data)

model.material_balance.add_production_data(production_data)

# Perform material balance
current_pressure = 2600
mb_results = model.material_balance.tank_material_balance(current_pressure)

print(f"Pressure drop: {mb_results['pressure_drop']:.0f} psi")
print(f"Cumulative production: {mb_results['cumulative_oil']:,.0f} STB")
```

### Stratigraphic Correlation

```python
from digitalmodel.reservoir import StratigraphicAnalysis, WellData

# Create stratigraphic analysis
strat_analysis = StratigraphicAnalysis()

# Add wells with log analyses
for well_id in ['WELL-A', 'WELL-B', 'WELL-C']:
    # Create well data and log analysis (see examples for details)
    well_data = WellData(well_id, x_coord=1000, y_coord=1000, measured_depth=2500)
    log_analysis = LogAnalysis(well_id)
    # ... add curves and formations ...

    strat_analysis.add_well_analysis(well_id, log_analysis)

# Create cross-section plot
fig = strat_analysis.plot_stratigraphic_section(
    track_names=['GR', 'RT', 'RHOB_NPHI'],
    show_facies=True,
    flatten=True
)
plt.show()
```

## Examples

Comprehensive examples are available in the `examples/` directory:

- **reservoir_analysis_examples.py**: Complete workflow demonstrations
- Run with: `python examples/reservoir_analysis_examples.py`

## Testing

Run the test suite to validate functionality:

```bash
python tests/test_reservoir_framework.py
```

The test framework includes:
- Unit tests for individual components
- Integration tests for workflows
- Performance tests for large datasets
- Validation against known results

## Migration from Legacy Code

If you have existing reservoir analysis code, use the migration utilities:

```python
from digitalmodel.reservoir.legacy_migration import migrate_legacy_stratigraphic_plot

# Migrate legacy stratigraphic plots
fig = migrate_legacy_stratigraphic_plot(wells_list, df_logs, statdata)
```

## API Reference

### Properties Module
- `ReservoirProperties`: Main container for reservoir properties
- `RockProperties`: Rock matrix and pore properties
- `FluidProperties`: Oil, gas, and water properties
- `PVTProperties`: Pressure-volume-temperature relationships
- `create_sandstone_reservoir()`: Quick sandstone setup
- `create_carbonate_reservoir()`: Quick carbonate setup

### Modeling Module
- `ReservoirModel`: Integrated reservoir model container
- `MaterialBalance`: Material balance calculations
- `WellTestAnalysis`: Pressure transient analysis
- `ReservoirSimulation`: Numerical simulation
- `ProductionForecast`: Decline curve analysis

### Analysis Module
- `ReservoirAnalysis`: Main analysis container
- `LogAnalysis`: Well log interpretation
- `StratigraphicAnalysis`: Multi-well correlation
- `WellCorrelation`: Well-to-well correlation
- `PerformanceAnalysis`: Production performance analysis

## Dependencies

- numpy: Numerical computations
- pandas: Data manipulation
- matplotlib: Plotting and visualization
- scipy: Scientific computing
- scikit-learn: Machine learning (facies analysis)

## Advanced Features

### Facies Analysis
Automated facies classification using machine learning:
- K-means clustering of log responses
- Support for custom facies models
- Integration with stratigraphic analysis

### Property Mapping
Spatial property distribution:
- Kriging and interpolation methods
- Structure mapping
- Property correlation analysis

### Production Forecasting
Multiple decline curve methods:
- Arps exponential, hyperbolic, and harmonic
- Material balance integration
- Economic analysis support

### Well Test Analysis
Pressure transient interpretation:
- Buildup and drawdown analysis
- Permeability and skin calculation
- Multiple flow regimes

## Contributing

When adding new functionality:
1. Follow existing code patterns and style
2. Add comprehensive docstrings
3. Include type hints
4. Write unit tests
5. Update examples as needed

## Version History

- v1.0.0: Initial release with core functionality
- Migration from legacy domain structure
- Comprehensive test framework
- Example workflows and documentation

---

For more information, see the main DigitalModel documentation.