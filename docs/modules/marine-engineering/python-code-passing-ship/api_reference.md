# API Reference

## Module Overview

The `digitalmodel.modules.marine_analysis.python_code_passing_ship` module provides a comprehensive API for calculating hydrodynamic interaction forces between passing vessels.

## Core Classes

### PassingShipCalculator

The main calculation engine for passing ship forces.

```python
from digitalmodel.modules.marine_analysis.python_code_passing_ship import PassingShipCalculator
```

#### Constructor

```python
PassingShipCalculator(config_path: Union[str, Path, Dict])
```

**Parameters:**
- `config_path` (Union[str, Path, Dict]): Path to YAML configuration file or dictionary containing configuration

**Example:**
```python
# From file
calculator = PassingShipCalculator('config.yaml')

# From dictionary
config = {
    'vessel': {...},
    'environment': {...},
    'calculation': {...}
}
calculator = PassingShipCalculator(config)
```

#### Methods

##### calculate()

Performs the force calculation based on the provided configuration.

```python
def calculate(self, stagger_distance: Optional[float] = None) -> Dict[str, Any]
```

**Parameters:**
- `stagger_distance` (Optional[float]): Override the stagger distance from config. If None, uses full range.

**Returns:**
- `Dict[str, Any]`: Dictionary containing:
  - `surge_force` (np.ndarray): Surge force values [N]
  - `sway_force` (np.ndarray): Sway force values [N]
  - `yaw_moment` (np.ndarray): Yaw moment values [N·m]
  - `stagger_distances` (np.ndarray): Stagger distance points [m]
  - `metadata` (Dict): Calculation metadata and parameters

**Example:**
```python
results = calculator.calculate()
print(f"Max surge force: {results['surge_force'].max():.2f} N")
```

##### visualize()

Generates visualization plots of the calculated forces.

```python
def visualize(self, 
              results: Optional[Dict] = None,
              save_path: Optional[Union[str, Path]] = None,
              show: bool = True,
              format: str = 'png',
              dpi: int = 300) -> Optional[Figure]
```

**Parameters:**
- `results` (Optional[Dict]): Calculation results. If None, performs calculation first.
- `save_path` (Optional[Union[str, Path]]): Path to save the plot
- `show` (bool): Whether to display the plot interactively
- `format` (str): Output format ('png', 'pdf', 'svg')
- `dpi` (int): Resolution for raster formats

**Returns:**
- `Optional[Figure]`: Matplotlib figure object if not shown interactively

**Example:**
```python
# Generate and save visualization
calculator.visualize(save_path='forces.png', format='png', dpi=150)
```

##### batch_process()

Process multiple configurations in parallel.

```python
def batch_process(self,
                 configs: List[Union[str, Path, Dict]],
                 parallel: int = 4,
                 progress: bool = True) -> List[Dict[str, Any]]
```

**Parameters:**
- `configs` (List[Union[str, Path, Dict]]): List of configurations to process
- `parallel` (int): Number of parallel workers
- `progress` (bool): Show progress bar

**Returns:**
- `List[Dict[str, Any]]`: List of calculation results

**Example:**
```python
configs = ['config1.yaml', 'config2.yaml', 'config3.yaml']
results = calculator.batch_process(configs, parallel=3)
```

##### export()

Export results to various formats.

```python
def export(self,
          results: Dict[str, Any],
          output_path: Union[str, Path],
          format: str = 'json') -> None
```

**Parameters:**
- `results` (Dict[str, Any]): Calculation results to export
- `output_path` (Union[str, Path]): Output file path
- `format` (str): Export format ('json', 'csv', 'excel', 'mat')

**Example:**
```python
results = calculator.calculate()
calculator.export(results, 'output.json', format='json')
calculator.export(results, 'output.csv', format='csv')
```

---

## Configuration Classes

### VesselConfig

Vessel parameters configuration using Pydantic models.

```python
from digitalmodel.modules.marine_analysis.python_code_passing_ship.configuration import VesselConfig
```

```python
class VesselConfig(BaseModel):
    length: float  # Vessel length [m]
    beam: float    # Vessel beam [m]
    draft: float   # Vessel draft [m]
    displacement: Optional[float] = None  # Displacement [tonnes]
    block_coefficient: float = 0.85  # Block coefficient [-]
    
    @validator('length', 'beam', 'draft')
    def positive_dimensions(cls, v):
        if v <= 0:
            raise ValueError("Dimensions must be positive")
        return v
```

### EnvironmentConfig

Environmental conditions configuration.

```python
from digitalmodel.modules.marine_analysis.python_code_passing_ship.configuration import EnvironmentConfig
```

```python
class EnvironmentConfig(BaseModel):
    water_depth: Union[float, Literal['infinite']]  # Water depth [m] or 'infinite'
    water_density: float = 1025.0  # Water density [kg/m³]
    separation_distance: float  # Lateral separation [m]
    current_velocity: float = 0.0  # Ambient current [m/s]
    
    @validator('water_depth')
    def validate_depth(cls, v):
        if isinstance(v, str) and v != 'infinite':
            raise ValueError("String value must be 'infinite'")
        if isinstance(v, (int, float)) and v <= 0:
            raise ValueError("Water depth must be positive")
        return v
```

### CalculationConfig

Calculation settings and parameters.

```python
from digitalmodel.modules.marine_analysis.python_code_passing_ship.configuration import CalculationConfig
```

```python
class CalculationConfig(BaseModel):
    stagger_range: Tuple[float, float] = (-400.0, 400.0)  # Stagger range [m]
    stagger_points: int = 81  # Number of calculation points
    use_finite_depth_correction: bool = True  # Apply depth corrections
    harmonic_terms: int = 10  # Number of harmonic terms
    integration_tolerance: float = 1e-6  # Numerical integration tolerance
    use_cache: bool = True  # Enable result caching
    
    @validator('stagger_points')
    def validate_points(cls, v):
        if v < 2:
            raise ValueError("Need at least 2 points")
        return v
```

---

## Mathematical Functions

### Sectional Area Functions

```python
from digitalmodel.modules.marine_analysis.python_code_passing_ship.formulations import (
    sectional_area_s1,
    sectional_area_s2
)
```

#### sectional_area_s1()

Calculates the sectional area curve for the moored vessel.

```python
def sectional_area_s1(x: float, L1: float, B1: float, T1: float) -> float
```

**Parameters:**
- `x` (float): Longitudinal position [-L/2, L/2]
- `L1` (float): Moored vessel length [m]
- `B1` (float): Moored vessel beam [m]
- `T1` (float): Moored vessel draft [m]

**Returns:**
- `float`: Sectional area [m²]

#### sectional_area_s2()

Calculates the sectional area curve for the passing vessel.

```python
def sectional_area_s2(xi: float, L2: float, B2: float, T2: float) -> float
```

**Parameters:**
- `xi` (float): Longitudinal position [-L/2, L/2]
- `L2` (float): Passing vessel length [m]
- `B2` (float): Passing vessel beam [m]
- `T2` (float): Passing vessel draft [m]

**Returns:**
- `float`: Sectional area [m²]

### Kernel Functions

```python
from digitalmodel.modules.marine_analysis.python_code_passing_ship.formulations import (
    kernel_function_f,
    kernel_function_g
)
```

#### kernel_function_f()

Calculates the F kernel integral for force computation.

```python
def kernel_function_f(x: float, s: float, y0: float, params: Dict) -> float
```

**Parameters:**
- `x` (float): Integration variable (moored vessel position)
- `s` (float): Stagger distance [m]
- `y0` (float): Lateral separation [m]
- `params` (Dict): Vessel parameters

**Returns:**
- `float`: F kernel value

#### kernel_function_g()

Calculates the G kernel integral for moment computation.

```python
def kernel_function_g(x: float, s: float, y0: float, params: Dict) -> float
```

**Parameters:**
- `x` (float): Integration variable
- `s` (float): Stagger distance [m]
- `y0` (float): Lateral separation [m]
- `params` (Dict): Vessel parameters

**Returns:**
- `float`: G kernel value

### Force Calculations

```python
from digitalmodel.modules.marine_analysis.python_code_passing_ship.formulations import (
    calculate_surge_force,
    calculate_sway_force,
    calculate_yaw_moment
)
```

#### calculate_surge_force()

Computes the total surge force on the moored vessel.

```python
def calculate_surge_force(s: float, params: Dict) -> float
```

**Parameters:**
- `s` (float): Stagger distance [m]
- `params` (Dict): Complete calculation parameters

**Returns:**
- `float`: Surge force [N]

#### calculate_sway_force()

Computes the total sway force on the moored vessel.

```python
def calculate_sway_force(s: float, params: Dict) -> float
```

**Parameters:**
- `s` (float): Stagger distance [m]
- `params` (Dict): Complete calculation parameters

**Returns:**
- `float`: Sway force [N]

#### calculate_yaw_moment()

Computes the total yaw moment on the moored vessel.

```python
def calculate_yaw_moment(s: float, params: Dict) -> float
```

**Parameters:**
- `s` (float): Stagger distance [m]
- `params` (Dict): Complete calculation parameters

**Returns:**
- `float`: Yaw moment [N·m]

---

## Visualization Functions

```python
from digitalmodel.modules.marine_analysis.python_code_passing_ship.visualization import (
    plot_forces,
    plot_parametric_study,
    create_comparison_plot
)
```

### plot_forces()

Creates a standard three-panel plot of forces vs stagger distance.

```python
def plot_forces(results: Dict[str, Any],
               title: str = "Passing Ship Forces",
               save_path: Optional[str] = None) -> Figure
```

**Parameters:**
- `results` (Dict[str, Any]): Calculation results dictionary
- `title` (str): Plot title
- `save_path` (Optional[str]): Path to save the figure

**Returns:**
- `Figure`: Matplotlib figure object

### plot_parametric_study()

Creates visualization for parametric sensitivity analysis.

```python
def plot_parametric_study(parameter_name: str,
                         parameter_values: List[float],
                         results_list: List[Dict],
                         save_path: Optional[str] = None) -> Figure
```

**Parameters:**
- `parameter_name` (str): Name of varied parameter
- `parameter_values` (List[float]): Parameter values tested
- `results_list` (List[Dict]): Results for each parameter value
- `save_path` (Optional[str]): Path to save the figure

**Returns:**
- `Figure`: Matplotlib figure object

### create_comparison_plot()

Creates comparison plots for multiple scenarios.

```python
def create_comparison_plot(scenarios: Dict[str, Dict],
                          force_type: str = 'surge',
                          save_path: Optional[str] = None) -> Figure
```

**Parameters:**
- `scenarios` (Dict[str, Dict]): Dictionary of scenario names and results
- `force_type` (str): Force component to plot ('surge', 'sway', 'yaw')
- `save_path` (Optional[str]): Path to save the figure

**Returns:**
- `Figure`: Matplotlib figure object

---

## Export Functions

```python
from digitalmodel.modules.marine_analysis.python_code_passing_ship.exporters import (
    export_to_json,
    export_to_csv,
    export_to_orcaflex,
    export_to_aqwa
)
```

### export_to_json()

Export results to JSON format.

```python
def export_to_json(results: Dict[str, Any],
                  output_path: Union[str, Path],
                  indent: int = 2) -> None
```

**Parameters:**
- `results` (Dict[str, Any]): Calculation results
- `output_path` (Union[str, Path]): Output file path
- `indent` (int): JSON indentation level

### export_to_csv()

Export results to CSV format.

```python
def export_to_csv(results: Dict[str, Any],
                 output_path: Union[str, Path]) -> None
```

**Parameters:**
- `results` (Dict[str, Any]): Calculation results
- `output_path` (Union[str, Path]): Output file path

### export_to_orcaflex()

Export forces as OrcaFlex-compatible time series.

```python
def export_to_orcaflex(results: Dict[str, Any],
                      output_path: Union[str, Path],
                      time_step: float = 0.1,
                      duration: float = 100.0,
                      vessel_speed: float = 5.0) -> None
```

**Parameters:**
- `results` (Dict[str, Any]): Calculation results
- `output_path` (Union[str, Path]): Output file path
- `time_step` (float): Time step for time series [s]
- `duration` (float): Total duration [s]
- `vessel_speed` (float): Passing vessel speed [m/s]

### export_to_aqwa()

Export forces for AQWA analysis.

```python
def export_to_aqwa(results: Dict[str, Any],
                  output_path: Union[str, Path],
                  reference_point: Tuple[float, float, float] = (0, 0, 0)) -> None
```

**Parameters:**
- `results` (Dict[str, Any]): Calculation results
- `output_path` (Union[str, Path]): Output file path
- `reference_point` (Tuple[float, float, float]): Force reference point [m]

---

## CLI Interface

The module provides a comprehensive command-line interface following repository standards.

### Basic Commands

```bash
# Show help
python -m digitalmodel.modules.marine_analysis.python_code_passing_ship --help

# Single calculation
python -m digitalmodel.modules.marine_analysis.python_code_passing_ship \
    --config config.yaml \
    --output-directory results/

# Batch processing
python -m digitalmodel.modules.marine_analysis.python_code_passing_ship \
    --input-directory configs/ \
    --pattern "*.yaml" \
    --output-directory results/ \
    --parallel 4
```

### CLI Parameters

| Parameter | Alias | Type | Description |
|-----------|-------|------|-------------|
| `--config` | `-c` | str | Configuration file path |
| `--input-directory` | `--directory`, `-d` | str | Input directory for batch processing |
| `--output-directory` | `--output`, `-o` | str | Output directory for results |
| `--pattern` | `-p` | str | File pattern for batch processing |
| `--recursive` | `-r` | flag | Search directories recursively |
| `--parallel` | | int | Number of parallel workers |
| `--visualize` | `-v` | flag | Generate visualization plots |
| `--format` | `-f` | str | Output format (json, csv, excel) |
| `--verbose` | | flag | Enable verbose output |
| `--dry-run` | | flag | Preview without execution |

### CLI Examples

```bash
# Generate visualization with specific format
python -m digitalmodel.modules.marine_analysis.python_code_passing_ship \
    --config tanker.yaml \
    --visualize \
    --format png \
    --output-directory plots/

# Batch process with CSV output
python -m digitalmodel.modules.marine_analysis.python_code_passing_ship \
    --input-directory scenarios/ \
    --pattern "scenario_*.yaml" \
    --format csv \
    --output-directory csv_results/ \
    --parallel 8

# Dry run to preview operations
python -m digitalmodel.modules.marine_analysis.python_code_passing_ship \
    --input-directory configs/ \
    --dry-run \
    --verbose
```

---

## Error Handling

The module implements comprehensive error handling with custom exceptions:

```python
from digitalmodel.modules.marine_analysis.python_code_passing_ship.exceptions import (
    ConfigurationError,
    CalculationError,
    ConvergenceError,
    ValidationError
)
```

### ConfigurationError

Raised when configuration parsing or validation fails.

```python
try:
    calculator = PassingShipCalculator('invalid_config.yaml')
except ConfigurationError as e:
    print(f"Configuration error: {e}")
```

### CalculationError

Raised when calculation encounters an error.

```python
try:
    results = calculator.calculate()
except CalculationError as e:
    print(f"Calculation failed: {e}")
```

### ConvergenceError

Raised when numerical integration fails to converge.

```python
try:
    force = calculate_surge_force(s, params)
except ConvergenceError as e:
    print(f"Integration did not converge: {e}")
    # Retry with relaxed tolerance
    params['integration_tolerance'] = 1e-4
    force = calculate_surge_force(s, params)
```

### ValidationError

Raised when input validation fails.

```python
try:
    config = VesselConfig(length=-100)  # Invalid negative length
except ValidationError as e:
    print(f"Validation error: {e}")
```

---

## Performance Considerations

### Caching

The module implements intelligent caching to avoid redundant calculations:

```python
# Enable caching (default)
calculator = PassingShipCalculator(config)
calculator.enable_cache = True

# Clear cache when parameters change significantly
calculator.clear_cache()
```

### Parallel Processing

For batch operations, use parallel processing:

```python
from concurrent.futures import ProcessPoolExecutor

def process_config(config_path):
    calc = PassingShipCalculator(config_path)
    return calc.calculate()

# Process multiple configs in parallel
with ProcessPoolExecutor(max_workers=4) as executor:
    configs = ['config1.yaml', 'config2.yaml', 'config3.yaml']
    results = list(executor.map(process_config, configs))
```

### Memory Management

For large batch operations:

```python
# Process in chunks to manage memory
def process_batch_chunked(configs, chunk_size=100):
    results = []
    for i in range(0, len(configs), chunk_size):
        chunk = configs[i:i+chunk_size]
        chunk_results = calculator.batch_process(chunk)
        results.extend(chunk_results)
        # Optional: Write intermediate results
        export_to_json(chunk_results, f'results_chunk_{i}.json')
    return results
```

---

## Integration Examples

### Integration with Ship Design Module

```python
from digitalmodel.modules.ship_design import ShipDesigner
from digitalmodel.modules.marine_analysis.python_code_passing_ship import PassingShipCalculator

# Use ship design parameters
designer = ShipDesigner()
vessel_params = designer.get_vessel_parameters()

# Configure passing ship calculation
config = {
    'vessel': {
        'moored': vessel_params,
        'passing': {...}
    },
    'environment': {...}
}

calculator = PassingShipCalculator(config)
forces = calculator.calculate()
```

### Integration with Mooring Analysis

```python
from digitalmodel.modules.mooring_analysis import MooringAnalyzer
from digitalmodel.modules.marine_analysis.python_code_passing_ship import PassingShipCalculator

# Calculate passing ship forces
calc = PassingShipCalculator('config.yaml')
forces = calc.calculate()

# Apply to mooring analysis
mooring = MooringAnalyzer()
mooring.add_external_forces(
    surge=forces['surge_force'],
    sway=forces['sway_force'],
    yaw=forces['yaw_moment']
)
mooring.analyze()
```

---

## Version Information

```python
from digitalmodel.modules.marine_analysis.python_code_passing_ship import __version__

print(f"Module version: {__version__}")
# Output: Module version: 1.0.0
```

## Module Constants

```python
from digitalmodel.modules.marine_analysis.python_code_passing_ship.constants import (
    DEFAULT_WATER_DENSITY,
    DEFAULT_BLOCK_COEFFICIENT,
    MAX_HARMONIC_TERMS,
    INTEGRATION_LIMITS
)

print(f"Default water density: {DEFAULT_WATER_DENSITY} kg/m³")
print(f"Default block coefficient: {DEFAULT_BLOCK_COEFFICIENT}")
print(f"Maximum harmonic terms: {MAX_HARMONIC_TERMS}")
print(f"Integration limits: {INTEGRATION_LIMITS}")
```