# Phase 1 API Reference
## Digital Model - Engineering Asset Lifecycle Management

**Version:** 2.0.0
**Date:** October 3, 2025

---

## Table of Contents

1. [Fatigue Analysis Module](#1-fatigue-analysis-module)
2. [Marine Analysis Module](#2-marine-analysis-module)
3. [Mooring Module](#3-mooring-module)
4. [Common Utilities](#4-common-utilities)

---

## 1. Fatigue Analysis Module

### 1.1 SNCurvePlotter

**Module:** `digitalmodel.fatigue.sn_curve_plotter`

Main class for S-N curve plotting and analysis.

#### Class: SNCurvePlotter

```python
class SNCurvePlotter:
    """
    S-N Curve Plotter for fatigue analysis.

    Generates S-N curve plots with various options matching the reference
    Excel file functionality.
    """
```

##### Constructor

```python
def __init__(self, data_path: Optional[str] = None)
```

**Description:** Initialize the S-N curve plotter.

**Parameters:**
- `data_path` (str, optional): Path to fatigue_curves_structured.csv. If None, uses default repository path.

**Raises:**
- `FileNotFoundError`: If data file not found
- `ValueError`: If required columns missing from data

**Example:**
```python
# Use default data path
plotter = SNCurvePlotter()

# Use custom data path
plotter = SNCurvePlotter(data_path='/path/to/custom_curves.csv')
```

---

##### Method: get_curve_data

```python
def get_curve_data(self, lookup_index: int) -> Dict
```

**Description:** Get S-N curve parameters for a specific lookup index.

**Parameters:**
- `lookup_index` (int): The lookup index of the curve (1-221)

**Returns:**
- `Dict`: Dictionary containing all curve parameters

**Raises:**
- `ValueError`: If lookup_index not found in database

**Return Dictionary Structure:**
```python
{
    'Lookup Index': int,
    'Curve Type': str,
    'Joint Type': str,
    'In air (or) Corrosion Protected in seawater (or) Free Corrosion in seawater': str,
    '# of Slopes': int,
    'Log a1~': float,
    'm1': float,
    'Log a2~': float,  # if applicable
    'm2': float,       # if applicable
    'Transfer Cycles 1': float,
    'Transfer Stress 1 (Mpa)': float,
    'Thickness Correction (mm)': float,
    'Thickness Coefficient': float,
    'References Used': str,
    'Special Notes*': str
}
```

**Example:**
```python
curve = plotter.get_curve_data(64)
print(f"Curve Type: {curve['Curve Type']}")
print(f"Number of Slopes: {curve['# of Slopes']}")
print(f"Log a1: {curve['Log a1~']}, m1: {curve['m1']}")
```

---

##### Method: calculate_sn_points

```python
def calculate_sn_points(
    self,
    lookup_index: int,
    n_min: float = 1e3,
    n_max: float = 1e10,
    num_points: int = 1000,
    scf: float = 1.0,
    include_fatigue_limit: bool = True
) -> Tuple[np.ndarray, np.ndarray]
```

**Description:** Calculate S-N curve data points for plotting or analysis.

**Parameters:**
- `lookup_index` (int): The lookup index of the curve (1-221)
- `n_min` (float, optional): Minimum number of cycles. Default: 1e3
- `n_max` (float, optional): Maximum number of cycles. Default: 1e10
- `num_points` (int, optional): Number of points to generate. Default: 1000
- `scf` (float, optional): Stress concentration factor. Default: 1.0
- `include_fatigue_limit` (bool, optional): Include fatigue limit cut-off. Default: True

**Returns:**
- `Tuple[np.ndarray, np.ndarray]`: (N_cycles, stress_range_MPa)

**Raises:**
- `ValueError`: If lookup_index not found or invalid parameters

**Mathematical Formula:**
```
S = (10^log_a / N)^(1/m)

With SCF:
S_design = S / SCF

With Fatigue Limit:
S_design = max(S_design, S_fatigue_limit / SCF)
```

**Example:**
```python
# Basic calculation
N, S = plotter.calculate_sn_points(lookup_index=64)

# With stress concentration factor
N, S = plotter.calculate_sn_points(
    lookup_index=64,
    scf=2.0,
    include_fatigue_limit=True
)

# Custom cycle range
N, S = plotter.calculate_sn_points(
    lookup_index=64,
    n_min=1e4,
    n_max=1e8,
    num_points=500
)

# Find fatigue life at specific stress
target_stress = 100  # MPa
idx = np.argmin(np.abs(S - target_stress))
fatigue_life = N[idx]
print(f"Life at {target_stress} MPa: {fatigue_life:.2e} cycles")
```

---

##### Method: plot_curves

```python
def plot_curves(
    self,
    lookup_indices: Union[int, List[int]],
    scf: float = 1.0,
    include_fatigue_limit: bool = True,
    plot_type: str = 'log-log',
    figsize: Tuple[int, int] = (12, 8),
    title: Optional[str] = None,
    save_path: Optional[str] = None,
    show_plot: bool = True
) -> plt.Figure
```

**Description:** Plot S-N curves for comparison.

**Parameters:**
- `lookup_indices` (int or List[int]): Single index or list of indices to plot
- `scf` (float, optional): Stress concentration factor. Default: 1.0
- `include_fatigue_limit` (bool, optional): Include fatigue limit. Default: True
- `plot_type` (str, optional): 'log-log' or 'linear-log'. Default: 'log-log'
- `figsize` (Tuple[int, int], optional): Figure size (width, height). Default: (12, 8)
- `title` (str, optional): Plot title. Auto-generated if None
- `save_path` (str, optional): Path to save figure. None = don't save
- `show_plot` (bool, optional): Display the plot. Default: True

**Returns:**
- `plt.Figure`: Matplotlib figure object

**Raises:**
- `ValueError`: If invalid plot_type or lookup_indices

**Example:**
```python
# Plot single curve
plotter.plot_curves(lookup_indices=64)

# Plot multiple curves
plotter.plot_curves(
    lookup_indices=[64, 175, 183],
    scf=1.5,
    plot_type='log-log',
    title='API vs BS vs BV S-N Curves',
    save_path='outputs/comparison.png'
)

# Linear-log plot without fatigue limit
plotter.plot_curves(
    lookup_indices=[1, 5, 10],
    include_fatigue_limit=False,
    plot_type='linear-log',
    figsize=(14, 10),
    show_plot=False
)
```

---

##### Method: list_curves

```python
def list_curves(
    self,
    curve_type_filter: Optional[str] = None,
    environment_filter: Optional[str] = None,
    joint_type_filter: Optional[str] = None
) -> pd.DataFrame
```

**Description:** List available curves with optional filtering.

**Parameters:**
- `curve_type_filter` (str, optional): Filter by curve type (e.g., 'DNV', 'API', 'BS')
- `environment_filter` (str, optional): Filter by environment (e.g., 'Air', 'Seawater')
- `joint_type_filter` (str, optional): Filter by joint type (e.g., 'Plated', 'Tubular')

**Returns:**
- `pd.DataFrame`: Filtered dataframe with columns:
  - Lookup Index
  - Curve Type
  - Joint Type
  - Environment
  - # of Slopes

**Example:**
```python
# List all DNV curves
dnv_curves = plotter.list_curves(curve_type_filter='DNV')
print(dnv_curves)

# List curves in air
air_curves = plotter.list_curves(environment_filter='Air')

# List DNV curves in seawater
dnv_seawater = plotter.list_curves(
    curve_type_filter='DNV',
    environment_filter='Seawater'
)

# List tubular joints
tubular = plotter.list_curves(joint_type_filter='Tubular')
```

---

##### Method: create_comparison_plot

```python
def create_comparison_plot(
    self,
    reference_index: int,
    comparison_indices: List[int],
    scf: float = 1.0,
    include_fatigue_limit: bool = True,
    plot_type: str = 'log-log',
    save_path: Optional[str] = None
) -> plt.Figure
```

**Description:** Create a comparison plot highlighting a reference curve.

**Parameters:**
- `reference_index` (int): The reference curve to highlight (plotted in red)
- `comparison_indices` (List[int]): List of curves to compare against
- `scf` (float, optional): Stress concentration factor. Default: 1.0
- `include_fatigue_limit` (bool, optional): Include fatigue limit. Default: True
- `plot_type` (str, optional): 'log-log' or 'linear-log'. Default: 'log-log'
- `save_path` (str, optional): Path to save figure

**Returns:**
- `plt.Figure`: Matplotlib figure object

**Example:**
```python
# Compare API reference against other standards
plotter.create_comparison_plot(
    reference_index=64,  # API X curve
    comparison_indices=[5, 10, 175, 183],  # DNV, BS, BV
    scf=2.0,
    save_path='outputs/standards_comparison.png'
)
```

---

## 2. Marine Analysis Module

### 2.1 RAODataProcessor

**Module:** `digitalmodel.modules.marine_analysis.rao_processor`

#### Class: RAODataProcessor

```python
class RAODataProcessor:
    """Process Response Amplitude Operator data from various sources."""
```

##### Constructor

```python
def __init__(self, config: Optional[Dict[str, Any]] = None)
```

**Parameters:**
- `config` (Dict, optional): Configuration dictionary

**Example:**
```python
processor = RAODataProcessor()

# With configuration
config = {'validation': {'strict_mode': True}}
processor = RAODataProcessor(config=config)
```

---

##### Method: import_aqwa_lis_file

```python
def import_aqwa_lis_file(
    self,
    file_path: str,
    use_enhanced_parser: bool = True
) -> RAOData
```

**Description:** Import RAO data from ANSYS AQWA .lis file.

**Parameters:**
- `file_path` (str): Path to ANSYS AQWA .lis output file
- `use_enhanced_parser` (bool, optional): Use enhanced parser. Default: True

**Returns:**
- `RAOData`: RAO data object with 6-DOF displacement RAOs

**Raises:**
- `RAOImportError`: If file cannot be parsed or data is invalid
- `FileNotFoundError`: If file not found

**Example:**
```python
# Basic import
rao_data = processor.import_aqwa_lis_file('vessel_analysis.lis')

# With standard parser
rao_data = processor.import_aqwa_lis_file(
    'vessel_analysis.lis',
    use_enhanced_parser=False
)

print(f"Vessel: {rao_data.vessel_name}")
print(f"Frequencies: {rao_data.frequencies}")
print(f"Headings: {rao_data.headings}")
```

---

### 2.2 RAOData

**Module:** `digitalmodel.modules.marine_analysis.rao_processor`

#### Dataclass: RAOData

```python
@dataclass
class RAOData:
    """Container for RAO data with metadata."""
    frequencies: np.ndarray      # rad/s
    headings: np.ndarray         # degrees
    raos: Dict[str, Dict[str, np.ndarray]]  # {dof: {'amplitude': array, 'phase': array}}
    units: Dict[str, str]
    source_file: str
    vessel_name: str
    analysis_date: str
    metadata: Dict[str, Any]
```

**Attributes:**
- `frequencies` (np.ndarray): Frequencies in rad/s
- `headings` (np.ndarray): Wave headings in degrees (0-360)
- `raos` (Dict): RAO data for each DOF
  - Structure: `{'surge': {'amplitude': array, 'phase': array}, ...}`
  - DOFs: surge, sway, heave, roll, pitch, yaw
- `units` (Dict): Unit definitions for each DOF
- `source_file` (str): Origin file path
- `vessel_name` (str): Vessel identifier
- `analysis_date` (str): Analysis timestamp
- `metadata` (Dict): Additional metadata

**Methods:**

```python
def is_valid(self) -> bool:
    """Check if RAO data is valid and complete."""
```

**Example:**
```python
# Access RAO data
surge_amplitude = rao_data.raos['surge']['amplitude']  # Shape: (n_freq, n_heading)
surge_phase = rao_data.raos['surge']['phase']

# Check validity
if rao_data.is_valid():
    print("✅ Complete 6-DOF data")
else:
    print("⚠️ Missing DOF data")

# Access specific point
freq_idx = 5
head_idx = 0
heave_amp = rao_data.raos['heave']['amplitude'][freq_idx, head_idx]
heave_phase = rao_data.raos['heave']['phase'][freq_idx, head_idx]

print(f"Heave RAO at freq={rao_data.frequencies[freq_idx]:.2f} rad/s, "
      f"heading={rao_data.headings[head_idx]}°:")
print(f"  Amplitude: {heave_amp:.3f} {rao_data.units['heave']}")
print(f"  Phase: {heave_phase:.1f}°")
```

---

### 2.3 RAODataValidators

**Module:** `digitalmodel.modules.marine_analysis.rao_validators`

#### Class: RAODataValidators

```python
class RAODataValidators:
    """Validators for RAO data quality and physical constraints."""
```

##### Method: validate_rao_data

```python
def validate_rao_data(self, rao_data: RAOData) -> ValidationReport
```

**Description:** Comprehensive validation of RAO data.

**Parameters:**
- `rao_data` (RAOData): RAO data to validate

**Returns:**
- `ValidationReport`: Validation results with warnings and errors

**Validation Checks:**
1. Physical constraints (frequency > 0, heading 0-360°)
2. Data completeness (all DOFs present)
3. Array shape consistency
4. Amplitude range validation
5. Phase continuity
6. Symmetry checks

**Example:**
```python
validators = RAODataValidators()
report = validators.validate_rao_data(rao_data)

if report.is_valid:
    print("✅ Validation passed")
else:
    print("⚠️ Validation issues:")
    for error in report.errors:
        print(f"  ERROR: {error}")
    for warning in report.warnings:
        print(f"  WARNING: {warning}")

# Access specific checks
if report.has_symmetry_issues:
    print("Symmetry issues detected:")
    for issue in report.symmetry_issues:
        print(f"  {issue}")
```

---

### 2.4 RAOInterpolator

**Module:** `digitalmodel.modules.marine_analysis.rao_interpolator`

#### Class: RAOInterpolator

```python
class RAOInterpolator:
    """Interpolate RAO data to different frequency/heading grids."""
```

##### Method: interpolate_to_standard_grid

```python
def interpolate_to_standard_grid(
    self,
    rao_data: RAOData,
    target_frequencies: np.ndarray,
    target_headings: np.ndarray,
    method: str = 'cubic'
) -> RAOData
```

**Description:** Interpolate RAO data to a standard grid.

**Parameters:**
- `rao_data` (RAOData): Original RAO data
- `target_frequencies` (np.ndarray): Target frequencies in rad/s
- `target_headings` (np.ndarray): Target headings in degrees
- `method` (str, optional): Interpolation method ('cubic', 'linear'). Default: 'cubic'

**Returns:**
- `RAOData`: Interpolated RAO data on new grid

**Raises:**
- `ValueError`: If target grid outside source data range

**Example:**
```python
interpolator = RAOInterpolator()

# Define standard grid
standard_freq = np.arange(0.2, 1.2, 0.1)  # 0.2 to 1.1 rad/s
standard_head = np.arange(0, 360, 15)     # Every 15 degrees

# Interpolate
standardized = interpolator.interpolate_to_standard_grid(
    rao_data,
    target_frequencies=standard_freq,
    target_headings=standard_head,
    method='cubic'
)

print(f"Original grid: {len(rao_data.frequencies)} x {len(rao_data.headings)}")
print(f"New grid: {len(standardized.frequencies)} x {len(standardized.headings)}")
```

---

## 3. Mooring Module

### 3.1 Mooring

**Module:** `digitalmodel.modules.mooring.mooring`

#### Class: Mooring

```python
class Mooring:
    """Mooring system analysis framework."""
```

##### Constructor

```python
def __init__(self)
```

**Example:**
```python
mooring = Mooring()
```

---

##### Method: router

```python
def router(self, cfg_base: Dict) -> Dict
```

**Description:** Route configuration to appropriate mooring analysis.

**Parameters:**
- `cfg_base` (Dict): Configuration dictionary with mooring parameters

**Returns:**
- `Dict`: Analysis results

**Configuration Structure:**
```python
{
    'mooring': {
        'flag': bool,           # Enable mooring analysis
        'type': str,            # Mooring type (e.g., 'SALM')
        'analysis_type': str,   # 'static' or 'dynamic'
        'vessel': {
            'name': str,
            'draft': float,     # m
            'displacement': float  # tonnes
        },
        'environment': {
            'water_depth': float,  # m
            'current_profile': str,
            'max_current': float,  # m/s
            'wave': {
                'Hs': float,    # m
                'Tp': float     # s
            }
        },
        'mooring_lines': [
            {
                'name': str,
                'azimuth': float,   # degrees
                'length': float,    # m
                'diameter': float,  # m
                'material': str
            }
        ],
        'output': {
            'file_format': str,
            'save_path': str
        }
    }
}
```

**Example:**
```python
config = {
    'mooring': {
        'flag': True,
        'type': 'SALM',
        'analysis_type': 'static',
        'vessel': {
            'name': 'FPSO_001',
            'draft': 20.0,
            'displacement': 150000
        },
        'environment': {
            'water_depth': 1500,
            'current_profile': 'linear',
            'max_current': 1.5,
            'wave': {'Hs': 5.0, 'Tp': 12.0}
        },
        'mooring_lines': [
            {
                'name': 'Line_1',
                'azimuth': 0,
                'length': 2000,
                'diameter': 0.15,
                'material': 'Chain_R3'
            }
        ],
        'output': {
            'file_format': 'csv',
            'save_path': 'outputs/mooring_results.csv'
        }
    }
}

results = mooring.router(config)
print(f"Max tension: {results['max_tension']:.1f} kN")
```

---

## 4. Common Utilities

### 4.1 Data Validation

**Module:** `digitalmodel.common`

#### Function: validate_numeric_input

```python
def validate_numeric_input(
    value: Any,
    min_value: Optional[float] = None,
    max_value: Optional[float] = None,
    allow_none: bool = False
) -> float
```

**Description:** Validate numeric input with range checking.

**Parameters:**
- `value` (Any): Value to validate
- `min_value` (float, optional): Minimum allowed value
- `max_value` (float, optional): Maximum allowed value
- `allow_none` (bool, optional): Allow None values. Default: False

**Returns:**
- `float`: Validated numeric value

**Raises:**
- `ValueError`: If value outside range or invalid type

**Example:**
```python
# Basic validation
scf = validate_numeric_input(2.5, min_value=0.1, max_value=10.0)

# With None allowed
optional_value = validate_numeric_input(None, allow_none=True)
```

---

### 4.2 File Operations

#### Function: ensure_directory_exists

```python
def ensure_directory_exists(file_path: str) -> None
```

**Description:** Ensure output directory exists for file path.

**Parameters:**
- `file_path` (str): File path to check

**Example:**
```python
ensure_directory_exists('outputs/plots/figure.png')
# Creates 'outputs/plots/' if it doesn't exist
```

---

## 5. Exception Classes

### 5.1 RAOImportError

**Module:** `digitalmodel.modules.marine_analysis.rao_processor`

```python
class RAOImportError(Exception):
    """User-friendly RAO import error with suggested solutions."""

    def __init__(self, message: str, suggestions: List[str] = None):
        self.message = message
        self.suggestions = suggestions or []

    def user_message(self) -> str:
        """Format error message for user interface."""
```

**Attributes:**
- `message` (str): Error description
- `suggestions` (List[str]): Suggested solutions

**Example:**
```python
try:
    rao_data = processor.import_aqwa_lis_file('invalid_file.lis')
except RAOImportError as e:
    print(e.user_message())
    # Output includes message and numbered suggestions
```

---

## 6. Type Definitions

### 6.1 Common Types

```python
# Frequency array (rad/s)
Frequencies = np.ndarray  # Shape: (n_frequencies,)

# Heading array (degrees)
Headings = np.ndarray     # Shape: (n_headings,)

# RAO amplitude/phase arrays
RAOArray = np.ndarray     # Shape: (n_frequencies, n_headings)

# S-N curve points
SNPoints = Tuple[np.ndarray, np.ndarray]  # (N_cycles, stress_MPa)

# Configuration dictionary
Config = Dict[str, Any]
```

---

## 7. Constants

### 7.1 Fatigue Module Constants

```python
# Default cycle range
DEFAULT_N_MIN = 1e3
DEFAULT_N_MAX = 1e10

# Default number of points
DEFAULT_NUM_POINTS = 1000

# Default plot size
DEFAULT_FIGSIZE = (12, 8)

# Supported plot types
PLOT_TYPES = ['log-log', 'linear-log']
```

### 7.2 Marine Analysis Constants

```python
# DOF names
DOF_NAMES = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']

# Default units
DEFAULT_UNITS = {
    'frequency': 'rad/s',
    'heading': 'deg',
    'surge': 'm/m',
    'sway': 'm/m',
    'heave': 'm/m',
    'roll': 'deg/m',
    'pitch': 'deg/m',
    'yaw': 'deg/m'
}

# Interpolation methods
INTERPOLATION_METHODS = ['cubic', 'linear']
```

---

## 8. Usage Patterns

### 8.1 Complete Fatigue Workflow

```python
from digitalmodel.fatigue.sn_curve_plotter import SNCurvePlotter
import numpy as np

# 1. Initialize
plotter = SNCurvePlotter()

# 2. Explore curves
curves = plotter.list_curves(curve_type_filter='DNV')
print(f"Found {len(curves)} DNV curves")

# 3. Get curve details
curve_64 = plotter.get_curve_data(64)
print(f"Curve: {curve_64['Curve Type']}")

# 4. Calculate points
N, S = plotter.calculate_sn_points(64, scf=2.0)

# 5. Find fatigue life
target_stress = 100  # MPa
idx = np.argmin(np.abs(S - target_stress))
life = N[idx]
print(f"Life at {target_stress} MPa: {life:.2e} cycles")

# 6. Create plot
plotter.plot_curves(
    lookup_indices=[64, 175],
    scf=2.0,
    save_path='outputs/comparison.png'
)
```

### 8.2 Complete RAO Workflow

```python
from digitalmodel.modules.marine_analysis.rao_processor import RAODataProcessor
import numpy as np

# 1. Initialize
processor = RAODataProcessor()

# 2. Import data
rao = processor.import_aqwa_lis_file('vessel.lis')

# 3. Validate
report = processor.validators.validate_rao_data(rao)
if not report.is_valid:
    for error in report.errors:
        print(f"ERROR: {error}")

# 4. Interpolate
standard_rao = processor.interpolator.interpolate_to_standard_grid(
    rao,
    target_frequencies=np.arange(0.2, 1.2, 0.1),
    target_headings=np.arange(0, 360, 15)
)

# 5. Export
df = processor.export_to_dataframe(standard_rao)
df.to_csv('outputs/rao_data.csv')
```

---

## 9. Version History

| Version | Date | Changes |
|---------|------|---------|
| 2.0.0 | Oct 3, 2025 | Phase 1 complete - Initial API release |

---

## 10. Support

For issues or questions:
- GitHub Issues: https://github.com/vamseeachanta/digitalmodel/issues
- Email: vamsee.achanta@aceengineer.com

---

**Document Version:** 1.0
**Last Updated:** October 3, 2025
