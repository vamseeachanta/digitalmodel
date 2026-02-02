# BEMRosetta Integration Module

A Python interface for hydrodynamic coefficient conversion with focus on AQWA to OrcaFlex workflow.

## Overview

This module provides tools for:
- Parsing AQWA diffraction analysis outputs (.LIS, .DAT, .QTF files)
- Extracting RAOs, added mass, damping, and QTF coefficients
- Validating data quality (symmetry, causality, physical limits)
- Exporting to OrcaFlex-compatible formats (YAML, CSV)
- Converting panel mesh formats (GDF, DAT, STL)

## Installation

The module is included in the digitalmodel package. Install with:

```bash
uv pip install -e .
```

## Quick Start

### Python API

```python
from digitalmodel.bemrosetta import (
    AQWAParser,
    OrcaFlexConverter,
    validate_coefficients,
)

# Parse AQWA results
parser = AQWAParser()
results = parser.parse("analysis.LIS")

# Validate coefficients
report = validate_coefficients(results)
if report.is_valid:
    print("Coefficients are valid")
else:
    print(f"Errors: {report.errors}")

# Convert to OrcaFlex format
converter = OrcaFlexConverter(output_dir="./output")
converter.convert(results)
```

### Command Line Interface

```bash
# Convert AQWA to OrcaFlex
bemrosetta convert analysis.LIS -o ./output

# Include QTF data
bemrosetta convert analysis.LIS --qtf analysis.QTF -o ./output

# Display file information
bemrosetta info analysis.LIS

# Validate coefficients
bemrosetta validate analysis.LIS --strict --causality

# Convert mesh formats
bemrosetta convert-mesh hull.gdf -o hull.stl

# Validate mesh quality
bemrosetta validate-mesh hull.gdf --check-normals

# Show module status
bemrosetta status
```

## Features

### Parsers

| Parser | Extensions | Description |
|--------|------------|-------------|
| `AQWAParser` | .LIS, .DAT | AQWA diffraction analysis output |
| `QTFParser` | .QTF | Second-order wave forces (sum/difference frequency) |

### Converters

| Converter | Output | Description |
|-----------|--------|-------------|
| `OrcaFlexConverter` | YAML, CSV | OrcaFlex vessel type and coefficient files |

### Mesh Handlers

| Handler | Format | Description |
|---------|--------|-------------|
| `GDFHandler` | .gdf | WAMIT panel mesh format |
| `DATHandler` | .dat | AQWA/NEMOH panel mesh format |
| `STLHandler` | .stl | STereoLithography format (ASCII/binary) |

### Validators

| Validator | Checks |
|-----------|--------|
| `CoefficientValidator` | Matrix symmetry, positive definiteness, physical limits |
| `CausalityChecker` | Kramers-Kronig relations between added mass and damping |

## Data Models

### BEMSolverMetadata

Metadata from BEM solver output including solver name, version, water depth, vessel name.

```python
from digitalmodel.bemrosetta import BEMSolverMetadata

# Metadata attributes:
# - solver_type: BEMSolverType (AQWA, WAMIT, NEMOH, etc.)
# - solver_version: str
# - water_depth: float (inf for deep water)
# - project_name: str
# - body_names: List[str]
```

### QTFData

Quadratic Transfer Function data for second-order wave forces with sum/difference frequency types.

```python
from digitalmodel.bemrosetta import QTFData, QTFType

# QTFData attributes:
# - qtf_type: QTFType.SUM_FREQUENCY or QTFType.DIFFERENCE_FREQUENCY
# - body_name: str
# - components: List[QTFComponent]
# - frequencies: np.ndarray
# - headings: np.ndarray
# - metadata: dict
```

### PanelMesh

Panel mesh geometry with vertices, panels, normals, areas, and symmetry information.

```python
from digitalmodel.bemrosetta import PanelMesh, MeshFormat

# PanelMesh attributes:
# - vertices: np.ndarray (N x 3)
# - panels: np.ndarray (M x 4, quad panels)
# - normals: Optional[np.ndarray]
# - areas: Optional[np.ndarray]
# - symmetry_plane: Optional[str] ('xz', 'yz', 'xy')
# - format_origin: MeshFormat
# - name: str
```

### MeshQualityReport

Mesh quality metrics including panel statistics, aspect ratios, and quality score.

```python
from digitalmodel.bemrosetta import MeshQualityReport

# MeshQualityReport attributes:
# - n_vertices: int
# - n_panels: int
# - min_area: float
# - max_area: float
# - mean_area: float
# - min_aspect_ratio: float
# - max_aspect_ratio: float
# - quality_score: float (0-100)
# - warnings: List[str]
# - errors: List[str]
```

## API Reference

### Feature Detection

```python
from digitalmodel.bemrosetta import is_bemrosetta_available, get_module_info

# Check if BEMRosetta executable is available
if is_bemrosetta_available():
    print("BEMRosetta executable found")

# Get module information
info = get_module_info()
print(info)
# Output:
# {
#     "name": "bemrosetta",
#     "version": "1.0.0",
#     "bemrosetta_executable_available": False,
#     "supported_input_formats": ["AQWA (.LIS, .DAT, .QTF)"],
#     "supported_output_formats": ["OrcaFlex (YAML, CSV)"],
#     "supported_mesh_formats": ["GDF", "DAT", "STL"],
# }
```

### Parsing AQWA Files

```python
from digitalmodel.bemrosetta import AQWAParser

parser = AQWAParser()

# Check if file can be parsed
if parser.can_parse("analysis.LIS"):
    results = parser.parse("analysis.LIS")

    # Access metadata
    print(f"Vessel: {parser.metadata.vessel_name}")
    print(f"Water depth: {parser.metadata.water_depth}")

    # Access results
    print(f"Frequencies: {results.frequencies}")
    print(f"Headings: {results.headings}")
    print(f"Added mass matrices: {len(results.added_mass)}")
    print(f"Damping matrices: {len(results.damping)}")
    print(f"RAO data points: {len(results.raos)}")
```

### Parsing QTF Files

```python
from digitalmodel.bemrosetta import QTFParser, QTFType

parser = QTFParser()
qtf_data = parser.parse("analysis.QTF")

print(f"QTF type: {qtf_data.qtf_type.value}")
print(f"Frequencies: {qtf_data.frequencies}")
print(f"Headings: {qtf_data.headings}")
print(f"Components: {len(qtf_data.components)}")

# Access individual components
for comp in qtf_data.components:
    print(f"DOF {comp.dof}, Heading {comp.heading}")
    print(f"  Amplitude shape: {comp.amplitude.shape}")
    print(f"  Phase shape: {comp.phase.shape}")
```

### Mesh Conversion

```python
from digitalmodel.bemrosetta import GDFHandler, STLHandler, convert_mesh

# Read GDF mesh
handler = GDFHandler()
mesh = handler.read("hull.gdf")

# Validate quality
report = handler.validate_mesh(mesh)
print(f"Quality score: {report.quality_score}/100")
print(f"Panels: {report.n_panels}")
print(f"Vertices: {report.n_vertices}")

# Convert to STL
convert_mesh("hull.gdf", "hull.stl")

# Or use handlers directly
stl_handler = STLHandler()
stl_handler.write(mesh, "hull.stl")
```

### Validation

```python
from digitalmodel.bemrosetta import (
    CoefficientValidator,
    CausalityChecker,
    validate_coefficients,
)

# Quick validation
report = validate_coefficients(results, strict=True)
print(f"Valid: {report.is_valid}")
print(f"Errors: {report.errors}")
print(f"Warnings: {report.warnings}")

# Detailed validation
validator = CoefficientValidator(
    check_symmetry=True,
    check_positive_definite=True,
    check_physical_limits=True,
    tolerance=0.01,
)
report = validator.validate(results)

# Causality check (Kramers-Kronig relations)
checker = CausalityChecker(tolerance=0.1)
kk_report = checker.validate(results)
print(f"Causality satisfied: {kk_report.is_valid}")
```

### OrcaFlex Conversion

```python
from digitalmodel.bemrosetta import OrcaFlexConverter, convert_to_orcaflex
from pathlib import Path

# Using converter class
converter = OrcaFlexConverter(output_dir=Path("./output"))
result = converter.convert(aqwa_results)

print(f"Output files: {result.output_files}")
print(f"Vessel type file: {result.vessel_type_file}")

# Using convenience function
output_path = convert_to_orcaflex(
    aqwa_results,
    output_dir="./output",
    include_qtf=True,
)
```

## Integration with Existing Modules

This module integrates with:
- `diffraction` module - Uses `DiffractionResults`, `RAOSet`, `OrcaFlexExporter`
- `hydrodynamics` module - Compatible with `HydrodynamicMatrix`, `VesselProperties`

```python
from digitalmodel.diffraction import DiffractionResults
from digitalmodel.bemrosetta import CoefficientValidator

# Validate diffraction results
validator = CoefficientValidator()
report = validator.validate(diffraction_results)
```

## External Dependencies

### Optional: BEMRosetta Executable

The module provides native Python parsers for AQWA files. For extended features (additional solver support, advanced mesh operations), install BEMRosetta at:

```
D:/software/BEMRosetta/BEMRosetta_cl.exe
```

Or set the `BEMROSETTA_PATH` environment variable:

```bash
export BEMROSETTA_PATH=/path/to/BEMRosetta_cl.exe
```

## Error Handling

```python
from digitalmodel.bemrosetta import (
    BEMRosettaError,
    ParserError,
    ConverterError,
    ValidationError,
    MeshError,
)

try:
    parser.parse("invalid.LIS")
except ParserError as e:
    print(f"Parse error: {e}")
    print(f"File: {e.context.get('file_path')}")
except MeshError as e:
    print(f"Mesh error: {e}")
    print(f"Mesh file: {e.context.get('mesh_file')}")
except BEMRosettaError as e:
    # Base exception for all module errors
    print(f"BEMRosetta error: {e}")
```

## Testing

```bash
# Run all BEMRosetta tests
uv run pytest tests/modules/bemrosetta/ -v

# Run specific test categories
uv run pytest tests/modules/bemrosetta/test_parsers.py -v
uv run pytest tests/modules/bemrosetta/test_converters.py -v
uv run pytest tests/modules/bemrosetta/test_mesh.py -v
uv run pytest tests/modules/bemrosetta/test_validators.py -v
uv run pytest tests/modules/bemrosetta/test_cli.py -v
uv run pytest tests/modules/bemrosetta/test_integration.py -v

# Run with coverage
uv run pytest tests/modules/bemrosetta/ --cov=src/digitalmodel/modules/bemrosetta
```

## File Format Reference

### AQWA .LIS File Structure

```
AQWA <version>

STRUCTURE NAME: <vessel_name>
WATER DEPTH = <depth> | INFINITE

ADDED MASS-VARIATION WITH WAVE PERIOD/FREQUENCY
  PERIOD   FREQ     M11        M22        M33        M44        M55        M66        M13        M15        M24        M26        M35        M46
  <period> <freq>   <values...>

DAMPING-VARIATION WITH WAVE PERIOD/FREQUENCY
  PERIOD   FREQ     C11        C22        C33        C44        C55        C66        C13        C15        C24        C26        C35        C46
  <period> <freq>   <values...>

R.A.O.S-VARIATION WITH WAVE PERIOD/FREQUENCY
  PERIOD   FREQ   DIRECTION    X-AMP   X-PHA    Y-AMP   Y-PHA    Z-AMP   Z-PHA   RX-AMP  RX-PHA   RY-AMP  RY-PHA   RZ-AMP  RZ-PHA
  <period> <freq> <heading>    <values...>
```

### GDF Mesh File Structure

```
# Comment line (optional)
ULEN GRAV           # Unit length and gravity
ISX ISY             # Symmetry flags (0=none, 1=xz symmetry)
NPAN                # Number of panels
X1 Y1 Z1            # Panel 1, vertex 1
X2 Y2 Z2            # Panel 1, vertex 2
X3 Y3 Z3            # Panel 1, vertex 3
X4 Y4 Z4            # Panel 1, vertex 4
...                 # More panels
```

### DAT Mesh File Structure (NEMOH format)

```
NVERT NPAN          # Number of vertices and panels
X1 Y1 Z1            # Vertex 1
X2 Y2 Z2            # Vertex 2
...                 # More vertices
V1 V2 V3 V4         # Panel 1 (1-based vertex indices)
...                 # More panels
0 0 0 0             # Terminator
```

## Version History

- **1.0.0** - Initial release
  - AQWA parser for .LIS files
  - QTF parser for second-order forces
  - OrcaFlex converter (YAML, CSV)
  - Mesh handlers (GDF, DAT, STL)
  - Coefficient and causality validators
  - Click-based CLI

## License

Part of the digitalmodel package.
