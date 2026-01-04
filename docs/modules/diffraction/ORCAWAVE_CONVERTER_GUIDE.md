# OrcaWave Converter Guide

**ABOUTME**: Complete guide for using the OrcaWave converter to extract diffraction data from OrcaFlex models and convert to unified schema.

**Version**: 3.0.0 (Phase 3.2)
**Status**: Data Extraction Complete
**Date**: 2026-01-04

---

## Overview

The `OrcaWaveConverter` extracts diffraction analysis results from OrcaFlex models and converts them to the unified diffraction output schema. This enables:

- Standardized access to OrcaWave/OrcaFlex diffraction data
- Consistent format with AQWA converter output
- Direct export to OrcaFlex vessel type files
- Cross-tool validation and comparison

---

## Requirements

### Software Requirements

1. **OrcaFlex** - Licensed installation with Python API
2. **OrcFxAPI** - Python package (installed with OrcaFlex)
3. **Python 3.8+** - Python environment
4. **digitalmodel package** - This repository

### Installation Check

```python
# Check if OrcFxAPI is available
try:
    import OrcFxAPI
    print(f"OrcFxAPI Version: {OrcFxAPI.Version()}")
except ImportError:
    print("OrcFxAPI not available - install OrcaFlex with Python API")
```

### Verify Converter Availability

```python
from digitalmodel.modules.diffraction import ORCAWAVE_AVAILABLE

if ORCAWAVE_AVAILABLE:
    from digitalmodel.modules.diffraction import OrcaWaveConverter
    print("OrcaWave converter ready")
else:
    print("OrcFxAPI required for OrcaWave converter")
```

---

## Quick Start

### Basic Usage

```python
from digitalmodel.modules.diffraction import OrcaWaveConverter

# Initialize converter with OrcaFlex model file
converter = OrcaWaveConverter(
    model_file="path/to/model.sim",
    vessel_name="MyVessel"  # Optional - auto-detects if None
)

# Convert to unified schema
results = converter.convert_to_unified_schema(water_depth=1200.0)

# Export to OrcaFlex formats
from digitalmodel.modules.diffraction import OrcaFlexExporter

exporter = OrcaFlexExporter(results, output_dir="output/")
output_files = exporter.export_all()
```

### One-Step Conversion

```python
from digitalmodel.modules.diffraction import convert_orcawave_results

# Convert and export in one step
output_dir = convert_orcawave_results(
    model_file="path/to/model.sim",
    water_depth=1200.0,
    output_folder="output/",
    vessel_name="MyVessel"  # Optional
)
```

---

## Detailed Usage

### 1. Initialize Converter

```python
from pathlib import Path
from digitalmodel.modules.diffraction import OrcaWaveConverter

# Option A: With vessel name specified
converter = OrcaWaveConverter(
    model_file=Path("models/fpso_analysis.sim"),
    vessel_name="FPSO_A"
)

# Option B: Auto-detect vessel (uses first vessel found)
converter = OrcaWaveConverter(
    model_file=Path("models/fpso_analysis.sim"),
    vessel_name=None
)
```

**Supported file types**:
- `.sim` - OrcaFlex simulation files
- `.dat` - OrcaFlex data files
- Diffraction database files (if standalone)

### 2. Convert to Unified Schema

```python
# Convert with all data extraction
results = converter.convert_to_unified_schema(
    water_depth=1200.0,
    load_model=True  # Load and extract from model
)

# Access converted data
print(f"Vessel: {results.vessel_name}")
print(f"Tool: {results.analysis_tool}")  # "OrcaWave"
print(f"RAO frequencies: {len(results.raos.surge.frequencies.values)}")
print(f"Headings: {len(results.raos.surge.headings.values)}")
```

### 3. Validate Results

```python
from digitalmodel.modules.diffraction import validate_results

# Run validation
validation_report = validate_results(results)

print(f"Status: {validation_report['overall_status']}")

if validation_report['overall_status'] == 'PASS':
    print("All validations passed!")
else:
    print("Issues found:")
    for category, issues in validation_report.items():
        if isinstance(issues, dict) and issues:
            print(f"  {category}: {sum(len(v) for v in issues.values())} issues")
```

### 4. Export to OrcaFlex Format

```python
from digitalmodel.modules.diffraction import OrcaFlexExporter
from pathlib import Path

exporter = OrcaFlexExporter(
    results,
    output_dir=Path("exports/FPSO_A")
)

# Export all formats
outputs = exporter.export_all()

# Or export individually
vessel_type = exporter.export_vessel_type()
rao_csv = exporter.export_raos_csv()
excel_wb = exporter.export_excel_workbook()
```

---

## Data Extraction Details

### RAO Extraction

The converter extracts Response Amplitude Operators for all 6 DOFs:

```python
# RAOs are extracted from vessel type LoadRAOs table
# - Magnitude and phase for each frequency and heading
# - All 6 DOFs: Surge, Sway, Heave, Roll, Pitch, Yaw
```

**OrcFxAPI Access**:
```python
model = OrcFxAPI.Model("model.sim")
vessel = model["VesselName"]
rao_table = vessel.VesselType.LoadRAOs
# Extract data from rao_table...
```

### Added Mass Extraction

Frequency-dependent 6×6 added mass matrices:

```python
# Extracted from vessel type AddedMassAndDamping data
# - One 6×6 matrix per frequency
# - Units: kg, kg·m, kg·m²
```

### Damping Extraction

Frequency-dependent 6×6 damping matrices:

```python
# Extracted from vessel type AddedMassAndDamping data
# - One 6×6 matrix per frequency
# - Units: N·s/m, N·m·s/rad
```

---

## Integration with OrcaWave Orchestrator

### Adding Converter to Workflow

```python
from digitalmodel.modules.orcawave.diffraction.orchestrator import OrcaWaveOrchestrator
from digitalmodel.modules.diffraction import OrcaWaveConverter, OrcaFlexExporter

# Run OrcaWave analysis
orchestrator = OrcaWaveOrchestrator(vessel_name="sea_cypress")
orchestrator.run_workflow()

# Convert results after analysis complete
results_dir = orchestrator.results_dir / "sea_cypress"
model_file = results_dir / "sea_cypress.sim"

converter = OrcaWaveConverter(model_file, "sea_cypress")
results = converter.convert_to_unified_schema(water_depth=1200.0)

# Export for OrcaFlex
exporter = OrcaFlexExporter(results, results_dir / "orcaflex_export")
exporter.export_all()
```

---

## Error Handling

### Common Issues

**1. OrcFxAPI Not Available**
```python
try:
    from digitalmodel.modules.diffraction import OrcaWaveConverter
except ImportError as e:
    print("OrcFxAPI is required. Install OrcaFlex with Python API.")
```

**2. Model File Not Found**
```python
from pathlib import Path

model_file = Path("path/to/model.sim")
if not model_file.exists():
    raise FileNotFoundError(f"Model file not found: {model_file}")

converter = OrcaWaveConverter(model_file)
```

**3. Vessel Not Found**
```python
try:
    converter = OrcaWaveConverter("model.sim", vessel_name="NonExistent")
    results = converter.convert_to_unified_schema(1200.0)
except ValueError as e:
    print(f"Error: {e}")
    # Try auto-detection
    converter = OrcaWaveConverter("model.sim", vessel_name=None)
```

**4. Model Load Failure**
```python
try:
    converter = OrcaWaveConverter("model.sim")
    results = converter.convert_to_unified_schema(1200.0)
except RuntimeError as e:
    print(f"Failed to load model: {e}")
```

---

## Current Implementation Status

### ✅ Implemented (Phase 3.1 & 3.2)

**Phase 3.1 - Core Framework**:
- Core converter class structure
- OrcFxAPI integration framework
- Unified schema conversion
- Module exports and availability checking
- Error handling infrastructure
- Documentation

**Phase 3.2 - Data Extraction (COMPLETE)**:
- ✅ Actual RAO table parsing from OrcFxAPI
- ✅ Added mass matrices extraction from vessel type data
- ✅ Damping matrices extraction from vessel type data
- ✅ Frequency and heading extraction from actual data
- ✅ Data validation and error handling
- ✅ Warning system for missing data

**Data Extraction Features**:
- `OrcaWaveDataExtractor` class for low-level OrcFxAPI access
- RAO extraction for all 6 DOFs (Surge, Sway, Heave, Roll, Pitch, Yaw)
- Symmetric 6×6 matrix extraction (21 independent values)
- Frequency/period conversion (ω = 2π/T)
- Complex RAO to magnitude/phase conversion
- Validation checks for empty data
- Warning messages for missing frequency data

### 📋 Future Enhancements

**Advanced Features**:
- [ ] Support for QTF extraction (if available)
- [ ] Mean drift force extraction
- [ ] Metadata extraction (analysis date, settings)
- [ ] Integration with OrcaWave orchestrator Phase 3 results
- [ ] Support for multiple vessel types in same model
- [ ] Batch processing framework

---

## Example Workflows

### Workflow 1: Simple Conversion

```python
from digitalmodel.modules.diffraction import convert_orcawave_results

output_dir = convert_orcawave_results(
    model_file="analysis/fpso.sim",
    water_depth=1200.0,
    output_folder="exports/fpso"
)

print(f"Results exported to: {output_dir}")
```

### Workflow 2: With Validation

```python
from digitalmodel.modules.diffraction import (
    OrcaWaveConverter,
    validate_results,
    OrcaFlexExporter
)

# Convert
converter = OrcaWaveConverter("model.sim", "FPSO_A")
results = converter.convert_to_unified_schema(1200.0)

# Validate
report = validate_results(results, output_file="validation.json")

if report['overall_status'] in ['PASS', 'WARNING']:
    # Export if validation passed
    exporter = OrcaFlexExporter(results, "output/")
    exporter.export_all()
else:
    print("Validation failed - check validation.json")
```

### Workflow 3: Batch Processing

```python
from pathlib import Path
from digitalmodel.modules.diffraction import OrcaWaveConverter, OrcaFlexExporter

models = [
    ("fpso_a.sim", "FPSO_A", 1200.0),
    ("fpso_b.sim", "FPSO_B", 1500.0),
    ("tanker.sim", "Tanker", 800.0),
]

for model_file, vessel_name, depth in models:
    converter = OrcaWaveConverter(model_file, vessel_name)
    results = converter.convert_to_unified_schema(depth)

    output_dir = Path(f"exports/{vessel_name}")
    exporter = OrcaFlexExporter(results, output_dir)
    exporter.export_all()

    print(f"Completed: {vessel_name}")
```

---

## Comparison with AQWA Converter

| Feature | AQWA Converter | OrcaWave Converter |
|---------|----------------|-------------------|
| Input Format | .LIS text files | .sim/.dat OrcaFlex files |
| Data Access | File parsing | OrcFxAPI direct access |
| RAO Extraction | Text parsing | API query |
| Matrix Extraction | Text parsing | API query |
| Dependencies | None (text files) | OrcFxAPI required |
| Performance | Fast (direct parse) | Fast (API access) |
| Accuracy | Parse-dependent | API-guaranteed |

---

## API Reference

### OrcaWaveConverter Class

```python
class OrcaWaveConverter:
    def __init__(self, model_file: Path, vessel_name: Optional[str] = None)

    def convert_to_unified_schema(
        self,
        water_depth: float,
        load_model: bool = True
    ) -> DiffractionResults
```

### Convenience Function

```python
def convert_orcawave_results(
    model_file: str,
    water_depth: float,
    output_folder: str,
    vessel_name: Optional[str] = None
) -> Path
```

---

## Related Documentation

- **Diffraction Module README**: `docs/modules/diffraction/README.md`
- **AQWA Converter Guide**: `docs/modules/aqwa/QUICK_START.md`
- **Phase 2 Completion**: `docs/modules/diffraction/PHASE_2_COMPLETION.md`
- **OrcaWave Orchestrator**: `docs/modules/orcawave/README.md`
- **OrcFxAPI Documentation**: https://www.orcina.com/webhelp/OrcFxAPI/

---

**Last Updated**: 2026-01-04
**Version**: 3.0.0 - Phase 3.2
**Status**: Data Extraction Complete
