# AQWA .LIS File Parser

**Version**: 1.0.0
**Status**: Production
**Module**: `digitalmodel.diffraction.aqwa_lis_parser`

## Overview

The AQWA .LIS parser extracts hydrodynamic coefficients from ANSYS AQWA diffraction analysis output files (.LIS format).

## Features

- **RAO Extraction**: Response Amplitude Operators for all 6 DOFs (surge, sway, heave, roll, pitch, yaw)
- **Added Mass Matrices**: 6x6 symmetric matrices at each frequency
- **Damping Matrices**: 6x6 symmetric matrices at each frequency
- **Frequency Data**: Wave frequencies and periods
- **Heading Data**: Wave headings (directions)

## Usage

### Direct Parser Usage

```python
from pathlib import Path
from digitalmodel.diffraction.aqwa_lis_parser import parse_aqwa_lis_file

# Parse entire .LIS file
lis_file = Path("analysis/vessel.LIS")
data = parse_aqwa_lis_file(lis_file)

# Access extracted data
frequencies = data['frequencies']  # List of frequencies in rad/s
headings = data['headings']        # List of headings in degrees
added_mass = data['added_mass']    # Dict: freq -> 6x6 matrix
damping = data['damping']          # Dict: freq -> 6x6 matrix
raos = data['raos']                # Dict: (freq, heading) -> DOF data
```

### Via AQWAConverter (Recommended)

```python
from pathlib import Path
from digitalmodel.diffraction import AQWAConverter, OrcaFlexExporter

# Convert AQWA results to unified schema
converter = AQWAConverter(
    analysis_folder=Path("./aqwa_analysis"),
    vessel_name="FPSO_A"
)

results = converter.convert_to_unified_schema(water_depth=1200.0)

# Export to OrcaFlex format
exporter = OrcaFlexExporter(results, Path("./output"))
output_files = exporter.export_all()
```

## .LIS File Format

The AQWA .LIS file contains several sections:

### 1. Added Mass Table
```
ADDED MASS-VARIATION WITH WAVE PERIOD/FREQUENCY
-----------------------------------------------
PERIOD   FREQ    M11       M22       M33       M44       M55       M66       ...
------   -----   --------  --------  --------  --------  --------  --------  ...
 62.83   0.100  1.95E+06  2.60E+07  1.80E+08  2.93E+09  2.19E+11  5.59E+10  ...
```

### 2. Damping Table
```
DAMPING-VARIATION WITH WAVE PERIOD/FREQUENCY
--------------------------------------------
PERIOD   FREQ    C11       C22       C33       C44       C55       C66       ...
------   -----   --------  --------  --------  --------  --------  --------  ...
 62.83   0.100  1.85E+02  4.27E+02  2.44E+06  4.26E+02  3.13E+07  1.88E+03  ...
```

### 3. RAO Table
```
R.A.O.S-VARIATION WITH WAVE PERIOD/FREQUENCY
--------------------------------------------
PERIOD   FREQ   DIRECTION          X                 Y                 Z      ...
------   -----  --------- -------------------------------------------------...
(SECS)  (RAD/S) (DEGREES)    AMP      PHASE    AMP      PHASE    AMP      PHA...

  62.83   0.100   -180.00    1.5204  -90.03    0.0000  -91.02    0.9973   -0.00  ...
```

## Extracted Data Structure

### RAO Data
```python
{
    'frequencies': array([0.1, 0.407, 0.715, ...]),  # rad/s
    'headings': array([-180, -135, -90, ...]),       # degrees
    'raos': {
        (freq, heading): {
            'surge': (amplitude, phase),
            'sway': (amplitude, phase),
            'heave': (amplitude, phase),
            'roll': (amplitude, phase),
            'pitch': (amplitude, phase),
            'yaw': (amplitude, phase)
        },
        ...
    }
}
```

### Added Mass / Damping
```python
{
    'frequencies': array([0.1, 0.407, 0.715, ...]),
    'added_mass': {
        0.1: array([[M11, M12, ..., M16],
                    [M21, M22, ..., M26],
                    ...]),
        ...
    },
    'damping': {
        0.1: array([[C11, C12, ..., C16],
                    [C21, C22, ..., C26],
                    ...]),
        ...
    }
}
```

## Implementation Details

### Matrix Construction

The .LIS file provides 12 coefficients per frequency:
- **Diagonal**: M11, M22, M33, M44, M55, M66
- **Off-diagonal**: M13, M15, M24, M26, M35, M46

The parser constructs full 6x6 symmetric matrices by:
1. Setting diagonal elements
2. Setting specified off-diagonal coupling terms
3. Using symmetry to fill remaining elements

### Scientific Notation Parsing

All numerical values are in scientific notation (e.g., `1.95E+06`). The parser:
- Uses regex patterns that require 'E' to avoid matching divider lines
- Handles both positive and negative exponents
- Converts to Python floats

### Heading Extraction

RAO data includes multiple wave headings per frequency. The parser:
- Extracts unique heading values from RAO tables
- Sorts headings in ascending order
- Typically finds 9 headings: -180°, -135°, -90°, -45°, 0°, 45°, 90°, 135°, 180°

## Validation

The parser performs basic validation:
- Checks for required section headers
- Verifies numeric data format
- Logs warnings for missing data
- Ensures matrix symmetry

Comprehensive validation is done by `OutputValidator` after conversion to unified schema.

## Error Handling

Common errors and solutions:

| Error | Cause | Solution |
|-------|-------|----------|
| `FileNotFoundError` | .LIS file not found | Check file path |
| `ValueError: No valid frequency data found` | Wrong .LIS file format | Ensure file contains diffraction results |
| `ValueError: could not convert string` | Parsing error | Check .LIS file encoding (UTF-8) |
| Missing matrices for some frequencies | Incomplete analysis | Check AQWA run completed successfully |

## Performance

- **Parse time**: ~0.1-0.5 seconds for typical .LIS files (1-10 MB)
- **Memory usage**: Minimal (data stored in numpy arrays)
- **File size**: Handles files up to 100 MB efficiently

## Limitations

1. **Format dependency**: Designed for AQWA 2022 R2+ format. May need adjustments for older versions.
2. **Single structure**: Extracts data for first structure only in multi-structure analyses.
3. **Encoding**: Requires ASCII or UTF-8 encoding. May fail with other encodings.
4. **QTF data**: Currently not extracted (future enhancement).

## Future Enhancements

- [ ] Multi-structure support
- [ ] QTF (quadratic transfer function) extraction
- [ ] Mean drift forces
- [ ] Sum and difference frequency effects
- [ ] Automatic AQWA version detection
- [ ] Support for .HYD files as alternative source

## See Also

- [CLI Guide](CLI_GUIDE.md) - Command-line usage
- [API Reference](../../API.md) - Full API documentation
- [OrcaWave Parser](ORCAWAVE_PARSER.md) - Companion parser for OrcaWave results
