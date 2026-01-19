# Unified RAO Reader - AI Agent Integration Guide

## Overview

The Unified RAO Reader provides a single, consistent interface for reading Response Amplitude Operator (RAO) data from multiple marine engineering software formats. This guide is specifically designed for AI agents to understand and use the module effectively.

**Version**: 2.0.0
**License Requirements**: None (pure text parsing)
**Supported Formats**: ANSYS AQWA .lis, OrcaFlex YAML

---

## Quick Start

### Basic Usage (3 Lines of Code)

```python
from digitalmodel.modules.marine_analysis import read_rao_file

rao_data = read_rao_file('vessel_analysis.lis')
print(rao_data.get_available_types())  # [DISPLACEMENT, VELOCITY, ACCELERATION]
```

### Complete Example

```python
from digitalmodel.modules.marine_analysis import UnifiedRAOReader, RAOType

# Create reader
reader = UnifiedRAOReader()

# Read file (auto-detects format)
rao_data = reader.read('path/to/vessel.lis')

# Check what's available
if rao_data.has_displacement():
    disp = rao_data.displacement

    # Access data
    frequencies = disp.frequencies  # numpy array (rad/s)
    headings = disp.headings        # numpy array (degrees)

    # Get 6-DOF data
    surge_amplitude = disp.surge.amplitude  # 2D array [freq, heading]
    surge_phase = disp.surge.phase          # 2D array [freq, heading]

    # Access other DOFs
    sway = disp.sway
    heave = disp.heave
    roll = disp.roll
    pitch = disp.pitch
    yaw = disp.yaw

# Check for velocity RAOs
if rao_data.has_velocity():
    vel = rao_data.velocity
    surge_vel_amp = vel.surge.amplitude

# Check for acceleration RAOs
if rao_data.has_acceleration():
    acc = rao_data.acceleration
    surge_acc_amp = acc.surge.amplitude
```

---

## Supported File Formats

### 1. ANSYS AQWA .lis Files

**Format**: Text-based output from AQWA analysis
**Extension**: `.lis`
**License Required**: No (uses text parsing)
**RAO Types Supported**: Displacement, Velocity, Acceleration

**Example**:
```python
reader = UnifiedRAOReader()
rao_data = reader.read('aqwa_analysis.lis')

# All three RAO types available (if present in file)
print(rao_data.get_available_types())
# [RAOType.DISPLACEMENT, RAOType.VELOCITY, RAOType.ACCELERATION]
```

### 2. OrcaFlex YAML Files

**Format**: YAML export from OrcaFlex
**Extension**: `.yml`, `.yaml`
**License Required**: No (direct YAML reading)
**RAO Types Supported**: Displacement

**Example**:
```python
reader = UnifiedRAOReader()
rao_data = reader.read('orcaflex_vessel.yml')

# Only displacement available
print(rao_data.get_available_types())
# [RAOType.DISPLACEMENT]
```

---

## Data Structure

### UnifiedRAOData Container

```python
class UnifiedRAOData:
    displacement: Optional[DisplacementRAO]
    velocity: Optional[VelocityRAO]
    acceleration: Optional[AccelerationRAO]
    metadata: RAOMetadata

    # Methods
    def has_displacement() -> bool
    def has_velocity() -> bool
    def has_acceleration() -> bool
    def get_available_types() -> List[RAOType]
    def get_rao_data(rao_type: RAOType) -> Optional[RAOData]
```

### Individual RAO Data (Displacement/Velocity/Acceleration)

```python
class DisplacementRAO:
    frequencies: np.ndarray  # Shape: (n_freq,), Units: rad/s
    headings: np.ndarray     # Shape: (n_head,), Units: degrees

    # 6-DOF data (each is a DOFData object)
    surge: DOFData   # Units: m/m
    sway: DOFData    # Units: m/m
    heave: DOFData   # Units: m/m
    roll: DOFData    # Units: deg/m
    pitch: DOFData   # Units: deg/m
    yaw: DOFData     # Units: deg/m

    metadata: RAOMetadata
    units: Dict[str, str]
```

### DOF Data Container

```python
class DOFData:
    amplitude: np.ndarray  # Shape: (n_freq, n_head)
    phase: np.ndarray      # Shape: (n_freq, n_head), Units: degrees
```

---

## Coordinate System

### Axes Convention

- **X-axis (Surge)**: Forward/aft, positive forward
- **Y-axis (Sway)**: Port/starboard, positive starboard
- **Z-axis (Heave)**: Up/down, positive up
- **RX (Roll)**: Rotation about X-axis
- **RY (Pitch)**: Rotation about Y-axis
- **RZ (Yaw)**: Rotation about Z-axis

### Heading Convention

- **0°**: Head seas (waves from bow)
- **90°**: Beam seas from starboard
- **180°**: Following seas (waves from stern)
- **270°**: Beam seas from port

### Phase Convention

Leading positive convention (ISO 6954):
- Positive phase = motion leads wave elevation
- Phase in degrees

---

## Common AI Agent Tasks

### Task 1: Extract RAO Data for Analysis

```python
from digitalmodel.modules.marine_analysis import read_rao_file
import numpy as np

# Read data
rao_data = read_rao_file('vessel.lis')

# Get displacement RAOs
disp = rao_data.displacement

# Find peak surge response
surge_amp = disp.surge.amplitude
freq_idx, head_idx = np.unravel_index(
    surge_amp.argmax(),
    surge_amp.shape
)

peak_frequency = disp.frequencies[freq_idx]
peak_heading = disp.headings[head_idx]
peak_amplitude = surge_amp[freq_idx, head_idx]

print(f"Peak surge RAO: {peak_amplitude:.3f} m/m")
print(f"  at frequency: {peak_frequency:.3f} rad/s")
print(f"  at heading: {peak_heading:.1f}°")
```

### Task 2: Compare Multiple Vessels

```python
from digitalmodel.modules.marine_analysis import UnifiedRAOReader

reader = UnifiedRAOReader()

vessels = {
    'Vessel A': 'vessel_a.lis',
    'Vessel B': 'vessel_b.lis',
    'Vessel C': 'vessel_c.yml'
}

results = {}
for name, filepath in vessels.items():
    rao_data = reader.read(filepath)
    disp = rao_data.displacement

    # Get heave RAO at specific condition
    freq_idx = np.argmin(np.abs(disp.frequencies - 0.5))  # ~0.5 rad/s
    head_idx = np.argmin(np.abs(disp.headings - 0.0))     # Head seas

    heave_rao = disp.heave.amplitude[freq_idx, head_idx]
    results[name] = heave_rao

# Find best vessel (minimum heave)
best_vessel = min(results, key=results.get)
print(f"Best vessel: {best_vessel}")
```

### Task 3: Export to CSV for External Tools

```python
from digitalmodel.modules.marine_analysis import read_rao_file
import pandas as pd

rao_data = read_rao_file('vessel.lis')
disp = rao_data.displacement

# Create DataFrame for export
data = []
for i, freq in enumerate(disp.frequencies):
    for j, heading in enumerate(disp.headings):
        row = {
            'Frequency (rad/s)': freq,
            'Period (s)': 2 * np.pi / freq,
            'Heading (deg)': heading,
            'Surge Amplitude (m/m)': disp.surge.amplitude[i, j],
            'Surge Phase (deg)': disp.surge.phase[i, j],
            'Sway Amplitude (m/m)': disp.sway.amplitude[i, j],
            'Sway Phase (deg)': disp.sway.phase[i, j],
            'Heave Amplitude (m/m)': disp.heave.amplitude[i, j],
            'Heave Phase (deg)': disp.heave.phase[i, j],
            'Roll Amplitude (deg/m)': disp.roll.amplitude[i, j],
            'Roll Phase (deg)': disp.roll.phase[i, j],
            'Pitch Amplitude (deg/m)': disp.pitch.amplitude[i, j],
            'Pitch Phase (deg)': disp.pitch.phase[i, j],
            'Yaw Amplitude (deg/m)': disp.yaw.amplitude[i, j],
            'Yaw Phase (deg)': disp.yaw.phase[i, j],
        }
        data.append(row)

df = pd.DataFrame(data)
df.to_csv('rao_export.csv', index=False)
```

### Task 4: Validate Data Quality

```python
from digitalmodel.modules.marine_analysis import read_rao_file
import numpy as np

rao_data = read_rao_file('vessel.lis')
disp = rao_data.displacement

# Check 1: Frequencies are monotonically increasing
freq_check = np.all(np.diff(disp.frequencies) > 0)
print(f"Frequencies monotonic: {freq_check}")

# Check 2: No NaN or Inf values
for dof_name in ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']:
    dof = getattr(disp, dof_name)
    has_nan = np.any(np.isnan(dof.amplitude)) or np.any(np.isnan(dof.phase))
    has_inf = np.any(np.isinf(dof.amplitude)) or np.any(np.isinf(dof.phase))

    if has_nan or has_inf:
        print(f"WARNING: {dof_name} contains invalid values")

# Check 3: Reasonable amplitude ranges
surge_max = np.max(disp.surge.amplitude)
if surge_max > 5.0:
    print(f"WARNING: Surge amplitude {surge_max} seems high")

heave_max = np.max(disp.heave.amplitude)
if heave_max > 2.0:
    print(f"WARNING: Heave amplitude {heave_max} seems high")
```

### Task 5: Backward Compatibility with Legacy Code

```python
from digitalmodel.modules.marine_analysis import read_rao_file

# Read using new unified reader
rao_data = read_rao_file('vessel.lis')

# Convert to legacy format (for old code)
legacy_dict = rao_data.displacement.to_dict()

# Legacy dict structure:
# {
#   'frequencies': array([...]),
#   'headings': array([...]),
#   'raos': {
#     'surge': {'amplitude': array([...]), 'phase': array([...])},
#     'sway': {'amplitude': array([...]), 'phase': array([...])},
#     ...
#   }
# }

# Use with legacy processor
from digitalmodel.modules.marine_analysis import RAODataProcessor

processor = RAODataProcessor()
# ... legacy code continues
```

---

## Error Handling

### Standard Pattern

```python
from digitalmodel.modules.marine_analysis import (
    UnifiedRAOReader,
    RAOReaderError
)

reader = UnifiedRAOReader()

try:
    rao_data = reader.read('vessel.lis')
except RAOReaderError as e:
    print(f"Error: {e.message}")
    print("\nSuggestions:")
    for i, suggestion in enumerate(e.suggestions, 1):
        print(f"  {i}. {suggestion}")
    # Handle error appropriately
```

### Common Errors and Solutions

| Error | Cause | Solution |
|-------|-------|----------|
| "File not found" | Invalid path | Check file path, use absolute path |
| "Failed to parse" | Corrupted/invalid file | Verify file is valid RAO output |
| "No RAO data found" | File doesn't contain RAOs | Check analysis settings in source software |
| "Unsupported format" | Unknown file type | Use .lis or .yml files |

---

## Performance Considerations

### Large Files (>100MB)

```python
# For large files, extract only what you need
reader = UnifiedRAOReader()

# Only extract displacement (faster)
rao_data = reader.read(
    'large_vessel.lis',
    extract_displacement=True,
    extract_velocity=False,
    extract_acceleration=False
)
```

### Batch Processing

```python
from pathlib import Path
from digitalmodel.modules.marine_analysis import UnifiedRAOReader

reader = UnifiedRAOReader()

# Process all .lis files in directory
lis_files = Path('rao_results/').glob('*.lis')

for lis_file in lis_files:
    try:
        rao_data = reader.read(str(lis_file))
        # Process data...
    except RAOReaderError as e:
        print(f"Skipping {lis_file.name}: {e.message}")
        continue
```

---

## API Reference Summary

### Main Classes

```python
# Main entry point
UnifiedRAOReader()
    .read(file_path, extract_displacement, extract_velocity, extract_acceleration)
    .read_aqwa_lis(file_path, ...)
    .read_orcaflex_yml(file_path)
    .get_info(file_path)

# Convenience function
read_rao_file(file_path, extract_displacement, extract_velocity, extract_acceleration)

# Data containers
UnifiedRAOData
    .displacement: DisplacementRAO
    .velocity: VelocityRAO
    .acceleration: AccelerationRAO
    .has_displacement()
    .has_velocity()
    .has_acceleration()
    .get_available_types()
    .to_dict()

DisplacementRAO / VelocityRAO / AccelerationRAO
    .frequencies: np.ndarray
    .headings: np.ndarray
    .surge / sway / heave / roll / pitch / yaw: DOFData
    .metadata: RAOMetadata
    .to_dict()

DOFData
    .amplitude: np.ndarray
    .phase: np.ndarray
```

### Enums

```python
RAOType.DISPLACEMENT
RAOType.VELOCITY
RAOType.ACCELERATION

SourceFormat.AQWA_LIS
SourceFormat.ORCAFLEX_YML
```

---

## Migration from Legacy Code

### Old Way (v1.x)

```python
from digitalmodel.modules.marine_analysis import RAODataProcessor

processor = RAODataProcessor()
rao_data = processor.import_aqwa_lis_file('vessel.lis')
# Only displacement RAOs, complex API
```

### New Way (v2.0)

```python
from digitalmodel.modules.marine_analysis import read_rao_file

rao_data = read_rao_file('vessel.lis')
# All RAO types, simple API, backward compatible
```

---

## Troubleshooting

### Issue: Import Error

```python
# ❌ Wrong
from digitalmodel.marine_analysis import UnifiedRAOReader

# ✅ Correct
from digitalmodel.modules.marine_analysis import UnifiedRAOReader
```

### Issue: No velocity/acceleration RAOs found

```python
# Check what's available
rao_data = read_rao_file('vessel.lis')
print(rao_data.get_available_types())

# AQWA .lis may not contain all types depending on analysis
# OrcaFlex only provides displacement RAOs
```

### Issue: Array shape mismatch

```python
# Always check array shapes before operations
disp = rao_data.displacement
print(f"Frequencies: {disp.frequencies.shape}")
print(f"Headings: {disp.headings.shape}")
print(f"Surge amplitude: {disp.surge.amplitude.shape}")
# Should be: (n_freq, n_head)
```

---

## Version History

**v2.0.0** (Current)
- ✅ Unified interface for all RAO sources
- ✅ Displacement, velocity, acceleration RAO support
- ✅ Auto-format detection
- ✅ Comprehensive error handling
- ✅ AI agent friendly API
- ✅ Backward compatibility with v1.x

**v1.x** (Legacy)
- Displacement RAOs only
- Multiple separate readers
- Complex API

---

## Support

For issues or questions:
1. Check this documentation
2. Review code examples
3. Examine error messages and suggestions
4. Verify file format is supported
5. Check that analysis completed successfully in source software

---

**AI Agent Recommendation**: Use `read_rao_file()` for simple cases, `UnifiedRAOReader()` for advanced control.
