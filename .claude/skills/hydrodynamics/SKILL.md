---
name: hydrodynamics
description: Manage hydrodynamic coefficients, wave spectra, and environmental loading for vessel response analysis. Use for 6×6 matrix management, wave spectrum modeling, OCIMF loading calculations, and RAO interpolation.
---

# Hydrodynamics Skill

Manage hydrodynamic coefficients, wave spectra, and environmental loading for vessel and floater response analysis.

## When to Use

- 6×6 added mass and damping matrix management
- Wave spectrum modeling (JONSWAP, Bretschneider, PM)
- OCIMF wind and current loading calculations
- RAO interpolation and frequency-dependent coefficients
- Hydrodynamic coefficient database management
- Kramers-Kronig causality validation

## Prerequisites

- Python environment with `digitalmodel` package installed
- Hydrodynamic coefficient data (from AQWA, WAMIT, etc.)
- Environmental data for wave/wind/current loading

## Analysis Types

### 1. Coefficient Database Management

Store and retrieve hydrodynamic coefficients.

```yaml
hydrodynamics:
  coefficient_database:
    flag: true
    vessel_name: "FPSO"
    source_file: "data/hydro_coefficients.json"
    coefficients:
      - added_mass
      - damping
      - wave_excitation
    output:
      database_file: "results/coefficient_db.json"
```

### 2. Wave Spectra Modeling

Generate and analyze wave spectra.

```yaml
hydrodynamics:
  wave_spectra:
    flag: true
    spectrum_type: "jonswap"  # jonswap, bretschneider, pm, custom
    parameters:
      hs: 3.5  # Significant wave height (m)
      tp: 10.0  # Peak period (s)
      gamma: 3.3  # JONSWAP peakedness
    frequency_range:
      min: 0.02
      max: 0.5
      n_points: 100
    output:
      spectrum_file: "results/wave_spectrum.csv"
      plot_file: "results/spectrum_plot.html"
```

### 3. OCIMF Environmental Loading

Calculate wind and current loads per OCIMF guidelines.

```yaml
hydrodynamics:
  ocimf_loading:
    flag: true
    vessel:
      length: 300.0
      beam: 50.0
      draft: 20.0
      displacement: 200000
    environment:
      wind_speed: 25.0
      wind_direction: 45.0
      current_speed: 1.5
      current_direction: 90.0
    output:
      loads_file: "results/ocimf_loads.json"
```

### 4. RAO Interpolation

Interpolate RAOs across frequencies and directions.

```yaml
hydrodynamics:
  rao_interpolation:
    flag: true
    input_raos: "data/vessel_raos.csv"
    target_frequencies: [0.05, 0.1, 0.15, 0.2, 0.25]
    target_directions: [0, 30, 60, 90, 120, 150, 180]
    method: "cubic"  # linear, cubic, spline
    output:
      interpolated_file: "results/interpolated_raos.csv"
```

## Python API

### Coefficient Database

```python
from digitalmodel.modules.hydrodynamics.coefficient_database import CoefficientDatabase

# Initialize database
db = CoefficientDatabase()

# Store coefficients
db.store(
    vessel_name="FPSO",
    frequency=0.1,
    added_mass=added_mass_matrix,  # 6x6 numpy array
    damping=damping_matrix         # 6x6 numpy array
)

# Retrieve coefficients
A, B = db.get_matrices(vessel_name="FPSO", frequency=0.1)

# Get all frequencies
frequencies = db.get_frequencies("FPSO")
```

### Frequency-Dependent Matrices

```python
from digitalmodel.modules.hydrodynamics.freq_dependent import FrequencyDependentMatrix

# Initialize with frequency-dependent data
fdm = FrequencyDependentMatrix()
fdm.load("hydro_data.json")

# Interpolate to specific frequency
A_interp = fdm.interpolate_added_mass(frequency=0.15)
B_interp = fdm.interpolate_damping(frequency=0.15)

# Get infinite frequency added mass
A_inf = fdm.get_infinite_frequency_added_mass()
```

### Wave Spectra

```python
from digitalmodel.modules.hydrodynamics.wave_spectra import WaveSpectra

# Create JONSWAP spectrum
spectrum = WaveSpectra()
frequencies, S = spectrum.jonswap(
    hs=3.5,       # Significant wave height (m)
    tp=10.0,      # Peak period (s)
    gamma=3.3,    # Peakedness parameter
    freq_min=0.02,
    freq_max=0.5,
    n_points=100
)

# Alternative spectra
freq, S_pm = spectrum.pierson_moskowitz(hs=3.5, tp=10.0)
freq, S_bs = spectrum.bretschneider(hs=3.5, tp=10.0)

# Calculate spectral moments
m0 = spectrum.spectral_moment(frequencies, S, n=0)
m2 = spectrum.spectral_moment(frequencies, S, n=2)
Tz = np.sqrt(m0/m2)  # Zero-crossing period
```

### OCIMF Loading

```python
from digitalmodel.modules.hydrodynamics.ocimf_loading import OCIMFLoading

# Initialize calculator
ocimf = OCIMFLoading()

# Define vessel
vessel = {
    "length": 300.0,
    "beam": 50.0,
    "draft": 20.0,
    "displacement": 200000
}

# Calculate wind load
wind_load = ocimf.wind_load(
    vessel=vessel,
    wind_speed=25.0,
    wind_direction=45.0  # degrees from bow
)
# Returns: {"Fx": ..., "Fy": ..., "Mz": ...}

# Calculate current load
current_load = ocimf.current_load(
    vessel=vessel,
    current_speed=1.5,
    current_direction=90.0
)
```

### Coefficient Interpolation

```python
from digitalmodel.modules.hydrodynamics.interpolator import CoefficientsInterpolator

# Initialize interpolator
interp = CoefficientsInterpolator()

# Load RAO data
interp.load_raos("vessel_raos.csv")

# Interpolate to new frequencies
new_freqs = [0.05, 0.1, 0.15, 0.2]
interpolated = interp.interpolate_frequencies(new_freqs, method="cubic")

# Interpolate to new directions
new_dirs = [0, 45, 90, 135, 180]
interpolated = interp.interpolate_directions(new_dirs)
```

### Causality Validation

```python
from digitalmodel.modules.hydrodynamics.validation import HydroValidator

# Initialize validator
validator = HydroValidator()

# Load frequency-dependent coefficients
validator.load_coefficients("hydro_data.json")

# Kramers-Kronig check
kk_result = validator.kramers_kronig_check()
if not kk_result["passed"]:
    print(f"Causality issues at: {kk_result['violations']}")

# Check matrix properties
sym_check = validator.check_symmetry()
pd_check = validator.check_positive_definite()
```

## Key Classes

| Class | Purpose |
|-------|---------|
| `CoefficientDatabase` | Coefficient storage and retrieval |
| `FrequencyDependentMatrix` | 6×6 matrix interpolation |
| `WaveSpectra` | Spectrum generation (JONSWAP, PM, etc.) |
| `OCIMFLoading` | OCIMF wind/current calculations |
| `CoefficientsInterpolator` | 2D interpolation (freq × direction) |
| `HydroValidator` | Kramers-Kronig and matrix validation |

## Wave Spectrum Types

| Spectrum | Application |
|----------|-------------|
| JONSWAP | Fetch-limited seas (North Sea) |
| Pierson-Moskowitz | Fully developed seas |
| Bretschneider | General two-parameter spectrum |
| ISSC | Modified Pierson-Moskowitz |
| Ochi-Hubble | Bimodal sea states |

## Output Formats

### Coefficient Database JSON

```json
{
  "vessel_name": "FPSO",
  "frequencies_rad_s": [0.1, 0.2, 0.3],
  "added_mass": {
    "0.1": [[1.2e6, 0, 0, 0, 1.5e7, 0], ...],
    "0.2": [[1.1e6, 0, 0, 0, 1.4e7, 0], ...]
  },
  "damping": {
    "0.1": [[2.5e5, 0, 0, 0, 3.2e6, 0], ...],
    "0.2": [[2.8e5, 0, 0, 0, 3.5e6, 0], ...]
  }
}
```

### Wave Spectrum CSV

```csv
frequency_rad_s,frequency_hz,period_s,spectral_density
0.314,0.050,20.0,0.123
0.628,0.100,10.0,2.456
0.942,0.150,6.67,1.234
```

## Best Practices

1. **Frequency range** - Cover full wave spectrum of interest (typically 0.02-0.5 rad/s)
2. **Direction convention** - Use consistent direction convention (from/to, bow=0°)
3. **Unit consistency** - Verify units (rad/s vs Hz, degrees vs radians)
4. **Causality check** - Validate coefficients with Kramers-Kronig before use
5. **Matrix symmetry** - Verify added mass symmetry for physical consistency

## Related Skills

- [aqwa-analysis](../aqwa-analysis/SKILL.md) - Extract coefficients from AQWA
- [orcaflex-modeling](../orcaflex-modeling/SKILL.md) - Apply in OrcaFlex models
- [viv-analysis](../viv-analysis/SKILL.md) - Hydrodynamic coefficient usage

## References

- DNV-RP-C205: Environmental Conditions and Environmental Loads
- OCIMF: Mooring Equipment Guidelines
- Newman, J.N.: Marine Hydrodynamics
