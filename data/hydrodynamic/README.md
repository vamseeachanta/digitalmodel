# Hydrodynamic Analysis Data

**Domain:** Hydrodynamic coefficients for motion analysis
**Pipeline:** raw → processed → results

## Contents

### Added Mass Coefficients
- 84 frequency-dependent files (ω = 0.1 to 3.0 rad/s)
- 6 degrees of freedom (surge, sway, heave, roll, pitch, yaw)
- Format: `added_mass_omega_{frequency}.csv`

### Damping Coefficients
- 84 frequency-dependent files
- Radiation damping matrices
- Format: `damping_omega_{frequency}.csv`

### RAOs (Response Amplitude Operators)
- Vessel heave RAO example
- Additional RAOs to be added

### Frequency Data
- `frequencies.csv`: Frequency discretization for analysis

## File Naming Convention

```
added_mass_omega_0.5000.csv  → ω = 0.5 rad/s
damping_omega_1.2500.csv     → ω = 1.25 rad/s
```

## Frequency Range

- **Minimum:** 0.1 rad/s (T ≈ 63 sec)
- **Maximum:** 3.0 rad/s (T ≈ 2.1 sec)
- **Step:** ~0.035 rad/s
- **Total Points:** 84

## Usage Example

```python
import pandas as pd
import glob

# Load all added mass data
added_mass_files = sorted(glob.glob('data/hydrodynamic/raw/added_mass/*.csv'))
added_mass_data = {
    float(f.split('_')[-1].replace('.csv','')): pd.read_csv(f)
    for f in added_mass_files
}

# Access data at specific frequency
omega = 0.5  # rad/s
A_matrix = added_mass_data[omega]
```

## Data Sources
- Hydrodynamic analysis software (WAMIT, AQWA, etc.)
- Model basin test data
- CFD simulations

## Typical Analysis Workflow

1. Load frequency-dependent coefficients
2. Interpolate to target frequencies
3. Combine with environmental data
4. Calculate vessel motions (RAOs)
5. Generate motion statistics

## Future Enhancements
- Complete RAO database for multiple vessels
- Wave drift forces
- Second-order hydrodynamic coefficients
