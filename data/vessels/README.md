# Vessels Database

**Domain:** Offshore vessel specifications and capabilities
**Pipeline:** raw → processed → results

## Contents

### Vessel Types
- **FPSO Systems:** Floating Production, Storage and Offloading vessels
- **Drilling Rigs:** Deepwater and jackup drilling platforms
- **Pipelay Vessels:** Pipeline installation vessels
- **Fleet Information:** Drilling rig fleet data

## Datasets

| File | Description | Year | Vessels |
|------|-------------|------|---------|
| `fpso_database_2018.csv` | FPSO specifications and capabilities | 2018 | 200+ |
| `deepwater_drilling_rigs_2014.csv` | Deepwater rig specifications | 2014 | 100+ |
| `jackup_rigs_2015.csv` | Jackup rig database | 2015 | 150+ |
| `pipelay_vessels_2013.csv` | Pipelay vessel capabilities | 2013 | 50+ |
| `drilling_rigs_fleets.md` | Fleet information and operators | - | - |

## Usage Example

```python
import pandas as pd

# Load FPSO database
fpso_df = pd.read_csv('data/vessels/raw/fpso_database_2018.csv')

# Filter by specification
deepwater_units = fpso_df[fpso_df['water_depth'] > 1500]
```

## Data Sources
- Industry databases (2013-2018)
- Vessel operators and contractors
- Fleet tracking services

## Future Enhancements
- Real-time vessel tracking data
- Updated 2024-2025 specifications
- Operator contact information
- Charter rate databases
