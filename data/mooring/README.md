# Mooring & Station-Keeping Data

**Domain:** Mooring analysis, OCIMF coefficients, and CALM buoy systems
**Pipeline:** raw → processed → results

## Contents

### OCIMF Wind & Current Coefficients
- Production database with complete coefficient sets
- Sample database for testing
- Coverage: All major vessel types (tankers, LNG, FPSOs)

### CALM Buoy Systems
- Generic design range data
- Mature design specifications
- Project-specific analysis results

### Mooring Components
- Chain properties and specifications
- Wire rope properties
- Anchor specifications

## Key Datasets

| File | Description | Records |
|------|-------------|---------|
| `ocimf/ocimf_database.csv` | Complete OCIMF coefficients | 1000+ |
| `ocimf/ocimf_coefficients_production.csv` | Production coefficient sets | 500+ |
| `ocimf/ocimf_coefficients_sample.csv` | Sample data for testing | 50+ |
| `components/chain_properties.csv` | Mooring chain specifications | 100+ |
| `components/wire_properties.csv` | Wire rope properties | 50+ |

## OCIMF Coefficient Format

```
vessel_type, displacement_ratio, heading, CXw, CYw, CMw, CXc, CYc, CMc, displacement
```

Where:
- CXw, CYw, CMw: Wind force coefficients (surge, sway, yaw)
- CXc, CYc, CMc: Current force coefficients (surge, sway, yaw)

## Usage Example

```python
import pandas as pd

# Load OCIMF database
ocimf_df = pd.read_csv('data/mooring/raw/ocimf/ocimf_database.csv')

# Get coefficients for ballasted tanker at 45° heading
coeffs = ocimf_df[
    (ocimf_df['vessel_type'] == 'Ballasted Tanker') &
    (ocimf_df['heading'] == 45.0)
]
```

## Data Sources
- OCIMF publications
- CALM buoy manufacturers
- Mooring component suppliers

## Future Enhancements
- Updated OCIMF coefficients (2024+)
- Dynamic positioning data
- Mooring line fatigue analysis
