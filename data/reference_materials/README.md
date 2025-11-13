# Reference Materials

**Domain:** Industry standards, posters, and reference documents
**Pipeline:** raw → processed → results

## Contents

### Industry Posters
- **12 files** - Drilling rig poster advertisements and data
- Source: 2014 industry poster campaigns
- Contains equipment specifications and vendor information

| File | Description |
|------|-------------|
| `industry_posters_drilling_rigs_raw_2014.csv` | Raw poster data (originally: 062614drillrig_posterads_raw.csv) |
| `062614drillrig_posterads_process1.csv` | Processing stage 1 |
| `062614drillrig_posterads_process2.csv` | Processing stage 2 |
| `062614drillrig_posterads_sheet4.csv` | Additional poster data |

### Standards
*(To be added)*
- API standards
- DNV standards
- ISO standards

### Specifications
*(To be added)*
- Equipment specifications
- Design specifications
- Material specifications

## Usage Example

```python
import pandas as pd

# Load industry poster data
posters_df = pd.read_csv('data/reference_materials/raw/industry_posters/industry_posters_drilling_rigs_raw_2014.csv')
```

## Data Sources
- Industry advertisements
- Standards organizations (API, DNV, ISO, etc.)
- Technical publications

## Future Enhancements
- Current API/DNV/ISO standards (PDF)
- Equipment manufacturer specifications
- Industry best practices documents
