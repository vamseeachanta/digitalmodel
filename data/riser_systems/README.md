# Riser Systems Data

**Domain:** Riser and flowline system specifications
**Pipeline:** raw → processed → results

## Contents

### Drilling Risers
Complete drilling riser database with equipment specifications:

| File | Description |
|------|-------------|
| `drilling_riser_model_properties.csv` | Riser model properties and geometries |
| `drillrigs.csv` | Complete rig database |
| `drillrigs_bop_and_bop_control_details_sheet1.csv` | BOP specifications |
| `drillrigs_drilling_equipment_and_engineering_company_details_sheet1.csv` | Equipment suppliers |
| `drillrigs_lifting_equipment_sheet1.csv` | Crane and lifting specs |
| `drillrigs_mooring_and_station_keeping_sheet1.csv` | Mooring systems |
| `drillrigs_mud_pumps_sheet1.csv` | Mud pump specifications |
| `drillrigs_rig_data_sheet1.csv` | General rig data |
| `drillrigs_rig_rating_and_contract_sheet1.csv` | Rig ratings |
| `drillrigs_riser_and_tensioner_data_sheet1.csv` | Tensioner specifications |
| `drillrigs_vessel_particulars_sheet1.csv` | Vessel details |
| `drillship.md` | Drillship information |
| `moonpool.md` | Moonpool specifications |

### Production Risers
*(To be added)*

### Export Risers
*(To be added)*

## Usage Example

```python
import pandas as pd

# Load complete rig database
rigs_df = pd.read_csv('data/riser_systems/raw/drilling_risers/drillrigs.csv')

# Load riser model properties
riser_props = pd.read_csv('data/riser_systems/raw/drilling_risers/drilling_riser_model_properties.csv')

# Load tensioner data
tensioners = pd.read_csv('data/riser_systems/raw/drilling_risers/drillrigs_riser_and_tensioner_data_sheet1.csv')
```

## Data Sources
- Drilling contractor specifications
- Rig equipment databases
- Engineering design data
- Manufacturer catalogs

## Future Enhancements
- Production riser databases
- Export riser specifications
- Riser stress analysis results
- Fatigue life calculations
