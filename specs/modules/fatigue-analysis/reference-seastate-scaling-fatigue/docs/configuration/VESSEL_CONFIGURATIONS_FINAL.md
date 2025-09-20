# Vessel Configurations - Final Naming Convention

## 4 Distinct Configurations (Matching Input File Names)

### Configuration 1: fsts_l015
- **Config Name**: `fsts_l015`
- **Description**: FSTs Light (15% loaded) - No LNGC
- **File Pattern**: `fsts_l015_mwl_{case}_Strut{N}.csv`
- **Operational Mode**: Standby/Ready for operations

### Configuration 2: fsts_l095
- **Config Name**: `fsts_l095`
- **Description**: FSTs Full (95% loaded) - No LNGC
- **File Pattern**: `fsts_l095_mwl_{case}_Strut{N}.csv`
- **Operational Mode**: Storage mode

### Configuration 3: fsts_l015_125km3_l100_pb
- **Config Name**: `fsts_l015_125km3_l100_pb`
- **Description**: FSTs Light (15%) with LNGC Full (100%) - Port Berthing
- **File Pattern**: `fsts_l015_125km3_l100_pb_mwl_{case}_Strut{N}.csv`
- **Operational Mode**: Offloading operation (LNGC → FSTs)

### Configuration 4: fsts_l095_125km3_l000_pb
- **Config Name**: `fsts_l095_125km3_l000_pb`
- **Description**: FSTs Full (95%) with LNGC Empty (0%) - Port Berthing
- **File Pattern**: `fsts_l095_125km3_l000_pb_mwl_{case}_Strut{N}.csv`
- **Operational Mode**: Loading operation (FSTs → LNGC)

## Configuration Summary Table

| Config Name | FST Load | LNGC Status | LNGC Load | Port Berthing |
|-------------|----------|-------------|-----------|---------------|
| `fsts_l015` | 15% | Not Present | N/A | No |
| `fsts_l095` | 95% | Not Present | N/A | No |
| `fsts_l015_125km3_l100_pb` | 15% | Present | 100% | Yes |
| `fsts_l095_125km3_l000_pb` | 95% | Present | 0% | Yes |

## Example File Names

### Configuration 1: fsts_l015
```
Input files:
fsts_l015_mwl_wave01_Strut1.csv
fsts_l015_mwl_wave01_Strut2.csv
...
fsts_l015_mwl_wind16_Strut8.csv

Output files (following same pattern):
fsts_l015_mwl_wave01_Strut1_rainflow.csv
fsts_l015_mwl_wave01_Strut1_damage.csv
fsts_l015_fatigue_summary.csv
```

### Configuration 2: fsts_l095
```
Input files:
fsts_l095_mwl_wave01_Strut1.csv
fsts_l095_mwl_wave01_Strut2.csv
...
fsts_l095_mwl_wind16_Strut8.csv

Output files:
fsts_l095_mwl_wave01_Strut1_rainflow.csv
fsts_l095_mwl_wave01_Strut1_damage.csv
fsts_l095_fatigue_summary.csv
```

### Configuration 3: fsts_l015_125km3_l100_pb
```
Input files:
fsts_l015_125km3_l100_pb_mwl_wave01_Strut1.csv
fsts_l015_125km3_l100_pb_mwl_wave01_Strut2.csv
...
fsts_l015_125km3_l100_pb_mwl_wind16_Strut8.csv

Output files:
fsts_l015_125km3_l100_pb_mwl_wave01_Strut1_rainflow.csv
fsts_l015_125km3_l100_pb_mwl_wave01_Strut1_damage.csv
fsts_l015_125km3_l100_pb_fatigue_summary.csv
```

### Configuration 4: fsts_l095_125km3_l000_pb
```
Input files:
fsts_l095_125km3_l000_pb_mwl_wave01_Strut1.csv
fsts_l095_125km3_l000_pb_mwl_wave01_Strut2.csv
...
fsts_l095_125km3_l000_pb_mwl_wind16_Strut8.csv

Output files:
fsts_l095_125km3_l000_pb_mwl_wave01_Strut1_rainflow.csv
fsts_l095_125km3_l000_pb_mwl_wave01_Strut1_damage.csv
fsts_l095_125km3_l000_pb_fatigue_summary.csv
```

## Python Implementation Example

```python
# Configuration definitions using exact file naming
CONFIGURATIONS = {
    'fsts_l015': {
        'description': 'FSTs Light (15%) - No LNGC',
        'fst_load_percent': 15,
        'lngc_present': False,
        'lngc_load_percent': None,
        'port_berthing': False,
        'file_prefix': 'fsts_l015_mwl'
    },
    'fsts_l095': {
        'description': 'FSTs Full (95%) - No LNGC',
        'fst_load_percent': 95,
        'lngc_present': False,
        'lngc_load_percent': None,
        'port_berthing': False,
        'file_prefix': 'fsts_l095_mwl'
    },
    'fsts_l015_125km3_l100_pb': {
        'description': 'FSTs Light (15%) + LNGC Full (100%) - Port Berthing',
        'fst_load_percent': 15,
        'lngc_present': True,
        'lngc_load_percent': 100,
        'port_berthing': True,
        'file_prefix': 'fsts_l015_125km3_l100_pb_mwl'
    },
    'fsts_l095_125km3_l000_pb': {
        'description': 'FSTs Full (95%) + LNGC Empty (0%) - Port Berthing',
        'fst_load_percent': 95,
        'lngc_present': True,
        'lngc_load_percent': 0,
        'port_berthing': True,
        'file_prefix': 'fsts_l095_125km3_l000_pb_mwl'
    }
}

def get_input_file_path(config_name, case_type, case_num, strut_num):
    """Generate input file path matching production naming"""
    prefix = CONFIGURATIONS[config_name]['file_prefix']
    filename = f"{prefix}_{case_type}{case_num:02d}_Strut{strut_num}.csv"
    return f"D:\\1522\\ctr9\\fatigue_wsp_method\\07c_fatigue\\csv\\{filename}"

def get_output_file_path(config_name, case_type, case_num, strut_num, output_type):
    """Generate output file path matching input naming convention"""
    prefix = CONFIGURATIONS[config_name]['file_prefix']
    filename = f"{prefix}_{case_type}{case_num:02d}_Strut{strut_num}_{output_type}.csv"
    return f"output\\{config_name}\\{filename}"

# Example usage
input_file = get_input_file_path('fsts_l015_125km3_l100_pb', 'wave', 1, 1)
# Returns: D:\1522\ctr9\fatigue_wsp_method\07c_fatigue\csv\fsts_l015_125km3_l100_pb_mwl_wave01_Strut1.csv

output_file = get_output_file_path('fsts_l015_125km3_l100_pb', 'wave', 1, 1, 'rainflow')
# Returns: output\fsts_l015_125km3_l100_pb\fsts_l015_125km3_l100_pb_mwl_wave01_Strut1_rainflow.csv
```

## Metadata File Structure

```csv
configuration,case_type,case_num,strut_num,file_path,description
fsts_l015,wave,1,1,fsts_l015_mwl_wave01_Strut1.csv,FST Light - Wave case 1 - Strut 1
fsts_l015,wave,1,2,fsts_l015_mwl_wave01_Strut2.csv,FST Light - Wave case 1 - Strut 2
...
fsts_l095_125km3_l000_pb,wind,16,8,fsts_l095_125km3_l000_pb_mwl_wind16_Strut8.csv,FST Full LNGC Empty PB - Wind case 16 - Strut 8
```

## Key Naming Components

- **l015/l095**: FST loading percentage (15% or 95%)
- **125km3**: LNGC capacity of 125,000 m³
- **l100/l000**: LNGC loading percentage (100% or 0%)
- **pb**: Port Berthing (LNGC connected at port)
- **mwl**: Mean Water Level
- **wave##/wind##**: Reference seastate case number
- **Strut#**: Strut number (1-8)

## Directory Structure for Outputs

```
output/
├── fsts_l015/
│   ├── fsts_l015_mwl_wave01_Strut1_effective_tension.csv
│   ├── fsts_l015_mwl_wave01_Strut1_rainflow.csv
│   ├── fsts_l015_mwl_wave01_Strut1_damage.csv
│   └── fsts_l015_fatigue_summary.csv
├── fsts_l095/
├── fsts_l015_125km3_l100_pb/
└── fsts_l095_125km3_l000_pb/
```

This naming convention maintains complete consistency with the input files, making it easy to trace outputs back to their source data.