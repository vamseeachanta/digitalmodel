# Vessel Configurations - Updated Naming Convention

## 4 Distinct Configurations

### Configuration 1: FSTs Light Only
- **Short Name**: `FST_L015`
- **Full Name**: FSTs Light (15% loaded) - No LNGC
- **File Pattern**: `fsts_l015_mwl_*`
- **Description**: Both FSTs at 15% loading condition, no LNGC present
- **Operational Mode**: Standby/Ready for offloading

### Configuration 2: FSTs Full Only
- **Short Name**: `FST_L095`
- **Full Name**: FSTs Full (95% loaded) - No LNGC
- **File Pattern**: `fsts_l095_mwl_*`
- **Description**: Both FSTs at 95% loading condition, no LNGC present
- **Operational Mode**: Storage mode

### Configuration 3: FSTs Light + LNGC Full
- **Short Name**: `FST_L015_LNGC_L100`
- **Full Name**: FSTs Light (15%) with LNGC Full (100%)
- **File Pattern**: `fsts_l015_125km3_l100_pb_mwl_*`
- **Description**: FSTs at 15% with 125k m³ LNGC at 100% capacity
- **Operational Mode**: Offloading operation (LNGC → FSTs)

### Configuration 4: FSTs Full + LNGC Light  
- **Short Name**: `FST_L095_LNGC_L000`
- **Full Name**: FSTs Full (95%) with LNGC Empty (0%)
- **File Pattern**: `fsts_l095_125km3_l000_pb_mwl_*`
- **Description**: FSTs at 95% with 125k m³ LNGC at 0% capacity
- **Operational Mode**: Loading operation (FSTs → LNGC)

## Naming Convention Summary

| Config # | Short Name | FST Load | LNGC Load | Operation |
|----------|------------|----------|-----------|-----------|
| 1 | FST_L015 | 15% | N/A | Standby |
| 2 | FST_L095 | 95% | N/A | Storage |
| 3 | FST_L015_LNGC_L100 | 15% | 100% | Offloading |
| 4 | FST_L095_LNGC_L000 | 95% | 0% | Loading |

## File Pattern Mapping

```python
CONFIGURATION_PATTERNS = {
    'FST_L015': 'fsts_l015_mwl_',
    'FST_L095': 'fsts_l095_mwl_',
    'FST_L015_LNGC_L100': 'fsts_l015_125km3_l100_pb_mwl_',
    'FST_L095_LNGC_L000': 'fsts_l095_125km3_l000_pb_mwl_'
}
```

## Directory Structure in Production

```
D:\1522\ctr9\fatigue_wsp_method\07c_fatigue\csv\
├── Configuration 1 (FST_L015)
│   ├── fsts_l015_mwl_wave01_Strut[1-8].csv
│   ├── fsts_l015_mwl_wave[02-18]_Strut[1-8].csv
│   └── fsts_l015_mwl_wind[01-16]_Strut[1-8].csv
│
├── Configuration 2 (FST_L095)
│   ├── fsts_l095_mwl_wave01_Strut[1-8].csv
│   ├── fsts_l095_mwl_wave[02-18]_Strut[1-8].csv
│   └── fsts_l095_mwl_wind[01-16]_Strut[1-8].csv
│
├── Configuration 3 (FST_L015_LNGC_L100)
│   ├── fsts_l015_125km3_l100_pb_mwl_wave01_Strut[1-8].csv
│   ├── fsts_l015_125km3_l100_pb_mwl_wave[02-18]_Strut[1-8].csv
│   └── fsts_l015_125km3_l100_pb_mwl_wind[01-16]_Strut[1-8].csv
│
└── Configuration 4 (FST_L095_LNGC_L000)
    ├── fsts_l095_125km3_l000_pb_mwl_wave01_Strut[1-8].csv
    ├── fsts_l095_125km3_l000_pb_mwl_wave[02-18]_Strut[1-8].csv
    └── fsts_l095_125km3_l000_pb_mwl_wind[01-16]_Strut[1-8].csv
```

## Key Points

1. **L015/L095**: Refers to FST loading (15% or 95%)
2. **125km3**: LNGC capacity of 125,000 m³
3. **L100/L000**: LNGC loading (100% full or 0% empty)
4. **pb**: Passively ballasted (when LNGC is connected)
5. **mwl**: Mean water level

## Usage in Code

```python
# Configuration definitions
CONFIGURATIONS = {
    'FST_L015': {
        'description': 'FSTs Light Only',
        'fst_load': 0.15,
        'lngc_load': None,
        'pattern': 'fsts_l015_mwl_'
    },
    'FST_L095': {
        'description': 'FSTs Full Only',
        'fst_load': 0.95,
        'lngc_load': None,
        'pattern': 'fsts_l095_mwl_'
    },
    'FST_L015_LNGC_L100': {
        'description': 'FSTs Light + LNGC Full',
        'fst_load': 0.15,
        'lngc_load': 1.00,
        'pattern': 'fsts_l015_125km3_l100_pb_mwl_'
    },
    'FST_L095_LNGC_L000': {
        'description': 'FSTs Full + LNGC Empty',
        'fst_load': 0.95,
        'lngc_load': 0.00,
        'pattern': 'fsts_l095_125km3_l000_pb_mwl_'
    }
}
```

## Fatigue Analysis Requirements

Each configuration requires separate fatigue damage calculation because:
- Different loading conditions affect natural frequencies
- LNGC presence changes system dynamics
- Strut tensions vary significantly between configurations
- Environmental response differs with vessel mass/inertia

The fatigue life for the overall system will be determined by the weighted combination of all 4 configurations based on their operational occurrence percentages.