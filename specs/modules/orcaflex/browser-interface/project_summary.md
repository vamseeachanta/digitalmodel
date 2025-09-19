# Folder Size Summary - OrcaFlex Example-3

## Directory: `D:\github\digitalmodel\specs\modules\orcaflex\example-3`

### Overview
- **243 YML configuration files** in the root directory
- **7 active subdirectories** with specialized content  
- **Total: ~899 files** (excluding fsts_lngc_pretension folder)

### Subdirectory Breakdown

| Directory | File Count | Description |
|-----------|------------|-------------|
| **env/** | 334 files | Environment-related configuration and data files |
| **winches/** | 123 files | Winch configuration files for different line configurations |
| **fsts_lngc_pretension_iterated/** | 96 files | Iterated pretension analysis results and include files |
| **moorings/** | 70 files | Mooring line configuration files for various vessel configurations |
| **current_coeffs/** | 16 files | Current coefficient configuration files |
| **wind_coeffs/** | 16 files | Wind coefficient configuration files |
| **templates/** | 1 file | Template configuration file |

### Root Directory Configuration Files (Key Categories)

#### Analysis Types
- Static analysis configurations (`01_a_static_analysis_*.yml`)
- Dynamic analysis configurations (`01_b_dynamic_analysis_*.yml`)
- Fatigue analysis configurations (`*_fatigue.yml`)

#### Environment Settings
- Water depth and seabed configurations (`02a_*.yml`)
- Current, wave, and wind specifications (`02c_*.yml`, `02d_*.yml`, `02e_*.yml`)
- Ramp settings for simulations

#### Vessel Configurations
- FSTS (Floating Storage and Transfer System) models
- LNGC (LNG Carrier) configurations:
  - 125km³ vessels (Port/Starboard)
  - 180km³ vessels (Port/Starboard)

#### Water Level Scenarios
- **HWL** - High Water Level
- **MWL** - Mean Water Level  
- **LWL** - Low Water Level

#### Structural Components
- Jacket configurations (`09_*_jackets_*.yml`)
- Fender systems (`09_b_fenders_*.yml`, `12_*_fenders_*.yml`)
- Strut and contact line specifications
- Mooring system configurations

#### Special Analysis Cases
- Damage scenarios (`*_damage_strut_*.yml`)
- Lock conditions for different jackets
- Pretension and initial position analyses
- WSP (Wave Spectrum) variations with different damping/stiffness percentages

### Key Project Characteristics

1. **Configuration-Driven Architecture**
   - Extensive use of YML files for parameterization
   - Modular approach to vessel and environment specifications

2. **Marine Engineering Focus**
   - Offshore floating structures analysis
   - Mooring system design and optimization
   - Multi-vessel berthing scenarios

3. **Comprehensive Testing Matrix**
   - Multiple water levels (HWL, MWL, LWL)
   - Various vessel sizes and configurations
   - Different environmental conditions

4. **Analysis Types**
   - Static structural analysis
   - Dynamic response analysis
   - Fatigue life assessment
   - Mooring pretension optimization

### File Naming Conventions

- Numerical prefixes indicate processing order (01_, 02_, etc.)
- Suffixes indicate vessel/berth configuration:
  - `_pb` - Port/Portside berth
  - `_sb` - Starboard berth
  - `_l015`, `_l095` - Loading conditions
  - `_125km3`, `_180km3` - Vessel capacity

### Notes
- The `fsts_lngc_pretension/` folder has been excluded from this summary
- No large binary .sim files detected in the current directory structure
- Project appears to be a comprehensive OrcaFlex marine engineering analysis focused on FSTS-LNGC interaction and mooring systems