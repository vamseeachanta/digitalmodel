# Data Procurement Module
## Automated Marine Engineering Data Acquisition & Integration

---

### Overview

The Digital Model Data Procurement Module provides comprehensive web API integration for automated acquisition of marine engineering data from free, industry-standard sources. With modular DRY architecture and multi-tier caching, this module eliminates manual data collection bottlenecks and ensures consistent, validated data across all analysis workflows.

**Key Value Proposition**: Automate 90% of manual data collection tasks with FREE API integration for metocean, vessel, and mooring component data—eliminating costs while improving data quality and analysis turnaround time.

---

### Core Capabilities

- **Metocean Data Acquisition** - Wave, wind, current, bathymetry (NOAA, ERA5, GEBCO)
- **Vessel Database Integration** - RAOs, hydrostatics, principal dimensions (MarineTraffic, ShipXplorer)
- **Mooring Component Libraries** - Chains, ropes, anchors, connectors (manufacturer catalogs, standards)
- **Multi-Tier Caching System** - L1 (memory), L2 (Redis), L3 (disk) for performance
- **Format Conversion** - OrcaFlex, AQWA, MOSES, CSV, JSON, HDF5 output
- **Automated Validation** - Quality control, unit consistency, range checking
- **Configuration Management** - YAML-based, environment-variable secure

---

### Industry Standards Compliance

#### Data Sources & Standards
- **NOAA NDBC** - Wave buoy data, global coverage, 1000 requests/day FREE
- **ERA5 Copernicus** - Reanalysis 1940-present, unlimited FREE access
- **Open-Meteo** - Weather forecasting, unlimited FREE API
- **GEBCO** - Global bathymetry, public domain
- **MarineTraffic** - Vessel database, 5 requests/minute FREE tier
- **ShipXplorer** - Alternative vessel database, 100 requests/hour FREE

#### Engineering Standards
- **API RP 2SK** - Mooring system component specifications
- **DNV-RP-F109** - On-bottom stability, pipeline design
- **OCIMF Guidelines** - Vessel wind/current coefficients
- **ISO 19901-7** - Offshore structures, station keeping systems

---

### Technical Features

#### FREE API Integration (7 Sources)

```
Data Sources (All FREE, No Credit Card Required):
┌────────────────────────────────────────────────────────────────┐
│ NOAA NDBC       Wave buoys, metocean      1000/day            │
│ ERA5 Copernicus Reanalysis, 1940-present  Unlimited           │
│ Open-Meteo      Weather forecasts         Unlimited           │
│ GEBCO           Global bathymetry          Unlimited           │
│ MarineTraffic   Vessel database, AIS       5/min              │
│ ShipXplorer     Alternative vessel DB      100/hour           │
│ Generic DB      Repository-based           Local              │
└────────────────────────────────────────────────────────────────┘

Total Investment: $0/month for basic functionality
```

#### Modular DRY Architecture

```
Common Components Foundation
├── Universal API Client (REST, GraphQL, WebSocket)
├── Multi-Tier Caching (L1: Memory, L2: Redis, L3: Disk)
├── Standard Testing Framework (No mocks, >90% coverage)
├── Configuration Management (YAML, env variables)
├── Security & Authentication
└── Observability & Monitoring

Asset-Specific Modules
├── Metocean Data
│   ├── Wave parameters (Hs, Tp, direction, spectrum)
│   ├── Wind profiles (speed, direction, altitude)
│   └── Current profiles (surface, depth-varying)
├── Vessel Systems
│   ├── Principal dimensions (LOA, beam, draft, DWT)
│   ├── RAO data (6-DOF, all headings)
│   └── Hydrostatic properties (Cb, GM, KB, KG)
└── Mooring Components
    ├── Chains (R3, R4, R5, R6 grades)
    ├── Wire ropes (6x36 IWRC, 6x41 IWRC)
    ├── Synthetic ropes (Polyester, HMPE, Nylon)
    └── Anchors (Drag, suction, driven piles)
```

#### Multi-Tier Caching System

**Performance Optimization:**
- **L1 Cache (Memory)**: <1ms access, 100 MB default
- **L2 Cache (Redis)**: <10ms access, 1 GB shared
- **L3 Cache (Disk)**: <100ms access, unlimited

**Cache Strategy:**
- Automatic cache invalidation (configurable TTL)
- Smart prefetching for common queries
- Compression for large datasets (HDF5, gzip)
- Version tracking for data provenance

---

## Page 2: Output Formats & Integration

### Metocean Data Outputs

**OrcaFlex Wave Files:**
```yaml
# Automatically generated wave file
Wave:
  Name: "North Sea 100-year Storm"
  SignificantHeight: 16.0  # m
  PeakPeriod: 14.5         # s
  Direction: 270           # deg
  WaveType: JONSWAP
  Gamma: 3.3
  CurrentProfile:
    Surface: 1.5           # m/s at 0m depth
    Seabed: 0.3           # m/s at -200m depth
```

**AQWA RAO Input Files:**
- Frequency range: 0.1-3.0 rad/s (user configurable)
- Wave directions: 0-360° (customizable increments)
- Water depth: Finite or infinite depth
- Wave theory: Airy, Stokes, or irregular

**Available Formats:**
- NetCDF (CF-compliant)
- CSV (tabular data)
- HDF5 (large datasets)
- JSON (structured data)
- OrcaFlex YAML
- ANSYS AQWA DAT

### Vessel System Outputs

**Vessel Database Query Example:**

```
IMO: 9876543
Name: "VLCC Tanker"
Type: Crude Oil Carrier
Classification: VLCC

Principal Dimensions:
  LOA:   320.0 m
  Beam:   58.0 m
  Depth:  30.0 m
  Draft:  21.0 m (loaded)
  DWT:    320,000 tonnes

Hydrostatic Properties:
  Displacement: 350,000 tonnes
  Block Coefficient (Cb): 0.87
  GM (metacentric height): 8.5 m
  KB (center of buoyancy): 11.5 m
  KG (center of gravity): 15.0 m

RAO Data: 6-DOF available for 12 wave directions
Natural Periods: Heave 16.5s, Roll 18.2s, Pitch 12.8s
```

### Mooring Component Database

**Chain Specifications Example:**

| Grade | Nominal Diameter | MBL (tonnes) | Weight (kg/m) | Manufacturer |
|-------|-----------------|--------------|---------------|--------------|
| R3    | 76 mm           | 552          | 135.4         | Vicinay      |
| R4    | 84 mm           | 707          | 165.7         | Ramnas       |
| R5    | 84 mm           | 843          | 165.7         | Vicinay      |
| R6    | 76 mm           | 736          | 135.4         | Ramnas       |

**Database Coverage:**
- 1,000+ chain specifications
- 500+ wire rope options
- 200+ synthetic rope specs
- 100+ anchor types
- 500+ connector specifications

---

### Key Benefits

#### 1. **Cost Elimination**
   - **$0/month** for FREE API tier (vs. $500-2000/month for commercial metocean APIs)
   - **Unlimited access** to ERA5 reanalysis (40+ years of data)
   - **No licensing fees** for Open-Meteo weather data
   - **Public domain** bathymetry from GEBCO
   - **Free tier** vessel database (5-100 requests/min)

#### 2. **Time Savings**
   - **90% reduction** in manual data collection time
   - **Automated validation** - catch errors before analysis
   - **Batch processing** - multiple locations/vessels simultaneously
   - **Smart caching** - instant access to frequently used data
   - **One-command execution** - from query to OrcaFlex file

#### 3. **Data Quality & Consistency**
   - **Automated quality control** - 15+ validation checks
   - **Unit consistency** - automatic conversion and verification
   - **Version tracking** - complete data provenance
   - **Range validation** - physical plausibility checks
   - **Format standardization** - consistent across all projects

#### 4. **Workflow Integration**
   - **OrcaFlex YAML** - Direct model input generation
   - **AQWA DAT** - Hydrodynamic analysis automation
   - **MOSES** - Mooring analysis integration
   - **Python API** - Programmatic access for automation
   - **CLI tools** - Command-line batch processing

---

### Configuration Example

**YAML-Based Setup:**

```yaml
# metocean_config.yml
location:
  latitude: 57.0
  longitude: 2.0
  description: "North Sea, Central"

parameters:
  wave:
    - significant_height
    - peak_period
    - direction
    - spectrum
  wind:
    - speed_10m
    - direction
    - profile
  current:
    - surface_speed
    - direction
    - depth_profile

time_range:
  start: "2000-01-01"
  end: "2023-12-31"
  type: "historical"  # or "recent", "forecast"

quality_control:
  remove_outliers: true
  fill_missing: "interpolate"
  validate_ranges: true

output:
  format: "orcaflex_yaml"
  path: "data/metocean/"
  include_statistics: true

caching:
  enabled: true
  ttl_days: 30
  compression: true
```

---

### Quick Start Example

```python
from digitalmodel.data_procurement import MetoceanDataClient

# Initialize client (uses FREE APIs)
client = MetoceanDataClient(config='metocean_config.yml')

# Fetch wave data for location
wave_data = client.get_wave_data(
    lat=57.0, lon=2.0,
    start_date='2020-01-01',
    end_date='2023-12-31',
    parameters=['Hs', 'Tp', 'direction']
)

# Export to OrcaFlex
client.export_to_orcaflex(
    wave_data,
    output_file='north_sea_waves.yml'
)

# Get vessel RAOs
from digitalmodel.data_procurement import VesselDataClient

vessel_client = VesselDataClient(config='vessel_config.yml')
rao_data = vessel_client.get_vessel_raos(
    imo_number='9876543',
    drafts=[18.0, 21.0, 24.0],
    wave_directions=range(0, 360, 45)
)
```

---

### Integration Ecosystem

```
┌──────────────────────────────────────────────────────────────────┐
│              DATA PROCUREMENT INTEGRATION                        │
├──────────────────────────────────────────────────────────────────┤
│                                                                  │
│  FREE Data Sources        Digital Model        Analysis Outputs │
│                                                                  │
│  NOAA NDBC ────────┐     ┌──────────────┐     ┌─→ OrcaFlex     │
│  ERA5 Copernicus ──┤     │     Data     │     ├─→ AQWA         │
│  Open-Meteo ───────┼────→│ Procurement  │────→├─→ MOSES        │
│  GEBCO ────────────┤     │    Module    │     ├─→ Python API   │
│  MarineTraffic ────┤     └──────────────┘     └─→ CSV/JSON     │
│  Generic DB ───────┘            │                               │
│                                 ├─→ Marine Analysis             │
│                                 ├─→ OrcaFlex Integration        │
│                                 └─→ Mooring Design              │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
```

---

### Module Statistics

| Metric | Value |
|--------|-------|
| **FREE APIs** | 7 data sources (0 commercial APIs) |
| **Data Types** | 3 categories (metocean, vessel, mooring) |
| **Output Formats** | 6+ (OrcaFlex, AQWA, CSV, JSON, HDF5, NetCDF) |
| **Cache Performance** | <1ms (L1), <10ms (L2), <100ms (L3) |
| **Database Coverage** | 1,000+ chains, 500+ ropes, 100+ anchors |
| **Validation Checks** | 15+ automated quality control tests |
| **Test Coverage** | >90% (no mocks, real API integration) |

---

### Real-World Applications

- **FPSO Design** - Metocean data for station-keeping analysis
- **Mooring Design** - Component database for catenary/taut-leg systems
- **CALM Buoy** - Vessel RAOs for offloading operations
- **Pipeline Design** - Current profiles for on-bottom stability
- **Offshore Wind** - Wave/wind data for turbine foundation design
- **Drilling Riser** - Vessel motion and environmental loading
- **Dynamic Positioning** - Real-time metocean for DP operations
- **Installation** - Historical statistics for weather windows

---

### About Digital Model

**Digital Model** is a comprehensive engineering asset lifecycle management platform featuring:

- **20+ years** offshore/subsea engineering experience
- **200+ SURF engineers'** collective insights validated
- **Production-ready** - active use in major offshore projects
- **704+ Python modules** - comprehensive capability coverage
- **1,971+ test cases** - rigorous quality assurance
- **Open architecture** - MIT license, GitHub-hosted

**Dedicated to Mark Cerkovnik** - Chief Engineer, mentor, and inspiration.

---

### Contact & Resources

**Technical Support**
- Email: vamsee.achanta@aceengineer.com
- GitHub: https://github.com/vamseeachanta/digitalmodel

**Documentation**
- Module Guide: `/specs/modules/data-procurement/`
- API Documentation: `/docs/api/data_procurement.md`
- Configuration Examples: `/specs/modules/data-procurement/configs/`
- Implementation Summary: `/specs/modules/data-procurement/IMPLEMENTATION_SUMMARY.md`

**Installation**
```bash
pip install git+https://github.com/vamseeachanta/digitalmodel.git
```

---

*Digital Model Data Procurement Module - Version 1.0.0*
*Professional Engineering Software for Offshore & Marine Applications*
*© 2025 Digital Model Project - MIT License*
