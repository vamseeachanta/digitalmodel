# ABOUTME: Comprehensive documentation for universal data procurement framework
# ABOUTME: Installation, usage, and examples for metocean, vessel, and mooring modules

# Data Procurement Framework

Universal data procurement framework for marine engineering assets via web APIs and repository databases.

**Version:** 4.0.0
**Status:** Production Ready
**License:** MIT

## Overview

The data procurement framework provides unified access to marine engineering data through:

- **Metocean Data** - Waves, wind, current, tides, bathymetry via free APIs
- **Vessel Systems** - Ship data, RAOs, hydrostatics from public sources
- **Mooring Systems** - Chains, ropes, anchors, connectors from repository databases

### Key Principles

1. **Zero Storage Footprint** - All data streamed/generated in-memory, no intermediate files
2. **Free APIs Only** - No credit cards required, all data sources publicly accessible
3. **Direct Consumption** - OrcaFlex/AQWA YAML output for immediate use
4. **Configuration-Driven** - YAML configs for all modules
5. **Repository-Based Components** - Mooring component data embedded in code

## Architecture

```
data_procurement/
├── common/              # Shared base classes
│   ├── base_client.py
│   ├── stream_handler.py
│   ├── cache_manager.py
│   └── config_loader.py
├── metocean/           # Weather and ocean data
│   ├── client.py
│   └── api_clients/    # ERA5, NOAA, Open-Meteo, GEBCO
├── vessel/             # Ship data and RAOs
│   ├── client.py
│   └── api_clients/    # Generic RAOs, MarineTraffic, vessel DB
└── mooring/            # Mooring components
    ├── client.py
    └── database_clients/  # Chains, ropes, anchors, connectors
```

### Design Patterns

- **Streaming Architecture** - `Iterator[Dict[str, Any]]` for constant memory
- **Configuration-Driven** - YAML validation and loading
- **DRY Framework** - Shared common components across all modules
- **Standards Compliance** - API RP 2SK, DNV-OS-E301, ISO 20438

## Installation

### Prerequisites

```bash
# Python 3.8+
python --version

# Install dependencies
pip install numpy scipy pyyaml requests cdsapi
```

### API Keys Setup

#### 1. ERA5 Climate Data (Optional but recommended)

```bash
# Register at https://cds.climate.copernicus.eu/user/register
# Create ~/.cdsapirc:
cat > ~/.cdsapirc << EOF
url: https://cds.climate.copernicus.eu/api/v2
key: {uid}:{api-key}
EOF
```

#### 2. NOAA NCEI (Optional)

```bash
# Register at https://www.ncdc.noaa.gov/cdo-web/token
# Set environment variable:
export NOAA_TOKEN="your_token_here"
```

#### 3. MarineTraffic (Optional)

```bash
# Free tier available at https://www.marinetraffic.com/en/ais-api-services
export MARINETRAFFIC_API_KEY="your_key_here"
```

**Note:** All modules work without API keys by using fallback data sources!

## Quick Start

### Complete Workflow Example

Design a moored FPSO with metocean data, vessel RAOs, and mooring system:

```python
from digitalmodel.data_procurement import MetoceanClient, VesselClient, MooringClient
from datetime import datetime
import numpy as np

# 1. Get metocean design conditions
metocean_client = MetoceanClient.from_config('metocean_config.yml')

wave_heights = []
for record in metocean_client.query_metocean(
    start_date=datetime(2020, 1, 1),
    end_date=datetime(2023, 1, 1),
    location={'latitude': 25.0, 'longitude': -90.0},
    parameters=['significant_wave_height']
):
    wave_heights.append(record.get('significant_wave_height', 0))

design_hs = np.percentile(wave_heights, 99.9)  # 100-year return period
print(f"Design Hs: {design_hs:.2f} m")

# 2. Get vessel data with RAOs
vessel_client = VesselClient.from_config('vessel_config.yml')

vessel = {
    'name': 'Generic VLCC',
    'length': 330,
    'beam': 60,
    'depth': 30,
    'draft': 20.0
}

raos = vessel_client.get_vessel_raos(vessel_type='VLCC', draft=20.0)
hydro = vessel_client.calculate_hydrostatics(vessel, draft=20.0)

print(f"Displacement: {hydro['displacement']:.0f} tonnes")
print(f"GM: {hydro['gm_metacentric_height']:.2f} m")

# 3. Design mooring system
mooring_client = MooringClient()

mooring_line = mooring_client.design_mooring_line(
    design_load=5000,      # kN
    water_depth=1500,      # m
    mooring_type='catenary',
    soil_type='clay'
)

print(f"\nMooring Design:")
for i, segment in enumerate(mooring_line['segments']):
    print(f"  Segment {i+1}: {segment['name']}, {segment['length']:.1f}m")
print(f"  Anchor: {mooring_line['anchor']['type']}, {mooring_line['anchor']['dry_weight']}kg")

# 4. Generate complete OrcaFlex model
vessel_yaml = vessel_client.to_orcaflex_vessel(vessel, raos, hydro, draft=20.0)
mooring_yaml = mooring_client.to_orcaflex_mooring(mooring_line, name="Line1")

complete_model = vessel_yaml + "\n\n" + mooring_yaml

# Ready for OrcaFlex - no file I/O!
print(f"\nGenerated {len(complete_model.split('\\n'))} lines of OrcaFlex YAML")
```

## Module Guides

### 1. Metocean Data Module

Query weather and ocean data from free APIs.

**Configuration:**

```yaml
# metocean_config.yml
data_sources:
  - name: era5
    api_type: era5
    priority: 1
  - name: noaa
    api_type: noaa_ncei
    priority: 2

cache:
  enabled: true
  type: memory
  ttl: 3600
```

**Usage:**

```python
from digitalmodel.data_procurement import MetoceanClient
from datetime import datetime

client = MetoceanClient.from_config('metocean_config.yml')

# Stream significant wave height
for record in client.query_metocean(
    start_date=datetime(2023, 1, 1),
    end_date=datetime(2023, 1, 7),
    location={'latitude': 51.5, 'longitude': -3.0},
    parameters=['significant_wave_height', 'peak_wave_period']
):
    print(f"{record['timestamp']}: Hs={record['significant_wave_height']:.2f}m, "
          f"Tp={record['peak_wave_period']:.1f}s")

# Get bathymetry
depth = client.query_bathymetry(latitude=51.5, longitude=-3.0)
print(f"Water depth: {depth:.1f} m")
```

**Available Parameters:**
- `significant_wave_height` (m)
- `peak_wave_period` (s)
- `wave_direction` (degrees)
- `wind_speed` (m/s)
- `wind_direction` (degrees)
- `current_speed` (m/s)
- `current_direction` (degrees)

**Data Sources:**
1. **ERA5** - ECMWF reanalysis (1979-present, 0.25° resolution)
2. **NOAA NCEI** - US coastal data (hourly, buoy data)
3. **Open-Meteo** - Global marine forecast (7-day forecast)
4. **GEBCO** - Global bathymetry (15 arc-second resolution)

### 2. Vessel Systems Module

Access ship data, RAOs, and hydrostatics.

**Configuration:**

```yaml
# vessel_config.yml
data_sources:
  - name: generic_rao
    api_type: generic_rao
    priority: 1
  - name: vessel_db
    api_type: vessel_database
    priority: 2

cache:
  enabled: true
  type: memory
  ttl: 86400
```

**Usage:**

```python
from digitalmodel.data_procurement import VesselClient

client = VesselClient.from_config('vessel_config.yml')

# Get vessel RAOs (Response Amplitude Operators)
raos = client.get_vessel_raos(
    vessel_type='VLCC',  # or 'Suezmax', 'Aframax', 'LNG_Carrier', etc.
    draft=20.0,
    wave_periods=[6, 8, 10, 12, 14, 16, 18],  # seconds
    wave_headings=[0, 45, 90, 135, 180]       # degrees
)

# RAOs returned for 6 DOF: surge, sway, heave, roll, pitch, yaw
print(f"Heave RAO at 10s, head seas: {raos['raos'][2]['amplitude'][2]:.3f} m/m")

# Calculate hydrostatics
vessel = {
    'name': 'VLCC',
    'length': 330,
    'beam': 60,
    'depth': 30,
    'draft': 20.0
}

hydro = client.calculate_hydrostatics(vessel, draft=20.0)

print(f"Displacement: {hydro['displacement']:.0f} tonnes")
print(f"LCB: {hydro['lcb']:.2f} m from midships")
print(f"VCB: {hydro['vcb']:.2f} m from keel")
print(f"GM: {hydro['gm_metacentric_height']:.2f} m")

# Generate OrcaFlex vessel YAML
vessel_yaml = client.to_orcaflex_vessel(vessel, raos, hydro, draft=20.0)
print(vessel_yaml[:500])  # Preview
```

**Supported Vessel Types:**
- VLCC (330m × 60m)
- Suezmax (285m × 50m)
- Aframax (245m × 44m)
- LNG Carrier (290m × 46m)
- Container Ship (400m × 59m)
- Semi-Submersible (100m × 75m)

### 3. Mooring Systems Module

Design mooring systems with automatic component selection.

**Usage:**

```python
from digitalmodel.data_procurement import MooringClient

client = MooringClient()

# 1. Query component databases

# Chains - 204 specifications (6 grades × 34 diameters)
chains = client.chain_client.find_by_design_load(
    design_load=5000,        # kN
    grade='R4S',             # R3, R3S, R4, R4S, R5, R6
    safety_factor=1.67       # API RP 2SK intact condition
)

print(f"Found {len(chains)} suitable chains")
chain = chains[0]  # Smallest diameter that meets requirement
print(f"Selected: {chain['diameter']}mm, MBL={chain['minimum_breaking_load']}kN")

# Synthetic Ropes - 104 specifications (4 fiber types × 26 diameters)
ropes = client.synthetic_rope_client.find_by_design_load(
    design_load=8000,
    fiber_type='polyester'  # polyester, nylon, hmpe, aramid
)

rope = ropes[0]
print(f"Rope: {rope['diameter']}mm {rope['fiber_type']}, MBL={rope['minimum_breaking_load']}kN")

# Calculate nonlinear stiffness for OrcaFlex
stiffness = client.synthetic_rope_client.calculate_nonlinear_stiffness(rope)
print(f"Stiffness at 50% MBL: {stiffness['extension_percent'][5]:.1f}% elongation")

# Anchors - 75 specifications (5 types × 15 weights)
anchors = client.anchor_client.find_by_holding_capacity(
    holding_capacity=10000,  # kN
    soil='clay',             # clay, sand, rock
    safety_factor=1.5
)

anchor = anchors[0]
print(f"Anchor: {anchor['type']}, {anchor['dry_weight']}kg")

capacity = client.anchor_client.calculate_holding_capacity(
    anchor, soil_type='clay', su=50  # Undrained shear strength 50 kPa
)
print(f"Holding capacity: {capacity['holding_capacity_kn']:.0f} kN")

# 2. Design complete mooring line

# Catenary mooring (chain or chain-wire-chain)
catenary_line = client.design_mooring_line(
    design_load=5000,
    water_depth=1500,
    mooring_type='catenary',
    soil_type='clay'
)

print("\nCatenary Mooring:")
for segment in catenary_line['segments']:
    print(f"  {segment['name']}: {segment['length']:.1f}m")

# Taut-leg mooring (synthetic rope)
taut_line = client.design_mooring_line(
    design_load=8000,
    water_depth=2000,
    mooring_type='taut_leg',
    soil_type='sand'
)

print("\nTaut-Leg Mooring:")
for segment in taut_line['segments']:
    print(f"  {segment['name']}: {segment['length']:.1f}m")
    if segment['type'] == 'synthetic_rope':
        print(f"    Fiber: {segment['component']['fiber_type']}")

# Semi-taut mooring (chain-polyester-chain)
semi_taut_line = client.design_mooring_line(
    design_load=6000,
    water_depth=800,
    mooring_type='semi_taut',
    soil_type='clay'
)

# 3. Validate against standards
validation = catenary_line['validation']
print(f"\nValidation: {validation['valid']}")
print(f"Standard: {validation['standard']}")
if validation['warnings']:
    for warning in validation['warnings']:
        print(f"  Warning: {warning}")

# 4. Generate OrcaFlex YAML
mooring_yaml = client.to_orcaflex_mooring(catenary_line, name="Line1")
print(f"\nGenerated OrcaFlex YAML: {len(mooring_yaml.split('\\n'))} lines")
```

**Component Databases:**

| Component Type | Count | Grades/Types | Diameter/Weight Range |
|---------------|-------|--------------|---------------------|
| Chains | 204 | R3, R3S, R4, R4S, R5, R6 | 16-178 mm |
| Wire Ropes | 108 | 6x36 IWRC, 6x41 IWRC, 8x19 FC, 6x19 IWRC | 20-180 mm |
| Synthetic Ropes | 104 | Polyester, Nylon, HMPE, Aramid | 80-400 mm |
| Anchors | 75 | Stevpris Mk6, Vryhof Stevshark, Bruce Mk3, Suction pile, Driven pile | 5,000-100,000 kg |
| Connectors | 90 | Bow shackle, Anchor shackle, Screw-pin, H-link, C-link, Swivel | 50-250 mm |

**Mooring Types:**

1. **Catenary** - Chain or chain-wire-chain
   - Total length: ~1.5-2x water depth
   - Upper chain: 10% water depth
   - Wire rope (if deepwater): Water depth - upper - 200m
   - Lower chain: 200m
   - Anchor: Holding capacity ≈ 2 × design load

2. **Taut-Leg** - Synthetic rope (polyester/HMPE)
   - Total length: ~1.05-1.1x water depth
   - Upper chain: 3% water depth
   - Synthetic rope: Remainder
   - Lower chain: 50m
   - Anchor: Holding capacity ≈ 1.2 × design load

3. **Semi-Taut** - Chain-polyester-chain
   - Total length: ~1.2-1.3x water depth
   - Upper chain: 5% water depth
   - Polyester: 70% water depth
   - Lower chain: 100m
   - Anchor: Holding capacity ≈ 1.5 × design load

## Examples

### Example 1: Metocean Statistics

```python
from digitalmodel.data_procurement import MetoceanClient
from datetime import datetime
import numpy as np

client = MetoceanClient.from_config('metocean_config.yml')

# Collect 1 year of wave data
wave_heights = []
for record in client.query_metocean(
    start_date=datetime(2022, 1, 1),
    end_date=datetime(2023, 1, 1),
    location={'latitude': 57.0, 'longitude': 2.0},  # North Sea
    parameters=['significant_wave_height']
):
    wave_heights.append(record.get('significant_wave_height', 0))

# Calculate statistics
hs_mean = np.mean(wave_heights)
hs_max = np.max(wave_heights)
hs_p50 = np.percentile(wave_heights, 50)
hs_p95 = np.percentile(wave_heights, 95)
hs_p99 = np.percentile(wave_heights, 99)

print(f"Wave Statistics (1 year):")
print(f"  Mean Hs: {hs_mean:.2f} m")
print(f"  Max Hs: {hs_max:.2f} m")
print(f"  P50 Hs: {hs_p50:.2f} m")
print(f"  P95 Hs: {hs_p95:.2f} m")
print(f"  P99 Hs: {hs_p99:.2f} m (approx 10-year)")
```

### Example 2: Vessel Motion RAO Analysis

```python
from digitalmodel.data_procurement import VesselClient
import numpy as np

client = VesselClient.from_config('vessel_config.yml')

# Get RAOs for VLCC
raos = client.get_vessel_raos(
    vessel_type='VLCC',
    draft=20.0,
    wave_periods=[6, 8, 10, 12, 14, 16, 18, 20],
    wave_headings=[0, 45, 90, 135, 180]
)

# Extract heave RAO (index 2)
heave_rao = raos['raos'][2]
periods = heave_rao['wave_periods']
headings = heave_rao['wave_headings']

# Find maximum heave RAO
max_heave = 0
max_period = 0
max_heading = 0

for i, period in enumerate(periods):
    for j, heading in enumerate(headings):
        amp = heave_rao['amplitude'][i][j]
        if amp > max_heave:
            max_heave = amp
            max_period = period
            max_heading = heading

print(f"Maximum Heave RAO:")
print(f"  Amplitude: {max_heave:.3f} m/m")
print(f"  Period: {max_period:.1f} s")
print(f"  Heading: {max_heading:.0f} deg")
```

### Example 3: Mooring Component Comparison

```python
from digitalmodel.data_procurement import MooringClient

client = MooringClient()

# Compare chain grades for same design load
design_load = 5000  # kN
comparison = client.chain_client.compare_grades(design_load)

print("Chain Grade Comparison (5000 kN design load):")
for grade, chain in comparison.items():
    print(f"  {grade}: {chain['diameter']}mm, "
          f"MBL={chain['minimum_breaking_load']}kN, "
          f"Mass={chain['mass_per_meter']:.1f}kg/m")

# Compare synthetic rope fiber types
rope_comparison = client.synthetic_rope_client.compare_fiber_types(8000)

print("\nSynthetic Rope Fiber Comparison (8000 kN design load):")
for fiber, rope in rope_comparison.items():
    print(f"  {fiber.capitalize()}: {rope['diameter']}mm, "
          f"MBL={rope['minimum_breaking_load']}kN, "
          f"Elongation@50%={rope['elongation_at_50_mbl']}%, "
          f"Floats={rope['floats']}")
```

### Example 4: Complete FPSO Mooring Design

See `examples/complete_orcaflex_workflow.py` for complete workflow integrating all three modules.

## API Reference

### MetoceanClient

```python
class MetoceanClient:
    @classmethod
    def from_config(cls, config_path: str) -> 'MetoceanClient'

    def query_metocean(
        self,
        start_date: datetime,
        end_date: datetime,
        location: Dict[str, float],
        parameters: List[str]
    ) -> Iterator[Dict[str, Any]]

    def query_bathymetry(
        self,
        latitude: float,
        longitude: float
    ) -> float

    def to_orcaflex_sea_state(
        self,
        records: List[Dict[str, Any]]
    ) -> str
```

### VesselClient

```python
class VesselClient:
    @classmethod
    def from_config(cls, config_path: str) -> 'VesselClient'

    def get_vessel_raos(
        self,
        vessel_type: str,
        draft: float,
        wave_periods: List[float] = None,
        wave_headings: List[float] = None
    ) -> Dict[str, Any]

    def calculate_hydrostatics(
        self,
        vessel: Dict[str, Any],
        draft: float
    ) -> Dict[str, float]

    def to_orcaflex_vessel(
        self,
        vessel: Dict[str, Any],
        raos: Dict[str, Any],
        hydrostatics: Dict[str, float],
        draft: float
    ) -> str
```

### MooringClient

```python
class MooringClient:
    def design_mooring_line(
        self,
        design_load: float,
        water_depth: float,
        mooring_type: str = 'catenary',
        soil_type: str = 'clay',
        safety_factor: float = None
    ) -> Dict[str, Any]

    def validate_design(
        self,
        mooring_line: Dict[str, Any],
        safety_factor: float = None
    ) -> Dict[str, Any]

    def to_orcaflex_mooring(
        self,
        mooring_line: Dict[str, Any],
        name: str = "Line1"
    ) -> str

    # Component database clients
    chain_client: ChainDatabaseClient
    wire_rope_client: WireRopeDatabaseClient
    synthetic_rope_client: SyntheticRopeDatabaseClient
    anchor_client: AnchorDatabaseClient
    connector_client: ConnectorDatabaseClient
```

## Testing

Run integration tests:

```bash
# All tests
pytest tests/integration/

# Specific test
pytest tests/integration/test_complete_workflow.py::TestCompleteWorkflow::test_fpso_mooring_integration

# With verbose output
pytest tests/integration/ -v --tb=short
```

**Critical Tests:**
- `test_zero_storage_verification()` - Ensures no data files created
- `test_fpso_mooring_integration()` - Complete workflow (metocean + vessel + mooring)
- `test_standards_validation()` - API RP 2SK compliance
- `test_performance_metrics()` - Component search <1ms, mooring design <1s

## Performance

**Component Database Queries:**
- Chain selection: <1 ms per query
- Rope selection: <1 ms per query
- Anchor selection: <1 ms per query

**Mooring Design:**
- Catenary design: <1 second
- Taut-leg design: <1 second
- Semi-taut design: <1 second

**API Queries:**
- ERA5: ~2-5 seconds per request (depends on date range)
- NOAA: ~1-3 seconds per request
- GEBCO: <100 ms per bathymetry point

**Memory Footprint:**
- Streaming queries: Constant memory (one record at a time)
- Component databases: ~2 MB (all specifications in memory)

## Standards Compliance

### API RP 2SK (Stationkeeping Systems)

- **Safety Factors:**
  - Intact condition: 1.67
  - Damaged condition: 1.25
- **Component Capacity:** All components must have MBL ≥ design load × safety factor
- **Anchor Holding:** Must account for soil type and conditions

### DNV-OS-E301 (Position Mooring)

- Design load factor: 1.67 (ULS)
- Fatigue considerations for synthetic ropes
- Service life requirements

### API Spec 2F (Mooring Chain)

- Grade definitions (R3, R3S, R4, R4S, R5, R6)
- Proof load and MBL relationships
- Quality requirements

### ISO 20438 (Offshore Mooring Chain)

- Dimensional tolerances
- Mechanical properties
- Testing requirements

## Troubleshooting

### No Data Returned from Metocean APIs

**Problem:** `query_metocean()` returns empty iterator

**Solutions:**
1. Check date range (ERA5 has ~5-day delay for recent data)
2. Verify location coordinates (latitude: -90 to 90, longitude: -180 to 180)
3. Check API keys in environment variables or `~/.cdsapirc`
4. Try fallback API by setting `priority` in config

### RAO Calculation Issues

**Problem:** RAO values seem incorrect

**Solutions:**
1. Verify vessel type spelling (case-sensitive)
2. Check draft is within reasonable range (10-25m for VLCC)
3. Ensure wave periods are realistic (4-25 seconds)
4. Check wave headings are 0-180 degrees

### Mooring Design Validation Fails

**Problem:** `validation['valid'] == False`

**Solutions:**
1. Check design load vs. selected components
2. Verify safety factor (default 1.67 for intact)
3. Ensure anchor holding capacity sufficient (see `validation['issues']`)
4. For taut-leg, ensure water depth allows adequate pretension

### Component Database Returns No Results

**Problem:** `find_by_design_load()` returns empty list

**Solutions:**
1. Reduce safety factor (try 1.5 instead of 1.67)
2. Check design load is within database range (chains: 500-30,000 kN)
3. For specific grade/type, verify spelling (e.g., 'R4S' not 'R4s')
4. Use `compare_grades()` to see all options

## Contributing

Contributions welcome! Please ensure:

1. All new features include examples
2. Integration tests added for new functionality
3. Zero-storage principle maintained (no file I/O during operation)
4. Documentation updated

## License

MIT License - see LICENSE file for details

## Support

- **Documentation:** This README and example files
- **Issues:** Create GitHub issue with minimal reproducible example
- **Questions:** Include data source, parameters, and error message

## Citation

If using this framework in research/publications:

```
Digital Model Data Procurement Framework v4.0.0 (2025)
Universal data procurement for marine engineering via free APIs and repository databases
```

## Version History

- **4.0.0** (2025) - Repository integration, complete workflow examples, integration tests
- **3.0.0** (2025) - Added mooring systems module with component databases
- **2.0.0** (2025) - Added vessel systems module with RAOs and hydrostatics
- **1.0.0** (2024) - Initial release with metocean data module

---

**Remember:** Zero storage footprint - all data streamed/generated in-memory!
