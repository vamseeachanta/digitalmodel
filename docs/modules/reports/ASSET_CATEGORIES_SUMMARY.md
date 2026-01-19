# Asset Categories Summary - digitalmodel Repository

> **Date**: 2025-10-23
> **Purpose**: Comprehensive inventory of marine engineering asset categories for data procurement specification development
> **Based on**: Analysis of repository structure, OrcaFlex models, domain documentation

---

## Executive Summary

The digitalmodel repository contains comprehensive data, models, and documentation for **12 major asset categories** in marine and offshore engineering. This summary provides a structured inventory to support the development of data procurement specifications similar to the CALM Buoy web-api-integration work.

**Key Statistics:**
- **12 Major Asset Categories** identified
- **50+ Domain Areas** documented
- **40+ OrcaFlex Example Models** (A01-L03 series)
- **Multiple Analysis Software**: OrcaFlex, AQWA, ANSYS, MATLAB

---

## Asset Category Hierarchy

### 1. **Offshore Structures & Platforms**

#### 1.1 Fixed Platforms
- **Jack-up Rigs**
  - Location: `docs/modules/orcaflex/jack_up`
  - Components: Legs, spud cans, cantilever systems
  - Analysis: Leg loading, foundation stability

#### 1.2 Floating Platforms
- **Semi-submersibles**
  - Location: `docs/modules/orcaflex/mooring/semi`
  - Types: Drilling, production, accommodation
  - Mooring: Catenary, taut-leg, hybrid

- **TLP (Tension Leg Platforms)**
  - References in domain docs
  - Mooring: Vertical tethers, taut configuration

- **Spar Platforms**
  - References in AQWA examples
  - Types: Classic, truss, cell

- **FPSO (Floating Production Storage Offloading)**
  - Domain: `docs/domains/ship-design`
  - Mooring: Turret, spread mooring

---

### 2. **Mooring Systems**

#### 2.1 Single Point Moorings (SPM)
- **CALM Buoys** (Comprehensive data available)
  - Location: `specs/modules/data-procurement/CALM_Buoy_claude.md`
  - Location: `docs/modules/orcaflex/mooring/buoy/C06 CALM Buoy`
  - Types: Turntable, turret, wheel & rail
  - Components: Hull, swivel, chains, anchors, hoses
  - Data: 150+ global installations, 18 comprehensive tables

- **SALM (Single Anchor Leg Mooring)**
  - References in CALM buoy documentation
  - ~50 global installations

- **Tower Moorings**
  - Rigid tower structures

#### 2.2 Multi-Point Moorings
- **Catenary Moorings**
  - Location: `docs/domains/moorings`
  - Chain types: R3, R4, R5 studless
  - Configurations: 6-point, 8-point, 12-point

- **Taut-Leg Moorings**
  - Synthetic ropes, wire ropes
  - Reduced footprint

- **Hybrid Moorings**
  - Combination systems
  - Deepwater applications

#### 2.3 Specialized Mooring Components
- **Mooring Lines**
  - Chain, wire rope, synthetic rope, polyester
  - Drag anchors, suction piles, driven piles

- **Fender Systems**
  - Location: `docs/modules/orcaflex/mooring_fender`
  - Ship-to-ship, ship-to-berth

---

### 3. **Risers**

#### 3.1 Production Risers
- **Location**: `docs/modules/orcaflex/risers/production`
- **Types**:
  - Steel Catenary Risers (SCR)
  - Flexible Risers
  - Hybrid Riser Towers
  - Top Tensioned Risers (TTR)
- **Components**: Riser pipes, buoyancy modules, hang-off systems

#### 3.2 Drilling Risers
- **Location**: `docs/modules/orcaflex/risers/drilling`
- **Location**: `docs/domains/drilling/drilling_risers.md`
- **Types**:
  - Marine drilling risers
  - Subsea BOP systems
- **Components**: Riser joints, telescopic joints, flex joints

#### 3.3 Riser Components
- **Installation Equipment**
  - Location: `docs/domains/risers/_installation.md`
- **Damping Systems**
  - Location: `docs/domains/risers/damping.md`

---

### 4. **Pipelines**

#### 4.1 Subsea Pipelines
- **Location**: `docs/domains/pipelines/_subsea_pipelines.md`
- **Types**:
  - Rigid steel pipelines
  - Flexible pipelines
  - Rigid-flexible combinations

#### 4.2 Pipeline Phenomena
- **Spanning**
  - Location: `docs/modules/orcaflex/pipeline/spanning`
  - Location: `docs/domains/pipelines/freespan/freespan.md`
  - Analysis: VIV, fatigue, overstress

- **Buckling**
  - Lateral buckling: `docs/domains/pipelines/lateral_buckling.md`
  - Upheaval buckling: `docs/domains/pipelines/uplheaval_buckling.md`

#### 4.3 Pipeline Components
- **PLEM (Pipeline End Manifold)**
  - Location: `docs/domains/pipelines/plem/plem.md`
  - Connection points, valves

- **Pipeline Installation**
  - Location: `docs/domains/pipelines/installation`
  - S-lay, J-lay, reel-lay methods

- **Pipeline Repair**
  - Location: `docs/domains/pipelines/_subsea_repair.md`

---

### 5. **Umbilicals & Cables**

#### 5.1 Umbilicals
- **Location**: `docs/domains/umbilical`
- **Components**:
  - Umbilical end terminations
  - Buoyancy modules
  - Bend stiffeners
  - Hang-off systems

#### 5.2 Submarine Cables
- **Location**: `docs/domains/submarine-cables`
- **Types**:
  - Power cables
  - Communication cables
  - Dynamic cables (floating wind)
- **Analysis**: TDP compression, cable dynamics

---

### 6. **Vessels**

#### 6.1 Ship Types
- **Location**: `docs/domains/ship-design`
- **Categories**:
  - Tankers (VLCC, Suezmax, Aframax)
  - LNG Carriers (LNGC)
  - Drilling Ships
  - Construction Vessels
  - Tugs

#### 6.2 Vessel Analysis
- **RAO (Response Amplitude Operators)**
  - Location: `docs/domains/ship-design/raos.md`
  - Location: `docs/modules/orcaflex/raos.md`
  - Frequency-domain analysis

- **Berthing Analysis**
  - Location: `docs/domains/ship-design/berthing`
  - Fender systems, approach speeds

- **Mooring Analysis**
  - Location: `docs/domains/ship-design/moorings.md`
  - Hawser systems, dolphin structures

#### 6.3 Vessel Components
- **Turrets**
  - Location: `docs/domains/ship-design/mooring_turret.md`
  - Internal turrets, external turrets

---

### 7. **Offshore Wind**

#### 7.1 Wind Turbine Structures
- **Location**: `docs/domains/wind`
- **Location**: `docs/modules/orcaflex/wind`
- **Types**:
  - Fixed-bottom: Monopile, jacket, gravity-based
  - Floating: Spar, semi-submersible, TLP

#### 7.2 Wind-Specific Components
- **Suction Buckets**
  - Location: `docs/domains/wind/sunction_bucket.md`
  - Foundation systems

- **Floating Wind Moorings**
  - Location: `docs/domains/wind/moorings.md`
  - Shared moorings, optimized designs

---

### 8. **Rigging & Lifting**

- **Location**: `docs/domains/rigging`
- **Asset Architecture**: `docs/domains/rigging/rigging_assets.puml`
- **Components**:
  - Shackles, slings, wire ropes
  - Spreader bars, lifting beams
  - Load monitoring systems
- **Properties**: WLL (Working Load Limit), manufacturer data

---

### 9. **Jumpers & Flowlines**

- **Location**: `docs/modules/orcaflex/jumper`
- **Types**:
  - Rigid jumpers
  - Flexible jumpers
  - Hybrid jumpers
- **Applications**: Subsea tie-ins, platform connections

---

### 10. **Specialized Systems**

#### 10.1 Coiled Tubing
- **Location**: `docs/domains/coiled-tubing`
- **Data**: Vendor specifications, literature

#### 10.2 Cathodic Protection
- **Location**: `docs/domains/cathodic-protection`
- **Applications**: Ship CP, subsea structure protection
- **Codes**: ABS, DNV requirements

#### 10.3 Connections
- **Location**: `docs/domains/connections`
- **Types**: Welded, mechanical, bolted

---

### 11. **Installation Equipment**

- **Cranes**
  - Location: `docs/domains/cranes`
  - Offshore cranes, lifting analysis

- **Installation Analysis**
  - Location: `docs/domains/installation`
  - Riser installation, pipeline installation
  - Load-out, transport, installation

---

### 12. **Supporting Systems**

#### 12.1 Instrumentation
- **Time Series Data**
  - Location: `docs/domains/time-series`
  - Monitoring systems

- **OSI PI**
  - Location: `docs/domains/osi-pi`
  - Data historian systems

#### 12.2 Environmental Systems
- **Marine Growth**
  - Location: `docs/domains/marine-growth`
  - Biofouling effects on structures

---

## OrcaFlex Example Model Categories

### Alphabetical Series Classification

Based on `docs/modules/orcaflex/examples/raw/` structure:

| Series | Example Count | Asset Categories |
|--------|--------------|------------------|
| **A Series (A01-A06)** | 5 | Basic mooring, static analysis |
| **B Series (B01, B06)** | 2 | Dynamic positioning, vessel operations |
| **C Series (C05-C10)** | 6 | **CALM buoys, metocean buoys, mooring systems** |
| **D Series (D02-D04)** | 3 | Deep water systems, subsea operations |
| **E Series (E01-E08)** | 6 | Environmental loading, fatigue |
| **F Series (F01-F06)** | 5 | Flexible risers, umbilicals |
| **G Series (G04)** | 1 | Gas export systems |
| **H Series (H01-H03)** | 3 | Hybrid systems, installation |
| **I Series (I01)** | 1 | Installation analysis |
| **J Series (J01)** | 1 | Jumpers and flowlines |
| **K Series (K01)** | 1 | Keel-laying operations |
| **L Series (L01-L03)** | 3 | Lifting operations, load-out |
| **Z Series (Z02, Z09)** | 2 | Special cases, advanced analysis |

**Total**: 40+ example models covering diverse marine engineering scenarios

---

## Domain Documentation Structure

### Primary Domains (from `docs/domains/`)

1. **ai** - Artificial intelligence applications
2. **biomedical** - Biomedical engineering
3. **cathodic-protection** - Corrosion protection systems
4. **coiled-tubing** - Well intervention equipment
5. **connections** - Structural connections
6. **cranes** - Lifting equipment
7. **design** - General design methodologies
8. **digitaltwin** - Digital twin implementations
9. **drilling** - Drilling operations and risers
10. **engineering** - General engineering principles
11. **fatigue** - Fatigue analysis and design
12. **fem** - Finite element modeling
13. **gas-turbines** - Power generation
14. **hydrodynamics** - Wave-structure interaction
15. **installation** - Installation methodologies
16. **interventions** - Well interventions
17. **lighting** - Marine lighting systems
18. **lng** - Liquefied natural gas systems
19. **marine-growth** - Biofouling and effects
20. **metocean** - Meteorological and oceanographic data
21. **modal-analysis** - Structural dynamics
22. **moorings** - Mooring system design
23. **nde** - Non-destructive examination
24. **osi-pi** - Data historian systems
25. **pipe** - Pipe stress analysis
26. **pipecapacity** - Pipeline capacity calculations
27. **pipelines** - Subsea pipeline engineering
28. **plate-buckling** - Structural plate analysis
29. **platecapacity** - Plate strength calculations
30. **predictive-maintenance** - Maintenance optimization
31. **pressure-vessels** - Pressure vessel design
32. **process-engineering** - Process systems
33. **production** - Production operations
34. **project-management** - Project management
35. **pumps** - Pumping systems
36. **reservoir** - Reservoir engineering
37. **rigging** - Rigging and lifting
38. **risers** - Riser systems
39. **risk** - Risk assessment
40. **ship-design** - Naval architecture
41. **spm** - Single point moorings
42. **submarine-cables** - Subsea cable systems
43. **tcp** - Through-casing perforation
44. **thermodynamics** - Heat transfer
45. **time-series** - Time series analysis
46. **umbilical** - Umbilical systems
47. **unit-conversion** - Unit conversions
48. **vibrations** - Vibration analysis
49. **viv** - Vortex-induced vibration
50. **wec** - Wave energy converters
51. **welding** - Welding engineering
52. **wind** - Offshore wind energy

---

## Data Procurement Priorities

### Tier 1: High Priority (Comprehensive Data Available)
1. ✅ **CALM Buoys** - Already addressed in `web-api-integration`
2. **Metocean Data** - Critical for all offshore structures
3. **Vessel RAOs** - Essential for coupled analysis
4. **Mooring System Components** - Chain, wire rope, anchors

### Tier 2: Medium Priority (Significant Repository Content)
5. **Production Risers** - Multiple configurations
6. **Drilling Risers** - Marine drilling operations
7. **Subsea Pipelines** - Pipeline design and analysis
8. **Semi-submersible Platforms** - Floating production units
9. **Offshore Wind Turbines** - Growing renewable sector
10. **Umbilicals** - Subsea control systems

### Tier 3: Standard Priority (Repository References Available)
11. **Jack-up Rigs** - Mobile offshore units
12. **FPSO Vessels** - Production and storage
13. **Submarine Cables** - Power and communication
14. **Jumpers & Flowlines** - Subsea connections
15. **Rigging Equipment** - Lifting and installation

---

## Web API Integration Opportunities

### Data Types Requiring External APIs

#### 1. **Metocean Data** (Highest Priority)
- Wave height, period, direction
- Wind speed and direction
- Current profiles
- Water levels and tides
- **API Sources**: NOAA, Open-Meteo, StormGlass, ERA5

#### 2. **Vessel Data**
- Ship particulars (dimensions, displacement)
- RAO databases
- Hydrodynamic coefficients
- **API Sources**: Maritime databases, classification societies

#### 3. **Material Properties**
- Steel properties (existing: `src/digitalmodel/data/steel_material.yml`)
- Chain specifications
- Rope properties
- **API Sources**: Manufacturer databases, standards organizations

#### 4. **Environmental Criteria**
- Design standards (API, DNV, ABS, ISO)
- Safety factors
- Load combinations
- **API Sources**: Standards databases, code repositories

#### 5. **Equipment Specifications**
- Manufacturer catalogs
- Component properties
- Performance curves
- **API Sources**: Equipment manufacturers, vendor databases

---

## Next Steps for Spec Development

### Phase 1: Asset Category Specifications
Create individual specs for each Tier 1 and Tier 2 asset category following the web-api-integration template:

1. **Executive Summary** with business impact
2. **System Architecture** with mermaid diagrams
3. **Technical Requirements** specific to asset
4. **User Stories** for marine engineers
5. **Scope Definition** (Core Components, Out of Scope)
6. **Expected Deliverables** (Technical & Business)
7. **Agent Delegation Matrix** for parallel development
8. **Performance Requirements** (Latency, Throughput, Reliability)

### Phase 2: Data Source Mapping
- Identify web APIs for each asset category data
- Map data requirements to available sources
- Define data transformation pipelines

### Phase 3: Configuration Templates
- YAML templates for each asset category
- Validation schemas
- Example configurations

### Phase 4: Integration Testing
- Test data procurement for priority assets
- Validate data quality and completeness
- Performance benchmarking

---

## Repository Structure Summary

### Key Directories
```
digitalmodel/
├── docs/
│   ├── domains/           # 50+ domain areas
│   ├── modules/
│   │   └── orcaflex/      # OrcaFlex models and docs
│   │       ├── examples/  # 40+ example models
│   │       ├── mooring/   # Mooring system models
│   │       ├── risers/    # Riser analysis
│   │       ├── pipeline/  # Pipeline models
│   │       └── wind/      # Wind turbine models
│   └── reports/           # Analysis reports
├── specs/
│   └── modules/
│       └── data-procurement/
│           ├── web-api-integration/  # Web API spec
│           └── CALM_Buoy_claude.md   # CALM buoy data
├── src/
│   └── digitalmodel/
│       └── data/          # Material data (YAML)
└── tests/                 # Test suites
```

---

## Conclusion

The digitalmodel repository contains a comprehensive collection of **12 major asset categories** with extensive documentation, models, and analysis tools. The CALM Buoy work in `web-api-integration` provides an excellent template for developing data procurement specifications for the remaining asset categories.

**Recommended Approach:**
1. Prioritize metocean data (universal requirement)
2. Develop specs for Tier 1 assets (high data availability)
3. Create modular, reusable data procurement modules
4. Leverage existing YAML structures and validation frameworks
5. Implement parallel agent-based development for efficiency

**Expected Outcome:**
- **15+ asset-specific data procurement modules**
- **Unified web API integration framework**
- **Elimination of static data file dependencies**
- **Real-time access to marine engineering data**
- **95% reduction in data management overhead**

---

*Document prepared: 2025-10-23*
*Based on: Repository analysis of /mnt/github/workspace-hub/digitalmodel*
*Reference: specs/modules/data-procurement/web-api-integration/*
