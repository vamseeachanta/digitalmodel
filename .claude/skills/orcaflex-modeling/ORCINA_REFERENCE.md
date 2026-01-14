# Orcina OrcaFlex Documentation Reference

> **CRITICAL: Always use WebFetch to get latest information from these URLs before performing analysis.**
>
> Last Updated: 2026-01-13
> OrcaFlex Version: 11.6b

## Live Documentation URLs

| Resource | URL | Use For |
|----------|-----|---------|
| OrcaFlex Help | https://www.orcina.com/webhelp/OrcaFlex/ | Theory, parameters, modeling guidance |
| Orcina Examples | https://www.orcina.com/resources/examples/ | Working models, best practices |
| DNV Standards | https://www.dnv.com/rules-standards/ | Compliance requirements |

---

## OrcaFlex Documentation Structure (v11.6b)

The official OrcaFlex documentation is organized into these key sections:

### 1. User Interface
- **Introduction** - Getting started with OrcaFlex
- **OrcaFlex Files** - .dat, .yml, .sim file formats
- **Model Browser** - Navigating model hierarchy
- **Libraries** - Component libraries and templates
- **Menus** - Interface navigation
- **3D Views** - Visualization settings
- **Replays** - Animation playback
- **Data Forms** - Input data entry
- **Results** - Output data access
- **Graphs** - Plotting capabilities
- **Spreadsheets** - Tabular data handling
- **Text Windows** - Log and report viewing
- **Workspaces** - Layout management
- **Comparing Data** - Model comparison tools
- **Preferences** - Application settings

### 2. Automation
- Python API (OrcFxAPI)
- Batch processing
- Scripting capabilities

### 3. Theory

Core theoretical concepts for accurate modeling:

| Topic | Description | Use Case |
|-------|-------------|----------|
| **Coordinate Systems** | Global/local coordinate definitions | Model setup, results interpretation |
| **Direction Conventions** | Angle and direction standards | Wave, current, wind inputs |
| **Object Connections** | How objects connect and interact | Multi-body systems |
| **Interpolation Methods** | Data interpolation techniques | RAO, current profiles |
| **Static Analysis** | Equilibrium calculations | Initial conditions, mooring analysis |
| **Dynamic Analysis** | Time-domain simulation | Response analysis |
| **Friction Theory** | Contact and friction models | Seabed interaction, fenders |
| **Slamming Theory** | Wave impact calculations | Installation, splash zone |
| **Spectral Response Analysis** | Frequency-domain methods | Fatigue, extreme response |
| **Extreme Value Statistics** | Statistical methods for extremes | Design values |
| **Environment Theory** | Wave, current, wind modeling | Loading definition |
| **Vessel Theory** | Vessel hydrodynamics | RAOs, added mass, damping |
| **Line Theory** | Flexible line mechanics | Risers, moorings, cables |
| **6D Buoy Theory** | 6-DOF rigid body dynamics | Buoys, platforms |
| **3D Buoy Theory** | 3-DOF buoy dynamics | Simple buoys |
| **Winch Theory** | Winch mechanics | Tensioning, pay-out |
| **Shape Theory** | Geometric shapes | Seabed, structures |
| **Turbine Theory** | Turbine modeling | Offshore wind |

### 4. Modelling, Data and Results

Object types and their parameters:

| Object Type | Description | Key Parameters |
|-------------|-------------|----------------|
| **General Data** | Global model settings | Units, integration, convergence |
| **Environment** | Environmental conditions | Waves, current, wind, seabed |
| **Friction Coefficients** | Seabed friction data | Friction factors by soil type |
| **Wing Type Data** | Aerodynamic coefficients | Lift/drag for wind turbines |
| **Vessels** | Floating structures | RAOs, mass, CoG, mooring points |
| **Lines** | Flexible elements | Sections, segments, contents |
| **6D Buoys** | 6-DOF rigid bodies | Geometry, mass, hydrodynamics |
| **3D Buoys** | 3-DOF buoys | Simplified dynamics |
| **Winches** | Tensioning devices | Control modes, limits |
| **Links** | Rigid connections | Constraints, releases |
| **Shapes** | Geometric bodies | Contact surfaces |
| **Constraints** | Motion constraints | DOF restrictions |
| **Turbines** | Wind/tidal turbines | Blade data, control |
| **Supports** | Fixed supports | Boundary conditions |
| **Morison Elements** | Tubular members | Cd, Cm, diameter |

Additional modeling topics:
- Modelling Introduction
- Variation Models
- Restart Analyses
- Transferring Parent Model State
- Time Domain Filtering
- Time History Data
- Object Tags
- Variable Data
- All Objects Data Form
- Compound Object Properties Report

### 5. Modal Analysis
- Natural frequency extraction
- Mode shape visualization
- Eigenvalue analysis

### 6. Fatigue Analysis
- S-N curve application
- Damage accumulation
- Cycle counting

### 7. VIV Analysis
- Vortex-induced vibration
- SHEAR7 interface
- VIV fatigue

---

## Orcina Examples Catalog

**Live URL:** https://www.orcina.com/resources/examples/

Each example includes simulation files (.dat/.yml), PDF description, and supporting files.

### Example Categories

| Category | Key | Description |
|----------|-----|-------------|
| **A - Production Risers** | a | SCRs, lazy waves, hybrid risers, steel catenaries |
| **B - Drilling Risers** | b | Drilling riser systems |
| **C - Moorings** | c | Mooring systems, CALM buoys, fenders |
| **D - Riser Installation** | d | Installation sequences |
| **E - Pipelay and Recovery** | e | S-lay, J-lay, reeling, abandonment |
| **F - Payload Handling** | f | Lifting operations, crane analysis |
| **G - Deployment** | g | Subsea deployment operations |
| **H - Offloading Systems** | h | Tandem offloading, STS transfer |
| **I - Towed Systems** | i | Towing operations |
| **J - Defence** | j | Naval/defense applications |
| **K - Renewables** | k | Offshore wind, wave energy |
| **L - Diffraction** | l | OrcaWave integration, RAO generation |
| **M - Pipelines** | m | Pipeline analysis |
| **Z - Miscellaneous** | z | Special topics |

### Example URL Pattern

Access specific categories directly:
```
https://www.orcina.com/resources/examples/?key=<category_letter>
```

Examples:
- Production risers: `?key=a`
- Moorings: `?key=c`
- Renewables: `?key=k`

---

### Key Production Riser Examples (Category A)

| Example | Description | Key Concepts |
|---------|-------------|--------------|
| **A01** | Simple catenary riser | Basic SCR setup, catenary configuration |
| **A02** | Midwater arch systems | Lazy S, steep S, pliant S configurations, midwater arches, contact between lines |
| **A03** | Jumper to high tower | Deep water tower risers, flexible jumpers, spread-moored FPSO |
| **A04** | Disconnectable turret | FPSO turret with flexibles, connected/disconnecting/disconnected phases |
| **A05** | Steel catenary risers | SCR with spar, SCR with semisub configurations |

### Key Mooring Examples (Category C)

| Example | Description | Key Concepts |
|---------|-------------|--------------|
| **C05** | Single point mooring | 3-leg temporary mooring, chafe chain, mooring offset quantification |
| **C06** | CALM buoy | 6-line mooring, shuttle tanker, hawser, floating hose, coupled analysis, spar buoy short wave issue |
| **C08** | Fish farm | Two fish farm cages, cage moorings, 2-cell structure |
| **C09** | Fenders | Quayside fender modeling, constraint objects, non-linear deflection/damping, cell type fenders |

### Key Drilling Riser Examples (Category B)

| Example | Description | Key Concepts |
|---------|-------------|--------------|
| **B01** | Drilling riser system | Basic drilling riser configuration |
| **B02** | Tensioner systems | Drilling riser tensioning |

### Key Installation Examples (Category D)

| Example | Description | Key Concepts |
|---------|-------------|--------------|
| **D01** | Riser installation | Typical installation sequence |
| **D02** | J-tube pull-in | Subsea pull-in operations |

### Key Pipelay Examples (Category E)

| Example | Description | Key Concepts |
|---------|-------------|--------------|
| **E01** | S-lay vessel | S-lay pipelaying |
| **E02** | J-lay tower | J-lay operations |
| **E03** | Reeling | Reel-lay operations |

### Key Renewables Examples (Category K)

| Example | Description | Key Concepts |
|---------|-------------|--------------|
| **K01** | Floating wind turbine | Offshore wind modeling |
| **K02** | Wave energy converter | WEC dynamics |
| **K03** | Floating solar | Floating PV systems |

### Key Diffraction Examples (Category L)

| Example | Description | Key Concepts |
|---------|-------------|--------------|
| **L01** | OrcaWave vessel | Basic diffraction analysis |
| **L02** | Semi-submersible | Multi-body diffraction |
| **L03** | Coupled analysis | OrcaWave-OrcaFlex coupling |

---

## WebFetch Integration for Latest Information

**MANDATORY:** Before building new models or troubleshooting issues, use WebFetch to query the latest documentation.

### Query Templates

```python
# Query theory documentation
WebFetch(
    url="https://www.orcina.com/webhelp/OrcaFlex/",
    prompt="Explain the theory for [specific topic] in OrcaFlex"
)

# Query examples for specific application
WebFetch(
    url="https://www.orcina.com/resources/examples/",
    prompt="What examples are available for [application type]?"
)

# Get latest parameter guidance
WebFetch(
    url="https://www.orcina.com/webhelp/OrcaFlex/",
    prompt="What are the recommended settings for [parameter]?"
)
```

### Common Query Topics

| Topic | Query Prompt |
|-------|--------------|
| Line modeling | "Best practices for modeling flexible lines" |
| Vessel setup | "How to configure vessel RAOs and hydrodynamics" |
| Mooring analysis | "Key parameters for mooring system analysis" |
| Convergence | "Troubleshooting static analysis convergence issues" |
| Dynamic stability | "Time step selection for dynamic simulations" |
| Contact modeling | "How to model contact between lines and with shapes" |
| VIV analysis | "Setting up VIV analysis with SHEAR7 interface" |
| Fatigue | "Fatigue analysis setup and S-N curve selection" |

---

## Using Examples for Model Building

1. **Identify similar application** from example categories
2. **Download example ZIP** file for reference models
3. **Review PDF description** for modeling approach
4. **Adapt example** to your specific requirements
5. **Validate results** against example outputs

### Example Download Pattern

Each example typically contains:
- `.dat` or `.yml` model files
- PDF description document
- Workspace files (if applicable)
- Supporting data files

---

## Refresh Schedule

| Resource | Frequency | Action |
|----------|-----------|--------|
| Official docs | As needed | Use WebFetch for real-time queries |
| Examples catalog | Before new project types | Check for new/updated examples |
| Standards | Monthly | Review DNV, API, ISO updates |

---

## Local Examples Repository

Downloaded examples are stored locally for offline reference and as templates for new models.

### Directory Structure

```
data/orcaflex_examples/
├── yml_models/           # YAML configuration files
│   ├── C10_MultipleStatics.yml
│   ├── E08_PipelayConfig.yml
│   ├── L06_PotentialLoads.yml
│   └── L06_TimeVaryingQuadraticLoads.yml
├── python_scripts/       # OrcFxAPI automation scripts
│   ├── C10_MultipleStatics.py      # Multiple static analysis
│   ├── E08_CreateLayTable.py       # Pipelay table generation
│   ├── K01_GenPower.py             # Generator power results
│   ├── K01_PythonController.py     # NREL 5MW turbine controller
│   ├── K02_BladedControllerWrapper.py  # Bladed DLL wrapper
│   ├── K06_FPV_script.py           # Floating PV array builder
│   ├── L05_PanelPressures1.py      # Panel pressure extraction
│   └── L05_PanelPressures2.py      # 3D pressure visualization
└── [A-M,Z]_*/            # Category folders with extracted examples
```

### Update Script

Run the update script to download latest examples from Orcina:

```bash
# Update all categories
uv run python scripts/update_orcaflex_examples.py

# Update specific categories only
uv run python scripts/update_orcaflex_examples.py --categories a c k l

# Preview without downloading (dry run)
uv run python scripts/update_orcaflex_examples.py --dry-run

# Keep ZIP files after extraction
uv run python scripts/update_orcaflex_examples.py --keep-zips

# Remove PDF documentation (keep only code files)
uv run python scripts/update_orcaflex_examples.py --no-pdfs
```

### Script Features

- Downloads all example ZIPs from Orcina website
- Extracts and organizes files by category
- Removes binary files (.dat, .sim, .owr, .owd)
- Copies .yml and .py files to organized folders
- Supports selective category updates
- Dry-run mode for preview

### Recommended Update Schedule

| Trigger | Action |
|---------|--------|
| New OrcaFlex version release | Full update (`--categories` all) |
| Starting new project type | Update relevant category |
| Quarterly maintenance | Full update to catch new examples |

---

*This reference is auto-generated from Orcina documentation. For the latest information, always use WebFetch to query the live URLs.*
