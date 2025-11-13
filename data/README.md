# Digital Model Data Repository

**Version:** 1.0.0
**Last Updated:** 2025-10-24
**Total Files:** 267
**Total Size:** ~235 MB

## 📋 Overview

This repository contains comprehensive marine engineering data organized by domain with clear pipeline stages (raw → processed → results) for systematic data management, analysis, and reporting.

**Organization Philosophy:**
- **Domain-Driven Structure:** Engineering disciplines map to folder structure
- **Pipeline Stages:** Separate raw data, processed datasets, and analysis results
- **Web-Ready:** Templates and structure optimized for data enhancement from web sources
- **Analysis-Ready:** Supports engineering calculations, ML training, HTML reports, and API serving

**Future Alignment:** Domain names will align with module names across all repositories for consistency.

---

## 🗂️ Data Domains

### 1. Fatigue Analysis (`fatigue/`)
**Purpose:** Fatigue S-N curve database for structural analysis

**Contents:**
- 221 S-N curves from 17 international standards
- Standards: DNV, API, BS, BP, Norsok, Bureau Veritas, ABS
- Joint types: Plated welded, Tubular, Tubular nodal
- Environments: Air, Seawater with CP, Free Corrosion

**Quick Access:**
```python
import pandas as pd
curves = pd.read_csv('data/fatigue/processed/fatigue_curves_structured.csv')
```

**Documentation:** [fatigue/README.md](fatigue/README.md)

---

### 2. Vessels (`vessels/`)
**Purpose:** Offshore vessel specifications and databases

**Contents:**
- FPSO systems database (2018)
- Deepwater drilling rigs (2014)
- Jackup rigs (2015)
- Pipelay vessels (2013)
- Drilling rig fleet information

**Key Datasets:**
- `fpso_database_2018.csv` - FPSO specifications and capabilities
- `deepwater_drilling_rigs_2014.csv` - Deepwater rig specifications
- `jackup_rigs_2015.csv` - Jackup rig database
- `pipelay_vessels_2013.csv` - Pipelay vessel capabilities

**Documentation:** [vessels/README.md](vessels/README.md)

---

### 3. Mooring & Station-Keeping (`mooring/`)
**Purpose:** Mooring analysis, OCIMF coefficients, and CALM buoy systems

**Contents:**
- **OCIMF Data:** Wind and current coefficients for vessel types
  - Production database with full coefficient sets
  - Sample database for testing
- **CALM Buoy Systems:** Design data and analysis results
- **Mooring Components:** Chain, wire, anchors, connectors

**Key Datasets:**
- `ocimf/ocimf_database.csv` - Complete OCIMF coefficient database
- `calm_buoy/` - CALM buoy design and analysis pipeline

**Documentation:** [mooring/README.md](mooring/README.md)

---

### 4. Hydrodynamic Analysis (`hydrodynamic/`)
**Purpose:** Hydrodynamic coefficients for motion analysis

**Contents:**
- **Added Mass:** 83 frequency-dependent coefficient files (ω = 0.1 to 3.0 rad/s)
- **Damping:** 90 frequency-dependent coefficient files
- **RAOs:** Response Amplitude Operators (to be added)

**File Format:** `added_mass_omega_{frequency}.csv`, `damping_omega_{frequency}.csv`

**Quick Access:**
```python
import pandas as pd
import glob

# Load all added mass data
added_mass_files = glob.glob('data/hydrodynamic/raw/added_mass/*.csv')
added_mass_data = [pd.read_csv(f) for f in added_mass_files]
```

**Documentation:** [hydrodynamic/README.md](hydrodynamic/README.md)

---

### 5. Equipment Specifications (`equipment/`)
**Purpose:** Offshore equipment specifications and catalogs

**Contents:**
- **Anchors:** Anchor types, capacities, specifications
- **Buoys:** Buoy types and specifications
- **Fenders:** Fender design data and resources
- **Injector Heads:** Subsea injection equipment
- **Manifolds:** Subsea manifold specifications
- **SPM (Single Point Mooring):** Company contacts and cost estimates
- **X-Trees (Christmas Trees):** Subsea tree specifications

**Documentation:** [equipment/README.md](equipment/README.md)

---

### 6. Riser Systems (`riser_systems/`)
**Purpose:** Riser and flowline system specifications

**Contents:**
- **Drilling Risers:**
  - Offshore drilling riser model properties
  - Rig data, BOP specifications, drilling equipment
  - Mooring and station-keeping details
  - Mud pumps, lifting equipment
  - Riser and tensioner data
  - Vessel particulars
  - Moonpool and drillship information
- **Production Risers:** (to be added)
- **Export Risers:** (to be added)

**Key Datasets:**
- `drilling_risers/offshore/drilling_riser_model_properties.csv`
- `drilling_risers/offshore/drillrigs.csv` - Complete rig database
- Multiple rig detail sheets (BOP, equipment, mooring, etc.)

**Documentation:** [riser_systems/README.md](riser_systems/README.md)

---

### 7. Reference Materials (`reference_materials/`)
**Purpose:** Industry standards, posters, and reference documents

**Contents:**
- **Industry Posters:** Drilling rig poster advertisements and data
- **Standards:** (to be added)
- **Specifications:** (to be added)

**Documentation:** [reference_materials/README.md](reference_materials/README.md)

---

## 📂 Pipeline Stages

Each domain follows a consistent 3-stage pipeline:

### `raw/`
**Purpose:** Original, unmodified data from sources
- Web-scraped data (CSV/ASCII format)
- Manual data entry
- Vendor-provided specifications
- Standards and publications

**Rules:**
- Never modify raw data files
- Keep original file names when possible
- Add source metadata in domain README

### `processed/`
**Purpose:** Cleaned, standardized, analysis-ready data
- Standardized column names
- Unit conversions completed
- Missing data handled
- Combined/merged datasets
- Quality checks passed

**Rules:**
- Document processing steps in scripts
- Maintain traceability to raw data
- Store in consistent CSV/JSON formats

### `results/`
**Purpose:** Analysis outputs, reports, visualizations
- Calculation results
- Interactive HTML reports (Plotly, Bokeh, Altair)
- Statistical analyses
- Machine learning outputs
- Engineering design results

**Rules:**
- Include relative CSV paths for HTML reports
- No hardcoded paths
- Interactive plots only (no static matplotlib)

---

## 🚀 Quick Start Examples

### Python - Load Data from GitHub

```python
import pandas as pd

# Base URL for raw data
base_url = "https://raw.githubusercontent.com/vamseeachanta/digitalmodel/main/data"

# Load fatigue curves
fatigue_curves = pd.read_csv(f"{base_url}/fatigue/processed/fatigue_curves_structured.csv")

# Load OCIMF coefficients
ocimf_data = pd.read_csv(f"{base_url}/mooring/raw/ocimf/ocimf_database.csv")

# Load FPSO database
fpso_db = pd.read_csv(f"{base_url}/vessels/raw/fpso_database_2018.csv")
```

### Python - Local Data Access

```python
import pandas as pd
from pathlib import Path

# Define data root
data_root = Path(__file__).parent / "data"

# Load hydrodynamic data
added_mass = pd.read_csv(data_root / "hydrodynamic/raw/added_mass/added_mass_omega_0.5000.csv")

# Load vessel data
vessels = pd.read_csv(data_root / "vessels/raw/deepwater_drilling_rigs_2014.csv")
```

### R - Load Data

```r
# Load fatigue curves
base_url <- "https://raw.githubusercontent.com/vamseeachanta/digitalmodel/main/data"
fatigue_curves <- read.csv(paste0(base_url, "/fatigue/processed/fatigue_curves_structured.csv"))

# Load OCIMF data
ocimf_data <- read.csv(paste0(base_url, "/mooring/raw/ocimf/ocimf_database.csv"))
```

---

## 📊 Data Statistics

**Total Files:** 267
- CSV: 209 files
- PDF: 41 files
- Markdown: 9 files
- JSON: 3 files
- PNG: 4 files
- Other: 1 file

**Total Size:** ~235 MB

**Domain Distribution:**
- `hydrodynamic/`: 173 files (~228 MB) - Frequency-dependent coefficients
- `fatigue/`: 7 files (~349 KB) - S-N curve database
- `vessels/`: 5 files (~1.2 MB) - Vessel databases
- `mooring/`: 6 files (~500 KB) - OCIMF and CALM buoy data
- `riser_systems/`: 14 files (~5 MB) - Drilling riser data
- `equipment/`: 4 files (~50 KB) - Equipment specifications
- `reference_materials/`: 4 files (~1 MB) - Industry references

See [METADATA.json](METADATA.json) for complete inventory.

---

## 🌐 Web Data Enhancement

This repository is optimized for adding updated data from web sources:

### Templates Directory
Use standardized templates for web scraping:
- `templates/vessel_template.csv` - Vessel database schema
- `templates/equipment_template.csv` - Equipment specification schema
- `templates/metadata_template.json` - Metadata structure

### Adding New Data
1. Download/scrape data in CSV/ASCII format
2. Place in appropriate domain's `raw/` directory
3. Document source in domain README
4. Process using domain-specific scripts
5. Validate and commit

### Data Sources
- Real-time vessel specifications (vessel tracking APIs)
- Updated standards/regulations (classification society websites)
- Equipment manufacturer catalogs (vendor websites)
- Environmental/metocean data (NOAA, meteorological agencies)
- Market/economic data (industry reports)

---

## 🛠️ Usage Workflows

### Engineering Analysis
1. Load raw data from appropriate domain
2. Process using analysis scripts
3. Generate results in `results/` directory
4. Create HTML reports with interactive plots

### Machine Learning
1. Combine datasets from multiple domains
2. Preprocess in domain `processed/` directories
3. Train models using processed data
4. Store model outputs in `results/`

### HTML Reporting
1. Import processed CSV data with relative paths
2. Generate interactive plots (Plotly, Bokeh, Altair)
3. Create HTML reports in `results/` directory
4. Link to source CSV files

### API Data Serving
1. Load processed data from domains
2. Serve via REST/GraphQL endpoints
3. Cache frequently accessed datasets
4. Document API schemas in domain READMEs

---

## 📝 File Naming Conventions

**Standard Format:** `{category}_{subcategory}_{year}.{ext}`

**Examples:**
- ✅ `fpso_database_2018.csv` - Clear, descriptive, includes year
- ✅ `drilling_rigs_deepwater_2014.csv` - Category, subcategory, year
- ✅ `ocimf_coefficients_production.csv` - Standard format
- ✅ `fatigue_curves_structured.csv` - Processing stage clear
- ✅ `added_mass_omega_0.5000.csv` - Parameter in filename

**Avoid:**
- ❌ Date prefixes: `20141023_data.csv`
- ❌ Sheet references: `database_sheet1.csv`
- ❌ Generic names: `data.csv`, `output.csv`
- ❌ Special characters: `data(final).csv`, `db@2018.csv`

---

## 🔍 Domain README Files

Each domain contains a detailed README.md with:
- Domain purpose and scope
- Data sources and references
- File descriptions and schemas
- Usage examples (Python, R, Excel)
- Pipeline stage definitions
- Related domains and cross-references
- Known issues and limitations

---

## 📚 Related Documentation

- **Reorganization Plan:** [docs/DATA_REORGANIZATION_PLAN.md](../docs/DATA_REORGANIZATION_PLAN.md)
- **File Organization Standards:** [docs/FILE_ORGANIZATION_STANDARDS.md](../docs/FILE_ORGANIZATION_STANDARDS.md)
- **HTML Reporting Standards:** [docs/HTML_REPORTING_STANDARDS.md](../docs/HTML_REPORTING_STANDARDS.md)
- **Development Workflow:** [docs/DEVELOPMENT_WORKFLOW.md](../docs/DEVELOPMENT_WORKFLOW.md)

---

## 🤝 Contributing

### Adding New Data
1. Identify appropriate domain or create new domain
2. Place data in domain's `raw/` directory
3. Document source and metadata in domain README
4. Create processing scripts if needed
5. Submit pull request with data and documentation

### Data Quality Standards
- ✅ CSV format preferred for tabular data
- ✅ ASCII/text formats for compatibility
- ✅ Include units in column headers or metadata
- ✅ Document all assumptions and sources
- ✅ Validate data before committing

### Creating New Domains
1. Create domain directory with `raw/`, `processed/`, `results/` subdirectories
2. Add domain README.md using template
3. Update this file with domain description
4. Update METADATA.json with domain statistics
5. Create templates if applicable

---

## 📄 License

This database is compiled from publicly available standards, published research, and industry data. Please verify compliance with applicable standards and licenses for commercial use.

Individual datasets may have specific licenses - see domain README files for details.

---

## 📧 Contact

**Repository:** https://github.com/vamseeachanta/digitalmodel
**Issues:** https://github.com/vamseeachanta/digitalmodel/issues
**Maintainer:** Vamsee Achanta

For questions about specific domains, see individual domain README files for contact information.

---

**Last Updated:** 2025-10-24
**Repository Version:** 1.0.0
**Total Domains:** 7
**Total Files:** 267
