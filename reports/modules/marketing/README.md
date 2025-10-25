# Digital Model Marketing Materials

## Overview

This directory contains professional marketing brochures for Digital Model modules. Each brochure is available in both Markdown and PDF formats, targeting engineering managers with junior-engineer level technical detail.

## Available Brochures

### Completed (6 modules)

1. **Fatigue Analysis** - Comprehensive S-N curve database and fatigue assessment
   - File: `marketing_brochure_fatigue_analysis.md` / `.pdf`
   - Highlights: 221 S-N curves, 17 international standards, time/frequency domain analysis

2. **Stress Analysis** - Von Mises, nonlinear plasticity, pipe stress
   - File: `marketing_brochure_stress_analysis.md` / `.pdf`
   - Highlights: Multi-axial stress, ASME/API compliance, nonlinear plasticity

3. **Marine Analysis** - Hydrodynamics and RAO processing
   - File: `marketing_brochure_marine_analysis.md` / `.pdf`
   - Highlights: Unified RAO reader, AQWA/OrcaFlex/WAMIT support, wave loading

4. **Pipeline Analysis** - Sizing, capacity, on-bottom stability
   - File: `marketing_brochure_pipeline_analysis.md` / `.pdf`
   - Highlights: DNV-RP-F109 stability, API STD 2RD capacity, hydraulic sizing

5. **OrcaFlex Integration** - Automated modeling and post-processing
   - File: `marketing_brochure_orcaflex_integration.md` / `.pdf`
   - Highlights: YAML-based automation, real-time dashboard, 90% time savings

6. **API 579 Fitness for Service** - Asset integrity and corrosion management
   - File: `marketing_brochure_api579_ffs.md` / `.pdf`
   - Highlights: Complete API 579-1/ASME FFS-1 implementation, 13 assessment parts, 30-50% life extension

### Planned (10+ additional modules)

- VIV Analysis - Vortex-induced vibration assessment
- Data Procurement - Metocean data streaming (ERA5, NOAA, GEBCO)
- Catenary Analysis - Riser design (SCR, SLWR)
- Mooring Systems - SALM, anchor design
- Time Series Analysis - FFT, signal processing
- AQWA Integration - Hydrodynamic analysis
- Plate Analysis - Buckling and capacity
- RAO Analysis - Response amplitude operators
- Signal Analysis - Advanced signal processing
- Vertical Riser - Riser analysis tools

## Generating PDFs

### Prerequisites

Install pandoc and optionally wkhtmltopdf for better PDF quality:

```bash
# macOS
brew install pandoc
brew install wkhtmltopdf

# Linux (Debian/Ubuntu)
sudo apt-get install pandoc
sudo apt-get install wkhtmltopdf

# Windows
# Download from: https://pandoc.org/installing.html
```

### Generate All PDFs

Run the automated script:

```bash
cd /mnt/github/workspace-hub/digitalmodel
./scripts/generate_marketing_pdfs.sh
```

This will:
1. Find all `marketing_brochure_*.md` files
2. Generate professional PDFs with table of contents
3. Apply consistent styling and formatting
4. Output progress and summary

### Generate Individual PDF

```bash
pandoc marketing_brochure_fatigue_analysis.md \
    -o marketing_brochure_fatigue_analysis.pdf \
    --from markdown \
    --to pdf \
    --variable geometry:margin=1in \
    --variable fontsize=11pt \
    --toc \
    --toc-depth=2
```

## Brochure Structure

Each brochure follows a consistent 2-page structure:

### Page 1: Overview & Capabilities
- Module overview and value proposition
- Core capabilities (5-7 key features)
- Industry standards compliance
- Technical features breakdown

### Page 2: Benefits & Integration
- Key benefits (4 categories)
- Output examples with code/reports
- Integration capabilities
- Module statistics and applications
- Contact information

## Content Guidelines

### Technical Level
- **Audience**: Engineering managers
- **Depth**: Junior engineer level (detailed but accessible)
- **Style**: Professional, technical, quantifiable

### Key Elements
- ✅ **Quantifiable benefits** - "90% time reduction", "221 S-N curves"
- ✅ **Industry standards** - DNV, API, ASME, BS references
- ✅ **Real capabilities** - no marketing fluff, actual features
- ✅ **Code examples** - show ease of use
- ✅ **Output samples** - demonstrate value

## Creating New Brochures

### 1. Use the Template

Copy an existing brochure structure (e.g., `marketing_brochure_fatigue_analysis.md`)

### 2. Research Module Capabilities

```bash
# Read module documentation
cat src/digitalmodel/modules/<module_name>/__init__.py
cat src/digitalmodel/modules/<module_name>/README.md

# Check test files for capabilities
ls tests/<module_name>/
```

### 3. Extract Real Data

- S-N curve counts, standards from data files
- Feature lists from `__init__.py` imports
- Code examples from tests and examples
- Output examples from reports/ directory

### 4. Follow Naming Convention

- Markdown: `marketing_brochure_<module_name>.md`
- PDF: `marketing_brochure_<module_name>.pdf`
- Module name: lowercase, underscores (e.g., `fatigue_analysis`, `marine_analysis`)

### 5. Generate PDF

```bash
./scripts/generate_marketing_pdfs.sh
```

## Quality Checklist

Before finalizing a brochure, verify:

- [ ] **Accuracy**: All statistics and features are verified from code
- [ ] **Completeness**: Both pages filled with valuable content
- [ ] **Consistency**: Follows template structure
- [ ] **Technical**: Appropriate depth for junior engineers
- [ ] **Benefits**: Clear, quantifiable value propositions
- [ ] **Examples**: Real code snippets and outputs
- [ ] **Standards**: Industry codes explicitly referenced
- [ ] **Contact**: Up-to-date contact information
- [ ] **PDF**: Professional appearance, readable fonts, proper margins

## File Organization

```
reports/modules/marketing/
├── README.md                                    # This file
├── master_spec.md                               # Specification document
├── marketing_brochure_fatigue_analysis.md       # Markdown source
├── marketing_brochure_fatigue_analysis.pdf      # PDF output
├── marketing_brochure_stress_analysis.md
├── marketing_brochure_stress_analysis.pdf
├── marketing_brochure_marine_analysis.md
├── marketing_brochure_marine_analysis.pdf
├── marketing_brochure_pipeline_analysis.md
├── marketing_brochure_pipeline_analysis.pdf
├── marketing_brochure_orcaflex_integration.md
├── marketing_brochure_orcaflex_integration.pdf
└── ... (additional brochures)
```

## Distribution

These brochures are intended for:

- **Sales & Marketing** - Client presentations and proposals
- **Engineering Teams** - Internal capability overviews
- **Training** - New team member onboarding
- **Documentation** - Module capability summaries
- **Website** - Public marketing materials

## Version Control

- Brochures are stored in Git for version tracking
- PDF files are committed alongside markdown sources
- Update version numbers in footer when content changes significantly

## Contact

For questions or updates to marketing materials:

- Email: vamsee.achanta@aceengineer.com
- GitHub: https://github.com/vamseeachanta/digitalmodel/issues

---

*Digital Model Project - Professional Engineering Software*
*© 2025 - MIT License*
