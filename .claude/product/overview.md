# DigitalModel Product Overview

## What is DigitalModel?

DigitalModel is a comprehensive Python-based engineering analysis platform focused on offshore and marine engineering applications. It provides tools and modules for various engineering analyses including:

- **OrcaFlex Integration**: Advanced post-processing and analysis for OrcaFlex simulations
- **AQWA Integration**: Hydrodynamic analysis and processing
- **Structural Analysis**: FEA modeling, fatigue analysis, and capacity calculations
- **Marine Engineering**: Ship design, mooring analysis, and installation planning
- **Subsea Systems**: Pipeline analysis, riser design, and cathodic protection
- **Utilities**: Time series analysis, visualization, and data management

## Key Features

1. **Modular Architecture**: Organized into specialized modules for different engineering domains
2. **Multiple CAE Tool Integration**: Interfaces with OrcaFlex, AQWA, ANSYS, and other engineering software
3. **Automated Post-Processing**: Streamlines data extraction and analysis from simulation results
4. **Standards Compliance**: Implements various industry standards (API, DNV, ABS)
5. **Parallel Processing**: Recently added support for concurrent file processing in OrcaFlex module

## Target Users

- Offshore engineers
- Naval architects
- Subsea engineers
- Marine structural analysts
- Engineering consultants

## Core Capabilities

### Analysis Types
- Hydrodynamic analysis
- Structural fatigue assessment
- Installation analysis
- Mooring system design
- Pipeline and riser analysis
- Cathodic protection design

### Data Processing
- Time series analysis
- Statistical post-processing
- Visualization generation
- Report automation

## Technology Stack

- **Language**: Python 3.x
- **Package Management**: Poetry, pip, uv
- **Testing**: pytest
- **Version Control**: Git
- **Documentation**: Markdown, YAML configurations

## Development Workflow

### Specification-First Development
All features follow this structured approach:
1. **Specification First**: Features start with specs in `.agent-os/projects/` or `.ai/specs/`
2. **Configuration Templates**: Create/modify YAML configs in `base_configs/domains/`
3. **Module Implementation**: Implement analysis logic in `modules/`
4. **Testing**: Create comprehensive tests with mock APIs where needed
5. **Documentation**: Update relevant docs in `docs/` directory

### Configuration-Driven Architecture
- **YAML files** drive most analysis workflows
- **Single source of truth**: ASCII inputs generate multiple output types
- **Modularity**: Engineering assets imported via .yml files
- **Reusability**: Components designed for cross-project use

### Special Considerations

#### OrcaFlex Integration
- Requires commercial license for full functionality
- Use mock API patterns for testing without license
- Graceful fallback mechanisms when license unavailable
- Comprehensive error handling for license issues

#### Domain Expertise Requirements
- Code contains deep offshore engineering knowledge
- Follow API, DNV, ABS industry standards
- Maintain traceability to engineering specifications
- Include references to relevant codes in comments

#### YAML Configuration Primacy
- Primary interface for most functionality
- Extensive configuration validation required
- Template configs for common analysis patterns
- User configs extend base configuration templates

#### Result Format Standards
- Typically CSV, Excel formats for data exchange
- PNG/PDF for visualization outputs
- HTML reports for comprehensive results
- Pandas DataFrames for internal processing

## Project Structure

The codebase is organized into:
- `src/digitalmodel/`: Core source code and modules
- `tests/`: Test suites for various modules
- `docs/`: Technical documentation and references
- `.ai/` & `.agent-os/`: AI agent guidance and specifications
- `base_configs/domains/`: YAML configuration templates

### Engineering Domain Organization
- `aqwa/` - ANSYS AQWA hydrodynamic analysis
- `orcaflex/` - OrcaFlex simulation and post-processing
- `catenary/` - Catenary riser analysis
- `ship_design/` - Vessel design and analysis
- `pipe_capacity/` - Pipeline capacity calculations
- `mooring/` - Mooring system analysis