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

## Project Structure

The codebase is organized into:
- `src/digitalmodel/`: Core source code and modules
- `tests/`: Test suites for various modules
- `docs/`: Technical documentation and references
- `.ai/` & `.agent-os/`: AI agent guidance and specifications