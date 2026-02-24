# Project Context for AI Assistants

## Overview
DigitalModel is a Python library for engineering asset digital models focused on lifecycle analysis in offshore/marine engineering. It provides a single ASCII data source of truth to generate finite element models, analytical calculations, 3D CAD models, animations, and drawings.

## Architecture Principles
- **Configuration-Driven**: YAML files drive most analysis workflows
- **Vertical Slice Architecture**: Features organized by engineering domain
- **Single Source of Truth**: ASCII inputs generate multiple output types
- **Modularity**: Engineering assets imported via .yml files
- **Reusability**: Components designed for cross-project use

## Key Engineering Domains
- Offshore structures (risers, moorings, umbilicals)
- Marine vessel design and analysis
- Pipeline engineering and pressure calculations
- Fatigue and structural analysis
- Hydrodynamic analysis and wave loading
- Installation analysis and rigging

## Development Workflow
1. **Specification First**: All features start with specs in `specs/` directory
2. **Configuration Templates**: Create/modify YAML configs in `base_configs/modules/`
3. **Module Implementation**: Implement analysis logic in `modules/`
4. **Testing**: Create comprehensive tests with mock APIs where needed
5. **Documentation**: Update relevant docs in `docs/` directory

## Special Considerations
- **OrcaFlex Integration**: Requires license; use mock API for testing
- **Domain Expertise**: Code contains deep offshore engineering knowledge
- **YAML Configs**: Primary interface for most functionality
- **Result Formats**: Typically CSV, Excel, or visualization formats

## AI Assistant Guidelines
- Always check specifications before implementing features
- Follow existing code patterns and naming conventions
- Respect configuration-driven architecture
- Maintain compatibility with existing YAML templates
- Add appropriate tests with mock APIs for licensed software
- Update documentation to match code changes