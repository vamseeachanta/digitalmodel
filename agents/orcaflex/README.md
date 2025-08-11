# OrcaFlex Module Agent v2.0

## Overview
Specialized AI agent for OrcaFlex hydrodynamic analysis and offshore engineering simulations. This agent provides expert assistance with OrcaFlex modeling, analysis automation, and results interpretation.

## Capabilities

### Domain Expertise
- **Hydrodynamic Analysis**: Wave loads, vessel motions, current forces
- **Mooring System Design**: Line configuration, anchor patterns, tension analysis
- **Riser Analysis**: Static and dynamic riser behavior, interference checks
- **Installation Analysis**: Lifting operations, pipelay, cable lay simulations
- **Fatigue Assessment**: Rainflow counting, S-N curves, damage accumulation

### Supported Standards
- DNV-ST-F201 (Dynamic Risers)
- API RP 2SK (Station Keeping)
- ISO 19901-7 (Offshore Structures)

### Analysis Tools
- **OrcaFlex**: Primary analysis engine
- **OrcaWave**: Wave diffraction analysis
- **Python API**: Automation and batch processing

## Web Resources

### Official Documentation
- [OrcaFlex Documentation](https://www.orcina.com/webhelp/OrcaFlex/) - Complete user manual and theory
- [Python API Reference](https://www.orcina.com/SoftwareProducts/OrcaFlex/Documentation/Help/htm/index.htm) - Automation guide
- [Example Models](https://www.orcina.com/resources/examples/) - Industry case studies

### Standards & Guidelines
- [DNV Rules & Standards](https://www.dnv.com/rules-standards/) - Offshore certification requirements
- [API Standards](https://www.api.org/products-and-services/standards) - Industry best practices

### Technical Resources
- [Technical Papers](https://www.orcina.com/resources/papers/) - Validation studies and theory

## Usage Examples

### Basic Analysis Query
```
"Set up an OrcaFlex model for a turret-moored FPSO in 1500m water depth with 12 mooring lines"
```

### Python Automation
```python
import OrcFxAPI

model = OrcFxAPI.Model()
vessel = model.CreateObject(OrcFxAPI.otVessel, "FPSO")
# Agent will provide complete setup code
```

### Standards Compliance
```
"Verify the mooring system design against DNV-ST-F201 requirements for a 100-year storm"
```

## Workflows

### Enhanced Specifications
- Auto-generates detailed analysis specifications
- Includes load cases, environmental conditions, and acceptance criteria
- Learning-enabled for continuous improvement

### Analysis Automation
- Batch processing for multiple load cases
- Automated result extraction and reporting
- Parametric studies and optimization

## Internal Resources
- `src/modules/hydrodynamics/` - Hydrodynamic calculation modules
- `specs/modules/marine-engineering/` - Engineering specifications

## Resource Management

To add new resources:
```bash
python tools/manage-agent-resources.py add-link orcaflex --url "URL" --notes "Description"
```

To review all resources:
```bash
python tools/manage-agent-resources.py review orcaflex
```

## Version History
- **v2.0.0** (2025-08-10): Enhanced with web resources, specialized workflows, and standards compliance
- **v1.0.0** (2025-08-05): Initial creation

## Contact
For OrcaFlex-specific queries or to report issues with this agent, use the standard Agent OS feedback channels.