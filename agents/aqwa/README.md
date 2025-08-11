# ANSYS AQWA Module Agent v3.0

## Overview
Specialized AI agent for ANSYS AQWA hydrodynamic analysis of offshore structures, floating systems, and marine operations. This agent provides expert assistance with AQWA modeling, analysis automation, and results interpretation for offshore engineering applications.

## Core Capabilities

### Analysis Types
- **AQWA-LINE**: Frequency domain diffraction/radiation analysis
- **AQWA-DRIFT**: Time domain analysis with slow drift
- **AQWA-LIBRIUM**: Static and dynamic stability assessment
- **AQWA-NAUT**: Cable and line dynamics
- **AQWA-WAVE**: Coupled analysis with structural responses

### Hydrodynamic Features
- **Wave Forces**: First-order and second-order wave loads
- **Drift Forces**: Mean drift, slow drift, and wave drift damping
- **Multi-Body**: Hydrodynamic interaction between multiple vessels
- **Mooring Systems**: Catenary, taut, and dynamic mooring lines
- **RAOs**: Response Amplitude Operators for all 6 DOF
- **Added Mass & Damping**: Frequency-dependent coefficients

## Supported Standards
- **DNV-RP-C205**: Environmental conditions and loads
- **API RP 2SK**: Design and analysis of station keeping systems
- **ISO 19901-7**: Offshore structures - Station keeping
- **IEC 61400-3**: Wind turbines - Offshore requirements

## Web Resources

### Official Documentation
- [AQWA Theory Manual](https://ansyshelp.ansys.com) - Complete theoretical background
- [AQWA Reference Manual](https://ansyshelp.ansys.com) - Commands and workflow guide
- [AQWA Product Overview](https://www.ansys.com/products/structures/ansys-aqwa) - Features and capabilities
- [AQWA Training](https://www.ansys.com/training-center) - Official training materials

### Standards & Guidelines
- [DNV-RP-C205](https://www.dnv.com) - Environmental loads specification
- [API RP 2SK](https://www.api.org) - Station keeping best practices

## Usage Examples

### Basic Diffraction Analysis
```
"Set up AQWA-LINE analysis for a semi-submersible in 200m water depth with wave periods from 5-25 seconds"
```

### Mooring Analysis
```
"Configure 12-line spread mooring system for FPSO with chain-polyester-chain configuration in AQWA-DRIFT"
```

### Multi-Body Setup
```
"Model side-by-side offloading between FPSO and shuttle tanker with fenders and hawsers"
```

### Python Scripting
```python
# Example AQWA automation
import ansys.aqwa.core as aqwa

# Create model
model = aqwa.Model()
vessel = model.create_vessel("FPSO")
vessel.set_mesh_from_file("fpso_hull.dat")

# Define environment
env = model.create_environment()
env.water_depth = 1500.0
env.add_wave_spectrum("JONSWAP", Hs=8.0, Tp=12.0)

# Run analysis
results = model.solve_diffraction()
```

## V3.0 Agent Features

### Phased Document Processing
- **Phase 1: Discovery** - Document inventory and classification
- **Phase 2: Quality Assessment** - Quality scoring and prioritization
- **Phase 3: Extraction** - Knowledge extraction with source tracking
- **Phase 4: Synthesis** - Conflict resolution and consolidation
- **Phase 5: Validation** - Consistency checks and quality assurance
- **Phase 6: Integration** - Agent knowledge integration

### Modular Management
- **Specialization**: ANSYS AQWA Offshore Engineering
- **Context Optimization**: 16000 tokens focused on hydrodynamics
- **Cross-References**: OrcaFlex, ANSYS Mechanical
- **Auto-Refresh**: Enabled with high priority

### Context Engineering (v2.0)
- **Layered Architecture**: Domain, operational, episodic, semantic
- **Memory Management**: Optimized for analysis workflows
- **RAG Optimization**: Advanced chunking for technical documentation
- **Duplicate Detection**: SHA256-based content hashing

## Best Practices

### Mesh Generation
- Panel size: λ/7 to λ/5 (wavelength dependent)
- Symmetry exploitation for computational efficiency
- Refinement near waterline and corners

### Frequency Selection
- Minimum 40 frequencies for accurate QTF
- Focus on wave energy spectrum range
- Include natural periods of structure

### Convergence Checks
- Mesh sensitivity analysis
- Time step verification for time domain
- Damping linearization validation

## Structure
```
aqwa/
├── agent.yaml                 # Enhanced configuration
├── README.md                  # This file
├── context/                   # Context management
│   ├── external/             
│   │   └── web/              # Web resources
│   │       ├── web_resources.yaml
│   │       └── cache/        # Cached documentation
│   ├── repository/           # Local docs
│   └── optimized/            # Processed knowledge
├── processing/               # Phased processing
├── templates/                # AQWA templates
└── tools/                    # Automation scripts
```

## Resource Management

### Add New Resources
```bash
python tools/manage-agent-resources.py add-link aqwa --url "URL" --notes "Description"
```

### Review Resources
```bash
python tools/manage-agent-resources.py review aqwa
```

### Refresh Documentation
```bash
python tools/manage-agent-resources.py refresh aqwa
```

## Common Applications

### Floating Production Systems
- FPSO motion analysis
- Turret mooring design
- Offloading operations

### Renewable Energy
- Floating wind turbine platforms
- Wave energy converters
- Tidal energy systems

### Marine Operations
- Float-over installations
- Heavy lift operations
- Transportation analysis

## Integration with Other Tools
- **OrcaFlex**: Comparison studies and validation
- **ANSYS Mechanical**: Structural coupling
- **ANSYS Workbench**: Parametric optimization

## Version History
- **v3.0.0** (2025-08-10): Full v3.0 implementation with phased processing
- **v2.0.0** (2025-08-10): Enhanced with domain expertise and web resources
- **v1.0.0** (2025-08-10): Initial creation

## Support
For AQWA-specific queries or issues with this agent, use the standard Agent OS feedback channels or consult the ANSYS support portal.

---
*This agent is optimized for ANSYS AQWA 2024 R2 and later versions*