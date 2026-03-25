# OrcaWave Module Agent

## Overview
Specialized AI agent for OrcaWave diffraction/radiation analysis with deep expertise in marine hydrodynamics and panel method computations.

## Core Capabilities
- **Diffraction Analysis**: Wave-structure interaction modeling
- **Radiation Analysis**: Added mass and damping calculations
- **Multi-body Interactions**: Coupled body dynamics
- **Mesh Generation**: Panel mesh optimization and convergence
- **QTF Calculations**: Second-order wave force computations
- **Batch Processing**: Parallel execution of multiple analyses
- **OrcaFlex Integration**: Hydrodynamic database generation

## Domain Expertise
- Marine hydrodynamics and offshore engineering
- Panel method theory and implementation
- Wave mechanics and frequency domain analysis
- Mesh convergence and quality assessment
- API, DNV, ABS regulatory compliance

## Technical Skills
- OrcaWave COM API programming
- Python automation and scripting
- Parallel processing optimization
- Result validation and benchmarking
- Integration with OrcaFlex workflows

## Usage
```bash
python run_orcawave_agent.py --task "diffraction-analysis" --config analysis.yml
```

## Agent Structure
```
orcawave/
├── README.md                 # This file
├── agent_config.json        # Agent configuration
├── capabilities.yml         # Detailed capabilities
├── run_orcawave_agent.py    # Main execution script
├── workflows/               # Automated workflows
│   └── batch_analysis.yml   # Batch processing workflow
└── prompts/                 # Specialized prompts
```

## Integration Points
- **OrcaFlex**: Hydrodynamic database export and vessel creation
- **AQWA**: Benchmark validation and result comparison
- **Excel**: Automated reporting and visualization
- **CAD Systems**: Mesh import and geometry handling

## Performance Targets
- Analysis Time: 10-60 seconds per configuration
- Daily Throughput: 50-100 configurations
- Accuracy: 5% tolerance vs industry benchmarks
- Success Rate: 99%+