# FreeCAD Agent

## Overview

The FreeCAD Agent is an AI-powered automation tool for FreeCAD that enables natural language CAD operations, intelligent batch processing, and seamless integration with engineering analysis workflows.

## Features

- **Natural Language Processing**: Convert plain English commands to FreeCAD operations
- **Batch Processing**: Process multiple CAD files in parallel with pattern matching
- **Python Script Generation**: Automatically generate Python scripts from prompts
- **Marine Engineering Tools**: Specialized functions for offshore and marine applications
- **API Integration**: RESTful API and WebSocket support for external systems
- **FEM Preprocessing**: Automated mesh generation and boundary condition application

## Installation

### Prerequisites
- Python 3.8 or higher
- FreeCAD 1.0 or higher (November 2024 release)
- Git for version control

### Setup
```bash
# Clone the repository
git clone https://github.com/your-org/digitalmodel.git
cd digitalmodel/agents/freecad

# Create virtual environment
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt
```

## Usage

### Command Line Interface
```bash
# Basic usage
python run_freecad_agent.py --help

# Show agent capabilities
python run_freecad_agent.py --show-capabilities

# Process single file
python run_freecad_agent.py --file model.FCStd --operation "add fillet radius 5mm"

# Batch processing
python run_freecad_agent.py --pattern "*.FCStd" --input-directory ./models --parallel 4

# Natural language command
python run_freecad_agent.py --prompt "Create a hull with 150m length and 25m beam"
```

### Python API
```python
from freecad_agent import FreeCADAgent

# Initialize agent
agent = FreeCADAgent()

# Natural language operation
result = agent.execute_prompt("Create a box 100x50x25mm with chamfered edges")

# Batch processing
results = agent.batch_process(
    pattern="*.FCStd",
    input_directory="./models",
    operation="export_step",
    parallel_workers=4
)

# Script generation
script = agent.generate_script("Create parametric gear with 20 teeth")
```

### Configuration
Configuration is managed through `agent_config.json`:
```json
{
  "name": "FreeCAD Agent",
  "version": "1.0.0",
  "capabilities": [
    "cad_automation",
    "batch_processing",
    "parametric_design",
    "assembly_management",
    "fem_preprocessing",
    "drawing_generation"
  ],
  "settings": {
    "parallel_workers": 4,
    "cache_enabled": true,
    "auto_save": true,
    "validation_level": "strict"
  }
}
```

## Capabilities

### CAD Operations
- Parametric modeling and design tables
- Assembly creation and constraint solving
- Drawing generation with automatic dimensioning
- Feature-based modeling and modification

### Batch Processing
- Pattern-based file discovery (glob, regex)
- Parallel execution with adaptive worker scaling
- Progress tracking and error recovery
- Result aggregation and reporting

### Marine Engineering
- Hull design automation
- Stability calculations
- Mooring system configuration
- Structural analysis preprocessing

### Integration
- OrcaFlex data exchange
- AQWA diffraction analysis
- Signal analysis module connectivity
- REST API for external systems

## Architecture

```
freecad_agent/
├── src/
│   ├── core/
│   │   ├── agent.py           # Base agent implementation
│   │   ├── capabilities.py    # Capability registry
│   │   └── config.py          # Configuration management
│   ├── api/
│   │   ├── wrapper.py         # FreeCAD API wrapper
│   │   ├── document.py        # Document management
│   │   └── geometry.py        # Geometry operations
│   ├── nlp/
│   │   ├── parser.py          # Natural language parser
│   │   ├── generator.py       # Script generator
│   │   └── templates.py       # Operation templates
│   ├── batch/
│   │   ├── processor.py       # Batch processing engine
│   │   ├── discovery.py       # File discovery
│   │   └── parallel.py        # Parallel execution
│   └── marine/
│       ├── hull.py            # Hull design tools
│       ├── stability.py       # Stability calculations
│       └── mooring.py         # Mooring systems
├── tests/
│   ├── unit/                  # Unit tests
│   ├── integration/           # Integration tests
│   └── fixtures/              # Test fixtures
├── config/
│   ├── agent_config.json      # Agent configuration
│   └── workflows.yml          # Workflow definitions
└── docs/
    ├── user_guide.md          # User documentation
    ├── api_reference.md       # API documentation
    └── examples/              # Example scripts
```

## Development

### Running Tests
```bash
# Run all tests
pytest

# Run specific test module
pytest tests/unit/test_api_wrapper.py

# Run with coverage
pytest --cov=src --cov-report=html
```

### Contributing
1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass
6. Submit a pull request

## Performance

- **Batch Processing**: Up to 5x faster than sequential processing
- **Memory Optimization**: Efficient handling of large assemblies
- **Caching**: Intelligent caching of frequently used operations
- **Error Recovery**: Automatic retry with exponential backoff

## Troubleshooting

### Common Issues

1. **FreeCAD Import Error**
   - Ensure FreeCAD is installed and Python bindings are available
   - Add FreeCAD to Python path: `sys.path.append('/path/to/FreeCAD/lib')`

2. **License Error**
   - Some operations require FreeCAD GUI license
   - Use headless mode for server deployments

3. **Memory Issues**
   - Reduce parallel workers for large files
   - Enable garbage collection between operations

## Support

- **Documentation**: See `docs/` directory
- **Issues**: Report bugs on GitHub Issues
- **Questions**: Post in Discussions section
- **Email**: support@your-org.com

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- FreeCAD development team for the excellent Python API
- Community contributors for addons and macros
- OpenCASCADE for the geometry kernel

## Roadmap

### Version 1.1
- Machine learning integration for design optimization
- Cloud processing capabilities
- Real-time collaboration features

### Version 2.0
- AR/VR visualization support
- Cross-platform CAD support
- Industry-specific templates

## Related Projects

- [OrcaFlex Agent](../orcaflex/README.md) - Hydrodynamic analysis automation
- [AQWA Agent](../aqwa/README.md) - Diffraction analysis tools
- [DigitalModel](../../README.md) - Parent ecosystem

---

*For detailed technical documentation, see the [specification](../../specs/modules/freecad/freecad-agent/spec.md)*