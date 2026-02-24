# OrcaWave MCP Server

## Overview
MCP (Model Context Protocol) server for OrcaWave hydrodynamic analysis, enabling Claude CLI to interact with OrcaWave through AI-assisted workflows.

## Core Capabilities
- **COM API Control**: Programmatic control of OrcaWave via COM interface
- **Vessel Management**: Create and configure vessel models
- **Geometry Import**: Import CAD files (STL, GDF, etc.)
- **Mesh Optimization**: AI-driven mesh quality improvement
- **Analysis Execution**: Run diffraction/radiation analyses
- **Results Export**: Direct export to OrcaFlex vessel types
- **Batch Processing**: Parallel analysis of multiple vessels

## Technical Features
- Hybrid control (COM API + Vision monitoring)
- Real-time progress tracking
- Physics-based validation
- Multi-tier caching system
- Structured logging with correlation IDs

## Usage

### Starting the Server
```bash
# From repository root
python -m src.mcp.orcawave.core.mcp_server --config mcp/orcawave/config.yml

# Or using the run script
python mcp/orcawave/run_server.py
```

### Connecting with Claude CLI
```bash
claude --mcp-server localhost:3100

# Example workflow
> Connect to OrcaWave
> Create vessel "FPSO" type FPSO length 300 beam 60 draft 20
> Import geometry from "path/to/model.stl"
> Optimize mesh for quality > 0.85
> Run diffraction analysis
> Export to OrcaFlex
```

## MCP Tools Available
- `connect` - Connect to OrcaWave application
- `create_vessel` - Create new vessel with specifications
- `import_geometry` - Import CAD geometry
- `optimize_mesh` - Optimize mesh quality
- `setup_analysis` - Configure analysis parameters
- `run_analysis` - Execute hydrodynamic analysis
- `export_to_orcaflex` - Export results as vessel type

## MCP Resources
- `orcawave://model/current` - Current model state
- `orcawave://mesh/quality` - Mesh quality metrics
- `orcawave://results/hydrodynamics` - Analysis results
- `orcawave://validation/status` - Validation status

## Configuration
See `config.yml` for configuration options including:
- OrcaWave installation path
- Analysis parameters
- Mesh optimization settings
- Performance tuning

## Implementation Location
- **Source Code**: `src/mcp/orcawave/`
- **Configuration**: `mcp/orcawave/config.yml`
- **Documentation**: `mcp/orcawave/README.md`

## Performance Targets
- Analysis setup: < 2 minutes
- Mesh optimization: < 5 minutes
- Single analysis: 10-60 seconds
- Batch processing: 3x speedup with parallel execution

## Integration Points
- **OrcaFlex**: Direct vessel type export
- **Excel**: Automated reporting
- **AQWA**: Benchmark validation
- **CAD Systems**: Geometry import

## Agent Collaboration
Works with:
- OrcaWave Agent (domain expertise)
- CAD Agent (geometry preparation)
- Testing Agent (validation)
- OrcaFlex Agent (downstream integration)