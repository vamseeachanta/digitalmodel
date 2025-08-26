# MCP Servers Registry

Model Context Protocol (MCP) servers enable Claude CLI to interact with various applications and services through standardized interfaces.

## Available MCP Servers

### OrcaWave
- **Purpose**: Hydrodynamic diffraction analysis for offshore structures
- **Location**: `mcp/orcawave/`
- **Port**: 3100
- **Status**: Production Ready
- **Documentation**: [OrcaWave MCP](./orcawave/README.md)

### Future MCP Servers (Planned)
- **OrcaFlex**: Time-domain analysis and mooring systems
- **AQWA**: Hydrodynamic analysis validation
- **FreeCAD**: Parametric CAD modeling
- **GMsh**: Mesh generation and optimization
- **Excel**: Automated reporting and data analysis

## Repository Structure

```
mcp/                         # MCP server registry (similar to agents/)
├── README.md               # This file - registry of all MCP servers
├── orcawave/              # OrcaWave MCP server
│   ├── README.md          # Server documentation
│   ├── config.yml         # Default configuration
│   └── run_server.py      # Server entry point
└── [future servers]/      # Additional MCP servers

src/mcp/                    # MCP server implementations
├── __init__.py
├── orcawave/              # OrcaWave implementation
│   ├── api/              # COM/API wrappers
│   ├── core/             # MCP server core
│   ├── config/           # Configuration files
│   └── tests/            # Test suites
└── [future servers]/      # Additional implementations
```

## Usage Pattern

### Starting an MCP Server

```bash
# From repository root
python mcp/[server-name]/run_server.py

# Example for OrcaWave
python mcp/orcawave/run_server.py
```

### Connecting with Claude CLI

```bash
# Connect to MCP server
claude --mcp-server localhost:[port]

# Example for OrcaWave
claude --mcp-server localhost:3100
```

## Development Guidelines

### Adding a New MCP Server

1. **Create registry entry**: `mcp/[server-name]/`
   - README.md (documentation)
   - config.yml (default configuration)
   - run_server.py (entry point)

2. **Implement server**: `src/mcp/[server-name]/`
   - api/ (application-specific APIs)
   - core/ (MCP server implementation)
   - config/ (configuration management)
   - tests/ (test suite)

3. **Follow patterns**:
   - Use FastMCP framework
   - Implement standard MCP resources and tools
   - Include comprehensive error handling
   - Add structured logging
   - Create test coverage

### MCP Server Standards

All MCP servers should:
- Use consistent port numbering (31xx series)
- Implement health check endpoints
- Support configuration via YAML
- Include comprehensive documentation
- Provide test suites
- Follow security best practices

## Port Allocation

| Server | Port | Status |
|--------|------|--------|
| OrcaWave | 3100 | Active |
| OrcaFlex | 3101 | Planned |
| AQWA | 3102 | Planned |
| FreeCAD | 3103 | Planned |
| GMsh | 3104 | Planned |
| Excel | 3105 | Planned |

## Integration with Agents

MCP servers work in conjunction with domain agents:
- Agents provide domain expertise and workflow orchestration
- MCP servers provide application control and data access
- Together they enable comprehensive automation

## References

- [MCP Protocol Specification](https://modelcontextprotocol.io/)
- [FastMCP Documentation](https://github.com/jlowin/fastmcp)
- [Generic GUI Template](../specs/modules/mcp-server/generic-gui-template/)