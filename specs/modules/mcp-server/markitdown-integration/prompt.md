# MarkItDown MCP Integration - Prompt Documentation

## Original User Request

**Date**: 2025-09-19
**Request**: "add this mcp to the repo mcps. we will use these as part of all repo tasks: https://github.com/microsoft/markitdown"

**Follow-up**: "does this mcp support scanned PDFs"

## Context

The user requested integration of Microsoft's MarkItDown tool as an MCP (Model Context Protocol) server to enhance the repository's document processing capabilities. The tool converts various document formats to Markdown, making them accessible to Large Language Models (LLMs) for analysis and processing.

## Implementation Approach

### 1. Research Phase
- Investigated MarkItDown capabilities via GitHub repository
- Confirmed support for multiple document formats including PDFs, Word, PowerPoint, Excel
- Verified OCR support for scanned documents
- Identified LLM integration possibilities

### 2. Architecture Design
- Followed established MCP server patterns in the repository
- Created parallel structure to existing OrcaWave MCP
- Assigned port 3106 following sequential allocation
- Designed modular architecture for extensibility

### 3. Implementation Steps
1. Created MCP registry entry with documentation
2. Implemented core converter wrapper
3. Designed comprehensive configuration schema
4. Added to dependency management
5. Created test structure
6. Generated complete specification

### 4. Key Design Decisions
- **Port Assignment**: 3106 (next available in sequence)
- **Structure**: Followed repository's module-based organization
- **Configuration**: YAML-based with extensive options
- **Integration**: Designed for agent collaboration
- **Testing**: Comprehensive test strategy included

## Key Features Implemented

### Core Functionality
- Document to Markdown conversion
- Batch processing support
- OCR for scanned documents
- Table extraction
- Image description with LLM

### MCP Protocol Support
- Resources: capabilities, history, config
- Tools: convert_document, batch_convert, extract_tables, describe_image
- FastMCP framework ready (future integration)

### Configuration Options
- File type specific settings
- LLM integration support
- Azure Document Intelligence hooks
- Security restrictions
- Caching capabilities

## Technical Specifications

### Supported Formats
- Documents: PDF, Word (.docx), PowerPoint (.pptx), Excel (.xlsx)
- Images: PNG, JPG, JPEG, GIF (with OCR)
- Web: HTML to Markdown
- Data: CSV, JSON, XML
- Audio: Transcription support

### Performance Targets
- Single document: &lt; 5 seconds
- Batch processing: 10 parallel documents
- Memory usage: &lt; 500MB typical
- Large files: Streaming support

## Integration Points

### Agent Collaboration
```yaml
agents:
  primary: Documentation Agent
  supporting:
    - OrcaFlex Agent (technical reports)
    - Testing Agent (validation)
    - Infrastructure Agent (deployment)
```

### Workflow Integration
- OrcaFlex report processing
- Documentation pipeline
- Knowledge base creation
- Cross-format analysis

## Implementation Status

### Completed ✅
- MCP registry entry
- Core implementation structure
- Configuration management
- Dependency integration
- Basic test framework
- Complete specification

### Pending 🔄
- FastMCP framework integration
- Full MCP protocol implementation
- Complete test suite
- Production deployment
- Performance optimization

## Curated Reuse Prompt

To extend or modify the MarkItDown MCP integration:

```
I need to work with the MarkItDown MCP server in the DigitalModel repository. The server is located at:
- Registry: specs/modules/mcp-server/mcp/markitdown/
- Implementation: src/mcp/markitdown/
- Port: 3106

Current capabilities:
- Converts documents (PDF, Word, Excel, PowerPoint) to Markdown
- OCR support for scanned documents
- Batch processing
- Table extraction
- Optional LLM integration for image description

Please help me [specific task]:
- Add a new conversion format
- Enhance OCR accuracy with Azure
- Integrate with [specific agent/workflow]
- Optimize performance for large files
- Add caching layer
- Implement authentication

The implementation follows MCP protocol standards and integrates with the existing agent ecosystem.
```

## Lessons Learned

### What Worked Well
1. Following established MCP patterns accelerated development
2. Modular architecture enables easy extension
3. Comprehensive configuration covers diverse use cases
4. Clear separation between core and optional features

### Challenges Encountered
1. Python version compatibility with full feature set
2. Balancing feature richness with simplicity
3. Designing for both standalone and integrated use

### Best Practices Applied
1. Repository-wide consistency in structure
2. Comprehensive documentation from the start
3. Test-driven development approach
4. Security-first configuration design
5. Performance targets defined upfront

## Future Enhancements

### Short-term
- Complete FastMCP integration
- Full test coverage
- LLM provider abstraction
- Prometheus metrics

### Medium-term
- Azure Document Intelligence integration
- Plugin marketplace
- Distributed processing
- Real-time conversion API

### Long-term
- ML-based layout understanding
- Custom training for domain-specific documents
- Multi-language support
- Semantic extraction pipelines

## References

### External Resources
- [MarkItDown Repository](https://github.com/microsoft/markitdown)
- [MCP Protocol Specification](https://modelcontextprotocol.io/)
- [FastMCP Framework](https://github.com/jlowin/fastmcp)

### Internal Documentation
- MCP Registry: `specs/modules/mcp-server/mcp/README.md`
- OrcaWave MCP: `specs/modules/mcp-server/mcp/orcawave/`
- Agent Integration: `agents/README.md`

## Command Summary

### Setup
```bash
# Install dependencies
uv add markitdown

# Start server
python specs/modules/mcp-server/mcp/markitdown/run_server.py
```

### Usage
```bash
# Connect with Claude CLI
claude --mcp-server localhost:3106

# Convert documents
> Convert report.pdf to Markdown
> Extract tables from data.xlsx
> Process all documents in /specs folder
```

### Testing
```bash
# Run tests
uv run pytest src/mcp/markitdown/tests/

# With coverage
uv run pytest --cov=src.mcp.markitdown
```