# MarkItDown MCP Server

## Overview

MarkItDown MCP server provides document conversion capabilities to transform various file formats into Markdown, optimized for Large Language Model (LLM) processing and text analysis pipelines.

## Purpose

- **Document Conversion**: Convert PDF, PowerPoint, Word, Excel, images, audio, HTML, and more to Markdown
- **LLM Integration**: Provides Markdown output that is natively understood by LLMs like GPT-4o and Claude
- **Structure Preservation**: Maintains document structure and formatting in Markdown format
- **Multi-format Support**: Handles diverse file types through a unified interface

## Features

### Supported File Types
- **Documents**: PDF, Word (.docx), PowerPoint (.pptx), Excel (.xlsx)
- **Images**: PNG, JPG, JPEG with optional OCR and LLM-based description
- **Audio**: Audio transcription to text
- **Web**: HTML to Markdown conversion
- **Code**: Various code files with syntax highlighting preservation
- **Data**: CSV, JSON, XML with structured formatting

### Core Capabilities
- Command-line interface for batch processing
- Python API for programmatic access
- Plugin architecture for extensibility
- Azure Document Intelligence integration
- LLM-powered image description
- Table preservation from Excel and other formats

## Configuration

Default configuration in `config.yml`:

```yaml
server:
  name: markitdown
  port: 3106
  host: localhost

markitdown:
  # Optional LLM configuration for image description
  llm_model: null  # e.g., "gpt-4o" or "claude-3"
  llm_api_key: null
  
  # Azure Document Intelligence (optional)
  azure_endpoint: null
  azure_key: null
  
  # Processing options
  batch_size: 10
  timeout: 300  # seconds
  preserve_formatting: true
  include_metadata: true
  
  # Output options
  output_format: markdown
  code_syntax_highlighting: true
  table_format: github  # github, simple, grid
```

## MCP Resources

### Available Resources

1. **Conversion Capabilities**
   - Resource: `markitdown://capabilities`
   - Returns list of supported file types and conversion options

2. **Conversion History**
   - Resource: `markitdown://history`
   - Returns recent conversion operations

3. **Configuration**
   - Resource: `markitdown://config`
   - Returns current configuration settings

## MCP Tools

### convert_document
Convert a document to Markdown format.

```json
{
  "name": "convert_document",
  "parameters": {
    "file_path": "path/to/document.pdf",
    "output_path": "path/to/output.md",
    "options": {
      "include_images": true,
      "describe_images": false,
      "preserve_tables": true
    }
  }
}
```

### batch_convert
Convert multiple documents in a directory.

```json
{
  "name": "batch_convert",
  "parameters": {
    "input_directory": "path/to/documents/",
    "output_directory": "path/to/markdown/",
    "pattern": "*.pdf",
    "recursive": true
  }
}
```

### extract_text
Extract text content without full conversion.

```json
{
  "name": "extract_text",
  "parameters": {
    "file_path": "path/to/document.docx",
    "format": "plain"  // plain, markdown, json
  }
}
```

## Installation

### Prerequisites
```bash
# Install MarkItDown with all optional dependencies
uv add 'markitdown[all]'

# Or minimal installation
uv add markitdown
```

### Starting the Server

```bash
# From repository root
python specs/modules/mcp-server/mcp/markitdown/run_server.py

# With custom configuration
python specs/modules/mcp-server/mcp/markitdown/run_server.py --config custom_config.yml
```

## Usage Examples

### With Claude CLI

```bash
# Connect to MarkItDown MCP server
claude --mcp-server localhost:3106

# In Claude conversation
> Convert the PDF report to Markdown
> Extract tables from the Excel file
> Convert all PowerPoint presentations in the folder
```

### Python Integration

```python
from markitdown import MarkItDown

# Initialize converter
md = MarkItDown()

# Convert single file
result = md.convert("document.pdf")
print(result.text_content)

# With options
result = md.convert(
    "presentation.pptx",
    llm_model="gpt-4o",
    llm_api_key="your-key"
)

# Batch conversion
for file_path in Path("documents").glob("*.docx"):
    result = md.convert(str(file_path))
    output_path = file_path.with_suffix(".md")
    output_path.write_text(result.text_content)
```

## Integration with DigitalModel

### Use Cases

1. **Documentation Processing**
   - Convert technical PDFs to searchable Markdown
   - Extract specifications from Word documents
   - Process Excel-based configuration files

2. **Report Generation**
   - Convert analysis outputs to Markdown reports
   - Transform PowerPoint presentations to documentation
   - Create unified documentation from mixed formats

3. **Data Extraction**
   - Extract tables from PDFs for analysis
   - Convert Excel sheets to structured data
   - Process scanned documents with OCR

4. **LLM Pipeline Integration**
   - Prepare documents for AI analysis
   - Enable semantic search across document types
   - Support knowledge base creation

### Workflow Integration

```yaml
# Example workflow configuration
workflow:
  name: document_analysis
  steps:
    - name: convert_documents
      mcp_server: markitdown
      action: batch_convert
      input: specs/documents/
      output: specs/markdown/
      
    - name: analyze_content
      agent: documentation-agent
      input: specs/markdown/
      
    - name: generate_summary
      llm: claude-3
      template: technical_summary
```

## Development

### Running Tests

```bash
# Run MarkItDown MCP tests
uv run pytest src/mcp/markitdown/tests/

# Run with coverage
uv run pytest --cov=src.mcp.markitdown src/mcp/markitdown/tests/
```

### Extending Functionality

Create custom plugins in `src/mcp/markitdown/plugins/`:

```python
from markitdown import MarkItDown

class CustomConverter:
    def convert(self, file_path):
        # Custom conversion logic
        pass

# Register plugin
md = MarkItDown()
md.register_converter(".custom", CustomConverter())
```

## Security Considerations

- File access is restricted to configured directories
- Large file handling with configurable timeouts
- Input validation for all file operations
- Sanitization of extracted content
- Rate limiting for API endpoints

## Performance

- Parallel processing for batch conversions
- Caching of converted documents
- Streaming for large files
- Configurable memory limits

## References

- [MarkItDown GitHub](https://github.com/microsoft/markitdown)
- [MCP Protocol Specification](https://modelcontextprotocol.io/)
- [MarkItDown Documentation](https://github.com/microsoft/markitdown#readme)

## License

MarkItDown is licensed under the MIT License. See the [LICENSE](https://github.com/microsoft/markitdown/blob/main/LICENSE) file for details.