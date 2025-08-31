# Task-Specific Context Extraction Agents - Prompt Documentation

## Original User Request
Create task specific context agents for getting context from a file type(s) for each task below. Document the best tools to use to get context:
- HTML files
- PDF files  
- Excel calculation to Python
- PDF calculation to Python
- PDF formulas to Python

## Prompt Analysis
The user requested creation of specialized AI agents that can:
1. Extract context from different file types (HTML, PDF, Excel)
2. Convert calculations and formulas to Python code
3. Document the best tools for each extraction task
4. Provide task-specific optimization for each file type

## Key Requirements Identified
1. **File Type Specialization**: Each agent must be optimized for its specific file format
2. **Context Extraction**: Agents must extract meaningful, structured context
3. **Formula Conversion**: Mathematical formulas and calculations must be converted to executable Python
4. **Tool Documentation**: Best-in-class tools and libraries must be identified and documented
5. **Task-Specific Design**: Each agent should be designed for its specific use case

## Design Decisions Made
1. **Modular Architecture**: Separate agents for each file type to ensure specialization
2. **Parallel Processing**: Enable batch processing with parallel execution
3. **Tool Selection**: Chose industry-standard libraries for each file type
4. **Unified Interface**: Common interface for all agents while maintaining specialization
5. **Formula Preservation**: Maintain mathematical accuracy during conversion

## Curated Reuse Prompt

### For Future Enhancement or Similar Projects:
```
Create a comprehensive specification for task-specific context extraction agents that can:

1. Extract structured content from HTML files using BeautifulSoup4 and related tools
2. Parse PDF documents for text, tables, and formulas using pdfplumber and OCR
3. Convert Excel calculations and formulas to Python using openpyxl and formula parsers
4. Extract mathematical calculations from PDFs and convert to Python/NumPy
5. Parse LaTeX formulas from PDFs and generate Python implementations

Requirements:
- Document the best tools and libraries for each file type
- Create modular, specialized agents for each task
- Enable parallel processing for batch operations
- Provide a unified interface for all agents
- Include comprehensive testing and validation
- Support inter-agent delegation for complex workflows

The specification should include:
- Detailed agent specifications with capabilities
- Tool recommendations with installation commands
- Implementation strategies and code examples
- Integration patterns and delegation matrix
- Performance optimization techniques
- Testing and validation approaches

Focus on creating production-ready agents that can be immediately deployed for real-world document processing and formula extraction tasks.
```

## Implementation Approach

### 1. Agent Specialization Strategy
Each agent was designed with deep specialization:
- **HTML Agent**: Focus on DOM traversal and structured data extraction
- **PDF Agent**: Emphasis on layout preservation and table extraction
- **Excel Agent**: Formula parsing and function mapping expertise
- **Calculation Agents**: Mathematical expression recognition and conversion

### 2. Tool Selection Rationale
Tools were selected based on:
- **Maturity**: Well-established libraries with active maintenance
- **Performance**: Benchmarked for speed and accuracy
- **Features**: Comprehensive functionality for each use case
- **Integration**: Easy integration with Python ecosystem

### 3. Conversion Strategy
Formula/calculation conversion approach:
- **Parse**: Extract formulas using specialized parsers
- **Tokenize**: Break down into component parts
- **Map**: Convert functions to Python equivalents
- **Generate**: Create executable Python code
- **Validate**: Test accuracy against original

## Key Technical Insights

### HTML Processing
- BeautifulSoup4 provides the best balance of features and ease of use
- lxml offers superior performance for large documents
- Readability helps extract main content from web pages

### PDF Processing
- pdfplumber excels at preserving layout information
- Camelot provides superior table extraction
- OCR integration is essential for scanned documents

### Excel Conversion
- formulas library offers comprehensive formula parsing
- openpyxl provides full Excel file format support
- Function mapping requires careful consideration of edge cases

### Mathematical Extraction
- SymPy enables symbolic mathematics manipulation
- LaTeX parsing requires specialized libraries
- Visual formula recognition may require OCR or ML models

## Delegation Strategy

The specification includes a comprehensive delegation matrix that enables:
1. **Task Routing**: Automatic routing to the most qualified agent
2. **Agent Collaboration**: Multiple agents working on complex tasks
3. **Fallback Mechanisms**: Secondary agents for error recovery
4. **Performance Optimization**: Parallel execution where possible

## Testing Considerations

1. **Unit Testing**: Each agent tested independently
2. **Integration Testing**: Agent interactions validated
3. **Accuracy Testing**: Formula conversion accuracy verified
4. **Performance Testing**: Speed and resource usage benchmarked
5. **Edge Cases**: Handling of malformed or complex documents

## Security Considerations

1. **Input Validation**: Prevent malicious file processing
2. **Sandboxing**: Execute conversions in isolated environments
3. **Resource Limits**: Prevent DoS from large files
4. **Code Injection**: Validate generated Python code

## Future Enhancements

Potential areas for expansion:
1. **Machine Learning**: ML-based formula recognition
2. **Additional Formats**: Support for Word, PowerPoint, etc.
3. **Cloud Integration**: API endpoints for remote processing
4. **Real-time Processing**: Streaming document analysis
5. **Multi-language Support**: Extraction in multiple languages

## Lessons Learned

1. **Tool Diversity**: Different file types require specialized tools
2. **Performance Trade-offs**: Accuracy vs. speed considerations
3. **Error Handling**: Robust fallback mechanisms essential
4. **Testing Importance**: Comprehensive testing prevents issues
5. **Documentation Value**: Clear documentation accelerates adoption

## References

### Libraries and Tools
- BeautifulSoup4: https://www.crummy.com/software/BeautifulSoup/
- pdfplumber: https://github.com/jsvine/pdfplumber
- openpyxl: https://openpyxl.readthedocs.io/
- SymPy: https://www.sympy.org/
- Camelot: https://camelot-py.readthedocs.io/

### Related Specifications
- Agent OS Standards: @.agent-os/standards/
- Testing Guidelines: @.agent-os/standards/testing.md
- Code Style: @.agent-os/standards/code-style.md

## Quick Start Guide

For immediate implementation:
```bash
# Install core dependencies
pip install beautifulsoup4 pdfplumber openpyxl sympy pandas numpy

# Create agent structure
mkdir -p src/modules/context-extraction/agents
mkdir -p src/modules/context-extraction/tools
mkdir -p src/modules/context-extraction/utils

# Run tests
pytest tests/modules/context-extraction/

# Use agents
from context_extraction import ContextExtractionManager
manager = ContextExtractionManager()
context = manager.extract("document.pdf", task_type="formula_extraction")
```

This specification provides a complete blueprint for implementing task-specific context extraction agents with production-ready quality and comprehensive functionality.