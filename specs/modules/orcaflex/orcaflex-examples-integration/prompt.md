# Prompt Documentation

## Original Request

**User Request:**
"download examples from online portal https://www.orcina.com/resources/examples/ and add those files in docs/modules/orcaflex/examples. Convert all files into yml files (requires orcaflex license). assess the files for key features (modelled components, type of modelling, etc) and handover this knowledge to the module agent for future learnings and use"

## Interpretation

The user wants to:
1. **Collect** all OrcaFlex examples from the official Orcina resources portal
2. **Convert** them from native formats (.dat/.sim) to YAML format for better accessibility
3. **Analyze** each example to extract key technical features
4. **Integrate** this knowledge into the OrcaFlex module agent for enhanced capabilities

## Key Requirements Identified

### Technical Requirements
- Web scraping capability for Orcina portal
- OrcaFlex API access for file conversion
- YAML validation and formatting
- Feature extraction algorithms
- Knowledge base integration

### Functional Requirements
- Automated download process
- Batch conversion with error handling
- Component and pattern recognition
- Searchable catalog generation
- Module agent enhancement

## Design Decisions

### Architecture Choices
1. **Modular Pipeline**: Separate concerns for downloading, converting, analyzing, and integrating
2. **Batch Processing**: Handle multiple files efficiently with parallel processing where possible
3. **Error Resilience**: Implement retry logic and graceful degradation
4. **Knowledge Structure**: Use structured format for easy agent consumption

### Technology Stack
- **Web Scraping**: BeautifulSoup/requests for portal access
- **Conversion**: OrcFxAPI for file format transformation
- **Analysis**: Custom Python scripts for feature extraction
- **Storage**: YAML for converted files, JSON for metadata
- **Integration**: Direct updates to agent's knowledge base files

## Success Criteria

1. **Completeness**: All available examples downloaded and converted
2. **Accuracy**: Converted YAML files maintain full fidelity with originals
3. **Insight**: Comprehensive feature analysis for each example
4. **Integration**: Module agent can reference and learn from examples
5. **Documentation**: Clear catalog and usage guides

## Reuse Prompt

For future similar tasks, use this enhanced prompt:

"Create a comprehensive system to collect, convert, and analyze domain-specific examples from [SOURCE_URL]. The system should:
1. Automatically download all available examples to [TARGET_DIRECTORY]
2. Convert files from [SOURCE_FORMAT] to [TARGET_FORMAT] using [CONVERSION_TOOL]
3. Extract and categorize key features including [FEATURE_LIST]
4. Integrate analyzed knowledge into [MODULE_AGENT] for improved capabilities
5. Generate searchable documentation and catalog

Include error handling, batch processing, validation, and comprehensive testing for all components."