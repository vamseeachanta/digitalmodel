# OrcaFlex Examples Collection

This directory contains the comprehensive collection of OrcaFlex examples downloaded from the official Orcina portal, converted to YAML format, and analyzed for feature extraction.

## Directory Structure

```
examples/
├── raw/              # Original .dat and .sim files from Orcina
├── yaml/             # Converted YAML files
├── metadata/         # Feature analysis and metadata JSON files
├── reports/          # Analysis reports and summaries
├── catalog/          # Searchable catalog and index
└── README.md         # This file
```

## Collection Status

- **Total Examples**: [To be populated]
- **Categories**: [To be populated]
- **Last Updated**: 2024-12-19

## Usage

### Accessing Examples

All examples are organized by category in their respective directories. Each example includes:
- Original file (in `raw/`)
- YAML conversion (in `yaml/`)
- Feature analysis (in `metadata/`)

### Searching Examples

Use the catalog index in `catalog/` to search examples by:
- Model components (vessels, lines, buoys)
- Analysis types (static, dynamic, fatigue)
- Environmental conditions
- Industry applications

## Integration with OrcaFlex Module Agent

The examples and extracted knowledge are integrated into the OrcaFlex module agent at `agents/orcaflex/`. The agent uses this knowledge to:
- Reference real examples when answering questions
- Provide code snippets based on official patterns
- Explain concepts with practical implementations
- Suggest best practices from Orcina's guidelines

## Data Sources

All examples are sourced from:
- [Orcina Examples Portal](https://www.orcina.com/resources/examples/)
- Official OrcaFlex documentation
- Technical notes and validation cases

## License and Terms

These examples are provided by Orcina for educational and reference purposes. Please refer to Orcina's terms of use for specific licensing information.