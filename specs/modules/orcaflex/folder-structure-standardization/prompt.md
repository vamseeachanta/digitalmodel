# Original Prompt

**User Request**: "make this folder structure consistent for all orcaflex related modules and submodules."

## Context
The user had just completed updating the mooring analysis module to:
1. Save PNG visualizations in `specs/modules/orcaflex/mooring-tension-iteration/go-by/output/visual/`
2. Save CSV files in `specs/modules/orcaflex/mooring-tension-iteration/go-by/output/.csv/`
3. Use only relative paths (no absolute paths)
4. Ensure consistent file naming (*_pretension_analysis.csv, etc.)

## Intent
Create a standardized folder structure that:
- Maintains consistency across all OrcaFlex modules
- Uses relative paths exclusively
- Provides predictable output locations for reviewers
- Simplifies module development and maintenance

## Key Requirements Identified
1. **Consistency**: All modules follow identical structure
2. **Relative Paths**: No absolute paths in any configuration
3. **Clear Organization**: Separation of input, output, config, scripts
4. **Review-Friendly**: Predictable locations for all outputs
5. **Maintainable**: Easy to understand and extend

## Curated Reuse Prompt
"Implement the OrcaFlex folder structure standard defined in specs/modules/orcaflex/folder-structure-standardization/spec.md across all OrcaFlex modules. Ensure all configurations use relative paths only, maintain consistent output organization (analysis/, visual/, reports/, logs/), and follow the established naming conventions. Priority modules are mooring-tension-iteration, postprocess-optimization, and browser-interface. Create migration scripts to automate the conversion process and validation tools to ensure compliance."