# OrcaFlex Dashboard Specification - Prompt History

> Created: 2025-08-08  
> Module: visualization/orcaflex-dashboard  
> Purpose: Complete prompt history and reusable implementation prompt

## Original User Request

Create a comprehensive specification document for the OrcaFlex Results Visualization Dashboard following the Agent OS module-based structure. 

The specification should be placed in `specs/modules/visualization/orcaflex-dashboard/` directory and include:

1. **Main specification document** (`spec.md`) with:
   - Executive summary
   - Technical requirements based on the 5 planning iterations
   - Architecture overview  
   - Data structure analysis
   - Implementation roadmap

2. **Supporting files**:
   - `prompt.md` - Complete prompt history and reusable prompt
   - `tasks.md` - Detailed task breakdown
   - `architecture.md` - Technical architecture diagrams
   - `data-analysis.md` - Findings from CSV analysis

**Key Requirements to Include:**
- Web-based dashboard for browsing D:\\1522\\ctr7\\orcaflex\\rev_a08\\output\\csv
- Interactive polar plots of response quantities from dm_* summary files
- Time trace visualization with component grouping (struts vs jackets)
- Multi-level filtering by analysis case, loading condition, heading
- Professional engineering dashboard with export capabilities

**Data Structure Found:**
- Multiple analysis folders: 02c_005yr, 03c_100yr, 03c_100yr_env_sens, etc.
- dm_* files contain summary statistics with polar heading data (0° to 345°)
- Components: fst1, fst2, strut, jacket, lngc with different response types
- Loading conditions encoded in filenames (hwl/lwl, 125km3/180km3, pb/sb)
- File references in dm_*_inputs.csv link to time trace files

Create a professional, comprehensive specification following Agent OS standards.

## Context and Background

This specification was developed based on extensive analysis of OrcaFlex simulation results for offshore floating structures. The project involves:

1. **Data Source**: Complex CSV results from OrcaFlex simulations located at D:\\1522\\ctr7\\orcaflex\\rev_a08\\output\\csv
2. **Analysis Scope**: Multiple return periods (5-year, 100-year) and environmental sensitivity studies
3. **Engineering Domain**: Offshore marine engineering with focus on floating LNG terminals
4. **Technical Challenge**: Processing and visualizing large-scale simulation datasets with complex hierarchical structure

## Key Design Decisions

### Module-Based Organization
Following Agent OS standards, the specification is organized in the `specs/modules/visualization/orcaflex-dashboard/` structure to ensure:
- Clear separation of concerns
- Scalable architecture
- Integration with existing Agent OS workflows
- Reusable patterns for similar visualization projects

### Professional Engineering Focus
The specification emphasizes:
- Industry-standard data formats and conventions
- Professional visualization suitable for client presentations
- Robust error handling for production environments
- Performance optimization for large datasets

## Implementation Guidance

### Technology Stack Recommendations
- **Frontend**: React with TypeScript for type safety and maintainability
- **Visualization**: D3.js or Plotly for interactive charts with engineering-specific requirements
- **Backend**: Python with FastAPI for performance and async capabilities
- **Database**: SQLite for metadata with Redis caching for performance

### Critical Success Factors
1. **Performance**: Handle &gt;10GB datasets with sub-second response times
2. **Usability**: Intuitive interface for marine engineers with varying technical backgrounds
3. **Reliability**: Production-grade error handling and data validation
4. **Extensibility**: Architecture supporting additional analysis types and data sources

## Reusable Implementation Prompt

```
CONTEXT: You are implementing a professional web-based dashboard for visualizing OrcaFlex offshore engineering simulation results. This is a production system for marine engineers analyzing floating structure responses.

SPECIFICATION: Follow the complete specification at @specs/modules/visualization/orcaflex-dashboard/spec.md

KEY REQUIREMENTS:
1. Process CSV files from D:\\1522\\ctr7\\orcaflex\\rev_a08\\output\\csv with polar heading data (0° to 345°)
2. Create interactive polar plots for response quantities with component grouping (struts vs jackets)
3. Implement time trace visualization with statistical overlays
4. Build multi-level filtering by analysis case, loading condition, and heading
5. Ensure professional export capabilities (PNG, SVG, PDF, CSV, Excel)

ARCHITECTURE:
- Frontend: React/TypeScript with D3.js or Plotly for visualizations
- Backend: Python/FastAPI with background processing
- Data: SQLite for metadata, Redis for caching, file system interface
- Performance: &lt;3s initial load, &lt;500ms chart updates, &lt;2GB RAM usage

DATA STRUCTURE:
- Analysis folders: 02c_005yr, 03c_100yr, 03c_100yr_env_sens
- Summary files: dm_* with polar data (24 headings)
- Components: fst1, fst2, strut, jacket, lngc
- Loading conditions: hwl/lwl, 125km3/180km3, pb/sb
- Time traces: Referenced in dm_*_inputs.csv

STANDARDS:
- Follow Agent OS code style guidelines
- Implement comprehensive error handling
- Include mock patterns for testing licensed software
- Use SI units with proper boundary conversions
- Apply offshore engineering domain knowledge

DELIVERABLES:
- Complete web dashboard with all specified functionality
- Professional-grade visualizations suitable for client presentations
- Comprehensive test suite with mock data patterns
- Documentation for deployment and maintenance

Begin implementation following the phased approach outlined in the specification.
```

## Related Documentation

- **Main Specification**: @specs/modules/visualization/orcaflex-dashboard/spec.md
- **Technical Architecture**: @specs/modules/visualization/orcaflex-dashboard/architecture.md
- **Implementation Tasks**: @specs/modules/visualization/orcaflex-dashboard/tasks.md
- **Data Analysis Report**: @specs/modules/visualization/orcaflex-dashboard/data-analysis.md

---

*This prompt history follows Agent OS standards for comprehensive project documentation and enables consistent implementation across development sessions.*