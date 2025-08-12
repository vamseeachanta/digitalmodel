# OrcaFlex Results Dashboard - Prompt Documentation

## Original User Request

"There are 2 specs (specs\modules\orcaflex-browser and specs\modules\visualization\orcaflex-dashboard) for very similar work. Use /create-spec command in the repo to consolidate them. Follow the repo practices. Also note that I am using the spec in specs\modules\orcaflex-browser to reiterate and develop the latest dashboard."

## Context and Background

The user had been iteratively developing an OrcaFlex Results Dashboard with two parallel specification tracks:

1. **orcaflex-browser**: Implementation-focused specifications for the actual dashboard development
2. **visualization/orcaflex-dashboard**: Earlier visualization-focused specifications

Key developments during the iteration process included:
- Discovery of the `fe_filename` column in `dm*strut_dyn.csv` files for exact file matching
- Implementation of 20-core parallel processing for performance
- Auto-identification of maximum strut forces from summary files
- Automatic UI population with worst-case configuration
- Clean UI redesign matching user's screenshot requirements

## Curated Reuse Prompt

### For Creating Similar Marine Engineering Dashboards

```
Create a comprehensive web-based dashboard specification for [SYSTEM_NAME] that:

1. **Automatically identifies critical conditions** from simulation output files
2. **Uses parallel processing** (up to 20 cores) for performance optimization
3. **Provides real-time visualization** with interactive charts
4. **Follows module-based organization**: specs/modules/[module-name]/

Key requirements:
- Leverage summary files (like dm*) for pre-calculated maximum values
- Implement intelligent file pattern matching for time series data
- Auto-populate UI controls with worst-case scenario parameters
- Display time series data automatically upon folder selection
- Highlight maximum values in all force/stress charts
- Include busy state management for long-running operations
- Create both main dashboard and debug dashboard interfaces

Technical stack:
- Backend: Python Flask with ProcessPoolExecutor for parallel processing
- Frontend: HTML5 with Plotly.js for interactive charts
- Data: CSV file parsing with pandas
- Configuration: Excel or CSV-based parameter definitions

Performance targets:
- Process 1000+ files in under 20 seconds
- Instant max value identification from summary files
- Sub-2 second time series loading for single configuration

Include in specification:
1. Executive summary with business value
2. System architecture with mermaid diagrams
3. Data source patterns and file organization
4. Core logic flow (3 phases: identification, loading, interaction)
5. Implementation details for backend and frontend
6. Performance specifications and optimization strategies
7. UI/UX design system with color palette
8. Testing requirements (unit, integration, performance, UAT)
9. Deployment guidelines and security considerations
10. Future enhancements and migration path
```

### For Consolidating Multiple Specifications

```
Consolidate multiple related specifications for [SYSTEM_NAME]:

Current specs to merge:
- [SPEC_PATH_1]: [Description of focus area]
- [SPEC_PATH_2]: [Description of focus area]

Consolidation requirements:
1. Create unified specification in specs/modules/[module]/[system-name]-consolidated/
2. Include README.md with complete specification
3. Include prompt.md with original request and reuse prompts
4. Preserve all critical discoveries and optimizations from iterations
5. Follow repository's module-based organization pattern
6. Include executive summary highlighting key innovations
7. Add migration path from legacy specifications
8. Document deprecation timeline for old specs

Structure the consolidated spec with:
- Overview and executive summary
- System architecture (with mermaid diagrams)
- Data sources and patterns
- Core functionality phases
- Implementation details
- Performance specifications
- UI/UX design system
- Testing requirements
- Deployment guidelines
- Security considerations
- Future enhancements
- Migration path
- Appendices with examples
```

## Key Discoveries During Development

### Critical Technical Discoveries

1. **fe_filename Column Discovery**
   - Found in `dm*strut_dyn.csv` files
   - Contains exact `.sim` basename for file matching
   - Eliminates guesswork in file association
   - 100% accuracy in time series file selection

2. **Summary File Optimization**
   - `dm*` files contain pre-calculated max/min values
   - 1000x faster than scanning time series files
   - Enables instant worst-case identification

3. **Parallel Processing Sweet Spot**
   - 20 cores optimal for Windows systems
   - ProcessPoolExecutor handles file I/O efficiently
   - Reduces 952 file processing from minutes to ~15 seconds

### UI/UX Improvements

1. **Auto-Configuration Flow**
   - Folder selection → Auto max identification → UI population → Chart display
   - No manual searching required
   - Worst-case scenario visible immediately

2. **Clean Design Implementation**
   - Matched user's screenshot requirements
   - Separated FST1 (struts 1-4) and FST2 (struts 5-8) charts
   - Yellow highlighting for selected filename
   - Professional color scheme with proper contrast

3. **Debug Tools Integration**
   - Comprehensive debug dashboard for troubleshooting
   - Performance monitoring and benchmarking
   - Export logs functionality
   - Quick test buttons for all operations

## Lessons Learned

### What Worked Well

1. **Iterative Discovery Process**
   - User feedback led to critical `fe_filename` discovery
   - Performance issues drove parallel processing implementation
   - UI feedback resulted in cleaner, more professional design

2. **Module-Based Organization**
   - Clear separation of concerns
   - Easy to locate and update specifications
   - Supports multiple parallel development tracks

3. **Debug-First Development**
   - Early debug dashboard creation saved significant troubleshooting time
   - Performance metrics helped identify bottlenecks
   - Log export simplified issue reporting

### Areas for Improvement

1. **Initial File Pattern Assumptions**
   - Started with complex regex patterns
   - Discovery of `fe_filename` simplified everything
   - Lesson: Look for authoritative data sources first

2. **Performance Optimization Timing**
   - Should have implemented parallel processing earlier
   - Initial sequential processing was too slow for production
   - Lesson: Plan for scale from the beginning

3. **Specification Consolidation**
   - Multiple specs created confusion
   - Should have consolidated earlier in process
   - Lesson: Regular specification reviews and consolidation

## Future Reuse Scenarios

This specification pattern works well for:

1. **Any simulation post-processing dashboard**
   - Finite element analysis results
   - Computational fluid dynamics outputs
   - Structural analysis visualizations

2. **Large dataset visualization systems**
   - Automatic worst-case identification
   - Parallel file processing
   - Real-time chart updates

3. **Engineering analysis platforms**
   - Multi-parameter configuration systems
   - Time series data visualization
   - Performance-critical file operations

## Command History

```bash
# Initial attempts with missing tools
python tools/create-spec-enhanced.py orcaflex-results-dashboard orcaflex-browser enhanced
python tools/create-spec.py orcaflex-results-dashboard

# Manual consolidation created
Write: specs/modules/orcaflex-browser/orcaflex-results-dashboard-consolidated/README.md
Write: specs/modules/orcaflex-browser/orcaflex-results-dashboard-consolidated/prompt.md
```

## Related Specifications

- `specs/modules/orcaflex/browser-interface/` - Browser interface specifications
- `specs/modules/orcaflex/results-dashboard/implementation/` - Complete dashboard implementation
- `specs/modules/orcaflex/browser-interface/REVISED-SPEC-COMPLETE.md` - Latest working specification
- `specs/modules/orcaflex/force-analysis/strut-force-identification-spec.md` - Critical discovery documentation

---

*This prompt documentation captures the complete context, discoveries, and reusable patterns from the OrcaFlex Results Dashboard consolidation process.*