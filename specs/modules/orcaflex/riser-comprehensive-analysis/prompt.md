# OrcaFlex Riser Comprehensive Analysis Module - Prompt Documentation

## Original Request

**Command:** `/create-spec`
**Arguments:** `perform a comprehensive orcaflex run to define all inputs and requried outputs including reporting. submodule: riser`

**Date:** 2025-09-02
**Context:** Creating specification for comprehensive OrcaFlex riser analysis module

## Interpretation

The user requested a comprehensive specification for an OrcaFlex riser analysis module that:
1. Defines all possible inputs for riser analysis
2. Specifies all required outputs
3. Includes comprehensive reporting capabilities
4. Functions as a submodule within the OrcaFlex ecosystem

## Key Decisions Made

### 1. Scope Definition
- **Decision:** Include ALL riser types (drilling, SCR, lazy-wave, TTR, flexible, hybrid)
- **Rationale:** "Comprehensive" implies complete coverage of riser analysis capabilities

### 2. Analysis Types
- **Decision:** Include static, dynamic, fatigue, VIV, and extreme value analyses
- **Rationale:** These represent the full spectrum of riser engineering analyses

### 3. Input Structure
- **Decision:** Support multiple input formats (YAML, Excel, JSON)
- **Rationale:** Aligns with existing OrcaFlex module patterns in the repository

### 4. Output Requirements
- **Decision:** Provide engineering reports, data exports, and visualizations
- **Rationale:** Complete reporting requires multiple output formats for different stakeholders

### 5. Integration Approach
- **Decision:** Integrate with existing universal runner and module patterns
- **Rationale:** Maintains consistency with repository architecture

## Research Findings

### Repository Analysis
- Found existing OrcaFlex modules with established patterns
- Identified riser-related modules (vertical_riser, catenary, viv_analysis)
- Discovered universal runner pattern for batch processing
- Located signal analysis module for fatigue calculations

### Technical Considerations
- OrcaFlex API integration requirements
- Performance optimization needs (parallel processing)
- Standards compliance (API, DNV, ISO)
- Mock mode requirement for testing without license

## Specification Highlights

### Comprehensive Input Coverage
- **Riser Types:** All major riser configurations
- **Environmental:** Complete metocean, vessel, and soil inputs
- **Materials:** Full material property database
- **Analysis Control:** All analysis types with detailed parameters

### Complete Output Specification
- **Analysis Results:** Static, dynamic, fatigue, VIV, extreme
- **Reports:** Executive, technical, compliance formats
- **Data Exports:** CSV, Excel, JSON, HDF5
- **Visualizations:** Plots, contours, animations

### Integration Features
- CLI with standardized parameters
- Universal runner compatibility
- OrcaFlex API full integration
- Agent delegation strategy

## Curated Reuse Prompt

For future enhancements or similar specifications, use this prompt:

```
Create a comprehensive OrcaFlex analysis specification for [ANALYSIS_TYPE] that includes:

1. Complete input specifications covering:
   - All configuration parameters
   - Environmental conditions
   - Material properties
   - Analysis control settings

2. Detailed output requirements including:
   - Analysis results (specify types)
   - Engineering reports (formats and content)
   - Data exports (file formats)
   - Visualizations (plots and animations)

3. Integration requirements:
   - CLI interface with standard parameters
   - Universal runner compatibility
   - OrcaFlex API integration
   - Performance requirements

4. Quality assurance:
   - Validation requirements
   - Testing strategy
   - Documentation needs
   - Standards compliance

Ensure the specification follows the repository's module pattern at specs/modules/[module]/[feature]/ and includes:
- spec.md (main specification)
- tasks.md (implementation tasks with estimates)
- prompt.md (this documentation)
- diagrams/ (architecture diagrams)

The specification should be production-ready with clear success criteria and risk mitigation strategies.
```

## Lessons Learned

### What Worked Well
1. Comprehensive repository analysis provided excellent context
2. Following existing patterns ensured consistency
3. Detailed input/output specifications leave no ambiguity
4. Task breakdown with effort estimates aids planning

### Areas for Consideration
1. Specification is extensive - may need phased implementation
2. Some features may require specialized domain knowledge
3. Testing without OrcaFlex license requires mock mode
4. Performance requirements need validation with real data

## Follow-Up Recommendations

1. **Review with Domain Expert:** Validate riser engineering requirements
2. **Prioritize Features:** Consider MVP vs full implementation
3. **License Planning:** Ensure OrcaFlex license availability for testing
4. **Team Assignment:** Identify developers with OrcaFlex experience
5. **Validation Data:** Gather test cases for validation

## Related Specifications

- `specs/modules/orcaflex/mooring-tension-iteration/` - Mooring analysis patterns
- `specs/modules/orcaflex/orcaflex-examples-integration/` - Integration patterns
- `specs/modules/signal-analysis/` - Fatigue analysis methods
- `specs/modules/vertical-riser/` - Existing riser implementation

## Agent Assignments

### Primary Agent
- **OrcaFlex Agent:** Owns the complete riser analysis workflow

### Supporting Agents
- **Signal Analysis Agent:** Fatigue and rainflow counting
- **Testing Agent:** Parallel test execution
- **Documentation Agent:** Report generation
- **Visualization Agent:** Plot creation

## Version History

- **v1.0** (2025-09-02): Initial comprehensive specification created

---

*This prompt documentation serves as a reference for future iterations and similar specifications.*