# AQWA Ship Analysis Module - Prompt Documentation

## Original User Request
```
perform a comprehensive aqwa run to define all inputs and requried outputs including reporting: submodule: ship
```

## Interpreted Requirements

The user is requesting a comprehensive specification for an AQWA ship analysis module that:

1. **Defines all input parameters** needed for ship hydrodynamic analysis
2. **Specifies all required outputs** from AQWA calculations
3. **Includes comprehensive reporting** capabilities
4. **Creates a ship-specific submodule** within the AQWA module structure

## Context and Analysis

### Domain Understanding
AQWA is ANSYS's hydrodynamic analysis software suite used for:
- Frequency domain analysis (AQWA-LINE)
- Time domain simulations (AQWA-DRIFT/NAUT)
- Stability assessments (AQWA-LIBRIUM)
- Coupled analysis (AQWA-FLEX)

### Ship Analysis Requirements
Ship hydrodynamic analysis typically involves:
- Seakeeping performance evaluation
- Wave load predictions
- Motion response calculations
- Stability assessments
- Mooring and positioning analysis (for specific vessels)

### Key Technical Decisions

1. **Comprehensive Input Coverage**:
   - Hull geometry (mesh, parametric, GDF formats)
   - Mass properties and loading conditions
   - Environmental conditions (waves, wind, current)
   - Analysis settings for all AQWA modules

2. **Complete Output Specification**:
   - Hydrodynamic coefficients (added mass, damping)
   - Response Amplitude Operators (RAOs)
   - Time series results and statistics
   - Sectional loads and pressure distributions
   - Compliance and regulatory reports

3. **Reporting Architecture**:
   - Executive summaries for stakeholders
   - Technical reports with detailed analysis
   - Compliance reports for regulatory bodies
   - Visualization outputs (plots, animations)

## Specification Approach

### Module Organization
Created a hierarchical structure:
```
specs/modules/aqwa/ship-analysis/
├── spec.md         # Comprehensive specification
├── tasks.md        # Detailed task breakdown
└── prompt.md       # This documentation
```

### Technical Decisions

1. **Input Format**: YAML-based configuration for clarity and validation
2. **Processing Pipeline**: Pre-process → Solve → Post-process → Report
3. **Parallelization**: Multi-case and distributed processing support
4. **Integration**: Leverage existing AQWA module components
5. **Compliance**: Built-in checks for IMO, class society, and flag state rules

### Implementation Strategy

1. **Phase 1-2**: Foundation and input processing (2-3 weeks)
2. **Phase 3-4**: Solver interface and results extraction (2-3 weeks)
3. **Phase 5-6**: Reporting and integration (2-3 weeks)
4. **Phase 7**: Optimization and deployment (1-2 weeks)

## Agent Delegation Strategy

### Primary Agent
- **AQWA Agent**: Main coordinator for ship analysis workflows

### Supporting Agents
- **CAD Engineering Agent**: For geometry processing
- **Documentation Agent**: For report generation
- **Testing Agent**: For validation and benchmarking
- **DevOps Agent**: For deployment and CI/CD

### Inter-Agent Communication
```yaml
delegation:
  aqwa_agent:
    owns: [analysis_setup, solver_execution, results_processing]
    delegates_to:
      cad_agent: [geometry_validation, mesh_quality]
      doc_agent: [report_generation, compliance_docs]
      test_agent: [validation_cases, benchmarking]
```

## Reusable Prompt for Future Enhancements

```
Create a comprehensive AQWA [vessel_type] analysis module that:

1. Defines all inputs for [specific_analysis_type]:
   - Geometry: [mesh/parametric/GDF]
   - Environment: [waves/wind/current]
   - Operations: [loading_conditions]
   - Analysis: [frequency/time/stability]

2. Specifies all outputs:
   - Hydrodynamics: [coefficients/forces]
   - Motions: [RAOs/time_series/statistics]
   - Loads: [sectional/global/pressure]
   - Reports: [technical/compliance/executive]

3. Includes reporting for:
   - Stakeholders: [executives/engineers/regulators]
   - Standards: [IMO/class_society/flag_state]
   - Formats: [PDF/Excel/interactive]

4. Integrates with:
   - Existing modules: [AQWA/OrcaFlex/structural]
   - External tools: [ANSYS/classification_software]
   - Workflows: [optimization/digital_twin]

Key requirements:
- Automation level: [percentage]
- Performance targets: [models/hour]
- Compliance standards: [list]
- Visualization needs: [static/animated/interactive]
```

## Lessons Learned

1. **Comprehensive Coverage**: Ship analysis requires extensive input/output specifications
2. **Regulatory Focus**: Compliance reporting is critical for ship analysis
3. **Performance Considerations**: Large models require parallel processing
4. **Integration Complexity**: Multiple AQWA modules need coordination
5. **Reporting Diversity**: Different stakeholders need different report formats

## Next Steps

1. **Implementation Priority**:
   - Start with basic frequency domain analysis
   - Add time domain capabilities
   - Implement stability assessments
   - Enhance reporting features

2. **Testing Strategy**:
   - Use standard ship models for validation
   - Compare with manual AQWA results
   - Validate compliance checks
   - Performance benchmarking

3. **Documentation Needs**:
   - User manual with examples
   - API documentation
   - Troubleshooting guide
   - Best practices document

## Related Specifications

- `specs/modules/orcaflex/ship-mooring/`: Ship mooring analysis
- `specs/modules/structural/ship-strength/`: Structural assessment
- `specs/modules/cfd/ship-resistance/`: Resistance and propulsion

## Questions for User Clarification

1. **Vessel Types**: Which ship types are priority? (tanker, container, bulk, etc.)
2. **Analysis Focus**: Which analyses are most critical? (seakeeping, stability, mooring)
3. **Compliance Requirements**: Which regulations are mandatory?
4. **Integration Needs**: Which existing systems need connection?
5. **Performance Targets**: What are acceptable computation times?

## Technical Notes

- AQWA requires specific mesh quality for accurate results
- Forward speed effects need special consideration
- Shallow water effects may need additional modeling
- Non-linear effects (green water, slamming) need time domain analysis
- Coupling with structural analysis may be needed for springing/whipping

---
*Generated: 2025-09-02*
*Module: AQWA Ship Analysis*
*Version: 1.0.0*