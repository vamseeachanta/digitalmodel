# Prompt Documentation: Strut Foundation Fatigue Analysis

## Original Request Context

**User Request**: Create spec for a custom analysis using the file: `.temp\wlng_fatigue_methodology\fatigue_methodology.md`

**Date Created**: 2025-01-20
**Agent**: Claude Code (Opus 4.1)
**Repository**: digitalmodel

## Input Analysis

### Source Document Analysis
The specification was created based on a detailed fatigue methodology document (`fatigue_methodology.md`) that outlined a comprehensive 9-step procedure for strut foundation fatigue evaluation using rainflow counting. The methodology included:

1. **Environmental Load Cases**: 18 wave + 16 wind conditions
2. **Time Domain Analysis**: 3-hour simulations per condition  
3. **Rainflow Counting**: Extract load ranges and cycle counts for 8 mooring struts
4. **Load Scaling**: Scale loads for 81 fatigue conditions using wind/wave scaling laws
5. **Annual Weighting**: Weight by occurrence percentages
6. **FEA Integration**: Map loads to stresses using unit load approach
7. **Stress Mapping**: Convert load ranges to stress ranges with SCF
8. **Damage Calculation**: Apply ABS "E" S-N curve and Miner's rule
9. **Life Estimation**: Calculate fatigue life and verify against design requirements

### Key Technical Parameters Extracted
- **Load Conditions**: 34 simulation cases (18 wave @ Hs=0.5m, 16 wind @ 10m/s)
- **Fatigue Matrix**: 81 combined wind-wave conditions with occurrence percentages
- **Time Duration**: 3-hour simulations, 200-second rainflow analysis
- **Structural Elements**: 8 mooring struts with 5 critical stress locations each
- **S-N Curve Parameters**: ABS "E" in Air (A=1.04×10¹², m=3, C=1.48×10¹¹, r=5)
- **Scaling Laws**: Wind loads ∝ (wind speed)², Wave loads ∝ wave height

## Specification Development Process

### 1. Domain Analysis
- **Marine Engineering Focus**: Offshore fatigue analysis methodology
- **Signal Processing**: Rainflow counting algorithm requirements  
- **Finite Element Integration**: Stress mapping from unit loads
- **Standards Compliance**: ABS fatigue design requirements

### 2. Module Architecture Design
- **Modular Structure**: Separate modules for each analysis step
- **Configuration-Driven**: YAML files for environmental and fatigue conditions
- **CLI Interface**: Command-line interface with parallel processing support
- **Data Pipeline**: Clear input/output specifications with validation

### 3. Agent Delegation Strategy
- **Signal Analysis Agent**: Rainflow counting implementation
- **Marine Engineering Agent**: Fatigue methodology and load scaling
- **FEA Agent**: Stress mapping and unit load calculations
- **Testing Agent**: Comprehensive validation and benchmarking
- **Documentation Agent**: User guides and API documentation

### 4. Risk Assessment
- **High Complexity**: Specialized domain knowledge requirements
- **Algorithm Accuracy**: Critical validation needs for rainflow counting
- **Performance Requirements**: Large dataset processing (272 arrays)
- **Integration Challenges**: Multiple engineering disciplines coordination

## Implementation Approach

### Technical Framework
- **Python Module**: `src/digitalmodel/modules/fatigue_analysis/`
- **Dependencies**: NumPy, Pandas, SciPy for numerical computations
- **CLI Framework**: Click for command-line interface
- **Configuration**: YAML-based configuration management
- **Testing**: Comprehensive unit and integration test suites

### Quality Assurance Strategy
- **Algorithm Validation**: Test against ASTM E1049 standard examples
- **Manual Verification**: Validate against known manual calculations (1% tolerance)
- **Performance Benchmarking**: 5-minute processing target for full dataset
- **Documentation Standards**: Complete user guides and API documentation

### Integration Requirements
- **Repository Compliance**: Follow digitalmodel coding standards
- **UV Environment**: Mandatory use of repository UV environment
- **Module Pattern**: Conform to `specs/modules/<module>/` structure
- **OrcaFlex Integration**: Seamless workflow integration with existing tools

## Reuse Prompt for Similar Projects

### For Marine Structural Fatigue Analysis
```
Create a fatigue analysis specification for [structure type] using [methodology document]. 

Required elements:
1. Extract complete methodology from source document
2. Identify all load cases, scaling laws, and calculation procedures
3. Design modular Python implementation with CLI interface
4. Include comprehensive testing and validation requirements
5. Specify agent delegation for specialized domain knowledge
6. Follow digitalmodel repository patterns and UV environment usage

Key considerations:
- Marine engineering domain expertise requirements
- Signal processing algorithm accuracy validation
- Multi-agent coordination for complex implementations
- Performance requirements for large dataset processing
- Integration with existing OrcaFlex/AQWA workflows

Output: Complete specification with technical architecture, task breakdown, and implementation timeline.
```

### For Signal Processing Integration
```
Develop signal analysis module specification for [analysis type] based on [methodology source].

Focus areas:
1. Algorithm implementation requirements (rainflow, FFT, filtering, etc.)
2. Data processing pipeline design
3. Performance optimization for large time series datasets
4. Validation against established standards
5. Integration with marine engineering workflows

Agent assignments:
- Signal Analysis Agent: Core algorithm implementation
- Testing Agent: Validation and performance testing
- Marine Engineering Agent: Domain-specific requirements
- Documentation Agent: User guides and examples
```

### For Multi-Domain Engineering Analysis
```
Create comprehensive analysis system specification combining [domain 1] and [domain 2] methodologies.

Requirements:
1. Extract complete technical procedures from source documentation
2. Design inter-agent collaboration strategy
3. Implement configuration-driven approach
4. Include comprehensive validation framework
5. Follow repository standards and UV environment requirements

Critical success factors:
- Domain expert validation requirements
- Cross-disciplinary integration points
- Performance and scalability requirements
- Documentation and usability standards
```

## Lessons Learned

### Effective Practices
1. **Thorough Source Analysis**: Complete extraction of technical parameters before design
2. **Modular Architecture**: Clean separation of concerns for maintainability
3. **Agent Specialization**: Leverage domain-specific agents for accuracy
4. **Validation-First**: Build validation requirements into initial specification
5. **Configuration-Driven**: YAML configuration for flexibility and maintenance

### Risk Mitigation Strategies
1. **Domain Knowledge Gaps**: Mandatory escalation protocol for specialized questions
2. **Algorithm Complexity**: Use established libraries where possible
3. **Performance Requirements**: Parallel processing design from start
4. **Quality Assurance**: Comprehensive testing with manual validation benchmarks

### Repository Integration
1. **Standards Compliance**: Follow all repository coding and documentation standards
2. **UV Environment**: Mandatory UV usage for all development and testing
3. **Module Organization**: Strict adherence to `specs/modules/` structure
4. **Cross-Module Integration**: Design for seamless workflow integration

## Technical Debt Considerations

### Future Enhancement Opportunities
- **Additional S-N Curves**: Extend beyond ABS "E" to other material standards
- **Multi-Physics Integration**: Include thermal and corrosion effects
- **Probabilistic Analysis**: Monte Carlo uncertainty quantification
- **Real-Time Processing**: Streaming analysis for continuous monitoring

### Scalability Planning
- **Cloud Processing**: Design for distributed computing environments
- **Database Integration**: Structured storage for large analysis campaigns
- **API Development**: REST API for external tool integration
- **Visualization Tools**: Interactive dashboards for results exploration

## Success Metrics

### Immediate Goals
- [ ] Complete specification acceptance by stakeholders
- [ ] Agent assignment confirmation and capability verification
- [ ] Timeline approval and resource allocation
- [ ] Technical approach validation by domain experts

### Implementation Targets
- [ ] Functional prototype within 2 weeks
- [ ] Validation against manual calculations within 1% tolerance
- [ ] Performance requirements met (5-minute processing target)
- [ ] Full documentation and user guide completion
- [ ] Integration with existing OrcaFlex workflows

### Long-Term Vision
- [ ] Standard tool for all marine fatigue analysis projects
- [ ] Template for similar multi-domain analysis implementations
- [ ] Foundation for advanced probabilistic fatigue analysis
- [ ] Integration point for AI-driven design optimization