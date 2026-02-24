# OrcaWave Integration Module - Prompt Documentation

> Created: 2025-08-23
> Version: 1.0.0
> Status: Complete
> Module: OrcaWave
> Spec Reference: @specs/modules/orcawave/orcawave-integration/spec.md

## Original User Request

**Request**: Create a comprehensive specification for the OrcaWave module in D:/github/digitalmodel/specs/modules/orcawave/orcawave-integration/

**Context**: Based on the analysis of OrcaWave documentation, create comprehensive specification files including spec.md (main specification), tasks.md (detailed task breakdown), prompt.md (prompt documentation), executive-summary.md (stakeholder summary), and diagrams/architecture.mermaid (system architecture).

**Requirements Specified**:
1. **spec.md** - Main specification with executive summary, core capabilities, technical architecture, file formats, performance requirements
2. **tasks.md** - Detailed 5-phase task breakdown with 46 total hours estimated
3. **prompt.md** - Complete prompt history and curated reuse guidance  
4. **executive-summary.md** - Stakeholder-focused summary with business value
5. **diagrams/architecture.mermaid** - System architecture diagram

## Technical Analysis Performed

### Repository Structure Analysis
- **Existing Modules**: Analyzed `specs/modules/orcaflex/`, `specs/modules/signal-analysis/` for patterns
- **Agent Integration**: Identified existing agents in `agents/orcawave/` directory structure
- **Documentation Base**: Reviewed `docs/modules/orcawave/README.md` and benchmark references

### OrcaWave Domain Analysis
- **Core Capabilities**: Diffraction/radiation analysis using panel method
- **File Formats**: .gdf (mesh), .yml (config), .owr (results), .hyd (OrcaFlex export)
- **Integration Points**: OrcaFlex hydrodynamic databases, AQWA benchmark validation
- **Performance Characteristics**: 10-60 second analyses, batch processing requirements

### Architecture Assessment
- **COM Interface**: Python wrapper for OrcaWave COM automation
- **Batch Processing**: Parallel execution framework with resource management
- **Result Pipeline**: Extraction, validation, and export utilities
- **Quality Framework**: AQWA benchmark comparison and validation

## Key Requirements Identified

### Functional Requirements
1. **Complete Automation**: End-to-end workflow from configuration to results
2. **Batch Processing**: Parallel execution of 50+ analyses daily
3. **OrcaFlex Integration**: Seamless hydrodynamic database export
4. **AQWA Validation**: Automated benchmark comparison with &lt;5% tolerance
5. **Professional Reporting**: Excel exports with plots and engineering summaries

### Technical Requirements
1. **Agent Integration**: Specialized OrcaWave agent with domain expertise
2. **Parallel Processing**: Mandatory &gt;3x speed improvement through parallelization
3. **Error Recovery**: Robust error handling with automatic retry mechanisms
4. **Resource Management**: License pooling and memory optimization
5. **Standards Compliance**: API, DNV, ABS guidelines for wave load analysis

### Performance Requirements
1. **Throughput**: 50-100 configurations per day processing capability
2. **Time Reduction**: 75% reduction compared to manual workflows  
3. **Memory Efficiency**: Support for large mesh models (&gt;20,000 panels)
4. **Reliability**: 99% success rate with comprehensive error handling

## Implementation Approach

### Agent-Based Architecture
- **Primary Agent**: @agents/orcawave/ with specialized hydrodynamic analysis expertise
- **Subagent Delegation**: Task-specific subagents for parallel execution
  - API Integration Specialist (COM interface)
  - Hydrodynamic Analysis Specialist (diffraction/radiation)
  - Batch Processing Specialist (parallel execution)
  - Integration Specialist (OrcaFlex/AQWA)
  - Testing Specialist (validation framework)

### Parallel Processing Strategy
- **Phase Parallelization**: Independent tasks within each phase executed concurrently
- **Component Parallelization**: Multiple analysis components processed simultaneously
- **Resource Optimization**: Intelligent license and memory management
- **Pipeline Optimization**: Overlapped execution stages for maximum throughput

### Quality Assurance Framework
1. **Multi-Level Testing**: Unit tests, integration tests, benchmark validation
2. **Industry Benchmarks**: AQWA reference cases for continuous validation
3. **Performance Monitoring**: Real-time metrics and optimization
4. **Documentation Standards**: Complete API docs, user guides, examples

## Specification Structure Decisions

### File Organization
- **specs/modules/orcawave/orcawave-integration/** - Following repository module pattern
- **No date prefixes** - Descriptive folder names per Agent OS standards
- **Required files**: spec.md, tasks.md, prompt.md (mandatory)
- **Enhanced files**: executive-summary.md, architecture.mermaid (optional)

### Task Breakdown Strategy
- **5 Phases**: Foundation → Core Analysis → Automation → Integration → QA
- **46 Total Hours**: Realistic estimates based on complexity analysis
- **Parallel Opportunities**: Identified concurrent execution possibilities
- **Subagent Assignments**: Specialized agents for domain-specific tasks

### Technical Depth Decisions
- **Comprehensive Coverage**: Complete OrcaWave feature set integration
- **Production Ready**: Full error handling, logging, monitoring
- **Industry Standard**: API, DNV, ABS compliance requirements
- **Future Proof**: Extensible architecture for new capabilities

## Cross-Repository Integration

### AssetUtilities References
- **Common Patterns**: @assetutilities:src/modules/agent-os/enhanced-create-specs/
- **Shared Utilities**: @assetutilities:agents/registry/sub-agents/workflow-automation
- **Template Standards**: @assetutilities:.common-commands/utilities/parallel_utils.py

### DigitalModel Ecosystem Integration
- **Marine Engineering Module**: Shared environmental conditions and wave spectra
- **OrcaFlex Module**: Direct hydrodynamic database integration
- **Signal Analysis Module**: Post-processing integration for time series
- **Visualization Module**: 3D mesh rendering and result visualization

## Curated Reuse Prompt

### For Similar Marine Analysis Module Creation

```
Create a comprehensive specification for the [SOFTWARE_NAME] module integration in the DigitalModel repository.

CONTEXT:
- Marine engineering software integration following OrcaWave module pattern
- Agent OS standards with specialized domain agents
- Parallel processing mandatory for >3x speed improvement
- Industry compliance (API, DNV, ABS standards)

REQUIREMENTS:
1. **Specification Structure** (follow exactly):
   - specs/modules/[module]/[feature-name]/ (no date prefixes)
   - spec.md (comprehensive technical specification)
   - tasks.md (5-phase breakdown with effort estimates)
   - prompt.md (complete prompt documentation)
   - executive-summary.md (stakeholder summary)
   - diagrams/architecture.mermaid (system architecture)

2. **Technical Architecture**:
   - Python API wrapper for [SOFTWARE] COM/API interface
   - Batch processing with parallel execution framework
   - Configuration management (YAML-based)
   - Result extraction and validation utilities
   - Integration with existing DigitalModel modules

3. **Agent Integration**:
   - Create specialized [software] agent in agents/[module]/
   - Subagent delegation for parallel task execution
   - Domain expertise configuration
   - Integration with marine engineering agent ecosystem

4. **Implementation Phases**:
   - Phase 1: Foundation (agent, API wrapper, configuration)
   - Phase 2: Core Analysis (primary software functionality)
   - Phase 3: Automation (batch processing, parallel execution)
   - Phase 4: Integration (ecosystem integration, validation)
   - Phase 5: Quality Assurance (testing, documentation, optimization)

5. **Quality Standards**:
   - Industry benchmark validation
   - >90% test coverage
   - Production-ready error handling
   - Complete API documentation

6. **Performance Requirements**:
   - 75% time reduction vs manual workflows
   - Batch processing capability (50+ daily)
   - Parallel execution optimization
   - Resource management (licenses, memory)

DELIVERABLES:
- Complete specification following Agent OS standards
- Detailed task breakdown with effort estimates
- Professional stakeholder summaries
- System architecture diagrams
- Full prompt documentation for future reuse

Use existing marine engineering patterns and integrate with OrcaFlex, AQWA, and visualization modules.
```

### Adaptation Guidelines

**For Different Software Types**:
1. **CAD Integration**: Focus on geometry processing, file format conversion
2. **FEA Software**: Emphasize mesh handling, solver management, result processing
3. **CFD Analysis**: Highlight parallel computing, large data handling, visualization
4. **Design Tools**: Focus on parametric modeling, optimization, design validation

**For Different Industries**:
1. **Aerospace**: Adapt standards to FAA, EASA compliance requirements
2. **Automotive**: Focus on crash analysis, durability, emissions standards
3. **Civil Engineering**: Emphasize building codes, seismic analysis, structural design
4. **Energy**: Focus on grid integration, safety standards, environmental compliance

## Lessons Learned

### Successful Patterns
1. **Agent-First Approach**: Starting with specialized agents improves domain expertise
2. **Parallel by Design**: Early parallel processing design enables performance optimization
3. **Benchmark Integration**: Industry benchmark validation ensures quality and credibility
4. **Comprehensive Documentation**: Complete documentation reduces user adoption barriers

### Optimization Opportunities
1. **Template Reuse**: Common patterns can be templated for faster spec creation
2. **Subagent Libraries**: Reusable subagents reduce development time
3. **Integration Patterns**: Standardized integration reduces complexity
4. **Validation Frameworks**: Reusable validation reduces quality assurance effort

### Risk Mitigation Insights
1. **License Management**: Early license pooling prevents bottlenecks
2. **Memory Planning**: Large model support requires upfront architecture decisions
3. **Error Recovery**: Comprehensive error handling reduces production issues
4. **Version Management**: Software version compatibility requires ongoing attention

## Future Enhancement Opportunities

### Short-term Enhancements (3-6 months)
1. **Machine Learning Integration**: Automated parameter optimization
2. **Cloud Computing**: Distributed processing for large batch jobs
3. **Advanced Visualization**: Interactive 3D result exploration
4. **Mobile Monitoring**: Real-time progress tracking on mobile devices

### Long-term Vision (6-18 months)  
1. **Digital Twin Integration**: Real-time analysis with sensor data
2. **Multi-Physics Coupling**: Integration with structural and thermal analysis
3. **Optimization Loops**: Automated design optimization workflows
4. **AI-Assisted Analysis**: Machine learning for result interpretation

### Strategic Considerations
1. **Industry Evolution**: Monitor software vendor API changes
2. **Standards Updates**: Track industry standard evolution
3. **Technology Integration**: Evaluate emerging technologies for integration
4. **User Experience**: Continuous improvement based on user feedback

---

**This prompt documentation provides complete context for reproducing similar marine engineering software integration specifications within the DigitalModel ecosystem.**