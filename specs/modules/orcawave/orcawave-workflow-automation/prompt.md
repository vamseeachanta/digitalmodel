# OrcaWave Workflow Automation - Complete Prompt History

## Original User Request

Create a comprehensive specification for OrcaWave diffraction analysis workflow with the following structure:

**Context:**
- Location: specs/modules/orcawave/orcawave-workflow-automation/
- Integration: GMsh → OrcaWave → OrcaFlex pipeline
- Agents: GMsh agent for meshing, OrcaWave agent for analysis

**Requirements:**
1. **spec.md**: Automated OrcaWave diffraction analysis workflow
2. **tasks.md**: Detailed implementation breakdown
3. **prompt.md**: Complete context documentation
4. **executive-summary.md**: Business value and stakeholder summary

**Technical Workflow Steps:**
- Step 1 (User): Geometry preparation using GMsh (reference: agents/gmsh)
- Step 2 (AI): OrcaWave input file generation from GMsh mesh
- Step 3 (AI/User): Execution scripts with parallel validation
- Step 4 (AI/User): Post-processing and data extraction

## Context Analysis

### Repository Pattern Compliance
Following the mandatory `specs/modules/<module>/<feature>/` structure:
- **Location**: `D:\github\digitalmodel\specs\modules\orcawave\orcawave-workflow-automation\`
- **Required Files**: spec.md, tasks.md, prompt.md, executive-summary.md
- **Architecture**: Module-based organization with agent delegation

### Existing Agent Capabilities

#### GMsh Agent (`agents/gmsh/`)
**Core Capabilities:**
- 1D/2D/3D mesh generation with industry-standard algorithms
- Geometry processing (STEP, IGES, STL import and healing)
- Quality assessment with Jacobian, aspect ratio, skewness metrics
- Mesh optimization through Laplacian smoothing and remeshing
- Batch processing with parallel execution (4+ workers)
- Export integration to OrcaFlex, ANSYS, and analysis tools

**Performance Metrics:**
- Mesh Generation: >100K elements/second
- Quality Assessment: <1 second for 1M elements
- Batch Processing: Linear scaling with CPU cores

#### OrcaWave Agent (`agents/orcawave/`)
**Domain Expertise:**
- Marine hydrodynamics and offshore engineering knowledge
- Panel method theory and implementation
- Wave mechanics and frequency domain analysis
- API, DNV, ABS regulatory compliance
- Mesh convergence and quality assessment

**Technical Skills:**
- OrcaWave COM API programming and automation
- Python integration and scripting capabilities
- Parallel processing optimization
- Result validation and benchmarking
- OrcaFlex workflow integration

### Integration Requirements

#### Input Processing
- **Primary Input**: Sea Cypress_0.25 Mesh_Ascii.msh (existing geometry)
- **Format Support**: ASCII/Binary STL, OBJ, MSH formats
- **Quality Requirements**: Minimum Jacobian > 0.3, Aspect Ratio < 5.0

#### Output Requirements
- **OrcaWave Results**: RAO data, QTF matrices, hydrodynamic coefficients
- **OrcaFlex Integration**: Compatible vessel data (.yml, .json formats)
- **Validation Reports**: Convergence analysis, quality metrics
- **Visualizations**: Polar plots, frequency response analysis

### Agent Delegation Strategy

#### Inter-Agent Collaboration Protocol
Based on mandatory requirements from CLAUDE.md:

```yaml
delegation_matrix:
  mesh_processing:
    primary_agent: gmsh
    capabilities: [quality_assessment, format_conversion, optimization]
    support_agents: [orcawave, testing]
  
  analysis_execution:
    primary_agent: orcawave
    capabilities: [domain_expertise, configuration_generation, validation]
    support_agents: [testing, documentation]
  
  parallel_validation:
    primary_agent: testing
    capabilities: [script_validation, error_detection, quality_assurance]
    support_agents: [gmsh, orcawave]
  
  documentation:
    primary_agent: documentation
    capabilities: [report_generation, visualization, user_guides]
    support_agents: [orcawave]
```

#### Parallel Processing Requirements
Mandatory parallel execution for:
- Multiple mesh quality assessments
- Concurrent input validation testing
- Parallel analysis configuration generation
- Simultaneous result processing and reporting

### Technical Implementation Context

#### UV Environment Integration
**Mandatory Requirements:**
- All tasks must use `uv run` for script execution
- Dependencies managed through `uv add` to maintain pyproject.toml
- Testing through `uv run pytest` for environment consistency
- Package installations via UV to keep repository updated

#### Specialized Knowledge Escalation
**Critical Escalation Triggers:**
- Unknown OrcaWave file formats or configurations
- Marine engineering calculations requiring domain expertise
- Industry standards (API, DNV, ABS) compliance decisions
- Complex hydrodynamic algorithms requiring validation
- Tasks estimated >10 minutes compute time

### Business Value Proposition

#### Automation Benefits
- **Time Savings**: Reduce manual workflow from hours to minutes
- **Error Reduction**: Eliminate manual configuration and transcription errors
- **Consistency**: Standardized analysis procedures across projects
- **Scalability**: Enable batch processing of multiple geometries

#### Integration Value
- **Seamless Pipeline**: GMsh → OrcaWave → OrcaFlex integration
- **Quality Assurance**: Automated validation at each workflow stage
- **Traceability**: Complete audit trail from geometry to results
- **Standardization**: Consistent analysis procedures and reporting

## Curated Reuse Prompt

### For Future OrcaWave Workflow Extensions

**Context Template:**
```
Create an OrcaWave workflow automation for [specific analysis type] with the following requirements:

1. **Agent Integration**: Leverage existing GMsh and OrcaWave agents from agents/ directory
2. **Pipeline Structure**: [Input Source] → [Processing Steps] → [Output Format]
3. **Validation Strategy**: Parallel testing before user execution with specialized agents
4. **UV Environment**: Mandatory use of repository UV environment for all operations
5. **Escalation Protocol**: Clear escalation for specialized knowledge or >10min compute tasks

**Required Deliverables:**
- spec.md: Technical specification with mathematical formulations
- tasks.md: Implementation breakdown with effort estimates and agent assignments
- prompt.md: Complete context and reuse documentation
- executive-summary.md: Stakeholder value proposition

**Quality Standards:**
- 90% automation rate with <5% error tolerance
- Complete agent delegation with parallel processing
- Comprehensive validation with error recovery
- Integration with existing repository workflows
```

### Agent Capability References

**When working with marine hydrodynamics:**
- Reference `agents/orcawave/` for domain expertise
- Use `agents/gmsh/` for mesh quality and optimization
- Apply Panel Method theory: $F_e(\omega) = \sum_{j=1}^{6} A_{ij}(\omega) \ddot{x}_j + B_{ij}(\omega) \dot{x}_j$
- Validate convergence: $Q_{panel} = \frac{A_{min}}{A_{max}} \cdot \frac{J_{min}}{J_{ideal}}$

**For workflow automation:**
- Implement parallel validation before user execution
- Use agent delegation matrix for task assignment
- Ensure UV environment compatibility for all operations
- Include escalation protocols for specialized knowledge

### Success Metrics Template

**Performance Targets:**
- Automation Level: >90% of workflow steps
- Processing Time: Complete pipeline <30 minutes
- Error Rate: <5% failure on validated inputs
- Validation Accuracy: 99% agreement with manual verification

**Quality Assurance:**
- Comprehensive test coverage (>95%)
- Agent coordination validation
- Error handling and recovery testing
- User experience optimization

---

*This prompt provides complete context for OrcaWave workflow automation development and serves as a template for future marine analysis workflow specifications.*