# Prompt Documentation - OrcaWave Sea Cypress Diffraction Analysis

## Original User Request
```
/create-spec for orcawave diffraction analysis. 
The key inputs are:
geometry: Copy the geometries in below to spec 
J:\B1512 Nork & Mendez (legal) Hyundai Peacepia vs Sea Cypress sinking\ACMA Work\Sea Cypress Tug Data\Rhino Model Sea Cypress
- consider all 3 geoemtries below:
  - Sea Cypress_0.25 Mesh_Ascii.stl
  - Sea Cypress_0.25 Mesh_Binary.obj
  - Sea Cypress_0.25 Mesh_Binary.stl
- determine the most (if all 3 geometries are suitable) suitable geometry for use in diffraction analysis. Provide the reason why?
- create an input .yml file 
- create a batch file to run the .yml file to provide results
- save the spreadsheet results 
- import results to orcaflex file for further dynamic analysis

ensure to assign appropriate agents for tasks and subtasks as needed.
```

## Curated Reuse Prompt
```
Create a comprehensive OrcaWave diffraction analysis specification for vessel hydrodynamics with the following requirements:

GEOMETRY INPUTS:
- Multiple geometry formats available (STL ASCII, STL Binary, OBJ)
- Located at: [INSERT_GEOMETRY_PATH]
- Files: [LIST_GEOMETRY_FILES]
- Need evaluation of optimal format for OrcaWave

TECHNICAL REQUIREMENTS:
1. Geometry format selection with justification
2. YAML configuration file for analysis parameters
3. Batch execution script for automated processing
4. Results export to spreadsheet format
5. OrcaFlex integration for dynamic analysis

DELIVERABLES:
- Geometry evaluation report
- OrcaWave input configuration (.yml)
- Batch processing script (.bat)
- Results in Excel/CSV format
- OrcaFlex-ready hydrodynamic database
- Integration documentation

AGENT ASSIGNMENT:
- Use OrcaWave Agent for primary analysis
- CAD Engineering Agent for geometry validation
- OrcaFlex Agent for integration
- Testing Agent for validation
- Documentation Agent for deliverables

PARALLEL PROCESSING:
- Implement parallel geometry validation
- Concurrent results export
- Parallel test execution
- Achieve >3x speedup where possible
```

## Key Context and Decisions

### 1. Geometry Format Selection
**Decision**: Binary STL format recommended
**Rationale**:
- Native OrcaWave support via GDF converter
- Compact file size for efficient I/O
- Preserves mesh topology accurately
- Industry standard for marine CFD

### 2. Analysis Configuration
**Key Parameters**:
- Frequency range: 0.01 - 3.0 rad/s (100 steps)
- Wave directions: 0° - 180° (15° increments)
- Panel size: 0.25m (matching mesh resolution)
- Water depth: 100m (deep water assumption)

### 3. Agent Architecture
**Primary Agent**: OrcaWave Agent
- Domain expertise in diffraction analysis
- Batch processing capabilities
- Integration with OrcaFlex

**Support Agents**:
- CAD Engineering: Geometry validation
- Testing: Benchmark validation
- Documentation: Report generation

### 4. Parallel Processing Strategy
- Geometry validation: 3 files simultaneously
- Configuration tasks: 5 parallel streams
- Results export: 4 format exports in parallel
- Testing: Concurrent test execution

## Implementation Notes

### Critical Success Factors
1. **Geometry Quality**: Watertight mesh required
2. **License Management**: Queue system for OrcaWave
3. **Memory Optimization**: 16GB limit consideration
4. **Network Performance**: Local caching for large files
5. **Integration Testing**: OrcaFlex compatibility

### Technical Constraints
- OrcaWave v11.0+ required
- Python 3.8+ for automation
- Windows batch scripting
- Network drive access (J: drive)

### Risk Mitigation
- Pre-validation of all geometries
- Incremental testing approach
- Rollback procedures documented
- License availability checks
- Memory usage monitoring

## Agent Coordination Protocol

### Task Distribution
```yaml
orcawave_agent:
  responsibilities:
    - Analysis configuration
    - Batch execution
    - Results processing
    - OrcaFlex export
  
cad_agent:
  responsibilities:
    - Geometry validation
    - Mesh quality checks
    - Format conversion
    
testing_agent:
  responsibilities:
    - Benchmark validation
    - Integration testing
    - Performance testing
    
documentation_agent:
  responsibilities:
    - Technical reports
    - User guides
    - API documentation
```

### Communication Flow
1. **Initialization**: OrcaWave Agent coordinates setup
2. **Validation**: CAD Agent validates geometries in parallel
3. **Execution**: OrcaWave Agent runs analysis
4. **Verification**: Testing Agent validates results
5. **Documentation**: Documentation Agent creates deliverables

## Optimization Opportunities

### Performance Enhancements
- GPU acceleration for solver (if available)
- Distributed computing for large meshes
- Incremental analysis for parameter studies
- Result caching for repeated queries

### Workflow Improvements
- CI/CD pipeline integration
- Automated report generation
- Real-time progress monitoring
- Cloud storage integration

### Future Extensions
- Machine learning for parameter optimization
- Web-based execution interface
- Multi-vessel batch processing
- Automated mesh refinement

## Validation Criteria

### Technical Validation
- [ ] Mesh quality metrics within tolerance
- [ ] Conservation of energy satisfied
- [ ] Symmetry conditions verified
- [ ] Benchmark comparison &lt; 5% deviation

### Process Validation
- [ ] All agents successfully coordinated
- [ ] Parallel speedup &gt; 2.5x achieved
- [ ] Error handling tested
- [ ] Documentation complete

### Business Validation
- [ ] Results suitable for OrcaFlex
- [ ] Processing time &lt; 4 hours
- [ ] Automation reduces manual effort 60%
- [ ] Deliverables meet requirements

## Lessons Learned
1. Binary STL format most efficient for OrcaWave
2. Parallel geometry validation saves significant time
3. Agent coordination critical for complex workflows
4. Batch scripting essential for repeatability
5. Early validation prevents downstream issues