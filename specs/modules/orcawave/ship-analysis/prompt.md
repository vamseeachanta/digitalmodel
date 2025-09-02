# OrcaWave Ship Analysis Module - Prompt Documentation

## Original User Request
```
perform a comprehensive orcawave run to define all inputs and requried outputs including reporting. submodule: ship
```

## Interpreted Requirements

The user is requesting a comprehensive specification for an OrcaWave ship analysis module that:

1. **Defines all input parameters** for wave-structure interaction analysis
2. **Specifies all required outputs** from diffraction/radiation calculations
3. **Includes comprehensive reporting** capabilities
4. **Creates a ship-specific submodule** within the OrcaWave module structure

## Context and Analysis

### Domain Understanding
OrcaWave is Orcina's specialized software for:
- Wave diffraction and radiation analysis
- Hydrodynamic coefficient calculation
- Second-order wave force computation (QTF)
- Hydrodynamic database generation for OrcaFlex

### Ship Analysis Context
OrcaWave ship analysis is critical for:
- Generating hydrodynamic databases for time-domain simulation
- Calculating wave loads for structural design
- Computing drift forces for mooring analysis
- Validating seakeeping performance
- Meeting regulatory requirements

### Key Technical Decisions

1. **Input Specification Strategy**:
   - Comprehensive mesh definition (GDF, STL, OBJ formats)
   - Complete mass and hydrostatic properties
   - Full wave environment specification
   - Detailed analysis configuration options

2. **Output Requirements Coverage**:
   - First-order coefficients (added mass, damping, excitation)
   - Response amplitude operators (RAOs)
   - Second-order forces (mean drift, slow drift QTFs)
   - Pressure distributions and sectional loads
   - OrcaFlex-ready hydrodynamic database

3. **Reporting Architecture**:
   - Technical reports with methodology and validation
   - Visualization package (plots, animations)
   - Compliance documentation for regulations
   - OrcaFlex integration validation

## Specification Approach

### Module Organization
```
specs/modules/orcawave/ship-analysis/
├── spec.md         # Comprehensive technical specification
├── tasks.md        # Detailed task breakdown (384 hours)
└── prompt.md       # This documentation
```

### Technical Architecture Decisions

1. **Processing Pipeline**:
   ```
   Geometry → Environment → Diffraction → Radiation → QTF → Database → Reports
   ```

2. **Integration Focus**:
   - Primary: OrcaFlex vessel type database
   - Secondary: CAD systems for geometry
   - Validation: Benchmark cases and analytical solutions

3. **Performance Optimization**:
   - Parallel processing for frequency/heading combinations
   - Adaptive mesh refinement
   - Symmetry exploitation
   - Memory streaming for large datasets

4. **Quality Assurance**:
   - Mesh convergence studies
   - Benchmark validation suite
   - Physical bounds checking
   - Cross-validation with OrcaFlex

## Agent Delegation Strategy

### Primary Agent
- **OrcaWave Agent**: Orchestrates complete analysis workflow

### Supporting Agents
- **OrcaFlex Agent**: Validates database integration and motion predictions
- **CAD Engineering Agent**: Handles geometry import and mesh generation
- **Documentation Agent**: Generates technical and compliance reports
- **Testing Agent**: Runs benchmark validations and convergence studies

### Agent Coordination
```yaml
delegation_matrix:
  orcawave_agent:
    primary_tasks:
      - diffraction_analysis
      - radiation_calculation
      - qtf_computation
      - database_generation
    
    delegates_to:
      cad_agent:
        - mesh_import
        - geometry_validation
        - surface_preparation
      
      orcaflex_agent:
        - database_validation
        - motion_comparison
        - time_domain_check
      
      doc_agent:
        - report_generation
        - plot_creation
        - compliance_documentation
```

## Implementation Strategy

### Phase-Based Development
1. **Foundation** (Week 1): Architecture and configuration
2. **Geometry** (Week 2): Mesh handling and validation
3. **Analysis** (Weeks 3-5): Diffraction, radiation, QTF
4. **Integration** (Week 6): Database generation and OrcaFlex link
5. **Validation** (Week 7): Benchmarks and convergence
6. **Deployment** (Week 8): CLI, documentation, packaging

### Risk Mitigation
- **Performance**: Parallel processing and optimization
- **Accuracy**: Comprehensive validation suite
- **Integration**: Standardized database formats
- **Usability**: Automated workflows and clear documentation

## Reusable Prompt Template

```
Create a comprehensive OrcaWave [structure_type] analysis module that:

1. Input Requirements:
   - Geometry: [mesh_format] with [quality_criteria]
   - Mass: [properties_needed]
   - Environment: [wave_conditions]
   - Analysis: [diffraction/radiation/qtf_options]

2. Processing Pipeline:
   - Pre-processing: [mesh_validation/optimization]
   - First-order: [diffraction/radiation_analysis]
   - Second-order: [qtf_calculation_method]
   - Database: [export_format]

3. Output Specifications:
   - Hydrodynamics: [coefficients/matrices]
   - Responses: [raos/transfer_functions]
   - Loads: [pressures/forces/moments]
   - Database: [orcaflex/wamit/aqwa]

4. Reporting Requirements:
   - Technical: [methodology/validation/results]
   - Visualization: [plots/animations/contours]
   - Compliance: [standards/regulations]
   - Integration: [validation_reports]

5. Performance Targets:
   - Mesh size: [max_panels]
   - Computation time: [target_hours]
   - Accuracy: [validation_tolerance]
   - Memory: [optimization_strategy]

Key Integration Points:
- OrcaFlex: [database_transfer]
- CAD: [geometry_import]
- Classification: [compliance_export]
```

## Lessons Learned

1. **OrcaWave Specificity**: Focus on hydrodynamic database generation
2. **OrcaFlex Integration**: Critical for practical application
3. **Mesh Quality**: Essential for accurate results
4. **QTF Computation**: Major computational bottleneck
5. **Validation Importance**: Benchmarks essential for confidence

## Next Steps

### Implementation Priorities
1. Start with first-order analysis (diffraction/radiation)
2. Add QTF calculation capability
3. Implement OrcaFlex database export
4. Build validation suite
5. Create automated workflows

### Testing Strategy
1. Unit tests for each solver component
2. Integration tests for complete workflow
3. Benchmark validation against published data
4. OrcaFlex motion comparison
5. Performance optimization

### Documentation Requirements
1. User manual with workflow examples
2. API documentation for developers
3. Validation report template
4. Troubleshooting guide
5. Best practices document

## Questions for User Clarification

1. **Ship Types**: Which vessel types are priority? (FPSO, tanker, container, bulk carrier)
2. **Analysis Focus**: Most critical outputs? (RAOs, QTFs, loads, database)
3. **OrcaFlex Version**: Target OrcaFlex version for database compatibility?
4. **Performance Requirements**: Acceptable computation times for typical cases?
5. **Validation Data**: Available benchmark cases or experimental data?

## Technical Notes

### Critical Considerations
- **Waterline Mesh**: Requires careful refinement for accuracy
- **Irregular Frequencies**: Need removal techniques (lid method)
- **QTF Symmetry**: Exploit for computational efficiency
- **Memory Management**: Stream processing for large meshes
- **Forward Speed**: Not included in current spec (stationary analysis)

### Future Enhancements
- Forward speed effects for transit analysis
- Multi-body interactions for side-by-side operations
- Hydroelastic coupling for flexible structures
- Machine learning for mesh optimization
- Real-time analysis updates

## Related Specifications

- `specs/modules/orcaflex/ship-mooring/`: Time-domain mooring analysis
- `specs/modules/aqwa/ship-analysis/`: Alternative hydrodynamic tool
- `specs/modules/structural/wave-loads/`: Structural load application

---
*Generated: 2025-09-02*
*Module: OrcaWave Ship Analysis*
*Version: 1.0.0*
*Agent: OrcaWave Module Agent*