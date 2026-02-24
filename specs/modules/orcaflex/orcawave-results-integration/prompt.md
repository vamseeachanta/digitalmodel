# Prompt Documentation - OrcaFlex Integration of OrcaWave Results

## Original Context
This specification was created to separate the OrcaFlex integration tasks from the OrcaWave diffraction analysis specification, maintaining modularity and allowing independent execution of each phase.

## User Request
```
separate orcawave to orcaflex tasks into another dedicated spec to keep diffraction analysis simpler
```

## Curated Reuse Prompt
```
Create a comprehensive OrcaFlex integration specification for importing OrcaWave diffraction analysis results with the following requirements:

INPUT DATA:
- OrcaWave hydrodynamic database outputs
- Frequency-dependent coefficients (added mass, damping, excitation)
- Multiple file formats (YML, CSV, DAT)
- Phase information for all forces

TECHNICAL REQUIREMENTS:
1. Parse OrcaWave output files
2. Map coefficients to OrcaFlex structure
3. Create vessel model with imported hydrodynamics
4. Validate imported data integrity
5. Calculate and verify RAOs

DELIVERABLES:
- Python integration module using OrcFxAPI
- Data validation framework
- Batch processing capability
- Comprehensive documentation
- Test suite with benchmarks

AGENT ASSIGNMENT:
- OrcaFlex Agent for primary integration
- Testing Agent for validation
- Documentation Agent for guides
- OrcaWave Agent for format consultation

PARALLEL PROCESSING:
- Concurrent file parsing
- Parallel validation checks
- Batch vessel processing
- Achieve >2x speedup
```

## Key Context and Decisions

### 1. Separation Rationale
**Decision**: Separate OrcaFlex integration from OrcaWave analysis
**Benefits**:
- Modular specifications
- Independent execution
- Clear responsibility boundaries
- Easier maintenance and updates

### 2. Integration Architecture
**Key Components**:
- Data Parser: Handles multiple formats
- Coefficient Mapper: Transforms data structures
- OrcaFlex Importer: API-based integration
- Validator: Ensures physics and data integrity

### 3. Agent Responsibilities
**OrcaFlex Agent**:
- API integration
- Model creation
- Database import
- Batch processing

**Testing Agent**:
- Validation checks
- RAO verification
- Benchmark comparisons
- Integration testing

### 4. Critical Validations
- Matrix symmetry verification
- Positive definiteness checks
- Frequency range validation
- RAO comparison with OrcaWave
- Conservation principles

## Implementation Notes

### Technical Constraints
- OrcaFlex API license required
- Python 3.8+ with OrcFxAPI
- Memory considerations for large databases
- Thread-safe API operations

### Data Mapping Challenges
1. **Coordinate Systems**: Ensure consistent conventions
2. **Unit Conversions**: Handle metric/imperial
3. **Phase Information**: Preserve complex numbers
4. **Frequency Interpolation**: Maintain accuracy

### Performance Optimization
- Utilize numpy for matrix operations
- Implement caching for repeated calculations
- Batch API calls where possible
- Profile and optimize bottlenecks

## Validation Framework

### Required Checks
```python
validation_suite = {
    'symmetry': check_matrix_symmetry,
    'positive_definite': check_damping_pd,
    'frequency_coverage': validate_freq_range,
    'rao_comparison': compare_raos,
    'conservation': check_energy_conservation
}
```

### Tolerance Levels
- Matrix symmetry: 1e-6
- RAO comparison: 5%
- Force balance: 1e-3
- Frequency spacing: uniform preferred

## Risk Mitigation

### Identified Risks
1. **Format Changes**: OrcaWave output format updates
2. **API Version**: OrcaFlex API compatibility
3. **Large Datasets**: Memory management
4. **Numerical Precision**: Floating point errors

### Mitigation Strategies
- Version-aware parsers
- Compatibility layers
- Streaming data processing
- High-precision arithmetic

## Success Criteria

### Technical Success
- [ ] All coefficients imported correctly
- [ ] Validation suite passes 100%
- [ ] RAOs match within tolerance
- [ ] Batch processing functional
- [ ] No data loss in transfer

### Business Success
- [ ] Reduced integration time by 80%
- [ ] Zero manual intervention
- [ ] Complete traceability
- [ ] Comprehensive documentation
- [ ] User training completed

## Lessons Learned
1. Separation of concerns improves maintainability
2. Validation is critical at every stage
3. API integration requires robust error handling
4. Parallel processing significantly reduces time
5. Documentation prevents user errors

## Future Enhancements
1. Direct API bridge between OrcaWave and OrcaFlex
2. Real-time synchronization capabilities
3. Cloud-based processing options
4. Machine learning for validation
5. GUI for non-technical users