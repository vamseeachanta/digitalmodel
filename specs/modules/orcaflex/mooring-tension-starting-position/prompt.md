# Prompt Documentation: OrcaFlex Starting Mooring Tension Analysis

## Original User Request

The user requested a specification for defining starting mooring tension for marine analysis using OrcaFlex with the following requirements:

### Core Requirements
1. **Model Creation**
   - Create OrcaFlex model with vessel(s) in static configuration
   - Vessel definition should be 0DOF (or None) - vessel is fixed
   - Properly define mooring and fender locations

2. **Analysis Execution**
   - Run analysis to create .sim files
   - Extract results to determine status of lines, fenders, etc.

3. **Force Extraction** (using mooring.py module)
   - Global and local forces on each component
   - Specifically: lines, fenders, vessels, foundations
   - Comprehensive force data collection

4. **Force Balance Requirements**
   - X, Y, and Z forces should be balanced on vessels (moving components)
   - Achieve equilibrium state for all components

5. **Iteration Process**
   - Change vessel Z position (only) to balance Z force (achieve zero force)
   - Adjust tensions per min/max guidelines across all lines
   - Goal: Achieve zero X and Y forces on vessel

6. **Convergence Criteria** (whichever achieved first)
   - Maximum of 5 iterations
   - Change in line tension < 10 kN
   - Other criteria to be defined

## Analysis and Interpretation

### Technical Context
The user is requesting a system to determine optimal starting conditions for mooring analysis. This is a critical step in offshore engineering to establish:
- Proper pretension values for mooring lines
- Equilibrium vessel position
- Balanced force distribution

### Key Technical Challenges
1. **Multi-variable Optimization**: Balancing forces in 3D requires simultaneous adjustment of multiple parameters
2. **Constraint Management**: Must respect physical limits (tension ranges, draft limits)
3. **Convergence Strategy**: Need robust algorithm to handle potentially non-linear behavior
4. **Integration Complexity**: Must work with existing OrcaFlex API and mooring.py module

## Design Decisions

### Architecture Choices
1. **Modular Design**: Separate concerns into distinct modules (force analysis, optimization, convergence)
2. **Scipy Integration**: Use established optimization libraries for robustness
3. **Iterative Approach**: Implement step-wise optimization with monitoring
4. **Configuration-Driven**: YAML-based configuration for flexibility

### Algorithm Selection
1. **Z-Position**: Newton-Raphson method for single-variable optimization
2. **Tension Balance**: Multi-variable optimization using scipy.optimize
3. **Convergence**: Multiple criteria with early termination

### Integration Strategy
1. **Leverage Existing Modules**: Use mooring.py for force extraction
2. **Universal Runner**: Integrate with existing batch processing
3. **OrcaFlex API**: Direct manipulation of model parameters

## Implementation Approach

### Phase 1: Foundation
- Set up project structure
- Create configuration management
- Establish OrcaFlex interface

### Phase 2: Core Components
- Implement force extraction
- Create balance calculations
- Integrate with existing modules

### Phase 3: Optimization
- Develop position optimizer
- Create tension optimizer
- Implement convergence tracking

### Phase 4: Integration
- Build main orchestrator
- Add batch processing
- Create reporting system

## Questions for Clarification

### Technical Clarifications Needed
1. **Vessel Configuration**
   - Are there multiple vessels in the system?
   - How should vessel interactions be handled?
   - What are typical draft ranges?

2. **Mooring Configuration**
   - How many mooring lines typically?
   - Are there grouped lines with shared properties?
   - What are typical tension ranges?

3. **Convergence Criteria**
   - Should we add relative improvement as criterion?
   - What about oscillation detection?
   - How to handle non-convergent cases?

4. **Output Requirements**
   - What format for results output?
   - Integration with existing reporting tools?
   - Real-time monitoring needs?

### Assumptions Made
1. Single vessel system (extendable to multi-vessel)
2. Static analysis only (no dynamic effects)
3. Small displacement theory (linear approximations valid)
4. Standard SI units throughout
5. OrcaFlex Python API available

## Success Criteria

### Functional Success
- Achieves force balance within tolerance
- Converges within iteration limit
- Produces physically valid results
- Integrates with existing workflow

### Performance Success
- Iteration time < 60 seconds
- Convergence rate > 80%
- Minimal manual intervention
- Robust error handling

### Quality Success
- Comprehensive testing coverage
- Clear documentation
- Maintainable code structure
- Extensible architecture

## Reuse Prompt for Future Enhancements

### Prompt Template for Similar Features
```
Create a specification for [analysis type] in OrcaFlex that:
1. Sets up [model configuration]
2. Runs [analysis type] to generate results
3. Extracts [specific data] using [existing module]
4. Performs iterative optimization to achieve [target condition]
5. Uses convergence criteria: [list criteria]
6. Integrates with existing [module names]

Technical requirements:
- Must handle [constraints]
- Should optimize for [objectives]
- Needs to integrate with [systems]
- Expected performance: [metrics]

Output should include:
- Comprehensive specification document
- Detailed task breakdown with estimates
- Integration approach with existing modules
- Testing strategy
- Implementation architecture
```

## Lessons Learned

### Best Practices Identified
1. **Modular Architecture**: Separation of concerns essential for complex systems
2. **Configuration Management**: YAML-based config provides flexibility
3. **Convergence Strategy**: Multiple criteria prevent infinite loops
4. **Integration First**: Design around existing modules from start

### Potential Pitfalls
1. **Numerical Stability**: Optimization may encounter singularities
2. **API Limitations**: Some OrcaFlex operations may be restricted
3. **Performance**: Large models may require optimization
4. **Convergence**: Complex systems may not converge easily

## Next Steps

### Immediate Actions
1. Validate specification with domain expert
2. Confirm OrcaFlex API capabilities
3. Review existing mooring.py module documentation
4. Set up development environment

### Future Enhancements
1. Multi-vessel support
2. Dynamic analysis integration
3. Machine learning for initial guess
4. Real-time monitoring dashboard
5. Sensitivity analysis tools