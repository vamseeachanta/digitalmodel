# User Prompt: Marine Structural Analysis Module

## Project Overview

Create a comprehensive marine structural analysis module that processes vessel hull data and performs stress analysis calculations according to classification society rules (ABS, DNV, Lloyd's Register).

## Problem Statement

Marine engineers need a reliable tool to analyze hull structural integrity under various loading conditions. Current manual calculation methods are time-consuming, error-prone, and difficult to audit.

## Requirements

### Functional Requirements

1. **Data Input**
   - Accept hull geometry data (frames, stiffeners, plates)
   - Load material properties (steel grades, aluminum alloys)
   - Define loading scenarios (still water, wave loads, cargo distribution)
   - Support multiple input formats (CSV, Excel, JSON)

2. **Structural Analysis**
   - Perform finite element analysis for stress distribution
   - Calculate bending moments and shear forces
   - Evaluate buckling resistance
   - Assess fatigue life prediction
   - Generate safety factor calculations

3. **Validation & Compliance**
   - Verify against classification society rules (ABS Steel Vessel Rules)
   - Check longitudinal strength requirements
   - Validate local strength criteria
   - Apply required safety factors (typically 1.5 for ultimate strength)

4. **Output & Reporting**
   - Generate interactive HTML reports with stress plots
   - Export calculation results to CSV for further analysis
   - Create visual representations using Plotly (interactive 3D stress visualization)
   - Document all calculations with references to standards

5. **Performance**
   - Handle models with up to 10,000 structural elements
   - Complete analysis within 10 minutes
   - Maintain numerical accuracy of 1e-6

### Non-Functional Requirements

1. **Accuracy**: Results must match benchmark problems within 2% error
2. **Reliability**: 80%+ test coverage, all critical paths tested
3. **Maintainability**: Comprehensive logging for debugging
4. **Usability**: Command-line interface with YAML configuration
5. **Documentation**: Full calculation trail for audit purposes

## Expected Deliverables

1. Python module with CLI interface
2. YAML configuration schema
3. Interactive HTML analysis reports
4. Comprehensive test suite (unit, integration, verification)
5. User documentation
6. Validation against benchmark problems

## Success Criteria

- Correctly analyzes reference hull section within 2% of commercial FEA software
- Generates complete analysis report in under 10 minutes
- Passes all classification society rule checks
- 80%+ test coverage
- Peer-reviewed by licensed marine engineer

## Technical Constraints

- Python 3.11+
- Use numpy/scipy for numerical computations
- Plotly for interactive visualizations
- Must generate HTML reports (no static images)
- Follow workspace-hub development workflow standards

## Timeline

- Phase 1: Configuration & pseudocode (1 week)
- Phase 2: Core analysis algorithms (2 weeks)
- Phase 3: Validation & testing (1 week)
- Phase 4: Documentation & peer review (1 week)

Total: ~5 weeks

## References

- ABS Rules for Building and Classing Steel Vessels
- DNV Classification Notes No. 30.1
- SNAME T&R Bulletin 5-5A (Intact Strength of Ships)
