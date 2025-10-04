# Marine Engineering Integration Test Suite

Comprehensive integration tests validating cross-module communication and data flow for Phase 1 and Phase 2 modules.

## Overview

This test suite validates the integration of:
1. **Wave Spectra → Ship Dynamics** (motion analysis)
2. **Hydrodynamic Coefficients → RAO Processing**
3. **OCIMF Environmental Loading → Mooring Analysis**
4. **Catenary → Mooring Analysis**
5. **All modules → OrcaFlex Integration**

## Test Structure

```
integration/
├── __init__.py
├── test_wave_dynamics_integration.py       # 10+ tests for wave-dynamics integration
├── test_hydro_rao_integration.py          # 8+ tests for hydro-RAO integration
├── test_ocimf_mooring_integration.py      # 12+ tests for OCIMF-mooring integration
├── test_end_to_end_workflow.py            # 5+ complete workflow scenarios
├── test_performance_benchmarks.py         # Performance and timing tests
├── charts/                                # Generated validation charts
│   ├── wave_dynamics/
│   ├── hydro_rao/
│   ├── ocimf_mooring/
│   ├── end_to_end/
│   └── performance/
└── README.md                              # This file
```

## Running the Tests

### Run All Integration Tests
```bash
pytest tests/marine_engineering/integration/ -v
```

### Run Specific Test Module
```bash
# Wave-dynamics integration only
pytest tests/marine_engineering/integration/test_wave_dynamics_integration.py -v

# Hydro-RAO integration only
pytest tests/marine_engineering/integration/test_hydro_rao_integration.py -v

# OCIMF-mooring integration only
pytest tests/marine_engineering/integration/test_ocimf_mooring_integration.py -v

# End-to-end workflows only
pytest tests/marine_engineering/integration/test_end_to_end_workflow.py -v

# Performance benchmarks only
pytest tests/marine_engineering/integration/test_performance_benchmarks.py -v
```

### Generate Coverage Report
```bash
pytest tests/marine_engineering/integration/ --cov=marine_engineering --cov-report=html
```

## Test Categories

### 1. Wave-Dynamics Integration (10+ tests)

**Module:** `test_wave_dynamics_integration.py`

Tests that wave spectra calculations correctly feed into motion analysis:
- JONSWAP spectrum generates valid spectral density
- Spectral moments calculated correctly
- Significant wave height (Hs) recoverable from spectrum
- Zero-crossing period (Tz) vs peak period (Tp) relationship
- Spectral bandwidth in valid range
- Wave elevation time series generation
- Frequency response peaks at natural period
- Peak period affects motion amplitudes
- Wave energy distribution
- Complete spectral statistics

**Key Validations:**
- Spectrum should be non-negative
- Peak frequency matches Tp
- Hs recovery within 1%
- Tz/Tp ratio: 0.6-0.8 for JONSWAP
- Bandwidth: 0.5-0.9 for typical seas
- Response peaks near natural frequency

### 2. Hydro-RAO Integration (8+ tests)

**Module:** `test_hydro_rao_integration.py`

Tests that hydrodynamic coefficients correctly feed into RAO calculations:
- Database loading and validation
- Coefficient interpolation accuracy
- Added mass affects natural frequency
- Damping affects response amplitude
- Damping affects phase angle
- Coupling terms (heave-pitch, sway-roll)
- Full 6×6 matrix interpolation
- Frequency sweep for all DOF

**Key Validations:**
- Interpolation error < 10%
- Natural frequency increases with wave frequency
- Response peaks near natural frequency
- Phase transitions through -90° at resonance
- Matrix symmetry maintained
- Diagonal dominance preserved

### 3. OCIMF-Mooring Integration (12+ tests)

**Module:** `test_ocimf_mooring_integration.py`

Tests that environmental forces feed into mooring tension calculations:
- Wind force calculation
- Current force calculation
- Combined wind+current forces
- Heading variation effects
- Environmental forces → mooring tensions
- Combined loading worst-case scenarios
- Force balance validation
- Displacement effects on forces
- Mooring tension statistics
- Exceedance probability analysis

**Key Validations:**
- Force magnitudes reasonable
- Total = wind + current
- Peak lateral force at beam seas (90°)
- Force balance maintained
- Forces vary with displacement
- Maximum line tension > 0.5 MN for severe conditions

### 4. End-to-End Workflows (5+ tests)

**Module:** `test_end_to_end_workflow.py`

Complete workflows from environmental definition to mooring analysis:
- Complete mooring analysis (waves + vessel + mooring)
- Multi-directional environmental analysis
- Design condition envelope
- OrcaFlex export preparation
- Comprehensive workflow validation

**Key Validations:**
- All workflow steps complete without errors
- Data integrity preserved across modules
- Results physically reasonable
- OrcaFlex export format valid

### 5. Performance Benchmarks (5+ tests)

**Module:** `test_performance_benchmarks.py`

Performance and scalability validation:
- Wave spectrum computation performance
- OCIMF database query performance
- Complete workflow performance
- Scalability with problem size
- Memory efficiency
- Parallel execution potential

**Performance Targets:**
- Wave spectrum (2000 freqs): < 100ms
- OCIMF query: < 1ms
- Complete workflow mean: < 1s
- Complete workflow 95th percentile: < 5s
- Memory growth: < 50MB for 100 runs
- Sub-linear scaling with problem size

## Generated Charts

Each test module generates validation charts:

### Wave-Dynamics Charts
- `peak_period_effect.png` - Motion response vs wave period
- `energy_distribution.png` - Wave energy across frequencies
- `spectral_statistics.png` - Complete spectral analysis

### Hydro-RAO Charts
- `added_mass_natural_frequency.png` - Added mass frequency dependence
- `rao_amplitude.png` - Response amplitude operator
- `rao_phase.png` - Phase response
- `coefficient_matrices_heatmap.png` - 6×6 coefficient matrices
- `all_dof_frequency_sweep.png` - All degrees of freedom

### OCIMF-Mooring Charts
- `heading_variation.png` - Forces vs vessel heading
- `mooring_tensions.png` - Line tension distribution
- `worst_case_scenarios.png` - Top 10 worst loading cases
- `displacement_effect.png` - Force vs displacement
- `mooring_tension_statistics.png` - Statistical distribution

### End-to-End Charts
- `workflow_summary.png` - Complete workflow visualization
- `multi_directional_analysis.png` - Forces vs direction
- `design_envelope.png` - Design condition matrix
- `mooring_layout.png` - Mooring system arrangement

### Performance Charts
- `wave_spectrum_performance.png` - Computation time scaling
- `ocimf_performance.png` - Database query performance
- `workflow_performance.png` - Complete workflow timing
- `scalability.png` - Scaling with problem size
- `memory_efficiency.png` - Memory usage tracking
- `parallel_potential.png` - Parallel speedup potential

## Success Criteria

All tests must meet these criteria:

### Accuracy
- Data transfers preserve accuracy (±1%)
- Physical constraints satisfied
- Conservation laws maintained
- Industry standard compliance

### Performance
- Complete workflow < 5s (95th percentile)
- Individual modules < 1s
- Database queries < 1ms
- Memory efficient (< 50MB growth)

### Integration
- Cross-module data flow validated
- No data loss or corruption
- Consistent units across modules
- Proper error handling

### Visualization
- All integration points charted
- Results clearly presented
- Validation against benchmarks
- Professional quality output

## Dependencies

Required packages:
```
pytest >= 7.0
numpy >= 1.21
scipy >= 1.7
matplotlib >= 3.5
pandas >= 1.3
psutil >= 5.8
```

## Example Usage

```python
# Import test classes
from test_wave_dynamics_integration import TestWaveDynamicsIntegration
from test_hydro_rao_integration import TestHydroRAOIntegration
from test_ocimf_mooring_integration import TestOCIMFMooringIntegration

# Run specific test
test = TestWaveDynamicsIntegration()
test.test_jonswap_generates_valid_spectrum()

# Or run via pytest
pytest.main(['-v', 'test_wave_dynamics_integration.py::TestWaveDynamicsIntegration::test_jonswap_generates_valid_spectrum'])
```

## Integration with CI/CD

Add to `.github/workflows/tests.yml`:
```yaml
- name: Run Integration Tests
  run: |
    pytest tests/marine_engineering/integration/ -v --cov=marine_engineering --cov-report=xml

- name: Upload Coverage
  uses: codecov/codecov-action@v3
  with:
    file: ./coverage.xml
```

## Troubleshooting

### Common Issues

**Issue:** `ImportError: No module named 'marine_engineering'`
**Solution:** Ensure package is installed: `pip install -e .`

**Issue:** `FileNotFoundError: Chart directory not found`
**Solution:** Test creates directories automatically. Check write permissions.

**Issue:** Tests timeout on slow machines
**Solution:** Increase timeout: `pytest --timeout=300`

**Issue:** Memory errors during large tests
**Solution:** Run tests individually or increase system memory

## Contributing

When adding new integration tests:

1. Follow existing test structure
2. Include comprehensive assertions
3. Generate validation charts
4. Document expected behavior
5. Add performance benchmarks
6. Update this README

## References

### Standards
- DNV-RP-C205: Environmental conditions and environmental loads
- API RP 2A: Wave spectrum models
- ISO 19901-1: Metocean design and operating considerations
- OCIMF MEG4: Mooring Equipment Guidelines

### Code Style
- Follow existing patterns
- Use descriptive test names
- Include docstrings
- Comment complex logic

## License

Copyright (c) 2025 Digital Model Project
See LICENSE file for details
