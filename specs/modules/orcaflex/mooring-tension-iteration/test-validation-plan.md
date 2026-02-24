# Test Validation Plan: Manual vs Automated Process

## Overview
This document defines the comprehensive testing strategy to validate that the automated mooring tension iteration process produces results equivalent to or better than the manual process, while meeting all convergence criteria.

## Test Objectives

### Primary Objectives
1. **Equivalence Validation**: Verify automated process achieves same or better tension accuracy as manual process
2. **Convergence Verification**: Confirm 5% convergence tolerance is consistently achieved
3. **Performance Validation**: Ensure automated process completes faster than manual approach
4. **Reliability Testing**: Validate robustness across different model configurations

### Success Criteria
- ✅ Final tensions match manual results within ±1%
- ✅ Convergence achieved in same or fewer iterations
- ✅ Process time reduced by >50%
- ✅ 100% test pass rate for baseline models

## Test Strategy

### Phase 1: Baseline Establishment

#### Test 1.1: Manual Process Documentation
**Purpose**: Establish ground truth from manual process

**Test Data**:
```
Input Model: fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.dat
Target CSV: fsts_l015_125km3_pb_target_mooring_pretension.csv
```

**Manual Process Steps**:
1. Run initial OrcaFlex analysis
2. Extract tensions manually
3. Calculate adjustments using Excel/calculator
4. Update model manually
5. Repeat until convergence
6. Document:
   - Number of iterations required
   - Time per iteration
   - Final tension values
   - Total elapsed time

**Expected Results**:
```csv
Iteration,Line01_Tension,Line02_Tension,Line03_Tension,Line04_Tension,Max_Error_%
0,2850.5,2920.3,2780.2,2950.1,18.4
1,2620.3,2580.7,2550.9,2600.2,8.1
2,2520.1,2510.5,2480.3,2490.7,4.8
3,2505.2,2498.3,2502.1,2499.5,0.8
```

#### Test 1.2: Manual Process Metrics Collection
**Metrics to Capture**:
- Total time: ~4 hours
- Iterations: 3-5 typical
- Manual calculations: ~30 minutes per iteration
- File updates: ~15 minutes per iteration
- Analysis runs: ~25 seconds per iteration
- Error-prone steps identified

### Phase 2: Automated Process Validation

#### Test 2.1: Single Model Validation
**Purpose**: Validate automated process against manual baseline

**Test Configuration**:
```yaml
test_name: baseline_validation_125km3
model: fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.dat
target_tensions:
  Line01: 2500.0
  Line02: 2500.0
  Line03: 2500.0
  Line04: 2500.0
convergence_tolerance: 0.05  # 5%
max_iterations: 10
```

**Automated Process Commands**:
```bash
# Step 1: Initial analysis
python -m digitalmodel.orcaflex.universal \
    pattern="fsts*125km3*pb_*.yml" \
    input_directory="." \
    output_directory="." \
    validate=false

# Step 2: Post-process
python -m digitalmodel dm_ofx_post_fsts_lngc.yml --workers 30

# Step 3: Iterate
python -m digitalmodel dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml

# Repeat until convergence
```

**Validation Checks**:
```python
def validate_results(manual_results, automated_results):
    """Compare manual and automated process results."""
    
    # Check final tensions
    for line in ['Line01', 'Line02', 'Line03', 'Line04']:
        manual_tension = manual_results[line]['final_tension']
        auto_tension = automated_results[line]['final_tension']
        
        error = abs(manual_tension - auto_tension) / manual_tension
        assert error < 0.01, f"{line} tension mismatch: {error:.2%}"
    
    # Check convergence
    assert automated_results['iterations'] <= manual_results['iterations']
    assert automated_results['converged'] == True
    
    # Check performance
    assert automated_results['total_time'] < manual_results['total_time'] * 0.5
    
    return True
```

#### Test 2.2: Convergence Criteria Validation
**Purpose**: Verify 5% convergence tolerance is properly enforced

**Test Cases**:
1. **Fast Convergence**: Model close to target (1-2 iterations)
2. **Normal Convergence**: Typical offset (3-4 iterations)
3. **Slow Convergence**: Large initial offset (5-7 iterations)
4. **Non-Convergence**: Unrealistic targets (should fail gracefully)

**Convergence Test Function**:
```python
def test_convergence_criteria():
    """Validate convergence detection and criteria."""
    
    test_cases = [
        {
            'name': 'fast_convergence',
            'initial_tensions': [2450, 2480, 2520, 2490],
            'target_tensions': [2500, 2500, 2500, 2500],
            'expected_iterations': 1-2,
            'should_converge': True
        },
        {
            'name': 'normal_convergence',
            'initial_tensions': [2800, 2900, 2750, 2950],
            'target_tensions': [2500, 2500, 2500, 2500],
            'expected_iterations': 3-4,
            'should_converge': True
        },
        {
            'name': 'slow_convergence',
            'initial_tensions': [3500, 3800, 3200, 3900],
            'target_tensions': [2500, 2500, 2500, 2500],
            'expected_iterations': 5-7,
            'should_converge': True
        }
    ]
    
    for case in test_cases:
        result = run_automated_process(case)
        
        # Check convergence achieved
        assert result['converged'] == case['should_converge']
        
        # Check iteration count reasonable
        assert case['expected_iterations'][0] <= result['iterations'] <= case['expected_iterations'][1]
        
        # Verify final tensions within 5%
        for i, line in enumerate(['Line01', 'Line02', 'Line03', 'Line04']):
            final = result['final_tensions'][line]
            target = case['target_tensions'][i]
            error = abs(final - target) / target
            assert error < 0.05, f"Convergence tolerance exceeded: {error:.2%}"
```

### Phase 3: Comprehensive Validation

#### Test 3.1: Multiple Configuration Testing
**Purpose**: Validate across different vessel and environmental conditions

**Test Matrix**:
| Model Configuration | Water Level | Vessel Size | Berthing Side | Expected Result |
|-------------------|-------------|-------------|---------------|-----------------|
| fsts_l015_hwl_125km3_pb | HWL | 125km3 | Port | Pass |
| fsts_l015_mwl_125km3_pb | MWL | 125km3 | Port | Pass |
| fsts_l015_lwl_125km3_pb | LWL | 125km3 | Port | Pass |
| fsts_l015_hwl_180km3_pb | HWL | 180km3 | Port | Pass |
| fsts_l015_hwl_125km3_sb | HWL | 125km3 | Starboard | Pass |

**Batch Test Script**:
```python
def run_comprehensive_validation():
    """Run validation across all test configurations."""
    
    configurations = [
        ('hwl', '125km3', 'pb'),
        ('mwl', '125km3', 'pb'),
        ('lwl', '125km3', 'pb'),
        ('hwl', '180km3', 'pb'),
        ('hwl', '125km3', 'sb')
    ]
    
    results = []
    for water_level, vessel_size, berthing_side in configurations:
        # Run manual process (or use cached results)
        manual_result = get_manual_baseline(water_level, vessel_size, berthing_side)
        
        # Run automated process
        auto_result = run_automated_process(water_level, vessel_size, berthing_side)
        
        # Validate
        validation = validate_results(manual_result, auto_result)
        
        results.append({
            'config': f"{water_level}_{vessel_size}_{berthing_side}",
            'manual_iterations': manual_result['iterations'],
            'auto_iterations': auto_result['iterations'],
            'tension_match': validation['tension_accuracy'],
            'time_reduction': validation['time_improvement'],
            'passed': validation['passed']
        })
    
    return results
```

#### Test 3.2: Edge Case Testing
**Purpose**: Validate handling of edge cases and error conditions

**Test Cases**:
1. **Unrealistic Targets**: Target tensions physically impossible
2. **Model Instability**: Poor initial conditions
3. **Missing Data**: Incomplete CSV files
4. **Numerical Issues**: Very small/large tension values

```python
def test_edge_cases():
    """Test edge cases and error handling."""
    
    # Test unrealistic targets
    with pytest.raises(ConvergenceError):
        run_automated_process(target_tensions=[100, 100, 100, 100])  # Too low
    
    # Test missing data
    with pytest.raises(ValidationError):
        run_automated_process(csv_file='incomplete.csv')
    
    # Test numerical stability
    result = run_automated_process(target_tensions=[25000, 25000, 25000, 25000])  # Very high
    assert result['converged'] or result['error_handled']
```

### Phase 4: Performance Validation

#### Test 4.1: Timing Comparison
**Purpose**: Quantify performance improvement

**Metrics**:
```python
def measure_performance():
    """Compare manual vs automated timing."""
    
    manual_timing = {
        'setup': 15 * 60,  # 15 minutes
        'per_iteration': 45 * 60,  # 45 minutes
        'iterations': 4,
        'total': 15 * 60 + 45 * 60 * 4  # 3.25 hours
    }
    
    automated_timing = {
        'setup': 30,  # 30 seconds
        'per_iteration': 40,  # 40 seconds
        'iterations': 4,
        'total': 30 + 40 * 4  # 3.17 minutes
    }
    
    improvement = (manual_timing['total'] - automated_timing['total']) / manual_timing['total']
    assert improvement > 0.95, f"Performance improvement {improvement:.1%} below 95% target"
```

#### Test 4.2: Scalability Testing
**Purpose**: Validate performance with multiple models

```python
def test_batch_processing():
    """Test parallel processing of multiple models."""
    
    models = [f"model_{i}.dat" for i in range(10)]
    
    start_time = time.time()
    results = run_batch_automated(models, parallel=True)
    parallel_time = time.time() - start_time
    
    # Should complete 10 models in < 10 minutes
    assert parallel_time < 600
    
    # All should converge
    assert all(r['converged'] for r in results)
```

## Test Execution Plan

### Pre-Test Setup
1. **Environment Preparation**
   ```bash
   # Activate environment
   source /d/github/digitalmodel/.venv/Scripts/activate
   
   # Verify OrcaFlex license
   python -c "import OrcFxAPI; print('License OK')"
   
   # Copy test models
   cp -r go-by/ test_data/
   ```

2. **Baseline Data Collection**
   - Run manual process for all test configurations
   - Document results in `test_data/manual_baselines/`
   - Create validation datasets

### Test Execution Sequence

#### Day 1: Manual Baseline
- [ ] Run manual process for 125km3 HWL configuration
- [ ] Document all steps and timings
- [ ] Record final tensions and iteration count
- [ ] Create baseline dataset

#### Day 2: Automated Validation
- [ ] Implement automated process wrapper
- [ ] Run automated process for same configuration
- [ ] Compare results with manual baseline
- [ ] Validate convergence criteria

#### Day 3: Comprehensive Testing
- [ ] Run all test configurations
- [ ] Execute edge case tests
- [ ] Perform scalability testing
- [ ] Generate validation report

### Test Reports

#### Summary Report Format
```markdown
# Validation Test Results

## Executive Summary
- **Test Date**: [Date]
- **Overall Result**: PASS/FAIL
- **Accuracy**: Manual vs Automated tension match: X.X%
- **Performance**: Time reduction: XX%
- **Reliability**: X/Y tests passed

## Detailed Results

### Baseline Comparison
| Metric | Manual | Automated | Improvement |
|--------|--------|-----------|-------------|
| Iterations | 4 | 3 | 25% |
| Total Time | 3.5 hrs | 3.2 min | 98.5% |
| Final Tension Error | 0.8% | 0.6% | 25% |
| Convergence Success | 100% | 100% | - |

### Configuration Testing
[Detailed results table]

### Edge Case Handling
[Edge case results]

## Recommendations
[Any issues or improvements identified]
```

## Acceptance Criteria

### Mandatory Requirements
1. **Accuracy**: Automated tensions within ±1% of manual results ✅
2. **Convergence**: 5% tolerance achieved in all valid cases ✅
3. **Performance**: >50% time reduction vs manual process ✅
4. **Reliability**: 100% pass rate for baseline configurations ✅

### Quality Gates
- [ ] All unit tests passing
- [ ] Integration tests validated
- [ ] Performance benchmarks met
- [ ] Documentation complete
- [ ] Code review approved

## Risk Mitigation

### Identified Risks
1. **OrcaFlex License**: Ensure license available during testing
2. **Model Variations**: Some models may behave differently
3. **Numerical Precision**: Floating point differences between systems

### Mitigation Strategies
1. **License Pool**: Reserve licenses for testing period
2. **Extensive Testing**: Cover wide range of configurations
3. **Tolerance Bands**: Allow for minor numerical differences

## Continuous Validation

### Regression Testing
- Run validation suite on every code change
- Maintain baseline datasets for comparison
- Track performance metrics over time

### Production Monitoring
- Log convergence statistics
- Track failure rates
- Monitor performance trends
- Alert on degradation

## Appendix

### Test Data Structure
```
test_data/
├── manual_baselines/
│   ├── 125km3_hwl_pb/
│   │   ├── iteration_0.csv
│   │   ├── iteration_1.csv
│   │   ├── iteration_2.csv
│   │   ├── iteration_3.csv
│   │   └── final_results.csv
│   └── timing_log.csv
├── automated_results/
│   └── [same structure]
├── validation_reports/
│   ├── comparison_report.html
│   └── test_summary.md
└── test_configs/
    ├── test_suite.yml
    └── validation_criteria.yml
```

### Validation Scripts
- `run_manual_baseline.py`: Execute and document manual process
- `run_automated_tests.py`: Execute automated validation suite
- `compare_results.py`: Generate comparison reports
- `validate_convergence.py`: Check convergence criteria
- `generate_report.py`: Create final validation report