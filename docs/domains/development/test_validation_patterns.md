# Test Validation Patterns - Digital Model

## Overview
This document provides templates and patterns for adding key output validations to tests to ensure algorithm changes require proper reconciliation.

---

## 🏗️ Engineering Domain Validation Patterns

### Pattern 1: Marine Hydrodynamics Validation
```python
def validate_rao_analysis(rao_data):
    """Validate Response Amplitude Operator results."""
    # Wave directions must span meaningful range
    assert len(rao_data['wave_directions']) >= 5, "Need minimum 5 wave directions"
    assert rao_data['wave_directions'][0] == 0, "First direction should be 0°"
    assert rao_data['wave_directions'][-1] <= 180, "Max direction should be ≤180°"
    
    # Frequency range validation for offshore structures
    freqs = rao_data['frequencies']
    assert all(0.05 <= f <= 2.0 for f in freqs), "Frequencies should be 0.05-2.0 Hz"
    
    # RAO magnitude validation
    raos = rao_data['response_amplitude_operators']
    for dof, values in raos.items():
        assert all(v >= 0 for v in values), f"{dof} RAOs cannot be negative"
        assert max(values) <= 5.0, f"{dof} RAO peak seems too high: {max(values)}"
        
    # DOF-specific validation
    if 'surge' in raos:
        assert max(raos['surge']) >= 1.0, "Surge should show some amplification"
    if 'heave' in raos:
        assert max(raos['heave']) >= 0.5, "Heave should show some response"
```

### Pattern 2: Structural Analysis Validation  
```python
def validate_structural_analysis(struct_data):
    """Validate structural analysis results."""
    # Stress validation
    stresses = struct_data['stresses']
    yield_strength = struct_data['material']['yield_strength']
    
    assert all(s >= 0 for s in stresses['von_mises']), "Von Mises stress cannot be negative"
    assert max(stresses['von_mises']) <= 2.0 * yield_strength, "Stress exceeds 2x yield"
    
    # Safety factor validation
    safety_factors = struct_data['safety_factors']
    assert all(sf >= 1.0 for sf in safety_factors), "Safety factors must be ≥ 1.0"
    assert min(safety_factors) >= 1.5, "Minimum safety factor should be 1.5"
    
    # Displacement validation
    displacements = struct_data['displacements']
    max_disp = max(abs(d) for d in displacements)
    structure_length = struct_data['geometry']['length']
    assert max_disp <= structure_length / 50, "Displacement seems excessive"
```

### Pattern 3: Cathodic Protection Validation
```python
def validate_cathodic_protection(cp_data):
    """Validate cathodic protection system design."""
    system = cp_data['protection_system']
    results = cp_data['analysis_results']
    
    # Current density validation (DNV standards)
    cd = system['current_density']
    assert 0.005 <= cd <= 0.15, f"Current density {cd} A/m² outside typical range"
    
    # Protection potential validation (vs Ag/AgCl reference)
    pot = results['protection_potential']
    assert -1.2 <= pot <= -0.7, f"Protection potential {pot}V outside DNV range"
    
    # Anode spacing validation
    spacing = system['anode_spacing'] 
    assert 10 <= spacing <= 200, f"Anode spacing {spacing}m seems unrealistic"
    
    # Cross-validation: current requirement vs protected area
    current_req = results['current_requirement']
    protected_area = current_req / cd
    assert 50 <= protected_area <= 10000, f"Protected area {protected_area}m² estimate"
```

### Pattern 4: Pipeline Analysis Validation
```python
def validate_pipeline_analysis(pipeline_data):
    """Validate pipeline analysis results."""
    # Pressure validation
    pressures = pipeline_data['pressures']
    assert all(p >= 0 for p in pressures), "Pressures cannot be negative"
    
    # Buckling validation
    if 'buckling' in pipeline_data:
        buckling = pipeline_data['buckling']
        assert buckling['critical_load'] > 0, "Critical buckling load must be positive"
        assert buckling['safety_factor'] >= 2.0, "Buckling safety factor should be ≥2.0"
        
    # Flow validation  
    if 'flow' in pipeline_data:
        flow = pipeline_data['flow']
        assert flow['velocity'] >= 0, "Flow velocity cannot be negative"
        assert flow['pressure_drop'] >= 0, "Pressure drop must be positive"
        
        # Reynolds number check
        if 'reynolds_number' in flow:
            re = flow['reynolds_number'] 
            assert 1000 <= re <= 1e8, f"Reynolds number {re} seems unrealistic"
```

---

## 📊 Statistical/Numerical Validation Patterns

### Pattern 5: Histogram Validation
```python
def validate_histogram(hist_data, original_dataset):
    """Validate histogram analysis results."""
    hist = hist_data['histogram']
    
    # Structure validation
    assert len(hist) > 0, "Histogram cannot be empty"
    assert all(freq >= 0 for freq in hist), "All frequencies must be non-negative"
    
    # Conservation validation
    total_freq = sum(hist)
    assert total_freq == len(original_dataset), "Total frequency must equal dataset size"
    
    # Distribution validation
    non_zero_bins = sum(1 for f in hist if f > 0)
    assert non_zero_bins >= 1, "Must have at least one populated bin"
    
    # Statistical reasonableness
    max_freq = max(hist)
    avg_freq = total_freq / len(hist)
    assert max_freq <= 3 * avg_freq, "Distribution seems too peaked"
```

### Pattern 6: Time Series Analysis Validation
```python
def validate_time_series_analysis(ts_data):
    """Validate time series analysis results."""
    # FFT validation
    if 'fft' in ts_data:
        fft = ts_data['fft']
        assert len(fft['frequencies']) == len(fft['amplitudes']), "FFT arrays must match"
        assert all(f >= 0 for f in fft['frequencies']), "Frequencies must be non-negative"
        assert all(a >= 0 for a in fft['amplitudes']), "Amplitudes must be non-negative"
        
    # Rainflow validation  
    if 'rainflow' in ts_data:
        rf = ts_data['rainflow']
        assert all(r >= 0 for r in rf['ranges']), "Rainflow ranges must be positive"
        assert all(c > 0 for c in rf['counts']), "Rainflow counts must be positive"
        
        # Half-cycles vs full-cycles relationship
        total_halfs = sum(rf['counts'])
        total_fulls = total_halfs / 2
        assert abs(total_fulls - int(total_fulls)) < 0.1, "Half/full cycle inconsistency"
```

---

## 🔧 Material Properties Validation

### Pattern 7: Material Validation
```python
def validate_material_properties(material):
    """Validate material property consistency."""
    # Strength validation
    tensile = material['tensile_strength'] 
    yield_str = material['yield_strength']
    assert 0.5 <= yield_str/tensile <= 0.95, "Yield/tensile ratio should be 0.5-0.95"
    
    # Modulus validation
    elastic_mod = material['elastic_modulus']
    assert 1e9 <= elastic_mod <= 1e12, "Elastic modulus should be 1-1000 GPa"
    
    # Density validation
    if 'density' in material:
        density = material['density']
        assert 500 <= density <= 20000, f"Density {density} kg/m³ seems unrealistic"
        
    # Fatigue properties
    if 'fatigue_limit' in material:
        fatigue_limit = material['fatigue_limit']
        assert fatigue_limit <= 0.6 * yield_str, "Fatigue limit should be < 0.6 * yield"
```

---

## 🌊 Geometry Validation Patterns

### Pattern 8: Umbilical/Riser Validation
```python
def validate_umbilical_properties(umbilical):
    """Validate umbilical/riser geometric and mechanical properties."""
    geom = umbilical['geometry']
    mech = umbilical['mechanical']
    
    # Geometric validation
    outer_dia = geom['outer_diameter']
    assert 0.02 <= outer_dia <= 1.0, f"Outer diameter {outer_dia}m seems unrealistic"
    
    # Mass validation
    mass_per_length = geom['mass_per_unit_length']
    assert 1.0 <= mass_per_length <= 200.0, f"Mass per length {mass_per_length} kg/m"
    
    # Stiffness validation
    axial_stiff = mech['axial_stiffness']
    bending_stiff = mech['bending_stiffness']
    assert axial_stiff > 0, "Axial stiffness must be positive"
    assert bending_stiff > 0, "Bending stiffness must be positive"
    
    # Cross-validation: mass vs outer diameter
    area_estimate = π * (outer_dia/2)**2
    density_estimate = mass_per_length / area_estimate
    assert 500 <= density_estimate <= 5000, "Effective density seems unrealistic"
```

---

## 🎯 Validation Implementation Templates

### Template 1: Test Enhancement Pattern
```python
def enhanced_test_function():
    """Template for enhancing existing tests with validation."""
    
    # 1. Setup mocks with realistic data
    with patch('module.function') as mock_func:
        mock_func.return_value = {
            'status': 'completed',
            'results': {
                # ... realistic mock data ...
            }
        }
        
        # 2. Execute test
        result = function_under_test(inputs)
        
        # 3. Basic assertions
        assert result is not None
        assert result['status'] == 'completed'
        
        # 4. KEY OUTPUT VALIDATIONS
        validate_domain_specific_output(result['results'])
        
        # 5. Cross-validation
        validate_result_consistency(result)
        
        return result

def validate_domain_specific_output(results):
    """Domain-specific validation function."""
    # Add specific validation logic here
    pass
    
def validate_result_consistency(result):
    """Cross-validation of related parameters."""
    # Add consistency checks here
    pass
```

### Template 2: New Test Creation Pattern  
```python
def test_new_feature_with_validation():
    """Template for creating new tests with built-in validation."""
    
    # Test setup
    test_inputs = create_test_inputs()
    expected_ranges = define_expected_ranges()
    
    # Execute
    result = feature_function(test_inputs)
    
    # Validate structure
    assert_result_structure(result)
    
    # Validate content
    assert_result_ranges(result, expected_ranges)
    
    # Validate physics/engineering
    assert_engineering_constraints(result)
    
    # Validate cross-relationships
    assert_parameter_consistency(result)

def assert_result_structure(result):
    """Validate result data structure."""
    required_keys = ['status', 'data', 'metadata']
    assert all(key in result for key in required_keys)
    
def assert_result_ranges(result, ranges):
    """Validate numerical results are in expected ranges."""
    for param, (min_val, max_val) in ranges.items():
        value = result['data'][param]
        assert min_val <= value <= max_val, f"{param}={value} outside range [{min_val}, {max_val}]"
        
def assert_engineering_constraints(result):
    """Validate engineering/physics constraints."""
    # Domain-specific engineering validation
    pass
    
def assert_parameter_consistency(result):
    """Validate consistency between related parameters."""  
    # Cross-parameter validation
    pass
```

---

## 🔄 Maintenance Guidelines

### When Algorithm Changes Occur:
1. **Run Full Test Suite**: Identify which validations fail
2. **Review Engineering Impact**: Determine if changes are physically reasonable  
3. **Update Validation Ranges**: Adjust ranges if algorithm improvements are valid
4. **Document Changes**: Record why validation ranges were updated
5. **Peer Review**: Have engineering team review validation changes

### Adding New Validations:
1. **Identify Key Outputs**: What are the critical results that must be validated?
2. **Define Realistic Ranges**: Based on engineering knowledge and standards
3. **Add Cross-Validations**: Ensure related parameters are consistent  
4. **Test Edge Cases**: Validate behavior at parameter limits
5. **Document Rationale**: Explain why specific ranges were chosen

### Validation Failure Response:
1. **Don't Immediately Update Ranges**: First understand why the failure occurred
2. **Check Algorithm Changes**: Were intentional changes made?
3. **Validate Physics**: Do new results make engineering sense?
4. **Consult Domain Experts**: Get engineering review of changes
5. **Update Ranges Carefully**: Only after confirming changes are valid

---

*This document ensures that when algorithms change, test failures force conscious review and reconciliation of engineering outputs.*