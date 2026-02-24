# OrcaFlex Integration Specification - OrcaWave Results Import

## Overview
Implementation of automated integration pipeline to import OrcaWave diffraction analysis results into OrcaFlex for time-domain dynamic analysis. This specification focuses on data conversion, vessel model setup, and validation of imported hydrodynamic coefficients.

## Business Requirements
- Import OrcaWave-generated hydrodynamic databases into OrcaFlex
- Automate vessel model creation with proper coefficients
- Validate imported data against OrcaWave outputs
- Enable seamless transition from frequency to time domain analysis
- Support batch processing for multiple vessels

## Technical Requirements

### Input Data Requirements
**OrcaWave Output Files:**
- Hydrodynamic database (`.yml` or `.dat` format)
- Added mass matrices (frequency-dependent)
- Radiation damping coefficients
- Wave excitation forces (Froude-Krylov + Diffraction)
- Phase information for all coefficients
- Metadata including mesh details and analysis parameters

### OrcaFlex Vessel Configuration

#### Hydrodynamic Data Import
```python
import OrcFxAPI as ofx
import yaml
import numpy as np
from pathlib import Path

class OrcaWaveIntegrator:
    def __init__(self, orcawave_results_dir: str):
        self.results_dir = Path(orcawave_results_dir)
        self.model = ofx.Model()
        
    def create_vessel(self, vessel_name: str) -> ofx.OrcaFlexObject:
        """Create vessel with imported hydrodynamics"""
        vessel = self.model.CreateObject(ofx.otVessel, vessel_name)
        
        # Set calculation method
        vessel.CalculationMethod = "Frequency dependent"
        vessel.PrimaryMotionIsTreatedAs = "Calculated (6 DOF)"
        
        # Load hydrodynamic database
        hyd_file = self.results_dir / f"{vessel_name}_hydrodynamics.yml"
        vessel.LoadHydrodynamicDatabase(str(hyd_file))
        
        return vessel
        
    def set_vessel_properties(self, vessel: ofx.OrcaFlexObject, 
                            mass: float, cog: tuple, gyradius: tuple):
        """Configure vessel physical properties"""
        vessel.Mass = mass
        vessel.CentreOfMassX, vessel.CentreOfMassY, vessel.CentreOfMassZ = cog
        vessel.RadiusOfGyrationX = gyradius[0]
        vessel.RadiusOfGyrationY = gyradius[1]
        vessel.RadiusOfGyrationZ = gyradius[2]
        
    def validate_raos(self, vessel: ofx.OrcaFlexObject) -> dict:
        """Calculate and validate Response Amplitude Operators"""
        rao_data = {}
        frequencies = vessel.HydrodynamicFrequencies
        directions = vessel.HydrodynamicDirections
        
        for freq in frequencies:
            for direction in directions:
                rao = vessel.CalculateRAO(freq, direction)
                rao_data[(freq, direction)] = rao
                
        return rao_data
```

### Data Mapping Structure

#### Coefficient Mapping
```yaml
mapping:
  added_mass:
    orcawave_format: "A_ij"  # 6x6 matrix
    orcaflex_format: "AddedMass"
    frequency_dependent: true
    units: 
      translational: "kg"
      rotational: "kg.m²"
      coupling: "kg.m"
      
  damping:
    orcawave_format: "B_ij"  # 6x6 matrix
    orcaflex_format: "RadiationDamping"
    frequency_dependent: true
    units:
      translational: "kg/s"
      rotational: "kg.m²/s"
      coupling: "kg.m/s"
      
  excitation:
    orcawave_format: "F_i"  # 6 DOF forces
    orcaflex_format: "WaveExcitationForce"
    frequency_dependent: true
    phase_included: true
    units:
      force: "N/m"  # per unit wave amplitude
      moment: "N.m/m"
      
  qtf:  # Quadratic Transfer Functions (if available)
    orcawave_format: "QTF_ij"
    orcaflex_format: "QuadraticTransferFunction"
    types: ["sum", "difference"]
```

### Validation Framework

#### Validation Checks
1. **Conservation Checks**
   - Symmetry of added mass matrix
   - Positive definiteness of damping matrix
   - Reciprocity relations

2. **Frequency Range Validation**
   - Sufficient low-frequency coverage
   - High-frequency asymptotic behavior
   - Interpolation stability

3. **RAO Validation**
   - Physical realizability
   - Peak response frequencies
   - Comparison with OrcaWave predictions

#### Validation Script
```python
class HydrodynamicValidator:
    def __init__(self, tolerance: float = 1e-6):
        self.tolerance = tolerance
        
    def check_symmetry(self, matrix: np.ndarray) -> bool:
        """Verify matrix symmetry"""
        return np.allclose(matrix, matrix.T, atol=self.tolerance)
        
    def check_positive_definite(self, matrix: np.ndarray) -> bool:
        """Check positive definiteness of damping"""
        eigenvalues = np.linalg.eigvals(matrix)
        return np.all(eigenvalues > -self.tolerance)
        
    def validate_frequency_range(self, frequencies: list) -> dict:
        """Check frequency coverage"""
        min_freq = min(frequencies)
        max_freq = max(frequencies)
        spacing = np.diff(frequencies)
        
        return {
            'min_frequency': min_freq,
            'max_frequency': max_freq,
            'uniform_spacing': np.allclose(spacing, spacing[0]),
            'coverage_adequate': min_freq < 0.05 and max_freq > 2.0
        }
```

## System Architecture

### Integration Pipeline
```
OrcaWave Results → Data Parser → Coefficient Mapper → OrcaFlex Importer → Validation
       ↓               ↓              ↓                    ↓                ↓
   (YML/CSV)      (Structure)    (Transform)         (API Import)      (Checks)
```

### Component Responsibilities
1. **Data Parser** - Reads OrcaWave output formats
2. **Coefficient Mapper** - Transforms to OrcaFlex structure
3. **OrcaFlex Importer** - Uses API for database import
4. **Validator** - Ensures data integrity and physics

## Implementation Approach

### Phase 1: Data Import Pipeline
- Parse OrcaWave output files
- Structure hydrodynamic coefficients
- Handle frequency-dependent data
- Manage phase information

### Phase 2: OrcaFlex Model Creation
- Initialize vessel object
- Import hydrodynamic database
- Set vessel properties
- Configure calculation methods

### Phase 3: Validation Suite
- Implement conservation checks
- Verify RAO calculations
- Compare with benchmarks
- Generate validation reports

### Phase 4: Automation Framework
- Batch processing capability
- Error handling and recovery
- Progress monitoring
- Result archiving

## Testing Strategy

### Unit Tests
- Data parsing functions
- Coefficient transformations
- API interactions
- Validation algorithms

### Integration Tests
- End-to-end import workflow
- Multi-vessel processing
- Error recovery scenarios
- Performance benchmarks

### Validation Tests
- Physics conservation
- Numerical stability
- RAO accuracy
- Force balance verification

## Performance Requirements
- Import processing: &lt; 5 minutes per vessel
- Memory usage: &lt; 4 GB
- Validation suite: &lt; 2 minutes
- Batch capacity: 10+ vessels

## Documentation Requirements
- Integration user guide
- API reference documentation
- Validation report templates
- Troubleshooting handbook

## Success Criteria
1. ✅ Automated import from OrcaWave to OrcaFlex
2. ✅ 100% data fidelity maintained
3. ✅ Validation suite passes all checks
4. ✅ RAOs match within 5% tolerance
5. ✅ Batch processing operational
6. ✅ Complete traceability maintained

## Dependencies
- OrcaFlex v11.0+ with API license
- Python 3.8+ with OrcFxAPI package
- OrcaWave results in compatible format
- Sufficient memory for large databases

## Risk Mitigation
- **Format Incompatibility**: Multiple parser implementations
- **Data Loss**: Comprehensive validation checks
- **Performance Issues**: Optimized data structures
- **Version Conflicts**: Compatibility layer design

## Future Enhancements
- Direct OrcaWave-OrcaFlex API bridge
- Real-time synchronization
- Cloud-based processing
- Machine learning validation
- Automated optimization loops