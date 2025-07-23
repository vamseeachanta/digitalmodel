# Code Style Guide

## Python Code Standards

### General Principles
- Write clear, self-documenting code
- Prefer readability over cleverness
- Follow PEP 8 with project-specific exceptions
- Use type hints for all public functions

### Naming Conventions

#### Variables and Functions
- Use `snake_case` for variables and functions
- Be descriptive: `calculate_von_mises_stress` not `calc_vm`
- Boolean variables should ask a question: `is_valid`, `has_data`

#### Classes
- Use `PascalCase` for class names
- Suffix with type when appropriate: `OrcaFlexPostProcess`, `AQWAReader`

#### Constants
- Use `UPPER_SNAKE_CASE` for module-level constants
- Group related constants in classes or enums

### Code Organization

#### Imports
```python
# Standard library imports
import os
import sys
from typing import Dict, List, Optional

# Third party imports
import numpy as np
import pandas as pd

# Local imports
from digitalmodel.core import Engine
from digitalmodel.utils import calculate_stress
```

#### Function Structure
```python
def process_simulation_file(
    file_path: str,
    config: Dict[str, Any],
    validate: bool = True
) -> pd.DataFrame:
    """Process a single simulation file and return results.
    
    Args:
        file_path: Path to the simulation file
        config: Configuration dictionary
        validate: Whether to validate input data
        
    Returns:
        DataFrame containing processed results
        
    Raises:
        FileNotFoundError: If simulation file doesn't exist
        ValueError: If configuration is invalid
    """
    # Implementation
```

### Engineering-Specific Standards

#### Units
- Always document units in comments or docstrings
- Use SI units internally, convert at I/O boundaries
- Example: `force_kN = 1000.0  # Force in kilonewtons`

#### Coordinate Systems
- Document coordinate system orientation
- Use consistent naming: `x`, `y`, `z` or `north`, `east`, `down`

#### Numerical Precision
- Use appropriate precision for engineering calculations
- Document tolerance values
- Handle numerical edge cases explicitly

### Error Handling

```python
try:
    result = perform_calculation(data)
except CalculationError as e:
    logger.error(f"Calculation failed: {e}")
    # Provide meaningful context
    raise CalculationError(
        f"Failed to process {file_name}: {e}"
    ) from e
```

### Documentation

#### Module Level
```python
"""Module for OrcaFlex post-processing operations.

This module provides functionality for extracting and analyzing
results from OrcaFlex simulation files.
"""
```

#### Class Level
```python
class ResultProcessor:
    """Process and aggregate simulation results.
    
    Attributes:
        config: Configuration dictionary
        results: Accumulated results DataFrame
    """
```

### Testing Standards
- Write tests alongside code development
- Test function names should describe what they test
- Use fixtures for common test data
- Aim for descriptive assertions

### Performance Considerations
- Profile before optimizing
- Document performance-critical sections
- Use appropriate data structures
- Consider memory usage for large datasets

### Configuration Patterns

#### YAML Configuration Standards
- Use YAML for all configuration files
- Place base configs in `src/digitalmodel/base_configs/modules/`
- Follow consistent naming: `module_analysis.yml`
- Validate configuration schemas on load

```python
import yaml
from typing import Dict, Any

def load_config(config_path: str) -> Dict[str, Any]:
    """Load and validate YAML configuration."""
    with open(config_path, 'r') as f:
        config = yaml.safe_load(f)
    validate_config_schema(config)
    return config
```

#### Configuration Organization
- **Base configs**: Reusable templates in `base_configs/modules/`
- **User configs**: Project-specific YAML files
- **Template configs**: Standard patterns for common analyses

### Advanced Error Handling

#### Specific Exception Types
```python
class AnalysisError(Exception):
    """Base class for analysis errors."""
    pass

class ConfigurationError(AnalysisError):
    """Configuration validation errors."""
    pass

class CalculationError(AnalysisError):
    """Calculation and numerical errors."""
    pass
```

#### Engineering Error Context
```python
def validate_stress_input(stress_value: float) -> None:
    """Validate stress input with engineering constraints."""
    if stress_value < 0:
        raise CalculationError(
            f"Stress cannot be negative: {stress_value} Pa. "
            "Check input data or calculation method."
        )
    if stress_value > 1e9:  # 1 GPa reasonable upper limit
        raise CalculationError(
            f"Stress value unusually high: {stress_value} Pa. "
            "Verify units and input data."
        )
```

### Integration Patterns

#### External Software Integration
```python
class OrcaFlexAdapter:
    """Adapter pattern for OrcaFlex API integration."""
    
    def __init__(self, mock_mode: bool = False):
        if mock_mode or not self._orcaflex_available():
            self.api = MockOrcFxAPI()
        else:
            import OrcFxAPI
            self.api = OrcFxAPI
    
    def _orcaflex_available(self) -> bool:
        """Check if OrcaFlex license is available."""
        try:
            import OrcFxAPI
            return True
        except ImportError:
            return False
```

#### Mock API Patterns
```python
class MockOrcFxAPI:
    """Mock OrcaFlex API for testing without license."""
    
    class Model:
        def __init__(self, *args, **kwargs):
            self.general = MockGeneral()
            self.simulationComplete = True
            
        def LoadData(self, *args, **kwargs):
            pass
            
        def RunSimulation(self, *args, **kwargs):
            pass
```

### Engineering Domain Standards

#### Offshore Engineering Conventions
- Follow API, DNV, ABS naming standards
- Use industry-standard coordinate systems
- Include references to relevant codes in comments
- Maintain traceability to engineering specifications

#### Units and Physical Constraints
```python
def calculate_von_mises_stress(
    sigma_x: float,  # Pa - normal stress in x direction
    sigma_y: float,  # Pa - normal stress in y direction  
    tau_xy: float    # Pa - shear stress in xy plane
) -> float:
    """Calculate von Mises equivalent stress.
    
    Reference: API RP 2RD Section 5.3.2
    
    Args:
        sigma_x: Normal stress in x direction (Pa)
        sigma_y: Normal stress in y direction (Pa)
        tau_xy: Shear stress in xy plane (Pa)
        
    Returns:
        Von Mises equivalent stress (Pa)
        
    Raises:
        ValueError: If any stress component is physically unreasonable
    """
    # Validate physical constraints
    max_reasonable_stress = 1e9  # 1 GPa
    for stress, name in [(sigma_x, 'sigma_x'), (sigma_y, 'sigma_y'), (tau_xy, 'tau_xy')]:
        if abs(stress) > max_reasonable_stress:
            raise ValueError(f"{name} = {stress} Pa exceeds reasonable limit")
    
    # Calculate von Mises stress
    vm_stress = math.sqrt(sigma_x**2 - sigma_x*sigma_y + sigma_y**2 + 3*tau_xy**2)
    return vm_stress
```

### Testing Standards Enhancement

#### Configuration Testing
```python
def test_config_validation():
    """Test configuration validation with engineering constraints."""
    invalid_config = {
        "analysis": {
            "material_strength": -1000,  # Negative strength invalid
            "safety_factor": 0.5         # Below minimum safety factor
        }
    }
    
    with pytest.raises(ConfigurationError, match="Material strength cannot be negative"):
        validate_config(invalid_config)
```

#### Mock External Dependencies
```python
@pytest.fixture
def mock_orcaflex(monkeypatch):
    """Mock OrcaFlex API for testing without license."""
    mock_api = MockOrcFxAPI()
    monkeypatch.setattr("sys.modules['OrcFxAPI']", mock_api)
    return mock_api
```

### Version Control
- Make atomic commits
- Write descriptive commit messages
- Reference issue numbers when applicable
- Keep commits focused on single changes