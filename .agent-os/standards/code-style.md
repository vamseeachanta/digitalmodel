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

### Version Control
- Make atomic commits
- Write descriptive commit messages
- Reference issue numbers when applicable
- Keep commits focused on single changes