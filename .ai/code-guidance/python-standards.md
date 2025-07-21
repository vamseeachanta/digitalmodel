# Python Code Standards

## Code Style
- Use `black` for formatting
- Use `isort` for import sorting with black profile
- Follow PEP 8 guidelines
- Line length: 88 characters (black default)

## Type Hints
- Use type hints for all public functions and methods
- Use `mypy` for static type checking
- Import types from `typing` module when needed

## Testing Standards
- Use `pytest` as testing framework
- Test file naming: `test_[module]_[feature].py`
- Organize tests by module in `tests/modules/`
- Aim for high test coverage
- Use mock APIs for external dependencies (especially OrcaFlex)

## Import Organization
```python
# Standard library imports
import os
import sys

# Third party imports
import pandas as pd
import numpy as np

# Local imports
from digitalmodel.common import utilities
from digitalmodel.modules.orcaflex import analysis
```

## Configuration Patterns
- Use YAML for configuration files
- Place configs in `src/digitalmodel/base_configs/modules/`
- Load configs using consistent patterns
- Validate configuration schemas

## Error Handling
- Use specific exception types
- Provide meaningful error messages
- Log errors appropriately using loguru
- Fail fast with clear error context

## Documentation
- Use docstrings for all public functions/classes
- Include parameter types and return types
- Provide usage examples for complex functions
- Keep README files updated

## Engineering Domain Conventions
- Follow offshore engineering naming standards
- Use industry-standard units and conventions
- Include references to relevant codes/standards in comments
- Maintain traceability to engineering specifications