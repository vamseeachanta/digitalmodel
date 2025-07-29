# Architecture Patterns

## Vertical Slice Architecture

Organize code by engineering domain/feature rather than technical layers:

```
src/digitalmodel/modules/
├── aqwa/                    # ANSYS AQWA analysis
├── orcaflex/               # OrcaFlex simulation
├── catenary/               # Catenary riser analysis
├── ship_design/            # Vessel design
└── pipe_capacity/          # Pipeline calculations
```

Each module contains:
- Analysis logic
- Configuration schemas
- Post-processing utilities
- Domain-specific components

## Configuration-Driven Design

### YAML Configuration Pattern
```python
def load_config(config_path: str) -> Dict[str, Any]:
    """Load and validate YAML configuration."""
    with open(config_path, 'r') as f:
        config = yaml.safe_load(f)
    # Add validation logic
    return config

def run_analysis(config: Dict[str, Any]) -> Results:
    """Run analysis based on configuration."""
    # Initialize from config
    # Execute analysis
    # Return results
```

### Configuration Organization
- Base configs: `src/digitalmodel/base_configs/modules/`
- User configs: Local YAML files
- Template configs: Reusable across projects

## Module Structure Pattern

Each analysis module should follow:
```
module_name/
├── __init__.py
├── analysis.py              # Main analysis logic
├── components.py            # Reusable components
├── post_process.py          # Results processing
└── utilities.py             # Helper functions
```

## Data Flow Pattern

1. **Configuration Loading**: Load YAML config
2. **Initialization**: Create analysis objects
3. **Execution**: Run calculations/simulations
4. **Post-processing**: Process results
5. **Output**: Save in standard formats

## Error Handling Pattern

```python
class AnalysisError(Exception):
    """Base class for analysis errors."""
    pass

class ConfigurationError(AnalysisError):
    """Configuration validation errors."""
    pass

def validate_config(config: Dict[str, Any]) -> None:
    """Validate configuration and raise specific errors."""
    if 'required_field' not in config:
        raise ConfigurationError("Missing required field: required_field")
```

## Testing Patterns

### Mock External Dependencies
```python
# For OrcaFlex and other licensed software
class MockOrcFxAPI:
    def __init__(self):
        # Mock implementation
        pass

# Use in tests
sys.modules['OrcFxAPI'] = MockOrcFxAPI()
```

### Configuration Testing
```python
def test_config_validation():
    """Test configuration validation."""
    with pytest.raises(ConfigurationError):
        validate_config({})  # Missing required fields
```

## Integration Patterns

### External Software Integration
- Use adapter pattern for external APIs
- Implement mock versions for testing
- Handle license requirements gracefully
- Provide fallback behaviors where possible