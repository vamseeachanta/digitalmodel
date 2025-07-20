# AI Guidelines

This file provides comprehensive guidance for AI assistants working with this repository.

## Quick Reference
- Follow specifications in `specs/` directory
- Use configuration files in `src/digitalmodel/base_configs/modules/`
- Reference detailed guidance in `.ai/code-guidance/`
- Follow development process in `.ai/workflows/`

## Project Architecture

### Directory Structure
```
digitalmodel/
├── .ai/                           # AI assistant configuration
│   ├── code-guidance/            # Code standards and patterns
│   ├── commands/                 # Automation commands
│   ├── workflows/                # Development processes
│   ├── settings.json             # AI assistant settings
│   └── project-context.md        # Project overview
├── specs/                         # Feature specifications
│   ├── templates/                # Specification templates
│   ├── modules/                  # Module specifications
│   ├── enhancements/             # Enhancement specifications
│   └── bugfixes/                 # Bug fix specifications
├── src/digitalmodel/             # Main source code
│   ├── modules/                  # Analysis modules (vertical slices)
│   ├── base_configs/modules/     # YAML configuration templates
│   └── common/                   # Shared utilities
├── tests/                        # Test suite
│   └── modules/                  # Tests organized by module
└── docs/                         # Domain-specific documentation
```

### Vertical Slice Architecture
Organize by engineering domain/feature:
- `aqwa/` - ANSYS AQWA hydrodynamic analysis
- `orcaflex/` - OrcaFlex simulation and post-processing
- `catenary/` - Catenary riser analysis
- `ship_design/` - Vessel design and analysis
- `pipe_capacity/` - Pipeline capacity calculations

## OrcaFlex Testing

OrcaFlex tests require a license which may not be available in all environments. For debugging and testing purposes, use the mock approach:

### Mock OrcaFlex API for Testing

When debugging OrcaFlex tests without a license, create a mock OrcaFlex API before importing any modules:

```python
# Mock OrcFxAPI before any imports
class MockGeneral:
    def __init__(self):
        self.StageEndTime = [3600.0]  # 1 hour
        self.ImplicitUseVariableTimeStep = "No"
        self.ImplicitConstantTimeStep = 0.1
        self.ImplicitVariableMaxTimeStep = 0.1
        
class MockState:
    def __init__(self):
        self._name_ = "SimulationStopped"
        
class MockTimeStatus:
    def __init__(self):
        self.CurrentTime = 3600.0

class MockOrcFxAPI:
    class Model:
        def __init__(self, *args, **kwargs):
            self.general = MockGeneral()
            self.simulationComplete = True
            self.state = MockState()
            self.simulationStartTime = "2023-01-01 00:00:00"
            self.simulationStopTime = "2023-01-01 01:00:00"
            self.simulationTimeStatus = MockTimeStatus()
            
        def LoadData(self, *args, **kwargs):
            pass
            
        def RunSimulation(self, *args, **kwargs):
            pass
            
        def SaveSimulation(self, *args, **kwargs):
            pass
            
        def SaveData(self, *args, **kwargs):
            pass

sys.modules['OrcFxAPI'] = MockOrcFxAPI()
```

### Test Files Structure

OrcaFlex tests require specific file structure:
- Configuration files (.yml) in the `yml/` directory
- Corresponding simulation files (.sim) in the same directory
- Expected results in `results/` directory
- Test data CSV files for comparison

### Running OrcaFlex Tests

Use `uv run python mock_orcaflex_test.py` to run tests with the mock API instead of the direct test file.
