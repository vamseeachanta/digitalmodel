# AI Guidelines

Please follow [AI Guidelines directory](https://github.com/vamseeachanta/pyproject-starter/tree/master/.ai) 
for all development work.

## Specific Sections
- [Python programming](https://github.com/vamseeachanta/pyproject-starter/blob/master/.ai/code-guidance/AI_ASSISTANT-PYTHON-BASIC.md)


## Project Specific Directory Structure

Follow strict vertical slice architecture

assetutilities/
│
├── .ai/                            # AI assistant configuration
│   ├── commands/                   # Custom automation commands
│   │   ├── generate-spec.md        # Specification generation logic
│   │   └── execute-spec.md         # Specification execution logic
│   │── settings.json              # AI assistant permissions and preferences
│   └── AI_GUIDELINES.md            # Global AI assistant rules │
├── docs/                           # Documentation and reference materials
│   ├── chat-history/              # AI conversation logs
│   │   ├── README.md              # Session index
│   │   └── YYYY-MM-DD_topic.md    # Timestamped sessions
│   ├── modules/
│       └── [module-name].md       # Module-specific specifications
│   └── workflows/                 # Development workflows

│
├── specs/                          # Project Specification Documents
│   ├── templates/                  # Reusable specification templates
│   │   └── spec_base.md           # Base template structure
│   └── [feature-name].md          # Generated specifications
│   └── modules/
│       └── [module-name].md       # Module-specific specifications
├── src/
│   └── assetutilities/              # Main source code
│   └── base_configs
│       └── modules/
│   └── modules/
│       └── [module-name].md       # Module-specific specifications
│
├── tests/                          # Test scripts and modules
│   ├── __init__.py
│   └── modules/
│       └── [module-name].md       # Module-specific specifications
│
└── .github/                        # GitHub workflows

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
