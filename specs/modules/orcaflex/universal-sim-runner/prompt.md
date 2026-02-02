# Universal OrcaFlex Simulation Runner - Implementation Prompt

## Context

This specification defines a universal OrcaFlex simulation runner that builds upon completed work and incorporates proven architecture from production systems. The implementation should enable OrcaFlex simulation execution from any directory on any computer with comprehensive keyword argument support.

## Background

### Completed Work
1. **orcaflex-sim slash command**: Basic slash command implementation completed
   - Location: `.agent-os/commands/orcaflex-sim.py`
   - Features: Basic model running, mock mode, batch support

2. **run_to_sim modules**: Core simulation modules implemented
   - Location: `src/digitalmodel/modules/orcaflex/run_to_sim.py`
   - Location: `src/digitalmodel/modules/orcaflex/run_to_sim_cli.py`
   - Features: Model loading, static analysis, parallel processing

3. **Test infrastructure**: Comprehensive bash test suite created
   - Location: `tests/modules/orcaflex/batch_processing/test_run_to_sim.sh`
   - Coverage: CLI, mock mode, integration, performance tests

### Reference Architecture
The production `run_models_to_sim.py` from `D:\1522\ctr7\orcaflex\rev_a08\base_files\fsts_lngc_pretension\` provides:
- Proven parallel processing with ThreadPoolExecutor
- Robust error handling and recovery
- Efficient batch processing patterns
- Clear logging and progress tracking

## Requirements Summary

### Core Requirements
1. **Universal Access**: Run from any directory without path dependencies
2. **Flexible Arguments**: Support pattern, input_directory, output_directory, single_file, batch_processing via kwargs
3. **Library Architecture**: Follow digitalmodel repository patterns
4. **Production Ready**: Incorporate proven patterns from existing code
5. **Cross-Platform**: Support Windows, Linux, macOS

### Key Features
- Pattern-based model discovery (glob, regex)
- Parallel batch processing with adaptive scaling
- Comprehensive error recovery and retry logic
- Mock mode for testing without licenses
- Configuration file support (YAML)
- Real-time progress tracking
- Resource optimization

## Implementation Instructions

### Phase 1: Core Infrastructure

Create the foundation classes in `src/digitalmodel/modules/orcaflex/universal/`:

```python
# universal_runner.py
class UniversalOrcaFlexRunner:
    def __init__(self, **kwargs):
        # Accept flexible configuration
        pass
        
    def run(self, **kwargs):
        # Main entry point with keyword arguments
        # Support: pattern, input_directory, output_directory, 
        #         models, config_file, parallel, etc.
        pass
```

### Phase 2: Integration Points

1. **Library API**: Make importable from anywhere
   ```python
   from digitalmodel.orcaflex import UniversalSimRunner
   runner = UniversalSimRunner()
   results = runner.run(pattern="*.yml", input_directory=".")
   ```

2. **CLI Command**: Accessible via command line
   ```bash
   python -m digitalmodel.orcaflex.universal --pattern "*.yml"
   ```

3. **Slash Command**: Universal execution
   ```bash
   /orcaflex-universal-sim --all --output ./sims
   ```

### Phase 3: Key Implementation Details

#### Pattern Matching System
```python
def find_models(self, **kwargs):
    pattern = kwargs.get('pattern', '*.yml')
    input_dir = kwargs.get('input_directory', '.')
    recursive = kwargs.get('recursive', False)
    exclude = kwargs.get('exclude_patterns', [])
    
    # Implement flexible discovery
```

#### Parallel Processing
```python
def process_batch(self, models, **kwargs):
    max_workers = kwargs.get('max_workers', 30)
    chunk_size = kwargs.get('chunk_size', 'auto')
    
    with ThreadPoolExecutor(max_workers=max_workers) as executor:
        # Process with progress tracking
```

#### Configuration Support
```yaml
# Support configuration files
processing:
  pattern: "fsts_*.yml"
  input_directory: "./models"
  output_directory: "./simulations"
  parallel: true
  max_workers: 20
```

## Testing Requirements

### Unit Tests
- Test all keyword argument combinations
- Verify pattern matching accuracy
- Test error recovery mechanisms
- Validate configuration parsing

### Integration Tests
- Test from various directories
- Verify slash command execution
- Test batch processing with 100+ models
- Validate cross-platform compatibility

### Performance Benchmarks
- Process 100 models in <5 minutes
- Handle 1000+ models without memory issues
- Maintain <2GB memory per worker

## Success Criteria

1. **Accessibility**: Zero-configuration usage from any directory
2. **Flexibility**: All keyword arguments properly supported
3. **Performance**: Meets or exceeds current implementation
4. **Reliability**: 99%+ success rate for valid models
5. **Documentation**: Complete API and usage documentation

## Code Quality Standards

- Follow PEP 8 style guidelines
- Type hints for all public methods
- Comprehensive docstrings
- 90%+ test coverage
- No hard-coded paths

## Delivery Expectations

### Deliverables
1. Core universal runner implementation
2. Library API with keyword argument support
3. CLI and slash command interfaces
4. Comprehensive test suite
5. User and API documentation
6. Migration guide from existing scripts

### Timeline
- Phase 1-2: 3-4 days (Core implementation)
- Phase 3-4: 2-3 days (Interfaces and configuration)
- Phase 5-6: 2-3 days (Testing and documentation)
- Phase 7: 1 day (Integration and deployment)

Total: 8-11 days

## Additional Context

### Repository Structure
```
digitalmodel/
├── src/digitalmodel/modules/orcaflex/
│   ├── universal/
│   │   ├── __init__.py
│   │   ├── universal_runner.py
│   │   ├── model_discovery.py
│   │   ├── batch_processor.py
│   │   └── config_parser.py
│   └── universal_cli.py
├── tests/modules/orcaflex/universal/
│   ├── test_universal_runner.py
│   ├── test_discovery.py
│   └── test_integration.py
└── .agent-os/commands/
    └── orcaflex-universal-sim.py
```

### Key Considerations
- Maintain backward compatibility with existing scripts
- Optimize for large-scale batch processing
- Ensure robust error handling for production use
- Support both interactive and automated workflows
- Enable easy extension for future enhancements

## Questions for Implementation

1. Should the runner auto-detect OrcaFlex installation paths?
2. What should be the default behavior when no arguments provided?
3. How should license errors be handled in batch processing?
4. Should there be a priority queue for critical models?
5. What level of parallelism is optimal for different systems?

## Next Steps

1. Review and approve this specification
2. Set up development branch
3. Implement Phase 1 core infrastructure
4. Create initial test cases
5. Iterate based on testing feedback

---

*This prompt provides comprehensive context for implementing the Universal OrcaFlex Simulation Runner. The implementation should follow digitalmodel repository patterns while incorporating proven architecture from production systems.*