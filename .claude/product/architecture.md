# DigitalModel Architecture

## System Architecture Overview

DigitalModel follows a modular architecture pattern designed for extensibility and maintainability.

```
digitalmodel/
├── Core Engine
│   ├── engine.py (Main orchestrator)
│   ├── Configuration Management (YAML-based)
│   └── Data Pipeline Framework
├── Domain Modules
│   ├── OrcaFlex Integration
│   ├── AQWA Integration
│   ├── Structural Analysis
│   ├── Marine Engineering
│   └── Utilities
└── External Interfaces
    ├── CAE Software APIs
    ├── File I/O Handlers
    └── Visualization Tools
```

## Core Components

### 1. Engine Module (`engine.py`)
- Central orchestrator for all analyses
- Configuration loading and validation
- Module routing and execution
- Error handling and logging

### 2. Module Architecture
Each module follows a consistent pattern:
- **Input**: YAML configuration files
- **Processing**: Domain-specific calculations
- **Output**: Results, visualizations, reports

### 3. Data Flow
```
YAML Config → Engine → Module → Processing → Results
                ↓                    ↓
           Validation            External Tools
                                (OrcaFlex, AQWA, etc.)
```

## Key Design Patterns

### 1. Configuration-Driven
- All analyses defined via YAML configurations
- Separation of code and configuration
- Easy parameterization and repeatability

### 2. Module Independence
- Each module is self-contained
- Clear interfaces between modules
- Minimal inter-module dependencies

### 3. Parallel Processing Support
- ProcessPoolExecutor for concurrent operations
- Configurable worker pools
- Graceful fallback mechanisms

## Integration Points

### External Software
- **OrcaFlex**: COM interface for simulation data
- **AQWA**: File-based data exchange
- **ANSYS**: Workbench journal files
- **Visualization**: Matplotlib, custom plotting

### Data Formats
- Input: YAML, CSV, Excel, simulation files
- Output: CSV, Excel, PNG/PDF plots, HTML reports
- Intermediate: Pandas DataFrames, NumPy arrays

## Security Considerations
- No hardcoded credentials
- File path validation
- Input sanitization for configurations
- Error messages don't expose sensitive info

## Architecture Patterns

### Vertical Slice Architecture
Organize code by engineering domain/feature rather than technical layers:

```
src/digitalmodel/modules/
├── aqwa/                    # ANSYS AQWA hydrodynamic analysis
├── orcaflex/               # OrcaFlex simulation and post-processing  
├── catenary/               # Catenary riser analysis
├── ship_design/            # Vessel design and analysis
├── pipe_capacity/          # Pipeline capacity calculations
└── mooring/                # Mooring system analysis
```

Each module contains:
- Analysis logic and calculations
- Configuration schemas and validation
- Post-processing utilities
- Domain-specific components
- Integration with external software

### Configuration-Driven Design Pattern

All analyses follow this pattern:
```python
def load_config(config_path: str) -> Dict[str, Any]:
    """Load and validate YAML configuration."""
    with open(config_path, 'r') as f:
        config = yaml.safe_load(f)
    validate_config_schema(config)
    return config

def run_analysis(config: Dict[str, Any]) -> Results:
    """Run analysis based on configuration."""
    # Initialize components from config
    analyzer = create_analyzer(config)
    
    # Execute analysis pipeline
    results = analyzer.run()
    
    # Post-process and save results
    save_results(results, config["output"])
    return results
```

### Module Structure Pattern
Each analysis module follows consistent structure:
```
module_name/
├── __init__.py              # Module interface
├── analysis.py              # Main analysis orchestrator
├── components.py            # Reusable analysis components
├── post_process.py          # Results processing and formatting
├── utilities.py             # Helper functions and utilities
└── config_schemas.py        # Configuration validation schemas
```

### Data Flow Architecture
Standard data pipeline for all modules:

1. **Configuration Loading**: Parse and validate YAML config
2. **Data Preparation**: Load input data and validate formats
3. **Analysis Initialization**: Create analysis objects from config
4. **Execution**: Run calculations or external simulations
5. **Post-Processing**: Extract and process results
6. **Output Generation**: Save results in specified formats

```python
# Standard data flow implementation
def execute_analysis_pipeline(config_path: str) -> None:
    """Execute standard analysis pipeline."""
    
    # 1. Load configuration
    config = load_config(config_path)
    
    # 2. Prepare data
    input_data = prepare_input_data(config["inputs"])
    
    # 3. Initialize analysis
    analyzer = initialize_analyzer(config, input_data)
    
    # 4. Execute analysis
    raw_results = analyzer.execute()
    
    # 5. Post-process results
    processed_results = post_process_results(raw_results, config)
    
    # 6. Generate outputs
    generate_outputs(processed_results, config["outputs"])
```

### Integration Architecture

#### External Software Integration Pattern
```python
class ExternalSoftwareAdapter:
    """Base adapter for external engineering software."""
    
    def __init__(self, config: Dict[str, Any]):
        self.config = config
        self.mock_mode = not self._software_available()
        
    def _software_available(self) -> bool:
        """Check if software/license is available."""
        raise NotImplementedError
        
    def run_analysis(self, inputs: Any) -> Any:
        """Run analysis using external software."""
        if self.mock_mode:
            return self._mock_analysis(inputs)
        else:
            return self._real_analysis(inputs)
```

#### Parallel Processing Architecture
For computationally intensive operations:
```python
from concurrent.futures import ProcessPoolExecutor

def parallel_analysis(file_list: List[str], config: Dict) -> List[Results]:
    """Process multiple files in parallel."""
    max_workers = config.get("parallel", {}).get("max_workers", 4)
    
    with ProcessPoolExecutor(max_workers=max_workers) as executor:
        futures = [
            executor.submit(process_single_file, file_path, config)
            for file_path in file_list
        ]
        
        results = []
        for future in as_completed(futures):
            try:
                result = future.result()
                results.append(result)
            except Exception as e:
                logger.error(f"Analysis failed: {e}")
                
    return results
```

### Error Handling Architecture

#### Hierarchical Exception Structure
```python
class DigitalModelError(Exception):
    """Base exception for all DigitalModel errors."""
    pass

class ConfigurationError(DigitalModelError):
    """Configuration validation and loading errors."""
    pass

class AnalysisError(DigitalModelError):
    """Analysis execution errors."""
    pass

class IntegrationError(DigitalModelError):
    """External software integration errors."""
    pass
```

#### Error Context and Recovery
```python
def safe_analysis_execution(config: Dict) -> Results:
    """Execute analysis with comprehensive error handling."""
    try:
        return run_analysis(config)
    except ConfigurationError as e:
        logger.error(f"Configuration invalid: {e}")
        # Attempt to provide default configuration
        return run_with_defaults(config)
    except IntegrationError as e:
        logger.warning(f"External software failed: {e}")
        # Fall back to mock/simplified analysis
        return run_simplified_analysis(config)
    except Exception as e:
        logger.critical(f"Unexpected error: {e}")
        raise AnalysisError(f"Analysis failed: {e}") from e
```

## Performance Optimization
- Lazy loading of large datasets
- Parallel processing for multi-file operations
- Efficient memory management for large simulations
- Caching of intermediate results where appropriate
- ProcessPoolExecutor for CPU-intensive calculations
- Memory-mapped files for large data access