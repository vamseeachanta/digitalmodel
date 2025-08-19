# Universal OrcaFlex Simulation Runner Specification

## Overview

A comprehensive, library-based OrcaFlex simulation runner that can be executed from any directory on any computer with flexible keyword arguments for pattern matching, batch processing, and directory management. This builds upon the existing orcaflex-sim slash command work and incorporates proven architecture from production run_models_to_sim.py implementations.

## Business Requirements

### Primary Goals
1. **Universal Accessibility**: Execute OrcaFlex simulations from any directory without path dependencies
2. **Flexible Processing**: Support single file, batch, pattern-based, and directory-based processing
3. **Library Integration**: Seamless integration with digitalmodel repository architecture
4. **Production Ready**: Incorporate proven patterns from existing production code
5. **Cross-Platform**: Work on Windows, Linux, and macOS environments

### User Stories
- As an engineer, I want to run OrcaFlex simulations from any project directory
- As a batch processor, I want to process hundreds of models with configurable parallelism
- As a developer, I want a library API with comprehensive keyword arguments
- As an operator, I want clear progress tracking and error reporting
- As a system admin, I want resource management and license optimization

## Technical Architecture

### System Components

```
┌─────────────────────────────────────────────────────────────┐
│                    Universal Access Layer                    │
├─────────────────────────────────────────────────────────────┤
│  Slash Command  │  Library API  │  CLI Interface  │  Config │
├─────────────────────────────────────────────────────────────┤
│                    Core Processing Engine                    │
├─────────────────────────────────────────────────────────────┤
│  Pattern Matcher │ Batch Manager │ Parallel Executor │ Mock │
├─────────────────────────────────────────────────────────────┤
│                    OrcaFlex Integration                      │
├─────────────────────────────────────────────────────────────┤
│  Model Loader  │  Static Solver  │  Sim Generator  │ License│
└─────────────────────────────────────────────────────────────┘
```

### Core Classes

#### 1. UniversalOrcaFlexRunner
```python
class UniversalOrcaFlexRunner:
    """Universal OrcaFlex simulation runner with flexible configuration."""
    
    def __init__(self, 
                 base_dir: Optional[Path] = None,
                 mock_mode: bool = False,
                 max_workers: int = 30,
                 license_timeout: int = 300):
        """Initialize with configurable parameters."""
        
    def run(self, 
            pattern: Optional[str] = None,
            input_directory: Optional[Path] = None,
            output_directory: Optional[Path] = None,
            models: Optional[List[Path]] = None,
            recursive: bool = False,
            parallel: bool = True,
            **kwargs) -> RunResults:
        """Execute simulations with flexible keyword arguments."""
```

#### 2. ModelDiscovery
```python
class ModelDiscovery:
    """Discover and filter OrcaFlex models based on patterns."""
    
    def find_models(self,
                   directory: Path,
                   pattern: str = "*.yml",
                   recursive: bool = False,
                   exclude_patterns: List[str] = None) -> List[Path]:
        """Find models matching criteria."""
        
    def validate_model(self, model_path: Path) -> bool:
        """Validate if file is a valid OrcaFlex model."""
```

#### 3. BatchProcessor
```python
class BatchProcessor:
    """Manage batch processing with resource optimization."""
    
    def process_batch(self,
                     models: List[Path],
                     processor_func: Callable,
                     max_workers: int = 30,
                     chunk_size: int = None) -> BatchResults:
        """Process models in optimized batches."""
```

### Library API

#### Primary Interface
```python
from digitalmodel.modules.orcaflex import UniversalSimRunner

# Single file processing
runner = UniversalSimRunner()
result = runner.run(models=["model.yml"])

# Pattern-based batch processing
results = runner.run(
    pattern="fsts_*.yml",
    input_directory="/path/to/models",
    output_directory="/path/to/sims",
    parallel=True,
    max_workers=20
)

# Configuration-based processing
results = runner.run(config_file="batch_config.yml")

# Directory processing with filters
results = runner.run(
    input_directory=".",
    recursive=True,
    exclude_patterns=["*test*", "*backup*"],
    output_directory="./simulations"
)
```

### Configuration Schema

```yaml
# Universal runner configuration
runner:
  mode: production  # production, development, test
  mock: false
  license:
    timeout: 300
    retry_attempts: 3
    
processing:
  parallel: true
  max_workers: 30
  chunk_size: auto
  priority_patterns:
    - "*critical*.yml"
    - "*urgent*.yml"
    
discovery:
  patterns:
    - "fsts_*.yml"
    - "model_*.yml"
  exclude:
    - "*includefile*"
    - "*backup*"
    - "*test*"
  recursive: true
  
paths:
  input_directory: "./models"
  output_directory: "./simulations"
  temp_directory: "./temp"
  
logging:
  level: INFO
  file: "simulation_run.log"
  console: true
  
error_handling:
  continue_on_error: true
  save_error_reports: true
  max_retries: 2
```

## Implementation Details

### 1. Path Resolution System
```python
class PathResolver:
    """Resolve paths for universal access."""
    
    @staticmethod
    def find_digitalmodel_root() -> Path:
        """Find digitalmodel repository from any location."""
        strategies = [
            lambda: Path.cwd() / "digitalmodel",
            lambda: Path.home() / "github" / "digitalmodel",
            lambda: Path(os.environ.get("DIGITALMODEL_ROOT", "")),
            lambda: PathResolver._search_upward(".git"),
        ]
        
    @staticmethod
    def resolve_model_path(model_ref: str) -> Path:
        """Resolve model reference to absolute path."""
```

### 2. Parallel Processing Strategy
```python
class ParallelStrategy:
    """Optimized parallel processing strategies."""
    
    def adaptive_workers(self, model_count: int) -> int:
        """Calculate optimal worker count based on system resources."""
        cpu_count = os.cpu_count()
        memory_gb = psutil.virtual_memory().total / (1024**3)
        license_limit = self.get_license_limit()
        
        return min(
            model_count,
            cpu_count * 2,
            int(memory_gb / 2),  # 2GB per worker
            license_limit,
            30  # Default max
        )
```

### 3. Error Recovery
```python
class ErrorRecovery:
    """Handle and recover from processing errors."""
    
    def with_retry(self, func: Callable, max_attempts: int = 3):
        """Execute with automatic retry on failure."""
        
    def handle_license_error(self, error: Exception):
        """Handle OrcaFlex license errors with backoff."""
        
    def save_error_context(self, model: Path, error: Exception):
        """Save detailed error context for debugging."""
```

## Integration Points

### 1. Slash Command Integration
```python
# .agent-os/commands/orcaflex-universal-sim.py
def main():
    """Universal slash command entry point."""
    runner = UniversalSimRunner.from_environment()
    return runner.run_from_args(sys.argv[1:])
```

### 2. CLI Integration
```python
# src/digitalmodel/modules/orcaflex/universal_cli.py
@click.command()
@click.option('--pattern', help='File pattern to match')
@click.option('--input-dir', type=click.Path())
@click.option('--output-dir', type=click.Path())
def run_universal(pattern, input_dir, output_dir):
    """Universal OrcaFlex simulation runner."""
```

### 3. Python Package Integration
```python
# setup.py entry point
entry_points={
    'console_scripts': [
        'orcaflex-sim=digitalmodel.modules.orcaflex.universal_cli:main',
    ],
}
```

## Performance Considerations

### Resource Management
- **Memory**: Monitor and limit memory usage per worker
- **CPU**: Adaptive worker scaling based on system load
- **Disk I/O**: Batch write operations for efficiency
- **Network**: Handle network paths with appropriate timeouts

### Optimization Strategies
1. **Model Caching**: Cache parsed model configurations
2. **License Pooling**: Efficient license token management
3. **Batch Chunking**: Optimal chunk sizes for parallel processing
4. **Progress Streaming**: Real-time progress without blocking

## Testing Strategy

### Unit Tests
```python
class TestUniversalRunner:
    def test_pattern_matching(self):
        """Test various pattern matching scenarios."""
        
    def test_parallel_processing(self):
        """Test parallel execution with mock models."""
        
    def test_error_recovery(self):
        """Test error handling and recovery."""
```

### Integration Tests
```python
class TestIntegration:
    def test_cli_integration(self):
        """Test CLI command execution."""
        
    def test_slash_command(self):
        """Test slash command from various directories."""
        
    def test_batch_processing(self):
        """Test large batch processing."""
```

### Performance Tests
```python
class TestPerformance:
    def test_large_batch(self):
        """Test with 1000+ models."""
        
    def test_memory_usage(self):
        """Monitor memory during processing."""
        
    def test_adaptive_scaling(self):
        """Test worker scaling algorithms."""
```

### Real OrcaFlex Tests
```python
class TestRealOrcaFlex:
    """Test with actual OrcaFlex .dat files from test repository."""
    
    def setup_test_files(self):
        """Copy .dat files from tests/modules/orcaflex/orcaflex_analysis."""
        test_files = [
            "orcaflex_test2.dat",
            "moorings/pretension/fsts_lngc/01_qa/6dof/fsts_l095_mwl_125km3_l000_pb.dat",
            "moorings/pretension/fsts_lngc/01_qa/dof_none/fsts_l095_mwl_125km3_l000_pb.dat"
        ]
        
    def test_dat_to_sim_conversion(self):
        """Test converting .dat files to .sim with real OrcaFlex."""
        
    def test_batch_dat_processing(self):
        """Test parallel processing of multiple .dat files."""
        
    def test_status_reporting(self):
        """Verify real-time status updates during processing."""
```

### Live Status Reporting
```python
class StatusReporter:
    """Real-time status reporting in terminal/bash window."""
    
    def update_terminal_title(self, status: dict):
        """Update bash window title with current status."""
        title = f"OrcaFlex: {status['completed']}/{status['total']} | ✓{status['success']} ✗{status['failed']} | {status['current_model']}"
        if sys.platform == "win32":
            os.system(f"title {title}")
        else:
            sys.stdout.write(f"\033]0;{title}\007")
            
    def display_progress_bar(self, progress: float):
        """Show progress bar in terminal."""
        bar_length = 50
        filled = int(bar_length * progress)
        bar = '█' * filled + '░' * (bar_length - filled)
        return f"[{bar}] {progress*100:.1f}%"
        
    def generate_summary_report(self) -> dict:
        """Generate final summary of test results."""
        return {
            "total_runs": self.total,
            "successful": self.success,
            "failed": self.failed,
            "success_rate": (self.success/self.total)*100,
            "failed_models": self.failed_list,
            "execution_time": self.total_time,
            "output_files": self.sim_files_created
        }
```

## Security Considerations

1. **Path Validation**: Prevent directory traversal attacks
2. **Input Sanitization**: Validate all user inputs
3. **Resource Limits**: Prevent resource exhaustion
4. **License Protection**: Secure license token handling
5. **Audit Logging**: Log all operations for compliance

## Documentation Requirements

### User Documentation
- Quick start guide
- API reference
- Configuration examples
- Troubleshooting guide
- Performance tuning guide

### Developer Documentation
- Architecture overview
- Extension guide
- Testing guide
- Contribution guidelines

## Success Metrics

1. **Performance**: Process 100 models in under 5 minutes
2. **Reliability**: 99.9% success rate for valid models
3. **Accessibility**: Zero-configuration usage from any directory
4. **Scalability**: Handle 1000+ models in single batch
5. **Usability**: Single command execution with intuitive options

## Migration Path

### From Existing Scripts
```python
# Old approach
python run_models_to_sim.py --all

# New approach (from anywhere)
orcaflex-sim --all
# or
python -m digitalmodel.orcaflex.universal run --all
```

### Backward Compatibility
- Support existing configuration formats
- Maintain legacy command aliases
- Provide migration utilities for old scripts

## Future Enhancements

1. **Cloud Integration**: Run simulations on cloud infrastructure
2. **Distributed Processing**: Coordinate across multiple machines
3. **Smart Scheduling**: ML-based optimal scheduling
4. **Result Analytics**: Automatic result analysis and reporting
5. **Version Control**: Integrated model versioning