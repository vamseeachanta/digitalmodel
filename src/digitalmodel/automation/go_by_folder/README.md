# Create Go-By Folder Module

## Overview

The Create Go-By Folder module generates lightweight, representative versions of large folder structures, reducing size by >99% while preserving essential information for understanding, analysis, and automation.

## Features

### Core Functionality
- **Smart File Sampling**: Preserves one original file of each type unchanged
- **Intelligent Minimization**: Reduces file sizes while maintaining structure
- **Pattern Detection**: Identifies naming conventions, parameter sweeps, and sequences
- **Metadata Generation**: Creates comprehensive documentation and analysis

### Advanced Features
- **Checkpoint/Resume**: Fault-tolerant processing with auto-save
- **Parallel Processing**: Multi-core optimization for large folders
- **Analysis Mode**: Detects workflows, dependencies, and automation opportunities
- **Script Generation**: Creates automation scripts for batch processing

## Installation

The module is part of the digitalmodel repository and uses the existing uv environment:

```bash
# Ensure you're in the digitalmodel repository
cd D:/github/digitalmodel

# Module is already installed as part of digitalmodel
uv run python -m digitalmodel.automation.go_by_folder --help
```

## Usage

### Basic Usage

```bash
# Create go-by folder
python -m digitalmodel.automation.go_by_folder \
    -s ./source_folder \
    -t ./goby_output
```

### Advanced Usage

```bash
# With analysis mode and parallel processing
python -m digitalmodel.automation.go_by_folder \
    -s ./simulations \
    -t ./template \
    --analysis-mode \
    --parallel 4 \
    --overwrite
```

### Command Line Options

| Option | Description | Default |
|--------|-------------|---------|
| `-s, --source-folder` | Source folder to convert | Required |
| `-t, --target-folder` | Output location for go-by folder | Required |
| `--overwrite` | Overwrite existing target folder | False |
| `--max-file-size` | Maximum size for minimized files | 10KB |
| `--analysis-mode` | Enable parameter sweep detection | False |
| `--parallel N` | Use N workers for parallel processing | None |
| `--checkpoint-interval` | Auto-save every N files | 100 |
| `--yes-to-all` | Auto-confirm all prompts | False |
| `--exclude-patterns` | Patterns to exclude (comma-separated) | None |
| `--include-patterns` | Patterns to include (comma-separated) | None |
| `--config` | YAML configuration file | None |

### Configuration File

Create a YAML configuration file for complex setups:

```yaml
# goby_config.yaml
max_file_size: 5KB
variation_coverage: high
analysis_mode: true
parallel: 4
exclude_patterns:
  - "*.log"
  - "*.tmp"
  - "node_modules"
include_patterns:
  - "*.py"
  - "*.yaml"
  - "*.json"
```

Use with: `--config goby_config.yaml`

## Output Structure

```
target_folder/
├── _originals/           # Preserved original files (one per type)
├── _templates/           # Pattern templates and examples
├── _samples/             # Representative variation samples
├── metadata/             # Detailed JSON metadata files
│   ├── FILE_INVENTORY.json
│   ├── PATTERN_ANALYSIS.json
│   ├── SIZE_ANALYSIS.json
│   └── WORKFLOW_ANALYSIS.json
├── docs/                 # Documentation
│   ├── DEVELOPER_GUIDE.md
│   ├── FILE_STRUCTURE.md
│   └── USAGE_EXAMPLES.md
├── GO_BY_METADATA.json   # Main metadata file
├── AGENT_OVERVIEW.md     # AI-agent friendly overview
└── VARIATION_MAPPING.yml # Parameter variation analysis
```

## Use Cases

### 1. Template Creation
Create templates from existing projects:
```bash
python -m digitalmodel.automation.go_by_folder \
    -s ./completed_project \
    -t ./project_template \
    --analysis-mode
```

### 2. Parameter Sweep Analysis
Analyze simulation folders with parameter sweeps:
```bash
python -m digitalmodel.automation.go_by_folder \
    -s ./simulation_results \
    -t ./sweep_analysis \
    --analysis-mode \
    --generate-variation-script
```

### 3. Documentation Generation
Generate documentation for large codebases:
```bash
python -m digitalmodel.automation.go_by_folder \
    -s ./large_codebase \
    -t ./codebase_docs \
    --capture-workflow
```

### 4. CI/CD Integration
Use in CI/CD pipelines for artifact reduction:
```bash
python -m digitalmodel.automation.go_by_folder \
    -s ./build_artifacts \
    -t ./ci_artifacts \
    --max-file-size 1KB \
    --parallel 8
```

## File Processing Strategies

### Text Files
- Small files (<10KB): Preserved as-is
- Large files: Keep first/last 10 lines with omission marker
- CSV files: Preserve header and sample rows

### Code Files
- Preserve imports and structure
- Keep function/class signatures
- Remove implementation details
- Maintain docstrings

### Configuration Files
- JSON/YAML: Preserve structure, truncate arrays
- INI/CFG: Keep sections and key names
- Sensitive values are redacted

### Binary Files
- Replace with descriptive text stubs
- Include file metadata (size, type)
- Special handling for .sim files (OrcaFlex)

## Analysis Mode Features

When `--analysis-mode` is enabled:

1. **Parameter Sweep Detection**
   - Identifies parameters in filenames
   - Detects value ranges and distributions
   - Creates variation mapping

2. **Workflow Analysis**
   - Detects workflow type (simulation, batch, data processing)
   - Identifies processing stages
   - Maps data flow between file types

3. **Script Generation**
   - `generate_variations.py`: Create parameter variations
   - `batch_config.yml`: Batch processing configuration
   - `run_batch.sh`: Shell execution script

4. **Dependency Mapping**
   - Identifies file dependencies
   - Detects bottlenecks
   - Suggests parallelization opportunities

## Performance

### Benchmarks
- Small folders (<1GB, <1000 files): <10 seconds
- Medium folders (1-10GB, 1000-10000 files): <1 minute
- Large folders (10-100GB, 10000+ files): 2-10 minutes with parallel processing

### Optimization Tips
- Use `--parallel` for folders with many files
- Use `--optimize-workers` to auto-detect optimal worker count
- Increase `--checkpoint-interval` for very large folders
- Use `--exclude-patterns` to skip unnecessary files

## Troubleshooting

### Common Issues

1. **Permission Errors**
   ```
   Solution: Run with appropriate permissions or use --exclude-patterns to skip protected files
   ```

2. **Memory Issues with Large Folders**
   ```
   Solution: Use --parallel with fewer workers or process in batches
   ```

3. **Checkpoint Resume Fails**
   ```
   Solution: Delete .go_by_checkpoint folder and restart, or use --no-checkpoint
   ```

### Debug Mode

Enable detailed logging:
```bash
python -m digitalmodel.automation.go_by_folder \
    -s ./source -t ./target \
    --log-level DEBUG
```

## API Usage

```python
from digitalmodel.automation.go_by_folder import CreateGoBy

# Create orchestrator
config = {
    'overwrite': True,
    'max_file_size': '5KB',
    'parallel': 4,
    'analysis_mode': True
}

orchestrator = CreateGoBy(
    source=Path('./source'),
    target=Path('./target'),
    config=config
)

# Execute
success = orchestrator.execute()
```

## Contributing

This module is part of the digitalmodel repository. Follow the repository's contribution guidelines.

## License

See the digitalmodel repository license.

## Support

For issues or questions, please refer to the digitalmodel repository documentation or create an issue in the repository.