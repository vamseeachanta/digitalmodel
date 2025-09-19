# Create Go-By Folder from Existing Folder

## Overview
Create a compact, representative "go-by" version of an existing folder. The tool mirrors the directory structure, preserves one untouched original per file type/pattern, replaces redundant/large items with minimized representatives, and generates agent-friendly metadata.

Enhanced for analysis/simulation projects: detects parameter patterns, captures workflows, and produces reproducible templates.

## Arguments
Required
- source_folder: Path to the existing folder to convert (must exist and be readable)
- target_folder: Path to write the go-by folder (created if missing; if exists, should be empty)

Common optional flags (see sections below for details)
- max_file_size: e.g., 10KB. Truncation target for large text/data representatives
- preserve_recent: e.g., 30d. Prefer keeping newer examples unchanged (when applicable)
- include_patterns: Comma-separated globs to include (e.g., "*.yaml,*.config")
- exclude_patterns: Comma-separated globs to exclude (e.g., "node_modules,*.bin")
- variation_coverage: low|medium|high. Controls how many representatives to keep
- analysis_mode: true|false. Enable analysis workflow and variation capture
- preserve_variations: true|false. Keep first/middle/last of sequences
- capture_workflow: true|false. Emit workflow structure metadata
- parameter_detection: auto|off. Detect naming/parameter patterns
- time_series_sampling: linear|logarithmic. Downsample long series
- map_dependencies: true|false. Attempt dependency mapping
- generate_variation_script: true|false. Emit _generators/generate_variations.py
- log_level: DEBUG|INFO|WARN|ERROR
- dry_run: true|false. Report actions without writing files
- **overwrite**: true|false. Overwrite existing target folder (warns user first)

Shorthand
- -s, --source-folder
- -t, --target-folder

## Guarantees
- Directory hierarchy preserved end-to-end
- Exactly one original per file type/pattern kept bit-for-bit in _originals/
- Additional files replaced by minimized representatives or stubs
- Clear, self-describing metadata for agents and humans

## Produces (in target_folder)
- AGENT_OVERVIEW.md
- GO_BY_README.md
- GO_BY_MAPPING.json
- GO_BY_METADATA.json
- VARIATION_MAPPING.yml (when analysis_mode or parameter_detection=auto)
- WORKFLOW_STRUCTURE.yml (when capture_workflow=true)
- BATCH_TEMPLATE.yml (when batch_templates enabled)
- _originals/ with preserved files and ORIGINALS_INDEX.md
- _templates/ and _samples/ with extracted patterns and representatives
- Mirrored original structure populated with minimized files

## Preconditions and Safety
- source_folder must exist and be readable
- Parent of target_folder must exist; target_folder should be empty or new
- Original files in _originals/ are never modified
- See "Validation Checklist" below for pre-run verification

## Exit Codes
- 0: Success
- 1: Validation error (paths, permissions, arguments)
- 2: Runtime error during scanning or generation

## Conflict Resolution

### Existing Target Folder
- If `target_folder` exists and contains files:
  - Without `--overwrite`: Error with message to use --overwrite flag
  - With `--overwrite`: Warn user, then proceed to replace contents
- For incremental updates: Warning shown, then updates proceed
- Git-tracked folders: User is warned but operation continues (Git will track changes)

### Merge Scenarios
- When updating existing go-by: Files are regenerated based on current source
- Manual edits in `_originals/`: Preserved during updates
- User customizations: Document in `USER_CUSTOMIZATIONS.md` for preservation

## Size Boundaries

### Maximum Source Folder Size
- **Recommended limit**: 10GB for optimal performance
- Folders >10GB: User prompted for confirmation
- Memory usage scales with folder complexity, not just size

### Compressed Files Handling
- Already compressed files (.zip, .7z, .tar.gz, etc.):
  - User prompted: "Found compressed archives. Options: [1] Keep as-is [2] Extract samples [3] Skip"
  - Decision documented in GO_BY_METADATA.json
  - Case-by-case handling based on user preference

### Large File Strategy
- All files treated uniformly regardless of size
- Files >50MB generate warning in logs
- Original preservation applies to first instance regardless of size

## Selection Logic

### File Preservation Order
- **Primary criterion**: Chronological order (oldest first)
- Based on file creation time, fallback to modification time
- Ensures historical baseline is preserved

### Symlinks and Hard Links
- **Detection**: All symlinks/hard links identified during scan
- **User prompt**: "Found symlinks at: [paths]. Options: [1] Follow links [2] Preserve as links [3] Skip"
- **Documentation**: Decision recorded in `LINK_HANDLING.md`
- Future reference maintained for consistency

### Duplicate Named Files
- Files with identical names in different directories:
  - Content hash comparison performed
  - If content identical: Keep one, note duplication in metadata
  - If content differs and <50KB: Keep both as distinct patterns
  - If content differs and >50KB: Prompt user for resolution
- Iteration patterns recognized and preserved

## Security & Privacy

### Sensitive Data Handling
- **Not expected** in typical usage
- If potential sensitive patterns detected (passwords, API keys):
  - Scanning paused
  - User alerted: "Potential sensitive data detected in: [file]"
  - User decides: Continue, skip file, or abort
- No automatic redaction performed
- .gitignore patterns not automatically applied

See "Usage Instructions" below for command examples and "Implementation Steps" for the processing pipeline.

## Usage Instructions

### Basic Usage
```bash
# Create a go-by folder
python create_go_by.py --source-folder /path/to/existing/project --target-folder /path/to/project-go-by
```

### For AI Agents
When using this prompt with an AI agent, provide:
```
Create a go-by folder from the source folder at "D:/projects/large-application" 
and save it to "D:/templates/large-application-go-by"
```

### Interactive Usage
1. Specify the source folder you want to templatize
2. Specify where you want the go-by folder created
3. The system will analyze and create a minimal representative structure

### Example Commands
```bash
# Windows example
create-go-by --source-folder "D:\github\myproject" --target-folder "D:\templates\myproject-go-by"

# Linux/Mac example  
create-go-by --source-folder ~/projects/webapp --target-folder ~/templates/webapp-go-by

# Python script example
python scripts/create_go_by.py \
    --source-folder ./production-app \
    --target-folder ./examples/production-app-template
```

### Expected Inputs
- **Source folder**: Must be an existing directory with read permissions
- **Target folder**: Will be created if it doesn't exist (parent directory must exist)

## Core Requirements

### 1. Structure Preservation
- Maintain the complete directory structure of the source folder
- Preserve folder hierarchy and naming conventions
- Keep relative path relationships intact

### 2. File Representation Strategy

#### Original File Preservation (MANDATORY)
- **Preserve one original file of each type/pattern completely unchanged**:
  - Keep the FIRST instance of each file type in its ORIGINAL, UNMODIFIED state
  - Mark preserved originals with `.original` suffix or in `_originals/` subfolder
  - These files serve as the authoritative reference for format, structure, and content
  - Example: If you have `analysis_001.dat` through `analysis_100.dat`, keep `analysis_001.dat` completely unchanged

#### Variation File Handling
- **For all other variation files, create lightweight representatives**:
  - Binary files → Small stub files with metadata (e.g., `example.pdf.stub`)
  - Large data files → Sample excerpts or headers only
  - Images → Low-resolution thumbnails or placeholder images
  - Videos → Single frame screenshots or duration metadata
  - These can be modified to capture patterns and reduce size

### 3. Variation Coverage
- **Include one example of each file type pattern**:
  - If folder has `report_2023.pdf`, `report_2024.pdf` → Keep only one as `report_YYYY.pdf.example`
  - Multiple similar configs → Keep one of each variation type
  - Numbered sequences → Keep first, middle, and last examples

### 4. Content Minimization Techniques

#### For Original Preserved Files
- **NO MODIFICATIONS**: Original files marked as `.original` are kept exactly as-is
- **Full content preserved**: Complete file with all data, formatting, and metadata
- **Binary integrity**: Bit-for-bit identical to source file

#### For Variation/Representative Files
- **Code files**: Keep structure, remove implementation details, preserve signatures
- **Data files**: First 10-100 lines + last 10 lines + row count metadata
- **Config files**: Keep all keys, use placeholder values
- **Documentation**: Keep headings and first paragraph of each section
- **Test files**: Keep test structure and names, minimize test data

### 5. Metadata Preservation
Create a `GO_BY_METADATA.json` in each folder containing:
```json
{
  "original_path": "path/to/original",
  "creation_date": "ISO-8601",
  "statistics": {
    "total_files": 1234,
    "total_size": "2.5GB",
    "file_types": {
      ".py": 45,
      ".json": 23,
      ".pdf": 12
    }
  },
  "sampling_rules": {
    "large_files": "truncated_to_1KB",
    "binaries": "replaced_with_stubs",
    "sequences": "kept_first_middle_last"
  },
  "analysis_metadata": {
    "parameter_ranges": {
      "temperature": {"min": 20, "max": 80, "nominal": 50, "units": "C"},
      "pressure": {"min": 1, "max": 10, "nominal": 5, "units": "bar"}
    },
    "variation_count": {
      "total_combinations": 500,
      "sampled": 25,
      "coverage": "5%"
    },
    "workflow_info": {
      "pipeline_stages": 4,
      "parallel_processes": 20,
      "average_runtime": "45 minutes",
      "resource_requirements": "16GB RAM, 4 CPU cores"
    },
    "reproducibility": {
      "random_seeds": [42, 1337, 2024],
      "software_versions": {"orcaflex": "11.2", "python": "3.11"},
      "convergence_criteria": "1e-6"
    }
  }
}
```

### 6. Variation Patterns to Capture
- **Naming conventions**: camelCase, snake_case, kebab-case examples
- **File formats**: All unique extensions present
- **Depth variations**: Files at different nesting levels
- **Size variations**: Small, medium, large file examples
- **Age variations**: Old, recent, and latest modified files
- **Language variations**: If multilingual, include each language

### 7. Parametric Variation Capture (Analysis-Specific)
For analysis and simulation folders, capture parameter sweep patterns:

#### Pattern Detection
- **Parameter prefixes/suffixes**: `_v1`, `_v2`, `_caseA`, `_run001`
- **Numerical progressions**: `load_10`, `load_50`, `load_100`
- **Matrix combinations**: `temp25_pressure1`, `temp25_pressure2`
- **Time series**: `t0000`, `t0100`, `t0200`, `t0300`
- **Environmental conditions**: `calm`, `operational`, `storm`, `extreme`

#### Variation Mapping
Create `VARIATION_MAPPING.yml` documenting:
```yaml
parameter_patterns:
  loading:
    pattern: "load_{percentage}"
    values: [10, 25, 50, 75, 100]
    units: "percent"
    
  water_level:
    pattern: "{level}_water"
    values: [low, mean, high]
    abbreviations: [lwl, mwl, hwl]
    
  wave_conditions:
    pattern: "wave_{return_period}yr"
    values: [1, 10, 100]
    associated_heights: [2.5, 4.8, 7.2]
    
combination_rules:
  - all_loading_with_all_water_levels
  - extreme_cases_only_for_100yr_waves
  - operational_subset_for_sensitivity
```

#### Sampling Strategy
- Keep **first, middle, last** from numerical sequences
- Preserve **extreme cases** (min/max values)
- Include **nominal/baseline** configuration
- Sample **representative combinations** from matrix

### 8. Analysis Workflow Recognition
Identify and preserve analysis pipeline structures:

#### Workflow Patterns
```yaml
workflow_structure:
  input_folders:
    - raw_data/
    - configurations/
    - boundary_conditions/
  
  processing_stages:
    - preprocessing/
    - meshing/
    - analysis/
    - postprocessing/
  
  output_organization:
    - results/by_parameter/
    - results/by_case/
    - results/aggregated/
    - reports/
```

#### Dependency Tracking
- Document file dependencies in `DEPENDENCIES.json`
- Preserve batch configuration files
- Map input → processing → output relationships
- Identify parallel vs sequential processing patterns

### 9. Organization Structure
```
go-by-folder/
├── AGENT_OVERVIEW.md         # 🤖 AI Agent-friendly overview of entire structure
├── GO_BY_README.md          # Explains the go-by structure
├── GO_BY_MAPPING.json        # Maps go-by files to original patterns
├── VARIATION_MAPPING.yml     # Parameter variation patterns
├── WORKFLOW_STRUCTURE.yml    # Analysis workflow documentation
├── BATCH_TEMPLATE.yml        # Template for batch execution
├── _originals/               # PRESERVED ORIGINAL FILES (one of each type)
│   ├── data_file.csv         # Original, unmodified data file
│   ├── config.yml            # Original, unmodified config
│   ├── analysis.sim          # Original, unmodified simulation file
│   └── ORIGINALS_INDEX.md    # Index of preserved originals with descriptions
├── _templates/               # Extracted common patterns
│   ├── file_patterns.txt
│   └── naming_conventions.md
├── _samples/                 # Representative samples by type (modified/minimized)
│   ├── by_extension/
│   ├── by_size/
│   └── by_purpose/
└── [original structure]/     # Mirrored structure with minimal/representative files
```

## Variation Template Generator

### Auto-Generated Templates
The system creates templates for generating new variations:

#### VARIATION_TEMPLATE.yml
```yaml
# Auto-generated template for creating analysis variations
variation_generator:
  base_configuration: "baseline_config.yml"
  
  parameters:
    - name: loading_percentage
      type: numeric
      range: [0, 100]
      step: 10
      units: percent
      
    - name: environmental_condition  
      type: categorical
      options: [calm, mild, moderate, severe, extreme]
      probabilities: [0.3, 0.25, 0.25, 0.15, 0.05]
      
    - name: material_properties
      type: nested
      variations:
        density: {min: 7800, max: 8100, nominal: 7850}
        youngs_modulus: {min: 200e9, max: 210e9, nominal: 206e9}
  
  generation_strategies:
    full_factorial:
      description: "All parameter combinations"
      estimated_cases: 500
      
    latin_hypercube:
      description: "Space-filling design"
      samples: 50
      
    sensitivity_subset:
      description: "One-at-a-time variations"
      baseline_plus_variations: 21
      
  naming_convention:
    template: "{base_name}_{param1}_{param2}_{index:04d}"
    examples:
      - "analysis_load50_calm_0001"
      - "analysis_load100_extreme_0042"
```

### Reproducibility Scripts
Include generation scripts in `_generators/`:
```python
# generate_variations.py
def create_parameter_matrix(config_file):
    """Generate all variations from template"""
    # Load base configuration
    # Apply parameter variations
    # Generate file names
    # Create analysis matrix
```

## Analysis Pattern Library

### Common Analysis Patterns

#### 1. Load Case Patterns
```
load_cases/
├── operational/
│   ├── load_10_percent/
│   ├── load_50_percent/
│   └── load_100_percent/
├── extreme/
│   ├── survival_condition/
│   └── accidental_loads/
└── fatigue/
    ├── wave_scatter/
    └── load_spectrum/
```

#### 2. Environmental Matrix
```
environmental_conditions/
├── metocean/
│   ├── wave_Hs2.5_Tp8/
│   ├── wave_Hs5.0_Tp10/
│   └── wave_Hs7.5_Tp12/
├── current/
│   ├── current_0.5ms/
│   └── current_1.5ms/
└── wind/
    ├── wind_10ms/
    └── wind_25ms/
```

#### 3. Design Variations
```
design_variations/
├── geometry/
│   ├── config_A_baseline/
│   ├── config_B_modified/
│   └── config_C_optimized/
├── materials/
│   ├── steel_S355/
│   └── steel_S420/
└── dimensions/
    ├── thickness_nominal/
    └── thickness_corroded/
```

#### 4. Time Series Patterns
```
time_series/
├── snapshots/
│   ├── t0000_initial/
│   ├── t0100_1hour/
│   ├── t0600_6hours/
│   └── t2400_24hours/
└── intervals/
    ├── startup_phase/
    ├── steady_state/
    └── shutdown_phase/
```

## Batch Execution Support

### Batch Configuration Templates
Preserve and create batch execution templates:

#### BATCH_TEMPLATE.yml
```yaml
batch_execution:
  # Processing configuration
  processing:
    parallel: true
    max_workers: 20
    chunk_size: auto
    priority_cases: [extreme_cases, baseline]
    
  # Resource management  
  resources:
    max_memory_gb: 32
    cpu_cores: 8
    gpu_enabled: false
    license_pool_size: 4
    
  # Execution strategy
  strategy:
    order: priority_first  # priority_first, sequential, random
    error_handling: continue_on_failure
    checkpoint_interval: 10  # Save progress every N cases
    retry_failed: true
    max_retries: 3
    
  # Progress tracking
  monitoring:
    progress_bar: true
    log_level: INFO
    status_file: batch_status.json
    email_notification: on_completion
    
  # Output configuration
  outputs:
    results_directory: ./results/{timestamp}/
    aggregate_results: true
    compress_outputs: true
    generate_report: true
```

### Parallel Execution Patterns
```yaml
parallel_patterns:
  independent_cases:
    description: "Each case runs independently"
    suitable_for: [parameter_sweeps, monte_carlo]
    max_parallel: unlimited
    
  staged_pipeline:
    description: "Cases depend on previous stages"
    suitable_for: [iterative_solutions, coupled_analyses]
    stages:
      - preprocessing: {parallel: 10}
      - main_analysis: {parallel: 4}
      - postprocessing: {parallel: 10}
      
  resource_constrained:
    description: "Limited by licenses or memory"
    suitable_for: [large_models, licensed_software]
    queue_management: true
    resource_pools:
      - licenses: 4
      - high_memory_nodes: 2
```

## Agent Overview Documentation

### AGENT_OVERVIEW.md Structure
This file provides AI agents with a quick, comprehensive understanding of the go-by folder. It should be the FIRST file agents read when encountering the folder.

#### Required Content for AGENT_OVERVIEW.md:
```markdown
# 🤖 Agent Quick Start Guide

## Purpose
[Brief description of what this go-by folder represents]

## Source Information
- **Original Folder**: [path to source]
- **Creation Date**: [ISO-8601 timestamp]
- **Total Files**: [count]
- **Total Size**: Original: [size] | Go-By: [size]
- **Compression Ratio**: [percentage]

## Quick Navigation
- Original preserved files: `./_originals/`
- Template patterns: `./_templates/`
- Sample files: `./_samples/`
- Main structure: `./[folders]/`

## File Types Present
| Extension | Count | Original Example | Purpose |
|-----------|-------|------------------|---------|
| .yml      | 45    | config.yml       | Configuration files |
| .csv      | 120   | data_001.csv     | Time series data |
| .sim      | 25    | model.sim        | Simulation models |

## Key Patterns Detected
### Naming Conventions
- Parameter sweeps: `analysis_{param}_{value}`
- Time series: `output_t{timestamp}`
- Configurations: `config_{variant}`

### Variation Dimensions
- **Loading**: 10%, 25%, 50%, 75%, 100%
- **Environment**: calm, operational, storm
- **Materials**: steel_S355, steel_S420

## Workflow Structure
1. **Input Stage**: `/inputs/` - Raw data and configurations
2. **Processing Stage**: `/processing/` - Analysis and calculations  
3. **Output Stage**: `/outputs/` - Results and reports

## How to Use This Go-By
### For Understanding Structure
1. Review this AGENT_OVERVIEW.md first
2. Check `_originals/` for authentic file formats
3. Examine VARIATION_MAPPING.yml for parameter patterns

### For Generating New Variations
1. Use templates in `_templates/`
2. Follow patterns in BATCH_TEMPLATE.yml
3. Reference naming conventions

### For Reproducing Analysis
1. Start with baseline configuration in `_originals/`
2. Apply variation patterns from VARIATION_MAPPING.yml
3. Execute using BATCH_TEMPLATE.yml workflow

## Important Files to Review
1. **AGENT_OVERVIEW.md** - This file (start here)
2. **GO_BY_MAPPING.json** - Detailed file mappings
3. **VARIATION_MAPPING.yml** - Parameter variation patterns
4. **_originals/ORIGINALS_INDEX.md** - List of preserved files
5. **WORKFLOW_STRUCTURE.yml** - Processing pipeline

## Domain Context
[Include domain-specific information]
- Industry: [e.g., Offshore Engineering]
- Analysis Type: [e.g., Hydrodynamic Analysis]
- Software Used: [e.g., OrcaFlex, ANSYS]
- Standards: [e.g., API, DNV]

## Quick Stats
- Unique file patterns: [count]
- Directory depth: [max depth]
- Largest preserved original: [filename, size]
- Most common file type: [extension, count]
- Parameter combinations: [total possible]

## Agent Instructions
When working with this go-by folder:
1. ✅ Use preserved originals as format reference
2. ✅ Follow established naming patterns
3. ✅ Maintain variation consistency
4. ❌ Don't modify files in `_originals/`
5. ❌ Don't break established patterns
```

### Auto-Generation Requirements
The AGENT_OVERVIEW.md should be:
- **Auto-generated** during go-by creation
- **Comprehensive** but concise (2-3 pages max)
- **Structured** for quick scanning
- **Action-oriented** with clear next steps
- **Cross-referenced** with other metadata files

## Original File Preservation Strategy

### Selection Criteria for Original Files
1. **First Instance Rule**: Select the first occurrence of each file pattern
2. **Type Coverage**: Ensure one original for each unique file extension
3. **Pattern Representation**: One original for each naming pattern variant
4. **Size Categories**: Preserve originals from different size categories (small/medium/large)

### Preservation Methods
```yaml
preservation_strategy:
  location: "_originals/"
  naming: 
    - keep_original_name: true
    - add_metadata_suffix: false  # Don't modify original files
  
  selection_rules:
    by_extension:
      ".csv": "first_occurrence"
      ".yml": "first_occurrence" 
      ".sim": "smallest_valid_file"  # For large binaries
      ".dat": "baseline_configuration"
    
    by_pattern:
      "analysis_*": "analysis_001"  # Keep first in sequence
      "*_baseline*": "keep_all"     # Preserve all baseline files
      "config_*": "config_default"  # Keep default configuration
```

### Original File Index
Create `_originals/ORIGINALS_INDEX.md` documenting:
```markdown
# Preserved Original Files

## File Inventory
| Original File | Type | Size | Purpose | Variations Count |
|--------------|------|------|---------|------------------|
| analysis_001.dat | Data | 45MB | Time series data | 100 files |
| config_baseline.yml | Config | 2KB | Default configuration | 25 variants |
| model.sim | Binary | 180MB | Simulation model | 1 file |

## Usage Notes
- These files are bit-for-bit identical to source
- Use for format validation and structure reference
- Do not modify these files
```

## Implementation Steps

1. **Scan and analyze** the source folder:
   - Build complete file tree
   - Identify file type patterns
   - Calculate size distributions
   - Detect naming conventions
   - **Mark first instance of each pattern for preservation**

2. **Create preservation strategy**:
   - **Select original files to preserve unchanged**
   - Group remaining similar files for representation
   - Select representatives from each group
   - Identify edge cases and variants

3. **Generate go-by structure**:
   - Create directory skeleton
   - **Copy original files to `_originals/` without modification**
   - Place representative files (modified/minimized)
   - Generate stub files for large items
   - Create metadata files
   - **Generate AGENT_OVERVIEW.md as the primary entry point**

4. **Optimize for usage**:
   - **Create AGENT_OVERVIEW.md for AI agents to quickly understand structure**
   - Add navigation aids (README with tree view)
   - **Create index of preserved originals**
   - Include usage examples
   - Provide reconstruction instructions
   - **Ensure all documentation is agent-friendly with clear structure**

## Progress Reporting

### Real-Time Progress Display
- **Progress bar**: Shows [Current file / Total files] with percentage
- **Current operation**: Display current file being processed
- **Rate calculation**: Files/second and estimated time remaining
- **Memory monitoring**: Current memory usage and peak usage
- **Size tracking**: Original size processed vs go-by size generated

### Progress Output Format
```
Creating go-by folder: [████████████░░░░░░░░] 60% (6,234/10,390 files)
Current: processing data/analysis_042.csv
Rate: 125 files/sec | ETA: 33 seconds
Memory: 287MB / Peak: 341MB
Size: Original 2.8GB → Go-by 18MB (0.64%)
```

### Checkpoint Saves
- Automatic checkpoint every 1000 files
- Checkpoint location: `target_folder/.go_by_checkpoint/`
- Resume capability with `--resume` flag
- Checkpoint includes progress state and decisions made

## Performance Guidelines

### Expected Performance by Source Size
| Source Size | Scan Time | Generation Time | Memory Usage | Go-By Size |
|-------------|-----------|-----------------|--------------|------------|
| <100 MB     | <5 sec    | <10 sec        | <50 MB       | <1 MB      |
| 100MB-1GB   | 5-30 sec  | 10-60 sec      | 50-200 MB    | 1-10 MB    |
| 1-5 GB      | 30s-2 min | 1-5 min        | 200-500 MB   | 10-50 MB   |
| 5-10 GB     | 2-5 min   | 5-15 min       | 500MB-1GB    | 50-100 MB  |

### Performance Optimization Tips
- Use `--exclude-patterns` to skip large binary directories
- Enable `--parallel-scan` for multi-core systems (auto-detected)
- Set `--max-file-size` to limit representative file sizes
- Use `--quick-mode` for rapid structural overview only

### Bottleneck Indicators
- **I/O bound**: Progress slows on network drives → copy locally first
- **Memory bound**: Swap usage increases → reduce parallel workers
- **CPU bound**: High CPU with fast disk → normal operation

## Advanced Usage Examples

### Standard Usage
```bash
# Basic usage with required arguments
create-go-by --source-folder ./large-project --target-folder ./large-project-go-by

# With additional options
create-go-by \
  --source-folder ./large-project \
  --target-folder ./go-by \
  --max-file-size 10KB \
  --preserve-recent 30d \
  --include-patterns "*.config,*.yaml" \
  --variation-coverage high
```

### Analysis-Specific Examples

#### 1. Engineering Simulation Project
```bash
create-go-by \
  --source-folder ./orcaflex-simulations \
  --target-folder ./templates/orcaflex-go-by \
  --analysis-mode true \
  --preserve-variations true \
  --capture-workflow true \
  --batch-templates true

# Result: Compact template with variation patterns for 1,800+ simulation files
```

#### 2. Parameter Sweep Analysis
```bash
create-go-by \
  --source-folder ./sensitivity-analysis \
  --target-folder ./templates/sensitivity-template \
  --parameter-detection auto \
  --matrix-sampling "extremes+nominal" \
  --generate-variation-script true

# Result: Template with parameter ranges and generation scripts
```

#### 3. Time-Series Simulation
```bash
create-go-by \
  --source-folder ./cfd-results \
  --target-folder ./templates/cfd-template \
  --time-series-sampling logarithmic \
  --preserve-convergence-history true \
  --include-benchmarks true

# Result: Efficiently sampled time series with performance metrics
```

#### 4. Multi-Stage Pipeline
```bash
create-go-by \
  --source-folder ./analysis-pipeline \
  --target-folder ./templates/pipeline-template \
  --map-dependencies true \
  --preserve-stage-order true \
  --batch-configuration extract

# Result: Complete pipeline template with dependency graph
```

#### 5. Monte Carlo Simulation
```bash
create-go-by \
  --source-folder ./monte-carlo-10k \
  --target-folder ./templates/monte-carlo-template \
  --statistical-sampling true \
  --preserve-seeds true \
  --sample-size 100

# Result: Statistical representation of 10,000 runs in 100 files
```

### Real-world Examples
# 1. Create template from Node.js project
create-go-by --source-folder ./my-react-app --target-folder ./templates/react-app-template

# 2. Create go-by from data science project  
create-go-by --source-folder ~/notebooks/ml-pipeline --target-folder ~/templates/ml-pipeline-go-by

# 3. Create reference from microservice
create-go-by --source-folder ./services/user-service --target-folder ./docs/examples/user-service-template

## Validation Rules

### Pre-Execution Validation
Automatic checks before processing:
- [ ] Source folder exists and is readable
- [ ] Source folder size <10GB (or user confirmed for larger)
- [ ] Parent directory of target_folder exists and is writable
- [ ] Sufficient disk space available (estimate: source_size * 0.01 + 100MB buffer)
- [ ] No circular symbolic links detected
- [ ] Required permissions for file operations
- [ ] Valid argument combinations provided

### Runtime Validation
Continuous checks during processing:
- [ ] Memory usage within limits (<2GB default, configurable)
- [ ] Disk space remains sufficient
- [ ] File access permissions maintained
- [ ] No corrupted files encountered
- [ ] Progress advancing (timeout detection)

### Post-Execution Validation
Final verification checks:
- [ ] All required metadata files created
- [ ] At least one original preserved per file type
- [ ] AGENT_OVERVIEW.md is valid markdown
- [ ] Directory structure matches source
- [ ] Go-by size is <1% of original (excluding _originals/)
- [ ] No empty directories unless intentional
- [ ] All symlinks documented if present

## Error Handling & Recovery

### Automatic Recovery Features
- **Checkpoint System**: Save state every 1000 files
- **Partial Completion**: Valid go-by even if interrupted
- **Retry Logic**: Automatic retry (3x) for transient failures
- **Graceful Degradation**: Skip inaccessible files with logging

### Error Recovery Commands
```bash
# Resume from checkpoint after interruption
create-go-by --resume --target-folder ./my-go-by

# Rollback failed attempt
create-go-by --rollback --target-folder ./my-go-by

# Verify and repair incomplete go-by
create-go-by --repair --target-folder ./my-go-by
```

### Common Errors and Solutions
| Error Type | Common Cause | Solution |
|------------|--------------|-----------|
| PermissionError | Insufficient rights | Run elevated or use `--skip-inaccessible` |
| DiskFullError | Insufficient space | Clear space or use `--max-go-by-size` limit |
| MemoryError | Large file/folder | Use `--streaming-mode` or `--low-memory` |
| TimeoutError | Network/slow disk | Increase timeout with `--timeout` |
| EncodingError | Mixed encodings | Use `--force-utf8` or `--skip-encoding-errors` |

### Error Reporting
- Detailed error log: `target_folder/GO_BY_ERRORS.log`
- Summary in AGENT_OVERVIEW.md if errors occurred
- Non-fatal errors don't stop processing
- Fatal errors create partial go-by with error documentation

## Validation Checklist

### Standard Checks
Before execution, verify:
- [ ] Source folder exists and is accessible
- [ ] Target folder path is valid (parent directory exists)
- [ ] Sufficient disk space for go-by folder (typically <10MB)
- [ ] Read permissions on source folder
- [ ] Write permissions on target folder location

### Analysis-Specific Checks
For analysis folders, additionally verify:
- [ ] Parameter patterns are detectable (naming conventions)
- [ ] Workflow stages are identifiable (folder structure)
- [ ] Batch configurations exist (if applicable)
- [ ] Result files follow consistent patterns
- [ ] Dependencies between files can be traced
- [ ] Variation count is manageable (<10,000 recommended)
- [ ] Critical configuration files are present

## Enhanced Results Organization

### Smart Result Sampling
For analysis result folders, apply intelligent sampling:

#### Result File Patterns
```yaml
result_sampling:
  time_series_outputs:
    strategy: "logarithmic_sampling"
    keep_points: [1, 2, 5, 10, 20, 50, 100, 200, 500, 1000]
    
  convergence_history:
    strategy: "keep_milestones"
    milestones: [initial, 10%, 50%, 90%, converged]
    
  sensitivity_results:
    strategy: "keep_extremes_and_baseline"
    preserve: [min_response, max_response, nominal]
    
  statistical_outputs:
    strategy: "summary_statistics"
    keep: [mean, std, min, max, p50, p95, p99]
```

#### Aggregation Templates
Create templates for result aggregation:
```python
# result_aggregation_template.py
class ResultAggregator:
    def aggregate_parameter_sweep(self, result_files):
        """Aggregate results from parameter sweeps"""
        # Load all results
        # Create summary DataFrame
        # Generate comparison plots
        # Export to summary format
```

### Performance Benchmarks
Include performance data in metadata:
```json
"performance_benchmarks": {
  "single_case_runtime": "5.2 minutes",
  "parallel_speedup": 18.5,
  "memory_peak": "4.8 GB",
  "disk_io_rate": "120 MB/s",
  "convergence_iterations": 42
}
```

## Success Criteria

### General Criteria
- [ ] Go-by folder is < 1% of original size (excluding preserved originals)
- [ ] All directory paths are represented
- [ ] **Each file type has ONE ORIGINAL FILE preserved unchanged**
- [ ] **Original files are clearly marked and indexed in `_originals/`**
- [ ] **AGENT_OVERVIEW.md exists as primary entry point for AI agents**
- [ ] **Agent overview provides complete structural understanding in <5 min read**
- [ ] Each file type pattern has at least one example
- [ ] Variation files are properly minimized/represented
- [ ] Metadata allows understanding of original structure
- [ ] Can be used as a template for similar projects
- [ ] Captures organizational patterns and conventions
- [ ] Easy to navigate and understand for both humans and AI agents

### Analysis-Specific Criteria
- [ ] Parameter variations are documented and reproducible
- [ ] Analysis workflow is clearly mapped
- [ ] Batch execution templates are functional
- [ ] Variation generator can recreate full matrix
- [ ] Result aggregation patterns are preserved
- [ ] Performance benchmarks are captured
- [ ] Dependencies between files are documented
- [ ] Resource requirements are specified

## Notes

### General Guidelines
- Prioritize structure over content
- Focus on patterns rather than specific instances
- Include enough context to understand purpose
- Make it self-documenting with clear metadata
- Consider version control efficiency (small text files)

### Analysis-Specific Guidelines
- **Reproducibility First**: Ensure variations can be regenerated
- **Parameter Documentation**: Capture all parameter ranges and units
- **Workflow Preservation**: Maintain pipeline execution order
- **Resource Requirements**: Document computational needs
- **Convergence Criteria**: Preserve numerical tolerances
- **Validation Data**: Keep benchmark comparison points
- **Performance Metrics**: Include runtime and scaling data
- **Dependency Management**: Map all file relationships

### Best Practices for Analysis Folders
1. **Use Consistent Naming**: Follow patterns like `{parameter}_{value}_{unit}`
2. **Document Units**: Always include units in metadata
3. **Preserve Seeds**: Keep random seeds for reproducibility  
4. **Sample Intelligently**: Use logarithmic or statistical sampling
5. **Track Versions**: Include software versions in metadata
6. **Enable Automation**: Create templates that can regenerate analyses
7. **Think Scalability**: Design templates that work for larger datasets
8. **Include Validation**: Keep test cases and expected results