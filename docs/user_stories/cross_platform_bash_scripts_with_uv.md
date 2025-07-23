# User Story: Cross-Platform Script Compatibility with UV

**As a** developer working on the digitalmodel project across different operating systems  
**I want** bash script equivalents for all DOS batch files that utilize uv package management  
**So that** I can execute the same automation workflows on Linux/macOS environments with modern Python tooling

## Background
The project currently has 80+ DOS batch files that limit cross-platform development. Many use legacy conda/pip approaches while the project is moving toward uv for faster, more reliable dependency management.

## Current State Analysis

### Batch File Categories Found:
1. **UV-based scripts** (3 files in `/scripts/`) - already using uv but could be improved
2. **Environment management** (15+ files) - using conda/venv, could migrate to uv
3. **Analysis execution** (50+ files) - running Python scripts and external tools
4. **Development utilities** (10+ files) - git operations, documentation generation

### Key Files Requiring Conversion:

#### Scripts Directory (`/scripts/`)
- `uv-install.bat` - Fix process substitution issues, use `uv sync`
- `uv-update.bat` - Implement proper uv-based updates
- `uv-setup-test.bat` - Replace venv creation with `uv venv`
- `daily_routine.bat` - Convert DOS date formatting to bash

#### Environment Management (Multiple locations)
- `1Create_env.bat` - Replace conda with `uv venv`
- `2InstallPackages_Env_Simple.bat` - Convert to `uv pip install`
- `4Delete_Env.bat` - Update for uv environment cleanup

#### Analysis Execution (Tests and docs directories)
- `pipe_py_ecs.bat` - Multiple Python script executions
- `RunAll.bat` - Batch Python script processing
- `aqwa_anl_raos_*.bat` - ANSYS/AQWA analysis runs

## Scope

Create bash script equivalents (`.sh`) for all `.bat` files, organized by functionality:

### 1. Enhanced UV Package Management (`/scripts/`)
- **uv-install.sh** - Fix process substitution issues, use `uv sync`
- **uv-update.sh** - Implement proper uv-based updates  
- **uv-setup-test.sh** - Replace venv creation with `uv venv`
- **daily_routine.sh** - Convert DOS date formatting to bash `$(date +%Y%m%d)`

### 2. Environment Management (15+ files)
- Replace conda environments with `uv venv`
- Convert `pip install -r requirements.txt` to `uv pip install -r requirements.txt`
- Use `uv sync` for reproducible installations
- Implement proper bash error handling (`set -e`)

### 3. Analysis Execution Scripts (50+ files)
- Replace `CALL python script.py` with `uv run python script.py`
- Convert batch loops to bash arrays for multiple file processing
- Maintain same analysis workflow logic
- Add proper path handling for cross-platform compatibility

### 4. Development Utilities (10+ files)
- Convert DOS date formatting (`%date:~10,4%`) to bash `$(date +%Y%m%d)`
- Replace `CALL` commands with direct bash execution
- Modernize git workflows
- Add proper error checking and logging

## Acceptance Criteria

- [ ] Every `.bat` file has a corresponding `.sh` script in the same directory
- [ ] All bash scripts use `uv` commands where applicable (`uv run`, `uv sync`, `uv pip install`)
- [ ] Scripts are executable on Linux/macOS/Git Bash (`chmod +x`)
- [ ] Analysis workflows produce identical results across platforms
- [ ] Environment management is faster with uv vs conda
- [ ] Scripts follow bash best practices (error handling, proper quoting)
- [ ] Documentation updated with cross-platform usage instructions
- [ ] CI/CD pipeline can run on both Windows and Linux

## Technical Approach

### 1. Batch Processing
Group similar batch files for efficient conversion:
- Environment management scripts (conda → uv)
- Analysis execution scripts (python calls → uv run)
- Utility scripts (git, documentation)

### 2. UV Migration Pattern
Replace all pip/conda commands with uv equivalents:
```bash
# Before (conda)
conda create -n env_name python=3.5
conda activate env_name
pip install -r requirements.txt

# After (uv)
uv venv env_name --python 3.5
source env_name/bin/activate  # or uv venv activate
uv pip install -r requirements.txt
```

### 3. Cross-Platform Path Handling
Use bash-compatible path variables and proper quoting:
```bash
# Use forward slashes, proper variable expansion
working_dir="${PWD}/data"
uv run python "${working_dir}/script.py"
```

### 4. Error Handling
Add proper bash error checking:
```bash
#!/bin/bash
set -e  # Exit on error
set -u  # Exit on undefined variable
set -o pipefail  # Exit on pipe failure
```

### 5. File Permissions
Ensure all scripts are executable:
```bash
chmod +x *.sh
```

## Benefits

- **Cross-platform compatibility** - Windows/Linux/macOS support
- **Faster dependency management** - uv is significantly faster than pip/conda
- **Modern Python tooling** - Adoption of cutting-edge package management
- **Reduced Windows dependencies** - Less reliance on Windows-specific tooling
- **Improved CI/CD** - Better pipeline compatibility across platforms
- **Developer experience** - Consistent workflows regardless of OS
- **Future-proofing** - Moving toward modern Python ecosystem standards

## Implementation Priority

### High Priority
1. Core UV scripts in `/scripts/` directory
2. Environment management scripts (most frequently used)
3. Main analysis execution scripts

### Medium Priority
1. Test execution scripts
2. Documentation generation scripts
3. Utility scripts

### Low Priority
1. Legacy/rarely used analysis scripts
2. Vendor-specific tool executions
3. One-off utility scripts

## Success Metrics

- All 80+ batch files have bash equivalents
- CI/CD pipeline runs successfully on both Windows and Linux
- Environment setup time reduced by 50% (uv vs conda)
- Zero regression in analysis output accuracy
- Developer onboarding time reduced (cross-platform compatibility)

## Dependencies

- UV package manager installed on target systems
- Bash shell available (native on Linux/macOS, Git Bash on Windows)
- Python 3.8+ for uv compatibility
- Existing Python project structure maintained

## Risk Mitigation

- **Path compatibility** - Use forward slashes and proper variable expansion
- **Command differences** - Test scripts on all target platforms
- **Tool availability** - Verify external tools (ANSYS, etc.) work with bash execution
- **Performance** - Benchmark uv vs existing approaches
- **Rollback plan** - Keep original batch files until bash scripts are fully validated