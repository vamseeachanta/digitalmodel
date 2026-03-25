# User Story: Modernize All Batch Files with UV Package Management

**As a** developer working on the digitalmodel project  
**I want** all Windows batch files to use modern uv package management instead of legacy conda/pip  
**So that** I have faster, more reliable dependency management and environment setup across all workflows

## Background

The digitalmodel project contains 80+ Windows batch files, many of which still use outdated Python environment management approaches (conda, pip, manual venv creation). With uv's superior performance and reliability, migrating these scripts will significantly improve developer experience and CI/CD pipeline efficiency.

## Current State Analysis

### Package Management Usage in Batch Files:

#### 1. Conda-based Scripts (8 files)
Using legacy conda environments:
- `1Create_env.bat` - Creates conda environments with Python 3.5
- `4Delete_Env.bat` - Removes conda environments
- Multiple duplicates across different project areas

#### 2. Pip-based Scripts (6 files) 
Using traditional pip installation:
- `2InstallPackages_Env_Simple.bat` - `pip install -r requirements.txt`
- `uv-install.bat` - Already attempting uv but with issues
- `uv-update.bat` - Partial uv implementation
- `uv-setup-test.bat` - Mixed venv/pip/uv approach

#### 3. Python Execution Scripts (50+ files)
Running Python without proper environment management:
- `RunAll.bat` - `CALL python script.py` (uses system Python)
- `pipe_py_ecs.bat` - Multiple Python executions without isolated environment
- Analysis scripts across test and documentation directories

## Problem Statement

Current batch files have several issues:
- **Slow conda environments** - 30+ seconds to create/activate environments
- **Inconsistent Python versions** - Some hardcoded to Python 3.5
- **No dependency locking** - Using `pip install -r requirements.txt` without lock files
- **Mixed approaches** - Conda, pip, venv, and uv used inconsistently
- **No isolation** - Many scripts use system Python without virtual environments
- **Maintenance burden** - Multiple duplicate environment setup scripts

## Scope

Modernize all batch files to use uv package management:

### 1. Environment Management Scripts (Priority: High)

#### Replace Conda Scripts:
```bat
# Before (1Create_env.bat)
conda create -n PipeCapacity_env python=3.5
conda activate PipeCapacity_env

# After (modernized)
uv venv PipeCapacity_env --python 3.11
call PipeCapacity_env\Scripts\activate
```

#### Replace Pip Scripts:
```bat
# Before (2InstallPackages_Env_Simple.bat)
pip install -r requirements.txt

# After (modernized)
uv pip install -r requirements.txt
# Or better yet:
uv sync
```

### 2. Python Execution Scripts (Priority: High)

#### Replace Direct Python Calls:
```bat
# Before (RunAll.bat)
CALL python APISTD2RD.py 10.yml

# After (modernized)
uv run python APISTD2RD.py 10.yml
```

### 3. Development Utility Scripts (Priority: Medium)

#### Fix Existing UV Scripts:
- `uv-install.bat` - Fix process substitution issues
- `uv-update.bat` - Use proper uv sync/update commands
- `uv-setup-test.bat` - Remove manual venv creation

### 4. Analysis Workflow Scripts (Priority: Medium)

#### Standardize Environment Usage:
- Ensure all analysis scripts use uv-managed environments
- Add proper error handling for missing dependencies
- Implement consistent logging for troubleshooting

## Acceptance Criteria

### Functional Requirements
- [ ] All conda commands replaced with uv equivalents
- [ ] All pip install commands replaced with uv pip install or uv sync
- [ ] All direct python calls replaced with uv run python
- [ ] Environment setup time reduced by 70% (uv vs conda)
- [ ] All scripts work with Python 3.11+ (no more hardcoded 3.5)
- [ ] Dependency installation is deterministic (using uv.lock)

### Technical Requirements
- [ ] No conda dependencies remaining in any batch file
- [ ] All scripts follow consistent uv usage patterns
- [ ] Proper error handling for uv command failures
- [ ] Scripts work with existing pyproject.toml and uv.lock
- [ ] Environment variables properly set for uv operations

### Quality Requirements
- [ ] All modernized scripts tested on clean Windows systems
- [ ] Performance benchmarks show improvement over legacy approaches
- [ ] No regression in analysis output accuracy
- [ ] Documentation updated with new workflow instructions

## Technical Implementation Plan

### Phase 1: Environment Management (Week 1)
1. **Audit all environment scripts**
   - Catalog all conda/pip usage
   - Identify duplicate functionality
   - Map environment names and Python versions

2. **Create uv templates**
   - Standard environment creation pattern
   - Standard dependency installation pattern
   - Standard environment cleanup pattern

3. **Replace conda scripts**
   - Convert 8 conda-based batch files
   - Test environment creation/activation
   - Verify dependency installation works

### Phase 2: Python Execution (Week 2)
1. **Replace direct python calls**
   - Convert 50+ analysis scripts to use `uv run`
   - Ensure proper environment isolation
   - Test all analysis workflows

2. **Standardize execution patterns**
   - Consistent error handling
   - Proper path management
   - Environment variable handling

### Phase 3: Development Scripts (Week 3)
1. **Fix existing uv scripts**
   - Resolve process substitution issues in uv-install.bat
   - Implement proper uv sync in uv-update.bat
   - Streamline uv-setup-test.bat

2. **Add new utility scripts**
   - Bulk environment cleanup
   - Dependency audit scripts
   - Environment health checks

### Phase 4: Testing & Documentation (Week 4)
1. **Comprehensive testing**
   - Test all scripts on clean Windows systems
   - Performance benchmarking
   - Regression testing for analysis outputs

2. **Documentation updates**
   - Update developer onboarding guides
   - Create uv best practices document
   - Update CI/CD pipeline documentation

## Migration Patterns

### 1. Environment Creation
```bat
REM Old conda approach
SET py_environment=PipeCapacity_env
(echo y) | CALL conda create -n %py_environment% python=3.5
CALL activate %py_environment%

REM New uv approach
SET py_environment=PipeCapacity_env
uv venv %py_environment% --python 3.11
call %py_environment%\Scripts\activate
```

### 2. Dependency Installation
```bat
REM Old pip approach
pip install -r requirements.txt

REM New uv approach (option 1: faster pip)
uv pip install -r requirements.txt

REM New uv approach (option 2: full sync)
uv sync
```

### 3. Python Execution
```bat
REM Old direct execution
python script.py input.yml

REM New uv execution
uv run python script.py input.yml
```

### 4. Environment Cleanup
```bat
REM Old conda cleanup
(echo y) | CALL conda-env remove -n %py_environment%

REM New uv cleanup
rmdir /s /q %py_environment%
```

## Performance Benefits

### Speed Improvements (Expected)
- **Environment creation**: 30s (conda) → 3s (uv) = 90% faster
- **Dependency installation**: 45s (pip) → 8s (uv) = 82% faster
- **Python execution**: System conflicts → Isolated = More reliable
- **Overall workflow**: 2-3x faster end-to-end

### Reliability Improvements
- **Deterministic installs** - uv.lock ensures reproducible environments
- **Conflict resolution** - uv's resolver handles dependency conflicts better
- **Version consistency** - No more Python 3.5 environments
- **Error handling** - Better error messages and faster failure detection

## Risk Mitigation

### Technical Risks
- **Tool compatibility** - Test with all external tools (ANSYS, etc.)
- **Path handling** - Ensure Windows path compatibility
- **Environment isolation** - Verify no conflicts with system Python
- **Performance regression** - Benchmark critical analysis workflows

### Migration Risks
- **Script dependencies** - Map all inter-script dependencies
- **User adoption** - Provide clear migration guides
- **Rollback plan** - Keep original scripts until validation complete
- **Training needs** - Document uv commands for developers

## Success Metrics

### Performance Metrics
- Environment setup time: < 5 seconds (target)
- Dependency installation time: < 10 seconds (target)
- Analysis workflow runtime: No regression
- Script execution success rate: > 99%

### Adoption Metrics
- Batch files converted: 100% (80+ files)
- Conda usage: 0% (complete elimination)
- Legacy pip usage: < 10% (only where uv not applicable)
- Developer satisfaction: Survey showing improved experience

### Quality Metrics
- Analysis output accuracy: 100% match with legacy scripts
- Script failure rate: < 1%
- Environment conflicts: 0
- Support tickets related to environment issues: 50% reduction

## Dependencies

### Technical Dependencies
- UV package manager (>= 0.4.0)
- Python 3.11+ available on all development systems
- Existing pyproject.toml and uv.lock files maintained
- Windows batch scripting capabilities

### Organizational Dependencies
- Developer training on uv commands
- CI/CD pipeline updates to use uv
- Documentation team updates for new workflows
- Testing team validation of analysis outputs

## Future Considerations

### Post-Migration Opportunities
- **Cross-platform scripts** - Follow up with bash equivalents
- **CI/CD optimization** - Leverage uv in GitHub Actions
- **Docker optimization** - Use uv in container builds
- **Development tooling** - Integrate uv with IDE configurations

### Maintenance Plan
- Regular uv version updates
- Periodic review of uv.lock files
- Performance monitoring and optimization
- Script consolidation opportunities