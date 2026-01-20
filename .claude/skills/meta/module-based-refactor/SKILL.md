---
name: module-based-refactor
description: Reorganize a repository from flat structure to module-based 5-layer architecture. Use for codebase restructuring, module organization, import path updates, git history preservation, artifact cleanup, and directory consolidation.
version: 2.0.0
updated: 2025-01-20
category: meta
triggers:
- repository reorganization
- flat to module structure
- codebase restructuring
- module-based architecture
- import path migration
- git mv refactor
- 5-layer architecture
- artifact cleanup
- directory consolidation
- hidden folder cleanup
---

# Module-Based Refactor Skill

Reorganize a repository from flat structure to a consistent module-based 5-layer architecture while preserving git history. Includes comprehensive cleanup of artifacts, runtime data, and hidden folders.

## Version Metadata

```yaml
version: 2.0.0
python_min_version: '3.10'
dependencies: []
compatibility:
  tested_python:
  - '3.10'
  - '3.11'
  - '3.12'
  - '3.13'
  os:
  - Windows
  - Linux
  - macOS
```

## When to Use

- Reorganizing a flat repository structure to module-based layout
- Consolidating scattered modules into a unified hierarchy
- Standardizing project structure across multiple layers (src, tests, specs, docs, examples)
- Migrating import paths while preserving git history
- Creating a scalable architecture for growing codebases
- Cleaning up root-level artifacts (log files, temp files, build artifacts)
- Consolidating duplicate directories (agents/, coordination/, memory/)
- Reviewing and removing obsolete hidden folders
- Relocating test outputs and prototype code

## Pre-flight Checks

**CRITICAL**: Run these checks before starting any reorganization.

### 1. Git Tracking Status

```bash
# Check what is tracked vs untracked
git ls-files | head -50
git ls-files --others --exclude-standard | head -50

# List all untracked files at root level
git ls-files --others --exclude-standard | grep -v "/"

# Check for ignored files that might need cleanup
git status --ignored
```

### 2. Duplicate Directory Detection

```bash
# Find duplicate agent directories
ls -la agents/ .claude/agents/ .claude/agent-library/ 2>/dev/null

# Find duplicate coordination/memory directories
ls -la coordination/ memory/ .claude-flow/ 2>/dev/null

# Find duplicate skill locations
find . -type d -name "skills" 2>/dev/null
```

### 3. Identify Runtime vs Config Files

```bash
# Runtime data (should NOT be in git)
find . -name "*.log" -o -name "*.tmp" -o -name "__pycache__" 2>/dev/null

# Config files (SHOULD be in git)
find . -name "*.json" -o -name "*.yaml" -o -name "*.toml" 2>/dev/null | head -30

# Large files that might be test data
find . -size +1M -type f 2>/dev/null
```

### 4. Hidden Folder Inventory

```bash
# List all hidden folders at root
ls -la .* 2>/dev/null | grep "^d"

# Common hidden folders to review:
# .agent-os/     - May contain duplicate agent configs
# .ai/           - Legacy AI coordination
# .drcode/       - DR Code artifacts
# .claude/       - Claude configuration (KEEP)
# .claude-flow/  - Runtime data (consider .gitignore)
# .git/          - Git data (KEEP)
# .venv/         - Virtual environment (should be ignored)
```

### 5. Identify Stale Artifacts

```bash
# Find log files in root
ls *.log 2>/dev/null

# Find test output files
ls *_output.* test_*.txt *.html 2>/dev/null

# Find build artifacts
ls *.egg-info dist/ build/ 2>/dev/null
```

## Parallel Execution Strategy

### Agent Spawn Patterns

Use parallel subagents for faster reorganization. Tasks that don't have dependencies can run simultaneously.

#### Phase 1: Analysis (PARALLEL)

```javascript
// All analysis tasks can run in parallel
Task("Analyze source structure", "List all directories in src/ and identify modules vs infrastructure", "explorer")
Task("Analyze test structure", "List all directories in tests/ and identify test modules", "explorer")
Task("Analyze specs/docs", "List all directories in specs/ and docs/", "explorer")
Task("Inventory root artifacts", "List all files at root level, identify logs/temp/build artifacts", "explorer")
Task("Hidden folder audit", "List all hidden folders, check git tracking status", "explorer")
```

#### Phase 2: Directory Creation (SEQUENTIAL)

```bash
# Must be sequential - directories must exist before moves
mkdir -p src/<package>/modules
mkdir -p tests/modules
mkdir -p specs/modules
mkdir -p docs/modules
mkdir -p examples/modules
```

#### Phase 3: Module Moves (PARALLEL by module)

```javascript
// Each module can be moved in parallel (no cross-dependencies)
Task("Move aqwa module", "git mv all aqwa files across 5 layers", "coder")
Task("Move catenary module", "git mv all catenary files across 5 layers", "coder")
Task("Move orcaflex module", "git mv all orcaflex files across 5 layers", "coder")
Task("Move mooring module", "git mv all mooring files across 5 layers", "coder")
```

#### Phase 4: Import Updates (PARALLEL by file type)

```javascript
// Import updates can be parallelized by file type
Task("Update source imports", "Update imports in src/**/*.py", "coder")
Task("Update test imports", "Update imports in tests/**/*.py", "coder")
Task("Update example imports", "Update imports in examples/**/*.py", "coder")
```

#### Phase 5: Cleanup (PARALLEL)

```javascript
// Cleanup tasks are independent
Task("Remove root artifacts", "Delete log files, temp files from root", "coder")
Task("Consolidate agent dirs", "Move agents/ content to .claude/agent-library/", "coder")
Task("Clean hidden folders", "Remove obsolete .agent-os/, .ai/, .drcode/", "coder")
Task("Update .gitignore", "Add runtime data patterns to .gitignore", "coder")
```

#### Phase 6: Verification (PARALLEL then SEQUENTIAL)

```javascript
// Parallel verification
Task("Verify imports", "Test all import statements work", "tester")
Task("Verify git status", "Check git status is clean, no broken files", "tester")
Task("Run test suite", "Run pytest to verify nothing broken", "tester")

// Sequential final review
Task("Final structure review", "Compare actual structure to target structure", "reviewer")
```

### Parallel vs Sequential Decision Matrix

| Task Type | Parallel? | Reason |
|-----------|-----------|--------|
| Analysis/Inventory | YES | Read-only, no conflicts |
| Directory creation | NO | Order matters for parent dirs |
| Module moves (same module) | NO | Files may reference each other |
| Module moves (different modules) | YES | Independent file sets |
| Import updates (same file) | NO | Would cause conflicts |
| Import updates (different files) | YES | No file conflicts |
| Cleanup tasks | YES | Independent operations |
| Test execution | NO | May have shared fixtures |
| Verification | MIXED | Some parallel, final review sequential |

## Target Structure

The 5-layer module architecture:

```
src/<package>/modules/<module_name>/
tests/modules/<module_name>/
specs/modules/<module_name>/
docs/modules/<module_name>/
examples/modules/<module_name>/
```

### Example: digitalmodel Package

```
src/digitalmodel/modules/
├── __init__.py
├── aqwa/
│   ├── __init__.py
│   ├── aqwa_reader.py
│   └── aqwa_utilities.py
├── catenary/
│   ├── __init__.py
│   ├── catenary.py
│   └── catenary_riser.py
├── fea_model/
│   ├── __init__.py
│   └── line_components.py
├── mooring/
│   ├── __init__.py
│   └── mooring.py
├── orcaflex/
│   ├── __init__.py
│   └── OrcaFlexAnalysis.py
└── pipe_capacity/
    ├── __init__.py
    └── custom/

tests/modules/
├── conftest.py
├── aqwa/
│   └── test_aqwa_reader.py
├── catenary/
│   └── test_catenary.py
└── orcaflex/
    └── test_orcaflex_analysis.py

specs/modules/
├── .gitkeep
├── aqwa/
│   └── spec.md
└── orcaflex/
    └── spec.md

docs/modules/
├── aqwa/
│   └── README.md
└── orcaflex/
    └── README.md

examples/modules/
├── aqwa/
│   └── basic_analysis.py
└── orcaflex/
    └── batch_processing.py
```

## Process

### Phase 1: Analysis

1. **List all top-level directories**
   ```bash
   ls -la src/<package>/
   ls -la tests/
   ls -la specs/
   ```

2. **Identify modules vs infrastructure**
   - Modules: Domain-specific code directories (e.g., `aqwa/`, `catenary/`, `orcaflex/`)
   - Infrastructure: Support directories that should stay at root level

3. **Document current state**
   ```bash
   # List all Python files to understand module scope
   find src/<package> -name "*.py" -type f | head -50

   # Check for existing module imports
   grep -r "from <package>" --include="*.py" . | head -20
   ```

### Phase 2: Planning

1. **Create migration plan**
   - List all modules to be moved
   - Identify dependencies between modules
   - Plan import path changes

2. **Identify infrastructure folders (keep at root)**
   ```
   # Tests infrastructure
   tests/test_data/
   tests/test_configs/
   tests/fixtures/
   tests/conftest.py (root level)

   # Specs infrastructure
   specs/templates/
   specs/features/

   # Project infrastructure
   config/
   scripts/
   .claude/
   ```

### Phase 3: Create Target Directories

```bash
# Create module directories with git-tracked placeholders
mkdir -p src/<package>/modules
mkdir -p tests/modules
mkdir -p specs/modules
mkdir -p docs/modules
mkdir -p examples/modules

# Add .gitkeep for empty directories
touch specs/modules/.gitkeep
touch docs/modules/.gitkeep
touch examples/modules/.gitkeep
```

### Phase 4: Move with Git History Preservation

**CRITICAL**: Always use `git mv` to preserve history.

```bash
# Move source modules
git mv src/<package>/<module> src/<package>/modules/<module>

# Move test modules
git mv tests/<module> tests/modules/<module>

# Move spec modules
git mv specs/<module> specs/modules/<module>

# Move doc modules
git mv docs/<module> docs/modules/<module>

# Move example modules
git mv examples/<module> examples/modules/<module>
```

#### Example: Moving aqwa module

```bash
# Source
git mv src/digitalmodel/aqwa src/digitalmodel/modules/aqwa

# Tests
git mv tests/aqwa tests/modules/aqwa

# Specs (if exists)
git mv specs/aqwa specs/modules/aqwa

# Docs (if exists)
git mv docs/aqwa docs/modules/aqwa
```

### Phase 5: Update Imports

1. **Search for old import patterns**
   ```bash
   # Find all files with old import paths
   grep -r "from <package>.<module>" --include="*.py" .
   grep -r "import <package>.<module>" --include="*.py" .
   ```

2. **Update import statements**
   ```bash
   # Update imports (example using sed)
   find . -name "*.py" -exec sed -i 's/from digitalmodel.aqwa/from digitalmodel.modules.aqwa/g' {} \;
   find . -name "*.py" -exec sed -i 's/import digitalmodel.aqwa/import digitalmodel.modules.aqwa/g' {} \;
   ```

3. **Common import transformations**
   ```python
   # Before
   from digitalmodel.aqwa import aqwa_reader
   from digitalmodel.catenary.catenary import Catenary

   # After
   from digitalmodel.modules.aqwa import aqwa_reader
   from digitalmodel.modules.catenary.catenary import Catenary
   ```

### Phase 6: Create __init__.py Files

1. **Module root __init__.py**
   ```python
   # src/<package>/modules/__init__.py
   """
   Module-based architecture for <package>.

   Available modules:
   - aqwa: AQWA hydrodynamic analysis
   - catenary: Catenary riser analysis
   - fea_model: FEA model components
   - mooring: Mooring system analysis
   - orcaflex: OrcaFlex simulation
   - pipe_capacity: Pipe capacity calculations
   """

   from digitalmodel.modules import aqwa
   from digitalmodel.modules import catenary
   from digitalmodel.modules import fea_model
   from digitalmodel.modules import mooring
   from digitalmodel.modules import orcaflex
   from digitalmodel.modules import pipe_capacity

   __all__ = [
       "aqwa",
       "catenary",
       "fea_model",
       "mooring",
       "orcaflex",
       "pipe_capacity",
   ]
   ```

2. **Individual module __init__.py**
   ```python
   # src/<package>/modules/<module>/__init__.py
   """
   <Module Name> - Brief description.

   Key classes and functions exported from this module.
   """

   from digitalmodel.modules.<module>.<main_file> import MainClass
   from digitalmodel.modules.<module>.<utils_file> import utility_function

   __all__ = ["MainClass", "utility_function"]
   ```

### Phase 7: Verification

1. **Test imports work**
   ```python
   # test_imports.py
   import sys
   sys.path.insert(0, 'src')

   # Test new import paths
   from digitalmodel.modules.aqwa import aqwa_reader
   from digitalmodel.modules.catenary.catenary import Catenary
   from digitalmodel.modules.orcaflex import OrcaFlexAnalysis

   print("All imports successful!")
   ```

2. **Run test suite**
   ```bash
   uv run pytest tests/ -v
   ```

3. **Check for broken imports**
   ```bash
   # Find any remaining old import patterns
   grep -r "from digitalmodel\." --include="*.py" . | grep -v "modules"
   ```

## Checklist

### Pre-Refactor
- [ ] Git working directory is clean
- [ ] All tests pass before refactor
- [ ] Created backup branch
- [ ] Documented current structure
- [ ] **Pre-flight checks completed** (see Pre-flight Checks section)
- [ ] Duplicate directories identified
- [ ] Runtime vs config files categorized
- [ ] Hidden folders inventoried

### During Refactor

#### Module Structure
- [ ] All modules moved to modules/ subdirectory using `git mv`
- [ ] tests/modules/ created and populated
- [ ] specs/modules/ created (with .gitkeep if empty)
- [ ] docs/modules/ created (with .gitkeep if empty)
- [ ] examples/modules/ created (with .gitkeep if empty)

#### Root-Level Artifact Cleanup
- [ ] Log files removed (*.log)
- [ ] Temp files removed (*.tmp, *.temp)
- [ ] Build artifacts removed (dist/, build/, *.egg-info/)
- [ ] Cache files removed (__pycache__/, .pytest_cache/)
- [ ] Test output files relocated or removed
- [ ] Prototype/experimental code relocated to examples/ or removed

#### Directory Consolidation
- [ ] agents/ content moved to .claude/agent-library/
- [ ] coordination/ content moved to .claude-flow/
- [ ] memory/ content moved to .claude-flow/
- [ ] Duplicate skill directories merged
- [ ] Empty directories removed

#### Hidden Folder Review
- [ ] .agent-os/ - reviewed and cleaned (usually remove)
- [ ] .ai/ - reviewed and cleaned (usually remove)
- [ ] .drcode/ - reviewed and cleaned (usually remove)
- [ ] .claude/ - kept and organized
- [ ] .claude-flow/ - added to .gitignore if runtime data
- [ ] .venv/ - confirmed in .gitignore

#### Test Output Relocation
- [ ] HTML reports moved to tests/reports/ or removed
- [ ] Coverage reports moved to tests/coverage/ or removed
- [ ] Screenshot outputs moved to tests/snapshots/ or removed
- [ ] Benchmark results moved to tests/benchmarks/ or removed

### Post-Refactor
- [ ] `__init__.py` files created with proper exports
- [ ] All import references updated
- [ ] `pyproject.toml` updated if needed
- [ ] Tests pass after refactor
- [ ] No broken imports found
- [ ] Git history preserved (verify with `git log --follow`)
- [ ] **Post-cleanup verification completed** (see Post-cleanup Verification section)
- [ ] .gitignore updated with new patterns
- [ ] No untracked files at root level (except intended ones)

## Common Patterns Found During Reorganization

Based on real-world reorganization experience, these patterns frequently appear:

### 1. Duplicate Agent Directories

```
# Pattern: Multiple locations for agent definitions
agents/                          # Root-level (often untracked)
.claude/agents/                  # Claude-specific
.claude/agent-library/           # Preferred location
.agent-os/agents/                # Legacy AI OS structure

# Resolution: Consolidate to .claude/agent-library/
git mv agents/* .claude/agent-library/
rm -rf agents/
```

### 2. Scattered Runtime Data

```
# Pattern: Runtime data mixed with config
coordination/                    # Untracked runtime state
memory/                          # Untracked memory store
.claude-flow/                    # Runtime data (should be ignored)

# Resolution:
# - Move to .claude-flow/ (single location)
# - Add .claude-flow/ to .gitignore
# - Delete untracked coordination/ and memory/
```

### 3. Untracked Artifacts in Root

```
# Pattern: Various artifacts accumulate in root
*.log                            # Log files from various tools
*.html                           # Test reports, visualizations
*_output.*                       # Output files from scripts
*.sim                            # Simulation files
*.dat                            # Data files

# Resolution:
# - Delete if not needed
# - Move to inputs/ or outputs/ if needed
# - Add patterns to .gitignore
```

### 4. Multiple Skill/Command Locations

```
# Pattern: Skills defined in multiple places
.claude/skills/                  # Claude skills
skills/                          # Root-level skills
.agent-os/skills/                # Legacy location
commands/                        # Alternative naming

# Resolution: Consolidate to .claude/skills/
```

### 5. Legacy Hidden Folders

```
# Pattern: Accumulation of AI tool folders
.agent-os/                       # Custom agent OS (usually obsolete)
.ai/                             # Generic AI folder
.drcode/                         # DR Code specific
.cursor/                         # Cursor editor
.vscode/                         # VS Code settings

# Resolution:
# - .vscode/, .cursor/ - keep if used, add to .gitignore
# - .agent-os/, .ai/, .drcode/ - usually safe to remove
# - Check for any unique configs before deleting
```

### 6. Test Data in Wrong Locations

```
# Pattern: Test data scattered
tests/test_data/                 # Correct location
test_data/                       # Root level (wrong)
data/                            # Ambiguous location
fixtures/                        # Sometimes at root

# Resolution: Move all to tests/test_data/ or tests/fixtures/
```

### 7. Prototype Code Mixed with Production

```
# Pattern: Experimental code in src/
src/package/experimental/        # Should be separate
src/package/prototype_*.py       # Should be in examples/
scripts/scratch_*.py             # Should be in examples/experiments/

# Resolution: Move to examples/experiments/ or examples/prototypes/
```

## Post-cleanup Verification

### 1. Import Verification for Python Modules

```bash
# Create a verification script
cat > verify_imports.py << 'EOF'
#!/usr/bin/env python
"""Verify all imports work after reorganization."""
import sys
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent / "src"))

# Test all module imports
modules_to_test = [
    "digitalmodel",
    "digitalmodel.modules",
    "digitalmodel.modules.aqwa",
    "digitalmodel.modules.catenary",
    "digitalmodel.modules.orcaflex",
    "digitalmodel.modules.mooring",
    "digitalmodel.modules.fea_model",
    "digitalmodel.modules.pipe_capacity",
]

failed = []
for module in modules_to_test:
    try:
        __import__(module)
        print(f"✓ {module}")
    except ImportError as e:
        print(f"✗ {module}: {e}")
        failed.append(module)

if failed:
    print(f"\n{len(failed)} imports failed!")
    sys.exit(1)
else:
    print(f"\nAll {len(modules_to_test)} imports successful!")
EOF

# Run verification
uv run python verify_imports.py
```

### 2. Git Status Review

```bash
# Check for any unintended changes
git status

# Verify no broken file references
git diff --name-status

# Check for files that should be tracked but aren't
git ls-files --others --exclude-standard

# Verify history preservation for moved files
git log --follow --oneline -- src/digitalmodel/modules/aqwa/aqwa_reader.py
```

### 3. Directory Structure Validation

```bash
# Verify target structure exists
echo "=== Source Modules ==="
ls -la src/digitalmodel/modules/

echo "=== Test Modules ==="
ls -la tests/modules/

echo "=== Spec Modules ==="
ls -la specs/modules/

echo "=== Doc Modules ==="
ls -la docs/modules/

echo "=== Example Modules ==="
ls -la examples/modules/

# Verify no modules remain at old locations
echo "=== Check for stragglers ==="
ls src/digitalmodel/ | grep -v "modules\|__\|\.py"
```

### 4. Test Suite Execution

```bash
# Run full test suite
uv run pytest tests/ -v

# Run with coverage to verify all code is reachable
uv run pytest tests/ --cov=src/digitalmodel --cov-report=term-missing

# Run specific module tests
uv run pytest tests/modules/aqwa/ -v
```

### 5. Clean Root Verification

```bash
# List root directory contents - should be minimal
ls -la | grep -v "^d" | grep -v "^\."

# Expected root files:
# - pyproject.toml
# - README.md
# - LICENSE
# - .gitignore
# - CLAUDE.md (optional)

# Anything else should be questioned
```

### 6. Gitignore Verification

```bash
# Check .gitignore has all necessary patterns
cat .gitignore | grep -E "(log|tmp|cache|pycache|claude-flow)"

# Recommended patterns to add:
# *.log
# *.tmp
# __pycache__/
# .pytest_cache/
# .claude-flow/
# dist/
# build/
# *.egg-info/
# .coverage
# htmlcov/
```

## Common Issues and Solutions

### Issue: Circular imports after refactor
**Solution**: Use lazy imports or restructure dependencies
```python
# Use TYPE_CHECKING for type hints only
from typing import TYPE_CHECKING
if TYPE_CHECKING:
    from digitalmodel.modules.other import OtherClass
```

### Issue: Relative imports broken
**Solution**: Convert to absolute imports
```python
# Before (relative)
from .utils import helper

# After (absolute)
from digitalmodel.modules.mymodule.utils import helper
```

### Issue: Test discovery fails
**Solution**: Ensure conftest.py at tests/modules/ level
```python
# tests/modules/conftest.py
import sys
from pathlib import Path

# Add src to path for imports
src_path = Path(__file__).parent.parent.parent / "src"
sys.path.insert(0, str(src_path))
```

### Issue: Package not found after move
**Solution**: Update pyproject.toml package discovery
```toml
[tool.setuptools.packages.find]
where = ["src"]
include = ["digitalmodel*"]
```

## Best Practices

1. **Commit frequently** - Make small, focused commits for each module moved
2. **Use feature branch** - Create `refactor/module-structure` branch
3. **Update documentation** - Update README.md and any architecture docs
4. **Communicate changes** - If team project, document breaking changes
5. **Run full test suite** - Verify all tests pass before merging

## Related Skills

- [context-management](../context-management/SKILL.md) - Managing context during large refactors
- [session-start-routine](../meta/session-start-routine/SKILL.md) - Session initialization

## References

- Python Packaging Guide: https://packaging.python.org/
- Git mv documentation: https://git-scm.com/docs/git-mv
- PEP 420 - Implicit Namespace Packages

## Cleanup Categories Quick Reference

| Category | Examples | Action |
|----------|----------|--------|
| **Root-level artifacts** | *.log, *.tmp, *.html | Delete or move to outputs/ |
| **Build artifacts** | dist/, build/, *.egg-info | Delete (regenerated on build) |
| **Cache files** | __pycache__/, .pytest_cache/ | Delete and .gitignore |
| **Runtime data** | coordination/, memory/ | Move to .claude-flow/ |
| **Duplicate dirs** | agents/, .claude/agents/ | Consolidate to .claude/agent-library/ |
| **Legacy hidden** | .agent-os/, .ai/, .drcode/ | Review and usually delete |
| **Test outputs** | reports/, coverage/, snapshots/ | Move under tests/ |
| **Prototype code** | scratch_*.py, experimental/ | Move to examples/ |
| **Test data** | *.csv, *.json at root | Move to tests/test_data/ |
| **Config files** | *.yaml, *.toml, *.json | Keep at root or move to config/ |

## Quick Start Commands

```bash
# 1. Pre-flight check (run first)
git status && git ls-files --others --exclude-standard | wc -l

# 2. Find all hidden folders
ls -la .* 2>/dev/null | grep "^d"

# 3. Find duplicates
find . -type d -name "agents" -o -name "skills" -o -name "memory" 2>/dev/null

# 4. Find artifacts at root
ls *.log *.tmp *.html *.sim 2>/dev/null

# 5. Create module structure
mkdir -p src/<pkg>/modules tests/modules specs/modules docs/modules examples/modules

# 6. Verify after cleanup
uv run pytest tests/ -v && git status
```

---

## Version History

- **2.0.0** (2025-01-20): Major update with comprehensive cleanup categories
  - Added Pre-flight Checks section
  - Added Parallel Execution Strategy with agent spawn patterns
  - Added Common Patterns Found During Reorganization
  - Added Post-cleanup Verification section
  - Expanded Checklist with cleanup categories
  - Added Quick Reference tables and Quick Start commands
- **1.0.0** (2025-01-19): Initial release based on digitalmodel repository refactor experience
