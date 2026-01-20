---
name: module-based-refactor
description: Reorganize a repository from flat structure to module-based 5-layer architecture. Use for codebase restructuring, module organization, import path updates, and git history preservation during refactoring.
version: 1.0.0
updated: 2025-01-19
category: meta
triggers:
- repository reorganization
- flat to module structure
- codebase restructuring
- module-based architecture
- import path migration
- git mv refactor
- 5-layer architecture
---

# Module-Based Refactor Skill

Reorganize a repository from flat structure to a consistent module-based 5-layer architecture while preserving git history.

## Version Metadata

```yaml
version: 1.0.0
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

### During Refactor
- [ ] All modules moved to modules/ subdirectory using `git mv`
- [ ] tests/modules/ created and populated
- [ ] specs/modules/ created (with .gitkeep if empty)
- [ ] docs/modules/ created (with .gitkeep if empty)
- [ ] examples/modules/ created (with .gitkeep if empty)

### Post-Refactor
- [ ] `__init__.py` files created with proper exports
- [ ] All import references updated
- [ ] `pyproject.toml` updated if needed
- [ ] Tests pass after refactor
- [ ] No broken imports found
- [ ] Git history preserved (verify with `git log --follow`)

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

---

## Version History

- **1.0.0** (2025-01-19): Initial release based on digitalmodel repository refactor experience
