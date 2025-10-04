# Import Fix Plan - Marine Engineering Test Suite

**Generated:** 2025-10-03
**Status:** Ready for Implementation
**Estimated Time:** 4-6 hours

---

## Executive Summary

This document provides a **step-by-step plan** to fix all import issues preventing the 25 test files (150 tests) from running. The fixes are organized by priority and dependency order.

### Current State
- **0 of 150 tests** can run
- **3 critical blockers** preventing all imports
- **3 different import patterns** causing confusion

### Target State
- **150 of 150 tests** runnable
- **1 standardized import pattern**
- **Proper PYTHONPATH configuration**

---

## Phase 1: Critical Blockers (IMMEDIATE - 1 hour)

### Fix 1.1: Extraction Module Import Error

**Problem:**
```python
# File: src/digitalmodel/modules/marine_analysis/extraction/run_extraction.py:18
from extract_hydro_coefficients import HydrodynamicCoefficientExtractor
# ❌ Module 'extract_hydro_coefficients' does not exist
```

**Solution Options:**

#### Option A: Fix Import Path (RECOMMENDED)
```python
# Change line 18 from:
from extract_hydro_coefficients import HydrodynamicCoefficientExtractor

# To:
from .extract_hydro import HydrodynamicCoefficientExtractor
```

**Validation:**
- Check if `extract_hydro.py` has `HydrodynamicCoefficientExtractor` class
- If not, implement stub class

#### Option B: Make Import Optional
```python
# Change to graceful fallback:
try:
    from .extract_hydro import HydrodynamicCoefficientExtractor
except ImportError:
    HydrodynamicCoefficientExtractor = None
    import warnings
    warnings.warn("HydrodynamicCoefficientExtractor not available")
```

#### Option C: Comment Out (TEMPORARY)
```python
# Temporarily disable if not used:
# from extract_hydro_coefficients import HydrodynamicCoefficientExtractor
HydrodynamicCoefficientExtractor = None  # TODO: Fix extraction module
```

**Files to Modify:**
- `src/digitalmodel/modules/marine_analysis/extraction/run_extraction.py`

**Test After Fix:**
```bash
python -c "from digitalmodel.modules.marine_analysis import UnifiedRAOReader; print('SUCCESS')"
```

---

### Fix 1.2: RAOPlotter Import Path

**Problem:**
```python
# File: src/digitalmodel/modules/marine_analysis/__init__.py:45
try:
    from .rao_plotter import RAOPlotter  # ❌ Wrong path
except ImportError:
    RAOPlotter = None
```

**Actual Structure:**
```
src/digitalmodel/modules/marine_analysis/
├── visualization/
│   └── rao_plotter.py  ← RAOPlotter is HERE
└── __init__.py         ← Trying to import from wrong location
```

**Solution:**
```python
# Change line 45 in __init__.py:
try:
    from .visualization.rao_plotter import RAOPlotter
except ImportError:
    RAOPlotter = None
```

**Files to Modify:**
- `src/digitalmodel/modules/marine_analysis/__init__.py`

**Test After Fix:**
```bash
python -c "from digitalmodel.modules.marine_analysis import RAOPlotter; print(RAOPlotter)"
```

---

### Fix 1.3: PYTHONPATH Configuration

**Problem:**
- Tests cannot find `marine_engineering` module
- Tests cannot find `src.marine_engineering` module
- No pytest configuration for paths

**Solution A: Create conftest.py (RECOMMENDED)**

Create `tests/conftest.py`:
```python
"""
Pytest configuration for marine engineering tests.
Ensures proper Python path setup for all test modules.
"""
import sys
from pathlib import Path

# Get repository root
repo_root = Path(__file__).parent.parent

# Add src to Python path (for 'digitalmodel.*' and 'marine_engineering.*' imports)
src_path = repo_root / 'src'
if str(src_path) not in sys.path:
    sys.path.insert(0, str(src_path))

print(f"[conftest] Added to PYTHONPATH: {src_path}")
```

**Solution B: Create pytest.ini**

Create `pytest.ini`:
```ini
[pytest]
pythonpath = src
testpaths = tests
python_files = test_*.py
python_classes = Test*
python_functions = test_*
```

**Solution C: Install Package in Editable Mode**
```bash
pip install -e .
```

**Files to Create:**
- `tests/conftest.py` (Option A - RECOMMENDED)
- `pytest.ini` (Option B - supplementary)

**Test After Fix:**
```bash
cd tests/marine_engineering
pytest --collect-only 2>&1 | grep "collected"
# Should show: collected 150 items
```

---

## Phase 2: Standardize Imports (2-3 hours)

### Fix 2.1: Choose Standard Import Pattern

**Current Patterns Found:**

1. ✅ **Pattern A (RECOMMENDED):** `from digitalmodel.modules.marine_analysis import X`
2. ❌ **Pattern B:** `from src.marine_engineering import X`
3. ❌ **Pattern C:** `from marine_engineering import X`

**Decision: Use Pattern A for all tests**

**Rationale:**
- Matches package structure
- Works with installed package
- Most explicit and clear
- Already used in RAO tests

### Fix 2.2: Update Import Mapping

Create mapping for mass update:

| Old Import | New Import | Files Affected |
|------------|------------|----------------|
| `from marine_engineering.catenary import` | `from digitalmodel.modules.marine_analysis import` | 5 files |
| `from src.marine_engineering.catenary import` | `from digitalmodel.modules.marine_analysis import` | 3 files |
| `from marine_engineering.environmental_loading import` | `from digitalmodel.modules.marine_analysis import` | 6 files |
| `from marine_engineering.wave_spectra import` | *(needs new mapping)* | 5 files |
| `from marine_engineering.hydrodynamic_coefficients import` | *(needs new mapping)* | 3 files |

**Issue:** Some modules (wave_spectra, hydrodynamic_coefficients) don't exist in `digitalmodel.modules.marine_analysis`

**Solution:**
1. Check if they exist in `src/marine_engineering/`
2. If yes, add to PYTHONPATH
3. If no, create stub modules or update tests

### Fix 2.3: Update Test Files - Catenary Tests (5 files)

#### File: catenary/test_integration.py
```python
# BEFORE:
from src.marine_engineering.catenary import (
    CatenarySolver, CatenaryInput, CatenaryResults,
    catenaryEquation, catenaryForces
)
from src.marine_engineering.catenary.simplified import SimplifiedCatenarySolver

# AFTER:
from digitalmodel.modules.marine_analysis.catenary import (
    CatenarySolver, CatenaryInput, CatenaryResults,
    catenaryEquation, catenaryForces,
    SimplifiedCatenarySolver
)
```

**Note:** Verify `catenary` module is exported in `digitalmodel.modules.marine_analysis.__init__.py`

#### File: catenary/test_performance.py
```python
# BEFORE:
from src.marine_engineering.catenary import (...)
from src.marine_engineering.catenary.simplified import SimplifiedCatenarySolver

# AFTER:
from digitalmodel.modules.marine_analysis.catenary import (...)
```

#### File: catenary/test_simplified.py
```python
# BEFORE:
from src.marine_engineering.catenary.simplified import (...)

# AFTER:
from digitalmodel.modules.marine_analysis.catenary import (...)
```

#### File: catenary/test_run_quick.py
```python
# BEFORE:
from marine_engineering.catenary import (...)
from marine_engineering.catenary.simplified import SimplifiedCatenarySolver

# AFTER:
from digitalmodel.modules.marine_analysis.catenary import (...)
```

**Files to Modify:**
- tests/marine_engineering/catenary/test_integration.py
- tests/marine_engineering/catenary/test_performance.py
- tests/marine_engineering/catenary/test_simplified.py
- tests/marine_engineering/catenary/test_run_quick.py

### Fix 2.4: Update Test Files - Environmental Loading (1 file)

#### File: environmental_loading/test_ocimf.py
```python
# BEFORE:
from marine_engineering.environmental_loading import (
    OCIMFDatabase,
    VesselType
)

# AFTER:
# Option 1: If OCIMF is in marine_analysis
from digitalmodel.modules.marine_analysis.environmental_loading import (...)

# Option 2: If OCIMF is separate module in src/marine_engineering
from marine_engineering.environmental_loading import (...)
# (requires PYTHONPATH to include src/)
```

**Files to Modify:**
- tests/marine_engineering/environmental_loading/test_ocimf.py

### Fix 2.5: Update Test Files - Integration Tests (6 files)

#### Files Pattern:
```python
# BEFORE (example from test_hydro_rao_integration.py):
from marine_engineering.hydrodynamic_coefficients.coefficients import (...)

# AFTER:
# Need to verify where these modules live
from marine_engineering.hydrodynamic_coefficients.coefficients import (...)
# OR
from digitalmodel.modules.marine_engineering.hydrodynamic_coefficients import (...)
```

**Files to Modify:**
- tests/marine_engineering/integration/test_hydro_rao_integration.py
- tests/marine_engineering/integration/test_ocimf_mooring_integration.py
- tests/marine_engineering/integration/test_end_to_end_workflow.py
- tests/marine_engineering/integration/test_performance_benchmarks.py
- tests/marine_engineering/integration/test_wave_dynamics_integration.py
- tests/marine_engineering/integration/integration_example.py

### Fix 2.6: Update Test Files - Legacy Tests (4 files)

**Files to Modify:**
- tests/marine_engineering/legacy/test_component_database.py
- tests/marine_engineering/legacy/test_mooring_catenary.py
- tests/marine_engineering/legacy/test_validation.py
- tests/marine_engineering/legacy/test_wave_spectra.py

---

## Phase 3: Module Structure Verification (1 hour)

### Fix 3.1: Verify Module Exports

Check that all imported items are exported in `__init__.py` files:

#### Check List:

1. **digitalmodel.modules.marine_analysis.__init__.py**
   - [ ] Exports UnifiedRAOReader ✅
   - [ ] Exports RAOPlotter ❌ (needs fix)
   - [ ] Exports catenary module?
   - [ ] Exports environmental_loading?

2. **src/marine_engineering/ structure**
   - [ ] catenary/__init__.py exports all needed classes
   - [ ] environmental_loading/__init__.py exists
   - [ ] wave_spectra/__init__.py exists
   - [ ] hydrodynamic_coefficients/__init__.py exists

### Fix 3.2: Add Missing __init__.py Files

**Required Files:**
```bash
# Check and create if missing:
touch src/marine_engineering/__init__.py
touch src/marine_engineering/catenary/__init__.py
touch src/marine_engineering/environmental_loading/__init__.py
touch src/marine_engineering/wave_spectra/__init__.py
touch src/marine_engineering/hydrodynamic_coefficients/__init__.py
```

**Content Template:**
```python
"""
Marine Engineering Module: {module_name}

This module provides {description}.
"""

from .{main_file} import *

__all__ = [
    # List exports here
]
```

### Fix 3.3: Update __all__ Exports

Ensure all modules properly export their public API:

#### Example: src/marine_engineering/catenary/__init__.py
```python
"""Catenary analysis module."""

from .solver import CatenarySolver, CatenaryInput, CatenaryResults
from .simplified import SimplifiedCatenarySolver
from .adapter import catenaryEquation, catenaryForces

__all__ = [
    'CatenarySolver',
    'CatenaryInput',
    'CatenaryResults',
    'SimplifiedCatenarySolver',
    'catenaryEquation',
    'catenaryForces'
]
```

---

## Phase 4: Validation & Testing (1 hour)

### Test 4.1: Import Verification

Create validation script `tests/validate_imports.py`:

```python
"""
Validate all imports work correctly.
"""
import sys
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

def test_core_imports():
    """Test core marine_analysis imports."""
    print("Testing core imports...")

    try:
        from digitalmodel.modules.marine_analysis import (
            UnifiedRAOReader,
            RAOReaderError,
            RAOPlotter
        )
        print("✅ Core imports successful")
        print(f"  - UnifiedRAOReader: {UnifiedRAOReader}")
        print(f"  - RAOReaderError: {RAOReaderError}")
        print(f"  - RAOPlotter: {RAOPlotter}")
        return True
    except Exception as e:
        print(f"❌ Core imports failed: {e}")
        return False

def test_catenary_imports():
    """Test catenary module imports."""
    print("\nTesting catenary imports...")

    try:
        from marine_engineering.catenary import (
            CatenarySolver,
            SimplifiedCatenarySolver
        )
        print("✅ Catenary imports successful")
        return True
    except Exception as e:
        print(f"❌ Catenary imports failed: {e}")
        return False

def test_environmental_imports():
    """Test environmental loading imports."""
    print("\nTesting environmental imports...")

    try:
        from marine_engineering.environmental_loading import OCIMFDatabase
        print("✅ Environmental imports successful")
        return True
    except Exception as e:
        print(f"❌ Environmental imports failed: {e}")
        return False

if __name__ == '__main__':
    results = [
        test_core_imports(),
        test_catenary_imports(),
        test_environmental_imports()
    ]

    if all(results):
        print("\n" + "="*50)
        print("✅ ALL IMPORTS SUCCESSFUL")
        print("="*50)
        sys.exit(0)
    else:
        print("\n" + "="*50)
        print("❌ SOME IMPORTS FAILED")
        print("="*50)
        sys.exit(1)
```

**Run:**
```bash
python tests/validate_imports.py
```

### Test 4.2: Pytest Collection

```bash
# Should collect all 150 tests without errors
cd D:/workspace-hub/digitalmodel
pytest tests/marine_engineering/ --collect-only

# Expected output:
# collected 150 items
```

### Test 4.3: Run Sample Tests

```bash
# Run a few fast tests to verify
pytest tests/marine_engineering/test_unified_rao_reader.py -v
pytest tests/marine_engineering/catenary/test_simplified.py::TestSimplifiedCatenarySolverAngleBased::test_angle_based_basic -v
```

---

## Implementation Checklist

### Phase 1: Critical Blockers ☐

- [ ] Fix `run_extraction.py:18` import error
  - [ ] Identify correct module path
  - [ ] Update import statement
  - [ ] Test: `python -c "from digitalmodel.modules.marine_analysis import UnifiedRAOReader"`

- [ ] Fix RAOPlotter import path
  - [ ] Update `__init__.py:45` to use `from .visualization.rao_plotter import RAOPlotter`
  - [ ] Test: `python -c "from digitalmodel.modules.marine_analysis import RAOPlotter; print(RAOPlotter)"`

- [ ] Configure PYTHONPATH
  - [ ] Create `tests/conftest.py`
  - [ ] Test: `pytest tests/marine_engineering/ --collect-only`

### Phase 2: Standardize Imports ☐

- [ ] Update catenary tests (5 files)
  - [ ] test_integration.py
  - [ ] test_performance.py
  - [ ] test_simplified.py
  - [ ] test_run_quick.py
  - [ ] test_catenary_adapter.py

- [ ] Update environmental tests (1 file)
  - [ ] test_ocimf.py

- [ ] Update integration tests (6 files)
  - [ ] test_hydro_rao_integration.py
  - [ ] test_ocimf_mooring_integration.py
  - [ ] test_end_to_end_workflow.py
  - [ ] test_performance_benchmarks.py
  - [ ] test_wave_dynamics_integration.py
  - [ ] integration_example.py

- [ ] Update legacy tests (4 files)
  - [ ] test_component_database.py
  - [ ] test_mooring_catenary.py
  - [ ] test_validation.py
  - [ ] test_wave_spectra.py

- [ ] Update root level tests (7 files)
  - [ ] test_unified_rao_reader.py ✅ (already correct)
  - [ ] test_rao_verification_all_routes.py ✅ (already correct)
  - [ ] test_catenary_solver.py
  - [ ] test_adapter_manual.py
  - [ ] test_hydro_coefficients.py
  - [ ] test_performance.py

### Phase 3: Module Structure ☐

- [ ] Verify __init__.py files exist
  - [ ] src/marine_engineering/__init__.py
  - [ ] src/marine_engineering/catenary/__init__.py
  - [ ] src/marine_engineering/environmental_loading/__init__.py
  - [ ] src/marine_engineering/wave_spectra/__init__.py
  - [ ] src/marine_engineering/hydrodynamic_coefficients/__init__.py

- [ ] Update __all__ exports
  - [ ] digitalmodel.modules.marine_analysis.__init__.py
  - [ ] Each submodule __init__.py

### Phase 4: Validation ☐

- [ ] Create `tests/validate_imports.py`
- [ ] Run import validation: `python tests/validate_imports.py`
- [ ] Run pytest collection: `pytest --collect-only`
- [ ] Run sample tests
- [ ] Document any remaining issues

---

## Risk Assessment

### Low Risk (Safe to implement)
- ✅ Creating conftest.py
- ✅ Creating pytest.ini
- ✅ Fixing RAOPlotter import path (try/except handles failure)

### Medium Risk (Test after implementation)
- ⚠️ Fixing run_extraction.py import (may break extraction module)
- ⚠️ Updating test imports (may reveal missing modules)

### High Risk (Backup first)
- ❗ Modifying __init__.py exports (could break other code)
- ❗ Mass import updates (could introduce typos)

---

## Rollback Plan

If fixes cause issues:

1. **Git Reset:**
   ```bash
   git checkout -- src/digitalmodel/modules/marine_analysis/__init__.py
   git checkout -- src/digitalmodel/modules/marine_analysis/extraction/run_extraction.py
   ```

2. **Remove Changes:**
   ```bash
   rm tests/conftest.py
   rm pytest.ini
   ```

3. **Restore Tests:**
   ```bash
   git checkout -- tests/marine_engineering/
   ```

---

## Success Criteria

### Must Have (Phase 1)
- [ ] No import errors when importing `digitalmodel.modules.marine_analysis`
- [ ] `pytest --collect-only` succeeds
- [ ] 150 tests collected

### Should Have (Phase 2-3)
- [ ] All tests use consistent import pattern
- [ ] All modules properly export their API
- [ ] Documentation updated

### Nice to Have (Phase 4)
- [ ] All tests pass
- [ ] Coverage > 80%
- [ ] CI/CD configured

---

## Estimated Timeline

| Phase | Task | Time | Status |
|-------|------|------|--------|
| 1 | Fix extraction import | 15 min | ☐ |
| 1 | Fix RAOPlotter import | 15 min | ☐ |
| 1 | Create conftest.py | 15 min | ☐ |
| 1 | Test collection | 15 min | ☐ |
| 2 | Update catenary tests | 30 min | ☐ |
| 2 | Update environmental tests | 15 min | ☐ |
| 2 | Update integration tests | 45 min | ☐ |
| 2 | Update legacy tests | 30 min | ☐ |
| 3 | Verify module structure | 30 min | ☐ |
| 3 | Add missing __init__.py | 15 min | ☐ |
| 3 | Update exports | 30 min | ☐ |
| 4 | Create validation script | 15 min | ☐ |
| 4 | Run tests | 30 min | ☐ |
| 4 | Fix issues | 60 min | ☐ |
| **TOTAL** | | **5-6 hours** | |

---

**Next Steps:**
1. Review this plan
2. Get approval to proceed
3. Create backup branch: `git checkout -b fix/test-imports`
4. Execute Phase 1
5. Validate after each phase
