# Requirements Migration Notice

## ⚠️ IMPORTANT: Dependencies Moved to pyproject.toml

All Python dependencies have been consolidated and moved to `pyproject.toml`.

### Previous requirements.txt files:
- config/requirements-dev.txt
- config/requirements_improved.txt
- docs/domains/platecapacity/StiffnerBuckling_Cal/Python_Environment/requirements.txt
- docs/guides/legacy/apirp2rd/COD/DNV-OS-F101/Rev2/Python_Environment/requirements.txt
- docs/legacy/apirp2rd/COD/DNV-OS-F101/Rev2/Python_Environment/requirements.txt
- docs/leg_apirp2rd/COD/DNV-OS-F101/Rev2/Python_Environment/requirements.txt
- docs/sub_platecapacity/StiffnerBuckling_Cal/Python_Environment/requirements.txt
- scripts/requirements.txt
- src/modules/visualization/orcaflex-dashboard/backend/requirements.txt

### New dependency management:

1. **Install dependencies:**
   ```bash
   uv pip install -e .
   ```

2. **Install with dev dependencies:**
   ```bash
   uv pip install -e .[dev]
   ```

3. **Add new dependencies:**
   Edit `pyproject.toml` and add to the `dependencies` list.

### Benefits:
- ✅ Single source of truth for dependencies
- ✅ Better dependency resolution
- ✅ Support for optional dependencies
- ✅ Integrated with modern Python packaging
- ✅ MANDATORY parallel processing for all operations

### Parallel Processing:
All operations in this repository now MANDATORILY use parallel processing for:
- File operations
- Test execution
- Linting and formatting
- Package installation
- Data processing

See `.common/parallel_utils.py` for parallel processing utilities.
