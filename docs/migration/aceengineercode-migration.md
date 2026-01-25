# AceEngineerCode Migration Documentation

> Last Updated: January 2026
> Status: Complete

## Executive Summary

The aceengineercode repository has been consolidated into digitalmodel as part of the workspace-hub unification effort. This migration merged common utilities, configuration files, and select analysis modules while preserving digitalmodel's more mature implementations of OrcaFlex, VIV, and fatigue analysis capabilities.

The migration consolidates marine offshore structural engineering tools into a single, maintainable codebase, eliminating duplicate functionality and establishing digitalmodel as the primary engineering analysis platform.

## Migration Details

| Field | Value |
|-------|-------|
| **Migration Date** | January 2026 |
| **Source Repository** | aceengineercode |
| **Archive Tag** | archived-v1.0 |
| **Target Repository** | digitalmodel |
| **Migration Type** | Selective merge |

## Files Migrated

### Common Utilities

**Location:** `digitalmodel/common/`

- **Files Migrated:** 15 files
- **Lines of Code:** ~6,800 lines
- **Components Include:**
  - Data processing utilities
  - Mathematical helper functions
  - File I/O handlers
  - Database connectivity modules
  - Report generation utilities
  - YAML configuration parsers

### Configuration Files

**Location:** Various module directories

- **Files Migrated:** 170 YAML configuration files
- **Categories:**
  - Analysis parameter templates
  - Project configuration schemas
  - Module-specific settings
  - Report formatting templates
  - Database connection configurations

### Core Modules Migrated

The following analysis modules were migrated from aceengineercode:

| Module | Description | Status |
|--------|-------------|--------|
| Plate_Buckling | Plate buckling analysis per industry standards | Migrated |
| PipeCapacity | Pipeline capacity calculations | Migrated |
| PipeSizing | Pipe sizing optimization tools | Migrated |
| API_579 | Fitness-for-service assessments | Migrated |
| Structural_Analysis | General structural analysis components | Migrated |
| Project_Management | Engineering project timeline tools | Migrated |
| Finance_Analysis | Cost analysis and economics | Migrated |

## Import Path Changes

All imports from aceengineercode must be updated to use the new digitalmodel paths:

### Before Migration

```python
from common.data_processing import DataProcessor
from common.yaml_utils import load_config
from common.report_generator import ReportBuilder
```

### After Migration

```python
from digitalmodel.common.data_processing import DataProcessor
from digitalmodel.common.yaml_utils import load_config
from digitalmodel.common.report_generator import ReportBuilder
```

### Update Script

For bulk updates, use the following sed command:

```bash
find . -name "*.py" -exec sed -i 's/from common\./from digitalmodel.common./g' {} \;
```

## Modules NOT Migrated

The following modules were intentionally NOT migrated because digitalmodel already contains more complete implementations:

### OrcaFlex Integration

- **Reason:** digitalmodel has 60+ OrcaFlex-related files vs aceengineercode's limited implementation
- **digitalmodel Coverage:** Complete OrcaFlex API integration, model generation, post-processing
- **Action:** Use `digitalmodel.orcaflex` module

### VIV Analysis

- **Reason:** digitalmodel VIV implementation is more mature with additional analysis modes
- **digitalmodel Coverage:** Multiple VIV assessment methodologies, SHEAR7 integration
- **Action:** Use `digitalmodel.viv` module

### Fatigue Analysis

- **Reason:** digitalmodel fatigue module is more complete with fracture mechanics
- **digitalmodel Coverage:** S-N curves, fracture mechanics, cycle counting, damage accumulation
- **Action:** Use `digitalmodel.fatigue` module

## BSEE Migration

**Status:** NO-OP (No action taken)

The BSEE (Bureau of Safety and Environmental Enforcement) related functionality was NOT migrated to digitalmodel because the worldenergydata repository already contains a more complete implementation.

- **Source:** aceengineercode BSEE modules
- **Preferred Location:** worldenergydata repository
- **Reason:** worldenergydata has comprehensive BSEE data integration, reporting, and compliance tools

## Known Issues

### OrcaFlex License Requirement

Some migrated modules that interface with OrcaFlex require a valid OrcaFlex license to function:

- Dynamic analysis workflows
- Model generation from templates
- Post-processing automation

**Workaround:** Modules can be imported without a license, but analysis functions will raise `LicenseError` when executed.

### assetutilities Dependency

Some common utilities have dependencies on the `assetutilities` package:

```python
from assetutilities.common.path_resolver import resolve_path
```

**Resolution:** Ensure `assetutilities` is installed in the environment:

```bash
pip install assetutilities
```

Or add to requirements.txt:

```
assetutilities>=1.0.0
```

### Configuration Path Updates

Some hardcoded configuration paths may reference the old aceengineercode structure. Check for:

```python
# Old path references
cfg_path = "/path/to/aceengineercode/cfg/"

# Should be updated to
cfg_path = "/path/to/digitalmodel/cfg/"
```

## Rollback Procedure

If issues arise that require reverting to the original aceengineercode implementation:

### 1. Access Archived Repository

The aceengineercode repository has been archived with tag `archived-v1.0`:

```bash
git clone https://github.com/[org]/aceengineercode.git
cd aceengineercode
git checkout archived-v1.0
```

### 2. Restore Specific Modules

To restore individual modules:

```bash
# From aceengineercode archived-v1.0
cp -r modules/[module_name] /path/to/target/
```

### 3. Restore Import Paths

Revert import statements if needed:

```bash
find . -name "*.py" -exec sed -i 's/from digitalmodel.common./from common./g' {} \;
```

### 4. Contact Information

For migration support or questions:

- Review archived-v1.0 documentation
- Check commit history for migration changes
- Reference original module documentation in aceengineercode

## Verification Checklist

After migration, verify the following:

- [ ] All migrated modules import correctly
- [ ] Unit tests pass for migrated components
- [ ] YAML configurations load without errors
- [ ] Database connections establish successfully
- [ ] Report generation functions properly
- [ ] OrcaFlex integration works (with valid license)
- [ ] No duplicate functionality exists between repositories

## Related Documentation

- [digitalmodel Module Documentation](/docs/modules/)
- [Common Utilities Reference](/docs/guides/)
- [OrcaFlex Integration Guide](/docs/software/)
- [Configuration System Overview](/docs/guides/)
