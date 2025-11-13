# Modules Directory - Archival Review

**Date**: 2025-11-11
**Reviewer**: Analysis of modules/ folder after new structure implementation

## Summary

The `modules/` directory contains **OLD STRUCTURE** files that are **NO LONGER NEEDED** with the new modular structure. All functionality has been replaced by the new `base_files/` and `analysis_models/` directories.

## Current Content

### A. Direction-Specific Subdirectories (36 files - OBSOLETE)

```
modules/
├── operability_000deg/
│   ├── _03c_waves.yml          # Old: User-specified components, 100 lines
│   ├── _03d_current.yml         # Old: Direction-specific
│   └── _03e_wind.yml            # Old: Direction-specific
├── operability_030deg/
│   ├── _03c_waves.yml
│   ├── _03d_current.yml
│   └── _03e_wind.yml
... (12 directories × 3 files = 36 files)
└── operability_330deg/
    ├── _03c_waves.yml
    ├── _03d_current.yml
    └── _03e_wind.yml
```

**Status**: ❌ **OBSOLETE** - Replaced by `base_files/env/` (108 files with JONSWAP format)

**Issues with old files**:
- Uses "user specified components" (opaque, 100 lines of numbers)
- Cannot determine Hs, Tz values without reverse engineering
- Not comparable between files
- Only 1-year return period
- Nested directory structure (not industry standard)

**New replacement**:
- `base_files/env/waves_000deg_1yr.yml` (JONSWAP format, 29 lines, clear Hs/Tz)
- `base_files/env/waves_000deg_10yr.yml` (10-year conditions)
- `base_files/env/waves_000deg_100yr.yml` (100-year conditions)
- Same pattern for all 12 directions

### B. Root-Level Module Files (19 files - DUPLICATES)

```
modules/
├── _01a_units_analysis.yml      # DUPLICATE of base_files/_01a_units_analysis.yml
├── _01b_statics.yml             # DUPLICATE of base_files/_01b_statics.yml
├── _01c_dynamics.yml            # DUPLICATE of base_files/_01c_dynamics.yml
├── _01d_stages.yml              # DUPLICATE of base_files/_01d_stages.yml
├── _01e_view.yml                # DUPLICATE of base_files/_01e_view.yml
├── _02_variable_data.yml        # DUPLICATE of base_files/_02_variable_data.yml
├── _03a_sea_density.yml         # DUPLICATE of base_files/_03a_sea_density.yml
├── _03b_seabed.yml              # DUPLICATE of base_files/_03b_seabed.yml
├── _03c_waves.yml               # OBSOLETE: Old format (user components, direction=180°)
├── _03c_waves_jonswap.yml       # OBSOLETE: Replaced by base_files/env/waves_*.yml
├── _03d_current.yml             # OBSOLETE: Replaced by base_files/env/current_*.yml
├── _03e_wind.yml                # OBSOLETE: Replaced by base_files/env/wind_*.yml
├── _04_vessel_types.yml         # DUPLICATE of base_files/_04_vessel_types.yml
├── _05_line_types.yml           # DUPLICATE of base_files/_05_line_types.yml
├── _06_buoys_discretised.yml    # DUPLICATE of base_files/_06_buoys_discretised.yml
├── _06_vessels_buoys.yml        # DUPLICATE of base_files/_06_vessels_buoys.yml
├── _07_lines.yml                # DUPLICATE of base_files/_07_lines.yml
├── _07_lines_discretised.yml    # DUPLICATE of base_files/_07_lines_discretised.yml
├── _08_groups.yml               # DUPLICATE of base_files/_08_groups.yml
└── _08_groups_discretised.yml   # DUPLICATE of base_files/_08_groups_discretised.yml
```

**Status**:
- Files `_01a` to `_02`: ❌ **EXACT DUPLICATES** - Already in `base_files/`
- Files `_03c`, `_03d`, `_03e`: ❌ **OBSOLETE** - Replaced by `base_files/env/` standalone files
- Files `_04` to `_08`: ❌ **EXACT DUPLICATES** - Already in `base_files/`

## Comparison: Old vs New Structure

### Old Structure (modules/)
```
modules/
├── operability_000deg/
│   ├── _03c_waves.yml (user components, 100 lines, opaque)
│   ├── _03d_current.yml
│   └── _03e_wind.yml
├── operability_030deg/
│   └── ... (same pattern)
...
└── _01a_units_analysis.yml (shared)
    _02_variable_data.yml (shared)
    ... (other shared files)
```

**Problems**:
- ❌ Nested directories (hard to navigate)
- ❌ Only 1-year return period
- ❌ User-specified wave components (opaque)
- ❌ Mixed shared + direction-specific files
- ❌ Not industry standard

### New Structure (base_files/ + analysis_models/)
```
base_files/
├── env/
│   ├── waves_000deg_1yr.yml (JONSWAP, 29 lines, transparent)
│   ├── waves_000deg_10yr.yml
│   ├── waves_000deg_100yr.yml
│   ├── current_000deg_1yr.yml
│   ├── current_000deg_10yr.yml
│   └── ... (108 files total)
├── _01a_units_analysis.yml (shared)
├── _02_variable_data.yml (shared)
└── ... (19 shared modules)

analysis_models/
├── NSE_CALM_001_000deg_1yr_simple.yml
├── NSE_CALM_001_000deg_10yr_simple.yml
├── NSE_CALM_001_000deg_100yr_simple.yml
└── ... (72 files total)
```

**Benefits**:
- ✅ Flat structure (industry standard)
- ✅ All 3 return periods (1yr, 10yr, 100yr)
- ✅ JONSWAP format (simple, transparent, comparable)
- ✅ Clear separation (base_files vs env vs analysis)
- ✅ McDermott Woodfibre FST convention

## Recommendation

### Option 1: Archive (RECOMMENDED)

**Action**: Move `modules/` directory to `_archived/modules_old_structure/`

**Benefits**:
- Preserves historical data
- No risk of accidental use
- Can reference if needed
- Clean working directory

**Command**:
```bash
cd projects/TEST_OPERABILITY/orcaflex
mkdir -p _archived
mv modules _archived/modules_old_structure
echo "Archived on 2025-11-11. Replaced by base_files/ and analysis_models/" > _archived/modules_old_structure/ARCHIVED.txt
```

### Option 2: Delete (CLEAN)

**Action**: Delete `modules/` directory entirely

**Benefits**:
- Cleanest solution
- No confusion
- All content recreatable from new structure

**Risk**: Loss of historical files (low risk since duplicates exist)

**Command**:
```bash
cd projects/TEST_OPERABILITY/orcaflex
rm -rf modules
```

### Option 3: Keep (NOT RECOMMENDED)

**Action**: Keep `modules/` directory as-is

**Problems**:
- ❌ Confusion (two sources of truth)
- ❌ Risk of using old files
- ❌ Cluttered project structure
- ❌ Maintenance burden

## Files Status Summary

| Category | Count | Status | Replacement |
|----------|-------|--------|-------------|
| **Direction subdirs** | 36 | ❌ OBSOLETE | `base_files/env/*.yml` (108 files) |
| **Shared modules** | 16 | ❌ DUPLICATE | `base_files/*.yml` (19 files) |
| **Old env base** | 4 | ❌ OBSOLETE | `base_files/env/*.yml` (standalone) |
| **TOTAL** | **56** | **DELETE/ARCHIVE** | **New structure (206 files)** |

## Impact Assessment

### If modules/ is deleted/archived:

**No impact on current workflow**:
- ✅ All analysis models reference `base_files/` (not `modules/`)
- ✅ All environment data available in `base_files/env/` with better format
- ✅ All structural modules duplicated in `base_files/`
- ✅ Complete documentation exists for new structure

**Benefits**:
- ✅ No confusion about which files to use
- ✅ Cleaner project structure
- ✅ Easier maintenance
- ✅ Industry-standard organization

**No loss of functionality**:
- All 12 directions available (same as old)
- Additional 10-year and 100-year data (NEW)
- Better wave format (JONSWAP vs user components)
- All structural modules preserved

## Migration Complete

The migration from `modules/` to `base_files/` + `analysis_models/` is **COMPLETE**:

✅ **108 environment files** created (vs 36 old files)
✅ **72 analysis models** created (vs 0 old equivalent)
✅ **All 19 base modules** copied and verified
✅ **JONSWAP format** implemented (vs opaque user components)
✅ **3 return periods** supported (vs 1 in old structure)
✅ **Flat naming** implemented (McDermott standard)
✅ **Complete documentation** provided

## Recommended Action

**Archive the modules/ directory** to preserve history while moving to the new structure.

```bash
cd D:/workspace-hub/digitalmodel/projects/TEST_OPERABILITY/orcaflex
mkdir -p _archived
mv modules _archived/modules_old_structure_20251111
echo "Archived on 2025-11-11" > _archived/modules_old_structure_20251111/ARCHIVED.txt
echo "Replaced by:" >> _archived/modules_old_structure_20251111/ARCHIVED.txt
echo "  - base_files/ (19 modules + 108 env files)" >> _archived/modules_old_structure_20251111/ARCHIVED.txt
echo "  - analysis_models/ (72 ready-to-run models)" >> _archived/modules_old_structure_20251111/ARCHIVED.txt
echo "See STRUCTURE_SUMMARY.md for details" >> _archived/modules_old_structure_20251111/ARCHIVED.txt
```

---

*This review conducted as part of OrcaFlex model structure modernization*
*New structure follows McDermott Woodfibre FST project convention*
*All files verified and tested*
