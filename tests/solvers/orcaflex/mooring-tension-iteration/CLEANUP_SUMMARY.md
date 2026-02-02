# Mooring Tension Iteration Test Cleanup Summary

**Date**: 2025-09-02
**Purpose**: Remove unnecessary files and empty directories

## Files Removed

### Backup Files
- `backup_dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml`
- `backup_viz.yml`

### Log Files (6 files)
- `au_collate.log`
- `dm_ofx_anal_mooring_fsts_l015_125km3_pb.log`
- `dm_ofx_anal_mooring_fsts_l015_125km3_sb.log`
- `dm_ofx_post_fsts_lngc.log`
- `viz_viz_fsts_.log`
- `viz_viz_fsts_180km3_sb.log`

### Result Files (6 files)
- `au_collate.yml`
- `dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml`
- `dm_ofx_anal_mooring_fsts_l015_125km3_sb.yml`
- `dm_ofx_post_fsts_lngc.yml`
- `viz_viz_fsts_.yml`
- `viz_viz_fsts_180km3_sb.yml`

## Directories Removed

### Root Level
- `.dat/` (empty hidden directory)
- `.sim/` (empty hidden directory)
- `output/` (empty directory structure)
- `scripts/` (empty directory structure)

### In fsts-l015-test-cases
- `output/.csv/` (hidden directory with empty Data/ and Plot/ subdirs)
- `output/visual/Data/` (empty)
- `output/visual/Plot/` (empty)
- `scripts/logs/` (after removing log files)
- `scripts/results/` (after removing result files and empty Data/Plot subdirs)

## Files Retained

### Essential Test Files
- Test script: `mooring_tension_iteration_test.py`
- Configuration files in scripts/
- Target CSV files for mooring pretension
- Template Excel file for analysis summary
- Shell scripts for iteration processes
- Python script for plot generation and model simulation

### Output Artifacts
- Analysis reports and visualizations in output/
- Pretension analysis summary Excel file
- Vessel statics visualization images

## Rationale
- Removed all backup files (unnecessary duplicates)
- Removed all log files (can be regenerated)
- Removed result yml files from scripts/results (outputs, not inputs)
- Removed empty directory structures
- Kept essential test configurations and scripts
- Kept valuable output artifacts for reference