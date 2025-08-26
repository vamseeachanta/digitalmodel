# OrcaWave Geometry Test Results

**Test Run:** 2025-08-25T05:08:03.757562
**Duration:** 0.94 seconds

## Summary

- **Total Tests:** 8
- **Passed:** 7 (87.5%)
- **Failed:** 1
- **Skipped:** 0

## Test Details

### [FAIL] validate_formats

**Status:** FAILED

**passed_files:** `7`

**failed_files:** `5`

**details:** `
============================================================
VALIDATING ALL GDF FILES
============================================================

============================================================
GDF VALIDATION REPORT: simple_box_test.gdf
============================================================

[INFO] File Information:
  header: Rhino->WAMIT file export (mesh)
  length_scale: 1.0
  gravity: 9.80665
  x_symmetry: 0
  y_symmetry: 0
  declared_vertices: 30
  actual_vertices: 30
 `

### [PASS] check_files

**Status:** PASSED

**Message:** All 8 required files exist

### [PASS] verify_dimensions

**Status:** PASSED

**geometries:** `{'simple_box_test.gdf': {'vertex_count': 30, 'expected': 30, 'match': True}, 'sea_cypress_orcawave.gdf': {'vertex_count': 72996, 'expected': 72996, 'match': True}}`

### [PASS] test_converters

**Status:** PASSED

**scripts:** `{'scripts/convert_to_orcawave_gdf.py': {'exists': True, 'valid_python': True}, 'scripts/create_simple_box_gdf.py': {'exists': True, 'valid_python': True}, 'scripts/test_orcawave_geometry.py': {'exists': True, 'valid_python': True}}`

### [PASS] validate_configs

**Status:** PASSED

**configs:** `{'configs/test_simple_box.yml': {'valid_yaml': True, 'missing_fields': [], 'complete': True}, 'configs/sea_cypress_diffraction.yml': {'valid_yaml': True, 'missing_fields': [], 'complete': True}}`

### [PASS] check_orcawave

**Status:** PASSED

**Message:** OrcaWave installation found

**path:** `C:\Program Files (x86)\Orcina\OrcaFlex\11.5\OrcaWave.exe`

### [PASS] test_simple_geometry

**Status:** PASSED

**Message:** Simple box geometry validated

**checks:** `{'header': True, 'gravity': True, 'vertex_count': True}`

### [PASS] test_full_geometry

**Status:** PASSED

**Message:** Full vessel geometry validated

**checks:** `{'header': True, 'gravity': True, 'vertex_count': True, 'sufficient_lines': True}`

## Recommendations

[WARNING] Some tests failed. Please review the errors above.

Troubleshooting:
1. Check file paths and permissions
2. Verify OrcaWave installation
3. Run individual test scripts for debugging
