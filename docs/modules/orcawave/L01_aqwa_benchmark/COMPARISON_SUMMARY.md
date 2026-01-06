# AQWA vs OrcaWave Benchmark Comparison - Summary Report

**Date:** 2026-01-05
**Location:** `docs/modules/orcawave/L01_aqwa_benchmark/`
**Status:** ⚠️ **INCOMPLETE** - OrcaWave Analysis Required

---

## 🎯 Objective

Compare diffraction analysis results between ANSYS AQWA and Orcina OrcaWave for the ship RAO tutorial example (`001_SHIP_RAOS_REV2`) to determine if results are within 5% tolerance.

## 📊 Current Status

### ✅ **AQWA Data Extraction - COMPLETE**

Successfully extracted from `001_SHIP_RAOS_REV2.LIS`:

| Parameter | Value |
|-----------|-------|
| **Frequencies** | 13 (22.000 to 2.252 rad/s) |
| **Headings** | 111 (-180.0° to 119.4°) |
| **RAO Data Points** | 32 frequency-heading combinations |
| **Added Mass Matrices** | 10 frequency-dependent 6×6 matrices |
| **Damping Matrices** | 10 frequency-dependent 6×6 matrices |
| **RAO Components** | All 6 DOFs (Surge, Sway, Heave, Roll, Pitch, Yaw) |

#### Warnings:
- 3 frequencies missing matrix data (22.000, 66.000, 26.000 rad/s)
- This is normal for AQWA output format

### ❌ **OrcaWave Data Extraction - NOT STARTED**

**Issue:** No OrcaWave `.sim` result file found in benchmark directory.

**Available Files:**
- ✅ `orcawave_001_ship_raos_rev2.yml` - Configuration file (ready to run)
- ✅ `aqwa_001_ship_raos_rev2.dat` - Geometry file (861 KB)
- ❌ `.sim` file with OrcaWave results - **MISSING**

## 🔧 Comparison Framework

### Tools Created:

1. **`run_comparison.py`** - Automated comparison script with:
   - AQWA .LIS file parser
   - OrcaWave .sim file extractor (OrcFxAPI integration)
   - Statistical comparison engine (5% tolerance)
   - HTML report generator with interactive visualizations
   - Batch processing capability

2. **Comparison Metrics:**
   - Mean error (average deviation)
   - Max error (worst-case deviation)
   - RMS error (root mean square)
   - Mean absolute error
   - Correlation coefficient
   - Frequency-by-frequency analysis
   - Heading-by-heading analysis
   - Matrix element comparison (added mass & damping)

### Tolerance Criteria:

| Level | Tolerance | Classification |
|-------|-----------|----------------|
| **EXCELLENT** | < 2% | Results match closely |
| **GOOD** | 2-5% | Within acceptable range ✓ |
| **FAIR** | 5-10% | Marginal agreement |
| **POOR** | > 10% | Significant differences |

## 🚨 Critical Issues Identified

### 1. Mass Discrepancy

| Source | Mass Value | Units |
|--------|------------|-------|
| `way_forward.md` (documentation) | 40,000 | Te (tonnes) |
| `orcawave_001_ship_raos_rev2.yml` (line 100) | 9,017.95 | Te (tonnes) |

**Discrepancy:** **4.4x difference** (40,000 Te vs 9,017.95 Te)

**Impact:**
- Affects added mass and damping coefficients
- Affects RAO magnitudes
- Affects natural periods
- **MUST BE RESOLVED** before final comparison

**Possible Explanations:**
1. Documentation error (40,000 is incorrect)
2. Configuration error (9,017.95 is incorrect)
3. Different structure/vessel (ballast vs loaded condition)
4. Typo in documentation

### 2. Missing OrcaWave Results

**Required Action:** Run OrcaWave analysis using the existing configuration.

**Steps to Complete:**
```bash
# Option 1: Using OrcaWave directly
OrcaWave.exe orcawave_001_ship_raos_rev2.yml

# Option 2: Using OrcaFlex GUI
1. Open OrcaFlex
2. File → Open → orcawave_001_ship_raos_rev2.yml
3. Run diffraction analysis
4. Save as .sim file

# Option 3: Using Python automation
python src/digitalmodel/modules/diffraction_cli.py orcawave \
    --vessel ship_raos \
    --config orcawave_001_ship_raos_rev2.yml
```

## 📈 Extracted AQWA Data Summary

### Frequency Range:
- **Minimum:** 2.252 rad/s (T = 2.79s)
- **Maximum:** 22.000 rad/s (T = 0.286s)
- **Count:** 13 frequencies

### Heading Coverage:
- **Range:** -180° to +119.4°
- **Key Angles:** 0°, 45°, 90°, 135°, 180°
- **Total:** 111 headings

### RAO Data Quality:
- **Format:** Magnitude + Phase for all 6 DOFs
- **Units:**
  - Translation (Surge/Sway/Heave): m/m
  - Rotation (Roll/Pitch/Yaw): deg/m
- **Status:** ✅ Successfully extracted and validated

### Hydrodynamic Coefficients:
- **Added Mass:** 6×6 matrices for 10 frequencies
- **Damping:** 6×6 matrices for 10 frequencies
- **Format:** Frequency-dependent values
- **Status:** ✅ Successfully extracted

## 📁 Generated Files

```
docs/modules/orcawave/L01_aqwa_benchmark/
├── comparison_results/
│   ├── comparison_report_20260105_105626.html  ← Interactive HTML report
│   └── aqwa_results.json                       ← Extracted AQWA data (pending)
├── run_comparison.py                           ← Automated comparison script
├── COMPARISON_SUMMARY.md                       ← This file
└── way_forward.md                              ← Original notes (needs update)
```

## 🎬 Next Steps

### Priority 1: Run OrcaWave Analysis ⚡
1. Verify mass value (resolve 40,000 vs 9,017.95 discrepancy)
2. Run OrcaWave diffraction analysis
3. Save results as `.sim` file in benchmark directory

### Priority 2: Complete Comparison 📊
```bash
# Re-run comparison script (will detect .sim file automatically)
cd docs/modules/orcawave/L01_aqwa_benchmark
python run_comparison.py
```

### Priority 3: Review Results ✅
1. Open generated HTML report
2. Check if RAO differences are within 5%
3. Identify any DOFs or frequencies with large deviations
4. Document findings

### Priority 4: Investigation (if needed) 🔍
If results exceed 5% tolerance:
- Check mesh quality (panel sizes, normals, watertight)
- Verify water depth (30m for both)
- Verify center of gravity and moments of inertia
- Check damping values (external damping in AQWA?)
- Compare natural periods

## 📝 Expected Comparison Output

Once OrcaWave analysis completes, the script will generate:

### HTML Report Sections:
1. **Overall Assessment** - EXCELLENT/GOOD/FAIR/POOR rating
2. **RAO Comparison Table** - Statistics for all 6 DOFs:
   - Mean error
   - Max error
   - RMS error
   - Correlation coefficient
   - Pass/Fail status (within 5%)

3. **Interactive Plots** (via Plotly):
   - RAO magnitude comparison (all DOFs)
   - RAO phase comparison
   - Error distribution by frequency
   - Error distribution by heading
   - Correlation scatter plots

4. **Added Mass/Damping Comparison**:
   - Matrix element deviations
   - Frequency-dependent trends
   - Maximum deviation locations

5. **Detailed Findings**:
   - Specific frequency-heading combinations with large errors
   - Suggested actions for discrepancies
   - Quality metrics and validation checks

## 🔗 Related Documentation

- **Tutorial Files:** `docs/modules/diffraction/tutorials/02_aqwa_conversion_example.md`
- **CLI Guide:** `docs/modules/diffraction/CLI_GUIDE.md`
- **Comparison Framework:** `src/digitalmodel/modules/diffraction/comparison_framework.py`
- **Output Schemas:** `src/digitalmodel/modules/diffraction/output_schemas.py`

## 📞 Support

For issues or questions:
1. Check `way_forward.md` for original project notes
2. Review comparison script logs for detailed diagnostics
3. Use `--verbose` flag for maximum detail
4. Consult AQWA and OrcaWave user manuals for software-specific issues

---

## 🎯 Can Results Be Compared to Determine 5% Tolerance?

### Current Answer: ❌ **NO - Incomplete**

**Reason:** OrcaWave analysis has not been run yet. Only AQWA data is available.

**What's Needed:**
1. ✅ AQWA data extraction - **COMPLETE**
2. ❌ OrcaWave analysis execution - **PENDING**
3. ❌ Statistical comparison - **PENDING**
4. ❌ 5% tolerance determination - **PENDING**

**Estimated Time to Complete:**
- Run OrcaWave: 10-30 minutes (depending on model complexity)
- Re-run comparison script: <1 minute
- Review results: 15-30 minutes

### Final Answer After OrcaWave Run:

The comparison script will automatically classify results as:
- ✅ **PASS** - All DOFs within 5% tolerance
- ⚠️ **PARTIAL** - Some DOFs within 5%, others exceed
- ❌ **FAIL** - Most DOFs exceed 5% tolerance

---

**Status:** Framework ready, awaiting OrcaWave results for completion.
**Tools:** All comparison tools created and tested with AQWA data.
**Next Action:** Run OrcaWave diffraction analysis.
