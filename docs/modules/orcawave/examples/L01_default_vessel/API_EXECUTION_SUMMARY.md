# OrcaWave Python API Execution Summary

**Date**: 2026-01-05
**OrcFxAPI Version**: 11.6
**System**: Windows, 64 CPU threads

---

## ✅ SUCCESS: Simple Test (16 Cases)

### Configuration
- **File**: `L01_license_test.yml`
- **Cases**: 16 (4 periods × 4 headings)
- **Expected time**: ~6 seconds
- **Actual time**: **4.26 seconds** ⚡

### Results
```
Output files:
  - L01_license_test.owr (0.13 MB)
  - L01_license_test_data.dat (0.01 MB)

Hydrodynamic Data:
  - Frequencies: 4 values (0.05-0.25 rad/s)
  - Periods: 4 values (4-20 s)
  - Headings: 4 values (0°-180°)
  - Load RAOs: (4, 4, 6) array, max: 153524.02
  - Added Mass: (4, 6, 6) array
  - Damping: (4, 6, 6) array
```

### Thread Performance
| Threads | Status | Time | Memory | Notes |
|---------|--------|------|--------|-------|
| 64 | ✅ Success | 4.20s | ~70 MB | Original test |
| 57 | ✅ Success | 4.20s | ~70 MB | 90% CPU |
| 12 | ✅ Success | 4.26s | ~70 MB | **Default** |

**Conclusion**: Simple test scales well across thread counts. No GUI window opens. True batch mode achieved.

---

## ❌ FAILED: Benchmark Test (180 Cases)

### Configuration
- **File**: `orcawave_001_ship_raos_rev2.yml`
- **Cases**: 180 (20 periods × 9 headings)
- **Expected time**: ~54 seconds (0.9 minutes)
- **Actual time**: **Timeout** (>10 minutes)

### Execution Attempts

#### Attempt 1: 57 Threads
**Result**: **OUT OF MEMORY**
```
Error code: 63
Operation not available

Out of memory

There was not enough available memory to perform the calculation -
please try reducing the thread count.
```

**Memory usage**: Process started, validation passed with warnings, but failed during calculation.

**Validation Warnings**:
- Control surface mesh: Large aspect ratio panels (723)
- Control surface mesh: Large panels vs wavelength (1-2163)
- Calculation mesh: Non-planar panels from mesh file
- Calculation mesh: Large panels vs shortest wave (2.0s period)

#### Attempt 2: 24 Threads
**Result**: **TIMEOUT** (>3 minutes)
- Process started successfully
- Memory usage: ~55 GB
- No output files generated
- Process hung or extremely slow

#### Attempt 3: 12 Threads
**Result**: **TIMEOUT** (>3 minutes)
- Process terminated after timeout
- No output files generated

#### Attempt 4: 6 Threads
**Result**: **TIMEOUT** (>10 minutes)
- Process started successfully
- Memory usage: ~14 GB (much lower than 24 threads)
- No output files generated
- Process terminated manually

### Key Findings

**NOT a memory issue** (6 threads used only 14 GB):
- 6 threads should have plenty of memory
- Process hung despite low memory usage
- Similar behavior to original GUI hanging issue

**Possible Causes**:
1. **Configuration complexity**: Mesh warnings about non-planar panels and large aspect ratios
2. **Computational scaling**: May not scale linearly from 16 to 180 cases
3. **Mesh quality issues**: Warnings suggest mesh may cause solver problems
4. **API limitation**: Possible blocking or synchronization issue with larger problems

---

## 🎯 Current Status

### ✅ Achievements
1. **Python API method validated**: Successfully bypasses GUI requirement
2. **Simple test working**: 16 cases run in 4.26 seconds
3. **Correct file naming**: `.owr` extension for results files
4. **Default thread count**: Set to **12 threads** for memory safety
5. **File size tracking**: Implemented in output

### ❌ Outstanding Issues
1. **Benchmark test fails**: 180-case configuration doesn't complete
2. **Unknown root cause**: Not memory-related, possibly mesh or solver issue
3. **Long execution time**: Even small thread counts timeout

### 📊 Recommended Defaults

**OrcaWave (Diffraction Analysis)**:
```python
Default threads: 12
# Provides memory safety while maintaining good performance
# Simple tests: ~4 seconds
# Complex tests: Requires investigation
```

**Note**: User requested 90% CPU utilization (57 threads), but this causes memory issues for complex problems. Conservative default of 12 threads recommended.

---

## 🔧 Next Steps

### Immediate Actions
1. **Review mesh quality**: Address non-planar panels and aspect ratio warnings
2. **Test intermediate complexity**: Try 90-case configuration (half of benchmark)
3. **Check OrcaWave documentation**: Look for limitations on problem size via API
4. **Contact Orcina support**: Report hanging behavior with benchmark configuration

### Alternative Approaches
1. **Use OrcaWave GUI**: Run benchmark interactively to verify it completes
2. **Batch mode menu**: Use GUI "Calculation → Batch Processing" option
3. **Mesh refinement**: Fix mesh warnings before re-running
4. **Problem decomposition**: Split 180 cases into smaller batches

---

## 📝 Script Details

### File Locations
- **API Script**: `/docs/modules/orcawave/examples/L01_default_vessel/run_orcawave_diffraction.py`
- **Simple Test Config**: `/docs/modules/orcawave/examples/L01_default_vessel/L01_license_test.yml`
- **Benchmark Config**: `/docs/modules/orcawave/L01_aqwa_benchmark/orcawave_001_ship_raos_rev2.yml`

### Command Examples
```bash
# Simple test (default 12 threads)
python run_orcawave_diffraction.py L01_license_test.yml

# Custom thread count
python run_orcawave_diffraction.py L01_license_test.yml --threads 6

# Benchmark (currently failing)
python run_orcawave_diffraction.py orcawave_001_ship_raos_rev2.yml --threads 6
```

### Key Code Sections
```python
# Create diffraction object with thread control
diffraction = OrcFxAPI.Diffraction(threadCount=thread_count)

# Load OrcaWave configuration
diffraction.LoadData(str(config_path.absolute()))

# Run calculation (blocking call)
diffraction.Calculate()

# Save results to .owr file
diffraction.SaveResults(str(results_file))
```

---

## 📈 Performance Metrics

### Simple Test (16 Cases)
- **Load time**: 0.00 seconds
- **Calc time**: 4.26 seconds
- **Total time**: 4.26 seconds
- **Results file**: 0.13 MB
- **Data file**: 0.01 MB

### Benchmark Test (180 Cases)
- **Expected**: 54 seconds (11.25x simple test)
- **Actual**: Timeout (>600 seconds)
- **Scaling**: **NOT LINEAR** ⚠️

---

**Generated**: 2026-01-05 19:45:00
**Status**: Simple test ✅ Production-ready | Benchmark ❌ Requires investigation
**Recommendation**: Use for simple/medium problems. Benchmark needs troubleshooting.
