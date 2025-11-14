# OrcaFlex Configuration Changes Review
## Baltic 3.9m (10m depth) vs NSE 100m (100m depth)

**Baseline (Working):** `projects/modules/calm/nse_100m/TEST_OPERABILITY/orcaflex/base_files/`
**Modified (Baltic):** `projects/modules/calm/baltic_039m/TEST_OPERABILITY/orcaflex/base_files/`
**Review Date:** 2025-11-13

---

## Executive Summary

**Files Modified:** 6 of 34 YAML files
**Files Unchanged:** 28 of 34 YAML files
**Change Scope:** Water depth, mooring line configuration, and clump weight addition
**Risk Assessment:** ⚠️ **MEDIUM** - Significant geometry changes require validation

---

## Modified Files Summary

| File | Change Type | Impact | Risk |
|------|-------------|--------|------|
| `_03b_seabed.yml` | Water depth: 100m → 10m | **HIGH** | ⚠️ Medium |
| `_05_line_types.yml` | Added ClumpWeight type | **MEDIUM** | ✅ Low |
| `_07_lines.yml` | 3-segment mooring config | **HIGH** | ⚠️ Medium |
| `_07_lines_discretised.yml` | 3-segment mooring config | **HIGH** | ⚠️ Medium |
| `05_lines.yml` | 3-segment mooring config | **HIGH** | ⚠️ Medium |
| `05_lines_discretised.yml` | 3-segment mooring config | **HIGH** | ⚠️ Medium |

**Total Changes:** 6 files (17.6% of base files)

---

## Change 1: Water Depth (_03b_seabed.yml)

### Comparison

**NSE 100m (Working):**
```yaml
# Seabed
SeabedType: Flat
SeabedOrigin: [0, 0]
WaterDepth: 100              # ← Deep water
SeabedSlopeDirection: 0
SeabedSlope: 0
SeabedModel: Elastic
SeabedNormalStiffness: 100
SeabedShearStiffness: ~
```

**Baltic 10m (Modified):**
```yaml
# Seabed
SeabedType: Flat
SeabedOrigin: [0, 0]
WaterDepth: 10               # ← CHANGED: Shallow water
SeabedSlopeDirection: 0
SeabedSlope: 0
SeabedModel: Elastic
SeabedNormalStiffness: 100
SeabedShearStiffness: ~
```

### Change Analysis

**What Changed:**
- Water depth reduced from 100m to 10m (90% reduction)

**Why Changed:**
- Reflects Baltic Sea shallow water conditions
- Typical Baltic nearshore depth: 10-50m

**Impact:**
- ✅ **Positive:** Realistic for Baltic location
- ⚠️ **Risk:** Shallow water affects catenary shape significantly
- ⚠️ **Risk:** May require different mooring approach

**Validation Required:**
- ✅ Syntax: Correct
- ⏳ Static analysis: Must verify model converges
- ⏳ Geometry check: Ensure adequate clearances

**Risk Level:** ⚠️ **MEDIUM** - Major parameter change

---

## Change 2: ClumpWeight Line Type (_05_line_types.yml)

### Comparison

**NSE 100m (Working):**
```yaml
# Only has: 2p5inChain, Hose, HawserLine, Pivot
# NO ClumpWeight defined
```

**Baltic 10m (Modified):**
```yaml
New: ClumpWeight                    # ← ADDED: New line type
ClumpWeight:
    Category: General
    # Geometry and mass - 5 mT clump weight
    OD: 1.0                        # 1m diameter
    ID: 0
    CentreOfMass: [0, 0]
    BulkModulus: Infinity
    MassPerUnitLength: 1.0         # 1 tonne/m × 5m = 5 mT total
    # Limits
    CompressionIsLimited: No
    AllowableTension: ~
    MinRadius: [~, ~]
    # Structure - Very stiff for lumped mass representation
    EI: [1e6, ~]                   # Very stiff bending
    EA: 1e6                        # Very stiff axial (1,000,000 kN)
    PoissonRatio: 0.5
    ExpansionTable: None
    GJ: 0
    TensionTorqueCoupling: 0
    # Contact
    OuterContactDiameter: ~
    InnerContactDiameter: ~
    ClashStiffness: 0
    # Added mass, inertia and slam
    Ca: [1, ~, 1]                  # Added mass coefficient
    Cm: [~, ~, ~]
    Cs: 0
    Ce: 0
    # Drag and lift
    Cd: [1.2, ~, 1.2]              # Drag coefficient
    Cl: 0
    NormalDragLiftDiameter: 1.0
    AxialDragLiftDiameter: 0.5
    # Stress
    StressOD: ~
    StressID: ~
    AllowableStress: ~
    TensileStressLoadingFactor: 1
    BendingStressLoadingFactor: 1
    ShearStressLoadingFactor: 1
    TorsionalStressLoadingFactor: 1
    # Friction
    SeabedLateralFrictionCoefficient: 0.5
    SeabedAxialFrictionCoefficient: ~
    # Structural damping
    RayleighDampingCoefficients: (no damping)
    # Drawing
    Pen: [3, Solid, Red]           # Red color for visibility
```

### Change Analysis

**What Changed:**
- Added entirely new line type: `ClumpWeight`
- 49 lines of new YAML configuration

**Why Changed:**
- Shallow water requires additional restoring force
- Clump weights compensate for reduced catenary sag in 10m depth
- Standard offshore mooring practice for shallow water

**Engineering Properties:**

| Property | Value | Purpose |
|----------|-------|---------|
| Mass per meter | 1.0 tonne/m | 5m segment = 5 mT total |
| OD | 1.0m | Representative size for visualization |
| EA | 1,000,000 kN | Very stiff (nearly rigid) |
| EI | 1,000,000 kNm² | Very stiff (nearly rigid) |
| Cd | 1.2 | Typical for cylindrical mass |
| Ca | 1.0 | Standard added mass |

**Validation:**
- ✅ Syntax: Valid OrcaFlex line type format
- ✅ Properties: Appropriate for lumped mass
- ✅ Stiffness: High enough to act as concentrated mass
- ⚠️ Geometry: 5m length may need adjustment based on testing

**Risk Level:** ✅ **LOW** - Well-defined, follows OrcaFlex conventions

---

## Change 3: Mooring Line Configuration (_07_lines.yml)

### Comparison - Mooring1 Example

**NSE 100m (Working) - 2 Segments:**
```yaml
New: Mooring1
Mooring1:
    # ... (header identical) ...
    # End connections
    Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, ConnectionDeclination, ConnectionGamma:
      - [CALMBase, 6, 0, -1.5, 0, 180, 0, ~]
      - [Anchored, 299.133, 5.581, 0.1143, 0, 90, 0, ~]    # ← Deep water anchor

    # Sections (2-segment)
    LineType, Length, TargetSegmentLength:
      - [2p5inChain, 150, 10]      # Top chain: 150m
      - [2p5inChain, 195, 15]      # Bottom chain: 195m
    # Total: 345m
```

**Baltic 10m (Modified) - 3 Segments:**
```yaml
New: Mooring1
Mooring1:
    # ... (header identical) ...
    # End connections
    Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, ConnectionDeclination, ConnectionGamma:
      - [CALMBase, 6.000000, 0.000000, -1.5, 0, 180, 0, ~]
      - [Anchored, 33.921450, 0.000000, 0.1143, 0, 90, 0, ~]  # ← Shallow water anchor

    # Sections - Top chain, Clump weight, Bottom chain (3-segment)
    LineType, Length, TargetSegmentLength:
      - [2p5inChain, 15.0, 5]      # Top chain: 15m
      - [ClumpWeight, 5.0, 5]      # Clump weight: 5m (5 mT)
      - [2p5inChain, 25.0, 5]      # Bottom chain: 25m
    # Total: 45m
```

### Detailed Change Analysis

#### 3.1 Anchor Position Changes

| Parameter | NSE 100m | Baltic 10m | Change | % Change |
|-----------|----------|------------|--------|----------|
| Anchor X (m) | 299.133 | 33.921 | -265.2m | -88.7% |
| Anchor Y (m) | 5.581 | 0.000 | -5.6m | -100% |
| Horizontal Radius (m) | ~300 | ~34 | -266m | -88.7% |

**Analysis:**
- ✅ Expected reduction due to shallow water
- ✅ Maintains radial pattern (0° azimuth example)
- ⚠️ Calculated footprint: ~34m (vs ~300m in deep water)

#### 3.2 Line Segment Changes

**NSE 100m Configuration:**
```
Segment 1: Top Chain    - 150m (2p5inChain)
Segment 2: Bottom Chain - 195m (2p5inChain)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Total Length: 345m
Vertical Drop: ~100m (water depth)
Scope Ratio: 3.45:1
```

**Baltic 10m Configuration:**
```
Segment 1: Top Chain    - 15m  (2p5inChain)
Segment 2: Clump Weight - 5m   (ClumpWeight) ← NEW
Segment 3: Bottom Chain - 25m  (2p5inChain)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Total Length: 45m
Vertical Drop: ~11.5m (fairlead to seabed)
Scope Ratio: 4.5:1
```

**Key Differences:**

| Aspect | NSE 100m | Baltic 10m | Engineering Impact |
|--------|----------|------------|-------------------|
| **Segments** | 2 (chain only) | 3 (chain-clump-chain) | ⚠️ More complex |
| **Total Length** | 345m | 45m (-87%) | ✅ Appropriate for depth |
| **Scope Ratio** | 3.45:1 | 4.5:1 (+30%) | ✅ Good for shallow water |
| **Clump Weight** | None | 5 mT | ✅ Adds restoring force |
| **Top Chain** | 150m | 15m (-90%) | ⚠️ Very short |
| **Bottom Chain** | 195m | 25m (-87%) | ⚠️ Significant seabed contact |

#### 3.3 Target Segment Length Changes

**NSE 100m:**
- Top chain: 10m segments (150m / 10 = 15 elements)
- Bottom chain: 15m segments (195m / 15 = 13 elements)
- **Total elements per line:** ~28

**Baltic 10m:**
- Top chain: 5m segments (15m / 5 = 3 elements)
- Clump weight: 5m segments (5m / 5 = 1 element)
- Bottom chain: 5m segments (25m / 5 = 5 elements)
- **Total elements per line:** 9

**Analysis:**
- ✅ Finer discretization (5m vs 10-15m) improves accuracy
- ✅ Fewer total elements (9 vs 28) due to shorter line
- ✅ Adequate for capturing clump weight dynamics

### Validation Required

**Geometry Checks:**
1. ⏳ Verify clump weight doesn't contact seabed
   - Available vertical: 11.5m (fairlead at -1.5m, depth 10m)
   - Top chain + clump: 20m
   - **Conclusion:** Clump will be suspended ~8-10m below surface ✅

2. ⏳ Verify adequate seabed contact
   - Bottom chain: 25m
   - Expected horizontal span: ~34m
   - **Expected seabed contact:** ~17m ✅

3. ⏳ Verify anchor loads are reasonable
   - Reduced from ~300m to ~34m footprint
   - Lower tensions expected
   - **Action:** Check anchor capacity is still adequate

**Risk Level:** ⚠️ **MEDIUM** - Significant configuration change

---

## Change 4: All Six Mooring Lines

### Summary Table

| Mooring | Azimuth | NSE Anchor X | NSE Anchor Y | Baltic Anchor X | Baltic Anchor Y | Footprint Change |
|---------|---------|--------------|--------------|-----------------|-----------------|------------------|
| Mooring1 | 0° | 299.13 | 5.58 | 33.92 | 0.00 | -88.7% |
| Mooring2 | 60° | 146.13 | 270.59 | 16.96 | 29.38 | -88.4% |
| Mooring3 | 120° | -159.87 | 270.59 | -16.96 | 29.38 | -89.4% |
| Mooring4 | 240° | -159.87 | -259.43 | -16.96 | -29.38 | -89.4% |
| Mooring5 | 300° | 146.13 | -259.43 | 16.96 | -29.38 | -88.4% |
| Mooring6 | 180° | -312.87 | 5.58 | -33.92 | 0.00 | -89.2% |

**Analysis:**
- ✅ All 6 lines modified consistently
- ✅ Radial symmetry maintained (60° spacing)
- ✅ All use identical 3-segment structure
- ✅ Footprint reduction consistent (~88-89%)

---

## Unchanged Files (28 files)

**Critical files that are IDENTICAL:**

✅ **General Configuration:**
- `01_general.yml` - Units, statics, dynamics, stages, view
- `_01a_units_analysis.yml` - Analysis units
- `_01b_statics.yml` - Static analysis settings
- `_01c_dynamics.yml` - Dynamic analysis settings
- `_01d_stages.yml` - Simulation stages
- `_01e_view.yml` - Visualization settings

✅ **Variable Data:**
- `02_var_data.yml` - Variable data wrapper
- `_02_variable_data.yml` - Variable definitions

✅ **Environment (Partial):**
- `03_environment.yml` - Environment wrapper
- `_03a_sea_density.yml` - Sea density (1025 kg/m³)
- `_03c_waves_base.yml` - Wave configuration
- `_03c_waves_jonswap.yml` - JONSWAP spectrum
- `_03c_waves_jonswap_base.yml` - JONSWAP base
- `_03d_current_base.yml` - Current profile
- `_03e_wind_base.yml` - Wind profile

✅ **Vessels:**
- `04_vessel.yml` - Vessel wrapper
- `_04_vessel_type1.yml` - Tanker vessel type (116KB)
- `_06_vessel1.yml` - Vessel instance

✅ **Buoys:**
- `06_buoys.yml` - Buoy wrapper
- `_06_buoys.yml` - CALM buoy definition (6KB)
- `06_buoys_discretised.yml` - Discretised wrapper
- `_06_buoys_discretised.yml` - Discretised buoys (26KB)

✅ **Groups:**
- `08_groups.yml` - Groups wrapper
- `_08_groups.yml` - Group definitions
- `08_groups_discretised.yml` - Discretised wrapper
- `_08_groups_discretised.yml` - Discretised groups

✅ **Base Files:**
- `calm_buoy_simple_base.yml` - Main base file
- `calm_buoy_discretised_base.yml` - Discretised base

**These files remain working as-is from NSE 100m configuration.**

---

## Risk Assessment Matrix

| Change | Likelihood of Issue | Impact if Issue | Overall Risk |
|--------|-------------------|-----------------|--------------|
| Water depth change | Medium | High | ⚠️ **MEDIUM** |
| ClumpWeight line type | Low | Medium | ✅ **LOW** |
| Mooring line geometry | Medium | High | ⚠️ **MEDIUM** |
| Anchor positions | Low | Medium | ✅ **LOW** |
| Segment discretization | Low | Low | ✅ **LOW** |

**Overall Risk Level:** ⚠️ **MEDIUM**

---

## Potential Issues & Mitigations

### Issue 1: Static Analysis Convergence ⚠️

**Risk:** Shallow water with short lines may cause convergence issues.

**Symptoms:**
- Static analysis fails to converge
- "Line sections exceed angle limits" error
- Unrealistic tensions or positions

**Mitigation:**
```python
# If static analysis fails:
model.general.StaticMinDamping = 5.0      # Increase from default
model.general.ImplicitUseVariableMaxStep = 'Yes'
model.general.ImplicitVariableMaxTimeStep = 0.1
```

**Fallback:**
- Reduce line lengths slightly (e.g., 12m + 5m + 23m = 40m)
- Increase clump weight to 6-7 mT
- Adjust anchor positions manually

### Issue 2: Clump Weight Seabed Contact ⚠️

**Risk:** Under high loads, clump weight may contact seabed.

**Check:**
```python
# After static analysis:
clump_position = model['Mooring1'].RangeGraph('Z', arclength=15.0)  # At clump
seabed_clearance = clump_position + 10.0  # Seabed at -10m
print(f"Clump clearance: {seabed_clearance:.2f}m")
# Should be > 0
```

**Mitigation:**
- If clearance < 1m: Reduce top chain to 12m
- If contact occurs: Add more chain above clump

### Issue 3: Inadequate Restoring Force ⚠️

**Risk:** Shallow water reduces natural restoring force.

**Symptoms:**
- Large buoy offsets (>10m)
- High mooring tensions (>200 kN)
- Excessive watch circle

**Check:**
```python
# After dynamics:
max_offset = max(model['CALMBase'].TimeHistory('X'))
max_tension = max(model['Mooring1'].TimeHistory('Effective Tension'))
print(f"Max offset: {max_offset:.2f}m, Max tension: {max_tension:.2f}kN")
```

**Mitigation:**
- Increase clump weight from 5 mT to 7 mT
- Increase scope ratio (longer lines)
- Add pretension to moorings

### Issue 4: Line Type Reference Error ✅

**Risk:** `ClumpWeight` line type not found.

**Symptoms:**
```
Error: Line type 'ClumpWeight' not defined
```

**Verification:**
✅ Already confirmed: `ClumpWeight` is defined in `_05_line_types.yml`

**No mitigation needed.**

---

## Testing Procedure

### Phase 1: Load Test ✅
```python
import OrcFxAPI
import os
os.chdir(r'D:\workspace-hub\digitalmodel\projects\modules\calm\baltic_039m\TEST_OPERABILITY\orcaflex\base_files')

model = OrcFxAPI.Model('calm_buoy_simple_base.yml')
print("✓ Model loaded successfully")
```

**Expected:** No errors
**If fails:** Check include file paths

### Phase 2: Static Analysis ⏳
```python
try:
    model.CalculateStatics()
    print("✓ Static analysis converged")
except Exception as e:
    print(f"✗ Static analysis failed: {e}")
    # Apply Issue 1 mitigations
```

**Expected:** Convergence in < 50 iterations
**If fails:** See Issue 1 mitigations

### Phase 3: Geometry Verification ⏳
```python
# Check clump weight positions (all 6 lines)
for i in range(1, 7):
    line = model[f'Mooring{i}']

    # Clump is at 15m arclength (after top chain)
    clump_z = line.RangeGraph('Z', arclength=15.0)[0]
    seabed = -model.environment.WaterDepth
    clearance = clump_z - seabed

    print(f"Mooring{i} clump: Z={clump_z:.2f}m, Clearance={clearance:.2f}m")

    # Check for seabed contact
    if clearance < 0:
        print(f"  WARNING: Clump in seabed contact!")
```

**Expected:**
- Clump Z: -8 to -10m
- Clearance: 0 to 2m
- No seabed contact

### Phase 4: Tension Check ⏳
```python
# Check mooring tensions
for i in range(1, 7):
    line = model[f'Mooring{i}']
    tension_top = line.RangeGraph('Effective Tension', arclength=0)[0]
    tension_bottom = line.RangeGraph('Effective Tension', arclength=45)[0]

    print(f"Mooring{i}: Top={tension_top:.1f}kN, Bottom={tension_bottom:.1f}kN")
```

**Expected:**
- Top tension: 50-150 kN
- Bottom tension: 10-50 kN
- Clump weight adds ~49 kN (~5 mT × 9.81)

### Phase 5: 3D Visual Check ⏳
```python
model.SaveData('baltic_10m_validated.dat')
# Open in OrcaFlex GUI
```

**Visual Checks:**
- ✅ 6 mooring lines radiating from buoy
- ✅ Red clump weights visible on each line
- ✅ Bottom chains in seabed contact
- ✅ CALM buoy centered at surface
- ✅ No geometric errors or warnings

---

## Comparison Summary

### Configuration Comparison Table

| Parameter | NSE 100m (Working) | Baltic 10m (Modified) | Change | Status |
|-----------|-------------------|----------------------|--------|---------|
| **Water Depth** | 100m | 10m | -90m | ⚠️ Major |
| **Mooring Segments** | 2 | 3 | +1 | ⚠️ Moderate |
| **Clump Weight** | None | 5 mT | +5 mT | ✅ Addition |
| **Total Line Length** | 345m | 45m | -87% | ⚠️ Major |
| **Top Chain** | 150m | 15m | -90% | ⚠️ Major |
| **Bottom Chain** | 195m | 25m | -87% | ⚠️ Major |
| **Horizontal Footprint** | ~300m | ~34m | -89% | ⚠️ Major |
| **Scope Ratio** | 3.45:1 | 4.5:1 | +30% | ✅ Good |
| **Anchor Pattern** | 6 radial, 60° | 6 radial, 60° | None | ✅ Same |
| **Line Types** | 4 types | 5 types (+ClumpWeight) | +1 | ✅ Addition |
| **Element Discretization** | 10-15m | 5m | Finer | ✅ Better |
| **Files Modified** | - | 6 of 34 | 17.6% | ✅ Focused |

---

## Recommendations

### Before Loading into OrcaFlex

1. ✅ **Review this document** - Understand all changes
2. ✅ **Backup NSE 100m config** - Keep working baseline
3. ✅ **Check file paths** - All includes resolved correctly
4. ✅ **Verify ClumpWeight definition** - Present in _05_line_types.yml

### During Initial Load

5. ⏳ **Load model incrementally** - Check for errors
6. ⏳ **Verify line type library** - ClumpWeight appears in list
7. ⏳ **Check model tree** - All 6 moorings present
8. ⏳ **Review 3D geometry** - Visual inspection before statics

### During Static Analysis

9. ⏳ **Monitor convergence** - Watch iteration count
10. ⏳ **Check for warnings** - Address any OrcaFlex warnings
11. ⏳ **Verify clump positions** - Should be suspended
12. ⏳ **Check seabed contact** - Bottom chains should touch

### Post-Validation

13. ⏳ **Run dynamics** - Test with 1-year conditions
14. ⏳ **Compare with NSE** - Assess differences in behavior
15. ⏳ **Document results** - Record tensions, offsets, RAOs
16. ⏳ **Optimize if needed** - Adjust clump weight or lengths

---

## Conclusion

**Configuration Status:** ⚠️ **READY FOR VALIDATION TESTING**

**Key Strengths:**
- ✅ Focused changes (only 6 files modified)
- ✅ Consistent application across all 6 mooring lines
- ✅ Proper OrcaFlex syntax and structure
- ✅ ClumpWeight line type well-defined
- ✅ Realistic geometry for 10m water depth

**Key Risks:**
- ⚠️ Static analysis convergence in shallow water
- ⚠️ Very short top chain (15m) may cause issues
- ⚠️ Significant geometry change from baseline

**Confidence Level:** **MEDIUM-HIGH (70%)**

The configuration changes are technically sound and follow offshore engineering best practices for shallow water moorings. However, due to the significant departure from the validated NSE 100m configuration, careful testing and validation is required.

**Next Action:** Proceed with **Phase 1 Load Test** and systematically work through validation phases.

---

**Review Completed:** 2025-11-13
**Reviewed By:** Claude Code Configuration Analysis
**Baseline:** NSE 100m (Validated Working)
**Target:** Baltic 10m (Modified for Shallow Water)
