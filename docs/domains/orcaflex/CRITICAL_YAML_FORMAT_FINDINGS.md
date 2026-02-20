# CRITICAL: OrcaFlex YAML Format Findings

**Date:** 2025-11-15
**Status:** 🔴 **PHASE 1 IMPLEMENTATION INVALID** - Generated files do not load in OrcaFlex
**Impact:** HIGH - All generated templates require complete rewrite

## Executive Summary

Testing revealed that **NONE** of the Phase 1 generated files load successfully in OrcaFlex. The templates were built using incorrect YAML syntax and structure based on incomplete understanding of OrcaFlex's YAML format.

### Key Finding

OrcaFlex YAML format has **TWO valid approaches**:
1. **Modern flat YAML** - Direct property syntax (simpler, newer)
2. **Traditional include-based** - List of include files (current manual approach)

**Generated files used a hybrid approach that OrcaFlex does not support.**

## Test Results

```
File Loading Test Results:
❌ test_calm_buoy_base.yml - FAILED
❌ 01_general.yml - FAILED (ImplicitVariableMaxTimeStep not recognized)
❌ 02_var_data.yml - FAILED (WaterDepth not recognized as variable)
❌ 04_vessel.yml - FAILED (VesselType context not recognized)
❌ 05_lines.yml - FAILED (CategoryType not recognized)
❌ 06_buoys.yml - FAILED (Diameter property not recognized)
❌ env_10yr_000deg.yml - FAILED (WaveType in wrong context)

Success Rate: 0/7 (0%)
```

## Correct OrcaFlex YAML Formats

### Format 1: Modern Flat YAML (Preferred for Simple Models)

**From:** `NSE_CALM_001_000deg_1yr_simple/test_minimal_complete.yml`

```yaml
%YAML 1.1
---
General:
  UnitsSystem: SI

Environment:
  WaterDepth: 300

VesselTypes:
  - Name: Simple_Buoy
    Length: 10

LineTypes:
  - Name: Chain
    OD: 0.12
    EA: 407000e3

Vessels:
  - Name: CALM_Buoy
    VesselType: Simple_Buoy
    Connection: Free
    InitialPosition: [0, 0, 0]

Lines:
  - Name: Mooring1
    Connection: [CALM_Buoy, Anchored]
    ConnectionX: [[0, 0], [300, 0]]
    ConnectionZ: [[-1.5, 0], [0.1, 0]]
    LineType: [Chain]
    Length: [350]
```

**Key Features:**
- Top-level sections: `General:`, `Environment:`, `VesselTypes:`, `LineTypes:`, etc.
- YAML lists for arrays: `- [10, 100]`
- Direct property assignment
- NO custom variables (WaterDepth is under Environment:, not a variable)

### Format 2: Include-Based Structure (Used in Manual CALM Projects)

**Master File:** `calm_buoy_simple_base.yml`
```yaml
- includefile: 01_general.yml
- includefile: 02_var_data.yml
- includefile: 03_environment.yml
- includefile: 04_vessel.yml
- includefile: 06_buoys.yml
- includefile: 05_lines.yml
- includefile: 08_groups.yml
- includefile: _90_calculated_positions_simple.yml
```

**Analysis Model:** `CALM_atb_000deg_1yr_simple.yml`
```yaml
BaseFile: ../base_files/calm_buoy_simple_base.yml
includefile: ../base_files/env/_env_000deg_1yr.yml
```

**Environment Composite:** `_env_000deg_1yr.yml`
```yaml
Environment:
  - includefile: ../base_files/env/waves_000deg_1yr.yml
  - includefile: ../base_files/env/current_000deg_1yr.yml
  - includefile: ../base_files/env/wind_000deg_1yr.yml
```

**Key Features:**
- Top-level is a **list of include files** (starts with `-`)
- Each modular file contains section-specific properties
- Nested includes supported (`04_vessel.yml` → `includefile: _04_vessel_type1.yml`)
- Environment properties grouped under `Environment:` context

## Correct Modular File Formats

### Stages File (`_01d_stages.yml`)

```yaml
# Stages
StartTime: ~
FirstStage: ~
RampStartTime: ~
RampFinishTime: ~
TimeHistoryImportFrom: ~
TimeHistoryImportTo: ~
StageDuration:
  - 10      # ✅ YAML list format works for StageDuration
  - 100
RestartStateRecordingPeriodicCount: 0
RestartStateRecordingTest:
```

**✅ Correct:** `StageDuration` as YAML list
**❌ Generated:** Added invalid `ImplicitVariableMaxTimeStep: ~`

### Wave File (`waves_000deg_1yr.yml`)

```yaml
KinematicStretchingMethod: Vertical stretching
UserSpecifiedRandomWaveSeeds: No
WaveFrequencySpectrumDiscretisationMethod: Equal energy
WaveTrains:
  - Name: Wave 1
    WaveType: JONSWAP
    WaveDirection: 0
    WaveOrigin: [0, 0]
    WaveTimeOrigin: 0
    WaveNumberOfSpectralDirections: 1
    WaveJONSWAPParameters: Partially specified
    WaveHs: 2.5
    WaveTz: 5.3
    WaveGamma: 3.3
    WaveNumberOfComponents: 100
    WaveSpectrumMinRelFrequency: 0.5
    WaveSpectrumMaxRelFrequency: 10
    WaveSpectrumMaxComponentFrequencyRange: 0.05
```

**✅ Correct:** `WaveTrains:` is a **list** of wave train objects
**❌ Generated:** Used flat `WaveType: JONSWAP` at top level (invalid context)

### Vessel Type File (`_04_vessel_type1.yml`)

```yaml
New: VesselType1
VesselType1:
  Name: VesselType1
  Length: 103
  RAOResponseUnits: degrees
  RAOWaveUnit: amplitude
  WavesReferredToBy: period (s)
  RAOPhaseConvention: lags
  # ... (many convention settings)
  Draughts:
    - Name: Draught1
      Mass: 9017.95
      MomentOfInertiaTensorX, MomentOfInertiaTensorY, MomentOfInertiaTensorZ:
        - [254.9374465e3, 0, 0]
        - [0, 5.979802645e6, 0]
        - [0, 0, 5.979802645e6]
      CentreOfMass: [2.53, 0, -1.974]
      DisplacementRAOs:
        RAOOrigin: [2.53, 0, -1.974]
        PhaseOrigin: [~, ~, ~]
        RAOs:
          - RAODirection: 0
            RAOPeriodOrFrequency, RAOSurgeAmp, RAOSurgePhase, ...:
              - [0, 0, 360, 0, 0, ...]
              - [4, 0.00624, 227.13, ...]
```

**✅ Correct:** Named vessel type with `New: VesselType1` declaration
**❌ Generated:** Used `Vessel:` → `VesselType:` nested structure (invalid)

### Vessel Instance File (`_06_vessel1.yml`)

```yaml
New: Vessel1
Vessel1:
  VesselType: VesselType1
  InitialPosition: [0, 0, 0]
  InitialAttitude: [0, 0, 0]
  IncludedInStatics: Yes
```

**Format:** Vessel instances reference vessel types by name

## What Was Wrong in Generated Templates

### 1. Invalid General Properties

```yaml
# ❌ GENERATED (WRONG)
General:
  StageDuration:
    - -10.0  # ❌ Negative value (fixed to 10.0)
    - 100.0
  ImplicitVariableMaxTimeStep: ~  # ❌ Property doesn't exist!

# ✅ CORRECT
StageDuration:  # Not under General: in modular files
  - 10
  - 100
```

### 2. Invalid Variable Declarations

```yaml
# ❌ GENERATED (WRONG)
WaterDepth: 39.0
NumMooringLines: 6
VesselLength: 195.0

# ✅ CORRECT
# No custom variables in OrcaFlex YAML!
# WaterDepth is a property under Environment:
Environment:
  WaterDepth: 39.0
```

### 3. Invalid Wave Configuration

```yaml
# ❌ GENERATED (WRONG)
WaveType: JONSWAP
WaveDirection: 0
WaveHs: 3.5

# ✅ CORRECT
WaveTrains:
  - Name: Wave 1
    WaveType: JONSWAP
    WaveDirection: 0
    WaveHs: 3.5
    WaveTz: 7.5
    WaveGamma: 3.3
```

### 4. Invalid Vessel Structure

```yaml
# ❌ GENERATED (WRONG)
Vessel:
  - Name: Vessel1
    VesselType:
      Name: crowley650_atb
      Length: 195.0

# ✅ CORRECT (Separate VesselType and Vessel)
# In vessel type file:
New: crowley650_atb
crowley650_atb:
  Name: crowley650_atb
  Length: 195.0

# In vessel instance file:
New: Vessel1
Vessel1:
  VesselType: crowley650_atb
  InitialPosition: [0, 0, 0]
```

### 5. Invalid Line Properties

```yaml
# ❌ GENERATED (WRONG)
LineType:
  - Name: MooringChain
    CategoryType: General  # ❌ Property doesn't exist!

# ✅ CORRECT
LineTypes:
  - Name: MooringChain
    OD: 0.084
    EA: 1.0e9
    # No CategoryType property
```

## Root Cause Analysis

### Assumptions Made (Incorrect)

1. **Assumed** OrcaFlex YAML accepts arbitrary custom variables
   - **Reality:** Only predefined properties are valid

2. **Assumed** flat property structure like `WaveType: JONSWAP`
   - **Reality:** Waves must be under `WaveTrains:` list

3. **Assumed** nested inline vessel type definitions
   - **Reality:** VesselTypes and Vessels are separate sections

4. **Assumed** general Python/YAML conventions apply
   - **Reality:** OrcaFlex has specific property names and contexts

5. **Assumed** templates from POC were correct
   - **Reality:** POC templates were never validated in OrcaFlex

### Why This Happened

1. **No OrcaFlex validation** during template development
2. **Relied on documentation** instead of working reference files
3. **Prototype success** (file generation) mistaken for correctness (OrcaFlex compatibility)
4. **Incomplete reference analysis** - didn't study actual working files deeply enough

## Recommended Fix Approach

### Option 1: **Complete Template Rewrite** (Recommended)

**Pros:**
- Generates files that actually work in OrcaFlex
- Follows proven patterns from manual work
- Can be validated by loading in OrcaFlex

**Cons:**
- Requires rewriting all 13 templates
- Requires updating generators for new structure
- Existing tests need updates

**Estimated Effort:** 4-6 hours

### Option 2: **Abandon Generator, Use File Copying**

**Pros:**
- Faster short-term solution
- Uses proven working files
- No template complexity

**Cons:**
- Not truly automated
- Harder to parameterize
- Defeats purpose of agent

**Not Recommended**

### Option 3: **Hybrid Approach - Simple Models Only**

Generate simple flat YAML models (Format 1) for basic scenarios, use manual files for complex cases.

**Pros:**
- Simpler templates
- Works for POC/testing
- Faster to implement

**Cons:**
- Doesn't match manual project structure
- Limited to simple models
- Two different approaches confusing

## Correct Template Structure (Format 2 - Include-Based)

### Master Base File Template
```jinja2
- includefile: 01_general.yml
- includefile: 02_environment.yml
- includefile: 04_vessel_types.yml
- includefile: 05_vessels.yml
- includefile: 06_line_types.yml
- includefile: 07_lines.yml
- includefile: 08_buoys.yml
- includefile: 09_groups.yml
```

### Stages Template (`01_general.yml`)
```jinja2
StageDuration:
  - {{ stage_duration_buildup }}
  - {{ stage_duration_simulation }}
StaticsMinDamping: 5
DynamicsSolutionMethod: Implicit time domain
```

### Wave Template (`waves_{{ heading }}_{{ period }}.yml`)
```jinja2
WaveTrains:
  - Name: Wave 1
    WaveType: JONSWAP
    WaveDirection: {{ heading }}
    WaveHs: {{ Hs }}
    WaveTz: {{ Tz }}
    WaveGamma: {{ gamma }}
    WaveNumberOfComponents: 100
```

### Environment Composite Template
```jinja2
Environment:
  - includefile: waves_{{ heading }}_{{ period }}.yml
  - includefile: current_{{ heading }}_{{ period }}.yml
  - includefile: wind_{{ heading }}_{{ period }}.yml
```

## Action Items

### Immediate (Must Do)

1. ✅ **Document findings** (this document)
2. ⏳ **Get user approval** on fix approach
3. ⏳ **Rewrite templates** using correct OrcaFlex format
4. ⏳ **Add OrcaFlex loading test** to CI/test suite
5. ⏳ **Regenerate test files** and validate in OrcaFlex
6. ⏳ **Update documentation** to reflect correct approach

### Future (Phase 2+)

1. Create OrcaFlex YAML validator
2. Add template validation tests
3. Extract vessel data from RAO/AMD files
4. Generate complete vessel type definitions

## Lessons Learned

1. **Always validate with actual tool** - File generation ≠ File validity
2. **Study working examples first** - Documentation may be incomplete
3. **Test early, test often** - Could have caught this in POC phase
4. **Don't assume format conventions** - Every tool has specific requirements

## References

**Working Reference Files:**
- `D:\1522\ctr7\rev_a09\base_files\fsts_l095_lwl.yml` - Include-based structure
- `D:\workspace-hub\digitalmodel\projects\modules\calm\baltic_039m\...test_minimal_complete.yml` - Flat YAML
- `D:\workspace-hub\digitalmodel\projects\modules\calm\baltic_039m\...\_04_vessel_type1.yml` - Vessel type format
- `D:\workspace-hub\digitalmodel\projects\modules\calm\baltic_039m\...\waves_000deg_1yr.yml` - Wave format

---

**Status**: Waiting for user decision on fix approach
**Priority**: HIGH - Blocks Phase 1 completion
**Impact**: All generated files invalid until fixed
