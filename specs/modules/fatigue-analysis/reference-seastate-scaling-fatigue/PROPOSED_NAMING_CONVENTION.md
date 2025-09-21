# Proposed Naming Convention for Seastates

## Problem Statement
We need to clearly distinguish between:
1. **Reference Seastates** - Baseline calibration data (wind01, wave01)
2. **Project/Field Seastates** - Actual environmental conditions to analyze

## Proposed Naming Convention

### 1. Reference Seastates (Baseline Data)
**Format: REF_{type}{##}**
- `REF_WIND01` - Reference wind condition (10 m/s baseline)
- `REF_WAVE01` - Reference wave condition (Hs=0.5m baseline)
- `REF_CURR01` - Reference current condition (if needed)
- `REF_COMB01` - Reference combined condition (if needed)

**File naming:**
```
{vessel}_mwl_REF_WIND01_Strut{#}.csv
{vessel}_mwl_REF_WAVE01_Strut{#}.csv
```

### 2. Project/Field Seastates (Actual Conditions)
**Format: SS{###}** (Sea State)
- `SS001` - Sea State 1 (e.g., operational condition)
- `SS002` - Sea State 2 (e.g., 1-year storm)
- `SS003` - Sea State 3 (e.g., 10-year storm)
- `SS004` - Sea State 4 (e.g., 100-year storm)

**Alternative Format: ENV{###}** (Environmental Condition)
- `ENV001` - Environmental condition 1
- `ENV002` - Environmental condition 2
- etc.

**File naming:**
```
{vessel}_SS{###}_Strut{#}.csv
{vessel}_ENV{###}_Strut{#}.csv
```

### 3. Mapping Table Structure
```
| ID | Type | Description | Wind (m/s) | Hs (m) | Source |
|----|------|-------------|------------|--------|---------|
| REF_WIND01 | Reference | Wind baseline | 10 | - | Calibration |
| REF_WAVE01 | Reference | Wave baseline | - | 0.5 | Calibration |
| SS001 | Field | Operational | 15 | 0.75 | Project spec |
| SS002 | Field | 1-yr storm | 20 | 1.0 | Project spec |
```

## Alternative Options

### Option A: Project-Specific Prefix
- `PRJ_001` - Project condition 001
- `PRJ_002` - Project condition 002
- `REF_WIND01` - Reference wind
- `REF_WAVE01` - Reference wave

### Option B: Clear Type Indicators
- `FIELD_001` - Field condition 001
- `OPER_001` - Operational condition 001
- `STORM_001` - Storm condition 001
- `REF_WIND01` - Reference wind
- `REF_WAVE01` - Reference wave

### Option C: Hybrid Approach
- `REF_W01` - Reference Wind 01
- `REF_S01` - Reference Sea (wave) 01
- `FLD_001` - Field condition 001
- `FLD_002` - Field condition 002

## Questions for Your Review

1. **Primary distinction**: Do you prefer:
   - SS### for field conditions (Sea State)?
   - ENV### for field conditions (Environmental)?
   - PRJ### for field conditions (Project)?
   - Something else?

2. **Reference naming**: Do you prefer:
   - REF_WIND01, REF_WAVE01 (clear but verbose)?
   - REF_W01, REF_S01 (shorter)?
   - Keep as wind01, wave01 (current)?

3. **Should we include condition type in the name?**
   - OPER_001 (operational)
   - STORM_001 (storm)
   - Or just sequential numbers?

4. **File naming pattern preference:**
   - `{vessel}_SS001_Strut1.csv` (simple)
   - `{vessel}_FIELD_SS001_Strut1.csv` (explicit)
   - `{vessel}_mwl_SS001_Strut1.csv` (keep mwl indicator)

## Recommended Approach (My Suggestion)

**For clarity and industry alignment:**
- **References**: `REF_WIND01`, `REF_WAVE01` (explicitly marked as reference)
- **Field/Project**: `SS###` (Sea State - industry standard term)
- **Clear separation**: References always start with REF_, field conditions never do

This makes it immediately obvious which files are reference baselines vs actual project conditions.

## Please Review and Provide Feedback

What changes would you like to make to this naming convention?