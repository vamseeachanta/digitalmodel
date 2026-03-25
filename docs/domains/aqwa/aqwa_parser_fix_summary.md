# AQWA Parser Fix Summary

## Problem Identified

The AQWA .LIS parser was missing a critical feature: **heading inheritance** across multiple period rows within the same heading block.

### Issue Description

In AQWA .LIS files, RAO data is organized in blocks by wave heading. Within each heading block:
- The first row contains: `PERIOD`, `FREQUENCY`, `DIRECTION`, and RAO values
- Subsequent rows contain only: `PERIOD`, `FREQUENCY` (direction column is blank), and RAO values

The direction value must be **inherited** from the first row of each block for all subsequent rows.

### Example Data Format

```
PERIOD   FREQ   DIRECTION     X          Y          Z     ...
------   -----  ---------   ------     ------     ------

62.83   0.100   -180.00    1.5204     0.0000     0.9973   <- Direction EXPLICIT
15.42   0.407              0.6665     0.0000     0.6882   <- Direction INHERITED (-180°)
 8.79   0.715              0.0621     0.0001     0.2758   <- Direction INHERITED (-180°)
 6.15   1.022              0.0258     0.0001     0.0302   <- Direction INHERITED (-180°)

62.83   0.100   -135.00    1.0765     1.0716     0.9987   <- New heading block
15.42   0.407              0.5556     0.5188     0.8445   <- Direction INHERITED (-135°)
...
```

### Impact Before Fix

- **Only first row** of each heading block was captured
- For 5 headings × 8 periods = 40 expected data points, **only 5 were extracted** (one per heading)
- **87.5% data loss**
- RAO plots showed incomplete coverage

## Solution Implemented

### Code Changes

Modified `src/digitalmodel/modules/marine_analysis/parsers/aqwa_lis_parser.py`:

**File**: `aqwa_lis_parser.py`
**Method**: `_parse_rao_section()`
**Lines**: 300-363

#### Change 1: Added State Variable

```python
# Line 303: Added current_heading tracking
current_heading = None  # Track current heading for inheritance
```

#### Change 2: Modified Parsing Logic

```python
# Lines 332-363: Rewrote data parsing to handle heading inheritance

if period_str and freq_str:
    # Line has period and frequency
    period = float(period_str)
    freq = float(freq_str)
    direction_str = line[16:27].strip()

    # Check if direction is provided on this line
    if direction_str:
        # New heading block - direction explicitly provided
        try:
            direction = float(direction_str)
            if -180 <= direction <= 360:
                current_heading = direction  # Track this heading
            else:
                continue  # Invalid direction, skip this line
        except ValueError:
            continue  # Not a valid direction, skip
    else:
        # Direction column is blank - inherit from current_heading
        if current_heading is None:
            continue  # No heading to inherit, skip this line
        direction = current_heading  # <-- KEY FIX: Inherit heading

    current_freq = freq
    current_period = period

    dof_values = self._extract_dof_values(line, 27)

    if dof_values:
        if freq not in rao_data:
            rao_data[freq] = {}
        rao_data[freq][direction] = dof_values
```

## Validation Results

### Test 1: Logic Verification

**File**: `scripts/test_aqwa_heading_fix.py`

Sample data test (4 periods × 2 headings):
```
[PASS] Extracted all 8 data points (4 periods x 2 headings)
[PASS] Correctly identified 2 unique headings
[PASS] Correctly inherited headings for 6 rows

Heading source breakdown:
  Explicit:  2 rows (first row of each heading block)
  Inherited: 6 rows (3 periods × 2 headings)
```

### Test 2: Actual File Extraction

**File**: `docs/domains/aqwa/examples/03_dat/001_ship_raos/001_SHIP_RAOS.LIS`

```
Total data points: 40
Inherited headings: 35
Unique headings: 5

Data points per heading:
  -180.00 deg: 8 periods  ✓
  -135.00 deg: 8 periods  ✓
   -90.00 deg: 8 periods  ✓
   -45.00 deg: 8 periods  ✓
     0.00 deg: 8 periods  ✓

[PASS] Complete extraction: 40 points (5 headings × 8 periods)
```

**File**: `specs/modules/orcaflex/mooring-analysis/go-by-fsts-lngc/aqwa_to_ofx/input/FST2L015_FST1L015_HWL.LIS`

```
Total data points: 53 (varies by section and structure count)
Inherited headings: 52
```

## Results Summary

### Before Fix
- ❌ Extracted: 5 data points (one per heading)
- ❌ Coverage: 12.5% (5 out of 40)
- ❌ Inherited headings: 0

### After Fix
- ✅ Extracted: 40 data points (all rows)
- ✅ Coverage: 100% (40 out of 40)
- ✅ Inherited headings: 35 (87.5% of data correctly inherited)

## Files Changed

1. **Parser Implementation**
   - `src/digitalmodel/modules/marine_analysis/parsers/aqwa_lis_parser.py` (lines 303, 332-363)

2. **Test Scripts Created**
   - `scripts/test_aqwa_heading_fix.py` - Logic verification
   - `scripts/verify_parser_extraction.py` - Full parser test
   - `scripts/aqwa_data_helper.py` - Interactive debugging tool
   - `scripts/aqwa_data_template.csv` - CSV template for manual data entry

## Next Steps

1. ✅ Parser fix implemented and validated
2. ✅ Tests confirm 100% data extraction
3. ⏳ **Next**: Regenerate RAO QA plots with complete data
   - Re-run: `tests/marine_engineering/plots/rao_qa/rao_amplitude_all_dof.png`
   - Expect: Full coverage across all headings and periods

## Technical Notes

### Heading Inheritance Rules

1. **Explicit heading**: Direction column populated → Set `current_heading`
2. **Inherited heading**: Direction column blank → Use `current_heading`
3. **Invalid heading**: No `current_heading` available → Skip row

### Column Layout (Fortran Fixed-Width Format)

- Columns 0-8: Period (seconds)
- Columns 8-16: Frequency (rad/s)
- Columns 16-27: Direction (degrees) - **May be blank**
- Columns 27+: RAO values (6 DOF × 2 for amplitude/phase)

### Edge Cases Handled

1. ✅ First row with no heading (skip)
2. ✅ Invalid direction values (skip)
3. ✅ Section boundaries (stop parsing)
4. ✅ Empty lines and separators (skip)
5. ✅ Multiple structures (parse each independently)

---

**Fix validated**: 2025-10-03
**Validation coverage**: 2 different .LIS files, multiple structures
**Test result**: ✅ PASS - 100% data extraction achieved
