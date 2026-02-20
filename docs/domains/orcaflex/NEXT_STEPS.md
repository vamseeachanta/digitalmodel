# OrcaFlex Agent - Next Steps

**Date:** 2025-11-17
**Current Status:** Wrapper template system implemented, 2/9 files validated
**Last Commit:** bd1e5bef - Implement wrapper template system for OrcaFlex agent

## Current State

### ✅ Completed
1. **Wrapper Template System (100%)**
   - 3-tier structure: Master → Wrapper → Data
   - 4 wrapper templates created
   - 4 data templates renamed
   - Pure list format in master file
   - Correct include order (types before instances)

2. **Core Validation (22%)**
   - ✅ 01_general.yml loads successfully in OrcaFlex 11.5e
   - ✅ 02_environment.yml loads successfully in OrcaFlex 11.5e

3. **Generator Updates (100%)**
   - Now generates 13 files (wrapper + data pairs)
   - Methods return tuples for multi-file generation
   - Proper file naming conventions

### 🔧 Current Issue

**LineType Reference Error:**
```
Error: Failed to set LineType[0]=MooringChain (invalid index)
Location: _07_lines_data.yml (line 8)
Context: Lines trying to reference LineType before it's defined
```

**Potential Causes:**
1. LineTypes list format may be incorrect for OrcaFlex 11.5e
2. LineTypes properties may have invalid values
3. File loading order issue (though order appears correct)
4. LineTypes context may need different structure

## Immediate Next Steps (1-2 hours)

### Step 1: Validate LineTypes Structure
**Goal:** Confirm LineTypes format matches OrcaFlex 11.5e expectations

**Actions:**
```bash
# Test LineTypes file individually
python scripts/test_orcaflex_loading.py tests/output/test_cli_base/06_line_types.yml
```

**If it loads:** LineTypes is valid, problem is in Lines reference
**If it fails:** LineTypes needs format/property fixes

### Step 2: Compare with Reference
**Goal:** Identify differences between our LineTypes and working reference

**Actions:**
1. Read reference line type file:
   ```
   projects/modules/calm/baltic_039m/TEST_OPERABILITY/orcaflex/base_files/_05_line_types.yml
   ```
2. Compare structure:
   - Context wrapper format
   - List vs object format
   - Property names and values
   - Required vs optional properties

### Step 3: Test Minimal LineTypes
**Goal:** Start with absolute minimum properties

**Actions:**
1. Create minimal LineTypes template with only:
   ```yaml
   LineTypes:
     - Name: MooringChain
       OD: 0.084
       MassPerUnitLength: 49.0
       EA: 1000000000.0
   ```
2. Test loading
3. Add properties incrementally

### Step 4: Debug Lines Reference
**Goal:** Ensure Lines correctly reference LineType

**Actions:**
1. Check if LineType reference uses correct syntax
2. Verify EndA/EndB connections exist
3. Test single line before multiple lines
4. Confirm anchor coordinates are valid

## Detailed Investigation Path

### Option A: LineTypes Format Issue (Most Likely)
**Time:** 30 minutes

1. Check if LineTypes should use `New:` declarations instead of list
2. Reference format might be:
   ```yaml
   LineTypes:
     New: MooringChain
     MooringChain:
       OD: 0.084
       # ...
   ```
3. Test both formats

### Option B: Property Validation (Medium Likelihood)
**Time:** 1 hour

1. Extract EXACT properties from working reference file
2. Remove all speculative properties
3. Build up template property by property
4. Validate each property loads

### Option C: Include Order Issue (Low Likelihood)
**Time:** 30 minutes

1. Despite correct order in master, check if OrcaFlex processes differently
2. Try merging LineTypes into single file with Lines
3. Test loading sequence manually

## Testing Strategy

### Test Script Usage
```bash
# Individual file
python scripts/test_orcaflex_loading.py <file.yml>

# Master file (default)
python scripts/test_orcaflex_loading.py

# With regeneration
cd D:/workspace-hub/digitalmodel && \
uv run python -m digitalmodel.agents.orcaflex.cli generate base-files \
  --project test --output tests/output/test_cli_base \
  --vessel crowley650_atb --water-depth 39.0 --mooring-lines 8 && \
cp tests/output/test_cli_base/test_base.yml tests/output/test_cli_base/test_calm_buoy_base.yml && \
python scripts/test_orcaflex_loading.py
```

## Success Criteria

### Short Term (Next Session)
- [ ] LineTypes file loads individually in OrcaFlex
- [ ] Lines correctly reference MooringChain LineType
- [ ] At least 5/9 base files loading successfully
- [ ] Master file loads without errors

### Medium Term (1-2 sessions)
- [ ] All 9 base files load individually
- [ ] Complete master file loads in OrcaFlex
- [ ] Model can run static analysis
- [ ] Model can run dynamic simulation

### Long Term (Complete)
- [ ] Unit tests updated and passing
- [ ] Environmental files validated
- [ ] Documentation complete
- [ ] Ready for production use

## Key Learnings So Far

1. **Property Context is Critical:**
   - ALL properties must be under correct contexts
   - General:, Environment:, VesselTypes:, Vessels:, Lines:, 6DBuoys:

2. **File Structure Pattern:**
   - Master: Pure list of includes
   - Wrapper: Provides context + include directive
   - Data: Actual New: declarations at top level

3. **Order Matters:**
   - Types (VesselTypes, LineTypes) before instances (Vessels, Lines)
   - General settings before environment
   - Environment before objects

4. **Version-Specific Properties:**
   - OrcaFlex 11.5e doesn't support all documented properties
   - Minimal property approach is safest
   - Reference files are source of truth

5. **Incremental Testing Works:**
   - Test individual files first
   - Build complexity gradually
   - Validate at each step

## Resources

### Reference Files (OrcaFlex 11.5e Compatible)
- `projects/modules/calm/baltic_039m/TEST_OPERABILITY/orcaflex/base_files/`
  - `_01*.yml` - General settings
  - `_04_vessel_type1.yml` - Vessel type definition
  - `_05_line_types.yml` - Line type definitions
  - `_06_vessel1.yml` - Vessel instance
  - `_07_lines.yml` - Line instances

### Documentation
- `docs/domains/orcaflex/CRITICAL_YAML_FORMAT_FINDINGS.md`
- `docs/domains/orcaflex/TEMPLATE_FIX_STATUS.md`
- `docs/domains/orcaflex/NEXT_STEPS.md` (this file)

### Test Scripts
- `scripts/test_orcaflex_loading.py` - OrcaFlex validation script

### Generated Output
- `tests/output/test_cli_base/` - All generated files for testing

## Command Quick Reference

```bash
# Generate files
uv run python -m digitalmodel.agents.orcaflex.cli generate base-files \
  --project test --output tests/output/test_cli_base \
  --vessel crowley650_atb --water-depth 39.0 --mooring-lines 8

# Test loading
python scripts/test_orcaflex_loading.py [file.yml]

# View generated file
cat tests/output/test_cli_base/<filename>

# Compare with reference
cat projects/modules/calm/baltic_039m/TEST_OPERABILITY/orcaflex/base_files/<reference_file>

# Git status
git status
git diff
git add -A
git commit -m "message"
git push
```

---

**Last Updated:** 2025-11-17 01:40 UTC
**Next Session Focus:** LineTypes format validation and debugging
