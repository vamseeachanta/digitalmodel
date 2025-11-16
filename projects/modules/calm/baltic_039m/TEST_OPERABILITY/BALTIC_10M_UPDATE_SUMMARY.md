# Baltic CALM Buoy - 10m Water Depth Configuration Update

**Date:** 2025-11-13
**Project:** Baltic 3.9m CALM Buoy - TEST_OPERABILITY
**Update Type:** Water Depth Reduction and Clump Weight Addition

## Summary

Successfully updated the Baltic CALM buoy configuration from 100m to 10m water depth with 5 mT clump weights added to each of the 6 mooring lines.

## Changes Made

### 1. Project Configuration (`project_config.yml`)

**Water Depth:**
- Changed from: 120m → 10m (line 34)

**Mooring Line Segments:**
Updated from 2-segment to 3-segment configuration:

| Segment | Type | Length (old) | Length (new) | Mass per Meter | Notes |
|---------|------|--------------|--------------|----------------|-------|
| Top Chain | R4 Studless Chain (76mm) | 150m | 15m | 88.3 kg/m | Fairlead to clump weight |
| Clump Weight | Steel Clump Weight | - | 5m | 1000 kg/m | **NEW**: 5 mT total mass |
| Bottom Chain | R4 Studless Chain (76mm) | 200m | 25m | 88.3 kg/m | Clump weight to anchor |

**Total Line Length:** 350m (old) → 45m (new)

### 2. OrcaFlex Seabed Configuration (`_03b_seabed.yml`)

- Water depth: 100m → 10m (line 4)
- Seabed type: Flat
- Seabed model: Elastic
- Normal stiffness: 100 kN/m

### 3. OrcaFlex Line Types (`_05_line_types.yml`)

**Added New Line Type: `ClumpWeight`**
```yaml
ClumpWeight:
    OD: 1.0m
    MassPerUnitLength: 1.0 tonne/m (5 mT total over 5m)
    EA: 1e6 kN (very stiff for lumped mass)
    EI: 1e6 kNm² (very stiff for lumped mass)
    Cd: 1.2 (normal and axial drag)
    Ca: 1.0 (added mass coefficient)
```

### 4. Mooring Line Configurations

**Updated Files:**
- `_07_lines.yml` (simple model)
- `_07_lines_discretised.yml` (discretised model)
- `05_lines.yml` (alternative location)
- `05_lines_discretised.yml` (alternative location)

**Mooring Pattern:**
- 6 radial lines at 60° spacing
- Azimuths: 0°, 60°, 120°, 180°, 240°, 300°

**Line Configuration (per mooring):**
```yaml
Sections:
  - [2p5inChain, 15m, 5m segment length]
  - [ClumpWeight, 5m, 5m segment length]
  - [2p5inChain, 25m, 5m segment length]
```

**Geometric Changes:**

| Parameter | Old Value | New Value |
|-----------|-----------|-----------|
| Fairlead depth | -1.5m | -1.5m (unchanged) |
| Fairlead radius | 6.0m | 6.0m (unchanged) |
| Seabed elevation | 0.1143m | 0.1143m (unchanged) |
| Horizontal footprint | ~300m | ~28m |
| Anchor radius | ~306m | ~34m |

## Engineering Rationale

### Water Depth Reduction (100m → 10m)
- Reflects shallow water Baltic Sea conditions
- Typical Baltic water depth: 10-50m
- Suitable for nearshore/coastal CALM buoy installation

### Clump Weight Addition (5 mT per line)
- **Purpose:** Increase mooring system restoring force in shallow water
- **Total Added Mass:** 30 mT (6 lines × 5 mT)
- **Benefits:**
  - Improves catenary shape in shallow water
  - Reduces anchor loads
  - Increases horizontal stiffness
  - Reduces watch circle radius

### Line Length Reduction
- **Methodology:** Adjusted for shallow water catenary
- **Total length:** 45m (vs 350m in deep water)
- **Approximate scope ratio:** 4.5:1 (45m line / 10m depth)
- **Seabed layback:** Significant (~17m expected on seabed)

## Files Modified

### Configuration Files
1. `project_config.yml` - Main project configuration
   - Water depth: line 34
   - Mooring segments: lines 103-126

### OrcaFlex Base Files
2. `_03b_seabed.yml` - Seabed and water depth
   - WaterDepth: line 4

3. `_05_line_types.yml` - Line type definitions
   - Added ClumpWeight: lines 50-98

4. `_07_lines.yml` - Simple model mooring lines
   - All 6 mooring lines updated with 3-segment configuration
   - Loadhose, Hawser, Pivot1, Pivot2 (unchanged)

5. `_07_lines_discretised.yml` - Discretised model mooring lines
   - Identical updates to simple model

6. `05_lines.yml` - Alternative location (simple)
   - Identical to _07_lines.yml

7. `05_lines_discretised.yml` - Alternative location (discretised)
   - Identical to _07_lines_discretised.yml

### Scripts Created
8. `scripts/update_baltic_moorings_10m.py` - Automated update script
   - Calculates catenary footprint
   - Generates mooring line YAML configurations
   - Updates all line files consistently

## Validation Requirements

### OrcaFlex Model Checks
- [ ] Load models in OrcaFlex (simple and discretised)
- [ ] Verify water depth displays as 10m
- [ ] Check mooring line catenary shapes
- [ ] Confirm clump weights appear in 3D view
- [ ] Run static analysis to check equilibrium

### Expected Results
- Clump weights should rest ~8-10m below waterline
- Significant seabed contact (~17m of bottom chain)
- Anchor loads < 150 kN (reduced from deep water)
- Watch circle radius < 50m

### Critical Checks
- ⚠️ **Mooring pretension:** May need adjustment for shallow water
- ⚠️ **Anchor capacity:** Verify adequate for new geometry
- ⚠️ **Clearances:** Check buoy-seabed clearance with waves
- ⚠️ **Dynamic response:** Run time-domain simulation

## Next Steps

1. **Load and validate in OrcaFlex:**
   ```python
   import OrcFxAPI
   model = OrcFxAPI.Model("NSE_CALM_001_000deg_1yr_simple.yml")
   model.CalculateStatics()
   ```

2. **Run operability analysis:**
   - All 12 directions (0°-330° in 30° increments)
   - 1-year, 10-year, 100-year return periods
   - Check motions, tensions, clearances

3. **Document results:**
   - Create validation report
   - Compare with deep water (100m) results
   - Assess suitability for Baltic conditions

4. **Consider design refinements:**
   - Optimize clump weight size (currently 5 mT)
   - Adjust line lengths if needed
   - Verify anchor holding capacity

## References

- Original NSE 100m configuration: `projects/modules/calm/nse_100m/TEST_OPERABILITY/`
- OrcaFlex documentation: Shallow water mooring design
- API RP 2SK: Design and Analysis of Stationkeeping Systems

---

**Script Location:** `scripts/update_baltic_moorings_10m.py`
**Configuration Status:** ✅ Complete
**Validation Status:** ⏳ Pending OrcaFlex testing
