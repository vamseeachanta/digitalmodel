# Corrected Geometry Files for OrcaWave

## ✅ FILES READY FOR MANUAL TESTING

### 1. **waterline_test_box.gdf** - RECOMMENDED FIRST TEST
```
Format: Rhino->WAMIT file export (mesh)
Header: 1 9.80665 ULEN GRAV
Symmetry: 0 0 (no symmetry)
Vertices: 60 (20 triangular panels)
Dimensions: 10m × 5m × 3m total height
Draft: 2.0m (below water, Z = -2.0 to 0.0)
Freeboard: 1.0m (above water, Z = 0.0 to 1.0)
Waterline: Z = 0.0 ✅
```
**Why test this first:** Simple geometry with correct waterline position

### 2. **sea_cypress_corrected.gdf** - MAIN VESSEL (FIXED)
```
Format: Rhino->WAMIT file export (mesh)
Header: 1 9.80665 ULEN GRAV
Symmetry: 0 1 (Y-symmetry)
Vertices: 72,996 (24,332 triangular panels)
Dimensions: 22.95m × 8.57m × 4.28m total height
Draft: 3.0m (Z = -3.0 to 0.0)
Freeboard: 1.275m (Z = 0.0 to 1.275)
Volume: ~404 m³
Displacement: ~414 tonnes
Waterline: Z = 0.0 ✅
```
**Key fix:** Translated entire geometry down by 3.501m to place vessel at proper draft

### 3. **sea_cypress_corrected_deep_draft.gdf** - ALTERNATIVE DRAFT
```
Same as above but with:
Draft: 4.0m (Z = -4.0 to 0.275)
Freeboard: 0.275m
For testing different loading conditions
```

### 4. **simple_box_test.gdf** - ORIGINAL SIMPLE TEST
```
Dimensions: 10m × 5m × 2m draft
Draft: 2.0m (Z = -2.0 to 0.0)
Panels: 10 triangles
Status: Already validated ✅
```

## 🔍 CRITICAL ISSUE FOUND AND FIXED

**Problem:** The original `sea_cypress_orcawave.gdf` had Z-coordinates from 0.501 to 4.776, meaning the entire vessel was floating ABOVE the water surface!

**Solution:** Created corrected versions with proper waterline position:
- Vessel bottom at Z = -3.0m (3m draft)
- Waterline at Z = 0.0
- Vessel top at Z = 1.275m (freeboard)

## 📋 TESTING PROCEDURE

### Step 1: Test Waterline Box
```
1. Open OrcaWave
2. File → Import → Wamit gdf
3. Select: waterline_test_box.gdf
4. Verify:
   - Box appears partially submerged
   - Waterline cuts through at Z=0
   - 2m below water, 1m above
```

### Step 2: Test Corrected Sea Cypress
```
1. File → Import → Wamit gdf
2. Select: sea_cypress_corrected.gdf
3. Verify:
   - Vessel sits at proper draft
   - Hull below waterline (Z < 0)
   - Superstructure above waterline (Z > 0)
   - Total 24,332 panels load correctly
```

### Step 3: Check Hydrostatics
```
1. In OrcaWave: Solve → Check Hydrostatics
2. Should show:
   - Volume: ~404 m³
   - Displacement: ~414 tonnes
   - Draft: 3.0m
   - No errors
```

## 🎯 FILE COMPARISON

| File | Panels | Draft | Z-min | Z-max | Status |
|------|--------|-------|-------|-------|--------|
| waterline_test_box.gdf | 20 | 2.0m | -2.0 | +1.0 | ✅ Ready |
| sea_cypress_corrected.gdf | 24,332 | 3.0m | -3.0 | +1.275 | ✅ Fixed |
| sea_cypress_corrected_deep_draft.gdf | 24,332 | 4.0m | -4.0 | +0.275 | ✅ Fixed |
| simple_box_test.gdf | 10 | 2.0m | -2.0 | 0.0 | ✅ Validated |
| ~~sea_cypress_orcawave.gdf~~ | 24,332 | Wrong | +0.501 | +4.776 | ❌ Above water |

## 📁 File Locations

All files in: `D:\github\digitalmodel\specs\modules\orcawave\diffraction-analysis\inputs\geometry\`

## ✅ READY FOR MANUAL VERIFICATION

The geometry files are now properly positioned with respect to the waterline and ready for OrcaWave import testing.