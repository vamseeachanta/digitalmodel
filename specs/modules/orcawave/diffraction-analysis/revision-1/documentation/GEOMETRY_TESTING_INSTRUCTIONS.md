# OrcaWave Geometry Testing Instructions

## Problem Summary
The Sea Cypress geometry is not being read successfully by OrcaWave. We've created multiple format options to test.

## Available Geometry Files

### 1. Simple Test Geometries (Start Here)
These are minimal geometries to verify OrcaWave can read our GDF format:

- **simple_box_test.gdf** - 10m x 5m x 2m box (10 panels)
- **small_box_test.gdf** - 5m x 3m x 1m box (10 panels)

### 2. Sea Cypress Geometries (Full Vessel)
Different format attempts for the actual vessel:

- **sea_cypress_orcawave.gdf** - GDF format matching L01 example (72,996 vertices)
- **sea_cypress_gmsh_optimized.dat** - AQWA DAT format from GMsh (24,332 panels)
- **sea_cypress_trimesh.gdf** - Original Trimesh conversion (24,332 panels)

## Testing Procedure

### Step 1: Test Simple Box First
1. Open OrcaWave: `C:\Program Files (x86)\Orcina\OrcaFlex\11.5\OrcaWave.exe`
2. **File → New Project**
3. **File → Import → Wamit gdf**
4. Navigate to: `specs/modules/orcawave/diffraction-analysis/inputs/geometry/`
5. Select: `simple_box_test.gdf`
6. If it loads successfully, the format is correct
7. If it fails, note the exact error message

### Step 2: Test with Configuration File
1. In OrcaWave: **File → Open**
2. Navigate to: `specs/modules/orcawave/diffraction-analysis/configs/`
3. Open: `test_simple_box.yml`
4. Check if geometry loads automatically
5. Try: **Solve → Check Hydrostatics**

### Step 3: Test Sea Cypress Geometries
If simple box works, try the vessel geometries in this order:

1. **sea_cypress_orcawave.gdf** (Wamit gdf format)
   - File → Import → Wamit gdf
   - Should show 24,332 panels

2. **sea_cypress_gmsh_optimized.dat** (Aqwa dat format)
   - File → Import → AQWA DAT
   - Should show 24,332 panels

3. Update configuration file to use working geometry:
   ```yaml
   BodyMeshFileName: ../inputs/geometry/[working_file]
   BodyMeshFormat: [Wamit gdf or Aqwa dat]
   ```

## Alternative Methods

### Method A: Direct Creation in OrcaWave
1. Open OrcaWave
2. **Model → New Body → From CAD**
3. Import STL directly: `Sea Cypress_0.25 Mesh_Ascii.stl`
4. Let OrcaWave create its own mesh
5. Save as OrcaWave data file

### Method B: Via OrcaFlex
1. Open OrcaFlex
2. Import the STL or OBJ file
3. Create vessel object
4. Export to OrcaWave format
5. Open in OrcaWave

### Method C: Use Example as Template
1. Open working example: `docs/modules/orcawave/examples/L01_default_vessel/`
2. Load `L01 Vessel mesh.gdf` 
3. Study the exact format
4. Modify our converter to match exactly

## Troubleshooting

### If GDF doesn't load:
- Check OrcaWave error log
- Verify file encoding (should be ASCII)
- Check line endings (CRLF vs LF)
- Verify header format matches examples
- Try with fewer panels

### If geometry loads but looks wrong:
- Check coordinate system (Y-up vs Z-up)
- Verify units (meters)
- Check normal directions
- Verify mesh is watertight

### If analysis fails:
- Check mass properties
- Verify mesh quality
- Ensure sufficient panel density
- Check frequency range

## Expected Results

When geometry loads successfully:
- Vessel should appear in 3D view
- Dimensions: ~30m length, ~9m beam
- Mesh should be smooth, no gaps
- Hydrostatics check should pass
- Volume: ~400-450 m³

## Next Steps After Success

1. Run full diffraction analysis
2. Process results with Python scripts
3. Export to OrcaFlex format
4. Validate against expected response

## Files Location
All files in: `D:\github\digitalmodel\specs\modules\orcawave\diffraction-analysis\`
- Geometries: `inputs/geometry/`
- Configurations: `configs/`
- Scripts: `scripts/`
- Documentation: Current directory