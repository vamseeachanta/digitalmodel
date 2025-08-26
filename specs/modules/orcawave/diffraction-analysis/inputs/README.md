# Input Files for Sea Cypress Diffraction Analysis

## Geometry Files

Located in `geometry/` subdirectory:

### Available Formats

1. **sea_cypress_trimesh.gdf** (READY FOR ORCAWAVE) ✅
   - WAMIT GDF format
   - Converted from STL using Trimesh
   - 24,332 panels, watertight mesh
   - 100% correct normal orientation
   - Ready for direct OrcaWave import

2. **Sea Cypress_0.25 Mesh_Binary.stl** (SOURCE GEOMETRY)
   - Binary STL format
   - Most efficient for conversion
   - Panel size: 0.25 meters
   - Compact file size

3. **Sea Cypress_0.25 Mesh_Ascii.stl**
   - ASCII STL format
   - Human-readable
   - Same mesh as binary version
   - Larger file size

4. **Sea Cypress_0.25 Mesh_Binary.obj**
   - Wavefront OBJ format
   - Alternative format
   - May require additional conversion

5. **conversion_comparison_results.json**
   - Detailed mesh quality metrics
   - Conversion performance data
   - Validation results

## Mesh Properties

- **Panel Size**: 0.25 meters
- **Vessel Type**: Tug
- **Approximate Dimensions**:
  - Length: ~30 meters
  - Beam: ~10 meters
  - Draft: ~3.5 meters

## Usage

These geometry files are referenced by the OrcaWave diffraction module configuration:
- Primary config: `src/modules/orcawave/diffraction/configs/vessels/sea_cypress.yml`
- Validation script: `src/modules/orcawave/diffraction/scripts/validate_geometry.py`

## Validation

To validate these geometry files:

```bash
# From repository root
uv run python src/modules/orcawave/diffraction/scripts/validate_geometry.py \
    --path specs/modules/orcawave/sea-cypress-diffraction-analysis/inputs/geometry \
    --vessel sea_cypress
```

## Source

Original geometry files from:
- Project: B1512 Nork & Mendez (legal) Hyundai Peacepia vs Sea Cypress sinking
- Source: ACMA Work / Sea Cypress Tug Data / Rhino Model

## Configuration Files

### orcawave_example_ship_raos.yml
A working example OrcaWave configuration file demonstrating:
- Full QTF calculation setup
- Environment parameters (water depth: 30m, density: 1.025)
- Wave period range: 2-22 seconds
- Wave heading range: 0-180 degrees (9 directions)
- Body properties and mass/inertia matrices
- Panel mesh settings and control surface configuration
- Direct LU solver parameters
- Haskind and control surface load calculation methods

This example serves as a go-by reference for configuring OrcaWave diffraction analysis.

## Notes

- All mesh files represent the same geometry with identical panel sizing
- Binary STL is recommended for best performance
- Mesh has been validated for watertight properties
- Panel size of 0.25m is suitable for frequencies up to 3.0 rad/s
- Example YAML configuration provided for reference setup