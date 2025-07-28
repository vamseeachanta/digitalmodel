# Output File Log

Describes the 2 folders below:

- Profile
- Elevation

# folder, Profile

Used Profile tool (QGIS plugin) to generate the data.

- Install the Profile tool from the QGIS plugin manager.
  - Open the Profile tool from the menu bar: `Raster` -> `Extraction` -> `Profile tool`
- Click on profileTool icon and generate profile for the line.
  - Add the DEM layer using "Add Layer" option
  - Select the line layer in "Layers" for which the profile is to be generated
  - Change default option to "Selected Layer"  (from "Temporary Polyline")
  - Profile will be generated.
  - Options for avarious outputs are available

## Content Description

- layer_name_3d.dxf: distance along line, X-coordinate, Y-Coordinate, Z-coordinate (Elevation). Uniform scale for all coordinates. Z-coordinate is in mm instead of m. Hence an error of 1000.
- layer_name_2d.dxf: distance along line, Z-coordinate (Elevation). Uniform scale for all coordinates.
- layer_name.png: Picture of distance along line, Z-coordinate (Elevation).
- all_lines.xls : For all lines, distance along line, X-coordinate, Y-Coordinate, Z-coordinate (Elevation).

## layer_names

- 4inch_ct
- 4inch_saline
- 18inch Flowline_main
- 18inch Flowline_infield
- umbilical_main
- umbilical_infield

## Interpretation and Use

- For layer_name_3d.dxf file contains:
  - distance, X and Y are in m.
  - However, Z-coordinate is in mm instead of m. Hence an <span style="color:red"> error of 1000</span>.
- For layer_name_2d.dxf file contains:
  - distance is in m
  - Z-coordinate is also in m
- For all_lines.xls file contains:
  - distance, X and Y and Z are in m.
  - The numbers are in m

# folder, Elevation

Use Elevation Profile (QGIS native) to generate the data.

- In main menu, "view" -> "Elevation Profile"
- "Elevation Profile" window will open
   - Ensure DEM layer is selected for following elevation data to be visible in Elevation profile. See below for details:
    - For older versions (eg: version 3.34) of QGIS, tick the "Represents Spatial Elevation Data" in DEM layer properties -> Elevation tab
  - Choose DEM profile in "Elevation profile" window
- To generate profile data, Select "Capture Curve from Feature"
- The profile will be generated based on the DEM profile chosen in "Elevation profile" window
- Export the data in .dxf and other format. 
- .DXF file will have units missing.

##  Content Description

layer_name_3d.dxf: distance along line, X-coordinate, Y-Coordinate, Z-coordinate (Elevation). Uniform scale for all coordinates. SI units.

### Raw Folder

raw/layer_name_3d.dxf: distance along line, X-coordinate, Y-Coordinate, Z-coordinate (Elevation). Uniform scale for all coordinates. Units are NOT specified.

## Interpretation and Use

- Export to .DXF gives file without units
- Manually added units to the file by adding the following:


`
  $INSUNITS
 70
     6
  9
`

Added a github issue of this unit discrepancy for QGIS project below. Helps in tracking the issue and also give-back to open-source QGIS community: 
https://github.com/qgis/QGIS/issues/59832
