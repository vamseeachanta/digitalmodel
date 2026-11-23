# FreeCAD Simple Shapes

> Basic 3D primitives and composite shapes for FreeCAD
>
> Location: `docs/domains/freecad/simple/`
> Last Updated: 2025-11-11

## Overview

This directory contains FreeCAD files (`.FCStd`) demonstrating basic 3D geometric primitives and simple compositions. All files use parametric feature-based modeling and millimeters as the unit of measurement.

## Files in This Directory

### Basic Primitives

1. **`cube_200mm.FCStd`** - 200×200×200mm cube (Part::Box)
2. **`sphere_r100mm.FCStd`** - Sphere with 100mm radius (Part::Sphere)

### Composite Shapes

3. **`dumbbell_spheres_60mm_cylinder_1000mm.FCStd`** - Two spheres connected by cylinder (Part::MultiFuse)
4. **`side_by_side_sphere_50mm_cylinder_50x1000mm_cube_100mm.FCStd`** - Three shapes displayed horizontally
5. **`tower_cube_100mm_cylinder_50x1000mm_sphere_50mm.FCStd`** - Vertical tower stack (Part::MultiFuse)

---

## File Details

### 1. cube_200mm.FCStd

**Basic box/cube primitive demonstrating Part::Box object.**

**Object Type**: `Part::Box`

**Dimensions**:
- Length: 200mm
- Width: 200mm
- Height: 200mm

**Placement**: Corner at origin (0, 0, 0)

**Properties**:
```
Type: Part::Box
Length: 200.0 mm
Width: 200.0 mm
Height: 200.0 mm
Placement: Px=0, Py=0, Pz=0
```

**Modifying Dimensions**:
1. Open file in FreeCAD
2. Select "cube" object in tree view
3. Modify properties in Property panel:
   - **Length**: X dimension
   - **Width**: Y dimension
   - **Height**: Z dimension
4. Model updates automatically (parametric)
5. Save with new filename following naming convention

---

### 2. sphere_r100mm.FCStd

**Basic sphere primitive demonstrating Part::Sphere object.**

**Object Type**: `Part::Sphere`

**Dimensions**:
- Radius: 100mm (10cm)
- Diameter: 200mm (20cm)

**Placement**: Centered at origin (0, 0, 0)

**Properties**:
```
Type: Part::Sphere
Radius: 100.0 mm
Angle1: -90.0° (bottom hemisphere)
Angle2: 90.0° (top hemisphere)
Angle3: 360.0° (full rotation)
Placement: Px=0, Py=0, Pz=0
```

**Angle Parameters**:
- **Angle1**: Start angle for vertical sweep (latitude)
  - -90° = bottom pole
  - 0° = equator
  - 90° = top pole
- **Angle2**: End angle for vertical sweep (latitude)
- **Angle3**: Horizontal sweep angle (longitude)
  - 360° = full sphere
  - 180° = half sphere (hemisphere)

**Modifying Dimensions**:
1. Select "sphere" object
2. Modify **Radius** property
3. Adjust angles to create partial spheres:
   - Hemisphere: Angle3 = 180°
   - Quarter sphere: Angle1 = 0°, Angle2 = 90°, Angle3 = 180°

---

### 3. dumbbell_spheres_60mm_cylinder_1000mm.FCStd

**Composite shape: two spheres connected by cylinder using boolean union.**

**Object Types**:
- 3 primitives: 2× `Part::Sphere`, 1× `Part::Cylinder`
- 1 union: `Part::MultiFuse`

**Dimensions**:
- Spheres: 60mm radius (120mm diameter, 6cm radius)
- Cylinder: 30mm radius (60mm diameter), 1000mm height (100cm)
- Sphere placement: Z = ±575mm

**Objects**:
1. **cylinder**:
   - Type: Part::Cylinder
   - Radius: 30mm
   - Height: 1000mm
   - Placement: Z = -500mm (centered vertically)
   - Visibility: Hidden (part of union)

2. **sphere** (bottom):
   - Type: Part::Sphere
   - Radius: 60mm
   - Placement: Z = -575mm
   - Visibility: Hidden (part of union)

3. **sphere001** (top):
   - Type: Part::Sphere
   - Radius: 60mm
   - Placement: Z = +575mm
   - Visibility: Hidden (part of union)

4. **union**:
   - Type: Part::MultiFuse
   - Links: cylinder, sphere, sphere001
   - Visibility: Visible (final result)

**Structure**:
```
       ⚫ sphere001 (top, z=575mm)
       │
       │
    ║      ║
    ║  cyl ║  1000mm, centered at z=0
    ║      ║  (z=-500 to z=+500)
       │
       │
       ⚫ sphere (bottom, z=-575mm)
```

**Modifying**:
1. Expand "union" in tree view
2. Show hidden objects (View → Show Hidden)
3. Modify individual sphere/cylinder properties
4. Right-click union → "Recompute" to update
5. Or delete union and create new: Part → Boolean → Union

---

### 4. side_by_side_sphere_50mm_cylinder_50x1000mm_cube_100mm.FCStd

**Three basic primitives displayed side-by-side (no union).**

**Object Types**: Independent objects (no boolean operations)

**Dimensions & Placement**:

1. **sphere**:
   - Type: Part::Sphere
   - Radius: 50mm
   - Placement: X = -200mm, Y = 0, Z = 0

2. **cylinder**:
   - Type: Part::Cylinder
   - Radius: 25mm (50mm diameter)
   - Height: 1000mm
   - Placement: X = 0, Y = 0, Z = -500mm
   - Note: Centered vertically (extends ±500mm from origin)

3. **cube**:
   - Type: Part::Box
   - Length: 100mm
   - Width: 100mm
   - Height: 100mm
   - Placement: X = 150mm, Y = -50mm, Z = -50mm

**Layout**:
```
    ⚫              ║      ║            ▢
  sphere        cylinder             cube
  r=50mm       d=50mm,h=1000        100mm
  x=-200          x=0               x=+150
```

**Purpose**: Visual comparison of all three basic primitives with consistent spacing.

**Modifying**:
- Objects are independent
- Modify each separately in Property panel
- Adjust X placement to change spacing
- Can add more objects for extended comparison

---

### 5. tower_cube_100mm_cylinder_50x1000mm_sphere_50mm.FCStd

**Vertical stack: cube base, cylinder shaft, sphere top using boolean union.**

**Object Types**:
- 3 primitives: `Part::Box`, `Part::Cylinder`, `Part::Sphere`
- 1 union: `Part::MultiFuse`

**Dimensions & Heights**:

| Part | Type | Dimensions | Z Position | Z Range |
|------|------|------------|------------|---------|
| Base | Cube | 100×100×100mm | Z=0 (bottom) | 0 to 100mm |
| Shaft | Cylinder | 25mm radius, 1000mm height | Z=100 | 100 to 1100mm |
| Top | Sphere | 50mm radius | Z=1100 (center) | 1050 to 1150mm |

**Objects**:

1. **cube** (base):
   - Type: Part::Box
   - Dimensions: 100×100×100mm
   - Placement: X=-50mm, Y=-50mm, Z=0
   - Note: Centered on XY plane, sits on ground (Z=0)
   - Visibility: Hidden (part of union)

2. **cylinder** (shaft):
   - Type: Part::Cylinder
   - Radius: 25mm (50mm diameter)
   - Height: 1000mm
   - Placement: Z=100mm (standing on cube)
   - Visibility: Hidden (part of union)

3. **sphere** (top):
   - Type: Part::Sphere
   - Radius: 50mm
   - Placement: Z=1100mm (sitting on cylinder)
   - Visibility: Hidden (part of union)

4. **union**:
   - Type: Part::MultiFuse
   - Links: cube, cylinder, sphere
   - Visibility: Visible (final tower)

**Structure**:
```
Height
1150mm → ⚫ sphere top (r=50mm)
         │
1100mm → ┴ sphere center
         ║
         ║ cylinder (d=50mm, h=1000mm)
         ║
 100mm → ┴ cylinder base / cube top
         ▢
         ▢ cube (100mm)
         ▢
   0mm → ┴ ground level
```

**Modifying**:
1. Show hidden objects to edit primitives
2. Modify dimensions in Property panel
3. Adjust Z placements for different proportions
4. Recompute union after changes

---

## FreeCAD Basics

### File Format

`.FCStd` files are ZIP archives containing:
- `Document.xml` - Object structure and properties
- `*.brp` files - Shape geometry (boundary representation)
- `GuiDocument.xml` - GUI settings (colors, visibility)
- `thumbnails/` - Preview images

**Viewing contents**:
```bash
unzip -l filename.FCStd
```

### Object Types Used

#### Part::Box (Cube)
- Rectangular prism/box primitive
- Properties: Length, Width, Height
- Placement: By corner or center

#### Part::Sphere
- Spherical primitive
- Properties: Radius, Angle1, Angle2, Angle3
- Can create partial spheres with angle parameters

#### Part::Cylinder
- Cylindrical primitive
- Properties: Radius (or Diameter), Height, Angle
- Can create partial cylinders (Angle < 360°)

#### Part::MultiFuse (Union)
- Boolean operation to combine multiple shapes
- Fuses multiple objects into single solid
- Links to source objects

---

## Working with FreeCAD Files

### Opening Files

1. Launch FreeCAD
2. File → Open
3. Navigate to `docs/domains/freecad/simple/`
4. Select `.FCStd` file

### Viewing Objects

**Tree View** (left panel):
- Shows object hierarchy
- Right-click for options
- Toggle visibility with spacebar

**Properties Panel** (left panel, below tree):
- **View tab**: Display properties (color, transparency)
- **Data tab**: Object parameters (dimensions, placement)

**3D View** (center):
- Mouse controls:
  - Middle button: Pan
  - Right button: Rotate
  - Scroll wheel: Zoom
  - Shift+Middle: Rotate around point

### Modifying Objects

1. **Select object** in tree view
2. **Switch to Data tab** in Properties panel
3. **Modify values**:
   - Length, Width, Height (for boxes)
   - Radius (for spheres/cylinders)
   - Placement (position and rotation)
4. **Press Enter** to apply changes
5. Model updates automatically (parametric!)

### Placement System

**Placement Property**:
- **Position (Px, Py, Pz)**: XYZ coordinates in mm
- **Rotation (Q0, Q1, Q2, Q3)**: Quaternion rotation
- **Axis/Angle**: Alternative rotation representation

**Common Placement Operations**:
- Move object: Change Px, Py, Pz values
- Center on origin: Set Px=0, Py=0, Pz=0
- Rotate: Use Placement → Rotation section

---

## Boolean Operations in FreeCAD

### Creating Unions (Fuse)

**Method 1: Menu**
1. Select objects in tree view (Ctrl+Click for multiple)
2. Part → Boolean → Union
3. New "Fusion" object created
4. Source objects hidden but preserved

**Method 2: Part Design**
1. Create first object
2. Create second object
3. Part → Boolean → Fuse
4. Specify objects to fuse

**Properties**:
- **Shapes**: List of linked objects
- **Refine**: Remove redundant edges (optional)

### Other Boolean Operations

- **Cut (Difference)**: Part → Boolean → Cut
  - First object minus second object
- **Common (Intersection)**: Part → Boolean → Common
  - Overlapping volume only
- **Fragments**: Part → Boolean → Boolean Fragments
  - Creates separate pieces

---

## Creating Parametric Variants

### Workflow for Creating Variants

1. **Open existing file** matching desired shape type
2. **Show all objects** (View → Show Hidden)
3. **Modify dimensions**:
   - Select primitive object
   - Change dimensions in Data tab
4. **Update placement** if needed:
   - Adjust Px, Py, Pz for position
5. **Recompute union** (if present):
   - Right-click union → Recompute
   - Or delete and recreate union
6. **Test the model**:
   - Check for overlaps
   - Verify dimensions
7. **Save with new filename**:
   - File → Save As
   - Follow naming convention

### Example: Creating tower_cube_150mm_cylinder_60x1200mm_sphere_60mm.FCStd

Starting from: `tower_cube_100mm_cylinder_50x1000mm_sphere_50mm.FCStd`

1. Open file
2. Show hidden objects
3. Modify cube:
   - Length: 150mm
   - Width: 150mm
   - Height: 150mm
   - Placement: X=-75mm, Y=-75mm, Z=0
4. Modify cylinder:
   - Radius: 30mm (60mm diameter)
   - Height: 1200mm
   - Placement: Z=150mm
5. Modify sphere:
   - Radius: 60mm
   - Placement: Z=1350mm (150 + 1200)
6. Recompute union
7. Save as: `tower_cube_150mm_cylinder_60x1200mm_sphere_60mm.FCStd`

---

## Advanced Features

### Part Design Workbench

For more complex parametric models:
1. Switch to Part Design workbench
2. Create sketches with constraints
3. Use Pad, Pocket, Revolution operations
4. Full parametric history

### Spreadsheet Integration

Create dimension tables:
1. Insert → Spreadsheet
2. Define dimensions as cells
3. Reference in object properties: `=Spreadsheet.A1`
4. Change dimensions by editing spreadsheet

### Python Scripting

Automate model creation:
```python
import FreeCAD
import Part

# Create box
box = FreeCAD.ActiveDocument.addObject("Part::Box", "cube")
box.Length = 200
box.Width = 200
box.Height = 200

# Recompute
FreeCAD.ActiveDocument.recompute()
```

---

## Export Options

### STL Export (3D Printing)

1. Select object to export
2. File → Export
3. Choose "STL Mesh (*.stl)"
4. Settings:
   - Deviation: Lower = smoother (try 0.01)
   - Angular Deviation: Lower = smoother (try 15°)

### STEP Export (CAD Interchange)

1. File → Export
2. Choose "STEP (*.step *.stp)"
3. Good for sharing with other CAD software
4. Preserves parametric features

### Other Formats

- **IGES**: CAD standard format
- **OBJ**: 3D graphics format
- **SVG**: 2D drawings from projections
- **DXF**: 2D CAD format

---

## FreeCAD Best Practices

### 1. Name Objects Meaningfully

**Bad**:
```
Box
Sphere001
Cylinder002
```

**Good**:
```
base_cube
tower_shaft
tower_top
```

### 2. Organize with Groups

Part → Create Group
- Group related objects
- Name groups by function
- Collapse groups to reduce clutter

### 3. Use Spreadsheets for Dimensions

Benefits:
- Central location for all dimensions
- Easy to create variants
- Can use formulas for derived dimensions
- Documentation of design parameters

### 4. Save Incremental Versions

- Use version numbers: `tower_v1.FCStd`, `tower_v2.FCStd`
- Or dates: `tower_2025-11-11.FCStd`
- Keep backups of working versions

### 5. Document Design Intent

Add notes:
1. Insert → Annotation → Text
2. Describe purpose, constraints, assumptions
3. Note special requirements

---

## Troubleshooting

### Common Issues

**Issue**: "Recompute failed"
- **Cause**: Invalid geometry or circular references
- **Solution**: Check object dependencies, ensure valid dimensions

**Issue**: Objects disappear after boolean operation
- **Cause**: Source objects hidden automatically
- **Solution**: View → Show Hidden, or select union and expand tree

**Issue**: Union creates unexpected result
- **Cause**: Overlapping faces or touching vertices
- **Solution**: Adjust placements to ensure clean intersections

**Issue**: Cannot modify dimensions
- **Cause**: Object is locked or part of assembly
- **Solution**: Right-click → Toggle Locked, or edit source object

**Issue**: File won't open
- **Cause**: Corrupted file or version incompatibility
- **Solution**: Try opening in newer FreeCAD version, or check file integrity

---

## Performance Tips

### Optimizing Large Models

1. **Use simple shapes when possible**
   - Avoid unnecessary boolean operations
   - Simplify geometry

2. **Reduce display quality during editing**
   - View → Display Mode → Wireframe
   - Edit → Preferences → Display → 3D View

3. **Disable automatic recompute**
   - Edit → Preferences → Document → Recompute automatically
   - Manual recompute: Edit → Refresh or Ctrl+R

---

## File Naming Convention

When creating new shapes, follow this pattern:

```
<description>_<dimensions>.FCStd
```

**Examples**:
- `cube_150mm.FCStd` - 150mm cube
- `sphere_r75mm.FCStd` - Sphere with 75mm radius
- `cylinder_40x500mm.FCStd` - 40mm diameter, 500mm height
- `tower_cube_80mm_cylinder_40x800mm_sphere_40mm.FCStd` - Composite

---

## Unit System

All files use: **Standard (mm, kg, s, °)**

Check/change units:
1. Edit → Preferences → General → Units
2. Select "Standard (mm, kg, s, °)"

Dimension entry:
- Direct numbers are in mm: `200` = 200mm
- Can specify units: `20cm` = 200mm = `0.2m`

---

## Learning Resources

### Official Documentation
- FreeCAD Wiki: https://wiki.freecad.org/
- Getting Started: https://wiki.freecad.org/Getting_started
- Part Workbench: https://wiki.freecad.org/Part_Workbench

### Video Tutorials
- FreeCAD Official: https://www.youtube.com/@FreeCADNews
- Mango Jelly Solutions: https://www.youtube.com/@MangoJellySolutions

### Community
- FreeCAD Forum: https://forum.freecad.org/
- Reddit: r/FreeCAD
- Discord: FreeCAD Community Server

---

## Related Documentation

- **Main Overview**: `docs/domains/SIMPLE_SHAPES_OVERVIEW.md`
- **OpenSCAD Shapes**: `docs/domains/openscad/simple/README.md`
- **Parametric Design Guide**: See main overview document

---

## Contributing

To add new shapes:
1. Follow naming convention
2. Use meaningful object names
3. Create clean boolean operations (if applicable)
4. Test exports (STL, STEP)
5. Update this README
6. Save with descriptive filename

---

**Last Updated**: 2025-11-11
**FreeCAD Version**: Compatible with FreeCAD 0.20 and later
**File Format Version**: FCStd format version 1.0
