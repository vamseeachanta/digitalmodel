# Simple 3D Shapes - Overview Documentation

> Comprehensive documentation for basic 3D primitive shapes in OpenSCAD and FreeCAD
>
> Last Updated: 2025-11-11
> Location: `docs/modules/openscad/simple/` and `docs/modules/freecad/simple/`

## Table of Contents

1. [Overview](#overview)
2. [File Naming Convention](#file-naming-convention)
3. [Available Shapes](#available-shapes)
4. [OpenSCAD vs FreeCAD](#openscad-vs-freecad)
5. [Parametric Design Guide](#parametric-design-guide)
6. [File Renaming History](#file-renaming-history)

---

## Overview

This collection contains basic 3D geometric primitives and simple composite shapes created in both OpenSCAD and FreeCAD. These files serve as:

- **Learning examples** for beginners
- **Parametric templates** for future design variations
- **Reference models** for understanding basic 3D CAD operations
- **Test cases** for CAD software workflows

All shapes use **millimeters (mm)** as the base unit, consistent with standard CAD practices.

---

## File Naming Convention

### Naming Pattern

Files follow a descriptive dimension-based naming convention:

```
<shape_description>_<dimensions>.{scad|FCStd}
```

### Dimension Format

- **Cubes/Boxes**: `cube_<size>mm.scad`
  - Example: `cube_200mm.scad` = 200mm × 200mm × 200mm cube

- **Spheres**: `sphere_r<radius>mm.scad`
  - Example: `sphere_r100mm.scad` = sphere with 100mm radius
  - Note: "r" prefix denotes radius

- **Cylinders**: `cylinder_<diameter>x<height>mm.scad`
  - Example: `cylinder_50x1000mm.scad` = 50mm diameter, 1000mm height

- **Composite Shapes**: `<description>_<component1>_<component2>_<component3>.scad`
  - Example: `tower_cube_100mm_cylinder_50x1000mm_sphere_50mm.scad`
  - Describes shape type and all component dimensions

### Benefits

1. **Self-documenting**: Dimensions visible in filename
2. **Searchable**: Find files by dimension (e.g., search "100mm")
3. **Parametric-ready**: Easy to create variants with different dimensions
4. **Consistent**: Identical naming between OpenSCAD and FreeCAD
5. **Future-proof**: Scalable for parametric libraries

---

## Available Shapes

### 1. Basic Cube - `cube_200mm`

**Dimensions**: 200mm × 200mm × 200mm (20cm cube)

**Files**:
- OpenSCAD: `cube_200mm.scad`
- FreeCAD: `cube_200mm.FCStd`

**Description**: Simple cube demonstrating basic box primitive. Shows two placement options: corner at origin or centered at origin.

**Key Concepts**:
- Basic primitive creation
- Placement/positioning
- Centering options

---

### 2. Basic Sphere - `sphere_r100mm`

**Dimensions**: 100mm radius (200mm diameter, 10cm radius)

**Files**:
- OpenSCAD: `sphere_r100mm.scad`
- FreeCAD: `sphere_r100mm.FCStd`

**Description**: Simple sphere demonstrating spherical primitive. Includes options for direct radius specification and scale transformations.

**Key Concepts**:
- Sphere primitive
- Radius vs diameter
- Facet control ($fn in OpenSCAD)
- Scale transformations

---

### 3. Dumbbell Shape - `dumbbell_spheres_60mm_cylinder_1000mm`

**Dimensions**:
- Spheres: 60mm radius (120mm diameter, 6cm radius)
- Cylinder: 60mm diameter, 1000mm length (100cm, 1 meter)
- Gap between spheres: 150mm (15cm center-to-center)

**Files**:
- OpenSCAD: `dumbbell_spheres_60mm_cylinder_1000mm.scad`
- FreeCAD: `dumbbell_spheres_60mm_cylinder_1000mm.FCStd`

**Description**: Two spheres connected by a cylinder, resembling a dumbbell or barbell. Demonstrates boolean union operations and precise positioning.

**Key Concepts**:
- Boolean union operations
- Translate/positioning
- Composite shapes
- Gap management

**Structure**:
```
[Sphere] ---- [Cylinder] ---- [Sphere]
  60mm         60mm dia         60mm
  radius       1000mm           radius
               length
```

---

### 4. Side-by-Side Display - `side_by_side_sphere_50mm_cylinder_50x1000mm_cube_100mm`

**Dimensions**:
- Sphere: 50mm radius (5cm)
- Cylinder: 50mm diameter, 1000mm height (5cm dia, 100cm height)
- Cube: 100mm sides (10cm cube)
- Spacing: 200mm between centers

**Files**:
- OpenSCAD: `side_by_side_sphere_50mm_cylinder_50x1000mm_cube_100mm.scad`
- FreeCAD: `side_by_side_sphere_50mm_cylinder_50x1000mm_cube_100mm.FCStd`

**Description**: Three basic primitives displayed horizontally for visual comparison. Useful for understanding relative sizes and shapes.

**Key Concepts**:
- Multiple object arrangement
- Horizontal spacing
- Visual comparison
- Independent objects (no union)

**Layout**:
```
[Sphere]  ---200mm---  [Cylinder]  ---200mm---  [Cube]
  50mm                   50x1000mm                100mm
  (left)                 (center)                 (right)
```

---

### 5. Vertical Tower - `tower_cube_100mm_cylinder_50x1000mm_sphere_50mm`

**Dimensions**:
- Base Cube: 100mm × 100mm × 100mm (10cm)
- Middle Cylinder: 50mm diameter, 1000mm height (5cm dia, 100cm)
- Top Sphere: 50mm radius (5cm)
- Total Height: ~1150mm (~115cm)

**Files**:
- OpenSCAD: `tower_cube_100mm_cylinder_50x1000mm_sphere_50mm.scad`
- FreeCAD: `tower_cube_100mm_cylinder_50x1000mm_sphere_50mm.FCStd`

**Description**: Vertical stack of three shapes forming a tower structure. Demonstrates precise vertical positioning and boolean union of multiple objects.

**Key Concepts**:
- Vertical stacking
- Boolean union (multiple objects)
- Cumulative positioning
- Composite structures

**Structure**:
```
     ⚫ Sphere (50mm radius)
      │
      │ Cylinder (50mm dia, 1000mm height)
      │
     ▢ Cube (100mm base)
```

---

## OpenSCAD vs FreeCAD

### File Formats

| Aspect | OpenSCAD | FreeCAD |
|--------|----------|---------|
| **Format** | `.scad` (text/code) | `.FCStd` (ZIP archive with XML) |
| **Approach** | Code-based, procedural | GUI-based, parametric features |
| **Version Control** | Git-friendly (plain text) | Binary (requires special handling) |
| **Parametric** | Variables in code | Feature parameters in properties |
| **Editing** | Any text editor | FreeCAD GUI application |

### Modeling Philosophy

**OpenSCAD**:
- Write code to define geometry
- Constructive Solid Geometry (CSG)
- Functional programming approach
- Ideal for programmatic generation

**FreeCAD**:
- Interactive GUI modeling
- Feature-based parametric design
- Visual manipulation
- Ideal for manual design work

### Advantages

**OpenSCAD**:
- ✅ Excellent for version control (plain text)
- ✅ Easy to parametrize with variables
- ✅ Scriptable and automatable
- ✅ Lightweight files
- ✅ Can generate shapes algorithmically

**FreeCAD**:
- ✅ Visual feedback during modeling
- ✅ Rich feature set (sketches, constraints, etc.)
- ✅ Professional CAD environment
- ✅ Better for complex assemblies
- ✅ Extensive workbenches (FEM, CAM, etc.)

### When to Use Which

**Use OpenSCAD when**:
- Creating parametric libraries
- Generating shapes from data/algorithms
- Need version control integration
- Prefer code-based workflow
- Creating simple geometric shapes

**Use FreeCAD when**:
- Complex mechanical designs
- Need professional CAD features
- Working with assemblies
- Require technical drawings
- Need FEM/analysis tools

---

## Parametric Design Guide

### Creating Variants in OpenSCAD

All OpenSCAD files use variables for dimensions, making it easy to create variants:

**Example: Scaling the cube**

Original `cube_200mm.scad`:
```openscad
cube([200, 200, 200]);
```

Parametric version:
```openscad
// Parameters
cube_size = 200;  // Change this value for different sizes

// Geometry
cube([cube_size, cube_size, cube_size]);
```

**Creating variants**:
- `cube_100mm.scad`: Set `cube_size = 100`
- `cube_300mm.scad`: Set `cube_size = 300`
- `cube_50mm.scad`: Set `cube_size = 50`

### Creating Variants in FreeCAD

FreeCAD stores parameters as properties in each object:

1. Open the `.FCStd` file in FreeCAD
2. Select the object in the tree view
3. Modify properties in the Property panel:
   - **Length**: Change X dimension
   - **Width**: Change Y dimension
   - **Height**: Change Z dimension
   - **Radius**: Change sphere/cylinder radius
4. Save with new filename reflecting new dimensions

### Recommended Parametric Workflow

1. **Start with existing file** matching desired shape type
2. **Modify dimensions** (code in OpenSCAD, properties in FreeCAD)
3. **Test the model** (render in OpenSCAD, recompute in FreeCAD)
4. **Save with new filename** following naming convention
5. **Document** any special features or constraints

### Future Parametric Library

These files form the foundation for a parametric shape library. Future enhancements:

- **Parametric scripts** that generate families of shapes
- **Automated generation** from CSV/JSON dimension data
- **Test suites** for validating generated shapes
- **Documentation generation** from shape parameters
- **CI/CD integration** for automated testing

---

## File Renaming History

### Original Filenames

The files were initially created with short, cryptic names:

| Original | Type | Description |
|----------|------|-------------|
| `cube.scad` / `cube.FCStd` | Basic | Simple cube |
| `sp.scad` / `sp.FCStd` | Basic | Sphere |
| `sp n cyl.scad` / `sp n cyl1.FCStd` | Composite | Spheres and cylinder |
| `sp cyl cube.scad` / `cyl sp cube.FCStd` | Composite | Side-by-side shapes |
| `new shape.scad` / `new shape1.FCStd` | Composite | Vertical tower |

### Renaming Rationale

**Problems with original names**:
- ❌ Not descriptive (what is "sp"?)
- ❌ No dimension information
- ❌ Inconsistent naming between OpenSCAD/FreeCAD
- ❌ Spaces in filenames (problematic for scripts)
- ❌ Generic names like "new shape"
- ❌ Hard to search and filter

**Improvements in new names**:
- ✅ Descriptive shape names
- ✅ Embedded dimension information
- ✅ Consistent between platforms
- ✅ No spaces (underscore separators)
- ✅ Specific, meaningful names
- ✅ Searchable by dimension or shape type

### Renaming Summary

**Date**: 2025-11-11

**OpenSCAD Files** (`docs/modules/openscad/simple/`):

| Old Name | New Name |
|----------|----------|
| `cube.scad` | `cube_200mm.scad` |
| `sp.scad` | `sphere_r100mm.scad` |
| `sp n cyl.scad` | `dumbbell_spheres_60mm_cylinder_1000mm.scad` |
| `sp cyl cube.scad` | `side_by_side_sphere_50mm_cylinder_50x1000mm_cube_100mm.scad` |
| `new shape.scad` | `tower_cube_100mm_cylinder_50x1000mm_sphere_50mm.scad` |

**FreeCAD Files** (`docs/modules/freecad/simple/`):

| Old Name | New Name |
|----------|----------|
| `cube.FCStd` | `cube_200mm.FCStd` |
| `sp.FCStd` | `sphere_r100mm.FCStd` |
| `sp n cyl1.FCStd` | `dumbbell_spheres_60mm_cylinder_1000mm.FCStd` |
| `cyl sp cube.FCStd` | `side_by_side_sphere_50mm_cylinder_50x1000mm_cube_100mm.FCStd` |
| `new shape1.FCStd` | `tower_cube_100mm_cylinder_50x1000mm_sphere_50mm.FCStd` |

---

## Technical Details

### Units

All dimensions use **millimeters (mm)** as the base unit:
- OpenSCAD: Unitless numbers treated as mm (common practice)
- FreeCAD: Explicit mm unit system ("Standard (mm, kg, s, °)")

### Coordinate Systems

Both OpenSCAD and FreeCAD use right-handed coordinate systems:
- **X-axis**: Red (left-right)
- **Y-axis**: Green (front-back)
- **Z-axis**: Blue (up-down)
- **Origin**: (0, 0, 0)

### Boolean Operations

**OpenSCAD**:
- `union()` - Combine shapes (addition)
- `difference()` - Subtract shapes
- `intersection()` - Common volume

**FreeCAD**:
- `Part::MultiFuse` - Union/fusion of multiple shapes
- `Part::Cut` - Boolean subtraction
- `Part::Common` - Boolean intersection

---

## Future Enhancements

### Planned Additions

1. **More primitives**:
   - Cones
   - Torus
   - Pyramids
   - Geodesic spheres

2. **Parametric scripts**:
   - Family generators
   - Dimension tables
   - Automated variations

3. **Documentation**:
   - Visual guides
   - Tutorial videos
   - Interactive examples

4. **Testing**:
   - Automated validation
   - Dimension verification
   - Export format testing

### Contributing

To add new shapes to this collection:

1. Follow the naming convention
2. Include dimension information in filename
3. Create both OpenSCAD and FreeCAD versions
4. Document in this file
5. Add example images (optional)
6. Test exports (STL, STEP, etc.)

---

## Related Documentation

- **OpenSCAD Manual**: https://openscad.org/documentation.html
- **FreeCAD Documentation**: https://wiki.freecad.org/
- **Parametric Design Principles**: See `docs/PARAMETRIC_DESIGN.md` (if available)
- **CAD Best Practices**: See workspace-hub standards

---

## Quick Reference

### Shape Dimensions Cheat Sheet

| Shape | Key Dimensions | Files |
|-------|----------------|-------|
| Cube | 200×200×200mm | `cube_200mm.*` |
| Sphere | r=100mm (d=200mm) | `sphere_r100mm.*` |
| Dumbbell | 2×60mm spheres, 60×1000mm cylinder | `dumbbell_spheres_60mm_cylinder_1000mm.*` |
| Side-by-side | 50mm sphere, 50×1000mm cyl, 100mm cube | `side_by_side_sphere_50mm_cylinder_50x1000mm_cube_100mm.*` |
| Tower | 100mm cube, 50×1000mm cyl, 50mm sphere | `tower_cube_100mm_cylinder_50x1000mm_sphere_50mm.*` |

### File Locations

```
docs/modules/
├── openscad/
│   └── simple/
│       ├── cube_200mm.scad
│       ├── sphere_r100mm.scad
│       ├── dumbbell_spheres_60mm_cylinder_1000mm.scad
│       ├── side_by_side_sphere_50mm_cylinder_50x1000mm_cube_100mm.scad
│       └── tower_cube_100mm_cylinder_50x1000mm_sphere_50mm.scad
└── freecad/
    └── simple/
        ├── cube_200mm.FCStd
        ├── sphere_r100mm.FCStd
        ├── dumbbell_spheres_60mm_cylinder_1000mm.FCStd
        ├── side_by_side_sphere_50mm_cylinder_50x1000mm_cube_100mm.FCStd
        └── tower_cube_100mm_cylinder_50x1000mm_sphere_50mm.FCStd
```

---

**Last Updated**: 2025-11-11
**Maintained By**: Digital Model Project Team
**Version**: 1.0
