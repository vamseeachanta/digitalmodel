# OpenSCAD Simple Shapes

> Basic 3D primitives and composite shapes for OpenSCAD
>
> Location: `docs/modules/openscad/simple/`
> Last Updated: 2025-11-11

## Overview

This directory contains OpenSCAD files demonstrating basic 3D geometric primitives and simple compositions. All files are code-based, parametric, and use millimeters as the unit of measurement.

## Files in This Directory

### Basic Primitives

1. **`cube_200mm.scad`** - 200×200×200mm cube
2. **`sphere_r100mm.scad`** - Sphere with 100mm radius

### Composite Shapes

3. **`dumbbell_spheres_60mm_cylinder_1000mm.scad`** - Two spheres connected by cylinder
4. **`side_by_side_sphere_50mm_cylinder_50x1000mm_cube_100mm.scad`** - Three shapes displayed horizontally
5. **`tower_cube_100mm_cylinder_50x1000mm_sphere_50mm.scad`** - Vertical tower stack

---

## File Details

### 1. cube_200mm.scad

**Basic cube demonstrating box primitive and centering options.**

```openscad
// 20 cm cube (OpenSCAD uses unitless numbers; common practice is to treat them as mm)
// Option A: cube with one corner at the origin
cube([200, 200, 200]);

// Option B: centered cube (centered at the origin)
// cube(200, center=true);
```

**Key Concepts**:
- Box/cube primitive
- Placement at origin vs centered
- Vector notation `[x, y, z]` vs scalar with `center=true`

**Parametric Version**:
```openscad
// Parameters
cube_size = 200;  // mm

// Geometry
cube([cube_size, cube_size, cube_size]);
// Or: cube(cube_size, center=true);
```

**Variations to Try**:
- Change `cube_size` to create different sizes
- Toggle `center=true` to see placement difference
- Use non-uniform dimensions: `cube([100, 200, 300])`

---

### 2. sphere_r100mm.scad

**Basic sphere demonstrating spherical primitive and facet control.**

```openscad
// Sphere of radius 10 cm (100 mm)
// Option A: if you treat units as millimetres:
sphere(r = 100, $fn = 128); // r = 100 mm -> 10 cm

// Option B: if you prefer to think in centimetres (enable this and comment out A):
// scale([10,10,10]) sphere(r = 1, $fn = 128); // r = 1 cm scaled to 10 cm

// Notes:
// - $fn controls facet count (higher = smoother, but heavier to render).
// - sphere() is centered at the origin by default.
```

**Key Concepts**:
- Sphere primitive
- Radius specification
- `$fn` parameter for smoothness/facet count
- Scale transformation (alternative approach)
- Default centering at origin

**Parametric Version**:
```openscad
// Parameters
sphere_radius = 100;  // mm
sphere_resolution = 128;  // facets ($fn)

// Geometry
sphere(r = sphere_radius, $fn = sphere_resolution);
```

**Facet Resolution Guide**:
- `$fn = 32` - Low resolution (fast render, visible facets)
- `$fn = 64` - Medium resolution (good for preview)
- `$fn = 128` - High resolution (smooth, recommended for final)
- `$fn = 256` - Very high resolution (very smooth, slow render)

**Variations to Try**:
- Adjust `$fn` to see smoothness changes
- Try different radii
- Use `$fa` and `$fs` for automatic facet control

---

### 3. dumbbell_spheres_60mm_cylinder_1000mm.scad

**Composite shape: two spheres connected by a cylinder (dumbbell/barbell).**

```openscad
// Units: millimeters (1 cm = 10 mm)
sphere_r = 60;      // 6 cm
cyl_dia = 60;       // 6 cm
cyl_len = 1000;     // 100 cm (1 meter)
gap = 150;          // 15 cm between spheres (center-to-center gap)

// Combine shapes
union() {
    // Cylinder in the middle
    cylinder(d = cyl_dia, h = cyl_len, center = true);

    // Left sphere
    translate([0, 0, -(cyl_len / 2 + gap / 2)])
        sphere(r = sphere_r);

    // Right sphere
    translate([0, 0, (cyl_len / 2 + gap / 2)])
        sphere(r = sphere_r);
}
```

**Key Concepts**:
- `union()` - Boolean operation to combine shapes
- `translate([x, y, z])` - Move objects in 3D space
- `cylinder(d=diameter, h=height, center=true)` - Cylinder primitive
- Calculated positioning (using expressions)

**Structure Explanation**:
```
       ⚫ sphere (top)
       │  at z = (cyl_len/2 + gap/2) = 575mm
       │
       │
    ║      ║
    ║  cyl ║  1000mm long, centered at z=0
    ║      ║  (from z=-500 to z=+500)
       │
       │
       ⚫ sphere (bottom)
          at z = -(cyl_len/2 + gap/2) = -575mm
```

**Parametric Version**:
```openscad
// Parameters
sphere_radius = 60;        // mm
cylinder_diameter = 60;    // mm
cylinder_length = 1000;    // mm
sphere_gap = 150;          // mm (center-to-center)

// Calculated positions
sphere_offset = cylinder_length / 2 + sphere_gap / 2;

// Geometry
union() {
    cylinder(d = cylinder_diameter, h = cylinder_length, center = true);
    translate([0, 0, -sphere_offset]) sphere(r = sphere_radius);
    translate([0, 0, +sphere_offset]) sphere(r = sphere_radius);
}
```

**Variations to Try**:
- Change gap between spheres
- Use different sphere sizes
- Adjust cylinder diameter
- Add more spheres along the length

---

### 4. side_by_side_sphere_50mm_cylinder_50x1000mm_cube_100mm.scad

**Three basic primitives displayed side-by-side for comparison.**

```openscad
// Units: millimeters (OpenSCAD defaults to mm)

sphere_r = 50;      // 5 cm
cyl_dia  = 50;      // 5 cm
cyl_h    = 1000;    // 100 cm
cube_side = 100;    // 10 cm

// Arrange objects side by side for clarity
translate([-200, 0, 0])  // move left
    sphere(r = sphere_r);

translate([0, 0, 0])     // center
    cylinder(d = cyl_dia, h = cyl_h, center = true);

translate([200, 0, 0])   // move right
    cube([cube_side, cube_side, cube_side], center = true);
```

**Key Concepts**:
- Multiple independent objects (no union)
- Horizontal spacing with `translate()`
- All objects centered for visual consistency
- Simple comparison layout

**Layout**:
```
    ⚫              ║      ║            ▢
  sphere        cylinder             cube
  r=50mm       d=50mm,h=1000        100mm
  x=-200          x=0               x=+200
```

**Parametric Version**:
```openscad
// Parameters
sphere_radius = 50;
cylinder_diameter = 50;
cylinder_height = 1000;
cube_size = 100;
spacing = 200;  // Distance between shape centers

// Geometry
translate([-spacing, 0, 0])
    sphere(r = sphere_radius);

translate([0, 0, 0])
    cylinder(d = cylinder_diameter, h = cylinder_height, center = true);

translate([spacing, 0, 0])
    cube([cube_size, cube_size, cube_size], center = true);
```

**Variations to Try**:
- Adjust spacing between objects
- Try different sizes for each shape
- Add more shapes to the comparison
- Arrange in a circle instead of line

---

### 5. tower_cube_100mm_cylinder_50x1000mm_sphere_50mm.scad

**Vertical stack: cube base, cylinder shaft, sphere top (tower structure).**

```openscad
// Units in millimeters (1 cm = 10 mm)
sphere_r = 50;     // 5 cm radius
cyl_dia  = 50;     // 5 cm diameter
cyl_h    = 1000;   // 100 cm height
cube_side = 100;   // 10 cm cube

union() {
    // Cube base (centered at origin bottom)
    translate([-cube_side/2, -cube_side/2, 0])
        cube([cube_side, cube_side, cube_side]);

    // Cylinder standing on top of cube
    translate([0, 0, cube_side])
        cylinder(d = cyl_dia, h = cyl_h);

    // Sphere sitting on top of cylinder
    translate([0, 0, cube_side + cyl_h])
        sphere(r = sphere_r);
}
```

**Key Concepts**:
- Vertical stacking with cumulative `translate()`
- `union()` to combine all parts
- Calculated positioning (z = previous height + current offset)
- Building bottom-up

**Structure Explanation**:
```
Height (z-axis)
       1150mm → ⚫ sphere (r=50mm, center at z=1100mm)
                │
       1100mm → ┴ top of cylinder
                ║
                ║ cylinder (d=50mm, h=1000mm)
                ║ from z=100 to z=1100
                ║
        100mm → ┴ top of cube
                ▢
                ▢ cube (100mm, from z=0 to z=100)
                ▢
          0mm → ┴ ground level
```

**Parametric Version**:
```openscad
// Parameters
cube_size = 100;
cylinder_diameter = 50;
cylinder_height = 1000;
sphere_radius = 50;

// Calculated positions
cube_top = cube_size;
cylinder_top = cube_top + cylinder_height;

// Geometry
union() {
    // Base cube (corner at origin)
    translate([-cube_size/2, -cube_size/2, 0])
        cube([cube_size, cube_size, cube_size]);

    // Cylinder on cube
    translate([0, 0, cube_top])
        cylinder(d = cylinder_diameter, h = cylinder_height);

    // Sphere on cylinder
    translate([0, 0, cylinder_top])
        sphere(r = sphere_radius);
}
```

**Variations to Try**:
- Add more levels (another cylinder and sphere)
- Taper the tower (decrease diameters as you go up)
- Use different ratios (thicker base, thinner top)
- Rotate elements (use `rotate()`)

---

## OpenSCAD Best Practices

### 1. Use Variables for Dimensions

**Bad**:
```openscad
cube([200, 200, 200]);
translate([0, 0, 200])
    cylinder(d = 50, h = 1000);
```

**Good**:
```openscad
cube_size = 200;
cyl_dia = 50;
cyl_height = 1000;

cube([cube_size, cube_size, cube_size]);
translate([0, 0, cube_size])
    cylinder(d = cyl_dia, h = cyl_height);
```

### 2. Add Comments

```openscad
// Parameters
cube_size = 200;  // mm - base platform size

// Geometry
cube([cube_size, cube_size, cube_size]);  // Base platform
```

### 3. Use Meaningful Variable Names

**Bad**: `a = 50; b = 100; c = 200;`

**Good**: `radius = 50; height = 100; width = 200;`

### 4. Group Related Operations

```openscad
// Create tower
union() {
    base();
    shaft();
    top();
}
```

### 5. Use Modules for Reusability

```openscad
module tower(base_size, shaft_dia, shaft_h, top_r) {
    union() {
        cube([base_size, base_size, base_size]);
        translate([0, 0, base_size])
            cylinder(d = shaft_dia, h = shaft_h);
        translate([0, 0, base_size + shaft_h])
            sphere(r = top_r);
    }
}

// Use the module
tower(100, 50, 1000, 50);
```

---

## Rendering Tips

### Preview vs Render

- **Preview (F5)**: Fast, lower quality for design work
  - Uses `$fn`, `$fa`, `$fs` defaults
  - Good for iterative design

- **Render (F6)**: Slow, high quality for final export
  - Uses specified `$fn` values
  - Required before STL export

### Performance Optimization

```openscad
// Use lower resolution during preview
$fn = $preview ? 32 : 128;

sphere(r = 100);  // Uses 32 facets in preview, 128 in render
```

### Export Settings

For STL export (File → Export → Export as STL):
- Ensure you've done a full render (F6) first
- Use `$fn = 128` or higher for smooth curves
- Check "Use ASCII format" for debugging (optional)

---

## Common OpenSCAD Operations

### Transformations

```openscad
translate([x, y, z]) object;  // Move
rotate([x, y, z]) object;     // Rotate (degrees)
scale([x, y, z]) object;      // Scale
mirror([x, y, z]) object;     // Mirror
```

### Boolean Operations

```openscad
union() { obj1; obj2; }        // Combine (addition)
difference() { obj1; obj2; }   // Subtract obj2 from obj1
intersection() { obj1; obj2; } // Common volume
```

### Primitives

```openscad
cube([x, y, z]);                    // Box
sphere(r = radius);                 // Sphere
cylinder(h = height, r = radius);   // Cylinder
cylinder(h = height, d = diameter); // Cylinder (diameter)
```

---

## Learning Resources

### Official Documentation
- OpenSCAD Manual: https://openscad.org/documentation.html
- Cheat Sheet: https://openscad.org/cheatsheet/
- Language Reference: https://en.wikibooks.org/wiki/OpenSCAD_User_Manual

### Recommended Learning Path

1. **Start with**: `cube_200mm.scad` and `sphere_r100mm.scad`
   - Understand basic primitives
   - Practice modifying dimensions

2. **Progress to**: `dumbbell_spheres_60mm_cylinder_1000mm.scad`
   - Learn `union()` and `translate()`
   - Practice positioning

3. **Advanced**: `tower_cube_100mm_cylinder_50x1000mm_sphere_50mm.scad`
   - Cumulative positioning
   - Complex assemblies

4. **Comparison**: `side_by_side_sphere_50mm_cylinder_50x1000mm_cube_100mm.scad`
   - Multiple objects
   - Visual layout

### Practice Exercises

1. **Modify existing shapes**: Change dimensions in all files
2. **Create variations**: Save modified versions with new filenames
3. **Combine elements**: Mix shapes from different files
4. **Add complexity**: Insert additional primitives
5. **Create modules**: Convert shapes to reusable modules

---

## Troubleshooting

### Common Issues

**Issue**: "Object is not a valid 2-manifold"
- **Cause**: Overlapping or touching faces
- **Solution**: Ensure small gaps or overlaps between parts

**Issue**: Slow rendering
- **Cause**: High `$fn` values
- **Solution**: Use lower `$fn` for preview, high for final render

**Issue**: Objects not appearing
- **Cause**: Objects positioned outside view
- **Solution**: Use "View All" or check translate coordinates

**Issue**: Unexpected boolean results
- **Cause**: Order matters in `difference()`
- **Solution**: First object is kept, subsequent objects are subtracted

---

## File Naming Convention

When creating new shapes, follow this pattern:

```
<description>_<dimensions>.scad
```

**Examples**:
- `cube_150mm.scad` - 150mm cube
- `sphere_r75mm.scad` - Sphere with 75mm radius
- `cylinder_40x500mm.scad` - 40mm diameter, 500mm height
- `tower_cube_80mm_cylinder_40x800mm_sphere_40mm.scad` - Composite shape

---

## Related Documentation

- **Main Overview**: `docs/modules/SIMPLE_SHAPES_OVERVIEW.md`
- **FreeCAD Shapes**: `docs/modules/freecad/simple/README.md`
- **Parametric Design Guide**: See main overview document

---

## Contributing

To add new shapes:
1. Follow naming convention
2. Use variables for all dimensions
3. Add comments explaining the shape
4. Test with different parameter values
5. Export STL to verify geometry
6. Update this README

---

**Last Updated**: 2025-11-11
**OpenSCAD Version**: Compatible with OpenSCAD 2021.01 and later
