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
