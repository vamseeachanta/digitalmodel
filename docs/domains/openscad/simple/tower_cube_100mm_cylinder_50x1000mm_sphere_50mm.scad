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
