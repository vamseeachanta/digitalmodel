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
