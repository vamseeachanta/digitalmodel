// Sphere of radius 10 cm (100 mm)
// Option A: if you treat units as millimetres:
sphere(r = 100, $fn = 128); // r = 100 mm -> 10 cm

// Option B: if you prefer to think in centimetres (enable this and comment out A):
// scale([10,10,10]) sphere(r = 1, $fn = 128); // r = 1 cm scaled to 10 cm

// Notes:
// - $fn controls facet count (higher = smoother, but heavier to render).
// - sphere() is centered at the origin by default.
