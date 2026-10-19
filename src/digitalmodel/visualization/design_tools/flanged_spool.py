"""ABOUTME: Parameter-driven FreeCAD solid and geometry-only spool metrology.

The metrology takes only the solid: it does not read the input parameters or
feature properties. Analytic cylinder/cone faces locate each physical region.
"""

from math import atan2, cos, hypot, pi, sin

from digitalmodel.solvers.gmsh_meshing.spool_pilot import (
    SolidMeasurements,
    SpoolParameters,
)


CUTTER_OVERLAP_MM = 1.0


def build_spool(p: SpoolParameters):
    """Revolve the weld-neck section and subtract the flange bolt holes."""
    import FreeCAD as App
    import Part

    a, r = p.bore_diameter / 2, p.pipe_outer_diameter / 2
    b, f = p.neck_base_diameter / 2, p.flange_outer_diameter / 2
    z1 = p.left_flange_thickness
    z2 = z1 + p.neck_length
    z3 = z2 + p.pipe_length
    z4 = z3 + p.neck_length
    z5 = z4 + p.right_flange_thickness
    section = [
        (a, 0),
        (f, 0),
        (f, z1),
        (b, z1),
        (r, z2),
        (r, z3),
        (b, z4),
        (f, z4),
        (f, z5),
        (a, z5),
        (a, 0),
    ]
    wire = Part.makePolygon([App.Vector(x, 0, z) for x, z in section])
    shape = Part.Face(wire).revolve(App.Vector(), App.Vector(0, 0, 1), 360)
    for z, thickness in ((0, p.left_flange_thickness), (z4, p.right_flange_thickness)):
        for i in range(p.bolt_hole_count):
            angle = 2 * pi * i / p.bolt_hole_count
            center = App.Vector(
                p.bolt_circle_diameter / 2 * cos(angle),
                p.bolt_circle_diameter / 2 * sin(angle),
                z - CUTTER_OVERLAP_MM,
            )
            hole = Part.makeCylinder(
                p.bolt_hole_diameter / 2, thickness + 2 * CUTTER_OVERLAP_MM, center
            )
            shape = shape.cut(hole)
    return shape.removeSplitter()


def measure_spool(shape) -> SolidMeasurements:
    """Read diameters and axial extents from B-rep surfaces, not input values.

    Unexpected topology raises instead of guessing. This pilot supports its
    axial, unfilleted, equally bored spool family only.
    """
    import Part

    coaxial, holes, cones = [], [], []
    for face in shape.Faces:
        surface = face.Surface
        if isinstance(surface, (Part.Cylinder, Part.Cone)):
            if abs(abs(surface.Axis.z) - 1) > 1e-8:
                raise ValueError("non-axial cylindrical or conical surface")
        if isinstance(surface, Part.Cylinder):
            entry = (
                surface.Radius,
                face.BoundBox.ZMin,
                face.BoundBox.ZMax,
                surface.Center.x,
                surface.Center.y,
            )
            (coaxial if hypot(*entry[3:]) < 1e-7 else holes).append(entry)
        elif isinstance(surface, Part.Cone):
            circles = [
                edge.Curve for edge in face.Edges if isinstance(edge.Curve, Part.Circle)
            ]
            if len(circles) != 2:
                raise ValueError("neck must have two circular boundaries")
            cones.append(
                (
                    face.BoundBox.ZMin,
                    face.BoundBox.ZMax,
                    2 * max(circle.Radius for circle in circles),
                )
            )
    if len(coaxial) != 4 or len(cones) != 2:
        raise ValueError("expected bore, pipe, two flange cylinders and two neck cones")
    coaxial.sort()  # smallest coaxial radius is bore; next is straight pipe
    bore, pipe, *flanges = coaxial
    flanges.sort(key=lambda item: item[1])
    cones.sort()
    dims = {
        "bore_diameter": 2 * bore[0],
        "pipe_outer_diameter": 2 * pipe[0],
        "pipe_length": pipe[2] - pipe[1],
        "total_length": shape.BoundBox.ZLength,
    }
    remaining_holes = list(holes)
    for end, flange, cone in zip(("left", "right"), flanges, cones):
        group = [
            hole
            for hole in remaining_holes
            if abs(hole[1] - flange[1]) < 1e-7 and abs(hole[2] - flange[2]) < 1e-7
        ]
        for hole in group:
            remaining_holes.remove(hole)
        group.sort(key=lambda hole: atan2(hole[4], hole[3]))
        dims.update(
            {
                f"{end}.flange_thickness": flange[2] - flange[1],
                f"{end}.flange_outer_diameter": 2 * flange[0],
                f"{end}.neck_length": cone[1] - cone[0],
                f"{end}.neck_base_diameter": cone[2],
                f"{end}.hole_count": len(group),
            }
        )
        for i, hole in enumerate(group):
            dims[f"{end}.hole_{i}.diameter"] = 2 * hole[0]
            dims[f"{end}.hole_{i}.bolt_circle"] = 2 * hypot(hole[3], hole[4])
            following = group[(i + 1) % len(group)]
            dims[f"{end}.hole_{i}.next_hole_spacing"] = hypot(
                following[3] - hole[3], following[4] - hole[4]
            )
    if remaining_holes:
        raise ValueError("unclassified cylindrical holes")
    return SolidMeasurements(
        shape.Volume, dims, shape.isValid() and shape.isClosed(), len(shape.Solids)
    )
