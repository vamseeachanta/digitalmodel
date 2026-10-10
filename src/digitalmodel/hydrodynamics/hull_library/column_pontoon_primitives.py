"""Target-sized convex columns, tanks, keel plates and pontoon sweeps."""
from math import ceil, pi
from typing import Any, TYPE_CHECKING, cast
import numpy as np
from numpy.typing import NDArray
from scipy.spatial import ConvexHull
if TYPE_CHECKING:
    from .column_pontoon_form import ColumnPontoonParameters

_EPS = 1e-8
Array = NDArray[np.float64]
Solid = dict[str, Any]

def _profile(width: float, height: float, radius: float, target: float) -> Array:
    """CCW section with target-derived circular facets and preserved circle area."""
    if width == height and radius == width / 2:
        # Twelve-fold sectors align the common 3/4/6 connector directions
        # without near-tangent facet wedges at their intersections.
        n = max(12, 12 * ceil(pi * width / (12 * target)))
        angles = np.arange(n) * 2 * pi / n
        scaled = radius * np.sqrt(2 * pi / (n * np.sin(2 * pi / n)))
        return cast(Array, scaled * np.column_stack((np.cos(angles), np.sin(angles))))
    points: list[Array] = []
    for sx, sy, start in ((1, 1, 0), (-1, 1, pi/2), (-1, -1, pi), (1, -1, 3*pi/2)):
        centre = np.array([sx * (width/2 - radius), sy * (height/2 - radius)])
        # Ten chords per quarter bound area loss by 0.411%, below 0.5%.
        minimum = 10
        angles = np.linspace(start, start + pi/2, max(minimum, ceil(pi*radius/(2*target))) + 1)
        arc = centre + radius * np.column_stack((np.cos(angles), np.sin(angles)))
        points.extend(arc if radius else [centre])
    cleaned: list[Array] = []
    for a, b in zip(points, points[1:] + points[:1]):
        if np.linalg.norm(b - a) > _EPS:
            cleaned.extend(a + t * (b-a) for t in np.arange(ceil(np.linalg.norm(b-a)/target))
                           / ceil(np.linalg.norm(b-a)/target))
    return np.array(cleaned)


def _grid_profile(profile: Array) -> Array:
    """Share the cap grid's extra boundary knots with every side ring."""
    # Four chains have equal index counts, not necessarily equal arc lengths.
    # The same padded profile supplies caps and every side ring.
    while len(profile) % 4:
        lengths = np.linalg.norm(profile - np.roll(profile, -1, axis=0), axis=1)
        i = int(np.argmax(lengths))
        profile = np.insert(profile, i + 1, (profile[i] + profile[(i+1)%len(profile)]) / 2, axis=0)
    return profile


def _cap_faces(profile: Array, origin: Array, e1: Array, e2: Array,
               reverse: bool = False) -> list[Array]:
    """Coons grid spans four boundary chains without a radial centre fan."""
    profile = _grid_profile(profile)
    m = len(profile) // 4
    loop = np.vstack((profile, profile[0]))
    bottom, right = loop[:m+1], loop[m:2*m+1]
    top, left = loop[2*m:3*m+1][::-1], loop[3*m:][::-1]
    grid = np.empty((m+1, m+1, 3))
    for j, v in enumerate(np.linspace(0, 1, m+1)):
        for i, u in enumerate(np.linspace(0, 1, m+1)):
            xy = ((1-v)*bottom[i] + v*top[i] + (1-u)*left[j] + u*right[j]
                  - ((1-u)*(1-v)*bottom[0] + u*(1-v)*bottom[-1]
                     + (1-u)*v*top[0] + u*v*top[-1]))
            grid[j, i] = origin + xy[0]*e1 + xy[1]*e2
    faces = []
    for j in range(m):
        for i in range(m):
            face = grid[[j, j, j+1, j+1], [i, i+1, i+1, i]]
            faces.append(face[::-1] if reverse else face)
    return faces


def _solid(profile: Array, origin: Array, e1: Array, e2: Array, axis: Array,
           length: float, target: float, area: float, name: str, inertia: float | None = None) -> Solid:
    profile = _grid_profile(profile)
    edge = np.linalg.norm(profile - np.roll(profile, -1, axis=0), axis=1).min()
    levels = np.linspace(0, length, ceil(length / min(target, 10 * edge)) + 1)
    rings = [origin + profile[:, :1]*e1 + profile[:, 1:]*e2 + z*axis for z in levels]
    faces = _cap_faces(profile, origin, e1, e2, reverse=True)
    faces += _cap_faces(profile, origin + length * axis, e1, e2)
    for lower, upper in zip(rings, rings[1:]):
        faces.extend(np.array([lower[i], lower[(i+1)%len(lower)],
                               upper[(i+1)%len(lower)], upper[i]]) for i in range(len(lower)))
    vertices = np.vstack(rings)
    planes = np.unique(np.round(ConvexHull(vertices).equations, 12), axis=0)
    waterplane = area if axis[2] == 1 and abs(vertices.max(0)[2]) < _EPS else 0.
    if inertia is None:
        inertia = 0.
    return dict(faces=faces, planes=planes, bounds=(vertices.min(0), vertices.max(0)),
                analytic_volume=area*length, analytic_waterplane_area=waterplane,
                centroid_z=origin[2]+axis[2]*length/2,
                analytic_bm=(inertia/(area*length),)*2 if waterplane else (0., 0.), name=name)


def _section_inertia(width: float, radius: float) -> float:
    """Centroidal second moment for an exact rounded square section."""
    c = width / 2 - radius
    return width**4 / 12 - 4 * (
        c*c*radius**2*(1-pi/4) + c*radius**3/3 + radius**4*(1/3-pi/16)
    )


def _primitives(p: "ColumnPontoonParameters") -> tuple[list[Solid], float]:
    target, solids = p.panel_target_size, []
    angles = np.arange(p.count)*2*pi/p.count + (pi/4 if p.count == 4 else pi/3)
    centres = np.column_stack((p.radius*np.cos(angles), p.radius*np.sin(angles), np.zeros(p.count)))
    for i, centre in enumerate(centres):
        plate_height, tank_depth = p.heave_plate_thickness, p.resolved_tank_depth
        width = p.diameter or p.square_side or 0
        section = _profile(width, width, p.diameter/2 if p.diameter else p.corner_radius, target)
        area = pi*(p.diameter/2)**2 if p.diameter else width**2 - (4-pi)*p.corner_radius**2
        solids.append(_solid(section, centre + [0, 0, -p.draft+plate_height], np.eye(3)[0],
                             np.eye(3)[1], np.eye(3)[2], p.draft-plate_height-tank_depth,
                             target, area, f"column_{i}",
                             inertia=_section_inertia(width, p.diameter/2 if p.diameter else p.corner_radius)))
        for diameter, height, bottom, name in (
            (p.heave_plate_diameter, plate_height, -p.draft, "heave_plate"),
            (p.hard_tank_diameter, tank_depth, -tank_depth, "hard_tank"),
        ):
            if diameter:
                solids.append(_solid(_profile(diameter, diameter, diameter/2, target),
                                     centre+[0, 0, bottom], np.eye(3)[0], np.eye(3)[1],
                                     np.eye(3)[2], height, target, pi*(diameter/2)**2,
                                     f"{name}_{i}", inertia=pi*diameter**4/64))
    if p.center_diameter:
        d = p.center_diameter
        solids.append(_solid(_profile(d, d, d/2, target), np.array([0, 0, -p.draft]),
                             np.eye(3)[0], np.eye(3)[1], np.eye(3)[2], p.draft,
                             target, pi*(d/2)**2, "centre_column", inertia=pi*d**4/64))
    links = []
    if p.pontoon_layout == "ring":
        links = [(centres[i], centres[(i+1)%p.count]) for i in range(p.count)]
        if p.center_diameter:
            links += [(np.zeros(3), c) for c in centres]
    elif p.pontoon_layout == "twin":
        links = [(centres[0], centres[1]), (centres[2], centres[3])]
    section_area = p.pontoon_width*p.pontoon_height - (4-pi)*p.pontoon_corner_radius**2
    for i, (a, b) in enumerate(links):
        length = float(np.linalg.norm(b-a))
        axis = (b-a)/length
        z = p.pontoon_center_z if p.pontoon_center_z is not None else -p.draft+p.pontoon_height/2
        solids.append(_solid(_profile(p.pontoon_width, p.pontoon_height, p.pontoon_corner_radius, target),
                             a+[0, 0, z], np.array([-axis[1], axis[0], 0]), np.eye(3)[2],
                             axis, length, target, section_area, f"pontoon_{i}"))
    return solids, section_area
