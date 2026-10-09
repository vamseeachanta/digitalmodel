"""Independent complete-domain checks for the frozen plate and circular hole."""
import math

from tests.ansys.padeye_pressure_study import _configuration, _nodes, _require, _topology

EPS = 1e-7  # CDB coordinate serialization envelope, mm.


def _outer_edge(a, b):
    return (any(abs(a[0]-x) <= EPS and abs(b[0]-x) <= EPS for x in (0, 400)) or
            any(abs(a[1]-y) <= EPS and abs(b[1]-y) <= EPS for y in (0, 300)))


def _boundaries(nodes, faces, incidence):
    outer, hole = [], []
    for owners in incidence.values():
        if len(owners) != 1:
            continue
        a, b = (nodes[node] for node in faces[owners[0]])
        if _outer_edge(a, b):
            outer.append((a, b))
        elif all(abs(math.dist(p, (200, 220))-40) <= EPS for p in (a, b)):
            hole.append((a, b))
        else:
            raise ValueError('boundary differs from frozen plate/hole')
    return outer, hole


def verify_frozen_pressure_domain(mesh):
    """Check full rectangle/hole coverage separately from traction integration."""
    try:
        count = _configuration(mesh)
        nodes = _nodes(mesh['nodes'])
        faces, incidence = _topology(mesh['elements'], nodes)
        for x, y in nodes.values():
            _require(-EPS <= x <= 400+EPS and -EPS <= y <= 300+EPS,
                     'node lies outside frozen plate')
            _require(math.hypot(x-200, y-220) >= 40-EPS, 'node lies inside hole')
        outer, hole = _boundaries(nodes, faces, incidence)
        perimeter = math.fsum(math.dist(a, b) for a, b in outer)
        _require(abs(perimeter-1400) <= EPS*len(outer), 'incomplete outer rectangle')
        _require(len(hole) == 2*count, 'incomplete circular hole')
        increments = [math.atan2((a[0]-200)*(b[1]-220)-(a[1]-220)*(b[0]-200),
                                (a[0]-200)*(b[0]-200)+(a[1]-220)*(b[1]-220))
                      for a, b in hole]
        _require(all(abs(angle-math.pi/count) <= EPS/40 for angle in increments),
                 'hole differs from fixed angular refinement')
        area = 0.0
        for element in mesh['elements']:
            xy = [nodes[n] for n in element['nodes']]
            area += sum(a[0]*b[1]-b[0]*a[1] for a, b in zip(xy, xy[1:]+xy[:1]))/2
        target = 120000-count*40**2*math.sin(math.pi/count)
        _require(abs(area-target) <= 1e-4, 'mesh does not cover frozen domain')
        return {'frozen_domain_verified': True, 'outer_perimeter_mm': perimeter,
                'area_mm2': area, 'hole_edges': len(hole)}
    except (KeyError, TypeError, OverflowError) as error:
        raise ValueError('malformed frozen-domain evidence') from error
