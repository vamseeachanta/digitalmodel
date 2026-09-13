"""Fixed eight-block mapped mesh for the approved preparation-only example.

This geometry/load intent is not native evidence or a qualified contact model.
"""
import math
from dataclasses import asdict

from digitalmodel.ansys.padeye import PadeyeGeometry

MESH_LEVELS = {10.0: (16, 26), 5.0: (32, 52), 2.5: (64, 104)}


def pressure_geometry(mesh_size: float) -> PadeyeGeometry:
    """Return the frozen 50 kN / 8 mm example at a prescribed mesh level."""
    if isinstance(mesh_size, bool) or mesh_size not in MESH_LEVELS:
        raise ValueError('mesh size must be 10, 5 or 2.5 mm')
    return PadeyeGeometry(thickness_mm=8.0, sling_load_kn=50.0,
                          element_size_mm=float(mesh_size))


def _validate_geometry(geometry):
    expected = asdict(pressure_geometry(geometry.element_size_mm))
    actual = asdict(geometry)
    for key, value in actual.items():
        if isinstance(value, bool) or value != expected[key]:
            raise ValueError(f'frozen pressure-study input differs: {key}')


def _angles(upper_edges):
    """Eight sectors end on cardinal rays and the four rectangle corners."""
    top, bottom = math.atan2(80, 200), math.atan2(220, 200)
    ends = [0, top, math.pi/2, math.pi-top, math.pi,
            math.pi+bottom, 1.5*math.pi, 2*math.pi-bottom, 2*math.pi]
    count = upper_edges // 4
    return [a+(b-a)*i/count for a, b in zip(ends, ends[1:])
            for i in range(count)]


def _outer_radius(angle):
    c, s = math.cos(angle), math.sin(angle)
    distances = []
    if abs(c) > 1e-12:
        distances.append(200/abs(c))
    if abs(s) > 1e-12:
        distances.append((80 if s > 0 else 220)/abs(s))
    return min(distances)


def _nodes(angles, layers):
    nodes = []
    for layer in range(layers + 1):
        for angle in angles:
            radius = 40 + (_outer_radius(angle)-40)*layer/layers
            x, y = 200+radius*math.cos(angle), 220+radius*math.sin(angle)
            nodes.append({'id': len(nodes)+1, 'x_mm': round(x, 12),
                          'y_mm': round(y, 12)})
    return nodes


def _elements(width, layers):
    elements = []
    for layer in range(layers):
        for i in range(width):
            j = (i+1) % width
            nodes = [layer*width+i+1, (layer+1)*width+i+1,
                     (layer+1)*width+j+1, layer*width+j+1]
            elements.append({'id': len(elements)+1, 'nodes': nodes})
    return elements


def _pressure_intent(nodes, upper_edges):
    p0 = 2*50000/(math.pi*40*8)
    unscaled_fx = unscaled_fy = 0.0
    rows = []
    for i in range(upper_edges):
        a, b = nodes[i], nodes[i+1]
        p1, p2 = [p0*(n['y_mm']-220)/40 for n in (a, b)]
        unscaled_fx += 8*(p1+p2)/2*(b['y_mm']-a['y_mm'])
        unscaled_fy -= 8*(p1+p2)/2*(b['x_mm']-a['x_mm'])
        rows.append({'element': i+1, 'face': 4, 'p1_mpa': p1, 'p2_mpa': p2})
    if not math.isfinite(unscaled_fy) or unscaled_fy <= 0:
        raise ValueError('nonpositive unscaled vertical force')
    alpha = 50000/unscaled_fy
    for row in rows:
        row['p1_mpa'] *= alpha
        row['p2_mpa'] *= alpha
    return rows, alpha, p0*alpha/40, [unscaled_fx, unscaled_fy]


def build_pressure_mesh(geometry):
    """Build intent with doubled angular/radial divisions; no FE solution."""
    _validate_geometry(geometry)
    upper_edges, layers = MESH_LEVELS[geometry.element_size_mm]
    angles = _angles(upper_edges)
    nodes = _nodes(angles, layers)
    pressures, alpha, slope, unscaled = _pressure_intent(nodes, upper_edges)
    return {'schema': 'padeye-pressure-preparation-v1',
            'geometry': asdict(geometry), 'nodes': nodes,
            'elements': _elements(len(angles), layers), 'pressures': pressures,
            'center_mm': [200.0, 220.0], 'radius_mm': 40.0,
            'thickness_mm': 8.0, 'upper_edges': upper_edges,
            'radial_layers': layers, 'alpha': alpha,
            'pressure_slope_mpa_per_mm': slope,
            'unscaled_force_n': unscaled,
            'native_binding_verified': False,
            'native_qualification_complete': False,
            'stress_solve_authorized': False}
