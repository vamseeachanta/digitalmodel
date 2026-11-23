"""Fixed eight-block mapped mesh for the approved preparation-only example.

This geometry/load intent is not native evidence or a qualified contact model.
"""
import math
from dataclasses import asdict

from digitalmodel.ansys.padeye import PadeyeGeometry

MESH_LEVELS = {10.0: (16, 5), 5.0: (32, 10), 2.5: (64, 20)}


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
    return [i*math.pi/upper_edges for i in range(2*upper_edges)]


def _square_point(index, width):
    corners = [(260, 220), (260, 280), (200, 280), (140, 280),
               (140, 220), (140, 160), (200, 160), (260, 160), (260, 220)]
    count = width // 8
    sector, local = divmod(index, count)
    a, b = corners[sector:sector+2]
    return tuple(x+(y-x)*local/count for x, y in zip(a, b))


def _nodes(angles, layers):
    nodes = []
    for layer in range(layers + 1):
        for index, angle in enumerate(angles):
            inner = (200+40*math.cos(angle), 220+40*math.sin(angle))
            outer = _square_point(index, len(angles))
            x, y = (a+(b-a)*layer/layers for a, b in zip(inner, outer))
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


def _axis_points(ends, counts, scale):
    return [a+(b-a)*i/(n*scale) for a, b, n in zip(ends, ends[1:], counts)
            for i in range(n*scale)] + [ends[-1]]


def _outer_blocks(nodes, elements, upper_edges):
    scale = upper_edges // 16
    xs = _axis_points([0, 140, 200, 260, 400], [14, 4, 4, 14], scale)
    ys = _axis_points([0, 160, 220, 280, 300], [16, 4, 4, 2], scale)
    lookup = {(n['x_mm'], n['y_mm']): n['id'] for n in nodes}
    for x0, x1 in zip(xs, xs[1:]):
        for y0, y1 in zip(ys, ys[1:]):
            if 140 <= x0 < 260 and 160 <= y0 < 280:
                continue
            ids = []
            for x, y in [(x0, y0), (x1, y0), (x1, y1), (x0, y1)]:
                point = (round(x, 12), round(y, 12))
                if point not in lookup:
                    lookup[point] = len(nodes)+1
                    nodes.append({'id': len(nodes)+1, 'x_mm': point[0], 'y_mm': point[1]})
                ids.append(lookup[point])
            elements.append({'id': len(elements)+1, 'nodes': ids})


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
    elements = _elements(len(angles), layers)
    _outer_blocks(nodes, elements, upper_edges)
    return {'schema': 'padeye-pressure-preparation-v1',
            'geometry': asdict(geometry), 'nodes': nodes,
            'elements': elements, 'pressures': pressures,
            'center_mm': [200.0, 220.0], 'radius_mm': 40.0,
            'thickness_mm': 8.0, 'upper_edges': upper_edges,
            'radial_layers': layers, 'alpha': alpha,
            'pressure_slope_mpa_per_mm': slope,
            'unscaled_force_n': unscaled,
            'native_binding_verified': False,
            'native_qualification_complete': False,
            'stress_solve_authorized': False}
