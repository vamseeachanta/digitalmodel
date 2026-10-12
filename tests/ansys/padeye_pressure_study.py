"""Independent structured PLANE182 pressure verification; no stress assessment.

Coordinate membership uses a fixed 1e-7 mm CDB serialization envelope.
This accommodates formatted coordinate export, not engineering geometry error.
Pressure comparisons include E16.9 rounding and propagated coordinate bounds.
Input dictionaries alone do not establish native or frozen-source provenance.
"""
import math
from collections import defaultdict


COORDINATE_ENVELOPE_MM = 1e-7
FACE_NODES = {1: (1, 0), 2: (2, 1), 3: (3, 2), 4: (0, 3)}
TARGET_FORCE_N = 50000.0


def _require(condition, message):
    if not condition:
        raise ValueError(message)


def _number(value):
    _require(type(value) in (int, float) and math.isfinite(value), "nonfinite or nonnumeric input")
    return float(value)


def _identifier(value):
    _require(type(value) is int and value > 0, "IDs must be positive integers")
    return value


def _configuration(mesh):
    _require(_number(mesh["thickness_mm"]) == 8, "frozen thickness must be 8 mm")
    _require(_number(mesh["radius_mm"]) == 40, "frozen hole radius must be 40 mm")
    _require(len(mesh["center_mm"]) == 2, "center must have two coordinates")
    center = tuple(_number(value) for value in mesh["center_mm"])
    _require(center == (200, 220), "frozen center must be (200,220) mm")
    count = _identifier(mesh["upper_edges"])
    _require(count in (16, 32, 64), "upper-edge count is outside the approved matrix")
    return count


def _nodes(rows):
    _require(isinstance(rows, list) and rows, "nodes must be a nonempty list")
    nodes, coordinates = {}, set()
    for row in rows:
        identifier = _identifier(row["id"])
        xy = (_number(row["x_mm"]), _number(row["y_mm"]))
        _require(identifier not in nodes, "duplicate node ID")
        _require(xy not in coordinates, "duplicate node coordinates")
        nodes[identifier] = xy
        coordinates.add(xy)
    return nodes


def _cross(a, b, c):
    return (b[0] - a[0]) * (c[1] - b[1]) - (b[1] - a[1]) * (c[0] - b[0])


def _topology(rows, nodes):
    _require(isinstance(rows, list) and rows, "elements must be a nonempty list")
    elements, faces, incidence = {}, {}, defaultdict(list)
    used, signatures = set(), set()
    for row in rows:
        identifier = _identifier(row["id"])
        ids = tuple(_identifier(value) for value in row["nodes"])
        _require(identifier not in elements, "duplicate element ID")
        _require(len(ids) == 4 and len(set(ids)) == 4, "PLANE182 requires four distinct corners")
        _require(set(ids) <= nodes.keys(), "unknown element node")
        _require(frozenset(ids) not in signatures, "duplicate element connectivity")
        points = [nodes[value] for value in ids]
        _require(all(_cross(points[i], points[(i+1) % 4], points[(i+2) % 4]) > 0
                     for i in range(4)), "element must be convex and CCW")
        elements[identifier] = ids
        used.update(ids)
        signatures.add(frozenset(ids))
        for face, (first, second) in FACE_NODES.items():
            edge = (ids[first], ids[second])
            key = (identifier, face)
            faces[key] = edge
            incidence[tuple(sorted(edge))].append(key)
    _require(used == nodes.keys(), "mesh contains unused nodes")
    _connected_manifold(elements, faces, incidence)
    return faces, incidence


def _connected_manifold(elements, faces, incidence):
    adjacency, boundary_degree = defaultdict(set), defaultdict(int)
    for edge, owners in incidence.items():
        _require(len(owners) in (1, 2), "nonmanifold mesh edge")
        if len(owners) == 2:
            first, second = owners
            _require(faces[first] == faces[second][::-1], "interior edge orientation mismatch")
            adjacency[first[0]].add(second[0])
            adjacency[second[0]].add(first[0])
        else:
            for node in edge:
                boundary_degree[node] += 1
    _require(all(degree == 2 for degree in boundary_degree.values()), "open or branched boundary")
    visited, pending = set(), [next(iter(elements))]
    while pending:
        current = pending.pop()
        if current not in visited:
            visited.add(current)
            pending.extend(adjacency[current] - visited)
    _require(visited == elements.keys(), "disconnected element mesh")


def _on_upper_circle(point):
    x, y = point[0] - 200, point[1] - 220
    return abs(math.hypot(x, y) - 40) <= COORDINATE_ENVELOPE_MM and y >= -COORDINATE_ENVELOPE_MM


def _upper_arc(nodes, faces, incidence, count):
    upper = {}
    for owners in incidence.values():
        if len(owners) != 1:
            continue
        key = owners[0]
        edge = faces[key]
        if all(_on_upper_circle(nodes[node]) for node in edge):
            upper[key] = edge
    _require(len(upper) == count, "missing or extra true upper-hole boundary faces")
    arc_nodes = {node for edge in upper.values() for node in edge}
    _require(len(arc_nodes) == count + 1, "upper arc is not a single open chain")
    ordered = sorted(arc_nodes, key=lambda node: math.atan2(max(0, nodes[node][1]-220), nodes[node][0]-200))
    _require(math.dist(nodes[ordered[0]], (240, 220)) <= COORDINATE_ENVELOPE_MM and
             math.dist(nodes[ordered[-1]], (160, 220)) <= COORDINATE_ENVELOPE_MM,
             "upper arc must include both diameter endpoints")
    expected = set(zip(ordered, ordered[1:]))
    _require(set(upper.values()) == expected, "upper arc is incomplete, crossed or reversed")
    for edge in expected:
        first, second = (nodes[node] for node in edge)
        _require(math.dist(first, second) > COORDINATE_ENVELOPE_MM, "collapsed arc edge")
    return upper


def _quadrature(first, second, p1, p2):
    """Two-point Gauss rule integrates linear traction and quadratic moment."""
    dx, dy = second[0] - first[0], second[1] - first[1]
    length = math.hypot(dx, dy)
    _require(length > 0 and math.isfinite(length), "invalid face length")
    # Documented J-I etc. face direction: into-element normal lies to its right.
    nx, ny = dy / length, -dx / length
    fx = fy = moment = 0.0
    for xi in (-1 / math.sqrt(3), 1 / math.sqrt(3)):
        ratio = (xi + 1) / 2
        pressure = (1 - ratio) * p1 + ratio * p2
        x, y = first[0] + ratio * dx - 200, first[1] + ratio * dy - 220
        force = pressure * 8 * length / 2
        fx += force * nx
        fy += force * ny
        moment += force * (x * ny - y * nx)
    return fx, fy, moment


def _integrate(nodes, edges, values):
    samples = [_quadrature(nodes[edges[key][0]], nodes[edges[key][1]], *pressure)
               for key, pressure in values.items()]
    result = tuple(math.fsum(sample[i] for sample in samples) for i in range(3))
    _require(all(math.isfinite(value) for value in result), "nonfinite integrated load")
    return result


def _pressure_rounding(value):
    # CDB E16.9: conservatively budget nine significant mantissa digits.
    return 0.5 * 10 ** (math.floor(math.log10(abs(value))) - 8) if value else 0


def _load_values(rows, upper, nodes, alpha, gradient):
    _require(isinstance(rows, list), "native pressures must be a list")
    values = {}
    for row in rows:
        key = (_identifier(row["element"]), _identifier(row["face"]))
        _require(key in upper, "pressure is not on a true upper-hole boundary face")
        _require(key not in values, "duplicate face pressure")
        pressure = (_number(row["p1_mpa"]), _number(row["p2_mpa"]))
        for node, actual in zip(upper[key], pressure):
            _require(actual >= 0, "negative face pressure")
            expected = alpha * gradient * max(0, nodes[node][1] - 220)
            tolerance = (4 * COORDINATE_ENVELOPE_MM * alpha * gradient +
                         _pressure_rounding(actual) + _pressure_rounding(expected))
            _require(abs(actual - expected) <= tolerance, "pressure shape or endpoint order mismatch")
        values[key] = pressure
    _require(values.keys() == upper.keys(), "missing upper-hole face pressure")
    return values


def verify_pressure_mesh(mesh: dict, pressures: list[dict]) -> dict:
    """Verify structured geometry/load evidence, raising ValueError on refusal."""
    try:
        count = _configuration(mesh)
        nodes = _nodes(mesh["nodes"])
        faces, incidence = _topology(mesh["elements"], nodes)
        upper = _upper_arc(nodes, faces, incidence, count)
        gradient = 2 * TARGET_FORCE_N / (math.pi * 40**2 * 8)
        unscaled = {key: tuple(gradient * max(0, nodes[node][1] - 220) for node in edge)
                    for key, edge in upper.items()}
        original = _integrate(nodes, upper, unscaled)
        _require(original[1] > 0, "nonpositive unscaled vertical force")
        alpha = TARGET_FORCE_N / original[1]
        _require(math.isfinite(alpha) and alpha > 0, "invalid normalization factor")
        values = _load_values(pressures, upper, nodes, alpha, gradient)
        fx, fy, moment = _integrate(nodes, upper, values)
        _require(math.hypot(fx, fy - TARGET_FORCE_N) <= 50, "force error exceeds 50 N")
        _require(abs(moment) <= 2000, "hole-center moment exceeds 2000 N mm")
        return {"status": "verified_preparation", "force_n": [fx, fy],
                "moment_hole_nmm": moment, "moment_origin_nmm": moment + 200*fy - 220*fx,
                "alpha": alpha, "unscaled_force_n": list(original[:2]),
                "unscaled_moment_hole_nmm": original[2], "upper_edges": count,
                "evidence_scope": "structured_pressure_mesh_only",
                "native_binding_verified": False, "native_qualification_complete": False}
    except (KeyError, TypeError, OverflowError) as error:
        raise ValueError(f"malformed structured mesh or pressure evidence: {error}") from error
