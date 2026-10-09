"""Synthetic structured-mesh checks; no native load transfer is established."""
import copy
import math

import pytest


def synthetic_ring(count=16, nonuniform=False):
    """Independent annular fixture, not the production rectangle generator."""
    upper = [math.pi * i / count for i in range(count + 1)]
    if nonuniform == "asymmetric":
        upper = [math.pi * (i / count)**4 for i in range(count + 1)]
    elif nonuniform:
        upper = [a + 0.15 * math.sin(2 * a) for a in upper]
    angles = upper + [math.pi + a for a in upper[1:-1]]
    nodes = []
    for radius in (40, 60):
        for angle in angles:
            nodes.append({"id": len(nodes) + 1, "x_mm": 200 + radius * math.cos(angle),
                          "y_mm": 220 + radius * math.sin(angle)})
    elements = []
    for i in range(2 * count):
        j = (i + 1) % (2 * count)
        elements.append({"id": i + 1, "nodes": [i + 1, i + 1 + 2 * count,
                                                               j + 1 + 2 * count, j + 1]})
    mesh = {"nodes": nodes, "elements": elements, "thickness_mm": 8,
            "center_mm": [200, 220], "radius_mm": 40, "upper_edges": count}
    p0 = 2 * 50000 / (math.pi * 40 * 8)
    # Exact vertical resultant on chords: n_y ds = -dx.
    unscaled_y = sum(p0 * 8 * (math.sin(a) + math.sin(b)) / 2 *
                     40 * (math.cos(a) - math.cos(b)) for a, b in zip(upper, upper[1:]))
    alpha = 50000 / unscaled_y
    pressures = [{"element": i + 1, "face": 4,
                  "p1_mpa": alpha * p0 * math.sin(upper[i]),
                  "p2_mpa": alpha * p0 * math.sin(upper[i + 1])} for i in range(count)]
    return mesh, pressures


def verify(mesh, pressures):
    from tests.ansys.padeye_pressure_study import verify_pressure_mesh
    return verify_pressure_mesh(mesh, pressures)


@pytest.mark.parametrize("count", [16, 32, 64])
def test_regular_polygon_matches_analytic_normalization(count):
    mesh, pressures = synthetic_ring(count)
    result = verify(mesh, pressures)
    assert result["alpha"] == pytest.approx(math.pi / (count * math.sin(math.pi / count)))
    assert result["force_n"] == pytest.approx([0, 50000], abs=1e-7)
    assert result["moment_hole_nmm"] == pytest.approx(0, abs=1e-6)
    assert result["moment_origin_nmm"] == pytest.approx(10000000, abs=1e-5)
    assert result["native_binding_verified"] is False
    assert result["native_qualification_complete"] is False


def test_nonuniform_polygon_uses_actual_faces_not_regular_formula():
    mesh, pressures = synthetic_ring(nonuniform=True)
    result = verify(mesh, pressures)
    regular = math.pi / (16 * math.sin(math.pi / 16))
    assert abs(result["alpha"] - regular) > 1e-5
    assert result["force_n"] == pytest.approx([0, 50000], abs=1e-7)


@pytest.mark.parametrize("rotation", [0, 1, 2, 3])
def test_documented_plane182_face_mapping(rotation):
    mesh, pressures = synthetic_ring()
    for element in mesh["elements"]:
        ids = element["nodes"]
        element["nodes"] = ids[rotation:] + ids[:rotation]
    for row in pressures:
        row["face"] = (4 - rotation - 1) % 4 + 1
    assert verify(mesh, pressures)["status"] == "verified_preparation"


def test_cdb_formatted_coordinate_and_pressure_precision_is_supported():
    mesh, pressures = synthetic_ring(64, nonuniform=True)
    for node in mesh["nodes"]:
        for key in ("x_mm", "y_mm"):
            node[key] = float(f"{node[key]:.12e}")
    for row in pressures:
        for key in ("p1_mpa", "p2_mpa"):
            row[key] = float(f"{row[key]:.8e}")
    assert verify(mesh, pressures)["status"] == "verified_preparation"


def test_scalar_normalization_cannot_repair_asymmetric_polygon_moment():
    mesh, pressures = synthetic_ring(nonuniform="asymmetric")
    with pytest.raises(ValueError, match="moment exceeds"):
        verify(mesh, pressures)


@pytest.mark.parametrize("field", ["nodes", "elements", "center_mm"])
def test_malformed_container_types_are_refused(field):
    mesh, pressures = synthetic_ring()
    mesh[field] = 3
    with pytest.raises(ValueError):
        verify(mesh, pressures)


def test_reordered_export_rows_do_not_change_physical_integration():
    mesh, pressures = synthetic_ring(32)
    baseline = verify(mesh, pressures)
    mesh["nodes"].reverse()
    mesh["elements"].reverse()
    pressures.reverse()
    assert verify(mesh, pressures) == baseline


@pytest.mark.parametrize("damage", ["missing", "duplicate", "interior", "lower", "outer",
                                     "reversed_values", "negative", "nan", "inf", "shape",
                                     "wrong_face", "unknown_element", "boolean_id"])
def test_bad_face_loads_refuse(damage):
    mesh, pressures = synthetic_ring()
    if damage == "missing":
        pressures.pop()
    elif damage == "duplicate":
        pressures.append(copy.deepcopy(pressures[2]))
    elif damage in {"interior", "outer", "wrong_face"}:
        pressures[2]["face"] = {"interior": 1, "outer": 2, "wrong_face": 5}[damage]
    elif damage == "lower":
        pressures[2]["element"] += 16
    elif damage == "reversed_values":
        row = pressures[2]
        row["p1_mpa"], row["p2_mpa"] = row["p2_mpa"], row["p1_mpa"]
    elif damage in {"negative", "nan", "inf"}:
        pressures[2]["p1_mpa"] = {"negative": -1, "nan": float("nan"), "inf": float("inf")}[damage]
    elif damage == "shape":
        # A non-sinusoidal symmetric distribution can normalize to the same Fy.
        for row in pressures:
            row["p1_mpa"] = row["p2_mpa"] = 50000 / (8 * 80)
    else:
        pressures[2]["element"] = 99999 if damage == "unknown_element" else True
    with pytest.raises(ValueError):
        verify(mesh, pressures)


@pytest.mark.parametrize("damage", ["duplicate_node", "duplicate_element", "unknown_node",
                                     "clockwise", "repeated_corner", "off_circle", "thickness",
                                     "center", "radius", "edge_count", "nonfinite_node",
                                     "nonmanifold", "disconnected", "missing_mesh"])
def test_bad_mesh_or_metadata_refuses(damage):
    mesh, pressures = synthetic_ring()
    if damage == "duplicate_node":
        mesh["nodes"].append(copy.deepcopy(mesh["nodes"][0]))
    elif damage == "duplicate_element":
        mesh["elements"].append(copy.deepcopy(mesh["elements"][0]))
    elif damage == "unknown_node":
        mesh["elements"][0]["nodes"][0] = 99999
    elif damage == "clockwise":
        mesh["elements"][0]["nodes"].reverse()
    elif damage == "repeated_corner":
        mesh["elements"][0]["nodes"][0] = mesh["elements"][0]["nodes"][1]
    elif damage == "off_circle":
        mesh["nodes"][3]["y_mm"] += 0.01
    elif damage == "nonfinite_node":
        mesh["nodes"][3]["x_mm"] = float("nan")
    elif damage == "nonmanifold":
        duplicate = copy.deepcopy(mesh["elements"][0])
        duplicate["id"] = 1000
        mesh["elements"].append(duplicate)
    elif damage == "disconnected":
        mesh["nodes"] += [{"id": 1000 + i, "x_mm": x, "y_mm": y}
                          for i, (x, y) in enumerate([(0, 0), (10, 0), (10, 10), (0, 10)])]
        mesh["elements"].append({"id": 1000, "nodes": [1000, 1001, 1002, 1003]})
    elif damage == "missing_mesh":
        del mesh["elements"]
    else:
        key, value = {"thickness": ("thickness_mm", 80), "center": ("center_mm", [0, 0]),
                      "radius": ("radius_mm", 41), "edge_count": ("upper_edges", 17)}[damage]
        mesh[key] = value
    with pytest.raises(ValueError):
        verify(mesh, pressures)
