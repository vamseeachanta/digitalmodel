"""Offline frozen-canary mesh and basis tests; no native execution."""
import copy
from decimal import Decimal, localcontext

import pytest


CASES = [("ocv-zero-t60-n16", 16, 4833, 1536),
         ("ocv-t60-p10-n4", 4, 345, 96),
         ("ocv-t60-p10-n8", 8, 1265, 384),
         ("ocv-t60-p10-n16", 16, 4833, 1536)]


def build(case_id):
    from digitalmodel.ansys.cylinder_benchmark import build_case
    return build_case(case_id)


@pytest.mark.parametrize("case_id,n,node_count,element_count", CASES)
def test_explicit_q8_connectivity_and_frozen_counts(case_id, n, node_count, element_count):
    case = build(case_id)
    nodes = {node["node_id"]: node for node in case["nodes"]}
    assert len(nodes) == node_count
    assert len(case["elements"]) == element_count
    assert {node["x_mm"] for node in nodes.values()} == {
        str(Decimal(750) + Decimal(60) * i / (2 * n)).rstrip("0").rstrip(".")
        if "." in str(Decimal(750) + Decimal(60) * i / (2 * n)) else
        str(Decimal(750) + Decimal(60) * i / (2 * n)) for i in range(2 * n + 1)}
    for element in case["elements"]:
        ids = element["nodes"]
        assert len(ids) == len(set(ids)) == 8
        points = [(Decimal(nodes[i]["x_mm"]), Decimal(nodes[i]["y_mm"])) for i in ids]
        assert sum(a[0] * b[1] - b[0] * a[1] for a, b in
                   zip(points[:4], points[1:4] + points[:1])) > 0
        for mid, a, b in zip(points[4:], points[:4], points[1:4] + points[:1]):
            assert mid == ((a[0] + b[0]) / 2, (a[1] + b[1]) / 2)
        assert all(nodes[i]["node_type"] == "corner" for i in ids[:4])
        assert all(nodes[i]["node_type"] == "midside" for i in ids[4:])


@pytest.mark.parametrize("case_id,n,node_count,element_count", CASES)
def test_stations_are_corner_nodes_with_complete_adjacency(case_id, n, node_count, element_count):
    case = build(case_id)
    assert len(case["stations"]) == 9
    assert {(s["x_mm"], s["y_mm"]) for s in case["stations"]} == {
        (x, y) for x in ("750", "780", "810") for y in ("60", "120", "180")}
    assert len({s["id"] for s in case["stations"]}) == 9
    for station in case["stations"]:
        adjacent = [e["element_id"] for e in case["elements"] if station["node_id"] in e["nodes"][:4]]
        assert station["adjacent_element_ids"] == adjacent
        assert station["node_type"] == "corner"
        assert len(adjacent) == (4 if station["radial_id"] == "middle" else 2)


@pytest.mark.parametrize("case_id,n,node_count,element_count", CASES)
def test_loads_and_complete_bottom_midside_restraints(case_id, n, node_count, element_count):
    case = build(case_id)
    nodes = {v["node_id"]: v for v in case["nodes"]}
    assert set(case["bottom_node_ids"]) == {i for i, v in nodes.items() if v["y_mm"] == "0"}
    assert len(case["bottom_node_ids"]) == 2 * n + 1
    elements = {v["element_id"]: v for v in case["elements"]}
    faces = case["pressure_faces"]
    assert len(faces) == (0 if "zero" in case_id else 6 * n)
    for face in faces:
        assert face["face"] == 4 and face["pressure_mpa"] == "10"
        assert all(nodes[e]["x_mm"] == "750" for e in
                   [elements[face["element_id"]]["nodes"][i] for i in (0, 3, 7)])


@pytest.mark.parametrize("value", ["60", "60.0", "60.000", "6e1", "+60"])
def test_exact_decimal_basis_normalization(value):
    from digitalmodel.ansys.cylinder_benchmark import frozen_basis, validate_basis
    basis = frozen_basis()
    basis["wall_thickness_mm"] = value
    assert validate_basis(basis)["wall_thickness_mm"] == "60"


@pytest.mark.parametrize("field,value", [("wall_thickness_mm", "6"),
    ("wall_thickness_mm", 60), ("poisson_ratio", "NaN"), ("inner_radius_mm", "750.001"),
    ("delta_temperature_C", "1"), ("external_pressure_mpa", "10"),
    ("length_unit", "m"), ("pressure_unit", "Pa"), ("keyopts", {"1": 0, "3": 0, "6": 0})])
def test_refuse_changed_basis_or_units(field, value):
    from digitalmodel.ansys.cylinder_benchmark import frozen_basis, validate_basis
    basis = copy.deepcopy(frozen_basis())
    basis[field] = value
    with pytest.raises(ValueError):
        validate_basis(basis)


@pytest.mark.parametrize("case_id", ["ocv-t45-p10-n4", "ocv-t60-p10-n5", "../case", None, 4])
def test_only_four_frozen_cases(case_id):
    with pytest.raises(ValueError):
        build(case_id)


def test_build_is_deterministic_and_returns_unshared_metadata():
    a, b = build("ocv-t60-p10-n4"), build("ocv-t60-p10-n4")
    assert a == b
    assert isinstance(a["deck_bytes"], bytes) and b"\r" not in a["deck_bytes"]
    assert len(a["case_token"]) <= 8
    a["nodes"][0]["x_mm"] = "0"
    assert build("ocv-t60-p10-n4") == b


def test_mesh_identity_does_not_depend_on_callers_decimal_precision():
    expected = build("ocv-t60-p10-n16")
    with localcontext() as context:
        context.prec = 3
        assert build("ocv-t60-p10-n16") == expected


def test_mesh_identity_ignores_ambient_exponent_bounds_and_traps():
    from decimal import Overflow, Underflow
    expected = {case_id: build(case_id)["deck_sha256"] for case_id, *_ in CASES}
    with localcontext() as context:
        context.Emax = 2
        context.Emin = -2
        context.traps[Overflow] = True
        context.traps[Underflow] = True
        assert {case_id: build(case_id)["deck_sha256"] for case_id, *_ in CASES} == expected


@pytest.mark.parametrize("field,value", [("pressure_mpa", 10), ("pressure_mpa", "10.001"),
    ("radial_divisions", True), ("axial_divisions", 23), ("case_token", "BAD")])
def test_case_metadata_cannot_relax_frozen_identity(field, value):
    from digitalmodel.ansys.cylinder_benchmark import case_definition, validate_case
    case = case_definition("ocv-t60-p10-n4")
    case[field] = value
    with pytest.raises(ValueError):
        validate_case(case)
