# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Explicit quadratic beam ordering preserves existing CalculiX callers."""

import numpy as np
import pytest

from digitalmodel.solvers.calculix.inp_writer import INPWriter


def test_gmsh_b32_is_permuted_on_export(tmp_path):
    nodes = np.zeros((3, 3))
    elements = {"Line 3": {"connectivity": np.array([[0, 2, 1]])}}
    writer = INPWriter(nodes, elements, beam_node_order="gmsh")
    text = writer.write(tmp_path / "beam.inp").read_text()
    assert "*ELEMENT, TYPE=B32, ELSET=EBEAM_B32\n1, 1, 2, 3\n" in text


@pytest.mark.parametrize("order", ["GMSH", "unknown", "", None, 0, [], {}])
def test_invalid_order_is_rejected_at_construction(order):
    with pytest.raises(ValueError, match="beam_node_order"):
        INPWriter(np.zeros((3, 3)), {}, beam_node_order=order)


@pytest.mark.parametrize("rows", [
    [0, 2, 1], [[0, 1]], [[0, 1, 2, 3]],
    [[0, 1, 2], [0, 1]], [[-1, 2, 1]], [[0, 3, 1]],
    [[0, True, 2]], [[0, np.bool_(True), 2]], [[0, 1.5, 2]],
    [[0, 1.0, 2]], [[0, "1", 2]], [[0, float("nan"), 2]],
    [[0, float("inf"), 2]], [[0, 1, 1]],
])
@pytest.mark.parametrize("existing", [False, True])
def test_invalid_gmsh_rows_leave_destination_and_state_unchanged(
        tmp_path, rows, existing):
    from copy import deepcopy

    elements = {"Line 3": {"connectivity": rows}}
    writer = INPWriter(np.zeros((3, 3)), elements, beam_node_order="gmsh")
    before = deepcopy(writer.__dict__)
    target = tmp_path / "invalid.inp"
    if existing:
        target.write_bytes(b"preserved destination\n")
    with pytest.raises(ValueError, match="B32"):
        writer.write(target)
    assert target.exists() is existing
    if existing:
        assert target.read_bytes() == b"preserved destination\n"
    assert repr(writer.__dict__) == repr(before)


@pytest.mark.parametrize("order", [None, "calculix", "gmsh"])
def test_mixed_mesh_repeated_export_preserves_inputs_and_memberships(tmp_path, order):
    from copy import deepcopy

    elements = {
        "unsupported": {"connectivity": np.array([[90, 91, 92]])},
        "Tetrahedron 4": {"connectivity": np.array([[3, 0, 7, 2]])},
        "Quadrilateral 4": {"connectivity": np.array([[6, 1, 4, 0]])},
        "Line 2": {"connectivity": np.array([[7, 3]])},
        "Line 3": {"connectivity": np.array([[1, 7, 5], [6, 0, 3]])},
    }
    snapshots = deepcopy(elements)
    nodes = np.arange(24).reshape(8, 3)
    for data in elements.values():
        data["connectivity"].flags.writeable = False
    nodes.flags.writeable = False
    kwargs = {} if order is None else {"beam_node_order": order}
    writer = INPWriter(nodes, elements, **kwargs)
    writer.add_material("MAT", 200e9, 0.3)
    writer.add_shell_section("ESHELL_S4", "MAT", 0.01)
    writer.add_element_set("QUADRATIC", [3, 4])
    writer.add_beam_section("QUADRATIC", "MAT", "CIRC", (0.2,))
    writer.add_node_set("ENDS", [1, 7])
    before = repr(writer.__dict__)
    target = tmp_path / "mixed.inp"
    text = writer.write(target).read_text()
    assert "*ELEMENT, TYPE=C3D4, ELSET=EALL\n1, 4, 1, 8, 3\n" in text
    assert "*ELEMENT, TYPE=S4, ELSET=ESHELL_S4\n2, 7, 2, 5, 1\n" in text
    assert "*ELEMENT, TYPE=B31, ELSET=EBEAM_B31\n3, 8, 4\n" in text
    expected = ("4, 2, 6, 8\n5, 7, 4, 1\n" if order == "gmsh"
                else "4, 2, 8, 6\n5, 7, 1, 4\n")
    assert "*ELEMENT, TYPE=B32, ELSET=EBEAM_B32\n" + expected in text
    assert "*ELSET, ELSET=QUADRATIC\n4, 5\n" in text
    assert "*NSET, NSET=ENDS\n2, 8\n" in text
    assert "SECTION=CIRC\n0.2\n0.0, 0.0, 1.0\n" in text
    original_bytes = target.read_bytes()
    assert writer.write(target).read_bytes() == original_bytes
    assert repr(writer.__dict__) == before
    for name, data in elements.items():
        np.testing.assert_array_equal(data["connectivity"], snapshots[name]["connectivity"])
    np.testing.assert_array_equal(nodes, np.arange(24).reshape(8, 3))


@pytest.mark.parametrize("order", [None, "calculix"])
def test_default_mode_keeps_existing_id_coercion(tmp_path, order):
    elements = {"Line 3": {"connectivity": np.array([[0.0, 1.0, 2.0]])}}
    kwargs = {} if order is None else {"beam_node_order": order}
    writer = INPWriter(np.zeros((3, 3)), elements, **kwargs)
    assert "\n1, 1, 2, 3\n" in writer.write(tmp_path / "legacy.inp").read_text()
