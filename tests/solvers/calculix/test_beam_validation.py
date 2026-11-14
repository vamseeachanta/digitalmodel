"""Offline contracts for issue #1676's approved compatibility policy.

CIRC/B31 follows the issue's recorded v2.21 rejection, not a universal manual
prohibition. PIPE/B32R follows the approved review's inference from CalculiX
2.22 section 6.3 (general-section expansion to C3D20R). No solver is invoked.
All dimensions and memberships are synthetic serialization inputs.
"""

import pickle

import numpy as np
import pytest

from digitalmodel.solvers.calculix.inp_writer import INPWriter


def writer_for(elements):
    writer = INPWriter(np.zeros((8, 3)), elements)
    writer.add_material("MAT", 1000., .3)
    writer.add_node_set("FIX", [0])
    # Pending BCs/loads would mutate writer state if serialization were reached.
    writer.add_boundary_condition("FIX", 1)
    writer.add_load("FIX", "cload", 1.)
    return writer


def mixed_writer():
    """Exported IDs: C3D4=0, B32=1,2, B31=3, S4=4; triangles skipped."""
    writer = writer_for({
        "Triangle 3": {"connectivity": np.array([[0, 1, 2], [2, 3, 4]])},
        "Tetrahedron 4": {"connectivity": np.array([[0, 1, 2, 3]])},
        "Line 3": {"connectivity": np.array([[1, 5, 3], [3, 6, 7]])},
        "Line 2": {"connectivity": np.array([[0, 7]])},
        "Quadrilateral 4": {"connectivity": np.array([[0, 1, 2, 3]])},
    })
    writer.add_shell_section("ESHELL_S4", "MAT", .2)
    return writer


def assert_rejected(writer, tmp_path, match, existing=True):
    destination = tmp_path / "deck.inp"
    sentinel = b"original deck\r\n\x00"
    if existing:
        destination.write_bytes(sentinel)
    state = pickle.dumps(vars(writer))
    nodes, elements = writer.nodes, writer.elements
    with pytest.raises(ValueError, match=match):
        writer.write(destination)
    assert pickle.dumps(vars(writer)) == state
    assert writer.nodes is nodes and writer.elements is elements
    if existing:
        assert destination.read_bytes() == sentinel
    else:
        assert not destination.exists()


@pytest.mark.parametrize("existing", [False, True])
@pytest.mark.parametrize("explicit", [False, True])
@pytest.mark.parametrize("shape, gmsh, exported", [
    ("CIRC", "Line 2", "B31"), ("circ", "Line 2", "B31"),
    ("PIPE", "Line 2", "B31"), ("pipe", "Line 2", "B31"),
    ("PIPE", "Line 3", "B32"), ("pipe", "Line 3", "B32"),
])
def test_incompatible_sections_preserve_destination_and_state(
    tmp_path, existing, explicit, shape, gmsh, exported,
):
    width = 2 if gmsh == "Line 2" else 3
    writer = writer_for({gmsh: {"connectivity": np.arange(width).reshape(1, width)}})
    target = "CUSTOM" if explicit else f"EBEAM_{exported}"
    if explicit:
        writer.add_element_set(target, [0])
    dims = (.08, .01) if shape.upper() == "PIPE" else (.05,)
    writer.add_beam_section(target, "MAT", shape, dims)
    assert_rejected(writer, tmp_path, f"{shape.upper()}.*{target}.*{exported}", existing)


@pytest.mark.parametrize("shape", ["CIRC", "PIPE"])
@pytest.mark.parametrize("target", ["UNKNOWN", "EMPTY", "EBEAM_B32"])
def test_unresolved_or_empty_targets_rejected(tmp_path, shape, target):
    writer = writer_for({"Line 3": {"connectivity": np.empty((0, 3), dtype=int)}})
    writer.add_element_set("EMPTY", [])
    writer.add_beam_section(target, "MAT", shape, (.05,))
    assert_rejected(writer, tmp_path, f"{shape}.*{target}.*nonempty")


@pytest.mark.parametrize("shape", ["CIRC", "PIPE"])
@pytest.mark.parametrize("invalid", [
    -1, 5, True, False, np.bool_(True), 1.5, 1.0, "1", None,
])
def test_invalid_membership_ids_rejected_before_set_deduplication(tmp_path, shape, invalid):
    writer = mixed_writer()
    # True and 1.0 compare equal to valid ID 1; merging into a set hides them.
    writer.add_element_set("TARGET", [1, invalid])
    writer.add_beam_section("TARGET", "MAT", shape, (.05,))
    assert_rejected(writer, tmp_path, f"{shape}.*TARGET.*exported.*ID")


@pytest.mark.parametrize("shape", ["CIRC", "PIPE"])
@pytest.mark.parametrize("ids, exported", [
    ([0], "C3D4"), ([4], "S4"), ([1, 0], "C3D4"),
    ([1, 4], "S4"), ([1, 3], "B31"),
])
def test_nonbeam_and_mixed_memberships_rejected(tmp_path, shape, ids, exported):
    writer = mixed_writer()
    writer.add_element_set("TARGET", ids)
    writer.add_beam_section("TARGET", "MAT", shape, (.05,))
    assert_rejected(writer, tmp_path, f"{shape}.*TARGET.*{exported}")


@pytest.mark.parametrize("shape", ["CIRC", "PIPE"])
@pytest.mark.parametrize("generated", [False, True])
def test_same_named_sets_are_unions(tmp_path, shape, generated):
    writer = mixed_writer()
    target = "EBEAM_B32" if generated else "TARGET"
    # Later definitions cannot replace generated or earlier explicit members.
    writer.add_element_set(target, [3])
    writer.add_element_set(target, [1])
    writer.add_beam_section(target, "MAT", shape, (.05,))
    assert_rejected(writer, tmp_path, f"{shape}.*{target}.*B31")


@pytest.mark.parametrize("shape", ["CIRC", "PIPE"])
def test_generated_solid_group_cannot_be_replaced_by_beams(tmp_path, shape):
    writer = mixed_writer()
    writer.add_element_set("EALL", [1])
    writer.add_beam_section("EALL", "MAT", shape, (.05,))
    assert_rejected(writer, tmp_path, f"{shape}.*EALL.*C3D4")


@pytest.mark.parametrize("shape", ["CIRC", "PIPE"])
def test_skipped_groups_cannot_supply_a_beam_target(tmp_path, shape):
    writer = writer_for({"Triangle 3": {"connectivity": np.array([[0, 1, 2]])}})
    writer.add_element_set("TARGET", [0])
    writer.add_beam_section("TARGET", "MAT", shape, (.05,))
    assert_rejected(writer, tmp_path, f"{shape}.*TARGET.*exported.*ID")


@pytest.mark.parametrize("shape", ["CIRC", "circ"])
@pytest.mark.parametrize("target", ["EBEAM_B32", "CUSTOM"])
def test_circ_b32_keeps_dimensions_direction_and_connectivity(tmp_path, shape, target):
    writer = mixed_writer()
    writer.add_element_set("EBEAM_B32", [np.int64(1), 2, 1])
    writer.add_element_set("CUSTOM", [np.int64(1)])
    writer.add_element_set("CUSTOM", [2])
    writer.add_beam_section(target, "MAT", shape, (.05,), direction=(0., 1., 0.))
    original = writer.elements["Line 3"]["connectivity"].copy()
    text = writer.write(tmp_path / "deck.inp").read_text()
    assert ("*ELEMENT, TYPE=B32, ELSET=EBEAM_B32\n"
            "2, 2, 6, 4\n3, 4, 7, 8\n") in text
    assert (f"*BEAM SECTION, ELSET={target}, MATERIAL=MAT, SECTION=CIRC\n"
            "0.05\n0.0, 1.0, 0.0\n") in text
    np.testing.assert_array_equal(writer.elements["Line 3"]["connectivity"], original)


def test_rect_mixed_solid_behavior_is_preserved(tmp_path):
    writer = mixed_writer()
    writer.add_element_set("MIXED", [0, 3])
    writer.add_beam_section("MIXED", "MAT", "rect", (.1, .2))
    text = writer.write(tmp_path / "deck.inp").read_text()
    assert "*SOLID SECTION, ELSET=EALL, MATERIAL=MAT" in text
    assert "*BEAM SECTION, ELSET=MIXED, MATERIAL=MAT, SECTION=RECT\n0.1, 0.2" in text


def test_pipe_explains_unsupported_b32r_and_circ_policy_is_versioned(tmp_path):
    writer = mixed_writer()
    writer.add_beam_section("EBEAM_B32", "MAT", "PIPE", (.08, .01))
    assert_rejected(writer, tmp_path, "PIPE.*EBEAM_B32.*B32R.*unsupported.*B32")
    writer = mixed_writer()
    writer.add_beam_section("EBEAM_B31", "MAT", "CIRC", (.05,))
    assert_rejected(writer, tmp_path, r"CIRC.*EBEAM_B31.*B31.*v2\.21")


@pytest.mark.parametrize("existing", [False, True])
@pytest.mark.parametrize("groups, target, exported", [
    ([("ebeam_b31", [1])], "ebeam_b31", "B31"),
    ([("EbEaM_b31", [1])], "EbEaM_b31", "B31"),
    ([("ebeam_b32", [3])], "EBEAM_B32", "B31"),
    ([("target", [3]), ("TARGET", [1])], "TARGET", "B31"),
    ([("TARGET", [3]), ("target", [1])], "target", "B31"),
    ([("eall", [1])], "eall", "C3D4"),
    ([("eshell_s4", [1])], "eshell_s4", "S4"),
])
def test_case_variant_sets_cannot_hide_incompatible_members(
    tmp_path, existing, groups, target, exported,
):
    writer = mixed_writer()
    for name, ids in groups:
        writer.add_element_set(name, ids)
    writer.add_beam_section(target, "MAT", "CIRC", (.05,))
    assert_rejected(writer, tmp_path, f"CIRC.*{target}.*{exported}", existing)


@pytest.mark.parametrize("invalid", [True, 1.0, -1, 5])
def test_case_variant_members_are_validated_before_deduplication(tmp_path, invalid):
    writer = mixed_writer()
    writer.add_element_set("ebeam_b32", [invalid])
    writer.add_beam_section("EBEAM_B32", "MAT", "CIRC", (.05,))
    assert_rejected(writer, tmp_path, "CIRC.*EBEAM_B32.*exported.*ID")


@pytest.mark.parametrize("set_name, target", [
    (None, "ebeam_b32"), (None, "EbEaM_b32"),
    ("CUSTOM", "custom"), ("custom", "CUSTOM"), ("custom", "CuStOm"),
])
def test_circ_b32_accepts_case_variant_references_without_changing_output(
    tmp_path, set_name, target,
):
    writer = mixed_writer()
    if set_name is not None:
        writer.add_element_set(set_name, [1, 2])
    writer.add_beam_section(target, "MAT", "CIRC", (.05,))
    text = writer.write(tmp_path / "deck.inp").read_text()
    assert (f"*BEAM SECTION, ELSET={target}, MATERIAL=MAT, SECTION=CIRC\n"
            "0.05\n0.0, 0.0, 1.0\n") in text
    assert "*ELEMENT, TYPE=B32, ELSET=EBEAM_B32\n2, 2, 6, 4\n3, 4, 7, 8\n" in text
    if set_name is not None:
        assert f"*ELSET, ELSET={set_name}\n2, 3\n" in text
