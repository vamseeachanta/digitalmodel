"""Input-fidelity checks; no solver execution or physical response claims.

Source: CalculiX CrunchiX User's Manual 2.22 (2024), section 7.49
(*ELEMENT). Synthetic node coordinates and indices exercise serialization only.
Hand calculation: two input rows require two output elements, numbered 1 and 2;
each zero-based node index n is written as n + 1. No fitted constants are used.
"""

import numpy as np
import pytest

from digitalmodel.solvers.calculix.inp_writer import INPWriter


@pytest.mark.parametrize("unsupported", ["Triangle 3", "Pyramid 5", "unknown"])
@pytest.mark.parametrize("existing", [False, True])
def test_unsupported_type_rejected_without_writing(tmp_path, unsupported, existing):
    """One supported row plus an unknown row must never become one output row."""
    writer = INPWriter(np.zeros((4, 3)), {
        "Tetrahedron 4": {"connectivity": np.array([[0, 1, 2, 3]])},
        unsupported: {"connectivity": np.array([[0, 1, 2]])},
    })
    path = tmp_path / "mesh.inp"
    if existing:
        path.write_text("retained deck\n")

    with pytest.raises(ValueError, match=f"Unsupported element type.*{unsupported}"):
        writer.write(path)

    if existing:
        assert path.read_text() == "retained deck\n"
    else:
        assert not path.exists()


def test_unknown_empty_block_is_rejected(tmp_path):
    """The accepted subset is checked even if an unknown block has no rows."""
    writer = INPWriter(np.empty((0, 3)), {"unknown": {"connectivity": []}})
    with pytest.raises(ValueError, match="Unsupported element type.*unknown"):
        writer.write(tmp_path / "mesh.inp")


@pytest.fixture
def solid_writer():
    """Two synthetic tetrahedra: global indices {0, 1}, deck IDs {1, 2}."""
    return INPWriter(np.zeros((5, 3)), {
        "Tetrahedron 4": {
            "connectivity": np.array([[0, 1, 2, 3], [1, 2, 3, 4]]),
        },
    })


@pytest.mark.parametrize("materials", [[], ["MAT_A", "MAT_B"]])
def test_solid_requires_unambiguous_material(tmp_path, solid_writer, materials):
    """No material leaves two unassigned; two candidates do not select either.

    CalculiX 2.22 (2024), section 7.118 requires a MATERIAL reference.
    Unit modulus and zero Poisson ratio are synthetic serialization inputs,
    with no physical response evaluated.
    """
    for name in materials:
        solid_writer.add_material(name, 1.0, 0.0)
    path = tmp_path / "mesh.inp"
    path.write_text("retained deck\n")
    with pytest.raises(ValueError, match="solid.*material"):
        solid_writer.write(path)
    assert path.read_text() == "retained deck\n"


def test_single_material_default_remains_supported(tmp_path, solid_writer):
    """The unique candidate applies to both solids via the generated EALL set."""
    solid_writer.add_material("MAT_A", 1.0, 0.0)
    content = solid_writer.write(tmp_path / "mesh.inp").read_text()
    assert "*SOLID SECTION, ELSET=EALL, MATERIAL=MAT_A" in content


def assign_regions(writer):
    """Partition {0, 1} into {0} and {1}; material order must not select identity."""
    writer.add_material("MAT_B", 1.0, 0.0)
    writer.add_material("MAT_A", 1.0, 0.0)
    writer.add_element_set("REGION_A", [0])
    writer.add_element_set("REGION_B", [1])
    writer.add_solid_section("region_a", "mat_a")
    writer.add_solid_section("REGION_B", "MAT_B")


def test_explicit_regions_preserve_identity_and_count(tmp_path, solid_writer):
    """Hand enumeration: rows [0,1,2,3] and [1,2,3,4] become the literals below.

    Region A owns deck element 1; region B owns 2. Their disjoint union has
    exactly two elements. Keyword semantics: CalculiX 2.22 (2024), sections
    7.49, 7.53 and 7.118; case folding: section 7 (Input deck format).
    """
    assign_regions(solid_writer)
    path = solid_writer.write(tmp_path / "regions.inp")
    content = path.read_text()
    assert "*ELEMENT, TYPE=C3D4, ELSET=EALL\n1, 1, 2, 3, 4\n2, 2, 3, 4, 5\n" in content
    assert "*ELSET, ELSET=REGION_A\n1\n" in content
    assert "*ELSET, ELSET=REGION_B\n2\n" in content
    assert [line for line in content.splitlines() if line.startswith("*SOLID SECTION")] == [
        "*SOLID SECTION, ELSET=REGION_A, MATERIAL=MAT_A",
        "*SOLID SECTION, ELSET=REGION_B, MATERIAL=MAT_B",
    ]
    assert solid_writer.write(path).read_text() == content


@pytest.mark.parametrize("fault, message", [
    ("missing_region", "Unknown element set"),
    ("missing_material", "Unknown material"),
    ("incomplete", "Unassigned solid"),
    ("overlap", "multiple solid sections"),
    ("repeat_section", "multiple solid sections"),
    ("empty_region", "empty"),
    ("negative_id", "element ID"),
    ("out_of_range_id", "element ID"),
    ("fractional_id", "element ID"),
    ("boolean_id", "element ID"),
    ("duplicate_material", "Duplicate material"),
    ("duplicate_set", "Duplicate element set"),
    ("reserved_set", "Reserved element set"),
    ("reserved_beam_set", "Reserved element set"),
    ("invalid_name", "Invalid element set name"),
    ("invalid_material", "Invalid material name"),
])
def test_invalid_regions_fail_before_output(tmp_path, solid_writer, fault, message):
    """Any gap or overlap violates the hand partition {0} union {1} = {0, 1}."""
    assign_regions(solid_writer)
    if fault == "missing_region":
        solid_writer.add_solid_section("ABSENT", "MAT_A")
    elif fault == "missing_material":
        solid_writer.add_solid_section("REGION_A", "ABSENT")
    elif fault == "incomplete":
        solid_writer._solid_sections.pop()
    elif fault == "overlap":
        solid_writer._element_sets[1]["ids"] = [0, 1]
    elif fault == "repeat_section":
        solid_writer.add_solid_section("region_a", "MAT_A")
    elif fault == "empty_region":
        solid_writer._element_sets[0]["ids"] = []
    elif fault.endswith("_id"):
        invalid_ids = {
            "negative_id": -1, "out_of_range_id": 2,
            "fractional_id": 0.5, "boolean_id": True,
        }
        solid_writer._element_sets[0]["ids"] = [invalid_ids[fault]]
    elif fault == "duplicate_material":
        solid_writer.add_material("mat_a", 1.0, 0.0)
    elif fault == "duplicate_set":
        solid_writer.add_element_set("region_a", [1])
    elif fault == "reserved_set":
        solid_writer.add_element_set("eall", [0])
    elif fault == "reserved_beam_set":
        solid_writer.add_element_set("ebeam_b31", [0])
    elif fault == "invalid_name":
        solid_writer.add_element_set("BAD,ELSET=OTHER", [0])
    elif fault == "invalid_material":
        solid_writer.add_material("BAD\n*MATERIAL", 1.0, 0.0)

    path = tmp_path / "regions.inp"
    path.write_text("retained deck\n")
    with pytest.raises(ValueError, match=message):
        solid_writer.write(path)
    assert path.read_text() == "retained deck\n"


@pytest.mark.parametrize("solid_first", [False, True])
def test_mixed_blocks_use_global_element_indices(tmp_path, solid_first):
    """Hand count: one tet + one hex + one beam = three output rows.

    A beam-first ordering puts solids at zero-based indices {1, 2}, otherwise
    at {0, 1}; section sets must use that global order, never per-type indices.
    Node counts follow CalculiX 2.22 (2024), section 6.2 (C3D4, C3D8, B31).
    """
    solids = {
        "Tetrahedron 4": {"connectivity": np.array([[0, 1, 2, 3]])},
        "Hexahedron 8": {"connectivity": np.array([[0, 1, 2, 3, 4, 5, 6, 7]])},
    }
    beam = {"Line 2": {"connectivity": np.array([[0, 1]])}}
    writer = INPWriter(np.zeros((8, 3)),
                       {**solids, **beam} if solid_first else {**beam, **solids})
    offset = 0 if solid_first else 1
    writer.add_material("SOLID", 1.0, 0.0)
    writer.add_material("BEAM", 1.0, 0.0)
    writer.add_element_set("REGION", [offset, offset + 1])
    writer.add_solid_section("REGION", "SOLID")
    writer.add_beam_section("EBEAM_B31", "BEAM", "RECT", (1.0, 1.0))
    content = writer.write(tmp_path / "mixed.inp").read_text()
    assert f"*ELSET, ELSET=REGION\n{offset + 1}, {offset + 2}\n" in content
    assert "*SOLID SECTION, ELSET=REGION, MATERIAL=SOLID" in content
    assert "*BEAM SECTION, ELSET=EBEAM_B31, MATERIAL=BEAM, SECTION=RECT" in content
    element_rows = []
    in_elements = False
    for line in content.splitlines():
        if line.startswith("*"):
            in_elements = line.startswith("*ELEMENT,")
        elif in_elements:
            element_rows.append(int(line.split(",")[0]))
    assert element_rows == [1, 2, 3]


@pytest.mark.parametrize("region", ["MIXED", "EBEAM_B31"])
def test_solid_section_rejects_beam_members(tmp_path, region):
    """A solid section cannot own the beam at global index 1."""
    writer = INPWriter(np.zeros((4, 3)), {
        "Tetrahedron 4": {"connectivity": np.array([[0, 1, 2, 3]])},
        "Line 2": {"connectivity": np.array([[0, 1]])},
    })
    writer.add_material("MAT_A", 1.0, 0.0)
    writer.add_element_set("MIXED", [0, 1])
    writer.add_solid_section(region, "MAT_A")
    with pytest.raises(ValueError, match="non-solid"):
        writer.write(tmp_path / "mesh.inp")


def test_generated_eall_can_be_assigned_explicitly(tmp_path, solid_writer):
    """All solids may explicitly use one material even if another is defined."""
    solid_writer.add_material("UNUSED", 1.0, 0.0)
    solid_writer.add_material("CHOSEN", 1.0, 0.0)
    solid_writer.add_solid_section("eall", "chosen")
    content = solid_writer.write(tmp_path / "mesh.inp").read_text()
    assert "*SOLID SECTION, ELSET=EALL, MATERIAL=CHOSEN" in content


@pytest.mark.parametrize("gmsh_name, ccx_type, node_count", [
    ("Tetrahedron 4", "C3D4", 4),
    ("Tetrahedron 10", "C3D10", 10),
    ("Hexahedron 8", "C3D8", 8),
    ("Hexahedron 20", "C3D20", 20),
    ("Wedge 6", "C3D6", 6),
    ("Wedge 15", "C3D15", 15),
    ("Line 2", "B31", 2),
    ("Line 3", "B32", 3),
])
def test_accepted_subset_preserves_rows(tmp_path, gmsh_name, ccx_type, node_count):
    """Node counts: CalculiX 2.22 (2024), sections 6.2 and 7.49.

    Hand calculation: two identical connectivity rows produce IDs 1 and 2,
    each containing the consecutive node references 1 through node_count.
    This tests serialization, including unchanged beam order, not mesh quality.
    """
    row = np.arange(node_count)
    writer = INPWriter(np.zeros((node_count, 3)), {
        gmsh_name: {"connectivity": np.array([row, row])},
    })
    writer.add_material("MAT_A", 1.0, 0.0)
    content = writer.write(tmp_path / "mesh.inp").read_text()
    assert f"*ELEMENT, TYPE={ccx_type}," in content
    refs = ", ".join(str(n) for n in range(1, node_count + 1))
    assert f"\n1, {refs}\n2, {refs}\n" in content


@pytest.mark.parametrize("length, valid", [(80, True), (81, False)])
@pytest.mark.parametrize("kind", ["material", "element set"])
def test_name_length_limit(tmp_path, solid_writer, length, valid, kind):
    """The 80-character maximum is from CalculiX 2.22 (2024), section 7.

    Boundary calculation: 80 is accepted; 80 + 1 exceeds the limit.
    """
    name = "A" * length
    if kind == "material":
        solid_writer.add_material(name, 1.0, 0.0)
    else:
        solid_writer.add_material("MAT_A", 1.0, 0.0)
        solid_writer.add_element_set(name, [0, 1])
        solid_writer.add_solid_section(name, "MAT_A")
    if valid:
        assert solid_writer.write(tmp_path / "mesh.inp").exists()
    else:
        with pytest.raises(ValueError, match=f"Invalid {kind} name"):
            solid_writer.write(tmp_path / "mesh.inp")


def test_chain_rejects_unsupported_mesh_during_setup(tmp_path):
    """Setup must reject one unknown row before any solve is requested."""
    from digitalmodel.solvers.calculix.fem_chain import FEMChain

    chain = FEMChain(tmp_path)
    chain._nodes = np.zeros((3, 3))
    chain._elements = {"Triangle 3": {"connectivity": np.array([[0, 1, 2]])}}
    with pytest.raises(ValueError, match="Unsupported element type"):
        chain.setup_analysis({"name": "MAT_A", "E": 1.0, "nu": 0.0}, [], [])
    assert chain._inp_path is None
    assert not list(tmp_path.glob("*.inp"))
