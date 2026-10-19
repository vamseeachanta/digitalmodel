# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent synthetic STL/OBJ fixtures, without mesh-library writers."""

import struct

import pytest

from digitalmodel.cad import CadReadError, UnitsError, read_cad

ASCII_STL = """solid synthetic
facet normal 0 0 1
  outer loop
    vertex 0 0 0
    vertex 1000 0 0
    vertex 0 2000 0
  endloop
endfacet
endsolid synthetic
"""


def test_ascii_stl_requires_units(tmp_path):
    """Hand: (1000,0,0) mm -> (1,0,0) m; (0,2000,0) -> (0,2,0).

    NIST SI Prefixes, milli row, updated 2025-08-13. STL facets retain
    the input winding, per meshio 5.3.5 STL reader interface.
    """
    path = tmp_path / "triangle.stl"
    path.write_text(ASCII_STL, encoding="ascii")
    with pytest.raises(UnitsError, match="State units"):
        read_cad(path)
    result = read_cad(path, units="mm")
    triangle = tuple(result.points[i] for i in result.entities[0].indices)
    assert triangle == ((0, 0, 0), (1, 0, 0), (0, 2, 0))
    assert result.format == "stl"
    assert result.unit_decision.basis == "caller"


def test_binary_stl_with_solid_header(tmp_path):
    """Hand: 2 in = 2*(254/10000) = 0.0508 m.

    NIST SP 811 (2008), Appendix B.8, inch row. Synthetic STL: an 80-byte
    header, one uint32 facet count, twelve float32 values and a uint16
    attribute count, meshio 5.3.5 STL reader layout. Header text carries
    no trusted units, including when it starts with the word solid.
    """
    path = tmp_path / "triangle.STL"
    path.write_bytes(
        b"solid units=mm".ljust(80, b" ")
        + struct.pack("<I", 1)
        + struct.pack("<12fH", 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0)
    )
    result = read_cad(path, units="in")
    triangle = tuple(result.points[i] for i in result.entities[0].indices)
    assert triangle == ((0, 0, 0), (0.0508, 0, 0), (0, 0.0508, 0))


def test_obj_negative_indices_and_quad(tmp_path):
    """Hand: 2 ft = 6096/10000 m, 3 ft = 9144/10000 m.

    NIST SP 811 (2008), Appendix B.8, foot row. In OBJ a face with indices
    -4,-3,-2,-1 addresses the four most recent vertices in original order;
    meshio 5.3.5 OBJ reader handles indexing without triangulation.
    """
    path = tmp_path / "quad.obj"
    path.write_text(
        "# units=mm is an untrusted comment\n"
        "v 0 0 0\nv 2 0 0\nv 2 3 0\nv 0 3 0\nf -4 -3 -2 -1\n",
        encoding="ascii",
    )
    with pytest.raises(UnitsError):
        read_cad(path)
    result = read_cad(path, units="ft")
    assert result.format == "obj"
    assert result.points[2] == pytest.approx((0.6096, 0.9144, 0))
    assert result.entities[0].kind == "polygon"
    assert result.entities[0].indices == (0, 1, 2, 3)


def test_obj_triangle(tmp_path):
    """Hand: coordinates declared in metres are unchanged."""
    path = tmp_path / "triangle.obj"
    path.write_text("v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 3\n", encoding="ascii")
    result = read_cad(path, units="m")
    assert result.points == ((0, 0, 0), (1, 0, 0), (0, 1, 0))
    assert result.entities[0].kind == "triangle"


@pytest.mark.parametrize("suffix", ["stl", "obj"])
def test_bad_mesh_fails_with_package_error(tmp_path, suffix):
    path = tmp_path / f"broken.{suffix}"
    path.write_text("invalid", encoding="ascii")
    with pytest.raises(CadReadError):
        read_cad(path, units="m")


def test_nonfinite_mesh_fails(tmp_path):
    path = tmp_path / "nonfinite.obj"
    path.write_text("v nan 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 3\n", encoding="ascii")
    with pytest.raises(CadReadError, match="finite"):
        read_cad(path, units="m")


@pytest.mark.parametrize(
    "record", ["l 1 2", "p 1", "curv 0 1 1 2", "surf 0 1 0 1 1 2 3"]
)
def test_obj_unread_geometry_cannot_disappear(tmp_path, record):
    path = tmp_path / "unsupported.obj"
    path.write_text(
        "v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 3\n" + record + "\n",
        encoding="ascii",
    )
    with pytest.raises(CadReadError, match="OBJ.*unsupported"):
        read_cad(path, units="m")


def test_no_source_writes(tmp_path):
    path = tmp_path / "triangle.stl"
    path.write_text(ASCII_STL, encoding="ascii")
    before = path.read_bytes()
    read_cad(path, units="mm")
    assert path.read_bytes() == before
    assert sorted(p.name for p in tmp_path.iterdir()) == ["triangle.stl"]


@pytest.mark.parametrize("face", ["0 2 3", "1 2 4", "-4 -2 -1", "1 2"])
def test_invalid_obj_connectivity(tmp_path, face):
    path = tmp_path / "invalid.obj"
    path.write_text("v 0 0 0\nv 1 0 0\nv 0 1 0\nf " + face + "\n", encoding="ascii")
    with pytest.raises(CadReadError):
        read_cad(path, units="m")


def test_negative_obj_indices_are_relative_at_the_face(tmp_path):
    """Hand: -3,-2,-1 reference vertices 0,1,2 before vertex 3 is declared.

    Library of Congress OBJ format description, 2025-02-18, Notes,
    Potential interoperability issues; no metric conversion is applied.
    """
    path = tmp_path / "interleaved.obj"
    path.write_text(
        "v 0 0 0\nv 1 0 0\nv 0 1 0\nf -3 -2 -1\nv 9 9 9\n",
        encoding="ascii",
    )
    result = read_cad(path, units="m")
    assert result.entities[0].indices == (0, 1, 2)


def test_obj_independent_appearance_indexing_is_ignored(tmp_path):
    """Hand: all face positions address vertices 0,1,2 despite one shared normal.

    Geometry-only contract: position indices are one-based in OBJ (Library of
    Congress OBJ format description, 2025-02-18, Description).
    """
    path = tmp_path / "appearance.obj"
    path.write_text(
        "v 0 0 0\nv 1 0 0\nv 0 1 0\nvt 0 0\nvn 0 0 1\n"
        "f 1/1/1 2/1/1 3/1/1 # retained geometry\n",
        encoding="ascii",
    )
    result = read_cad(path, units="m")
    assert result.entities[0].indices == (0, 1, 2)
