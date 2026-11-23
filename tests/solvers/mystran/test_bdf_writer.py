#!/usr/bin/env python3
"""
ABOUTME: Tests for the MYSTRAN BDF writer — card formatting, continuation
lines, property/material handling, SPC/load emission. No solver needed.
"""

import numpy as np
import pytest

from digitalmodel.solvers.mystran.bdf_writer import (
    BDFWriter, _f, _split_card, _spc1_lines,
)


@pytest.fixture
def bar_mesh():
    nodes = np.array([[0.0, 0.0, 0.0], [0.5, 0.0, 0.0], [1.0, 0.0, 0.0]])
    elements = {
        "Line 2": {"connectivity": np.array([[0, 1], [1, 2]]), "dimension": 1}
    }
    return nodes, elements


@pytest.fixture
def hex_mesh():
    nodes = np.array([
        [0, 0, 0], [1, 0, 0], [1, 1, 0], [0, 1, 0],
        [0, 0, 1], [1, 0, 1], [1, 1, 1], [0, 1, 1],
    ], dtype=float)
    elements = {
        "Hexahedron 8": {
            "connectivity": np.array([[0, 1, 2, 3, 4, 5, 6, 7]]),
            "dimension": 3,
        }
    }
    return nodes, elements


@pytest.fixture
def tet_mesh():
    nodes = np.array([[0, 0, 0], [1, 0, 0], [0, 1, 0], [0, 0, 1]], dtype=float)
    elements = {
        "Tetrahedron 4": {"connectivity": np.array([[0, 1, 2, 3]]), "dimension": 3}
    }
    return nodes, elements


def _write(writer, tmp_path, name="job.bdf"):
    p = writer.write(tmp_path / name)
    return p.read_text().splitlines()


class TestDeckStructure:

    def test_exec_and_case_control(self, hex_mesh, tmp_path):
        w = BDFWriter(*hex_mesh, title="My title")
        w.add_material("STEEL", 210e9, 0.3)
        w.add_solid_section("Hexahedron 8", "STEEL")
        w.add_spc([0, 1, 2, 3], "123")
        w.add_force([4], 100.0)
        lines = _write(w, tmp_path)
        assert lines[0].startswith("ID JOB,")
        assert lines[1] == "SOL 101"
        assert "CEND" in lines
        assert "TITLE = My title" in lines
        assert "SPC = 1" in lines
        assert "LOAD = 1" in lines
        assert "DISP = ALL" in lines
        assert "STRESS = ALL" in lines
        assert "SPCFORCE = ALL" in lines
        assert "BEGIN BULK" in lines
        assert lines[-1] == "ENDDATA"
        assert lines.index("CEND") < lines.index("BEGIN BULK")

    def test_outputs_can_be_disabled(self, hex_mesh, tmp_path):
        w = BDFWriter(*hex_mesh)
        w.add_material("M", 1.0, 0.3)
        w.add_solid_section("Hexahedron 8", "M")
        w.set_outputs(disp=True, stress=False, spcforce=False)
        lines = _write(w, tmp_path)
        assert "DISP = ALL" in lines
        assert "STRESS = ALL" not in lines
        assert "SPCFORCE = ALL" not in lines

    def test_param_card(self, hex_mesh, tmp_path):
        w = BDFWriter(*hex_mesh)
        w.add_material("M", 1.0, 0.3)
        w.add_solid_section("Hexahedron 8", "M")
        w.add_param("POST", -1)
        lines = _write(w, tmp_path)
        assert "PARAM,POST,-1" in lines

    def test_grid_cards_are_1based(self, bar_mesh, tmp_path):
        w = BDFWriter(*bar_mesh)
        w.add_material("M", 1.0, 0.3)
        w.add_bar_section("Line 2", "M", 1.0, 1.0, 1.0, 1.0)
        grids = [l for l in _write(w, tmp_path) if l.startswith("GRID,")]
        assert grids == [
            "GRID,1,,0.0,0.0,0.0",
            "GRID,2,,0.5,0.0,0.0",
            "GRID,3,,1.0,0.0,0.0",
        ]


class TestElements:

    def test_cbar_with_orientation(self, bar_mesh, tmp_path):
        w = BDFWriter(*bar_mesh)
        w.add_material("M", 1.0, 0.3)
        w.add_bar_section("Line 2", "M", 1.0, 2.0, 3.0, 4.0, orientation=(0, 1, 0))
        bars = [l for l in _write(w, tmp_path) if l.startswith("CBAR,")]
        assert bars == ["CBAR,1,1,1,2,0.0,1.0,0.0", "CBAR,2,1,2,3,0.0,1.0,0.0"]

    def test_chexa_uses_continuation(self, hex_mesh, tmp_path):
        w = BDFWriter(*hex_mesh)
        w.add_material("M", 1.0, 0.3)
        w.add_solid_section("Hexahedron 8", "M")
        lines = _write(w, tmp_path)
        i = next(k for k, l in enumerate(lines) if l.startswith("CHEXA,"))
        assert lines[i] == "CHEXA,1,1,1,2,3,4,5,6,+H1"
        assert lines[i + 1] == "+H1,7,8"

    def test_ctetra_single_line(self, tet_mesh, tmp_path):
        w = BDFWriter(*tet_mesh)
        w.add_material("M", 1.0, 0.3)
        w.add_solid_section("Tetrahedron 4", "M", integration_order=2)
        lines = _write(w, tmp_path)
        assert "CTETRA,1,1,1,2,3,4" in lines
        assert "PSOLID,1,1,0,2" in lines

    def test_element_ids_sequential_across_blocks(self, tmp_path):
        nodes = np.array([[0, 0, 0], [1, 0, 0], [0, 1, 0], [0, 0, 1], [2, 0, 0]], float)
        elements = {
            "Tetrahedron 4": {"connectivity": np.array([[0, 1, 2, 3]]), "dimension": 3},
            "Line 2": {"connectivity": np.array([[1, 4]]), "dimension": 1},
        }
        w = BDFWriter(nodes, elements)
        w.add_material("M", 1.0, 0.3)
        w.add_solid_section("Tetrahedron 4", "M")
        w.add_bar_section("Line 2", "M", 1.0, 1.0, 1.0, 1.0)
        lines = _write(w, tmp_path)
        assert "CTETRA,1,1,1,2,3,4" in lines
        assert "CBAR,2,2,2,5,0.0,0.0,1.0" in lines

    def test_wrong_node_count_raises(self, tmp_path):
        nodes = np.zeros((4, 3))
        elements = {"Hexahedron 8": {"connectivity": np.array([[0, 1, 2, 3]]), "dimension": 3}}
        w = BDFWriter(nodes, elements)
        w.add_material("M", 1.0, 0.3)
        w.add_solid_section("Hexahedron 8", "M")
        with pytest.raises(ValueError, match="expected 8 nodes"):
            w.write(tmp_path / "x.bdf")

    def test_split_card_helper_20_nodes(self):
        gids = [str(i) for i in range(1, 21)]
        lines = _split_card("CHEXA", 7, 2, gids)
        assert lines[0] == "CHEXA,7,2,1,2,3,4,5,6,+H7"
        assert lines[1] == "+H7,7,8,9,10,11,12,13,14,+H7"
        assert lines[2] == "+H7,15,16,17,18,19,20"


class TestPropertiesAndMaterials:

    def test_mat1_card(self, hex_mesh, tmp_path):
        w = BDFWriter(*hex_mesh)
        w.add_material("STEEL", 210e9, 0.3, rho=7850.0)
        w.add_solid_section("Hexahedron 8", "STEEL")
        lines = _write(w, tmp_path)
        assert "MAT1,1,2.1E+11,,0.3,7850.0" in lines

    def test_pbar_card(self, bar_mesh, tmp_path):
        w = BDFWriter(*bar_mesh)
        w.add_material("M", 1.0, 0.3)
        w.add_bar_section("Line 2", "M", 0.01, 8.3333333e-6, 8.3333333e-6, 1.4e-5)
        lines = _write(w, tmp_path)
        assert "PBAR,1,1,0.01,8.3333333E-06,8.3333333E-06,1.4E-05" in lines

    def test_pbar_stress_points_continuation(self, bar_mesh, tmp_path):
        w = BDFWriter(*bar_mesh)
        w.add_material("M", 1.0, 0.3)
        w.add_bar_section(
            "Line 2", "M", 1.0, 1.0, 1.0, 1.0,
            stress_points=[(0.05, 0.05), (-0.05, 0.05)],
        )
        lines = _write(w, tmp_path)
        assert "PBAR,1,1,1.0,1.0,1.0,1.0,,,+P1" in lines
        assert "+P1,0.05,0.05,-0.05,0.05,0.0,0.0,0.0,0.0" in lines

    def test_pshell_card(self, tmp_path):
        nodes = np.array([[0, 0, 0], [1, 0, 0], [1, 1, 0], [0, 1, 0]], float)
        elements = {"Quadrilateral 4": {"connectivity": np.array([[0, 1, 2, 3]]), "dimension": 2}}
        w = BDFWriter(nodes, elements)
        w.add_material("M", 1.0, 0.3)
        w.add_shell_section("Quadrilateral 4", "M", 0.01)
        lines = _write(w, tmp_path)
        assert "CQUAD4,1,1,1,2,3,4" in lines
        assert "PSHELL,1,1,0.01,1,,1" in lines

    def test_section_type_mismatch_raises(self, hex_mesh):
        w = BDFWriter(*hex_mesh)
        w.add_material("M", 1.0, 0.3)
        with pytest.raises(ValueError, match="not a bar"):
            w.add_bar_section("Hexahedron 8", "M", 1, 1, 1, 1)
        with pytest.raises(ValueError, match="not a shell"):
            w.add_shell_section("Hexahedron 8", "M", 0.1)

    def test_unknown_material_raises(self, hex_mesh):
        w = BDFWriter(*hex_mesh)
        with pytest.raises(ValueError, match="Unknown material"):
            w.add_solid_section("Hexahedron 8", "NOPE")

    def test_duplicate_property_raises(self, hex_mesh):
        w = BDFWriter(*hex_mesh)
        w.add_material("M", 1.0, 0.3)
        w.add_solid_section("Hexahedron 8", "M")
        with pytest.raises(ValueError, match="already defined"):
            w.add_solid_section("Hexahedron 8", "M")

    def test_missing_property_at_write_raises(self, hex_mesh, tmp_path):
        w = BDFWriter(*hex_mesh)
        w.add_material("M", 1.0, 0.3)
        with pytest.raises(ValueError, match="No section/property"):
            w.write(tmp_path / "x.bdf")

    def test_unsupported_element_type_raises(self, tmp_path):
        nodes = np.zeros((3, 3))
        elements = {"Prism 6": {"connectivity": np.array([[0, 1, 2, 0, 1, 2]]), "dimension": 3}}
        w = BDFWriter(nodes, elements)
        w.add_material("M", 1.0, 0.3)
        with pytest.raises(ValueError, match="Unsupported element type"):
            w.add_solid_section("Prism 6", "M")


class TestConstraintsAndLoads:

    def _hex_writer(self, hex_mesh):
        w = BDFWriter(*hex_mesh)
        w.add_material("M", 1.0, 0.3)
        w.add_solid_section("Hexahedron 8", "M")
        return w

    def test_spc1_chunks_of_six(self, hex_mesh, tmp_path):
        w = self._hex_writer(hex_mesh)
        w.add_spc(range(8), "123")
        lines = _write(w, tmp_path)
        assert "SPC1,1,123,1,2,3,4,5,6" in lines
        assert "SPC1,1,123,7,8" in lines

    def test_spc_dofs_sorted_and_deduped(self, hex_mesh, tmp_path):
        w = self._hex_writer(hex_mesh)
        w.add_spc([0], "321123")
        lines = _write(w, tmp_path)
        assert "SPC1,1,123,1" in lines

    def test_enforced_displacement_uses_spc(self, hex_mesh, tmp_path):
        w = self._hex_writer(hex_mesh)
        w.add_spc([4, 5], "3", value=0.001)
        lines = _write(w, tmp_path)
        assert "SPC,1,5,3,0.001" in lines
        assert "SPC,1,6,3,0.001" in lines

    def test_bad_dof_string_raises(self, hex_mesh):
        w = self._hex_writer(hex_mesh)
        with pytest.raises(ValueError, match="Invalid DOF"):
            w.add_spc([0], "127")
        with pytest.raises(ValueError, match="Invalid DOF"):
            w.add_spc([0], "")

    def test_force_and_moment_cards(self, hex_mesh, tmp_path):
        w = self._hex_writer(hex_mesh)
        w.add_force([4, 5], 250.0, direction=(0, 0, 1))
        w.add_moment([6], 10.0, direction=(1, 0, 0))
        lines = _write(w, tmp_path)
        assert "FORCE,1,5,,250.0,0.0,0.0,1.0" in lines
        assert "FORCE,1,6,,250.0,0.0,0.0,1.0" in lines
        assert "MOMENT,1,7,,10.0,1.0,0.0,0.0" in lines

    def test_no_loads_means_no_load_case_control(self, hex_mesh, tmp_path):
        w = self._hex_writer(hex_mesh)
        lines = _write(w, tmp_path)
        assert "LOAD = 1" not in lines
        assert "SPC = 1" not in lines

    def test_node_index_out_of_range_raises(self, hex_mesh, tmp_path):
        w = self._hex_writer(hex_mesh)
        w.add_force([8], 1.0)
        with pytest.raises(ValueError, match="out of range"):
            w.write(tmp_path / "x.bdf")

    def test_spc1_helper(self):
        assert _spc1_lines("123456", [0]) == ["SPC1,1,123456,1"]


class TestFloatFormat:

    @pytest.mark.parametrize("value,expected", [
        (0.0, "0.0"),
        (-0.0, "0.0"),
        (1.0, "1.0"),
        (0.3, "0.3"),
        (210e9, "2.1E+11"),
        (8.3333333e-6, "8.3333333E-06"),
        (-1000.0, "-1000.0"),
        (0.25, "0.25"),
    ])
    def test_format(self, value, expected):
        assert _f(value) == expected

    def test_format_round_trips(self):
        for v in (1.0 / 3.0, 210e9, 8.3333e-6, -2.5e-3, 123456.789):
            assert abs(float(_f(v)) - v) / abs(v) < 1e-7
