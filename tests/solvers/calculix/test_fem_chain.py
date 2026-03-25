#!/usr/bin/env python3
"""
ABOUTME: Tests for CalculiX FEM preprocessing chain — INP writer, result parser,
and end-to-end FEM chain. Tests run WITHOUT CalculiX installed (except integration).
"""

import shutil
import tempfile
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.solvers.calculix.inp_writer import INPWriter
from digitalmodel.solvers.calculix.result_parser import CalculiXResultParser
from digitalmodel.solvers.calculix.fem_chain import FEMChain, is_calculix_available

try:
    import gmsh  # noqa: F401
    GMSH_AVAILABLE = True
except (ImportError, OSError):
    GMSH_AVAILABLE = False


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def simple_mesh():
    """Single tetrahedron mesh for basic tests."""
    nodes = np.array([
        [0.0, 0.0, 0.0],
        [1.0, 0.0, 0.0],
        [0.0, 1.0, 0.0],
        [0.0, 0.0, 1.0],
    ])
    elements = {
        "Tetrahedron 4": {
            "connectivity": np.array([[0, 1, 2, 3]]),
            "dimension": 3,
        }
    }
    return nodes, elements


@pytest.fixture
def two_tet_mesh():
    """Two tetrahedra sharing a face — tests multi-element output."""
    nodes = np.array([
        [0.0, 0.0, 0.0],
        [1.0, 0.0, 0.0],
        [0.0, 1.0, 0.0],
        [0.0, 0.0, 1.0],
        [1.0, 1.0, 1.0],
    ])
    elements = {
        "Tetrahedron 4": {
            "connectivity": np.array([[0, 1, 2, 3], [1, 2, 3, 4]]),
            "dimension": 3,
        }
    }
    return nodes, elements


@pytest.fixture
def tmp_dir():
    """Temporary directory cleaned up after test."""
    d = tempfile.mkdtemp()
    yield Path(d)
    shutil.rmtree(d, ignore_errors=True)


@pytest.fixture
def sample_frd_content():
    """Minimal ASCII FRD content with displacements and stresses.

    Uses CalculiX fixed-width format: 3-char flag, 10-char node ID,
    then 12-char fields for each value.
    """
    return (
        "    1C\n"
        "    2C\n"
        " -1\n"
        " -2         1 0.00000E+00 0.00000E+00 0.00000E+00\n"
        " -2         2 1.00000E+00 0.00000E+00 0.00000E+00\n"
        " -2         3 5.00000E-01 5.00000E-01 0.00000E+00\n"
        " -3\n"
        " 100CL  101        1           1PSTEP                         1           1\n"
        " -4  DISP        4    1\n"
        " -5  D1          1    2    1    0\n"
        " -5  D2          1    2    2    0\n"
        " -5  D3          1    2    3    0\n"
        " -5  ALL         1    2    0    0    1ALL\n"
        " -1         1 1.00000E-03 0.00000E+00 0.00000E+00\n"
        " -1         2 2.00000E-03 5.00000E-04 0.00000E+00\n"
        " -1         3 1.50000E-03 3.00000E-04 0.00000E+00\n"
        " -3\n"
        " -4  STRESS      6    1\n"
        " -5  SXX         1    4    1    1\n"
        " -5  SYY         1    4    2    2\n"
        " -5  SZZ         1    4    3    3\n"
        " -5  SXY         1    4    1    2\n"
        " -5  SYZ         1    4    2    3\n"
        " -5  SZX         1    4    3    1\n"
        " -1         1 1.00000E+02 0.00000E+00 0.00000E+00 0.00000E+00 0.00000E+00 0.00000E+00\n"
        " -1         2 3.00000E+02 5.00000E+01 0.00000E+00 2.50000E+01 0.00000E+00 0.00000E+00\n"
        " -1         3 1.50000E+02 1.00000E+01 0.00000E+00 5.00000E+00 0.00000E+00 0.00000E+00\n"
        " -3\n"
        "9999\n"
    )


@pytest.fixture
def sample_dat_content():
    """Minimal .dat content with reaction forces."""
    return (
        "\n"
        " statistics for all nodes of set N_FIX:\n"
        "   total force (fx,fy,fz)  for set N_FIX and target step 1\n"
        "     -1.00000E+04   0.00000E+00   0.00000E+00\n"
        "\n"
    )


# ===========================================================================
# INPWriter tests
# ===========================================================================

class TestINPWriter:
    """Tests for CalculiX INP file generation."""

    def test_init_stores_mesh(self, simple_mesh):
        nodes, elements = simple_mesh
        writer = INPWriter(nodes, elements)
        assert writer.nodes is nodes
        assert writer.elements is elements

    def test_write_creates_file(self, simple_mesh, tmp_dir):
        nodes, elements = simple_mesh
        writer = INPWriter(nodes, elements)
        writer.add_material("STEEL", 210000.0, 0.3)
        out = writer.write(tmp_dir / "test.inp")
        assert out.exists()
        assert out.suffix == ".inp"

    def test_heading_section(self, simple_mesh, tmp_dir):
        nodes, elements = simple_mesh
        writer = INPWriter(nodes, elements)
        writer.add_material("STEEL", 210000.0, 0.3)
        out = writer.write(tmp_dir / "test.inp")
        content = out.read_text()
        assert "*HEADING" in content

    def test_node_section_1based(self, simple_mesh, tmp_dir):
        """Node IDs in INP must be 1-based."""
        nodes, elements = simple_mesh
        writer = INPWriter(nodes, elements)
        writer.add_material("STEEL", 210000.0, 0.3)
        out = writer.write(tmp_dir / "test.inp")
        content = out.read_text()
        assert "*NODE" in content
        lines = content.split("\n")
        node_lines = [
            l for l in lines
            if l.strip() and not l.startswith("*") and l.split(",")[0].strip().isdigit()
        ]
        # Filter to lines in the NODE section
        in_node = False
        node_ids = []
        for line in lines:
            if line.strip().startswith("*NODE"):
                in_node = True
                continue
            if in_node and line.strip().startswith("*"):
                break
            if in_node and line.strip():
                node_ids.append(int(line.split(",")[0].strip()))
        assert node_ids[0] == 1, "First node ID must be 1 (1-based)"
        assert len(node_ids) == 4

    def test_element_section_c3d4(self, simple_mesh, tmp_dir):
        """Tetrahedron 4 maps to C3D4 element type."""
        nodes, elements = simple_mesh
        writer = INPWriter(nodes, elements)
        writer.add_material("STEEL", 210000.0, 0.3)
        out = writer.write(tmp_dir / "test.inp")
        content = out.read_text()
        assert "TYPE=C3D4" in content

    def test_element_connectivity_1based(self, simple_mesh, tmp_dir):
        """Element node references must be 1-based."""
        nodes, elements = simple_mesh
        writer = INPWriter(nodes, elements)
        writer.add_material("STEEL", 210000.0, 0.3)
        out = writer.write(tmp_dir / "test.inp")
        content = out.read_text()
        lines = content.split("\n")
        in_elem = False
        for line in lines:
            if "TYPE=C3D4" in line:
                in_elem = True
                continue
            if in_elem and line.strip().startswith("*"):
                break
            if in_elem and line.strip():
                parts = [int(x.strip()) for x in line.split(",")]
                # Element ID and all node refs must be >= 1
                for val in parts:
                    assert val >= 1, f"0-based index found in element: {line}"

    def test_material_section(self, simple_mesh, tmp_dir):
        nodes, elements = simple_mesh
        writer = INPWriter(nodes, elements)
        writer.add_material("STEEL", 210000.0, 0.3)
        out = writer.write(tmp_dir / "test.inp")
        content = out.read_text()
        assert "*MATERIAL, NAME=STEEL" in content
        assert "*ELASTIC" in content
        assert "210000.0" in content
        assert "0.3" in content

    def test_solid_section(self, simple_mesh, tmp_dir):
        nodes, elements = simple_mesh
        writer = INPWriter(nodes, elements)
        writer.add_material("STEEL", 210000.0, 0.3)
        out = writer.write(tmp_dir / "test.inp")
        content = out.read_text()
        assert "*SOLID SECTION" in content
        assert "MATERIAL=STEEL" in content

    def test_node_set(self, simple_mesh, tmp_dir):
        nodes, elements = simple_mesh
        writer = INPWriter(nodes, elements)
        writer.add_material("STEEL", 210000.0, 0.3)
        writer.add_node_set("FIX", [0, 1])
        out = writer.write(tmp_dir / "test.inp")
        content = out.read_text()
        assert "*NSET, NSET=FIX" in content
        # Node set IDs must also be 1-based
        lines = content.split("\n")
        in_nset = False
        for line in lines:
            if "NSET=FIX" in line:
                in_nset = True
                continue
            if in_nset and line.strip().startswith("*"):
                break
            if in_nset and line.strip():
                ids = [int(x.strip()) for x in line.split(",") if x.strip()]
                for nid in ids:
                    assert nid >= 1

    def test_element_set(self, simple_mesh, tmp_dir):
        nodes, elements = simple_mesh
        writer = INPWriter(nodes, elements)
        writer.add_material("STEEL", 210000.0, 0.3)
        writer.add_element_set("LOAD_FACE", [0])
        out = writer.write(tmp_dir / "test.inp")
        content = out.read_text()
        assert "*ELSET, ELSET=LOAD_FACE" in content

    def test_boundary_condition(self, simple_mesh, tmp_dir):
        nodes, elements = simple_mesh
        writer = INPWriter(nodes, elements)
        writer.add_material("STEEL", 210000.0, 0.3)
        writer.add_node_set("FIX", [0])
        writer.add_step()
        writer.add_boundary_condition("FIX", 1, 0.0)
        writer.add_boundary_condition("FIX", 2, 0.0)
        writer.add_boundary_condition("FIX", 3, 0.0)
        out = writer.write(tmp_dir / "test.inp")
        content = out.read_text()
        assert "*BOUNDARY" in content
        assert "*STEP" in content
        assert "*END STEP" in content

    def test_concentrated_load(self, simple_mesh, tmp_dir):
        nodes, elements = simple_mesh
        writer = INPWriter(nodes, elements)
        writer.add_material("STEEL", 210000.0, 0.3)
        writer.add_step()
        writer.add_load("LOAD_SET", "cload", 1000.0, direction=(1, 0, 0))
        out = writer.write(tmp_dir / "test.inp")
        content = out.read_text()
        assert "*CLOAD" in content

    def test_output_requests(self, simple_mesh, tmp_dir):
        nodes, elements = simple_mesh
        writer = INPWriter(nodes, elements)
        writer.add_material("STEEL", 210000.0, 0.3)
        writer.add_step()
        out = writer.write(tmp_dir / "test.inp")
        content = out.read_text()
        assert "*NODE FILE" in content
        assert "*EL FILE" in content

    def test_multi_element_mesh(self, two_tet_mesh, tmp_dir):
        nodes, elements = two_tet_mesh
        writer = INPWriter(nodes, elements)
        writer.add_material("STEEL", 210000.0, 0.3)
        out = writer.write(tmp_dir / "test.inp")
        content = out.read_text()
        # Should have 5 nodes and 2 elements
        lines = content.split("\n")
        in_elem = False
        elem_count = 0
        for line in lines:
            if "TYPE=C3D4" in line:
                in_elem = True
                continue
            if in_elem and line.strip().startswith("*"):
                break
            if in_elem and line.strip():
                elem_count += 1
        assert elem_count == 2

    def test_hex_element_type(self, tmp_dir):
        """Hexahedron 8 maps to C3D8."""
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
        writer = INPWriter(nodes, elements)
        writer.add_material("STEEL", 210000.0, 0.3)
        out = writer.write(tmp_dir / "hex.inp")
        content = out.read_text()
        assert "TYPE=C3D8" in content

    def test_tet10_element_type(self, tmp_dir):
        """Tetrahedron 10 maps to C3D10."""
        nodes = np.zeros((10, 3))
        elements = {
            "Tetrahedron 10": {
                "connectivity": np.array([[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]]),
                "dimension": 3,
            }
        }
        writer = INPWriter(nodes, elements)
        writer.add_material("STEEL", 210000.0, 0.3)
        out = writer.write(tmp_dir / "tet10.inp")
        content = out.read_text()
        assert "TYPE=C3D10" in content

    def test_b31_element_type(self, tmp_dir):
        """Line 2 maps to B31 beam element."""
        nodes = np.array([[0, 0, 0], [1, 0, 0], [2, 0, 0]], dtype=float)
        elements = {
            "Line 2": {
                "connectivity": np.array([[0, 1], [1, 2]]),
                "dimension": 1,
            }
        }
        writer = INPWriter(nodes, elements)
        writer.add_material("STEEL", 210000.0, 0.3)
        writer.add_beam_section(
            "EBEAM_B31", "STEEL", "RECT", (0.1, 0.1),
        )
        out = writer.write(tmp_dir / "b31.inp")
        content = out.read_text()
        assert "TYPE=B31" in content
        assert "ELSET=EBEAM_B31" in content

    def test_b32_element_type(self, tmp_dir):
        """Line 3 maps to B32 beam element."""
        nodes = np.array(
            [[0, 0, 0], [0.5, 0, 0], [1, 0, 0]], dtype=float,
        )
        elements = {
            "Line 3": {
                "connectivity": np.array([[0, 1, 2]]),
                "dimension": 1,
            }
        }
        writer = INPWriter(nodes, elements)
        writer.add_material("STEEL", 210000.0, 0.3)
        writer.add_beam_section(
            "EBEAM_B32", "STEEL", "CIRC", (0.05,),
        )
        out = writer.write(tmp_dir / "b32.inp")
        content = out.read_text()
        assert "TYPE=B32" in content

    def test_beam_section_output(self, tmp_dir):
        """Beam section writes correct keywords, dimensions, and direction."""
        nodes = np.array([[0, 0, 0], [1, 0, 0]], dtype=float)
        elements = {
            "Line 2": {
                "connectivity": np.array([[0, 1]]),
                "dimension": 1,
            }
        }
        writer = INPWriter(nodes, elements)
        writer.add_material("STEEL", 210000.0, 0.3)
        writer.add_beam_section(
            "EBEAM_B31", "STEEL", "RECT", (0.1, 0.2),
            direction=(0.0, 0.0, 1.0),
        )
        out = writer.write(tmp_dir / "beam_section.inp")
        content = out.read_text()
        assert "*BEAM SECTION, ELSET=EBEAM_B31, MATERIAL=STEEL, SECTION=RECT" in content
        assert "0.1, 0.2" in content
        assert "0.0, 0.0, 1.0" in content
        # No solid section for beam-only mesh
        assert "*SOLID SECTION" not in content

    def test_mixed_solid_and_beam(self, tmp_dir):
        """Mixed mesh writes both *SOLID SECTION and *BEAM SECTION."""
        nodes = np.array([
            [0, 0, 0], [1, 0, 0], [0, 1, 0], [0, 0, 1],
            [2, 0, 0],
        ], dtype=float)
        elements = {
            "Tetrahedron 4": {
                "connectivity": np.array([[0, 1, 2, 3]]),
                "dimension": 3,
            },
            "Line 2": {
                "connectivity": np.array([[1, 4]]),
                "dimension": 1,
            },
        }
        writer = INPWriter(nodes, elements)
        writer.add_material("STEEL", 210000.0, 0.3)
        writer.add_beam_section(
            "EBEAM_B31", "STEEL", "RECT", (0.05, 0.05),
        )
        out = writer.write(tmp_dir / "mixed.inp")
        content = out.read_text()
        assert "*SOLID SECTION, ELSET=EALL, MATERIAL=STEEL" in content
        assert "*BEAM SECTION, ELSET=EBEAM_B31, MATERIAL=STEEL, SECTION=RECT" in content
        assert "TYPE=C3D4" in content
        assert "TYPE=B31" in content

    def test_beam_element_connectivity_1based(self, tmp_dir):
        """Beam element node references must be 1-based."""
        nodes = np.array([[0, 0, 0], [1, 0, 0], [2, 0, 0]], dtype=float)
        elements = {
            "Line 2": {
                "connectivity": np.array([[0, 1], [1, 2]]),
                "dimension": 1,
            }
        }
        writer = INPWriter(nodes, elements)
        writer.add_material("STEEL", 210000.0, 0.3)
        writer.add_beam_section("EBEAM_B31", "STEEL", "RECT", (0.1, 0.1))
        out = writer.write(tmp_dir / "beam_1based.inp")
        content = out.read_text()
        lines = content.split("\n")
        in_elem = False
        for line in lines:
            if "TYPE=B31" in line:
                in_elem = True
                continue
            if in_elem and line.strip().startswith("*"):
                break
            if in_elem and line.strip():
                parts = [int(x.strip()) for x in line.split(",")]
                for val in parts:
                    assert val >= 1, f"0-based index in beam element: {line}"


# ===========================================================================
# CalculiXResultParser tests
# ===========================================================================

class TestResultParser:
    """Tests for CalculiX result file parsing."""

    def test_parse_frd_displacements(self, tmp_dir, sample_frd_content):
        job = "test_job"
        frd_path = tmp_dir / f"{job}.frd"
        frd_path.write_text(sample_frd_content)
        parser = CalculiXResultParser(job, tmp_dir)
        result = parser.parse_frd_file()
        assert "displacements" in result
        assert len(result["displacements"]) == 3

    def test_parse_frd_stresses(self, tmp_dir, sample_frd_content):
        job = "test_job"
        frd_path = tmp_dir / f"{job}.frd"
        frd_path.write_text(sample_frd_content)
        parser = CalculiXResultParser(job, tmp_dir)
        result = parser.parse_frd_file()
        assert "stresses" in result
        assert len(result["stresses"]) == 3

    def test_max_von_mises(self, tmp_dir, sample_frd_content):
        job = "test_job"
        frd_path = tmp_dir / f"{job}.frd"
        frd_path.write_text(sample_frd_content)
        parser = CalculiXResultParser(job, tmp_dir)
        parser.parse_frd_file()
        max_vm = parser.get_max_von_mises()
        # Node 2: sxx=300, syy=50, sxy=25
        # VM = sqrt(sxx^2 + syy^2 - sxx*syy + 3*sxy^2)
        # = sqrt(90000 + 2500 - 15000 + 1875) = sqrt(79375) ~ 281.74
        assert max_vm > 250.0
        assert max_vm < 300.0

    def test_max_displacement(self, tmp_dir, sample_frd_content):
        job = "test_job"
        frd_path = tmp_dir / f"{job}.frd"
        frd_path.write_text(sample_frd_content)
        parser = CalculiXResultParser(job, tmp_dir)
        parser.parse_frd_file()
        max_d = parser.get_max_displacement()
        # Node 2 has ALL=2.06155E-03
        assert abs(max_d - 2.06155e-3) < 1e-6

    def test_stress_at_node(self, tmp_dir, sample_frd_content):
        job = "test_job"
        frd_path = tmp_dir / f"{job}.frd"
        frd_path.write_text(sample_frd_content)
        parser = CalculiXResultParser(job, tmp_dir)
        parser.parse_frd_file()
        stress = parser.get_stress_at_node(1)
        assert abs(stress["sxx"] - 100.0) < 1e-6
        assert abs(stress["syy"]) < 1e-6

    def test_parse_dat_reactions(self, tmp_dir, sample_dat_content):
        job = "test_job"
        dat_path = tmp_dir / f"{job}.dat"
        dat_path.write_text(sample_dat_content)
        parser = CalculiXResultParser(job, tmp_dir)
        result = parser.parse_dat_file()
        assert "reactions" in result

    def test_missing_frd_raises(self, tmp_dir):
        parser = CalculiXResultParser("nonexistent", tmp_dir)
        with pytest.raises(FileNotFoundError):
            parser.parse_frd_file()

    def test_missing_dat_raises(self, tmp_dir):
        parser = CalculiXResultParser("nonexistent", tmp_dir)
        with pytest.raises(FileNotFoundError):
            parser.parse_dat_file()


# ===========================================================================
# FEMChain tests
# ===========================================================================

class TestCantileverAnalytical:
    """Analytical verification of cantilever beam deflection (no solver needed).

    Reference: Euler-Bernoulli beam theory, any structural mechanics text.
    E.g. Gere & Timoshenko, "Mechanics of Materials", Table B-1.
    Formula: δ = PL³ / 3EI for a cantilever with tip point load.
    """

    def test_cantilever_deflection_formula(self):
        """Verify PL^3/3EI against known hand-calc values."""
        # Case: Steel cantilever, L=1m, 100mm square, P=1000N
        L, b, h = 1.0, 0.1, 0.1
        E = 210e9  # Pa
        P = 1000.0  # N
        I = b * h**3 / 12.0  # 8.3333e-6 m^4
        delta = P * L**3 / (3.0 * E * I)
        # Hand calc: 1000 / (3 * 210e9 * 8.3333e-6) = 1.9048e-4 m
        assert abs(delta - 1.9048e-4) / 1.9048e-4 < 0.001

    def test_cantilever_deflection_scales_with_length_cubed(self):
        """δ scales as L³ — doubling length gives 8× deflection."""
        E, P, I = 210e9, 1000.0, 8.3333e-6
        delta_1 = P * 1.0**3 / (3.0 * E * I)
        delta_2 = P * 2.0**3 / (3.0 * E * I)
        assert abs(delta_2 / delta_1 - 8.0) < 1e-10

    def test_cantilever_deflection_scales_with_inertia(self):
        """Doubling section height halves I⁻¹ → δ/8 (h³ in I)."""
        L, E, P, b = 1.0, 210e9, 1000.0, 0.1
        I_1 = b * 0.1**3 / 12.0
        I_2 = b * 0.2**3 / 12.0
        delta_1 = P * L**3 / (3.0 * E * I_1)
        delta_2 = P * L**3 / (3.0 * E * I_2)
        assert abs(delta_1 / delta_2 - 8.0) < 1e-10


class TestFEMChain:
    """Tests for end-to-end FEM chain orchestration."""

    def test_is_calculix_available_returns_bool(self):
        result = is_calculix_available()
        assert isinstance(result, bool)

    def test_init_creates_work_dir(self):
        chain = FEMChain()
        assert chain.work_dir.exists()

    def test_init_with_custom_dir(self, tmp_dir):
        chain = FEMChain(work_dir=tmp_dir)
        assert chain.work_dir == tmp_dir

    @pytest.mark.skipif(
        not (GMSH_AVAILABLE and is_calculix_available()),
        reason="Requires gmsh and CalculiX (ccx)",
    )
    def test_plate_with_hole_kt(self):
        """AC: Plate with hole Kt within 5% of 3.0 (infinite plate theory)."""
        chain = FEMChain()
        result = chain.run_plate_validation(sigma_applied=100.0)
        kt = result["kt"]
        assert abs(kt - 3.0) / 3.0 < 0.05, (
            f"Kt = {kt:.3f}, expected ~3.0 (within 5%)"
        )

    @pytest.mark.skipif(
        not (GMSH_AVAILABLE and is_calculix_available()),
        reason="Requires gmsh and CalculiX (ccx)",
    )
    def test_solve_returns_success(self, tmp_dir):
        """Integration: full chain solves without error."""
        chain = FEMChain(work_dir=tmp_dir)
        chain.create_plate_with_hole(
            plate_w=200.0, plate_h=200.0, hole_r=10.0,
            thickness=1.0, element_size=5.0,
        )
        chain.setup_analysis(
            material={"name": "STEEL", "E": 210000.0, "nu": 0.3},
            loads=[{"type": "cload", "node_set": "LOAD", "dof": 1, "magnitude": 100.0}],
            boundary_conditions=[{"node_set": "FIX", "dof_start": 1, "dof_end": 3}],
        )
        status = chain.solve()
        assert status["success"] is True

    @pytest.mark.skipif(
        not is_calculix_available(),
        reason="Requires CalculiX (ccx)",
    )
    def test_cantilever_beam_b31_deflection(self, tmp_dir):
        """AC: Cantilever beam tip deflection within 1% of PL^3/3EI."""
        # Cantilever: L=1m, rectangular 0.1x0.1m, E=210 GPa, P=1000 N at tip
        L = 1.0
        b, h = 0.1, 0.1
        E = 210000e6  # Pa
        P = 1000.0  # N
        I = b * h**3 / 12.0  # Second moment of area
        delta_exact = P * L**3 / (3.0 * E * I)

        n_elems = 10
        dx = L / n_elems
        nodes = np.array([[i * dx, 0.0, 0.0] for i in range(n_elems + 1)])
        elements = {
            "Line 2": {
                "connectivity": np.array(
                    [[i, i + 1] for i in range(n_elems)]
                ),
                "dimension": 1,
            }
        }

        writer = INPWriter(nodes, elements)
        writer.add_material("STEEL", E, 0.3)
        writer.add_beam_section(
            "EBEAM_B31", "STEEL", "RECT", (b, h),
            direction=(0.0, 0.0, 1.0),
        )
        writer.add_node_set("FIX", [0])
        writer.add_node_set("TIP", [n_elems])
        # Fix all 6 DOFs at root (3 translations + 3 rotations)
        for dof in range(1, 7):
            writer.add_boundary_condition("FIX", dof, 0.0)
        # Tip load in Y direction
        writer.add_load("TIP", "cload", P, direction=(0, 1, 0))
        writer.add_step()

        inp_path = writer.write(tmp_dir / "cantilever.inp")

        # Run ccx
        import subprocess, os
        result = subprocess.run(
            ["ccx", "-i", "cantilever"],
            cwd=str(tmp_dir),
            capture_output=True, text=True, timeout=60,
            env={**os.environ, "OMP_NUM_THREADS": "1"},
        )
        assert result.returncode == 0, f"ccx failed: {result.stderr[-500:]}"

        # Parse results — CalculiX expands beam nodes into 3D corners.
        # Tip expanded nodes are at x=L; average their y-displacement.
        parser = CalculiXResultParser("cantilever", tmp_dir)
        frd = parser.parse_frd_file()
        tip_dy_values = []
        for nid, disp in frd["displacements"].items():
            # All 4 corner nodes of the tip cross-section have same dy
            if abs(disp["dy"]) > 0 and nid >= (n_elems - 1) * 8 + 12:
                tip_dy_values.append(abs(disp["dy"]))
        assert len(tip_dy_values) > 0, "No tip displacement found"
        delta_ccx = max(tip_dy_values)

        error_pct = abs(delta_ccx - delta_exact) / delta_exact * 100
        assert error_pct < 1.0, (
            f"Tip deflection error {error_pct:.2f}%: "
            f"ccx={delta_ccx:.6e}, exact={delta_exact:.6e}"
        )

    def test_setup_analysis_writes_inp(self, tmp_dir, simple_mesh):
        """setup_analysis produces a valid .inp file without needing ccx."""
        chain = FEMChain(work_dir=tmp_dir)
        nodes, elements = simple_mesh
        chain._nodes = nodes
        chain._elements = elements
        chain._node_sets = {"FIX": [0], "LOAD": [1, 2, 3]}
        chain._element_sets = {}
        inp_path = chain.setup_analysis(
            material={"name": "STEEL", "E": 210000.0, "nu": 0.3},
            loads=[{"type": "cload", "node_set": "LOAD", "dof": 1, "magnitude": 100.0}],
            boundary_conditions=[{"node_set": "FIX", "dof_start": 1, "dof_end": 3}],
        )
        assert inp_path.exists()
        assert inp_path.suffix == ".inp"
        content = inp_path.read_text()
        assert "*HEADING" in content
        assert "*NODE" in content
        assert "TYPE=C3D4" in content
