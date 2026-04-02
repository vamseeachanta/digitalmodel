# ABOUTME: Tests for APDLGenerator — APDL command script generation from config
# ABOUTME: Verifies valid APDL command syntax in generated output

"""Tests for apdl_generator — APDLGenerator command generation."""

import pytest

from digitalmodel.ansys.apdl_generator import (
    APDLGenerator,
    APDLScriptConfig,
    BoundaryCondition,
    ElementTypeConfig,
    MaterialConfig,
    MeshConfig,
    PostProcessConfig,
    SolutionConfig,
)


def _gen() -> APDLGenerator:
    return APDLGenerator()


# ---------------------------------------------------------------------------
# Material generation
# ---------------------------------------------------------------------------

class TestGenerateMaterials:
    def test_generates_mp_ex_command(self):
        mat = MaterialConfig(mat_id=1, elastic_modulus_mpa=200000.0)
        text = _gen().generate_materials([mat])
        assert "MP,EX,1,200000.0" in text

    def test_generates_mp_nuxy_command(self):
        mat = MaterialConfig(mat_id=1, poissons_ratio=0.3)
        text = _gen().generate_materials([mat])
        assert "MP,NUXY,1,0.3" in text

    def test_generates_thermal_expansion(self):
        mat = MaterialConfig(mat_id=2, thermal_expansion_per_c=1.2e-5)
        text = _gen().generate_materials([mat])
        assert "MP,ALPX,2," in text

    def test_multiple_materials_have_unique_ids(self):
        mats = [
            MaterialConfig(mat_id=1, name="Steel"),
            MaterialConfig(mat_id=2, name="Aluminum"),
        ]
        text = _gen().generate_materials(mats)
        assert "MP,EX,1," in text
        assert "MP,EX,2," in text


# ---------------------------------------------------------------------------
# Element types
# ---------------------------------------------------------------------------

class TestGenerateElementTypes:
    def test_generates_et_command(self):
        et = ElementTypeConfig(type_id=1, element_name="SHELL181")
        text = _gen().generate_element_types([et])
        assert "ET,1,SHELL181" in text

    def test_generates_keyopt(self):
        et = ElementTypeConfig(type_id=1, element_name="SHELL181", keyopts={3: 2})
        text = _gen().generate_element_types([et])
        assert "KEYOPT,1,3,2" in text

    def test_generates_real_constants(self):
        et = ElementTypeConfig(type_id=1, element_name="BEAM188", real_constants=[25.0, 1000.0])
        text = _gen().generate_element_types([et])
        assert "R,1,25.0,1000.0" in text


# ---------------------------------------------------------------------------
# Mesh commands
# ---------------------------------------------------------------------------

class TestGenerateMeshCommands:
    def test_generates_esize(self):
        mesh = MeshConfig(element_size_mm=10.0)
        text = _gen().generate_mesh_commands(mesh)
        assert "ESIZE,10.0" in text

    def test_generates_mshape_for_tet(self):
        mesh = MeshConfig(mesh_shape="TET")
        text = _gen().generate_mesh_commands(mesh)
        assert "MSHAPE,1,3D" in text

    def test_generates_vmesh_all_by_default(self):
        mesh = MeshConfig()
        text = _gen().generate_mesh_commands(mesh)
        assert "VMESH,ALL" in text

    def test_generates_amesh_for_specific_areas(self):
        mesh = MeshConfig(target_areas=[1, 2, 3])
        text = _gen().generate_mesh_commands(mesh)
        assert "AMESH,1,2,3" in text


# ---------------------------------------------------------------------------
# Boundary conditions
# ---------------------------------------------------------------------------

class TestGenerateBoundaryConditions:
    def test_generates_displacement_bc(self):
        bc = BoundaryCondition(bc_type="displacement", target_id=5, dof="UX", value=0.0)
        text = _gen().generate_boundary_conditions([bc])
        assert "D,5,UX,0.0" in text

    def test_generates_force_bc(self):
        bc = BoundaryCondition(bc_type="force", target_id=10, dof="FY", value=5000.0)
        text = _gen().generate_boundary_conditions([bc])
        assert "F,10,FY,5000.0" in text

    def test_generates_pressure_on_area(self):
        bc = BoundaryCondition(bc_type="pressure", target="AREA", target_id=3, value=10.0)
        text = _gen().generate_boundary_conditions([bc])
        assert "SFA,3,1,PRES,10.0" in text


# ---------------------------------------------------------------------------
# Solution
# ---------------------------------------------------------------------------

class TestGenerateSolution:
    def test_generates_antype(self):
        sol = SolutionConfig(analysis_type="STATIC")
        text = _gen().generate_solution(sol)
        assert "ANTYPE,0" in text

    def test_generates_nlgeom_on(self):
        sol = SolutionConfig(large_deflection=True)
        text = _gen().generate_solution(sol)
        assert "NLGEOM,ON" in text

    def test_generates_solve(self):
        sol = SolutionConfig()
        text = _gen().generate_solution(sol)
        assert "SOLVE" in text

    def test_generates_nsubst(self):
        sol = SolutionConfig(num_substeps=20, max_substeps=100, min_substeps=5)
        text = _gen().generate_solution(sol)
        assert "NSUBST,20,100,5" in text


# ---------------------------------------------------------------------------
# Full script
# ---------------------------------------------------------------------------

class TestGenerateFullScript:
    def test_full_script_contains_prep7(self):
        config = APDLScriptConfig(
            materials=[MaterialConfig()],
            element_types=[ElementTypeConfig()],
        )
        text = _gen().generate_full_script(config)
        assert "/PREP7" in text

    def test_full_script_contains_title(self):
        config = APDLScriptConfig(title="My Test Analysis")
        text = _gen().generate_full_script(config)
        assert "/TITLE,My Test Analysis" in text

    def test_full_script_contains_solve(self):
        config = APDLScriptConfig()
        text = _gen().generate_full_script(config)
        assert "SOLVE" in text
