#!/usr/bin/env python3
"""
ABOUTME: Unit tests for OpenFOAM CFD data models covering case configuration,
boundary conditions, solver settings, and marine application enumerations.
"""

import pytest
from dataclasses import asdict

from digitalmodel.solvers.openfoam.models import (
    BoundaryCondition,
    BoundaryType,
    CaseType,
    DomainConfig,
    OpenFOAMCase,
    SolverConfig,
    TurbulenceModel,
    TurbulenceType,
)


# ============================================================================
# CaseType tests
# ============================================================================


class TestCaseType:
    """Test CaseType enumeration covers required marine applications."""

    def test_all_required_case_types_present(self):
        """All marine/offshore case types must be defined."""
        values = {ct.value for ct in CaseType}
        assert "wave_loading" in values
        assert "current_loading" in values
        assert "greenwater" in values
        assert "sloshing" in values
        assert "viv" in values

    def test_case_type_from_value(self):
        """CaseType members are retrievable by value."""
        ct = CaseType("wave_loading")
        assert ct == CaseType.WAVE_LOADING


# ============================================================================
# BoundaryCondition tests
# ============================================================================


class TestBoundaryCondition:
    """Test BoundaryCondition model serialisation to OpenFOAM dict syntax."""

    def test_fixed_value_bc_creation(self):
        """Fixed-value BC can be created with required fields."""
        bc = BoundaryCondition(
            patch_name="inlet",
            bc_type=BoundaryType.FIXED_VALUE,
            field="U",
            value="uniform (1.0 0 0)",
        )
        assert bc.patch_name == "inlet"
        assert bc.field == "U"
        assert bc.bc_type == BoundaryType.FIXED_VALUE

    def test_zero_gradient_bc(self):
        """zeroGradient BC does not require a value."""
        bc = BoundaryCondition(
            patch_name="outlet",
            bc_type=BoundaryType.ZERO_GRADIENT,
            field="p",
        )
        assert bc.value is None

    def test_to_foam_dict_fixed_value(self):
        """Fixed-value BC serialises to correct OpenFOAM dict block."""
        bc = BoundaryCondition(
            patch_name="inlet",
            bc_type=BoundaryType.FIXED_VALUE,
            field="U",
            value="uniform (1.0 0 0)",
        )
        foam = bc.to_foam_dict()
        assert "inlet" in foam
        assert "type" in foam["inlet"]
        assert foam["inlet"]["type"] == "fixedValue"
        assert "value" in foam["inlet"]

    def test_to_foam_dict_zero_gradient(self):
        """zeroGradient BC serialises without value key."""
        bc = BoundaryCondition(
            patch_name="outlet",
            bc_type=BoundaryType.ZERO_GRADIENT,
            field="p",
        )
        foam = bc.to_foam_dict()
        assert foam["outlet"]["type"] == "zeroGradient"
        assert "value" not in foam["outlet"]

    def test_wall_bc(self):
        """noSlip wall BC serialises correctly."""
        bc = BoundaryCondition(
            patch_name="hull",
            bc_type=BoundaryType.NO_SLIP,
            field="U",
        )
        foam = bc.to_foam_dict()
        assert foam["hull"]["type"] == "noSlip"


# ============================================================================
# TurbulenceModel tests
# ============================================================================


class TestTurbulenceModel:
    """Test TurbulenceModel configuration dataclass."""

    def test_k_omega_sst_defaults(self):
        """k-omega SST model has expected defaults."""
        tm = TurbulenceModel(turbulence_type=TurbulenceType.K_OMEGA_SST)
        assert tm.turbulence_type == TurbulenceType.K_OMEGA_SST
        assert tm.intensity is not None
        assert 0 < tm.intensity < 1.0

    def test_k_epsilon_model(self):
        """k-epsilon model can be instantiated."""
        tm = TurbulenceModel(turbulence_type=TurbulenceType.K_EPSILON)
        assert tm.turbulence_type == TurbulenceType.K_EPSILON

    def test_laminar_model(self):
        """Laminar (no turbulence) model can be selected."""
        tm = TurbulenceModel(turbulence_type=TurbulenceType.LAMINAR)
        d = tm.to_dict()
        assert d["simulationType"] == "laminar"


# ============================================================================
# SolverConfig tests
# ============================================================================


class TestSolverConfig:
    """Test SolverConfig dataclass for solver selection and tolerances."""

    def test_simple_foam_defaults(self):
        """simpleFoam config has correct solver name."""
        cfg = SolverConfig(solver_name="simpleFoam")
        assert cfg.solver_name == "simpleFoam"
        assert cfg.end_time > 0
        assert cfg.delta_t > 0

    def test_inter_foam_vof_flag(self):
        """interFoam config enables VOF by default."""
        cfg = SolverConfig(solver_name="interFoam", is_multiphase=True)
        assert cfg.is_multiphase is True

    def test_pimple_foam_time_stepping(self):
        """pimpleFoam config allows adjustable time step."""
        cfg = SolverConfig(
            solver_name="pimpleFoam",
            adjustable_time_step=True,
            max_co=0.9,
        )
        assert cfg.adjustable_time_step is True
        assert cfg.max_co == pytest.approx(0.9)

    def test_solver_config_to_control_dict_entries(self):
        """SolverConfig produces correct controlDict field values."""
        cfg = SolverConfig(
            solver_name="simpleFoam",
            end_time=1000.0,
            write_interval=100,
        )
        d = cfg.to_control_dict()
        assert d["application"] == "simpleFoam"
        assert d["endTime"] == pytest.approx(1000.0)
        assert d["writeInterval"] == 100


# ============================================================================
# DomainConfig tests
# ============================================================================


class TestDomainConfig:
    """Test DomainConfig for parametric domain sizing."""

    def test_domain_from_bounding_box(self):
        """Domain can be configured from a geometry bounding box."""
        dc = DomainConfig(
            min_coords=[-5.0, -3.0, -10.0],
            max_coords=[5.0, 3.0, 0.0],
        )
        assert dc.min_coords == [-5.0, -3.0, -10.0]
        assert dc.max_coords == [5.0, 3.0, 0.0]

    def test_automatic_domain_sizing(self):
        """Domain can be auto-sized from hull length and offsets."""
        dc = DomainConfig.from_hull_geometry(
            hull_length=100.0,
            hull_beam=20.0,
            hull_draft=10.0,
        )
        # Upstream should be at least 2L
        x_min = dc.min_coords[0]
        assert x_min <= -2.0 * 100.0

        # Downstream should be at least 5L
        x_max = dc.max_coords[0]
        assert x_max >= 5.0 * 100.0

    def test_cell_count_estimate(self):
        """Domain config produces a reasonable cell count estimate."""
        dc = DomainConfig(
            min_coords=[-200.0, -50.0, -20.0],
            max_coords=[500.0, 50.0, 20.0],
            base_cell_size=5.0,
        )
        count = dc.estimate_cell_count()
        assert count > 0

    def test_block_mesh_vertices(self):
        """Domain config generates 8 bounding box vertices for blockMeshDict."""
        dc = DomainConfig(
            min_coords=[0.0, 0.0, 0.0],
            max_coords=[10.0, 5.0, 3.0],
        )
        verts = dc.block_mesh_vertices()
        assert len(verts) == 8

    def test_block_mesh_vertices_order(self):
        """blockMesh vertices follow OpenFOAM hex ordering convention.

        OpenFOAM hex block vertex order (right-hand rule):
        0: (x0,y0,z0)  1: (x1,y0,z0)  2: (x1,y1,z0)  3: (x0,y1,z0)
        4: (x0,y0,z1)  5: (x1,y0,z1)  6: (x1,y1,z1)  7: (x0,y1,z1)
        """
        dc = DomainConfig(
            min_coords=[0.0, 0.0, 0.0],
            max_coords=[1.0, 1.0, 1.0],
        )
        verts = dc.block_mesh_vertices()
        # First vertex: (xmin, ymin, zmin)
        assert verts[0] == [0.0, 0.0, 0.0]
        # Vertex 6: (xmax, ymax, zmax)
        assert verts[6] == [1.0, 1.0, 1.0]
        # Vertex 7: (xmin, ymax, zmax) -- standard OpenFOAM hex ordering
        assert verts[7] == [0.0, 1.0, 1.0]


# ============================================================================
# OpenFOAMCase tests
# ============================================================================


class TestOpenFOAMCase:
    """Test top-level OpenFOAMCase configuration model."""

    def test_case_creation_with_defaults(self):
        """OpenFOAMCase can be created with minimal arguments."""
        case = OpenFOAMCase(
            name="cylinder_current",
            case_type=CaseType.CURRENT_LOADING,
        )
        assert case.name == "cylinder_current"
        assert case.case_type == CaseType.CURRENT_LOADING
        assert case.solver_config is not None
        assert case.turbulence_model is not None
        assert case.domain is not None

    def test_wave_loading_case_uses_interfoam(self):
        """Wave loading case selects interFoam by default."""
        case = OpenFOAMCase.for_case_type(CaseType.WAVE_LOADING, name="wave_test")
        assert case.solver_config.solver_name == "interFoam"
        assert case.solver_config.is_multiphase is True

    def test_current_loading_case_uses_simplefoam(self):
        """Steady current loading case selects simpleFoam."""
        case = OpenFOAMCase.for_case_type(
            CaseType.CURRENT_LOADING, name="current_test"
        )
        assert case.solver_config.solver_name in ("simpleFoam", "pimpleFoam")

    def test_sloshing_case_uses_interfoam(self):
        """Sloshing case selects interFoam (VOF for free surface)."""
        case = OpenFOAMCase.for_case_type(CaseType.SLOSHING, name="slosh_test")
        assert case.solver_config.solver_name == "interFoam"

    def test_viv_case_uses_pimplefoam(self):
        """VIV case selects pimpleFoam for transient simulation."""
        case = OpenFOAMCase.for_case_type(CaseType.VIV, name="viv_test")
        assert case.solver_config.solver_name == "pimpleFoam"

    def test_boundary_conditions_attached(self):
        """Boundary conditions can be added to a case."""
        case = OpenFOAMCase(
            name="test_case",
            case_type=CaseType.CURRENT_LOADING,
        )
        bc = BoundaryCondition(
            patch_name="inlet",
            bc_type=BoundaryType.FIXED_VALUE,
            field="U",
            value="uniform (1.5 0 0)",
        )
        case.add_boundary_condition(bc)
        assert len(case.boundary_conditions) == 1
        assert case.boundary_conditions[0].patch_name == "inlet"
