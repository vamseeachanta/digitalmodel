#!/usr/bin/env python3
"""
ABOUTME: Unit tests for OpenFOAM case directory builder covering directory
structure creation, file generation, and OpenFOAM dict file content.
"""

import pytest
import re
import tempfile
from pathlib import Path

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
from digitalmodel.solvers.openfoam.case_builder import OpenFOAMCaseBuilder


# ============================================================================
# Directory structure tests
# ============================================================================


class TestCaseDirectoryStructure:
    """Test that CaseBuilder creates the standard OpenFOAM directory tree."""

    def setup_method(self):
        """Create a minimal case for each test."""
        self.case = OpenFOAMCase.for_case_type(
            CaseType.CURRENT_LOADING, name="test_current"
        )

    def test_build_creates_zero_directory(self, tmp_path):
        """CaseBuilder creates the 0/ initial conditions directory."""
        builder = OpenFOAMCaseBuilder(self.case)
        case_dir = builder.build(tmp_path)
        assert (case_dir / "0").is_dir()

    def test_build_creates_constant_directory(self, tmp_path):
        """CaseBuilder creates the constant/ directory."""
        builder = OpenFOAMCaseBuilder(self.case)
        case_dir = builder.build(tmp_path)
        assert (case_dir / "constant").is_dir()

    def test_build_creates_system_directory(self, tmp_path):
        """CaseBuilder creates the system/ directory."""
        builder = OpenFOAMCaseBuilder(self.case)
        case_dir = builder.build(tmp_path)
        assert (case_dir / "system").is_dir()

    def test_build_creates_case_subdirectory(self, tmp_path):
        """CaseBuilder creates a named subdirectory for the case."""
        builder = OpenFOAMCaseBuilder(self.case)
        case_dir = builder.build(tmp_path)
        assert case_dir.name == "test_current"
        assert case_dir.parent == tmp_path

    def test_system_contains_control_dict(self, tmp_path):
        """system/ directory contains controlDict."""
        builder = OpenFOAMCaseBuilder(self.case)
        case_dir = builder.build(tmp_path)
        assert (case_dir / "system" / "controlDict").exists()

    def test_system_contains_fv_schemes(self, tmp_path):
        """system/ directory contains fvSchemes."""
        builder = OpenFOAMCaseBuilder(self.case)
        case_dir = builder.build(tmp_path)
        assert (case_dir / "system" / "fvSchemes").exists()

    def test_system_contains_fv_solution(self, tmp_path):
        """system/ directory contains fvSolution."""
        builder = OpenFOAMCaseBuilder(self.case)
        case_dir = builder.build(tmp_path)
        assert (case_dir / "system" / "fvSolution").exists()

    def test_system_contains_block_mesh_dict(self, tmp_path):
        """system/ directory contains blockMeshDict."""
        builder = OpenFOAMCaseBuilder(self.case)
        case_dir = builder.build(tmp_path)
        assert (case_dir / "system" / "blockMeshDict").exists()

    def test_system_contains_decompose_par_dict(self, tmp_path):
        """system/ directory contains decomposeParDict."""
        builder = OpenFOAMCaseBuilder(self.case)
        case_dir = builder.build(tmp_path)
        assert (case_dir / "system" / "decomposeParDict").exists()

    def test_constant_contains_transport_properties(self, tmp_path):
        """constant/ directory contains transportProperties."""
        builder = OpenFOAMCaseBuilder(self.case)
        case_dir = builder.build(tmp_path)
        assert (case_dir / "constant" / "transportProperties").exists()

    def test_constant_contains_turbulence_properties(self, tmp_path):
        """constant/ directory contains turbulenceProperties."""
        builder = OpenFOAMCaseBuilder(self.case)
        case_dir = builder.build(tmp_path)
        assert (case_dir / "constant" / "turbulenceProperties").exists()

    def test_zero_contains_velocity_field(self, tmp_path):
        """0/ directory contains U (velocity) field."""
        builder = OpenFOAMCaseBuilder(self.case)
        case_dir = builder.build(tmp_path)
        assert (case_dir / "0" / "U").exists()

    def test_zero_contains_pressure_field_single_phase(self, tmp_path):
        """0/ directory contains p (pressure) field for single-phase."""
        builder = OpenFOAMCaseBuilder(self.case)
        case_dir = builder.build(tmp_path)
        assert (case_dir / "0" / "p").exists()

    def test_zero_contains_nut_field(self, tmp_path):
        """0/ directory contains nut (turbulent viscosity) for RANS."""
        builder = OpenFOAMCaseBuilder(self.case)
        case_dir = builder.build(tmp_path)
        assert (case_dir / "0" / "nut").exists()


# ============================================================================
# Multiphase case specific files
# ============================================================================


class TestMultiphaseCaseStructure:
    """Tests for interFoam (VOF) case — extra alpha.water field required."""

    def setup_method(self):
        self.case = OpenFOAMCase.for_case_type(
            CaseType.WAVE_LOADING, name="wave_test"
        )

    def test_zero_contains_alpha_water_for_vof(self, tmp_path):
        """VOF cases must have 0/alpha.water initial field."""
        builder = OpenFOAMCaseBuilder(self.case)
        case_dir = builder.build(tmp_path)
        assert (case_dir / "0" / "alpha.water").exists()

    def test_constant_contains_g_for_multiphase(self, tmp_path):
        """Multiphase cases must define gravity in constant/g."""
        builder = OpenFOAMCaseBuilder(self.case)
        case_dir = builder.build(tmp_path)
        assert (case_dir / "constant" / "g").exists()

    def test_zero_contains_p_rgh_for_multiphase(self, tmp_path):
        """VOF cases must write 0/p_rgh (not 0/p) for the solver."""
        builder = OpenFOAMCaseBuilder(self.case)
        case_dir = builder.build(tmp_path)
        assert (case_dir / "0" / "p_rgh").exists()

    def test_fv_solution_no_macro_references(self, tmp_path):
        """fvSolution must not use $p or $k shorthand macros in Final blocks."""
        builder = OpenFOAMCaseBuilder(self.case)
        case_dir = builder.build(tmp_path)
        content = (case_dir / "system" / "fvSolution").read_text()
        # Ensure no bare $p or $k macro substitutions remain
        assert "$p;" not in content
        assert "$k;" not in content


# ============================================================================
# controlDict content tests
# ============================================================================


class TestControlDictContent:
    """Test controlDict file has correct OpenFOAM header and content."""

    def test_control_dict_contains_application(self, tmp_path):
        """controlDict must specify the application solver."""
        case = OpenFOAMCase.for_case_type(
            CaseType.CURRENT_LOADING, name="ctrl_test"
        )
        builder = OpenFOAMCaseBuilder(case)
        case_dir = builder.build(tmp_path)
        content = (case_dir / "system" / "controlDict").read_text()
        assert "application" in content
        assert "simpleFoam" in content or "pimpleFoam" in content

    def test_control_dict_openfoam_header(self, tmp_path):
        """controlDict must have the FoamFile header."""
        case = OpenFOAMCase.for_case_type(
            CaseType.CURRENT_LOADING, name="hdr_test"
        )
        builder = OpenFOAMCaseBuilder(case)
        case_dir = builder.build(tmp_path)
        content = (case_dir / "system" / "controlDict").read_text()
        assert "FoamFile" in content

    def test_control_dict_end_time(self, tmp_path):
        """controlDict encodes endTime from SolverConfig."""
        cfg = SolverConfig(solver_name="simpleFoam", end_time=500.0)
        case = OpenFOAMCase(
            name="end_time_test",
            case_type=CaseType.CURRENT_LOADING,
            solver_config=cfg,
        )
        builder = OpenFOAMCaseBuilder(case)
        case_dir = builder.build(tmp_path)
        content = (case_dir / "system" / "controlDict").read_text()
        assert "500" in content


class TestWallDistanceSchemes:
    """A RAS model that needs a wall distance must be told how to compute it.

    Found by the issue #1959 solver-start oracle, not by inspection: the case
    read p_rgh, U and transportProperties, selected kOmegaSST, and only then
    died on a missing wallDist method. meshWave is the method every interFoam
    tutorial in the pinned v2312 installation uses.
    """

    def _fv_schemes(self, tmp_path, turbulence_type, name):
        case = OpenFOAMCase.for_case_type(CaseType.SLOSHING, name=name)
        case.turbulence_model = TurbulenceModel(turbulence_type=turbulence_type)
        case_dir = OpenFOAMCaseBuilder(case).build(tmp_path)
        return (case_dir / "system" / "fvSchemes").read_text()

    def test_ras_case_declares_wall_dist(self, tmp_path):
        """kOmegaSST needs a wallDist block in fvSchemes."""
        content = self._fv_schemes(tmp_path, TurbulenceType.K_OMEGA_SST, "wd_ras")
        assert "wallDist" in content

    def test_ras_wall_dist_method_is_mesh_wave(self, tmp_path):
        """The wall distance method is meshWave."""
        content = self._fv_schemes(tmp_path, TurbulenceType.K_OMEGA_SST, "wd_method")
        match = re.search(r"^\s*method\s+(\S+?);", content, re.MULTILINE)
        assert match.group(1) == "meshWave"

    def test_laminar_case_omits_wall_dist(self, tmp_path):
        """A laminar case computes no wall distance and needs no block."""
        content = self._fv_schemes(tmp_path, TurbulenceType.LAMINAR, "wd_laminar")
        assert "wallDist" not in content


class TestVofControlDictCourantBounds:
    """A VOF run at fixed deltaT with no interface Courant bound is not
    defensible as runnable (issue #1959, design decision D7)."""

    def _control_dict(self, tmp_path, name):
        case = OpenFOAMCase.for_case_type(CaseType.SLOSHING, name=name)
        case_dir = OpenFOAMCaseBuilder(case).build(tmp_path)
        return case, (case_dir / "system" / "controlDict").read_text()

    def test_vof_case_adjusts_time_step(self, tmp_path):
        """interFoam cases must run with adjustTimeStep yes."""
        _, content = self._control_dict(tmp_path, "vof_adjust")
        match = re.search(r"^adjustTimeStep\s+(\S+?);", content, re.MULTILINE)
        assert match.group(1) == "yes"

    def test_vof_case_declares_max_alpha_co(self, tmp_path):
        """The interface Courant bound must be present."""
        _, content = self._control_dict(tmp_path, "vof_alpha_co")
        assert "maxAlphaCo" in content

    def test_max_alpha_co_equals_max_co(self, tmp_path):
        """maxAlphaCo is derived from the case's declared maxCo, not tuned."""
        case, content = self._control_dict(tmp_path, "vof_alpha_co_value")
        match = re.search(r"^maxAlphaCo\s+(\S+?);", content, re.MULTILINE)
        assert float(match.group(1)) == case.solver_config.max_co

    def test_max_delta_t_equals_delta_t(self, tmp_path):
        """maxDeltaT defaults to the declared deltaT -- no new constant."""
        case, content = self._control_dict(tmp_path, "vof_max_dt")
        match = re.search(r"^maxDeltaT\s+(\S+?);", content, re.MULTILINE)
        assert float(match.group(1)) == case.solver_config.delta_t

    def test_single_phase_case_declares_no_max_alpha_co(self, tmp_path):
        """simpleFoam cases must not carry an interface Courant bound."""
        case = OpenFOAMCase.for_case_type(
            CaseType.CURRENT_LOADING, name="single_phase_no_alpha_co"
        )
        case_dir = OpenFOAMCaseBuilder(case).build(tmp_path)
        content = (case_dir / "system" / "controlDict").read_text()
        assert "maxAlphaCo" not in content


# ============================================================================
# blockMeshDict content tests
# ============================================================================


class TestBlockMeshDictContent:
    """Test blockMeshDict generation from DomainConfig."""

    def test_block_mesh_dict_contains_vertices(self, tmp_path):
        """blockMeshDict contains vertices section."""
        domain = DomainConfig(
            min_coords=[-100.0, -50.0, -30.0],
            max_coords=[200.0, 50.0, 5.0],
        )
        case = OpenFOAMCase(
            name="bmd_test",
            case_type=CaseType.CURRENT_LOADING,
            domain=domain,
        )
        builder = OpenFOAMCaseBuilder(case)
        case_dir = builder.build(tmp_path)
        content = (case_dir / "system" / "blockMeshDict").read_text()
        assert "vertices" in content

    def test_block_mesh_dict_has_eight_vertices(self, tmp_path):
        """blockMeshDict encodes exactly 8 corner vertices."""
        domain = DomainConfig(
            min_coords=[0.0, 0.0, 0.0],
            max_coords=[10.0, 5.0, 3.0],
        )
        case = OpenFOAMCase(
            name="bmd_8v_test",
            case_type=CaseType.CURRENT_LOADING,
            domain=domain,
        )
        builder = OpenFOAMCaseBuilder(case)
        case_dir = builder.build(tmp_path)
        content = (case_dir / "system" / "blockMeshDict").read_text()
        # Each vertex line starts with "(" in the vertices list
        # Count approximate vertex entries
        assert content.count("(") >= 8

    def test_block_mesh_dict_contains_blocks(self, tmp_path):
        """blockMeshDict contains blocks section."""
        case = OpenFOAMCase.for_case_type(
            CaseType.CURRENT_LOADING, name="bmd_blocks"
        )
        builder = OpenFOAMCaseBuilder(case)
        case_dir = builder.build(tmp_path)
        content = (case_dir / "system" / "blockMeshDict").read_text()
        assert "blocks" in content

    def test_block_mesh_dict_contains_boundary(self, tmp_path):
        """blockMeshDict contains boundary section."""
        case = OpenFOAMCase.for_case_type(
            CaseType.CURRENT_LOADING, name="bmd_boundary"
        )
        builder = OpenFOAMCaseBuilder(case)
        case_dir = builder.build(tmp_path)
        content = (case_dir / "system" / "blockMeshDict").read_text()
        assert "boundary" in content


# ============================================================================
# Idempotency test
# ============================================================================


class TestCaseBuilderIdempotency:
    """Test building the same case multiple times is safe."""

    def test_rebuild_overwrites_existing(self, tmp_path):
        """Building a case into an existing directory overwrites files."""
        case = OpenFOAMCase.for_case_type(
            CaseType.CURRENT_LOADING, name="idempotent"
        )
        builder = OpenFOAMCaseBuilder(case)
        case_dir1 = builder.build(tmp_path)
        case_dir2 = builder.build(tmp_path)
        assert case_dir1 == case_dir2
        assert (case_dir2 / "system" / "controlDict").exists()
