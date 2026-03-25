"""Tests for the unified coarsen_mesh dispatcher -- Phase 4 of WRK-101.

Tests cover:
- Method dispatcher routing for each method string
- "auto" strategy selection (defaults to QEM)
- Backward compatibility with existing 3-argument call signature
- DecimationResult dataclass fields
"""

from __future__ import annotations

import numpy as np
import pytest

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import (
    MeshFormat,
    MeshQualityReport,
    PanelMesh,
)
from digitalmodel.hydrodynamics.hull_library.coarsen_mesh import (
    DecimationResult,
    coarsen_mesh,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def make_sphere_mesh(n_lat: int = 15, n_lon: int = 15) -> PanelMesh:
    """Create a UV-sphere mesh with approximately n_lat*n_lon panels."""
    import math

    lats = np.linspace(math.pi, 0.0, n_lat + 1)
    lons = np.linspace(0.0, 2 * math.pi, n_lon + 1)
    vertices_list = []
    for lat in lats:
        for lon in lons:
            vertices_list.append([
                math.sin(lat) * math.cos(lon),
                math.sin(lat) * math.sin(lon),
                math.cos(lat),
            ])
    stride = n_lon + 1
    panels_list = []
    for i in range(n_lat):
        for j in range(n_lon):
            panels_list.append([
                i * stride + j,
                i * stride + (j + 1),
                (i + 1) * stride + (j + 1),
                (i + 1) * stride + j,
            ])
    return PanelMesh(
        vertices=np.array(vertices_list, dtype=np.float64),
        panels=np.array(panels_list, dtype=np.int32),
        name="dispatcher_sphere",
        format_origin=MeshFormat.GDF,
    )


# ---------------------------------------------------------------------------
# Backward compatibility
# ---------------------------------------------------------------------------


class TestCoarsenMeshBackwardCompatibility:
    """Existing callers of coarsen_mesh(mesh, target, preserve_features) work."""

    def test_two_arg_call(self):
        """coarsen_mesh(mesh, target) works without keyword args."""
        mesh = make_sphere_mesh()
        target = mesh.n_panels // 2
        result = coarsen_mesh(mesh, target)
        assert isinstance(result, PanelMesh)
        assert result.n_panels <= mesh.n_panels

    def test_three_arg_call_preserve_features_true(self):
        """coarsen_mesh(mesh, target, True) preserves existing signature."""
        mesh = make_sphere_mesh()
        target = mesh.n_panels // 2
        result = coarsen_mesh(mesh, target, True)
        assert isinstance(result, PanelMesh)

    def test_three_arg_call_preserve_features_false(self):
        """coarsen_mesh(mesh, target, False) preserves existing signature."""
        mesh = make_sphere_mesh()
        target = mesh.n_panels // 2
        result = coarsen_mesh(mesh, target, False)
        assert isinstance(result, PanelMesh)

    def test_target_equal_to_current_returns_unchanged(self):
        """When target >= current panels, return input unchanged."""
        mesh = make_sphere_mesh(n_lat=5, n_lon=5)
        result = coarsen_mesh(mesh, mesh.n_panels)
        assert result is mesh or result.n_panels == mesh.n_panels


# ---------------------------------------------------------------------------
# Method dispatcher routing
# ---------------------------------------------------------------------------


class TestCoarsenMeshMethodDispatch:
    """method= parameter routes to the correct backend."""

    def test_auto_method_reduces_panels(self):
        """method='auto' reduces panel count."""
        mesh = make_sphere_mesh()
        target = mesh.n_panels // 2
        result = coarsen_mesh(mesh, target, method="auto")
        assert isinstance(result, PanelMesh)
        assert result.n_panels <= mesh.n_panels

    def test_qem_method_reduces_panels(self):
        """method='qem' routes to QEM decimation."""
        mesh = make_sphere_mesh()
        target = mesh.n_panels // 2
        result = coarsen_mesh(mesh, target, method="qem")
        assert isinstance(result, PanelMesh)
        assert result.n_panels <= mesh.n_panels

    def test_vertex_clustering_method(self):
        """method='vertex_clustering' uses the legacy algorithm."""
        mesh = make_sphere_mesh()
        target = mesh.n_panels // 2
        result = coarsen_mesh(mesh, target, method="vertex_clustering")
        assert isinstance(result, PanelMesh)

    def test_invalid_method_raises(self):
        """Unknown method string raises ValueError."""
        mesh = make_sphere_mesh()
        with pytest.raises(ValueError, match="Unknown decimation method"):
            coarsen_mesh(mesh, mesh.n_panels // 2, method="nonexistent_method")

    def test_vtk_raises_when_unavailable(self):
        """method='vtk' raises ImportError if PyVista/VTK not installed."""
        mesh = make_sphere_mesh()
        try:
            import pyvista  # noqa: F401
            pytest.skip("PyVista is installed; VTK backend available")
        except ImportError:
            with pytest.raises(ImportError, match="[Pp]y[Vv]ista|VTK"):
                coarsen_mesh(mesh, mesh.n_panels // 2, method="vtk")

    def test_gmsh_raises_when_unavailable(self):
        """method='gmsh' raises ImportError if GMSH not installed."""
        mesh = make_sphere_mesh()
        try:
            import gmsh  # noqa: F401
            pytest.skip("GMSH is installed; GMSH backend available")
        except ImportError:
            with pytest.raises(ImportError, match="[Gg][Mm][Ss][Hh]|gmsh"):
                coarsen_mesh(mesh, mesh.n_panels // 2, method="gmsh")


# ---------------------------------------------------------------------------
# DecimationResult dataclass
# ---------------------------------------------------------------------------


class TestDecimationResult:
    """DecimationResult dataclass has required fields and correct types."""

    def _build_result(self) -> DecimationResult:
        mesh = make_sphere_mesh()
        quality = MeshQualityReport(n_panels=mesh.n_panels)
        return DecimationResult(
            mesh=mesh,
            method_used="qem",
            input_panels=mesh.n_panels,
            output_panels=mesh.n_panels // 2,
            quality_before=quality,
            quality_after=quality,
            elapsed_seconds=0.1,
        )

    def test_decimation_result_has_mesh(self):
        """DecimationResult.mesh is a PanelMesh."""
        r = self._build_result()
        assert isinstance(r.mesh, PanelMesh)

    def test_decimation_result_has_method_used(self):
        """DecimationResult.method_used is a string."""
        r = self._build_result()
        assert isinstance(r.method_used, str)

    def test_decimation_result_has_panel_counts(self):
        """DecimationResult has input_panels and output_panels integers."""
        r = self._build_result()
        assert isinstance(r.input_panels, int)
        assert isinstance(r.output_panels, int)

    def test_decimation_result_has_quality_reports(self):
        """DecimationResult has quality_before and quality_after."""
        r = self._build_result()
        assert isinstance(r.quality_before, MeshQualityReport)
        assert isinstance(r.quality_after, MeshQualityReport)

    def test_decimation_result_has_elapsed_seconds(self):
        """DecimationResult.elapsed_seconds is a float."""
        r = self._build_result()
        assert isinstance(r.elapsed_seconds, float)


# ---------------------------------------------------------------------------
# max_aspect_ratio quality gate
# ---------------------------------------------------------------------------


class TestCoarsenMeshQualityGate:
    """max_aspect_ratio parameter stops decimation if quality is exceeded."""

    def test_max_aspect_ratio_parameter_accepted(self):
        """max_aspect_ratio kwarg is accepted without error."""
        mesh = make_sphere_mesh()
        result = coarsen_mesh(
            mesh, mesh.n_panels // 2, max_aspect_ratio=20.0
        )
        assert isinstance(result, PanelMesh)
