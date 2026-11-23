"""Closed-form controls for the failure-counterexample geometry diagnostic."""

import numpy as np
import pytest

pytest.importorskip("hullprod")

from digitalmodel.hydrodynamics.hull_library.hull_surface_brep import (
    bspline_face_from_grid,
)
from scripts.hull_library.diagnose_brep_geometry import sample_surface
from scripts.hull_library.diagnose_form_brep import native_acceptance


def test_plane_derivatives_and_bounds():
    x, z = np.meshgrid(np.linspace(0, 100, 9), np.linspace(-8, 0, 9), indexing="ij")
    face = bspline_face_from_grid(np.stack((x, np.full_like(x, 10), z), axis=-1))
    result = sample_surface(face, 17)
    assert result["bounds_min_m"] == pytest.approx([0, 10, -8], abs=1e-8)
    assert result["bounds_max_m"] == pytest.approx([100, 10, 0], abs=1e-8)
    assert result["projection_jacobian_negative_count"] == 0
    assert result["surface_jacobian_min_m2"] > 0
    assert result["max_abs_gaussian_curvature_m2_inverse"] < 1e-12


@pytest.mark.parametrize("bad_fraction", [None, float("nan"), 0.99])
def test_native_gate_rejects_missing_or_partial_area(bad_fraction):
    from types import SimpleNamespace

    signature = SimpleNamespace(
        a_flat=1,
        a_single=0,
        a_elliptic=0,
        a_saddle=0,
        as_vector=lambda: np.array([0, 0, 0, 1, 0, 0, 0]),
    )
    metrics = {
        key: {"status": "valid", "valid_area_fraction": 1}
        for key in ("developability_deviation", "curvature_classes", "surface_area")
    }
    metrics["curvature_classes"]["valid_area_fraction"] = bad_fraction
    metadata = {"metric_validity": metrics, "curvature_valid_area_fraction": 1}
    assert "missing or incomplete valid-area coverage" in native_acceptance(
        signature, metadata
    )


def test_native_gate_rejects_caution_despite_finite_normalized_classes():
    from types import SimpleNamespace

    signature = SimpleNamespace(
        a_flat=1,
        a_single=0,
        a_elliptic=0,
        a_saddle=0,
        as_vector=lambda: np.array([0, 0, 0, 1, 0, 0, 0]),
    )
    metrics = {
        key: {"status": "valid", "valid_area_fraction": 1}
        for key in ("developability_deviation", "curvature_classes", "surface_area")
    }
    metrics["curvature_classes"]["status"] = "caution_singular_measure_zero"
    errors = native_acceptance(
        signature, {"metric_validity": metrics, "curvature_valid_area_fraction": 1}
    )
    assert errors == ["curvature_classes: status is not valid"]


def test_diagnostic_worker_reports_construction_failure(monkeypatch, tmp_path):
    import json
    from types import SimpleNamespace

    from scripts.hull_library import diagnose_form_brep as diagnostic

    def rejected(_args):
        raise ValueError("synthetic construction failure")

    monkeypatch.setattr(diagnostic, "assess_case", rejected)
    output = tmp_path / "record.json"
    args = SimpleNamespace(
        case="rounded",
        sampling="section_arclength",
        nx=81,
        nz=41,
        side_only=False,
        output=output,
    )
    assert diagnostic.worker(args) == 1
    record = json.loads(output.read_text())
    assert record["error_type"] == "ValueError"
    assert record["qualified_repair"] is False
