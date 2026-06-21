"""Linux-side diffraction postprocessing (digitalmodel #928).

Covers the license-free path: DiffractionResults.to_dict() round-trip through
from_dict(), bundle loading, and the postprocess orchestrator producing exports
+ validation + index from a returned numeric bundle (no OrcFxAPI, no heavy files).
"""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.output_schemas import DiffractionResults
from digitalmodel.hydrodynamics.diffraction.postprocess import (
    PostprocResult,
    load_results_bundle,
    postprocess_diffraction_run,
)


def _assert_results_equal(a: DiffractionResults, b: DiffractionResults) -> None:
    assert a.vessel_name == b.vessel_name
    assert a.analysis_tool == b.analysis_tool
    assert a.water_depth == b.water_depth
    for dof in ("surge", "sway", "heave", "roll", "pitch", "yaw"):
        ca, cb = getattr(a.raos, dof), getattr(b.raos, dof)
        assert np.allclose(ca.magnitude, cb.magnitude)
        assert np.allclose(ca.phase, cb.phase)
        assert np.allclose(ca.frequencies.values, cb.frequencies.values)
        assert np.allclose(ca.headings.values, cb.headings.values)
        assert ca.unit == cb.unit
    for set_name in ("added_mass", "damping"):
        sa, sb = getattr(a, set_name), getattr(b, set_name)
        assert len(sa.matrices) == len(sb.matrices)
        for ma, mb in zip(sa.matrices, sb.matrices):
            assert np.allclose(ma.matrix, mb.matrix)
            assert ma.frequency == pytest.approx(mb.frequency)


def test_diffraction_results_to_dict_from_dict_roundtrip(mock_diffraction_results):
    rebuilt = DiffractionResults.from_dict(mock_diffraction_results.to_dict())
    _assert_results_equal(rebuilt, mock_diffraction_results)


def test_roundtrip_survives_json_serialization(dense_diffraction_results):
    blob = json.dumps(dense_diffraction_results.to_dict())
    rebuilt = DiffractionResults.from_dict(json.loads(blob))
    _assert_results_equal(rebuilt, dense_diffraction_results)


def test_load_results_bundle_reads_named_json(tmp_path: Path, mock_diffraction_results):
    (tmp_path / "diffraction_results.json").write_text(
        json.dumps(mock_diffraction_results.to_dict())
    )
    loaded = load_results_bundle(tmp_path)
    _assert_results_equal(loaded, mock_diffraction_results)


def test_load_results_bundle_discovers_unnamed_json(
    tmp_path: Path, mock_diffraction_results
):
    # A bundle whose results file is not the canonical name still loads.
    (tmp_path / "fpso_run_42.json").write_text(
        json.dumps(mock_diffraction_results.to_dict())
    )
    loaded = load_results_bundle(tmp_path)
    assert loaded.vessel_name == mock_diffraction_results.vessel_name


def test_load_results_bundle_raises_when_absent(tmp_path: Path):
    (tmp_path / "notes.json").write_text(json.dumps({"hello": "world"}))
    with pytest.raises(FileNotFoundError):
        load_results_bundle(tmp_path)


def test_postprocess_run_produces_exports_validation_and_index(
    tmp_path: Path, mock_diffraction_results
):
    bundle = tmp_path / "bundle"
    bundle.mkdir()
    (bundle / "diffraction_results.json").write_text(
        json.dumps(mock_diffraction_results.to_dict())
    )
    out = tmp_path / "postproc"

    result = postprocess_diffraction_run(bundle, out, make_plots=False)

    assert isinstance(result, PostprocResult)
    assert result.vessel == mock_diffraction_results.vessel_name
    assert result.validation_status  # non-empty status string
    # OrcaFlex export wrote a vessel type + coefficient CSVs.
    assert "vessel_type" in result.exports
    assert result.exports["vessel_type"].is_file()
    assert (out / "validation_report.json").is_file()
    index = json.loads((out / "index.json").read_text())
    assert index["vessel"] == mock_diffraction_results.vessel_name
    assert "exports" in index


def test_postprocess_run_no_heavy_files_emitted(
    tmp_path: Path, mock_diffraction_results
):
    bundle = tmp_path / "bundle"
    bundle.mkdir()
    (bundle / "diffraction_results.json").write_text(
        json.dumps(mock_diffraction_results.to_dict())
    )
    out = tmp_path / "postproc"
    postprocess_diffraction_run(bundle, out, make_plots=False)

    heavy = {".owr", ".owd", ".sim", ".gdf", ".dat", ".h5", ".bin"}
    offenders = [p for p in out.rglob("*") if p.suffix.lower() in heavy]
    assert offenders == [], f"postproc emitted heavy files: {offenders}"
