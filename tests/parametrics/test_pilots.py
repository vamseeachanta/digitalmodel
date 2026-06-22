"""Offline per-solver parametric pilots (parametrics P0, #968)."""

from __future__ import annotations

from pathlib import Path

from digitalmodel.parametrics.pilots import (
    ansys_padeye_sweep,
    orcaflex_fowt_watch_circle_sweep,
    orcawave_depth_sweep,
)

REPO_ROOT = Path(__file__).resolve().parents[2]


def test_ansys_padeye_sweep_prepares_inp_per_case(tmp_path: Path):
    summary = ansys_padeye_sweep(
        tmp_path, thicknesses_mm=(30.0, 40.0, 50.0), hole_diameters_mm=(70.0, 90.0)
    )
    assert len(summary.results) == 6  # 3 x 2
    assert all(r.status == "prepared" for r in summary.results)
    for r in summary.results:
        inp = Path(r.notes)
        assert inp.is_file()
        assert "PLANE182" in inp.read_text()


def test_orcawave_depth_sweep_prepares_validated_specs(tmp_path: Path):
    summary = orcawave_depth_sweep(
        tmp_path, water_depths_m=(100.0, 250.0, 500.0), repo_root=REPO_ROOT
    )
    assert len(summary.results) == 3
    assert all(r.status == "prepared" for r in summary.results), [
        (r.case_id, r.notes) for r in summary.results
    ]
    for r in summary.results:
        spec = Path(r.notes)
        assert spec.is_file() and spec.name == "spec.yml"


def test_orcaflex_fowt_sweep_returns_real_results(tmp_path: Path):
    summary = orcaflex_fowt_watch_circle_sweep(watch_circle_radii_m=(15.0, 25.0, 35.0))
    assert len(summary.results) == 3
    assert all(r.status == "completed" for r in summary.results)
    # A larger watch circle tightens the governing bend -> higher MBR utilisation.
    ucs = [r.max_utilisation for r in summary.results]
    assert all(u is not None for u in ucs)
    assert ucs[0] < ucs[-1]
    # Critical case is the largest watch-circle radius.
    critical = summary.get_critical_case("max_utilisation")
    assert critical.parameters["watch_circle_radius"] == 35.0
