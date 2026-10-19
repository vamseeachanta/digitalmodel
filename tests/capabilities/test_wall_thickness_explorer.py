# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Pressure-only projection contract from issue 1903's sidecar decision."""

import json
from types import SimpleNamespace

from scripts.capabilities import build_wall_thickness_explorer as builder


def test_pressure_only_projection_omits_only_unloaded_combined(monkeypatch):
    """Hand projection: {burst: 0.5, combined_loading: 0.25, combined: 0}
    becomes {burst: 0.5, combined_loading: 0.25}; max(0.5, 0.25, 0) = 0.5.
    These dimensionless fixture values are arbitrary, not design constants.
    The pressure-dependent combined_loading result remains published.
    """
    analysis = SimpleNamespace(
        checks={"burst": 0.5, "combined_loading": 0.25, "combined": 0.0},
        max_utilisation=0.5, governing_check="burst", is_safe=True,
        code_reference="synthetic pressure case",
    )

    class Analyzer:
        def __init__(self, geometry, material, loads, factors, code):
            assert loads.bending_moment == loads.effective_tension == 0.0

        def perform_analysis(self):
            return analysis

    monkeypatch.setattr(builder, "WallThicknessAnalyzer", Analyzer)
    result = builder._run(0.02, next(iter(builder.DesignCode)))
    assert result["checks"] == {"burst": 0.5, "combined_loading": 0.25}
    assert result["max_util"] == 0.5
    assert result["governing"] == "burst"
    assert result["is_safe"] is True
    assert analysis.checks["combined"] == 0.0


def test_pressure_only_limits_reach_page_and_sidecar(tmp_path, monkeypatch):
    from scripts.hf import build_capability_datasets as datasets

    output = tmp_path / "structural"
    monkeypatch.setattr(builder, "_OUT_DIR", output)
    monkeypatch.setattr(builder, "_REPO", tmp_path)
    builder.main()
    study = json.loads((output / "wall-thickness-explorer.json").read_text())
    html = (output / "wall-thickness-explorer.html").read_text()
    assert study["meta"]["load_case"] == "pressure-only"
    assert "tension" in study["data_limits"]
    assert "bending" in study["data_limits"]
    assert study["data_limits"] in html
    assert all(
        "combined" not in row["checks"]
        for rows in study["series"].values() for row in rows
    )
    assert 'data-theme="light"' in html
    assert 'href="../_assets/brand.css"' in html
    monkeypatch.setattr(datasets, "_API", tmp_path)
    rows = datasets.wall_thickness_rows()
    assert len(rows) == sum(len(points) for points in study["series"].values())
    assert all("check_combined" not in row for row in rows)
    assert any("check_combined_loading" in row for row in rows)
