# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Pressure-only exports retain the existing sweep and disclose its limits."""

import json
from types import SimpleNamespace

from scripts.capabilities import build_wall_thickness_explorer as builder
from scripts.hf import build_capability_datasets as datasets


def test_pressure_study_excludes_only_the_exact_combined_key(monkeypatch):
    result = SimpleNamespace(
        checks={"combined": 0.0, "combined_loading": 0.25, "burst": 0.5},
        max_utilisation=0.5, governing_check="burst", is_safe=True,
        code_reference="synthetic reference",
    )
    monkeypatch.setattr(builder, "WallThicknessAnalyzer", lambda *a, **k:
                        SimpleNamespace(perform_analysis=lambda: result))
    row = builder._run(0.02, builder.DesignCode.API_STD_2RD)
    assert row["checks"] == {"combined_loading": 0.25, "burst": 0.5}


def test_pressure_study_discloses_zero_tension_and_moment():
    study = builder.build_study()
    assert study["meta"]["load_case"] == "pressure-only"
    assert study["meta"]["effective_tension_n"] == 0.0
    assert study["meta"]["bending_moment_nm"] == 0.0
    assert "combined tension and bending are not evaluated" in study["meta"]["data_limits"]
    assert all("combined" not in row["checks"]
               for rows in study["series"].values() for row in rows)


def test_other_study_values_match_the_committed_sweep():
    baseline = json.loads(
        (builder._OUT_DIR / "wall-thickness-explorer.json").read_text()
    )
    actual = builder.build_study()
    for study in (baseline, actual):
        for key in ("load_case", "effective_tension_n", "bending_moment_nm", "data_limits"):
            study["meta"].pop(key, None)
        for rows in study["series"].values():
            for row in rows:
                row["checks"].pop("combined", None)
    assert actual == baseline
    assert len(actual["series"]) == 9
    assert sum(map(len, actual["series"].values())) == 369


def test_tidy_rows_remove_combined_but_retain_combined_loading(monkeypatch):
    monkeypatch.setattr(datasets, "_load", lambda _: builder.build_study())
    rows = datasets.wall_thickness_rows()
    assert len(rows) == 369
    assert all("check_combined" not in row for row in rows)
    assert any("check_combined_loading" in row for row in rows)


def test_main_is_repeatable_and_matches_committed_outputs(tmp_path, monkeypatch):
    expected = {suffix: (builder._OUT_DIR / f"wall-thickness-explorer.{suffix}").read_bytes()
                for suffix in ("html", "json")}
    output = tmp_path / "docs/api/structural"
    monkeypatch.setattr(builder, "_REPO", tmp_path)
    monkeypatch.setattr(builder, "_OUT_DIR", output)
    builder.main()
    first = {suffix: (output / f"wall-thickness-explorer.{suffix}").read_bytes()
             for suffix in expected}
    html = first["html"].decode().split("<script>", 1)[0].lower()
    assert "pressure-only" in html
    assert "zero effective tension" in html
    assert "zero bending moment" in html
    assert "combined tension and bending are not evaluated" in html
    builder.main()
    assert first == {suffix: (output / f"wall-thickness-explorer.{suffix}").read_bytes()
                     for suffix in expected}
    assert first == expected
