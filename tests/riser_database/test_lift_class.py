# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""A lift-class screening reference must not invent physical catalogue data."""

from pathlib import Path

from digitalmodel.riser_database.loader import RiserDatabase


def test_lift_class_is_loadable_and_has_a_public_basis():
    row = RiserDatabase.load().config("riser_lift_screen")
    assert row.riser_type == "lift"
    assert row.analysis_kind == "axial_screen"
    assert row.water_depth_m is None
    assert row.od_mm is None
    assert row.wt_mm is None
    assert row.material_grade == ""
    root = Path(__file__).resolve().parents[2]
    basis = (root / row.source_path).read_text()
    assert "https://www.jstage.jst.go.jp/" in basis
    assert "https://repository.tudelft.nl/" in basis
