"""PIANC bands and bank assumptions: independent boundary and consumer checks."""

import math

import pytest

from digitalmodel.hydrodynamics.parametric_hull_analysis.models import classify_depth
from digitalmodel.hydrodynamics.parametric_hull_analysis.shallow_water import (
    pianc_bank_clearance_width,
)
from digitalmodel.hydrodynamics.parametric_hull_analysis.models import BankSlopeType


@pytest.mark.parametrize("depth, expected", [
    (35, "deep"), (30, "medium"), (18, "medium"), (16, "medium"),
    (15, "shallow"), (14, "shallow"), (12, "very_shallow"),
    (11, "very_shallow"), (math.inf, "deep"),
])
def test_pianc_acceptance_values(depth, expected):
    assert classify_depth(depth, 10).value == expected


@pytest.mark.parametrize("edge, below, above", [
    (3.0, "medium", "deep"), (1.5, "shallow", "medium"),
    (1.2, "very_shallow", "shallow"),
])
def test_exact_edges_and_adjacent_floats(edge, below, above):
    assert classify_depth(math.nextafter(edge, 0), 1).value == below
    assert classify_depth(edge, 1).value == below
    assert classify_depth(math.nextafter(edge, math.inf), 1).value == above


@pytest.mark.parametrize("depth,draft", [
    (math.nan, 10), (-math.inf, 10), (0, 10), (-1, 10),
    (20, 0), (20, -1), (20, math.nan), (20, math.inf), (math.inf, 0),
])
def test_invalid_dimensions_do_not_silently_classify_as_deep(depth, draft):
    with pytest.raises(ValueError):
        classify_depth(depth, draft)


@pytest.mark.parametrize("depth, width, classification, multiplier", [
    (18, 15, "medium", 1), (14, 18, "shallow", 1.2),
    (12, 22.5, "very_shallow", 1.5), (11, 22.5, "very_shallow", 1.5),
])
def test_bank_clearance_and_assumption_metadata(depth, width, classification, multiplier):
    args = dict(beam=30, draft=10, water_depth=depth, speed_knots=10,
                slope=BankSlopeType.MODERATE)
    assert pianc_bank_clearance_width(**args) == pytest.approx(width)
    result = pianc_bank_clearance_width(**args, return_metadata=True)
    assert result["width_m"] == pytest.approx(width)
    assert result["depth_class"] == classification
    assert result["multiplier"] == multiplier
    assert result["multiplier_source"] == "uncited engineering assumption"


@pytest.mark.parametrize("depth, expected", [(18, "medium"), (14, "shallow"), (12, "very_shallow")])
def test_bem_sweep_propagates_pianc_bands_with_stubbed_solver(monkeypatch, tmp_path, depth, expected):
    from types import SimpleNamespace
    from unittest.mock import MagicMock
    from digitalmodel.hydrodynamics.parametric_hull_analysis import sweep
    from digitalmodel.hydrodynamics.parametric_hull_analysis.models import SweepConfig
    import digitalmodel.hydrodynamics.hull_library as library
    import digitalmodel.hydrodynamics.capytaine as bem

    profile = SimpleNamespace(length_bp=100, beam=20, draft=10, block_coefficient=0.7)
    space = SimpleNamespace(fixed_params={}, generate_profiles=lambda _: [("synthetic", profile)])
    monkeypatch.setattr(library, "HullParametricSpace", lambda **_: space)
    monkeypatch.setattr(library, "HullMeshGenerator", MagicMock())
    monkeypatch.setattr(library, "MeshGeneratorConfig", MagicMock())
    monkeypatch.setattr(library, "RAODatabase", MagicMock())
    monkeypatch.setattr(library, "export_scaled_gdf", MagicMock())
    monkeypatch.setattr(bem, "run_bem_analysis", MagicMock())
    monkeypatch.setattr(bem, "compute_rao", MagicMock())
    for helper in ("_build_wave_conditions", "_build_solver_config", "_profile_to_body_config",
                   "_to_rao_db_entry"):
        monkeypatch.setattr(sweep, helper, MagicMock())
    entries, _ = sweep.run_parametric_sweep(
        SweepConfig(base_hull_id="synthetic", water_depth=depth), object(), tmp_path,
    )
    assert entries[0].depth_class.value == expected
    assert sweep.sweep_to_dataframe(entries).iloc[0]["depth_class"] == expected


@pytest.mark.parametrize("slope, source", [
    (BankSlopeType.MODERATE, "table"), ("unlisted", "default 0.5"),
])
def test_bank_fraction_source_is_exposed(slope, source):
    args = dict(beam=30, draft=10, water_depth=18, speed_knots=10, slope=slope)
    result = pianc_bank_clearance_width(**args, return_metadata=True)
    assert result["fraction"] == 0.5
    assert result["fraction_source"] == source
    assert result["width_m"] == pianc_bank_clearance_width(**args)
