# ABOUTME: Tests for the stiffened-panel design-window grid builder
# ABOUTME: (panel_design_explorer.py): shape, monotonicity, allowable boundary.
"""Tests for digitalmodel.structural.panel_design_explorer."""

import pytest

from digitalmodel.structural.panel_design_explorer import (
    ExplorerConfig,
    build_design_grids,
    combo_key,
    MODE_CODE,
)


@pytest.fixture(scope="module")
def small_grids():
    # Small config -> fast, deterministic.
    cfg = ExplorerConfig(
        grades=["Grade A", "AH36"],
        spacings=[700.0],
        spans=[2400.0, 3600.0],
        thickness_min=8.0, thickness_max=16.0, thickness_step=2.0,
        load_min=0.0, load_max=200.0, load_step=50.0,
    )
    return cfg, build_design_grids(cfg)


def test_structure_and_keys(small_grids):
    cfg, d = small_grids
    assert set(d.keys()) >= {"meta", "thickness", "load", "grades", "spans", "grids"}
    # combo count = grades x profiles x spacings x spans
    expected = (len(cfg.grades) * len(cfg.profiles)
                * len(cfg.spacings) * len(cfg.spans))
    assert len(d["grids"]) == expected
    assert combo_key("AH36", "T400x120", 700.0, 2400.0) in d["grids"]


def test_grid_dimensions(small_grids):
    cfg, d = small_grids
    nt, nl = len(d["thickness"]), len(d["load"])
    for g in d["grids"].values():
        assert len(g["util"]) == nl          # rows = loads
        assert all(len(row) == nt for row in g["util"])
        assert len(g["mode"]) == nl
        assert len(g["allow"]) == nt          # one allowable per thickness


def test_utilisation_increases_with_load(small_grids):
    cfg, d = small_grids
    g = d["grids"][combo_key("AH36", "T400x120", 700.0, 2400.0)]
    col = 2  # a fixed thickness column
    util_by_load = [g["util"][j][col] for j in range(len(d["load"]))]
    assert util_by_load == sorted(util_by_load)   # non-decreasing in load


def test_zero_load_zero_utilisation(small_grids):
    cfg, d = small_grids
    j0 = d["load"].index(0.0)
    for g in d["grids"].values():
        assert all(u == pytest.approx(0.0) for u in g["util"][j0])


def test_allowable_boundary_consistent(small_grids):
    cfg, d = small_grids
    g = d["grids"][combo_key("AH36", "T400x120", 700.0, 2400.0)]
    for i, t in enumerate(d["thickness"]):
        allow = g["allow"][i]
        if allow is None:
            continue
        j = d["load"].index(allow)
        assert g["util"][j][i] <= 1.0 + 1e-9     # allowable load passes
        if j + 1 < len(d["load"]):
            assert g["util"][j + 1][i] > 1.0     # next load step fails


def test_modes_are_valid_codes(small_grids):
    cfg, d = small_grids
    valid = set(MODE_CODE.values())
    for g in d["grids"].values():
        for row in g["mode"]:
            assert all(m in valid for m in row)


def test_higher_grade_allows_more_load(small_grids):
    cfg, d = small_grids
    a = d["grids"][combo_key("Grade A", "T400x120", 700.0, 2400.0)]
    b = d["grids"][combo_key("AH36", "T400x120", 700.0, 2400.0)]
    # At each thickness, the higher-yield grade's allowable load is >= Grade A.
    for i in range(len(d["thickness"])):
        if a["allow"][i] is not None and b["allow"][i] is not None:
            assert b["allow"][i] >= a["allow"][i]


def test_longer_span_never_allows_more_load(small_grids):
    cfg, d = small_grids
    short = d["grids"][combo_key("AH36", "T400x120", 700.0, 2400.0)]
    longp = d["grids"][combo_key("AH36", "T400x120", 700.0, 3600.0)]
    # A longer frame span worsens column/tripping -> allowable load cannot rise.
    for i in range(len(d["thickness"])):
        if short["allow"][i] is not None and longp["allow"][i] is not None:
            assert longp["allow"][i] <= short["allow"][i]
