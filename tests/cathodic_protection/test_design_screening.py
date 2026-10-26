# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Geometric reach checks use explicit governing points and local anodes."""

import json

import pytest

from digitalmodel.cathodic_protection.design_screening import (
    DEFAULT_REACH_M,
    screen_current_distribution,
)


def screen(anodes, points, reach=5):
    """Screen one active zone with coordinates expressed in metres."""
    return screen_current_distribution(
        {"reach_m": reach, "reach_basis": "Specified synthetic geometric criterion", "zones": {"steel": {
            "anode_positions_m": anodes, "governing_points_m": points,
        }}},
        ["steel"],
    )["zones"]["steel"]


def test_boundary_reach_and_spacing_pass():
    result = screen([[0, 0, 0], [10, 0, 0]], [[5, 0, 0]])
    assert result["status"] == "PASS"
    assert result["spacing_margin_m"] == 0
    assert result["reach_margin_m"] == 0
    assert result["margin_m"] == 0
    assert result["max_spacing_m"] == 10
    assert result["reach_basis"] == "Specified synthetic geometric criterion"


def test_every_point_uses_nearest_anode_in_three_dimensions():
    result = screen([[0, 0, 0], [10, 0, 0]], [[0, 3, 4], [10, 0, 6]])
    assert result["status"] == "FLAG"
    assert result["reach_margin_m"] == -1
    assert [p["distance_m"] for p in result["points"]] == [5, 6]
    assert [p["nearest_anode_index"] for p in result["points"]] == [0, 1]
    assert result["margin_m"] == -1


def test_spacing_flags_separated_clusters_even_when_points_are_close():
    result = screen([[0, 0, 0], [1, 0, 0], [20, 0, 0], [21, 0, 0]],
                    [[0, 0, 0], [21, 0, 0]])
    assert result["reach_margin_m"] == 5
    assert result["max_spacing_m"] == 19
    assert result["spacing_margin_m"] == -9
    assert result["status"] == "FLAG"


def test_adjacent_spacing_does_not_use_total_array_span():
    result = screen([[x, 0, 0] for x in (0, 10, 20, 30)], [[15, 0, 0]])
    assert result["status"] == "PASS"
    assert result["max_spacing_m"] == 10


def test_one_anode_still_checks_reach():
    result = screen([[0, 0, 0]], [[0, 0, 3]])
    assert result["status"] == "PASS"
    assert result["max_spacing_m"] == 0
    assert result["margin_m"] == 2


@pytest.mark.parametrize("anodes,points", [([], [[0, 0, 0]]), ([[0, 0, 0]], []), ([], [])])
def test_incomplete_geometry_flags_with_no_nonfinite_json(anodes, points):
    result = screen(anodes, points)
    assert result["status"] == "FLAG"
    assert result["margin_m"] is None
    assert result["reasons"]
    json.dumps(result, allow_nan=False)


def test_unspecified_protected_zone_is_flagged():
    result = screen_current_distribution({"zones": {}}, ["steel", "remote"])
    assert set(result["zones"]) == {"steel", "remote"}
    assert all(z["status"] == "FLAG" for z in result["zones"].values())
    assert result["reach_m"] == DEFAULT_REACH_M == 4
    assert "ships.yml" in result["reach_basis"]


def test_zone_reach_override():
    result = screen_current_distribution({"reach_m": 1, "zones": {"steel": {
        "reach_m": 6, "reach_basis": "Specified local geometric criterion",
        "anode_positions_m": [[0, 0, 0]],
        "governing_points_m": [[0, 0, 5]],
    }}}, ["steel"])
    assert result["zones"]["steel"]["margin_m"] == 1
    assert result["zones"]["steel"]["status"] == "PASS"


@pytest.mark.parametrize("criterion", [
    {}, {"reach_basis": "A basis without an explicit reach"},
    {"reach_m": 4}, {"reach_m": 4, "reach_basis": None},
    {"reach_m": 4, "reach_basis": ""}, {"reach_m": 4, "reach_basis": " \t\n"},
])
def test_unqualified_reach_flags_even_when_geometric_limits_are_met(criterion):
    result = screen_current_distribution({**criterion, "zones": {"steel": {
        "anode_positions_m": [[0, 0, 0]], "governing_points_m": [[0, 0, 4]],
    }}}, ["steel"])["zones"]["steel"]
    assert result["status"] == "FLAG"
    assert any("basis" in reason for reason in result["reasons"])
    assert result["reach_basis"] is None
    # One anode: spacing = 0; reach margin = 4 - 4 = 0; limit = 2 * 4 = 8 m.
    assert result["max_spacing_m"] == pytest.approx(0)
    assert result["reach_margin_m"] == pytest.approx(0)
    assert result["spacing_limit_m"] == pytest.approx(8)
    json.dumps(result, allow_nan=False)


def test_zone_override_cannot_borrow_the_global_reach_basis():
    result = screen_current_distribution({
        "reach_m": 1, "reach_basis": "Basis for the global one-metre criterion",
        "zones": {"steel": {"reach_m": 6, "anode_positions_m": [[0, 0, 0]],
                            "governing_points_m": [[0, 0, 5]]}},
    }, ["steel"])["zones"]["steel"]
    assert result["status"] == "FLAG"
    assert result["reach_margin_m"] == pytest.approx(1)
    assert result["reach_basis"] is None
    assert any("basis" in reason for reason in result["reasons"])


def test_zone_basis_without_any_explicit_reach_cannot_qualify_the_default():
    result = screen_current_distribution({"zones": {"steel": {
        "reach_basis": "A basis without an explicit reach",
        "anode_positions_m": [[0, 0, 0]], "governing_points_m": [[0, 0, 4]],
    }}}, ["steel"])["zones"]["steel"]
    assert result["status"] == "FLAG"
    assert result["reach_basis"] is None


def test_zone_inherits_a_global_reach_and_its_basis():
    result = screen_current_distribution({
        "reach_m": 5, "reach_basis": "  Specified global geometric criterion  ",
        "zones": {"steel": {"anode_positions_m": [[0, 0, 0]],
                            "governing_points_m": [[0, 0, 5]]}},
    }, ["steel"])
    zone = result["zones"]["steel"]
    assert zone["status"] == "PASS"
    assert zone["reach_basis"] == result["reach_basis"] == "Specified global geometric criterion"


@pytest.mark.parametrize("local", [False, True])
@pytest.mark.parametrize("basis", [True, 4, [], {}, float("nan")])
def test_non_text_reach_basis_is_rejected(basis, local):
    config = {"reach_m": 5, "zones": {"steel": {
        "anode_positions_m": [[0, 0, 0]], "governing_points_m": [[0, 0, 4]],
    }}}
    target = config["zones"]["steel"] if local else config
    target["reach_basis"] = basis
    with pytest.raises(ValueError, match="reach_basis"):
        screen_current_distribution(config, ["steel"])


@pytest.mark.parametrize("reach", [0, -1, float("nan"), float("inf"), True, "bad"])
def test_invalid_reach_is_rejected(reach):
    with pytest.raises(ValueError, match="reach_m"):
        screen([[0, 0, 0]], [[0, 0, 0]], reach)


@pytest.mark.parametrize("point", [[1, 2], [1, 2, 3, 4], [1, 2, float("nan")],
                                   [True, 0, 0], "bad"])
def test_invalid_coordinates_are_rejected(point):
    with pytest.raises(ValueError, match="coordinates"):
        screen([[0, 0, 0]], [point])


def test_unknown_zone_is_rejected():
    with pytest.raises(ValueError, match="unknown.*zone"):
        screen_current_distribution({"zones": {"typo": {}}}, ["steel"])


@pytest.mark.parametrize("config", [
    {"reach": 5}, {"zones": []}, {"zones": {"steel": None}},
    {"zones": {"steel": {"surface_points": []}}},
])
def test_malformed_screen_inputs_are_rejected(config):
    with pytest.raises(ValueError, match="current_distribution"):
        screen_current_distribution(config, ["steel"])
