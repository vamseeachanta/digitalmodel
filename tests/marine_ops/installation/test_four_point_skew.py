# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent equilibrium, compatibility and workflow checks for #2667."""

import math
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.marine_ops.installation.four_point_skew import (
    FOUR_LEG_MINIMUM_SKL,
    FOUR_LEG_MINIMUM_SOURCE,
    calculate_skew,
    solve_case,
    tolerance_corners,
)


def symmetric():
    # Radius 5 m and height sqrt(75) m give L=10 m, 30 deg from vertical.
    a = 5 / math.sqrt(2)
    return {
        "lift_points_m": [[-a, -a, 0], [a, -a, 0], [a, a, 0], [-a, a, 0]],
        "hook_height_m": math.sqrt(75),
        "weight_N": 981000.0,
        "sling_EA_N": [150e6] * 4,
        "sling_length_m": [10.0] * 4,
        "cog_envelope_xy_m": [[0, 0]],
        "tolerances": {"general_length_m": 0.01, "padeye_elevation_m": 0},
    }


def test_centred_equal_quarters():
    result = solve_case(symmetric())
    assert result["vertical_load_share"] == pytest.approx([0.25] * 4)
    assert result["tensions_N"] == pytest.approx([981000 / (4 * math.cos(math.pi / 6))] * 4)
    assert result["slack"] == [False] * 4


@pytest.mark.parametrize("tau_mm", [5, 10, 15, 19, 25])
def test_hand_rigid_diagonal_closed_form_and_slack(tau_mm):
    tau = tau_mm / 1000
    result = solve_case(symmetric(), length_errors_m=[-tau, tau, -tau, tau])
    nominal = 981000 / (4 * math.cos(math.pi / 6))
    extra = min(15e6 * tau, nominal)
    assert result["tensions_N"] == pytest.approx(
        [nominal + extra, nominal - extra] * 2, rel=0.001, abs=1e-6
    )
    assert result["slack"] == [False, tau_mm >= 19, False, tau_mm >= 19]


def test_hand_flexible_series_compliance():
    settings = symmetric()
    settings["payload_warp_stiffness_N_per_m"] = 15e6
    result = solve_case(settings, length_errors_m=[-0.01, 0.01] * 2)
    nominal = 981000 / (4 * math.cos(math.pi / 6))
    # Independent series calculation: 0.01 / (10/150e6 + 0.75/15e6).
    delta = 85714.28571428571
    assert result["tensions_N"] == pytest.approx([nominal + delta, nominal - delta] * 2)


def test_published_worked_example_two_supporting_legs():
    # SLP Sling Catalog, undated, printed p.93 (web p.99), example:
    # 10,000 lbf / two supporting legs / 60 deg from horizontal -> 5,775 lbf.
    # https://www.sharrowlifting.com/wp-content/themes/sharrowliftingproducts/assets/flipbook/files/simple/99.html
    # Embed the published two-leg case as the slack-diagonal limit of four legs.
    settings = symmetric()
    settings["weight_N"] = 10000 * 4.4482216152605
    result = solve_case(settings, length_errors_m=[-0.1, 0.1] * 2)
    assert np.array(result["tensions_N"])[[0, 2]] / 4.4482216152605 == pytest.approx(
        [5775, 5775], rel=0.001
    )
    assert result["tensions_N"][1::2] == pytest.approx([0, 0], abs=1e-7)


def test_offset_cog_rejects_planted_equal_split():
    settings = symmetric()
    settings["cog_envelope_xy_m"] = [[0.7, 0]]
    result = solve_case(settings)
    # Full body-frame moment equilibrium includes the horizontal weight
    # component after finite rotation; a level-plane reaction shortcut does not.
    points = np.array(settings["lift_points_m"])
    hook = np.array([0, 0, settings["hook_height_m"]])
    directions = hook - points
    directions /= np.linalg.norm(directions, axis=1)[:, None]
    cog = np.array([0.7, 0, 0])
    vertical = (hook - cog) / np.linalg.norm(hook - cog)
    tension = np.array(result["tensions_N"])
    assert tension @ directions == pytest.approx(settings["weight_N"] * vertical, abs=1e-7)
    assert np.cross(points - cog, tension[:, None] * directions).sum(axis=0) == pytest.approx([0, 0, 0], abs=1e-6)
    assert result["vertical_load_share"] != pytest.approx([0.25] * 4)


def test_raised_diagonal_padeyes_reduce_their_tension():
    settings = symmetric()
    dz = 0.005
    result = solve_case(settings, elevation_errors_m=[dz, -dz] * 2)
    nominal = 981000 / (4 * math.cos(math.pi / 6))
    delta = dz * math.cos(math.pi / 6) * 15e6
    assert result["tensions_N"] == pytest.approx([nominal-delta, nominal+delta] * 2)


def test_unequal_stiffness_compatibility_and_full_force_balance():
    settings = symmetric()
    settings["sling_EA_N"] = [100e6, 150e6, 200e6, 250e6]
    result = solve_case(settings)
    tension = np.array(result["tensions_N"])
    n = np.array([1, -1, 1, -1])
    assert n @ (tension * np.array(settings["sling_length_m"]) / settings["sling_EA_N"]) == pytest.approx(0, abs=1e-10)
    points = np.array(settings["lift_points_m"])
    directions = np.array([0, 0, settings["hook_height_m"]]) - points
    directions /= np.linalg.norm(directions, axis=1)[:, None]
    assert tension @ directions == pytest.approx([0, 0, settings["weight_N"]], abs=1e-6)


def test_oblique_offset_geometry_against_displacement_solution():
    settings = symmetric()
    points = np.array([[-3., -2., 0.], [4., -1., 0.],
                       [2., 3., 0.], [-2., 2., 0.]])
    cog = np.array([0.4, 0.3])
    hook = np.array([*cog, 8.])
    directions = hook - points
    lengths = np.linalg.norm(directions, axis=1)
    directions /= lengths[:, None]
    settings.update(lift_points_m=points.tolist(), hook_height_m=8.,
                    cog_envelope_xy_m=[cog.tolist()],
                    sling_length_m=lengths.tolist(),
                    sling_EA_N=[100e6, 150e6, 200e6, 250e6])
    errors = np.array([-0.003, 0.002, -0.001, 0.004])
    result = solve_case(settings, length_errors_m=errors)
    tension = np.array(result["tensions_N"])
    # Independent displacement/stiffness method, without a self-stress mode.
    k = np.array(settings["sling_EA_N"]) / lengths
    force = np.array([0., 0., settings["weight_N"]])
    displacement = np.linalg.solve(directions.T @ (k[:, None] * directions),
                                   force + directions.T @ (k * errors))
    expected = k * (directions @ displacement - errors)
    assert tension == pytest.approx(expected, rel=1e-10)
    assert tension @ directions == pytest.approx(force, abs=1e-7)
    arms = points - np.array([*cog, 0.])
    assert np.cross(arms, tension[:, None] * directions).sum(axis=0) == pytest.approx([0.] * 3, abs=1e-7)


def test_single_slack_leg_has_equilibrium_and_unilateral_compatibility():
    settings = symmetric()
    cog = np.array([0.7, 0.2])
    points = np.array(settings["lift_points_m"])
    directions = np.array([*cog, settings["hook_height_m"]]) - points
    lengths = np.linalg.norm(directions, axis=1)
    directions /= lengths[:, None]
    settings.update(cog_envelope_xy_m=[cog.tolist()], sling_length_m=lengths.tolist())
    errors = np.array([0.1, 0., 0., 0.])
    result = solve_case(settings, length_errors_m=errors)
    assert result["slack"] == [True, False, False, False]
    active = np.array([1, 2, 3])
    # Three supporting legs are statically determinate.
    expected = np.linalg.solve(directions[active].T, [0., 0., settings["weight_N"]])
    tension = np.array(result["tensions_N"])
    assert tension[active] == pytest.approx(expected, rel=1e-10)
    assert tension[0] == pytest.approx(0, abs=1e-7)
    extension = tension * lengths / settings["sling_EA_N"]
    displacement = np.linalg.solve(directions[active], (extension + errors)[active])
    assert directions[0] @ displacement - errors[0] < 0


def test_corner_envelope_and_code_floor():
    settings = symmetric()
    settings["cog_envelope_xy_m"] = [[0, 0], [0.5, 0], [-0.5, 0]]
    settings["tolerances"] = {"general_length_m": 0.005, "padeye_elevation_m": 0.001}
    result = calculate_skew(settings)
    assert len(result["cases"]) == 3 * 16 * 16
    assert {tuple(row["cog_xy_m"]) for row in result["cases"]} == {(0, 0), (0.5, 0), (-0.5, 0)}
    assert result["design_skl"] == max(FOUR_LEG_MINIMUM_SKL, result["effective_skl"])
    settings["code_minimum_skl"] = 3
    assert calculate_skew(settings)["design_skl"] == 3


def test_matched_pair_difference_is_a_constraint_not_added_error():
    settings = symmetric()
    settings["tolerances"] = {"general_length_m": 0.01, "matched_pair_difference_m": 0.002, "padeye_elevation_m": 0}
    corners = tolerance_corners(settings)
    assert len(corners) == 36
    assert any(abs(e[0] - e[2]) == pytest.approx(0.002) for e, _ in corners)
    for errors, elevations in corners:
        assert max(abs(v) for v in errors) <= 0.0100000001
        assert abs(errors[0] - errors[2]) <= 0.0020000001
        assert abs(errors[1] - errors[3]) <= 0.0020000001
        assert elevations == [0.0] * 4
    # Pair matching does not constrain the DIFFERENCE between diagonal pairs.
    assert calculate_skew(settings)["effective_skl"] > 1.5


def test_nominal_is_per_cog():
    settings = symmetric()
    settings["cog_envelope_xy_m"] = [[0, 0], [0.7, 0.2]]
    result = calculate_skew(settings)
    assert result["effective_skl"] > 1
    assert len(result["cases"]) == 32
    assert result["cases"][16]["nominal_tensions_N"] != pytest.approx(result["cases"][0]["nominal_tensions_N"])


def test_translation_rotation_and_unequal_nominal_lengths():
    settings = symmetric()
    settings["sling_length_m"] = [9.998, 10.002] * 2
    reference = solve_case(settings)
    angle = 0.72
    rotation = np.array([[math.cos(angle), -math.sin(angle)],
                         [math.sin(angle), math.cos(angle)]])
    points = np.array(settings["lift_points_m"])
    points[:, :2] = points[:, :2] @ rotation.T + [1000, -2000]
    settings["lift_points_m"] = points.tolist()
    settings["cog_envelope_xy_m"] = [[1000, -2000]]
    assert solve_case(settings)["tensions_N"] == pytest.approx(reference["tensions_N"], rel=1e-10)
    equal = 981000 / (4 * math.cos(math.pi / 6))
    assert reference["tensions_N"][0] > equal
    assert reference["tensions_N"][1] < equal


def test_flexibility_monotonicity_and_rigid_limit():
    settings = symmetric()
    tensions = []
    for kw in [1e3, 1e6, 1e9, 1e15]:
        settings["payload_warp_stiffness_N_per_m"] = kw
        tensions.append(solve_case(settings, length_errors_m=[-0.01, 0.01] * 2)["tensions_N"][0])
    rigid = solve_case(symmetric(), length_errors_m=[-0.01, 0.01] * 2)["tensions_N"][0]
    assert tensions == sorted(tensions)
    assert tensions[-1] == pytest.approx(rigid, rel=1e-7)


@pytest.mark.parametrize("tolerances", [
    {"matched_pair_difference_m": 0.001},
    {"general_length_m": -0.01},
    {"general_length_m": 0.01, "matched_pair_difference_m": float("inf")},
    {"padeye_elevation_m": [0, 0, float("nan"), 0]},
])
def test_invalid_tolerance_bounds(tolerances):
    settings = symmetric()
    settings["tolerances"] = tolerances
    with pytest.raises(ValueError):
        calculate_skew(settings)


def test_nominal_slack_is_not_silently_omitted_from_ratio():
    settings = symmetric()
    settings["sling_length_m"] = [9.9, 10.1] * 2
    with pytest.raises(ValueError, match="nominal sling is slack"):
        calculate_skew(settings)


@pytest.mark.parametrize("field,value", [
    ("weight_N", 0), ("weight_N", float("nan")),
    ("hook_height_m", 0), ("code_minimum_skl", 0.9),
    ("sling_EA_N", [150e6, -1, 150e6, 150e6]),
    ("sling_length_m", [10, 10, 10]),
    ("cog_envelope_xy_m", []), ("code_minimum_source", ""),
    ("payload_warp_stiffness_N_per_m", 0),
])
def test_invalid_input_fails_closed(field, value):
    settings = symmetric()
    settings[field] = value
    with pytest.raises(ValueError):
        calculate_skew(settings)


def test_unsupported_geometry_and_flexibility_fail_closed():
    for kind in ["noncoplanar", "collinear", "outside", "flex_offset", "unknown"]:
        settings = symmetric()
        if kind == "noncoplanar":
            settings["lift_points_m"][0][2] = 1
        elif kind == "collinear":
            settings["lift_points_m"] = [[i, 0, 0] for i in range(4)]
        elif kind == "outside":
            settings["cog_envelope_xy_m"] = [[20, 0]]
        elif kind == "flex_offset":
            settings["payload_warp_stiffness_N_per_m"] = 1e7
            settings["cog_envelope_xy_m"] = [[0.2, 0]]
        else:
            settings["tolerances"] = {"length_typo_m": 1}
        with pytest.raises(ValueError):
            calculate_skew(settings)


def finite_rotation_reference(settings, cog):
    """Independent body-frame statics with exact stretched sling lengths.

    The rigid payload rotates until hook-to-CoG is vertical. In body axes the
    weight direction is therefore (H-CoG)/|H-CoG|. Solve three force equations
    for H; Hooke's law supplies tensions without any self-stress projection.
    """
    from scipy.optimize import root

    points = np.array(settings["lift_points_m"])
    lengths = np.array(settings["sling_length_m"])
    stiffness = np.array(settings["sling_EA_N"]) / lengths
    centre = np.r_[cog, points[0, 2]]

    def state(hook):
        arms = hook - points
        distances = np.linalg.norm(arms, axis=1)
        tensions = np.maximum(stiffness * (distances - lengths), 0)
        down = (hook - centre) / np.linalg.norm(hook - centre)
        residual = tensions @ (arms / distances[:, None]) - settings["weight_N"] * down
        return residual / settings["weight_N"], tensions

    solved = root(lambda hook: state(hook)[0], [0, 0, settings["hook_height_m"] + 0.03], tol=1e-10)
    assert solved.success, solved.message
    residual, tensions = state(solved.x)
    assert np.linalg.norm(residual) < 1e-9
    return tensions


@pytest.mark.parametrize("cog", [[0.5, 0.5], [0.7, 0.7]])
def test_diagonal_cog_against_independent_finite_rotation(cog):
    settings = symmetric()
    reference = finite_rotation_reference(settings, cog)
    if cog == [0.5, 0.5]:
        # Rounded independent reference supplied by the review, kN.
        assert reference / 1000 == pytest.approx([202.1, 282.1, 362.0, 282.1], abs=0.1)
    actual = solve_case(settings, cog_xy_m=cog)
    assert actual["tensions_N"] == pytest.approx(reference, rel=0.003)


def test_axis_tilt_fixes_level_geometry_understatement():
    settings = symmetric()
    result = solve_case(settings, cog_xy_m=[0.7, 0])
    reference = finite_rotation_reference(settings, [0.7, 0])
    assert result["tilt_deg"] == pytest.approx(4.621, abs=0.01)
    assert result["tilt_exceeds_3_deg"] is True
    assert max(reference) / 1000 == pytest.approx(338.0, abs=0.1)
    assert result["tensions_N"] == pytest.approx(reference, rel=0.003)
    settings["cog_envelope_xy_m"] = [[0, 0], [0.7, 0]]
    assert calculate_skew(settings)["tilt_exceeds_3_deg"] is True


def test_code_floor_reads_screen_value_without_importing_solver_package():
    import ast

    path = Path(__file__).resolve().parents[3] / "src/digitalmodel/orcaflex/installation_analysis.py"
    assignments = {
        node.targets[0].id: ast.literal_eval(node.value)
        for node in ast.parse(path.read_text()).body
        if isinstance(node, ast.Assign) and isinstance(node.targets[0], ast.Name)
        and node.targets[0].id in {"_SKL_BY_LEG_COUNT", "_SKL_REFERENCE"}
    }
    assert FOUR_LEG_MINIMUM_SKL == assignments["_SKL_BY_LEG_COUNT"][4] == 1.33
    assert FOUR_LEG_MINIMUM_SOURCE == assignments["_SKL_REFERENCE"]
    settings = symmetric()
    settings["tolerances"] = {"general_length_m": 0.0001, "padeye_elevation_m": 0}
    assert calculate_skew(settings)["design_skl"] == FOUR_LEG_MINIMUM_SKL
    settings = symmetric()
    settings.pop("tolerances")
    assert calculate_skew(settings)["effective_skl"] > 1
    assert len(calculate_skew(settings)["cases"]) == 16 * 16


@pytest.mark.parametrize("override", [
    {"code_minimum_skl": 1.25},
    {"code_minimum_source": "Free text without a value"},
    {"tolerances": {"general_length_m": 0}},
    {"tolerances": {"general_length_m": [0.01, 0, 0.01, 0.01]}},
    {"sling_length_m": [11] * 4},
])
def test_new_safety_bounds(override):
    settings = symmetric()
    settings.update(override)
    with pytest.raises(ValueError):
        calculate_skew(settings)


def test_matched_set_leg_to_leg_difference_conversion():
    settings = symmetric()
    delta = 0.02
    settings["tolerances"] = {"general_length_m": delta / 2, "padeye_elevation_m": 0}
    result = calculate_skew(settings)
    nominal = 981000 / (4 * math.cos(math.pi / 6))
    assert result["effective_skl"] == pytest.approx((nominal + 15e6 * delta / 2) / nominal)
    # Matching opposite partners alone still permits this full between-pair gap.
    settings["tolerances"]["matched_pair_difference_m"] = 0
    assert calculate_skew(settings)["effective_skl"] == pytest.approx(result["effective_skl"])


def test_design_outputs_retain_cog_share_and_per_leg_envelope():
    settings = symmetric()
    result = calculate_skew(settings)
    assert result["max_nominal_vertical_share"] == pytest.approx(0.25)
    assert result["governing_cog_xy_m"] == [0, 0]
    assert result["design_leg_tensions_N"] == pytest.approx([433190.307] * 4)
    settings["cog_envelope_xy_m"] = [[0, 0], [0.5, 0.5], [-0.7, 0]]
    result = calculate_skew(settings)
    nominals = [solve_case(settings, cog_xy_m=cog) for cog in settings["cog_envelope_xy_m"]]
    shares = [max(row["vertical_load_share"]) for row in nominals]
    assert result["max_nominal_vertical_share"] == pytest.approx(max(shares))
    assert result["governing_cog_xy_m"] == settings["cog_envelope_xy_m"][np.argmax(shares)]
    expected = np.max([row["tensions_N"] for row in nominals], axis=0) * result["design_skl"]
    assert result["design_leg_tensions_N"] == pytest.approx(expected)
    assert np.all(np.max([row["tensions_N"] for row in result["cases"]], axis=0) <= expected + 1e-7)
