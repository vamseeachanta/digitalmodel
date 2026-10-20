# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent references and screening regressions; see workflow README citations."""

import json
import math
import subprocess
import sys
from dataclasses import replace
from pathlib import Path

import numpy as np
import pytest
import yaml

from digitalmodel.well.drilling.wellbore_stability import (
    StabilityInput,
    _band,
    failure_margin,
    kirsch_wall,
    pressure_bounds,
    run_workflow,
    stress_in_well_axes,
)


def reference(**changes):
    # Sv and nu are explicit completion assumptions: the text's 2D problems
    # omit them. They preserve the published collapse-sector ordering and
    # the minimum hoop stress at tensile initiation.
    return replace(
        StabilityInput(
            sv_psi=7000,
            shmax_psi=6300,
            shmin_psi=4300,
            shmax_azimuth_deg=0,
            pore_pressure_psi=3080,
            ucs_psi=3500,
            friction_angle_deg=math.degrees(math.atan(0.6)),
            poisson_ratio=0.25,
            tensile_strength_psi=800,
            inclination_deg=0,
            azimuth_deg=0,
        ),
        **changes,
    )


def test_textbook_problem_6_1_collapse():
    # Introduction to Energy Geomechanics (2024-08-28), web p. node46,
    # §6.3 Problem 6.1: published rounded result 4279 psi. Tolerance 2 psi.
    out = pressure_bounds(reference())
    assert out["collapse"]["pressure_psi"] == pytest.approx(4279, abs=2)


def test_textbook_problem_6_3_fracture():
    # Same text, web p. node47, §6.4 Problem 6.3: 4320 psi. Tolerance 0.01 psi.
    out = pressure_bounds(reference())
    assert out["fracture_initiation"]["pressure_psi"] == pytest.approx(4320, abs=0.01)


def test_crossing_shear_bounds_do_not_hide_textbook_collapse_or_claim_window():
    # The textbook evaluates the collapse-prone hoop/radial sector. The full
    # tensor also finds renewed shear at the opposite sector before that bound.
    out = pressure_bounds(reference())
    assert out["collapse"]["pressure_psi"] == pytest.approx(4279, abs=2)
    # At theta=0, hoop=440-dP and axial=2920 psi. At recurrent shear,
    # UCS + q*hoop = axial; axial is now the major principal stress.
    q = (math.sqrt(1 + 0.6**2) + 0.6) ** 2
    assert out["shear_upper"]["pressure_psi"] == pytest.approx(
        3080 + 440 + (3500 - 2920) / q, abs=1e-6
    )
    assert out["shear_upper"]["pressure_psi"] < out["collapse"]["pressure_psi"]
    assert not out["feasible"]


def test_vertical_isotropic_closed_forms():
    case = reference(
        shmax_psi=5000, shmin_psi=5000, sv_psi=6000, ucs_psi=2000, friction_angle_deg=30
    )
    bounds = pressure_bounds(case)
    assert bounds["collapse"]["pressure_psi"] == pytest.approx(3540, abs=0.01)
    assert bounds["fracture_initiation"]["pressure_psi"] == pytest.approx(
        7720, abs=0.01
    )
    wall = kirsch_wall(case, 4000, np.array([0, 47, 90]))
    assert wall["radial_psi"] == pytest.approx([920] * 3)
    assert wall["hoop_psi"] == pytest.approx([2920] * 3)
    assert wall["axial_psi"] == pytest.approx([2920] * 3)


def test_rotation_preserves_invariants_and_horizontal_axis_mapping():
    case = reference(inclination_deg=63, azimuth_deg=29, shmax_azimuth_deg=71)
    tensor = stress_in_well_axes(case)
    assert np.linalg.eigvalsh(tensor) == pytest.approx([4300, 6300, 7000])
    assert tensor == pytest.approx(tensor.T)
    horizontal = stress_in_well_axes(reference(inclination_deg=90))
    assert np.diag(horizontal) == pytest.approx([7000, 4300, 6300])


def test_deviated_45_degree_hand_calculation():
    # Rotation at 45 deg gives Sxx=Szz=6650, Sxz=-350, Syy=4300.
    # At theta=90: effective hoop=9490-920, axial=3570+1175,
    # hoop-axial shear=-2*Sxz=700. Independent arithmetic oracle.
    wall = kirsch_wall(reference(inclination_deg=45), 4000, [90])
    assert wall["hoop_psi"][0] == pytest.approx(8570)
    assert wall["axial_psi"][0] == pytest.approx(4745)
    assert wall["hoop_axial_shear_psi"][0] == pytest.approx(700)
    assert wall["radial_psi"][0] == pytest.approx(920)


def test_high_pressure_shear_reversal_is_a_separate_upper_bound():
    case = reference(
        shmax_psi=5000, shmin_psi=5000, sv_psi=6000, ucs_psi=2000, friction_angle_deg=30
    )
    out = pressure_bounds(case)
    assert out["shear_upper"]["pressure_psi"] == pytest.approx(6460, abs=0.01)
    assert out["stable_pressure_intervals_psi"][0] == pytest.approx(
        [3540, 6460], abs=0.01
    )


def test_deviated_planted_defect_and_nonzero_axial_shear():
    case = reference(inclination_deg=60, azimuth_deg=35)
    wall = kirsch_wall(case, 4000, np.array([0, 37, 90]))
    assert np.max(np.abs(wall["hoop_axial_shear_psi"])) > 100
    # Eigenvalues independently assembled from the wall tensor.
    for j in range(3):
        matrix = [
            [wall["radial_psi"][j], 0, 0],
            [0, wall["hoop_psi"][j], wall["hoop_axial_shear_psi"][j]],
            [0, wall["hoop_axial_shear_psi"][j], wall["axial_psi"][j]],
        ]
        assert wall["principal_psi"][j] == pytest.approx(
            np.linalg.eigvalsh(matrix)[::-1]
        )
    tilted = pressure_bounds(case)
    assert abs(tilted["fracture_initiation"]["pressure_psi"] - 4320) > 100
    assert tilted["collapse"] != pressure_bounds(reference())["collapse"]


def test_azimuth_reference_invariance():
    a = pressure_bounds(reference(inclination_deg=45, azimuth_deg=33))
    b = pressure_bounds(
        reference(inclination_deg=45, azimuth_deg=113, shmax_azimuth_deg=80)
    )
    for key in ("collapse", "fracture_initiation"):
        assert a[key]["pressure_psi"] == pytest.approx(b[key]["pressure_psi"], abs=0.01)


def test_allowed_breakout_width_matches_vertical_edge_formula():
    case = reference()
    q = (1 + math.sin(math.radians(case.friction_angle_deg))) / (
        1 - math.sin(math.radians(case.friction_angle_deg))
    )
    expected = 3080 + (4440 + 4000 * math.cos(math.radians(30)) - 3500) / (1 + q)
    bounds = pressure_bounds(case, allowed_breakout_width_deg=30, angular_samples=720)
    # Half-degree width discretization; pressure error < 6 psi here.
    assert bounds["collapse"]["pressure_psi"] == pytest.approx(expected, abs=6)
    assert bounds["collapse"]["pressure_psi"] < 4279


def test_mogi_coefficients_reproduce_uniaxial_and_triaxial_strength():
    case = reference()
    q = (math.sqrt(1 + 0.6**2) + 0.6) ** 2
    principal = np.array([[3500, 0, 0], [3500 + q * 500, 500, 500]])
    assert failure_margin(case, principal, "mohr_coulomb") == pytest.approx(
        [0, 0], abs=1e-10
    )
    assert failure_margin(case, principal, "mogi_coulomb") == pytest.approx(
        [0, 0], abs=1e-10
    )
    assert np.isfinite(
        pressure_bounds(case, criterion="mogi_coulomb")["collapse"]["pressure_psi"]
    )


def test_no_window_is_explicit_and_json_safe():
    bounds = pressure_bounds(reference(ucs_psi=1))
    assert bounds["collapse"]["pressure_psi"] is None
    assert bounds["collapse"]["numerical_status"] == "NO_FEASIBLE_SAMPLED_PRESSURE"
    assert not bounds["feasible"]
    json.dumps(bounds, allow_nan=False)


def test_already_tensile_and_stable_at_pore():
    assert (
        pressure_bounds(reference(shmax_psi=14000))["fracture_initiation"][
            "numerical_status"
        ]
        == "ALREADY_TENSILE_AT_ZERO"
    )
    out = pressure_bounds(reference(ucs_psi=15000))
    assert out["collapse"]["pressure_psi"] == 3080
    assert out["collapse"]["numerical_status"] == "FLOOR_AT_PORE_PRESSURE"
    assert (
        out["shear_upper"]["numerical_status"]
        == "NO_SHEAR_RECURRENCE_BEFORE_TENSILE_CEILING"
    )


@pytest.mark.parametrize(
    "changes",
    [
        {"pore_pressure_psi": float("nan")},
        {"poisson_ratio": 0.5},
        {"friction_angle_deg": 90},
        {"friction_angle_deg": 89.9999999},
        {"inclination_deg": -1},
        {"shmax_psi": 4000},
        {"tensile_strength_psi": -1},
        {"ucs_psi": 0},
    ],
)
def test_bad_physical_inputs_rejected(changes):
    with pytest.raises(ValueError):
        reference(**changes)


@pytest.mark.parametrize(
    "kwargs",
    [
        {"criterion": "unknown"},
        {"angular_samples": 7},
        {"pressure_samples": 2},
        {"allowed_breakout_width_deg": 61},
        {"allowed_breakout_width_deg": float("nan")},
    ],
)
def test_bad_search_controls_rejected(kwargs):
    with pytest.raises(ValueError):
        pressure_bounds(reference(), **kwargs)


INPUT = Path(__file__).parents[3] / "examples/workflows/wellbore-stability/input.yml"


def test_workflow_four_curves_bands_polar_and_validation_labels(tmp_path):
    cfg = yaml.safe_load(INPUT.read_text())
    cfg["ensemble"]["samples"] = 16
    a = run_workflow(cfg, tmp_path)
    b = run_workflow(cfg, tmp_path / "repeat")
    assert a == b
    assert a["validation_status"] == "NOT_VALIDATED"
    assert a["not_covered"]
    assert a["model"]["validation_status"] == "NOT_VALIDATED"
    assert a["model"]["biot_coefficient"] == 1.0
    assert a["model"]["stress_sign"] == "compression positive"
    assert a["model"]["units"]["pressure"] == "psi"
    assert len(a["window"]) == len(cfg["profile"])
    for row in a["window"]:
        assert set(row["curves"]) == {
            "pore",
            "collapse",
            "fracture_initiation",
            "minimum_horizontal_stress",
        }
        for band in row["curves"].values():
            assert band["validation_status"] == "NOT_VALIDATED"
            if band["p50_psi"] is not None:
                assert band["p10_psi"] <= band["p50_psi"] <= band["p90_psi"]
                assert band["p50_emw_ppg"] == pytest.approx(
                    band["p50_psi"] / (0.052 * row["tvd_ft"])
                )
        assert (
            row["curves"]["collapse"]["bias_direction"] == "none (all draws resolved)"
        )
        floor = max(
            row["curves"]["pore"]["p90_psi"], row["curves"]["collapse"]["p90_psi"]
        )
        assert row["conservative_window"]["low_psi"] == floor
    assert len(a["polar"]) == len(cfg["polar"]["inclinations_deg"]) * len(
        cfg["polar"]["azimuths_deg"]
    )
    assert all(row["validation_status"] == "NOT_VALIDATED" for row in a["polar"])
    assert json.loads((tmp_path / "summary.json").read_text()) == a
    assert json.loads((tmp_path / "polar.json").read_text()) == a["polar"]


def test_uncertainty_does_not_discard_failed_draws(tmp_path):
    cfg = yaml.safe_load(INPUT.read_text())
    cfg["profile"] = [cfg["profile"][0]]
    cfg["profile"][0]["ucs_psi"] = 1
    cfg["ensemble"] = {"samples": 8, "seed": 4, "sigma": {}}
    out = run_workflow(cfg, tmp_path)
    row = out["window"][0]
    assert row["curves"]["collapse"]["p50_psi"] is None
    assert row["curves"]["collapse"]["bias_direction"] == "unknown (unresolved draws)"
    assert row["failed_draws"] == 8
    assert row["conservative_window"]["feasible"] is False


def test_partially_unresolved_ensemble_without_limit_is_indeterminate(tmp_path):
    cfg = yaml.safe_load(INPUT.read_text())
    cfg["profile"] = [cfg["profile"][0]]
    cfg["profile"][0]["ucs_psi"] = 3400
    cfg["ensemble"] = {"samples": 16, "seed": 4, "sigma": {"ucs_psi": 200}}
    row = run_workflow(cfg, tmp_path)["window"][0]
    assert 0 < row["failed_draws"] < 16
    assert row["curves"]["collapse"]["p50_psi"] is None
    assert row["curves"]["collapse"]["numerical_status"] == "INDETERMINATE"
    assert row["curves"]["collapse"]["unresolved_fraction_check"] == "NOT_EVALUATED"
    assert row["curves"]["collapse"]["bias_direction"] == "unknown (unresolved draws)"
    assert row["curves"]["fracture_initiation"]["p50_psi"] == pytest.approx(4320)
    assert row["conservative_window"]["feasible"] is False


@pytest.mark.parametrize(
    "values, limit, expected, status",
    [
        ([100, 150, 200], None, [110, 150, 190], "ALL_DRAWS_RESOLVED"),
        ([100, None, 150, 200], 0.25, [110, 150, 190], "RESOLVED_DRAWS_ONLY"),
        ([100, None, 150, 200], 0.2, [None] * 3, "INDETERMINATE"),
        ([100, None, 150, 200], None, [None] * 3, "INDETERMINATE"),
        ([None] * 4, 1, [None] * 3, "INDETERMINATE"),
        ([150, None], 0.5, [150] * 3, "RESOLVED_DRAWS_ONLY"),
        ([150, None], 0, [None] * 3, "INDETERMINATE"),
    ],
)
def test_resolved_draw_band_hand_calculation(values, limit, expected, status):
    """Synthetic arithmetic oracle; no engineering acceptance limit is assumed.

    Sample Quantiles in Statistical Packages (1996), eq. (1), Definition 7:
    for sorted [100, 150, 200], zero-based h=(n-1)*p is 0.2, 1, 1.8;
    linear interpolation gives 110, 150, 190 psi. One resolved value gives
    that same value at every quantile. One missing of four means f=1/4=0.25;
    the caller's illustrative limit 0.25 includes equality, 0.2 excludes it.
    Missing policy and zero resolved values cannot establish a usable band.
    EMW uses the existing workflow README conversion: 0.052*1000=52 psi/ppg.
    """
    band = _band(values, 1000, max_unresolved_fraction=limit)
    assert band["numerical_status"] == status
    assert band["validation_status"] == "NOT_VALIDATED"
    assert band["quantile_basis"] == "resolved_draws_only"
    assert band["resolved_draws"] == len(values) - values.count(None)
    assert band["unresolved_draws"] == values.count(None)
    assert band["unresolved_fraction"] == values.count(None) / len(values)
    assert band["max_unresolved_fraction"] == limit
    assert band["unresolved_fraction_check"] == (
        "NOT_EVALUATED" if limit is None else "EVALUATED"
    )
    for name, value in zip(("p10", "p50", "p90"), expected):
        assert band[name + "_psi"] == pytest.approx(value)
        assert band[name + "_emw_ppg"] == pytest.approx(
            None if value is None else value / 52
        )
    json.dumps(band, allow_nan=False)


def test_planted_unresolved_draw_keeps_conditional_collapse_band(tmp_path, monkeypatch):
    """Scripted draws [100, None, 150, 200] give f=1/4, Q=[110,150,190].

    Independent interpolation is written out in the band test above (1996,
    Definition 7). The illustrative caller limit is 1/4. The upper-bound
    fixture has one unresolved value too; conditional collapse quantiles
    must not establish a window. Nominal output is a separate fifth call.
    """
    cfg = yaml.safe_load(INPUT.read_text())
    cfg["profile"] = [cfg["profile"][0]]
    cfg["ensemble"] = {"samples": 4, "sigma": {}, "max_unresolved_fraction": 0.25}
    cfg["polar"] = {"inclinations_deg": []}
    pressures = iter([100, None, 150, 200, 150])
    calls = []

    def scripted_bounds(*args, **kwargs):
        calls.append(args[0])
        assert len(calls) <= 5, "expected four draws followed by one nominal call"
        pressure = next(pressures)
        return {
            "collapse": {"pressure_psi": pressure},
            "fracture_initiation": {"pressure_psi": 300},
            "conservative_ceiling_psi": None if pressure is None else 300,
            "feasible": pressure is not None,
        }

    monkeypatch.setattr(
        "digitalmodel.well.drilling.wellbore_stability.pressure_bounds", scripted_bounds
    )
    result = run_workflow(cfg, tmp_path)
    assert len(calls) == 5, "expected four draws followed by one nominal call"
    row = result["window"][0]
    band = row["curves"]["collapse"]
    assert band["bias_direction"] == "unknown (unresolved draws)"
    policy = result["uncertainty"]["failed_draw_policy"]
    assert "bias direction is unknown" in policy
    assert (
        "downward bias is a risk if omitted draws have higher collapse pressures"
        in policy
    )
    assert [band[name + "_psi"] for name in ("p10", "p50", "p90")] == pytest.approx(
        [110, 150, 190]
    )
    assert band["numerical_status"] == "RESOLVED_DRAWS_ONLY"
    assert band["unresolved_draws"] == row["failed_draws"] == 1
    assert band["unresolved_fraction"] == 0.25
    for name in ("pore", "fracture_initiation", "minimum_horizontal_stress"):
        assert row["curves"][name]["unresolved_fraction_check"] == "ALL_DRAWS_REQUIRED"
    assert row["conservative_window"]["low_psi"] is None
    assert row["conservative_window"]["high_psi"] is None
    assert row["conservative_window"]["feasible"] is False
    assert result["screening_only"] is True
    assert result["validation_status"] == "NOT_VALIDATED"
    assert result["uncertainty"]["max_unresolved_fraction"] == 0.25
    assert json.loads((tmp_path / "summary.json").read_text()) == result
    assert json.loads((tmp_path / "window.json").read_text()) == result["window"]


def test_numerical_miss_can_raise_resolved_only_collapse_median(tmp_path, monkeypatch):
    """Synthetic hand oracle: a missed sub-fracture root can raise the median.

    For the narrow case, at theta=90 degrees and dP=1013 psi, the axial,
    hoop and radial stresses are 1005, 2030-1013=1017 and 1013 psi.
    With zero friction, max-min=12=UCS; collapse is 1000+1013=2013 psi.
    The minimum hoop term gives fracture at 1000+1990=2990 psi.
    For the isotropic case q=3 and hoop=8000-dP, so collapse satisfies
    8000-dP=1000+3*dP, giving Pmud=1000+1750=2750 psi.
    The full two-draw median is (2013+2750)/2=2381.5 psi; excluding the
    lower, numerically missed root raises it to 2750 psi. The fraction
    limit 0.5 is an arithmetic fixture, not an engineering acceptance value.
    """
    narrow = reference(
        sv_psi=2005,
        shmax_psi=2010,
        shmin_psi=2000,
        pore_pressure_psi=1000,
        ucs_psi=12,
        friction_angle_deg=0,
        poisson_ratio=0,
        tensile_strength_psi=0,
    )
    isotropic = replace(
        narrow,
        sv_psi=5000,
        shmax_psi=5000,
        shmin_psi=5000,
        ucs_psi=1000,
        friction_angle_deg=30,
    )
    cfg = yaml.safe_load(INPUT.read_text())
    cfg["profile"] = [cfg["profile"][0]]
    cfg["ensemble"] = {"samples": 2, "sigma": {}, "max_unresolved_fraction": 0.5}
    cfg["polar"] = {"inclinations_deg": []}
    cfg["screening"] = {}
    cases = iter([narrow, isotropic, isotropic])
    sampled = []

    def sampled_bounds(*args, **kwargs):
        result = pressure_bounds(next(cases), **kwargs)
        sampled.append(result)
        return result

    monkeypatch.setattr(
        "digitalmodel.well.drilling.wellbore_stability.pressure_bounds", sampled_bounds
    )
    coarse = run_workflow(cfg, tmp_path / "coarse")
    assert len(sampled) == 3
    assert sampled[0]["collapse"]["pressure_psi"] is None
    assert sampled[0]["collapse"]["numerical_status"] == "NO_FEASIBLE_SAMPLED_PRESSURE"
    cases = iter([narrow, isotropic, isotropic])
    sampled = []
    cfg["screening"]["pressure_samples"] = 2049
    refined = run_workflow(cfg, tmp_path / "refined")
    assert len(sampled) == 3
    recovered = sampled[0]
    assert recovered["collapse"]["pressure_psi"] == pytest.approx(2013, abs=1e-6)
    assert recovered["fracture_initiation"]["pressure_psi"] == pytest.approx(2990)
    coarse_row = coarse["window"][0]
    refined_row = refined["window"][0]
    coarse_band = coarse_row["curves"]["collapse"]
    refined_band = refined_row["curves"]["collapse"]
    assert coarse_band["unresolved_draws"] == 1
    assert coarse_band["numerical_status"] == "RESOLVED_DRAWS_ONLY"
    assert coarse_band["p50_psi"] == pytest.approx(2750, abs=1e-6)
    assert refined_band["unresolved_draws"] == 0
    assert refined_band["p50_psi"] == pytest.approx(2381.5, abs=1e-6)
    assert coarse_band["p50_psi"] > refined_band["p50_psi"]
    assert coarse_band["bias_direction"] == "unknown (unresolved draws)"
    assert refined_band["bias_direction"] == "none (all draws resolved)"
    assert coarse_row["conservative_window"]["low_psi"] is None
    assert coarse_row["conservative_window"]["feasible"] is False
    assert coarse["screening_only"] is True
    assert coarse["validation_status"] == "NOT_VALIDATED"
    assert json.loads((tmp_path / "coarse" / "summary.json").read_text()) == coarse


@pytest.mark.parametrize("limit", [-0.1, 1.1, math.nan, math.inf, "0.25", True])
def test_invalid_unresolved_fraction_limit_rejected_before_output(tmp_path, limit):
    """Fractions lie in [0,1] by definition; nonnumeric policies have no meaning."""
    cfg = yaml.safe_load(INPUT.read_text())
    cfg["ensemble"]["max_unresolved_fraction"] = limit
    with pytest.raises(ValueError, match="max_unresolved_fraction"):
        run_workflow(cfg, tmp_path)
    assert not (tmp_path / "summary.json").exists()


def test_breakout_grid_refinement_and_pressure_scan_agree_with_edge_formula():
    case = reference()
    q = (math.sqrt(1 + 0.6**2) + 0.6) ** 2
    width = 31.3  # Deliberately off every tested grid.
    expected = 3080 + (4440 + 4000 * math.cos(math.radians(width)) - 3500) / (1 + q)
    errors = []
    for nodes in (180, 360, 720):
        out = pressure_bounds(
            case, allowed_breakout_width_deg=width, angular_samples=nodes
        )
        errors.append(abs(out["collapse"]["pressure_psi"] - expected))
    assert errors[2] < errors[1] < errors[0]
    assert errors[2] < 6
    dense = pressure_bounds(
        case,
        allowed_breakout_width_deg=width,
        angular_samples=720,
        pressure_samples=257,
    )
    assert dense["collapse"]["pressure_psi"] == pytest.approx(
        out["collapse"]["pressure_psi"], abs=1e-6
    )


@pytest.mark.parametrize(
    "mutation",
    [
        "negative_sigma",
        "unknown_sigma",
        "zero_depth",
        "invalid_draw",
        "polar_singularity",
    ],
)
def test_workflow_rejects_invalid_uncertainty_and_depth(tmp_path, mutation):
    cfg = yaml.safe_load(INPUT.read_text())
    if mutation == "negative_sigma":
        cfg["ensemble"]["sigma"] = {"ucs_psi": -1}
    elif mutation == "unknown_sigma":
        cfg["ensemble"]["sigma"] = {"typo": 1}
    elif mutation == "invalid_draw":
        cfg["ensemble"]["sigma"] = {"poisson_ratio": 100}
    elif mutation == "polar_singularity":
        cfg["polar"]["inclinations_deg"] = [180]
    else:
        cfg["profile"][0]["tvd_ft"] = 0
    with pytest.raises(ValueError):
        run_workflow(cfg, tmp_path)


def test_cli_runs_without_engine_or_licensed_solver(tmp_path):
    result = subprocess.run(
        [
            sys.executable,
            "-m",
            "digitalmodel.well.drilling.wellbore_stability",
            str(INPUT),
            "--output-dir",
            str(tmp_path),
        ],
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    assert (tmp_path / "summary.json").is_file()


@pytest.mark.parametrize("values", [[100, 150], [100, None], [None, None]])
def test_all_draws_required_band_reports_its_policy(values):
    band = _band(values, 1000, require_all_draws=True)
    assert band["unresolved_fraction_check"] == "ALL_DRAWS_REQUIRED"
    assert band["indeterminate_reason"] == (
        "ALL_DRAWS_REQUIRED" if None in values else None
    )
    assert band["p50_psi"] == (None if None in values else 125)
