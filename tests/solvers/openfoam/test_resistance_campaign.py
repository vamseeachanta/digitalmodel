"""Resistance workflow tests based on independent hand calculations.

All dimensional values below are invented arithmetic fixtures. With rho=1000,
S=10 and U=2, the coefficient denominator is 0.5*1000*10*2**2=20000 N.
A negative-x half-body force (-10 total, -4 pressure, -6 viscous) therefore
gives full-body R=20 N, Ct=0.001, Cp=0.0004 and Cv=0.0006.
The factor two follows reflection symmetry; the coefficient definition is
documented in the repository's calm-water resistance study, equation (2).
Policy values are illustrative required inputs, not published acceptance limits.
"""

from __future__ import annotations

import json
import runpy
from dataclasses import replace
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam.hull_case import HullCaseConfig
from digitalmodel.solvers.openfoam.hull_manifest import HullManifest
from digitalmodel.solvers.openfoam.resistance_campaign import (
    CampaignPolicy,
    ResistanceEvidence,
    assess_resistance_campaign,
    prepare_resistance_case,
    reduce_resistance_history,
    run_resistance_case,
)


@pytest.fixture
def policy():
    return CampaignPolicy(
        window=20,
        minimum_samples=3,
        maximum_relative_span=0.01,
        force_balance_relative_tolerance=1e-10,
        refinement_ratio_relative_tolerance=1e-10,
        source="Independent arithmetic fixture; illustrative policy only",
    )


def history(path, total=-10, *, rows=4, pressure=-4, viscous=None):
    """Write the v2312 total/pressure/viscous layout (forces.C, lines 443-450)."""
    if viscous is None:
        viscous = total - pressure
    path.write_text(
        "# Time total_x total_y total_z pressure_x pressure_y pressure_z "
        "viscous_x viscous_y viscous_z\n"
        + "".join(
            f"{i} ({total} 0 0) ({pressure} 0 0) ({viscous} 0 0)\n" for i in range(rows)
        )
    )
    return path


def test_signed_half_body_hand_calculation(tmp_path, policy):
    result = reduce_resistance_history(
        history(tmp_path / "force.dat"),
        density=1000,
        area=10,
        speed=2,
        half_domain=True,
        policy=policy,
    )
    assert result["resistance_n"] == pytest.approx(20)
    assert result["ct"] == pytest.approx(0.001)
    assert result["cp"] == pytest.approx(0.0004)
    assert result["cv"] == pytest.approx(0.0006)
    assert result["checks"]["stationarity"] == "PASS"


def test_full_domain_is_not_doubled(tmp_path, policy):
    result = reduce_resistance_history(
        history(tmp_path / "force.dat"),
        density=1000,
        area=10,
        speed=2,
        half_domain=False,
        policy=policy,
    )
    assert result["ct"] == pytest.approx(0.0005)


@pytest.mark.parametrize("total,viscous", [(10, 14), (-10, -5)])
def test_sign_and_decomposition_are_not_corrected(tmp_path, policy, total, viscous):
    result = reduce_resistance_history(
        history(tmp_path / "force.dat", total, viscous=viscous),
        density=1000,
        area=10,
        speed=2,
        half_domain=True,
        policy=policy,
    )
    assert "FAIL" in result["checks"].values()


@pytest.mark.parametrize(
    "row", ["1 2 3", "1 nan 0 0 -4 0 0 -6 0 0", "0 -10 0 0 -4 0 0 -6 0 0"]
)
def test_corrupt_or_restarted_history_is_refused(tmp_path, policy, row):
    path = history(tmp_path / "force.dat", rows=1)
    path.write_text(path.read_text() + row + "\n")
    with pytest.raises(ValueError):
        reduce_resistance_history(
            path, density=1000, area=10, speed=2, half_domain=True, policy=policy
        )


def test_window_is_time_span_and_insufficient_samples_are_explicit(tmp_path, policy):
    path = history(tmp_path / "force.dat")
    result = reduce_resistance_history(
        path,
        density=1000,
        area=10,
        speed=2,
        half_domain=True,
        policy=replace(policy, window=1),
    )
    assert result["samples"] == 1
    assert result["checks"]["stationarity"] == "NOT_EVALUATED"


@pytest.fixture
def config(manifest_dict, stl_file):
    return HullCaseConfig(
        HullManifest.from_dict(manifest_dict),
        stl_file,
        velocity=2,
        ranks=1,
        density=1000,
        kinematic_viscosity=1e-6,
        free_surface_cells_per_wavelength=None,
    )


def campaign(tmp_path, config):
    """Hand sequence: h=(1,2,4), Ct=(.001,.0013,.0025).

    Ct=.0009+.0001*h**2. Hence r=2, p=log(4)/log(2)=2,
    extrapolated Ct=.0009, fine-grid error=.0001 and absolute GCI=.000125.
    GCI safety factor 1.25: NASA grid-convergence tutorial (2021),
    'Grid Convergence Index'. Cell counts are metadata, not a spacing model.
    """
    result = []
    for index, (spacing, force, cells) in enumerate(
        ((1, -10, 512), (2, -13, 64), (4, -25, 8))
    ):
        case = prepare_resistance_case(config.replace(name=f"grid_{index}"), tmp_path)
        forces = history(case / "force.dat", force)
        result.append(ResistanceEvidence(case, forces, cells, spacing))
    return result


def test_three_grid_hand_calculation(tmp_path, config, policy):
    evidence = campaign(tmp_path, config)
    result = assess_resistance_campaign(evidence[::-1], policy)
    area = config.manifest.reference_wetted_surface_m2
    # Fixture manifest area differs from the simple 10 m2 calculation above.
    scale = 10 / area
    assert result["grid"]["observed_order"] == pytest.approx(2)
    assert result["grid"]["extrapolated_ct"] == pytest.approx(0.0009 * scale)
    assert result["grid"]["absolute_gci"] == pytest.approx(0.000125 * scale)
    assert result["grid"]["status"] == "EVALUATED"
    assert result["validation"]["status"] == "NOT_EVALUATED"
    assert result["checks"]["numerical_evidence"] == "PASS"


def test_two_grids_do_not_assume_order(tmp_path, config, policy):
    result = assess_resistance_campaign(campaign(tmp_path, config)[:2], policy)
    assert result["grid"]["status"] == "NOT_EVALUATED"
    assert result["checks"]["numerical_evidence"] == "NOT_EVALUATED"


@pytest.mark.parametrize("change", ["speed", "surface", "receipt"])
def test_mismatched_or_modified_case_is_refused(tmp_path, config, policy, change):
    evidence = campaign(tmp_path, config)
    case = evidence[1].case_dir
    if change == "surface":
        surface = case / "constant" / "triSurface" / config.stl_name
        surface.write_text(surface.read_text() + "\n")
    elif change == "speed":
        receipt = case / "resistance_input.json"
        data = json.loads(receipt.read_text())
        data["condition"]["velocity"] = 3
        receipt.write_text(json.dumps(data))
    else:
        (case / "system" / "controlDict").write_text("changed")
    with pytest.raises(ValueError, match="changed|match"):
        assess_resistance_campaign(evidence, policy)


@pytest.mark.parametrize("spacings", [(1, 1, 4), (1, 2, 5), (1, 1.05, 1.1025)])
def test_unsuitable_grid_spacing_has_no_gci(tmp_path, config, policy, spacings):
    evidence = [
        replace(e, spacing_m=h) for e, h in zip(campaign(tmp_path, config), spacings)
    ]
    result = assess_resistance_campaign(evidence, policy)
    assert result["grid"]["status"] == "NOT_EVALUATED"


def test_oscillation_has_no_richardson_band(tmp_path, config, policy):
    evidence = campaign(tmp_path, config)
    history(evidence[1].force_file, -30)
    result = assess_resistance_campaign(evidence, policy)
    assert result["grid"]["status"] == "NOT_EVALUATED"


def test_unsteady_companion_blocks_grid_estimate(tmp_path, config, policy):
    evidence = campaign(tmp_path, config)
    path = evidence[1].force_file
    # A final -20 N sample following -13 N samples exceeds the declared 1% span.
    path.write_text(path.read_text() + "4 (-20 0 0) (-4 0 0) (-16 0 0)\n")
    for item in (evidence[0], evidence[2]):
        path = item.force_file
        row = path.read_text().splitlines()[-1].split(" ", 1)[1]
        path.write_text(path.read_text() + "4 " + row + "\n")
    result = assess_resistance_campaign(evidence, policy)
    assert result["checks"]["numerical_evidence"] == "FAIL"
    assert result["grid"]["absolute_gci"] is None


def test_insufficient_companion_blocks_grid_estimate(tmp_path, config, policy):
    evidence = campaign(tmp_path, config)
    result = assess_resistance_campaign(evidence, replace(policy, minimum_samples=5))
    assert result["grid"]["absolute_gci"] is None
    assert result["checks"]["numerical_evidence"] == "NOT_EVALUATED"


def test_mismatched_final_time_is_refused(tmp_path, config, policy):
    evidence = campaign(tmp_path, config)
    history(evidence[1].force_file, -13, rows=5)
    with pytest.raises(ValueError, match="times do not match"):
        assess_resistance_campaign(evidence, policy)


@pytest.mark.parametrize("invalid", [0, -1, float("nan"), float("inf")])
def test_invalid_spacing_is_refused(tmp_path, invalid):
    with pytest.raises(ValueError, match="spacing_m"):
        ResistanceEvidence(tmp_path, tmp_path / "force.dat", 1, invalid)


def test_unsupported_force_layout_is_refused(tmp_path, policy):
    path = tmp_path / "force.dat"
    path.write_text("# Time pressure_x viscous_x\n0 -4 -6\n")
    with pytest.raises(ValueError, match="unsupported force header"):
        reduce_resistance_history(
            path, density=1000, area=10, speed=2, half_domain=True, policy=policy
        )


def test_prepare_never_overwrites_existing_case(tmp_path, config):
    case = prepare_resistance_case(config, tmp_path)
    with pytest.raises(FileExistsError):
        prepare_resistance_case(config, tmp_path)
    assert (case / "resistance_input.json").is_file()


def test_runner_dry_run_uses_complete_pipeline(tmp_path, config):
    # Illustrative caller-supplied execution budget, not an engineering limit.
    result = run_resistance_case(config, tmp_path, timeout_seconds=60, dry_run=True)
    assert result.status.value == "dry_run"
    assert result.solver == "interFoam"
    assert (result.case_dir / "resistance_input.json").is_file()
    assert not (result.case_dir / "log.interFoam").exists()


@pytest.mark.parametrize(
    "field,value",
    [
        ("window", 0),
        ("minimum_samples", 1),
        ("maximum_relative_span", float("nan")),
        ("source", ""),
    ],
)
def test_policy_rejects_invalid_inputs(policy, field, value):
    with pytest.raises(ValueError):
        replace(policy, **{field: value})


def test_worked_example_from_closed_geometry(tmp_path):
    """Independent box S=4*1+2*(4+1)*.6=10, V=4*1*.6=2.4.

    With the module docstring's inputs the example must produce Ct=.001,
    p=2, extrapolated Ct=.0009 and GCI=.000125, without running a solver.
    """
    example = (
        Path(__file__).resolve().parents[3]
        / "examples/openfoam/hull_resistance_prediction.py"
    )
    result = runpy.run_path(str(example))["run_example"](tmp_path / "worked")
    assert result["condition"]["area_m2"] == pytest.approx(10)
    assert result["condition"]["geometry"]["displacement_m3"] == pytest.approx(2.4)
    assert result["levels"][0]["ct"] == pytest.approx(0.001)
    assert result["grid"]["extrapolated_ct"] == pytest.approx(0.0009)
    assert result["grid"]["absolute_gci"] == pytest.approx(0.000125)
    assert (tmp_path / "worked" / "prediction.json").is_file()
