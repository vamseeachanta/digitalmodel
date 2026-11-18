# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline campaign checks; all numerical vectors are hand calculations.

The synthetic basis and acceptance limits are defined in
docs/domains/openfoam/resistance-campaign.md, Worked example. They are
arithmetic test inputs, not experimental coefficients or engineering limits.
"""

from copy import deepcopy
import json
from pathlib import Path
import subprocess
import sys

import pytest

from digitalmodel.solvers.openfoam.resistance_campaign import analyze_campaign


@pytest.fixture
def campaign():
    """Read the runnable, synthetic worked example as the integration input."""
    root = Path(__file__).resolve().parents[3]
    return json.loads((root / "examples/openfoam/resistance_campaign.json").read_text())


def test_hand_calculated_sweep_and_grid(campaign):
    """Ct errors: (1.01-1)/1 = 1%; (2.02-2)/2 = 1%.

    Increment error: [(2.02-1.01)-(2-1)]/max(1,2) = 0.005.
    Fine/medium/coarse values 1.01,1.04,1.16 follow f(h)=1+0.01h²
    at h=1,2,4. Thus p=log(0.12/0.03)/log(2)=2 and f_ext=1.
    GCI fine = 100*1.25*0.03/(1.01*3) = 1.2376...%;
    GCI medium = 100*1.25*0.12/(1.04*3) = 4.8077...%.
    The 1.25 factor is NASA spatial-convergence tutorial (2021), GCI.
    """
    report = analyze_campaign(campaign)
    assert report["status"] == "PASS"
    assert report["sweep"]["ct_errors_fraction"] == pytest.approx([0.01, 0.01])
    assert report["sweep"]["increment_errors_fraction"] == pytest.approx([0.005])
    grid = report["grid"]
    assert grid["gci"]["p"] == pytest.approx(2)
    assert grid["gci"]["f_ext"] == pytest.approx(1)
    assert grid["gci"]["gci_fine_pct"] == pytest.approx(1.25 / 1.01)
    assert grid["gci"]["gci_medium_pct"] == pytest.approx(5 / 1.04)


@pytest.mark.parametrize("factor", [0.8, 1.2])
def test_sweep_rejects_both_sides(campaign, factor):
    """A signed ±20% bias exceeds the example's symmetric ±5% limit."""
    for point, ref in zip(
        campaign["computed"]["points"], campaign["reference"]["points"]
    ):
        point["ct"] = ref["ct"] * factor
    assert analyze_campaign(campaign)["sweep"]["status"] == "FAIL"


@pytest.mark.parametrize("sign", [-1, 1])
@pytest.mark.parametrize("outside", [False, True])
def test_ct_band_includes_endpoints_only_with_roundoff(campaign, sign, outside):
    """Reference 1,2 scaled by 0.95 or 1.05 has exactly ±5% point error.

    An additional signed 1e-13 fraction lies outside the roundoff allowance.
    Increment errors are half the point errors, inside the supplied 5% band.
    """
    fraction = sign * (0.05 + (1e-13 if outside else 0))
    for point, ref in zip(
        campaign["computed"]["points"], campaign["reference"]["points"]
    ):
        point["ct"] = ref["ct"] * (1 + fraction)
    campaign["limits"]["increment_fraction"] = 0.05
    sweep = analyze_campaign(campaign)["sweep"]
    assert sweep["ct_pass"] is (not outside)
    assert sweep["increment_pass"] is True
    assert sweep["status"] == ("FAIL" if outside else "PASS")


@pytest.mark.parametrize("sign", [-1, 1])
@pytest.mark.parametrize("outside", [False, True])
def test_increment_band_includes_endpoints_only_with_roundoff(campaign, sign, outside):
    """With reference 1,2 and computed 1,2±0.02, increment error is ±1%.

    An additional signed 1e-13 fraction must fail; point errors remain below 5%.
    """
    fraction = sign * (0.01 + (1e-13 if outside else 0))
    campaign["computed"]["points"][0]["ct"] = 1
    campaign["computed"]["points"][1]["ct"] = 2 + 2 * fraction
    sweep = analyze_campaign(campaign)["sweep"]
    assert sweep["ct_pass"] is True
    assert sweep["increment_pass"] is (not outside)
    assert sweep["status"] == ("FAIL" if outside else "PASS")


@pytest.mark.parametrize(
    "limit,gate", [("ct_fraction", "ct_pass"), ("increment_fraction", "increment_pass")]
)
@pytest.mark.parametrize("offset", [0, 2e-13])
def test_zero_limits_reject_errors_beyond_roundoff(campaign, limit, gate, offset):
    campaign["computed"]["points"][0]["ct"] = 1
    campaign["computed"]["points"][1]["ct"] = 2 + offset
    campaign["limits"][limit] = 0
    sweep = analyze_campaign(campaign)["sweep"]
    assert sweep[gate] is (offset == 0)


@pytest.mark.parametrize(
    "section", ["reference", "computed", "prerequisite", "limits", "grids"]
)
def test_omitted_evidence_is_still_unevaluated(campaign, section):
    campaign.pop(section)
    assert analyze_campaign(campaign)["status"] == "NOT_EVALUATED"


def test_malformed_shape_is_rejected_before_evidence_gates():
    with pytest.raises(ValueError, match="object"):
        analyze_campaign({"grids": [None]})


@pytest.mark.parametrize("end_ct", [1.94, 2.06])
def test_increment_is_two_sided_even_inside_ct_band(campaign, end_ct):
    """Endpoint errors ±3% pass Ct; increment errors ±0.03 fail ±0.01."""
    campaign["computed"]["points"][0]["ct"] = 1
    campaign["computed"]["points"][1]["ct"] = end_ct
    sweep = analyze_campaign(campaign)["sweep"]
    assert sweep["ct_pass"] is True
    assert sweep["increment_pass"] is False
    assert sweep["status"] == "FAIL"


@pytest.mark.parametrize(
    "field,value",
    [
        ("body_condition", "fixed_even_keel"),
        ("appendages", "none"),
        ("wetted_surface_m2", 3),
        ("lpp_m", 3),
        ("density_kg_m3", 3),
        ("viscosity_m2_s", 3),
    ],
)
def test_condition_tuple_cannot_be_blended(campaign, field, value):
    campaign["computed"]["condition"][field] = value
    report = analyze_campaign(campaign)["sweep"]
    assert report["status"] == "NOT_EVALUATED"
    assert "condition" in report["reason"]
    assert "ct_errors_fraction" not in report


def test_reynolds_is_matched_at_each_speed(campaign):
    campaign["computed"]["points"][1]["reynolds"] *= 2
    assert analyze_campaign(campaign)["sweep"]["status"] == "NOT_EVALUATED"


@pytest.mark.parametrize("field", ["ct_fraction", "increment_fraction"])
def test_no_borrowed_acceptance_limits(campaign, field):
    campaign["limits"].pop(field)
    assert analyze_campaign(campaign)["sweep"]["status"] == "NOT_EVALUATED"


@pytest.mark.parametrize("section", ["reference", "computed", "prerequisite", "limits"])
def test_missing_source_is_not_a_pass(campaign, section):
    campaign[section]["source"] = ""
    assert analyze_campaign(campaign)["sweep"]["status"] == "NOT_EVALUATED"


def test_single_point_is_not_a_trend(campaign):
    for key in ("reference", "computed"):
        campaign[key]["points"] = campaign[key]["points"][:1]
    assert analyze_campaign(campaign)["sweep"]["status"] == "NOT_EVALUATED"


def test_incomplete_froude_coverage_is_not_interpolated(campaign):
    campaign["computed"]["points"][1]["froude"] = 0.3
    assert analyze_campaign(campaign)["sweep"]["status"] == "NOT_EVALUATED"


def test_prerequisite_must_have_passed(campaign):
    campaign["prerequisite"]["status"] = "NOT_EVALUATED"
    report = analyze_campaign(campaign)
    assert report["status"] == "NOT_EVALUATED"
    assert "prerequisite" in report["sweep"]["reason"]


def test_input_order_does_not_change_result(campaign):
    expected = deepcopy(analyze_campaign(campaign))
    for key in ("reference", "computed"):
        campaign[key]["points"].reverse()
    campaign["grids"].reverse()
    actual = analyze_campaign(campaign)
    for key in ("status", "sweep", "grid"):
        assert actual[key] == expected[key]


@pytest.mark.parametrize(
    "values,classification",
    [
        ([1.01, 1.0, 1.04], "oscillatory"),
        ([1.16, 1.04, 1.01], "divergent"),
        ([1.01, 1.01, 1.04], "degenerate"),
    ],
)
def test_unsupported_sequences_have_no_gci(campaign, values, classification):
    """Differences change sign, grow on refinement, or vanish, respectively."""
    for grid, value in zip(campaign["grids"], values):
        grid["point"]["ct"] = value
    report = analyze_campaign(campaign)["grid"]
    assert report["status"] == "NOT_EVALUATED"
    assert report["classification"] == classification
    assert "gci" not in report


@pytest.mark.parametrize(
    "mutation",
    ["missing", "unequal", "family", "condition", "froude", "reynolds", "unsettled"],
)
def test_incomparable_grids_are_not_a_study(campaign, mutation):
    if mutation == "missing":
        campaign["grids"].pop()
    elif mutation == "unequal":
        campaign["grids"][2]["h_m"] = 5
    elif mutation == "family":
        campaign["grids"][2]["mesh_family"] = "different"
    elif mutation == "condition":
        campaign["grids"][2]["condition"]["appendages"] = "none"
    elif mutation == "unsettled":
        campaign["grids"][2]["point"]["settled"] = False
    else:
        campaign["grids"][2]["point"][mutation] *= 2
    assert analyze_campaign(campaign)["grid"]["status"] == "NOT_EVALUATED"


def test_medium_gci_is_gated_not_fine_gci(campaign):
    """Fine GCI is below 2%, medium GCI above 2%; the gate must fail."""
    campaign["limits"]["gci_medium_pct"] = 2
    assert analyze_campaign(campaign)["grid"]["status"] == "FAIL"


def test_no_grid_limit_still_returns_diagnostics(campaign):
    campaign["limits"].pop("gci_medium_pct")
    grid = analyze_campaign(campaign)["grid"]
    assert grid["status"] == "NOT_EVALUATED"
    assert grid["gci"]["p"] == pytest.approx(2)


@pytest.mark.parametrize("bad", [float("nan"), float("inf"), -1, 0, True])
def test_nonphysical_coefficients_are_rejected(campaign, bad):
    campaign["computed"]["points"][0]["ct"] = bad
    with pytest.raises(ValueError, match="ct"):
        analyze_campaign(campaign)


def test_duplicate_speeds_are_rejected(campaign):
    campaign["computed"]["points"].append(deepcopy(campaign["computed"]["points"][0]))
    with pytest.raises(ValueError, match="duplicate"):
        analyze_campaign(campaign)


def test_input_is_not_mutated(campaign):
    original = deepcopy(campaign)
    analyze_campaign(campaign)
    assert campaign == original


def test_cli_matches_api_and_exposes_exit_status(campaign, tmp_path, monkeypatch):
    """JSON round-trip exercises the published command without a solver."""
    source = tmp_path / "input.json"
    output = tmp_path / "report.json"
    root = Path(__file__).resolve().parents[3]
    monkeypatch.setenv("PYTHONPATH", str(root / "src"))
    for prerequisite, expected_exit in [("PASS", 0), ("NOT_EVALUATED", 2), ("PASS", 1)]:
        campaign["prerequisite"]["status"] = prerequisite
        if expected_exit == 1:
            campaign["limits"]["gci_medium_pct"] = 2
        source.write_text(json.dumps(campaign))
        result = subprocess.run(
            [
                sys.executable,
                "-m",
                "digitalmodel.solvers.openfoam.resistance_campaign",
                str(source),
                "--output",
                str(output),
            ],
            capture_output=True,
            text=True,
            check=False,
        )
        assert result.returncode == expected_exit, result.stderr
        assert json.loads(output.read_text()) == analyze_campaign(campaign)


@pytest.mark.parametrize(
    "path",
    [
        (),
        ("reference",),
        ("computed",),
        ("prerequisite",),
        ("limits",),
        ("reference", "condition"),
        ("computed", "condition"),
        ("reference", "points", 0),
        ("computed", "points", 0),
        ("grids", 0),
        ("grids", 0, "condition"),
        ("grids", 0, "point"),
    ],
)
@pytest.mark.parametrize("bad", [None, [], "invalid-record", 1, True])
def test_cli_rejects_non_object_records(campaign, tmp_path, monkeypatch, path, bad):
    if path:
        parent = campaign
        for key in path[:-1]:
            parent = parent[key]
        parent[path[-1]] = bad
    else:
        campaign = bad
    _assert_invalid_cli(campaign, tmp_path, monkeypatch)


@pytest.mark.parametrize(
    "path", [("reference", "points"), ("computed", "points"), ("grids",)]
)
@pytest.mark.parametrize("bad", [None, {}, "invalid-list", 1, True])
def test_cli_rejects_non_list_collections(campaign, tmp_path, monkeypatch, path, bad):
    parent = campaign
    for key in path[:-1]:
        parent = parent[key]
    parent[path[-1]] = bad
    _assert_invalid_cli(campaign, tmp_path, monkeypatch)


def _assert_invalid_cli(data, tmp_path, monkeypatch):
    """Malformed JSON structures use the sanitized invalid-input protocol."""
    source = tmp_path / "input.json"
    output = tmp_path / "report.json"
    root = Path(__file__).resolve().parents[3]
    monkeypatch.setenv("PYTHONPATH", str(root / "src"))
    source.write_text(json.dumps(data))
    result = subprocess.run(
        [
            sys.executable,
            "-m",
            "digitalmodel.solvers.openfoam.resistance_campaign",
            str(source),
            "--output",
            str(output),
        ],
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 2
    assert result.stderr == "invalid campaign input or output (ValueError)\n"
    assert result.stdout == ""
    assert not output.exists()


def test_published_nasa_grid_example(campaign):
    """NASA, Examining Spatial (Grid) Convergence (2021), Example Grid
    Convergence Study: h=1,2,4; inlet pressure recovery
    0.97050,0.96854,0.96178. Published p=1.786170, extrapolate=0.97130,
    GCI fine=0.103083%, medium=0.356249%. These are mathematical
    reference values, not resistance measurements. Rounding tolerances below
    are half a unit of the final published decimal, not acceptance limits.
    """
    for grid, value in zip(campaign["grids"], [0.97050, 0.96854, 0.96178]):
        grid["point"]["ct"] = value
    gci = analyze_campaign(campaign)["grid"]["gci"]
    assert gci["p"] == pytest.approx(1.786170, abs=0.0000005, rel=0)
    assert gci["f_ext"] == pytest.approx(0.97130, abs=0.000005, rel=0)
    assert gci["gci_fine_pct"] == pytest.approx(0.103083, abs=0.0000005, rel=0)
    assert gci["gci_medium_pct"] == pytest.approx(0.356249, abs=0.0000005, rel=0)


def test_grid_must_belong_to_sweep(campaign):
    for grid in campaign["grids"]:
        grid["condition"]["appendages"] = "none"
    assert analyze_campaign(campaign)["grid"]["status"] == "NOT_EVALUATED"


def test_unqualified_reference_is_not_used(campaign):
    campaign["reference"]["qualified"] = False
    assert analyze_campaign(campaign)["sweep"]["status"] == "NOT_EVALUATED"


@pytest.mark.parametrize("ct_values", [[2, 1], [1, 1], [2, 1, 2]])
def test_falling_flat_and_valley_trends(campaign, ct_values):
    """Identical synthetic series have zero point and increment errors,
    including a falling interval, a zero increment, and a valley.
    """
    for key in ("reference", "computed"):
        campaign[key]["points"] = [
            dict(froude=(i + 1) / 10, reynolds=i + 1, ct=ct, settled=True)
            for i, ct in enumerate(ct_values)
        ]
    result = analyze_campaign(campaign)["sweep"]
    assert result["status"] == "PASS"
    assert result["increment_errors_fraction"] == [0] * (len(ct_values) - 1)


def test_extreme_grid_values_are_unevaluated(campaign):
    """Synthetic range test: differences near 1e300 cannot be extrapolated
    from a fine coefficient near 1e-300 without overflowing binary64.
    No finite numerical uncertainty should be claimed.
    """
    for grid, value in zip(campaign["grids"], [1e-300, 1e-200, 1e300]):
        grid["point"]["ct"] = value
    assert analyze_campaign(campaign)["grid"]["status"] == "NOT_EVALUATED"
