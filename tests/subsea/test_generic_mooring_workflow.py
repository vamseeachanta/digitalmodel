"""Independent hand calculations for the generic mooring workflow.

All numbers below are invented mathematical test inputs, not design data.
"""

import copy
import html
import re
from pathlib import Path

import pytest

from digitalmodel.mooring_analysis_workflow import (
    assess_case,
    build_case_table,
    catenary_screen,
    linear_offset,
    render_report,
)

ROOT = Path(__file__).resolve().parents[2]


def test_inventory_covers_product_and_retains_sources():
    cases = build_case_table(ROOT, {"spread": "Mooring_L1", "turret": "Mooring_L1"})
    assert len(cases) == len({c["case_id"] for c in cases}) == 32
    assert {c["heading_deg"] for c in cases} == set(range(0, 360, 45))
    for case in cases:
        assert case["environment"]["source"].endswith(".yml#Environment")
        assert len(case["basis_sha256"]) == 64
        assert case["model_status"] == "NOT_EVALUATED"
        assert case["removed_line"] == (
            "Mooring_L1" if case["condition"] == "damaged" else None
        )
    assert cases[0]["environment"]["values"]["WaterDepth"] == 200


def test_inventory_rejects_unknown_removed_line():
    with pytest.raises(ValueError, match="line"):
        build_case_table(ROOT, {"spread": "absent", "turret": "absent"})


def test_unspecified_damage_is_explicit():
    damaged = [c for c in build_case_table(ROOT) if c["condition"] == "damaged"]
    assert all(c["removed_line"] is None for c in damaged)
    assert all("damaged line" in c["limitations"] for c in damaged)


def test_catenary_hand_calculation():
    """H=100 kN, w=1 kN/m, h=25 m: a=100 m.

    s²=h²+2ah=5625 -> s=75 m; V=75 kN; T=125 kN.
    cosh(x/a)=1.25 -> x=100 ln(2)=69.31471805599453 m.
    L=100 m -> grounded=25 m; radius=x+25 m.
    Equations: MoorPy Catenary.py, ProfileType 2, inextensible limit.
    """
    result = catenary_screen(100, 1, 25, 100)
    assert result["top_tension_kn"] == pytest.approx(125)
    assert result["suspended_length_m"] == pytest.approx(75)
    assert result["anchor_radius_m"] == pytest.approx(94.31471805599453)


@pytest.mark.parametrize(
    "args",
    [
        (0, 1, 25, 100),
        (100, -1, 25, 100),
        (100, 1, float("nan"), 100),
        (100, 1, 25, 50),
    ],
)
def test_catenary_invalid_or_fully_suspended(args):
    with pytest.raises(ValueError):
        catenary_screen(*args)


def test_coupled_linear_offset_hand_calculation():
    """K=[[4,1],[1,3]] kN/m, F=[9,8] kN -> q=[19/11,23/11] m.

    From 4x+y=9 and x+3y=8: 11x=19, 11y=23.
    Linear force law: OpenStax University Physics v1 (2016), section 15.1.
    """
    assert linear_offset((9, 8), ((4, 1), (1, 3))) == pytest.approx((19 / 11, 23 / 11))


@pytest.mark.parametrize(
    "matrix",
    [((1, 2), (2, 1)), ((1, 1), (1, 1)), ((1, 2), (0, 1)), ((float("inf"), 0), (0, 1))],
)
def test_invalid_stiffness(matrix):
    with pytest.raises(ValueError):
        linear_offset((1, 1), matrix)


def metric(case):
    return {
        "basis_sha256": case["basis_sha256"],
        "solver_version": "example-only",
        "source": "synthetic hand calculation; not native results",
        "offset_xy_m": [3, 4],
        "tensions_kn": {
            name: (80 if name == "Mooring_L2" else 40) for name in case["active_lines"]
        },
    }


def screen_record(line_id="Mooring_L2", *, offset_m=4, top_tension_kn=64):
    """Invented analytical basis: H+w*h=64 kN and identity K gives 4 m."""
    return {
        "line_id": line_id,
        "offset_m": offset_m,
        "top_tension_kn": top_tension_kn,
        "source": "synthetic analytical basis & equations",
        "inputs": {
            "catenary": {
                "horizontal_kn": 40,
                "weight_kn_m": 1,
                "height_m": 24,
                "length_m": 100,
            },
            "linear_offset": {
                "force_xy_kn": [offset_m, 0],
                "stiffness_kn_m": [[1, 0], [0, 1]],
            },
        },
    }


def test_stored_metrics_and_comparison():
    """Hand check: max(40/100,80/200)=0.4; limit=0.5 -> utilisation=0.8.

    Offset norm sqrt(3²+4²)=5 m; screen 4 m -> difference=25.0%.
    Tension screen 64 kN vs stored maximum 80 kN -> difference=25.0%.
    Limits/capacities are arbitrary test inputs, not code criteria.
    """
    case = build_case_table(ROOT)[0]
    out = assess_case(
        case,
        metric(case),
        capacities_kn={
            name: (200 if name == "Mooring_L2" else 100)
            for name in case["active_lines"]
        },
        limit={"ratio": 0.5, "source": "test-only criterion"},
        screen=screen_record(),
    )
    assert out["status"] == "PASS"
    assert out["utilisation"] == pytest.approx(0.8)
    assert out["offset_m"] == 5
    assert out["offset_difference_pct"] == 25
    assert out["tension_difference_pct"] == 25
    assert out["qualified"] is False


def test_missing_criteria_and_missing_results():
    case = build_case_table(ROOT)[0]
    assert assess_case(case)["status"] == "NOT_EVALUATED"
    assert assess_case(case, metric(case))["status"] == "NOT_EVALUATED"


def test_failure_boundary_and_zero_screen():
    case = build_case_table(ROOT)[0]
    out = assess_case(
        case,
        metric(case),
        capacities_kn={name: 100 for name in case["active_lines"]},
        limit={"ratio": 0.5, "source": "test-only"},
        screen=screen_record(offset_m=0, top_tension_kn=0),
    )
    assert out["status"] == "FAIL"
    assert out["offset_difference_pct"] is None


@pytest.mark.parametrize("mutation", ["digest", "negative", "missing_line", "version"])
def test_result_validation(mutation):
    case = build_case_table(ROOT)[0]
    data = copy.deepcopy(metric(case))
    if mutation == "digest":
        data["basis_sha256"] = "0" * 64
    elif mutation == "negative":
        data["tensions_kn"]["Mooring_L1"] = -1
    elif mutation == "missing_line":
        data["tensions_kn"] = {}
    else:
        data["solver_version"] = ""
    with pytest.raises(ValueError):
        assess_case(case, data)


def test_report_missing_rows_and_escaping():
    cases = build_case_table(ROOT)
    report = render_report(cases, {}, revision="00", date="2026-10-20")
    assert "Mooring analysis" in report
    assert "NOT_EVALUATED" in report
    assert "0 / 32" in report
    assert "Revision history" in report
    assert "spread-intact-000" in report


def test_report_rejects_mixed_versions():
    cases = build_case_table(ROOT)
    first = assess_case(cases[0], metric(cases[0]))
    second = assess_case(cases[1], metric(cases[1]))
    second["solver_version"] = "another-version"
    with pytest.raises(ValueError, match="version"):
        render_report(
            cases,
            {cases[0]["case_id"]: first, cases[1]["case_id"]: second},
            revision="00",
            date="2026-10-20",
        )


def test_damage_metrics_exclude_removed_line():
    cases = build_case_table(ROOT, {"spread": "Mooring_L1"})
    case = next(c for c in cases if c["condition"] == "damaged")
    data = metric(case)
    assert "Mooring_L1" not in data["tensions_kn"]
    data["tensions_kn"]["Mooring_L1"] = 0
    with pytest.raises(ValueError, match="active lines"):
        assess_case(case, data)


def test_report_rejects_stale_assessment():
    cases = build_case_table(ROOT)
    assessment = assess_case(cases[0], metric(cases[0]))
    assessment["basis_sha256"] = "0" * 64
    with pytest.raises(ValueError, match="digest"):
        render_report(
            cases, {cases[0]["case_id"]: assessment}, revision="00", date="2026-10-20"
        )


def test_report_shows_limit_source_and_result_source():
    cases = build_case_table(ROOT)
    case = cases[0]
    out = assess_case(
        case,
        metric(case),
        capacities_kn={name: 200 for name in case["active_lines"]},
        limit={"ratio": 0.5, "source": "Example criterion & applicability"},
    )
    report = render_report(
        cases, {case["case_id"]: out}, revision="00", date="2026-10-20"
    )
    assert "0.500" in report
    assert "Example criterion &amp; applicability" in report
    assert "synthetic hand calculation; not native results" in report


def test_report_rejects_unknown_assessment():
    cases = build_case_table(ROOT)
    with pytest.raises(ValueError, match="unknown case"):
        render_report(cases, {"unknown": {}}, revision="00", date="2026-10-20")


def test_exact_limit_passes():
    """Invented 80/200=0.4 with a 0.4 supplied limit: utilisation equals 1."""
    case = build_case_table(ROOT)[0]
    out = assess_case(
        case,
        metric(case),
        capacities_kn={name: 200 for name in case["active_lines"]},
        limit={"ratio": 0.4, "source": "arbitrary test criterion"},
    )
    assert out["status"] == "PASS"
    assert out["utilisation"] == 1


def test_report_rounds_comparison_including_echo():
    """Independent (5-3)/3*100=200/3 %, presented to one decimal as 66.7 %."""
    cases = build_case_table(ROOT)
    case = cases[0]
    out = assess_case(case, metric(case), screen=screen_record(offset_m=3))
    report = render_report(
        cases, {case["case_id"]: out}, revision="00", date="2026-10-20"
    )
    assert "66.7" in report
    assert "66.666" not in report


@pytest.mark.parametrize("ratio", [0, -1, 1.1, float("nan"), float("inf")])
def test_invalid_criteria_are_rejected(ratio):
    case = build_case_table(ROOT)[0]
    with pytest.raises(ValueError):
        assess_case(
            case,
            metric(case),
            capacities_kn={name: 200 for name in case["active_lines"]},
            limit={"ratio": ratio, "source": "test-only"},
        )


def test_unspecified_damage_cannot_pass_supplied_criterion():
    case = next(c for c in build_case_table(ROOT) if c["condition"] == "damaged")
    out = assess_case(
        case,
        metric(case),
        capacities_kn={name: 200 for name in case["active_lines"]},
        limit={"ratio": 0.5, "source": "test-only"},
    )
    assert out["status"] == "NOT_EVALUATED"
    assert out["reason"] == "damaged line unspecified"


def test_comparison_matches_screened_line_when_another_line_governs():
    """Matched difference: 100*(40-64)/64=-37.5%, despite an 80 kN peak."""
    case = build_case_table(ROOT)[0]
    out = assess_case(
        case,
        metric(case),
        screen=screen_record("Mooring_L1"),
    )
    assert out["max_tension_kn"] == 80
    assert out["tension_difference_pct"] == -37.5


@pytest.mark.parametrize("line_id", [None, "", "absent", "Mooring_L1", []])
def test_comparison_requires_an_active_screened_line(line_id):
    case = next(
        c
        for c in build_case_table(ROOT, {"spread": "Mooring_L1"})
        if c["condition"] == "damaged"
    )
    screen = screen_record()
    del screen["line_id"]
    if line_id is not None:
        screen["line_id"] = line_id
    with pytest.raises(ValueError, match="active screened line"):
        assess_case(case, metric(case), screen=screen)


@pytest.mark.parametrize("basis_kind", ["inputs", "input_manifest"])
def test_screen_basis_is_retained_in_assessment_and_report(basis_kind):
    cases = build_case_table(ROOT)
    case = cases[0]
    screen = screen_record("Mooring_L1")
    if basis_kind == "input_manifest":
        del screen["inputs"]
        screen["input_manifest"] = "analytical-inputs.json#spread-intact-000"
    out = assess_case(case, metric(case), screen=screen)
    assert out["screen"] == screen
    report = render_report(
        cases, {case["case_id"]: out}, revision="01", date="2026-10-20"
    )
    echo = re.search(r'<section id="input-echo">(.*?)</section>', report, re.DOTALL)
    assert echo is not None
    echoed = dict(
        re.findall(
            r"<tr><td>(.*?)</td><td>(.*?)</td></tr>",
            html.unescape(echo.group(1)),
            re.DOTALL,
        )
    )
    prefix = f"assessments.{case['case_id']}.screen."
    assert echoed[prefix + "line_id"] == "Mooring_L1"
    assert echoed[prefix + "offset_m"] == "4"
    assert echoed[prefix + "top_tension_kn"] == "64"
    assert echoed[prefix + "source"] == screen["source"]
    if basis_kind == "inputs":
        assert echoed[prefix + "inputs.catenary.horizontal_kn"] == "40"
        assert echoed[prefix + "inputs.catenary.height_m"] == "24"
        assert echoed[prefix + "inputs.linear_offset.force_xy_kn"] == "[4, 0]"
        assert (
            echoed[prefix + "inputs.linear_offset.stiffness_kn_m"] == "[[1, 0], [0, 1]]"
        )
    else:
        assert echoed[prefix + "input_manifest"] == screen["input_manifest"]
    assert "Analytical screen for spread-intact-000, line Mooring_L1" in report
    assert "synthetic analytical basis &amp; equations" in report
    assert out["offset_difference_pct"] == 25
    assert out["tension_difference_pct"] == -37.5


@pytest.mark.parametrize("pending", ["missing_results", "unspecified_damage"])
def test_screen_basis_is_retained_without_an_evaluated_case(pending):
    cases = build_case_table(ROOT)
    case = cases[0] if pending == "missing_results" else cases[8]
    screen = screen_record()
    out = assess_case(
        case, None if pending == "missing_results" else metric(case), screen=screen
    )
    assert out["status"] == "NOT_EVALUATED"
    assert out["screen"] == screen


def test_retained_screen_basis_does_not_alias_caller_inputs():
    case = build_case_table(ROOT)[0]
    screen = screen_record()
    expected = copy.deepcopy(screen)
    out = assess_case(case, metric(case), screen=screen)
    screen["inputs"]["linear_offset"]["force_xy_kn"][0] = 999
    screen["source"] = "changed source"
    assert out["screen"] == expected


@pytest.mark.parametrize(
    "field,value",
    [
        ("source", None),
        ("source", ""),
        ("source", "  "),
        ("source", 1),
        ("inputs", None),
        ("inputs", {}),
        ("inputs", []),
        ("inputs", {"force_kn": float("nan")}),
        ("inputs", {"force_kn": float("inf")}),
        ("inputs", {"force_kn": object()}),
        ("input_manifest", ""),
        ("input_manifest", "  "),
        ("input_manifest", 1),
        ("offset_m", -1),
        ("offset_m", True),
        ("offset_m", float("nan")),
        ("offset_m", float("inf")),
        ("top_tension_kn", -1),
        ("top_tension_kn", False),
        ("top_tension_kn", float("nan")),
        ("top_tension_kn", float("inf")),
    ],
)
def test_invalid_screen_basis_is_rejected(field, value):
    case = build_case_table(ROOT)[0]
    screen = screen_record()
    if field == "input_manifest":
        del screen["inputs"]
    if value is None:
        del screen[field]
    else:
        screen[field] = value
    with pytest.raises(ValueError, match="screen"):
        assess_case(case, metric(case), screen=screen)
