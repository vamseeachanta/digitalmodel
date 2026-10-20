"""Independent hand calculations for the generic mooring workflow.

All numbers below are invented mathematical test inputs, not design data.
"""

import copy
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
        screen={"offset_m": 4, "top_tension_kn": 64},
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
        screen={"offset_m": 0, "top_tension_kn": 0},
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
    out = assess_case(case, metric(case), screen={"offset_m": 3, "top_tension_kn": 64})
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
