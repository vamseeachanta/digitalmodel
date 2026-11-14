"""Solver-free geometry contracts; synthetic inputs do not qualify S-lay.

Independent arithmetic: 1.10 D/(2R) is 1397/500000 for D=0.762 m,
R=150 m, and 1397/600000 for D=0.508 m, R=120 m. Strain 0.0026
lies between these limits. These are retained workflow criteria, not allowables.
"""

import json
import math
import sys
from pathlib import Path

import pytest
import yaml


@pytest.fixture
def probe(monkeypatch):
    monkeypatch.setitem(sys.modules, "OrcFxAPI", None)
    from digitalmodel.solvers.orcaflex.modular_generator import slay_probe

    return slay_probe


@pytest.fixture
def readings():
    return dict(
        wire_length=10,
        end_a_reaction=1,
        last_support_force=0,
        grounded_tension=[0, 1],
        overbend_strain=0,
        lift_off_arc=55,
        departure_angle=21,
        reference=dict(lift_off_arc=55, departure_angle=21),
        diameter=0.762,
        radius=150,
    )


ENTRY_POINTS = ["evaluate_static_acceptance", "assert_static_acceptance"]


@pytest.mark.parametrize("entry_point", ENTRY_POINTS)
@pytest.mark.parametrize(
    "omitted", [("diameter",), ("radius",), ("diameter", "radius")]
)
def test_geometry_keywords_are_required(probe, readings, entry_point, omitted):
    for name in omitted:
        del readings[name]
    with pytest.raises(TypeError, match="|".join(omitted)):
        getattr(probe, entry_point)(**readings)


@pytest.mark.parametrize("entry_point", ENTRY_POINTS)
@pytest.mark.parametrize(
    "diameter,radius,expected",
    [
        (0.762, 150, 1397 / 500000),
        (0.508, 150, 1397 / 750000),  # Change diameter alone.
        (0.762, 120, 1397 / 400000),  # Change radius alone.
        (0.508, 120, 1397 / 600000),
    ],
)
def test_report_records_geometry_and_limit(
    probe,
    readings,
    entry_point,
    diameter,
    radius,
    expected,
):
    readings.update(diameter=diameter, radius=radius)
    report = getattr(probe, entry_point)(**readings)
    assert report["diameter"] == diameter
    assert report["radius"] == radius
    assert math.isfinite(report["s1_limit"])
    assert report["s1_limit"] == pytest.approx(expected)
    assert report["units"] == {"diameter": "m", "radius": "m", "s1_limit": "1"}
    assert json.loads(json.dumps(report, allow_nan=False)) == report


def test_campaign_limit_matches_retained_criterion(probe, readings):
    campaign = (
        Path(__file__).resolve().parents[3]
        / "docs/domains/pipeline-installation/slay-campaign"
    )
    basis = yaml.safe_load((campaign / "case-table.yml").read_text(encoding="utf-8"))[
        "basis"
    ]
    criteria = json.loads((campaign / "criteria.json").read_text(encoding="utf-8"))
    readings.update(
        diameter=basis["pipeline"]["dimensions"]["outer_diameter"],
        radius=basis["equipment"]["stinger"]["radius"],
    )
    report = probe.evaluate_static_acceptance(**readings)
    assert report["s1_limit"] == pytest.approx(
        criteria["criteria"]["stinger_conformity"]["numeric_limit"]
    )


def test_alternate_geometry_rejects_issue_strain(probe, readings):
    readings.update(diameter=0.508, radius=120, overbend_strain=0.0026)
    report = probe.evaluate_static_acceptance(**readings)
    assert report["s1_limit"] == pytest.approx(1397 / 600000)
    assert report["s1_limit"] < readings["overbend_strain"] < 1397 / 500000
    assert report["rows"]["stinger_conformity"]["status"] == "FAIL"
    assert report["status"] == "rejected"
    assert report["counts"] == {"passed": 5, "failed": 1, "not_evaluated": 3}
    assert report["fully_accepted"] is False
    with pytest.raises(ValueError, match="S1"):
        probe.assert_static_acceptance(**readings)


@pytest.mark.parametrize("diameter,radius", [(0.762, 150), (0.508, 120)])
@pytest.mark.parametrize(
    "above,last_force,passed",
    [
        (False, 0, True),
        (True, 0, False),
        (False, math.nextafter(0, math.inf), False),
        (False, math.nextafter(0, -math.inf), False),
    ],
)
def test_s1_boundary_and_exact_zero_contact(
    probe,
    readings,
    diameter,
    radius,
    above,
    last_force,
    passed,
):
    readings.update(diameter=diameter, radius=radius)
    limit = probe.evaluate_static_acceptance(**readings)["s1_limit"]
    readings.update(
        overbend_strain=math.nextafter(limit, math.inf) if above else limit,
        last_support_force=last_force,
    )
    report = probe.evaluate_static_acceptance(**readings)
    assert report["rows"]["stinger_conformity"]["status"] == (
        "PASS" if passed else "FAIL"
    )
    assert report["counts"] == {
        "passed": 6 if passed else 5,
        "failed": 0 if passed else 1,
        "not_evaluated": 3,
    }


@pytest.mark.parametrize("entry_point", ENTRY_POINTS)
@pytest.mark.parametrize("field", ["diameter", "radius"])
@pytest.mark.parametrize("bad", [0, -1, math.nan, math.inf, -math.inf])
def test_invalid_geometry_retains_value_error(probe, readings, entry_point, field, bad):
    readings[field] = bad
    with pytest.raises(ValueError, match="Finite|Positive"):
        getattr(probe, entry_point)(**readings)
