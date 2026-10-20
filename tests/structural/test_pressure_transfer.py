# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand calculations for pressure-transfer conservation checks.

All dimensions, pressures and tolerances below are synthetic test inputs, not
design criteria. Force basis: NASA, Aerodynamic Forces (2025), pressure-area
summation. Moment basis: OpenStax, University Physics Volume 1 (2016), section
10.6, equations 10.22 and 10.24. No solver or measured input is represented.
"""

from dataclasses import replace
import json
from pathlib import Path
import runpy

import pytest

from digitalmodel.structural.pressure_transfer import (
    Resultant,
    SurfacePressure,
    TransferCriteria,
    compare_transfer,
    pressure_resultant,
)


def resultant(patches, origin=(0, 0, 0)):
    return pressure_resultant(
        patches, origin_m=origin, frame="common-cartesian", case_id="synthetic-instant"
    )


def criteria(force=(0, 0, 0), moment=(0, 0, 0)):
    return TransferCriteria(force, moment, "Synthetic exact arithmetic criterion")


def test_uniform_patch_hand_calculation():
    """p=2 Pa, S=(0,0,3) m², r=(4,5,0) m: Fz=-6 N.

    Mx=y*Fz=-30 N m; My=-x*Fz=24 N m; Mz=0 N m.
    Positive pressure acts opposite the outward area vector.
    """
    answer = resultant([SurfacePressure(2, (0, 0, 3), (4, 5, 0))])
    assert answer.force_n == (0, 0, -6)
    assert answer.moment_nm == (-30, 24, 0)


def test_all_components_and_shifted_origin():
    """p=2, S=(1,2,3): F=(-2,-4,-6); r=(4,5,6).

    r cross F=(-6,12,-6). About O=(1,1,1), r-O=(3,4,5),
    so M=(-4,8,-4). All six components follow direct multiplication.
    """
    patch = SurfacePressure(2, (1, 2, 3), (4, 5, 6))
    assert resultant([patch]).force_n == (-2, -4, -6)
    assert resultant([patch]).moment_nm == (-6, 12, -6)
    assert resultant([patch], (1, 1, 1)).moment_nm == (-4, 8, -4)


def test_zero_force_couple_retains_moment():
    """Opposed 6 N forces at x=+1 and x=-1 give Fy=0, Mz=-12 N m."""
    answer = resultant(
        [
            SurfacePressure(2, (0, 3, 0), (1, 0, 0)),
            SurfacePressure(2, (0, -3, 0), (-1, 0, 0)),
        ]
    )
    assert answer.force_n == (0, 0, 0)
    assert answer.moment_nm == (0, 0, -12)


def test_split_patch_conserves_both_resultants():
    """Two 3 m² patches at x=1,3 replace 6 m² at x=2: Fz=-12, My=24."""
    source = resultant([SurfacePressure(2, (0, 0, 6), (2, 0, 0))])
    mapped = resultant([SurfacePressure(2, (0, 0, 3), (x, 0, 0)) for x in (1, 3)])
    assert compare_transfer(source, mapped, criteria()).status == "PASS"


def test_equal_force_wrong_location_fails_moment():
    """Moving Fz=-6 from x=4 to x=5 preserves force but changes My by 6 N m."""
    source = resultant([SurfacePressure(2, (0, 0, 3), (4, 5, 0))])
    mapped = resultant([SurfacePressure(2, (0, 0, 3), (5, 5, 0))])
    check = compare_transfer(source, mapped, criteria())
    assert check.status == "FAIL"
    assert check.force_difference_n == (0, 0, 0)
    assert check.moment_difference_nm == (0, 6, 0)


def test_signed_pressure_is_permitted():
    """p=-2 Pa reverses the 6 N traction; it is a signed instantaneous load."""
    assert resultant([SurfacePressure(-2, (0, 0, 3), (0, 0, 0))]).force_n == (0, 0, 6)


@pytest.mark.parametrize("component", range(6))
def test_every_component_is_checked_at_and_beyond_limit(component):
    """Synthetic component limit=2; delta=2 passes and delta=3 fails."""
    source = Resultant((0, 0, 0), (0, 0, 0), (0, 0, 0), "frame", "instant")
    for delta, expected in [(2, "PASS"), (3, "FAIL")]:
        values = [0] * 6
        values[component] = -delta
        mapped = replace(source, force_n=tuple(values[:3]), moment_nm=tuple(values[3:]))
        assert (
            compare_transfer(source, mapped, criteria((2, 2, 2), (2, 2, 2))).status
            == expected
        )


@pytest.mark.parametrize(
    "field,value",
    [
        ("origin_m", (1, 0, 0)),
        ("frame", "another-frame"),
        ("case_id", "another-instant"),
    ],
)
def test_incompatible_metadata_is_rejected(field, value):
    source = resultant([SurfacePressure(2, (0, 0, 3), (4, 5, 0))])
    with pytest.raises(ValueError, match="same origin, frame and case"):
        compare_transfer(source, replace(source, **{field: value}), criteria())


def test_missing_criteria_never_passes_even_for_equal_loads():
    source = resultant([SurfacePressure(2, (0, 0, 3), (4, 5, 0))])
    check = compare_transfer(source, source, None)
    assert check.status == "NOT_EVALUATED"
    assert check.force_difference_n == (0, 0, 0)
    assert "criteria" in check.reason


@pytest.mark.parametrize("missing", ["source", "mapped", "both"])
def test_missing_evidence_is_not_evaluated(missing):
    load = resultant([SurfacePressure(2, (0, 0, 3), (4, 5, 0))])
    check = compare_transfer(
        load if missing == "mapped" else None,
        load if missing == "source" else None,
        criteria(),
    )
    assert check.status == "NOT_EVALUATED"
    assert check.force_difference_n is None
    assert check.moment_difference_nm is None


@pytest.mark.parametrize("bad", [float("nan"), float("inf"), -float("inf"), True, "2"])
def test_non_numeric_or_nonfinite_pressure_is_rejected(bad):
    with pytest.raises(ValueError):
        SurfacePressure(bad, (0, 0, 3), (4, 5, 0))


@pytest.mark.parametrize(
    "field,bad",
    [
        ("area_vector_m2", (0, 0, 0)),
        ("area_vector_m2", (1, 2)),
        ("area_vector_m2", (1, 2, float("nan"))),
        ("centroid_m", (1, float("inf"), 3)),
        ("centroid_m", "123"),
        ("centroid_m", (1, True, 3)),
    ],
)
def test_invalid_geometry_is_rejected(field, bad):
    args = {"pressure_pa": 2, "area_vector_m2": (0, 0, 3), "centroid_m": (4, 5, 0)}
    args[field] = bad
    with pytest.raises(ValueError):
        SurfacePressure(**args)


@pytest.mark.parametrize(
    "force,moment,source",
    [
        ((-1, 0, 0), (0, 0, 0), "synthetic"),
        ((0, 0, 0), (float("nan"), 0, 0), "synthetic"),
        ((0, 0, 0), (0, 0, 0), " "),
    ],
)
def test_invalid_criteria_are_rejected(force, moment, source):
    with pytest.raises(ValueError):
        TransferCriteria(force, moment, source)


def test_empty_surface_is_rejected():
    with pytest.raises(ValueError, match="nonempty"):
        resultant([])


def test_overflow_cannot_pass():
    """Extreme synthetic inputs exercise finite-result validation, not a load case."""
    with pytest.raises(ValueError):
        resultant([SurfacePressure(1e308, (0, 0, 3), (4, 5, 0))])


def test_worked_example_matches_independent_answers():
    """Original Fz=-6, M=(-30,24,0); x shift of 1 gives delta My=+6.

    Each half-area patch has Fz=-3 at x=3,5 and y=5, summing to
    Fz=-6, Mx=-30, My=9+15=24. The synthetic zero limits require
    exact agreement for these integer arithmetic cases only.
    """
    path = (
        Path(__file__).resolve().parents[2]
        / "examples/ansys/pressure-transfer/input.json"
    )
    data = json.loads(path.read_text())
    source = resultant([SurfacePressure(**patch) for patch in data["source"]])
    assert source.force_n == (0, 0, -6)
    assert source.moment_nm == (-30, 24, 0)
    for name, expected in [("mapped", "PASS"), ("shifted", "FAIL")]:
        mapped = resultant([SurfacePressure(**patch) for patch in data[name]])
        assert compare_transfer(source, mapped, criteria()).status == expected


def test_runnable_example_outputs_hand_calculated_comparisons(capsys):
    """The preceding hand calculation requires My difference=6 for the shifted case."""
    path = (
        Path(__file__).resolve().parents[2]
        / "examples/ansys/pressure-transfer/run_example.py"
    )
    runpy.run_path(str(path), run_name="__main__")
    data = json.loads(capsys.readouterr().out)
    assert data["source"]["force_n"] == [0, 0, -6]
    assert data["source"]["moment_nm"] == [-30, 24, 0]
    assert data["conserved"]["status"] == "PASS"
    assert data["shifted"]["status"] == "FAIL"
    assert data["shifted"]["moment_difference_nm"] == [0, 6, 0]
    assert data["missing_criteria"]["status"] == "NOT_EVALUATED"
    assert data["engineering_readiness"] == "NOT_EVALUATED"
