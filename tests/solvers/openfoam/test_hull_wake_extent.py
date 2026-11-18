# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent synthetic geometry for the wake preflight.

Basis: the linear deep-water dispersion relation omega**2 = g*k with phase
speed U = omega/k gives lambda = 2*pi*U**2/g. The lateral envelope follows
right-triangle geometry: half_beam + (bow_x - aft_x)*tan(half_angle).
All dimensions, speeds, gravity and angles below are synthetic test inputs,
not recommended engineering criteria. Choosing g=2*pi makes lambda=U**2 m.
The shared synthetic fixture has stern=-0.05, bow=6.15, half_beam=0.45 m.
"""

import copy
import json
import math
import runpy
from dataclasses import replace
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam import hull_domain as hd
from digitalmodel.solvers.openfoam.hull_manifest import HullManifest
from digitalmodel.solvers.openfoam.hull_placement import HullPlacementError


@pytest.fixture
def hull(manifest_dict):
    return HullManifest.from_dict(manifest_dict)


@pytest.fixture
def domain(hull):
    return hd.build_hull_domain(hull)


@pytest.fixture
def boxes():
    # These two nested boxes both contain the synthetic meshed hull.
    return [((-6.0, -11.0, -0.2), (8.0, 0.0, 1.2)),
            ((-5.0, -10.0, -0.1), (7.0, 0.0, 1.0))]


def preflight(hull, domain, boxes, **changes):
    inputs = dict(speed_m_s=1.0, gravity_m_s2=2 * math.pi,
                  required_wake_wavelengths=2.0, wedge_half_angle_deg=45.0)
    inputs.update(changes)
    return hd.preflight_wake_extent(hull, domain, boxes, **inputs)


def test_current_extents_ignore_speed(hull, domain):
    """Identical geometry has identical boxes while doubling U quadruples lambda."""
    slow_boxes = hd.refinement_boxes(hull, domain)
    fast_boxes = hd.refinement_boxes(hull, domain)
    slow_lambda = 2 * math.pi * 1.0**2 / (2 * math.pi)
    fast_lambda = 2 * math.pi * 2.0**2 / (2 * math.pi)
    assert slow_boxes == fast_boxes
    assert fast_lambda == 4 * slow_lambda


def test_same_hull_different_speed_changes_requirement(hull, domain):
    broad = [((-15.0, -18.0, -0.1), (7.0, 0.0, 1.0))]
    slow = preflight(hull, domain, broad)
    fast = preflight(hull, domain, broad, speed_m_s=2.0)
    assert slow.required_aft_extent_m == pytest.approx(2.0)
    assert fast.required_aft_extent_m == pytest.approx(8.0)
    assert slow.required_lateral_half_width_m == pytest.approx(8.65)
    assert fast.required_lateral_half_width_m == pytest.approx(14.65)


def test_gravity_changes_requirement(hull, domain, boxes):
    # Doubling synthetic gravity halves lambda, hence halves aft reach.
    a = preflight(hull, domain, boxes)
    b = preflight(hull, domain, boxes, gravity_m_s2=4 * math.pi)
    assert b.wavelength_m == pytest.approx(0.5)
    assert b.required_aft_extent_m == pytest.approx(1.0)
    assert a.required_aft_extent_m == pytest.approx(2.0)


def test_angle_changes_lateral_requirement(hull, domain, boxes):
    # atan(1/2) makes the widening exactly half the streamwise separation.
    report = preflight(hull, domain, boxes,
                       wedge_half_angle_deg=math.degrees(math.atan(0.5)))
    assert report.required_lateral_half_width_m == pytest.approx(4.55)


def test_current_hull_scaled_boxes_are_refused(hull, domain):
    with pytest.raises(hd.HullWakeExtentError, match="box.*aft"):
        preflight(hull, domain, hd.refinement_boxes(hull, domain))


def test_insufficient_wake_refused(hull, domain, boxes):
    with pytest.raises(hd.HullWakeExtentError, match="aft") as caught:
        preflight(hull, domain, boxes, required_wake_wavelengths=5.0)
    assert caught.value.report is not None
    assert not caught.value.report.meets_requirement
    assert caught.value.report.to_provenance()["status"] == "FAIL"


def test_lateral_envelope_refused(hull, domain):
    narrow = [((-5.0, -8.0, -0.1), (7.0, 0.0, 1.0))]
    with pytest.raises(hd.HullWakeExtentError, match="lateral"):
        preflight(hull, domain, narrow)


def test_achieved_extent_provenance(hull, domain, boxes):
    """lambda=1; aft=4.95; lateral-limited reach=10-0.45-6.20=3.35 m."""
    report = preflight(hull, domain, boxes)
    p = json.loads(json.dumps(report.to_provenance(), allow_nan=False))
    assert p["status"] == "PASS"
    assert p["speed_m_s"] == 1.0
    assert p["gravity_m_s2"] == 2 * math.pi
    assert p["wedge_half_angle_deg"] == 45.0
    assert p["wavelength_m"] == pytest.approx(1.0)
    assert p["stern_x_m"] == -0.05
    assert p["bow_x_m"] == 6.15
    assert p["hull_half_beam_m"] == 0.45
    assert p["required"]["aft_extent_m"] == pytest.approx(2.0)
    assert p["required"]["wake_wavelengths"] == 2.0
    assert p["required"]["aft_x_m"] == pytest.approx(-2.05)
    assert p["required"]["lateral_half_width_m"] == pytest.approx(8.65)
    assert p["achieved"]["aft_extent_m"] == pytest.approx(4.95)
    assert p["achieved"]["lateral_half_width_m"] == pytest.approx(10.0)
    assert p["achieved"]["wake_wavelengths"] == pytest.approx(3.35)
    assert p["domain"]["aft_extent_m"] == pytest.approx(26.95)
    assert p["domain"]["lateral_half_width_m"] == pytest.approx(18.0)
    assert len(p["boxes"]) == 2
    assert p["boxes"][0]["aft_extent_m"] == pytest.approx(5.95)
    assert "geometry only" in p["limits"]


def test_each_stage_must_meet_requirement(hull, domain, boxes):
    lo, hi = boxes[-1]
    boxes[-1] = ((-0.5, lo[1], lo[2]), hi)
    with pytest.raises(hd.HullWakeExtentError, match="box 2.*aft"):
        preflight(hull, domain, boxes)


@pytest.mark.parametrize("axis", [0, 1, 2])
def test_box_outside_domain_refused(hull, domain, boxes, axis):
    lo, hi = boxes[0]
    outside = list(lo)
    outside[axis] = [domain.x_outlet, domain.y_side, domain.z_levels[0]][axis] - 1
    boxes[0] = (tuple(outside), hi)
    with pytest.raises(hd.HullWakeExtentError, match="outside domain"):
        preflight(hull, domain, boxes)


@pytest.mark.parametrize("change, reason", [
    ({"x_outlet": -1.0}, "domain.*aft"),
    ({"y_side": -8.0}, "domain.*lateral"),
])
def test_required_envelope_outside_domain_refused(hull, domain, boxes, change, reason):
    # Trim the boxes too, so the refusal measures the domain's own shortfall.
    trimmed = [((max(lo[0], change.get("x_outlet", lo[0])),
                 max(lo[1], change.get("y_side", lo[1])), lo[2]), hi)
               for lo, hi in boxes]
    with pytest.raises(hd.HullWakeExtentError, match=reason):
        preflight(hull, replace(domain, **change), trimmed)


@pytest.mark.parametrize("name", ["speed_m_s", "gravity_m_s2", "required_wake_wavelengths"])
@pytest.mark.parametrize("bad", [0.0, -1.0, math.nan, math.inf])
def test_invalid_positive_inputs_refused(hull, domain, boxes, name, bad):
    with pytest.raises(hd.HullWakeExtentError, match=name):
        preflight(hull, domain, boxes, **{name: bad})


@pytest.mark.parametrize("angle", [-1.0, 0.0, 90.0, 180.0, math.nan, math.inf])
def test_invalid_wedge_angle_refused(hull, domain, boxes, angle):
    with pytest.raises(hd.HullWakeExtentError, match="wedge_half_angle_deg"):
        preflight(hull, domain, boxes, wedge_half_angle_deg=angle)


@pytest.mark.parametrize("change", [
    {"half_domain": False}, {"flow_direction": (1.0, 0.0, 0.0)},
    {"y_centreplane": 1.0}, {"waterline": math.nan},
    {"x_outlet": math.nan}, {"x_inlet": -30.0},
    {"z_levels": (0.0, 0.0, 1.0)},
])
def test_unsupported_domain_refused(hull, domain, boxes, change):
    with pytest.raises(hd.HullWakeExtentError, match="domain"):
        preflight(hull, replace(domain, **change), boxes)


@pytest.mark.parametrize("bad_box", [
    ((math.nan, -10.0, -0.1), (7.0, 0.0, 1.0)),
    ((-5.0, -10.0, -0.1), (-6.0, 0.0, 1.0)),
    ((-5.0, -10.0), (7.0, 0.0, 1.0)),
    ((-5.0, -10.0, 0.3), (7.0, 0.0, 1.0)),
])
def test_invalid_or_non_straddling_box_refused(hull, domain, bad_box):
    with pytest.raises(hd.HullWakeExtentError, match="box"):
        preflight(hull, domain, [bad_box])


@pytest.mark.parametrize("axis, edge", [(0, 6.15 - 1e-12), (1, -1e-12)])
def test_envelope_must_reach_bow_and_centreplane(hull, domain, boxes, axis, edge):
    # Even a gap smaller than the legacy hull-containment tolerance clips the wedge.
    lo, hi = boxes[-1]
    hi = list(hi)
    hi[axis] = edge
    boxes[-1] = (lo, tuple(hi))
    with pytest.raises(hd.HullWakeExtentError, match="bow and centreplane"):
        preflight(hull, domain, boxes)


def test_hull_containment_guard_preserved(hull, domain, boxes):
    lo, hi = boxes[-1]
    boxes[-1] = (lo, (6.0, hi[1], hi[2]))
    with pytest.raises(HullPlacementError, match="does not contain the hull"):
        preflight(hull, domain, boxes)


def test_nested_box_guard_preserved(hull, domain, boxes):
    with pytest.raises(HullPlacementError, match="escapes"):
        preflight(hull, domain, list(reversed(boxes)))


def test_empty_boxes_refused(hull, domain):
    with pytest.raises(hd.HullWakeExtentError, match="boxes"):
        preflight(hull, domain, [])


@pytest.mark.parametrize("changes", [
    {"speed_m_s": 1e308}, {"speed_m_s": 1e-200},
    {"required_wake_wavelengths": 1e308, "speed_m_s": 2.0},
    {"wedge_half_angle_deg": 5e-324},
])
def test_nonfinite_derived_extent_refused(hull, domain, boxes, changes):
    with pytest.raises(hd.HullWakeExtentError, match="finite"):
        preflight(hull, domain, boxes, **changes)


def test_no_mutation(hull, domain, boxes):
    before = copy.deepcopy((hull, domain, boxes))
    preflight(hull, domain, boxes)
    assert (hull, domain, boxes) == before


def test_exact_boundary_is_accepted(hull, domain):
    aft = -0.05 - 2.0
    width = 0.45 + (6.15 - aft) * math.tan(math.radians(45.0))
    boundary = [((aft, -width, -0.1), (7.0, 0.0, 1.0))]
    assert preflight(hull, domain, boundary).meets_requirement


def test_synthetic_example_runs():
    script = Path(__file__).resolve().parents[3] / "examples/openfoam/wake_preflight.py"
    rows = runpy.run_path(str(script))["example_results"]()
    assert [row["status"] for row in rows] == ["PASS", "FAIL"]
    assert rows[0]["wavelength_m"] == pytest.approx(1.0)
    assert rows[1]["wavelength_m"] == pytest.approx(4.0)
