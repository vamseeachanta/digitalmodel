# ABOUTME: Golden tests for the API 5L line-pipe catalog — ASME B36.10M OD /
# ABOUTME: weight validation, schedule lookup, grade integration, query API.
import math

import pytest

from digitalmodel.materials import (
    LinePipe,
    available_schedules,
    line_pipe,
    list_nps,
    od_for_nps,
    wt_for_schedule,
)
from digitalmodel.materials.grades import MaterialGrade


# --- ASME B36.10M outside diameters -------------------------------------------
@pytest.mark.parametrize(
    "nps, od",
    [
        (0.125, 0.405), (0.5, 0.840), (0.75, 1.050), (1.0, 1.315),
        (1.5, 1.900), (2.0, 2.375), (3.0, 3.500), (4.0, 4.500),
        (6.0, 6.625), (8.0, 8.625), (10.0, 10.750), (12.0, 12.750),
        (14.0, 14.0), (24.0, 24.0), (36.0, 36.0), (48.0, 48.0), (80.0, 80.0),
    ],
)
def test_outside_diameter_table(nps, od):
    assert od_for_nps(nps) == od


# --- Plain-end weight vs published ASME B36.10M (lb/ft) ------------------------
# Reference weights from the ASME B36.10M weight column; the formula is exact,
# published values carry rounding, so a 0.3% tolerance is used.
@pytest.mark.parametrize(
    "nps, schedule, pub_lb_ft",
    [
        (1.0, "STD", 1.679),
        (2.0, "STD", 3.653),
        (4.0, "STD", 10.79),
        (6.0, "STD", 18.97),
        (8.0, "STD", 28.55),
        (8.0, "XS", 43.39),
        (10.0, "STD", 40.48),
        (12.0, "STD", 49.56),
        (12.0, "XS", 65.42),
        (16.0, "SCH 40", 82.77),
        (24.0, "XS", 125.49),
    ],
)
def test_plain_end_weight_matches_asme(nps, schedule, pub_lb_ft):
    pipe = line_pipe("X65", nps=nps, schedule=schedule)
    assert math.isclose(pipe.weight_lb_ft, pub_lb_ft, rel_tol=3e-3)


def test_weight_is_area_times_density():
    # Internal consistency: lb/ft derives from kg/m derives from metal area.
    pipe = line_pipe("X52", nps=8.0, schedule="STD")
    area_m2 = pipe.metal_area_in2 * 0.0254 ** 2
    assert math.isclose(pipe.weight_kg_m, area_m2 * 7850.0, rel_tol=1e-12)
    assert math.isclose(pipe.weight_lb_ft, pipe.weight_kg_m * 0.671969, rel_tol=1e-6)


# --- Geometry -----------------------------------------------------------------
def test_geometry_derivations():
    pipe = line_pipe("X60", nps=8.0, schedule="STD")  # OD 8.625, t 0.322
    assert pipe.od_in == 8.625
    assert pipe.wt_in == 0.322
    assert math.isclose(pipe.id_in, 8.625 - 2 * 0.322, rel_tol=1e-12)
    assert math.isclose(pipe.d_over_t, 8.625 / 0.322, rel_tol=1e-12)
    expected_area = math.pi / 4 * (pipe.od_in ** 2 - pipe.id_in ** 2)
    assert math.isclose(pipe.metal_area_in2, expected_area, rel_tol=1e-12)
    assert math.isclose(pipe.od_mm, 8.625 * 25.4, rel_tol=1e-12)


def test_bore_volume_positive_and_consistent():
    pipe = line_pipe("X65", nps=12.0, schedule="SCH 80")
    assert pipe.bore_area_in2 > 0
    assert pipe.internal_volume_m3_per_m == pytest.approx(
        pipe.bore_area_in2 * 0.0254 ** 2)
    assert pipe.internal_volume_bbl_per_ft > 0


# --- Grade integration --------------------------------------------------------
def test_carries_material_grade():
    pipe = line_pipe("X65", nps=12.0, schedule="SCH 80")
    assert isinstance(pipe.grade, MaterialGrade)
    assert pipe.grade.name == "X65"
    assert pipe.grade.smys_mpa == 450.0
    assert "API 5L X65" in pipe.api_5l_reference
    assert "NPS 12" in pipe.api_5l_reference


def test_accepts_grade_object():
    from digitalmodel.materials import get

    g = get("X70")
    pipe = line_pipe(g, nps=10.0, schedule="STD")
    assert pipe.grade is g


# --- Schedule lookup & normalisation ------------------------------------------
def test_schedule_normalisation():
    assert wt_for_schedule(8.0, "SCH 80") == 0.500
    assert wt_for_schedule(8.0, "sch80") == 0.500
    assert wt_for_schedule(8.0, "80") == 0.500
    assert wt_for_schedule(8.0, "XS") == 0.500


def test_std_equals_sch40_for_small_sizes():
    assert wt_for_schedule(8.0, "STD") == wt_for_schedule(8.0, "SCH 40")


def test_available_schedules_nonempty():
    scheds = available_schedules(12.0)
    assert "STD" in scheds and "SCH 80" in scheds


# --- Explicit OD/WT path ------------------------------------------------------
def test_explicit_od_wt():
    pipe = line_pipe("X52", od_in=10.0, wt_in=0.5)
    assert pipe.od_in == 10.0 and pipe.wt_in == 0.5
    assert pipe.nps is None and pipe.schedule is None
    assert "OD 10 in" in pipe.api_5l_reference


# --- Error handling -----------------------------------------------------------
def test_errors():
    with pytest.raises(ValueError):
        line_pipe("X52", nps=8.0, od_in=8.625)          # both diameters
    with pytest.raises(ValueError):
        line_pipe("X52", nps=8.0)                        # no wall
    with pytest.raises(ValueError):
        line_pipe("X52", od_in=8.625, schedule="STD")   # schedule needs nps
    with pytest.raises(KeyError):
        line_pipe("X52", nps=8.0, schedule="SCH 999")    # bad schedule
    with pytest.raises(KeyError):
        od_for_nps(7.0)                                   # not a catalog NPS
    with pytest.raises(ValueError):
        LinePipe(od_in=8.625, wt_in=5.0, grade=None)     # wt >= od/2


# --- Catalog coverage ---------------------------------------------------------
def test_catalog_spans_full_range():
    sizes = list_nps()
    assert sizes[0] == 0.125
    assert sizes[-1] == 80.0
    assert 8.0 in sizes and 24.0 in sizes
