# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic geometry tests; no published dimensional or weight table."""
import math

import pytest

from digitalmodel.materials import (
    LinePipe,
    get,
    available_schedules,
    line_pipe,
    list_nps,
    od_for_nps,
    wt_for_schedule,
)
from digitalmodel.materials.grades import MaterialGrade


@pytest.mark.parametrize("nps", range(1, 18))
def test_outside_diameter_table(nps):
    # Synthetic dimensions are nps + 0.71, unrelated to product tables.
    assert od_for_nps(nps) == nps + 0.71


@pytest.mark.parametrize("nps", range(1, 12))
def test_plain_end_weight_from_synthetic_geometry(nps):
    pipe = line_pipe("X65", nps=nps, schedule="STD")
    od, wall = nps + 0.71, 0.071 + nps / 111
    expected = math.pi * wall * (od - wall) * 0.0254**2 * pipe.grade.rho_kg_m3
    assert pipe.weight_kg_m == pytest.approx(expected)
    assert pipe.weight_lb_ft == pytest.approx(expected * 0.671968994)


def test_weight_is_area_times_density():
    # Internal consistency: lb/ft derives from kg/m derives from metal area.
    pipe = line_pipe("X52", nps=8.0, schedule="STD")
    area_m2 = pipe.metal_area_in2 * 0.0254 ** 2
    assert math.isclose(pipe.weight_kg_m, area_m2 * pipe.grade.rho_kg_m3, rel_tol=1e-12)
    assert math.isclose(pipe.weight_lb_ft, pipe.weight_kg_m * 0.671969, rel_tol=1e-6)


# --- Geometry -----------------------------------------------------------------
def test_geometry_derivations():
    pipe = line_pipe("X60", nps=8.0, schedule="STD")  # Synthetic fixture geometry.
    assert pipe.od_in == 8.71
    assert pipe.wt_in == (0.071 + 8 / 111)
    assert math.isclose(pipe.id_in, 8.71 - 2 * (0.071 + 8 / 111), rel_tol=1e-12)
    assert math.isclose(pipe.d_over_t, 8.71 / (0.071 + 8 / 111), rel_tol=1e-12)
    expected_area = math.pi / 4 * (pipe.od_in ** 2 - pipe.id_in ** 2)
    assert math.isclose(pipe.metal_area_in2, expected_area, rel_tol=1e-12)
    assert math.isclose(pipe.od_mm, 8.71 * 25.4, rel_tol=1e-12)


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
    assert pipe.grade.smys_mpa == 317.0
    assert "API 5L X65" in pipe.api_5l_reference
    assert "NPS 12" in pipe.api_5l_reference


def test_accepts_grade_object():
    from digitalmodel.materials import get

    g = get("X70")
    pipe = line_pipe(g, nps=10.0, schedule="STD")
    assert pipe.grade is g


# --- Schedule lookup & normalisation ------------------------------------------
def test_schedule_normalisation():
    assert wt_for_schedule(8.0, "SCH 80") == (0.071 + 8 / 111 + 0.031)
    assert wt_for_schedule(8.0, "sch80") == (0.071 + 8 / 111 + 0.031)
    assert wt_for_schedule(8.0, "80") == (0.071 + 8 / 111 + 0.031)
    assert wt_for_schedule(8.0, "XS") == (0.071 + 8 / 111 + 0.031)


def test_std_equals_sch40_for_small_sizes():
    assert wt_for_schedule(8.0, "STD") == wt_for_schedule(8.0, "SCH 40")


def test_available_schedules_nonempty():
    scheds = available_schedules(12.0)
    assert "STD" in scheds and "SCH 80" in scheds


# --- Explicit OD/WT path ------------------------------------------------------
def test_explicit_od_wt():
    pipe = line_pipe("X52", od_in=10.0, wt_in=0.5,
                     geometry_source=get("X52").source)
    assert pipe.od_in == 10.0 and pipe.wt_in == 0.5
    assert pipe.nps is None and pipe.schedule is None
    assert "OD 10 in" in pipe.api_5l_reference


# --- Error handling -----------------------------------------------------------
def test_errors():
    with pytest.raises(ValueError):
        line_pipe("X52", nps=8.0, od_in=8.71)          # both diameters
    with pytest.raises(ValueError):
        line_pipe("X52", nps=8.0)                        # no wall
    with pytest.raises(ValueError):
        line_pipe("X52", od_in=8.71, schedule="STD")   # schedule needs nps
    with pytest.raises(KeyError):
        line_pipe("X52", nps=8.0, schedule="SCH 999")    # bad schedule
    with pytest.raises(KeyError):
        od_for_nps(99.0)                                   # not a catalog NPS
    with pytest.raises(ValueError):
        LinePipe(od_in=8.71, wt_in=5.0, grade=None)     # wt >= od/2


# --- Catalog coverage ---------------------------------------------------------
def test_catalog_spans_full_range():
    sizes = list_nps()
    assert sizes[0] == 1
    assert sizes[-1] == 20
    assert 8.0 in sizes and 20.0 in sizes
