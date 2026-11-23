"""Calibration checks using independent prismatic-tank hand calculations.

All dimensions are synthetic test inputs, rather than material properties.
A 10 m by 4 m box has area 40 m²: V = 40 h. At H = 3 m,
u = 3 - h, so V = 40 (3 - u). Linear interpolation follows the
two-point line in OpenStax, Precalculus 2e (2021), section 2.1.
Floating-point comparisons use pytest's numerical tolerance, not an
engineering acceptance limit.
"""

import math

import pytest

from digitalmodel.naval_architecture.tank_gauging import (
    CalibrationTable,
    sounding_to_ullage,
    ullage_to_sounding,
)


@pytest.mark.parametrize("sounding", [0.0, 0.25, 1.5, 3.0])
def test_depth_round_trip(sounding):
    assert sounding_to_ullage(sounding, 3) == pytest.approx(3 - sounding)
    assert ullage_to_sounding(sounding_to_ullage(sounding, 3), 3) == sounding


@pytest.mark.parametrize(
    "depth,height", [(-1, 3), (4, 3), (0, 0), (0, -1), (math.nan, 3), (0, math.inf)]
)
def test_invalid_depth(depth, height):
    for convert in (sounding_to_ullage, ullage_to_sounding):
        with pytest.raises(ValueError):
            convert(depth, height)


@pytest.mark.parametrize(
    "kind,points,reading",
    [
        ("sounding", ((0, 0), (1, 40), (3, 120)), 1.5),
        ("ullage", ((0, 120), (2, 40), (3, 0)), 1.5),
    ],
)
def test_table_interpolation_and_inverse(kind, points, reading):
    table = CalibrationTable(points, kind, 3)
    assert table.volume_at(reading) == pytest.approx(60)
    assert table.depth_at_volume(60) == pytest.approx(reading)
    for depth, volume in points:
        assert table.volume_at(depth) == volume
        assert table.depth_at_volume(volume) == depth


@pytest.mark.parametrize(
    "kind,points,queries",
    [
        ("sounding", ((0, 0), (1, 20), (3, 100)), ((0.5, 10), (2, 60))),
        ("ullage", ((0, 100), (2, 20), (3, 0)), ((1, 60), (2.5, 10))),
    ],
)
def test_piecewise_slopes_and_nonuniform_depth_spacing(kind, points, queries):
    """Synthetic non-box rows check local, rather than endpoint-only slopes.

    Sounding segments have slopes 20 and 40 m³/m: half the first
    interval gives 10 m³; half the second gives 20+(100-20)/2=60 m³.
    Ullage reverses the same geometry. Two-point line: OpenStax (2021), §2.1.
    """
    table = CalibrationTable(points, kind, 3)
    for depth, volume in queries:
        assert table.volume_at(depth) == pytest.approx(volume)
        assert table.depth_at_volume(volume) == pytest.approx(depth)


@pytest.mark.parametrize("query", [-0.1, 3.1, math.nan, math.inf])
def test_no_depth_extrapolation(query):
    table = CalibrationTable(((0, 0), (3, 120)), "sounding", 3)
    with pytest.raises(ValueError):
        table.volume_at(query)


@pytest.mark.parametrize("query", [-1, 121, math.nan, math.inf])
def test_no_volume_extrapolation(query):
    table = CalibrationTable(((0, 0), (3, 120)), "sounding", 3)
    with pytest.raises(ValueError):
        table.depth_at_volume(query)


@pytest.mark.parametrize(
    "points,kind,height",
    [
        (((0, 0),), "sounding", 3),
        (((0, 0), (0, 120)), "sounding", 3),
        (((3, 120), (0, 0)), "sounding", 3),
        (((0, 0), (3, 0)), "sounding", 3),
        (((0, 120), (3, 0)), "sounding", 3),
        (((0, 0), (3, 120)), "ullage", 3),
        (((-1, 0), (3, 120)), "sounding", 3),
        (((0, -1), (3, 120)), "sounding", 3),
        (((0, 0), (4, 120)), "sounding", 3),
        (((0, 0), (3, math.inf)), "sounding", 3),
        (((0, 0), (3, 120)), "unknown", 3),
        (((0, 0), (3, 120)), "sounding", math.nan),
    ],
)
def test_invalid_table(points, kind, height):
    with pytest.raises(ValueError):
        CalibrationTable(points, kind, height)


def test_partial_table_refuses_unmeasured_depth_and_volume():
    table = CalibrationTable(((1, 40), (2, 80)), "sounding", 3)
    with pytest.raises(ValueError):
        table.volume_at(0.5)
    with pytest.raises(ValueError):
        table.depth_at_volume(100)


def test_csv_loading(tmp_path):
    path = tmp_path / "calibration.csv"
    path.write_text("depth_m,volume_m3\n0,0\n1,40\n3,120\n", encoding="utf-8")
    table = CalibrationTable.from_csv(
        path, reading_kind="sounding", reference_height_m=3
    )
    assert table.volume_at(1.5) == 60


def test_yaml_loading(tmp_path):
    path = tmp_path / "calibration.yaml"
    path.write_text(
        "reading_kind: ullage\nreference_height_m: 3\nrows:\n"
        "  - {depth_m: 0, volume_m3: 120}\n"
        "  - {depth_m: 3, volume_m3: 0}\n",
        encoding="utf-8",
    )
    table = CalibrationTable.from_yaml(path)
    assert table.volume_at(1.5) == 60
    assert table.depth_at_volume(40) == 2


@pytest.mark.parametrize(
    "contents",
    [
        "depth_m,volume_m3,trim_correction_m\n0,0,0\n3,120,0\n",
        "depth_m,volume_m3\n0,0,unexpected\n3,120\n",
        "depth_m,depth_m\n0,0\n3,120\n",
        "depth_m,volume_m3\n0,0\n3,\n",
    ],
)
def test_csv_rejects_ambiguous_or_unsupported_columns(tmp_path, contents):
    path = tmp_path / "calibration.csv"
    path.write_text(contents, encoding="utf-8")
    with pytest.raises(ValueError):
        CalibrationTable.from_csv(path, reading_kind="sounding", reference_height_m=3)


@pytest.mark.parametrize(
    "contents",
    [
        "[]",
        "reading_kind: sounding",
        "!!python/object:untrusted {}",
        "reading_kind: sounding\nreference_height_m: 3\nrows: null\n",
        "reading_kind: sounding\nreference_height_m: 3\ncorrections: []\nrows: []\n",
        "reading_kind: sounding\nreference_height_m: 3\nrows:\n"
        "  - {depth_m: 0, volume_m3: 0, list_correction_m: 0}\n",
        "reading_kind: sounding\nreading_kind: ullage\nreference_height_m: 3\nrows:\n"
        "  - {depth_m: 0, volume_m3: 120}\n  - {depth_m: 3, volume_m3: 0}\n",
        "reading_kind: sounding\nreference_height_m: 3\nrows:\n"
        "  - {depth_m: 0, volume_m3: 120, volume_m3: 0}\n"
        "  - {depth_m: 3, volume_m3: 120}\n",
    ],
)
def test_yaml_rejects_malformed_or_unsupported_data(tmp_path, contents):
    path = tmp_path / "calibration.yaml"
    path.write_text(contents, encoding="utf-8")
    with pytest.raises(ValueError):
        CalibrationTable.from_yaml(path)
