"""Synthetic raster tests; expectations are independent coordinate calculations."""

import csv

import numpy as np
import pytest
from PIL import Image

from digitalmodel.marine_ops.artificial_lift.dynacard.rod_pump.digitization import (
    AxisCalibration,
    digitize_image,
    digitize_mask,
)


def axes():
    """Synthetic axes: x = 2*column; load = 100 - 10*row (arbitrary units)."""
    return AxisCalibration(0, 0, 4, 8), AxisCalibration(0, 100, 9, 10)


def test_calibration_and_thick_trace_centroids(tmp_path):
    """Rows 1,2 give 85; rows 7,8 give 25; x=0,2,...,8.

    Closed rectangle area = width 8 * load difference 60 = 480.
    This is a hand-created raster, not a field calibration.
    """
    mask = np.zeros((10, 5), dtype=bool)
    mask[1:3] = True
    mask[7:9] = True
    card = digitize_mask(mask, *axes(), max_gap_px=1)
    assert card.position_in == (0, 2, 4, 6, 8)
    assert card.upstroke_load_lb == (85,) * 5
    assert card.downstroke_load_lb == (25,) * 5
    assert card.run_counts == (2,) * 5
    x, load = card.closed_card()
    from digitalmodel.marine_ops.artificial_lift.dynacard.rod_pump import card_area

    assert card_area(x, load) == pytest.approx(480)
    target = tmp_path / "card.csv"
    card.write_csv(target)
    with target.open() as stream:
        rows = list(csv.DictReader(stream))
    assert len(rows) == 5
    assert float(rows[0]["upstroke_load_lb"]) == 85


def test_missing_merged_and_ambiguous_columns_are_reported():
    """No interpolation: blank, single-run and three-run columns are excluded."""
    mask = np.zeros((10, 5), dtype=bool)
    mask[[1, 8], 0] = True
    mask[1, 2] = True
    mask[[1, 4, 8], 3] = True
    mask[[1, 8], 4] = True
    card = digitize_mask(mask, *axes(), max_gap_px=1)
    assert card.position_in == (0, 8)
    assert card.excluded_columns == (1, 2, 3)
    envelope = digitize_mask(mask, *axes(), max_gap_px=1, allow_envelope=True)
    assert envelope.run_counts == (2, 3, 2)
    assert envelope.excluded_columns == (1, 2)


def test_reversed_axis_still_orders_positions_and_loads():
    """x=8-2*c; load=10*r; physical branches follow load, not image row."""
    mask = np.zeros((10, 5), dtype=bool)
    mask[[1, 8]] = True
    card = digitize_mask(
        mask, AxisCalibration(0, 8, 4, 0), AxisCalibration(0, 0, 9, 90), max_gap_px=1
    )
    assert card.position_in == (0, 2, 4, 6, 8)
    assert card.upstroke_load_lb == (80,) * 5


def test_image_crop_uses_original_pixel_coordinates(tmp_path):
    """Trace at original rows 2/7, columns 1..3: loads 80/30, x=2,4,6."""
    rgb = np.full((10, 5, 3), 255, dtype=np.uint8)
    rgb[[2, 7], 1:4] = (10, 20, 30)
    path = tmp_path / "synthetic.png"
    Image.fromarray(rgb).save(path)
    card = digitize_image(
        path,
        *axes(),
        plot_box=(1, 1, 4, 9),
        rgb_min=(10, 20, 30),
        rgb_max=(10, 20, 30),
        max_gap_px=1,
    )
    assert card.position_in == (2, 4, 6)
    assert card.upstroke_load_lb == (80,) * 3
    assert card.downstroke_load_lb == (30,) * 3


@pytest.mark.parametrize(
    "values", [(0, 0, 0, 1), (0, 0, 1, 0), (0, float("nan"), 1, 1)]
)
def test_invalid_calibration(values):
    """Coincident pixels, zero physical scale and nonfinite values are invalid."""
    with pytest.raises(ValueError):
        AxisCalibration(*values)


@pytest.mark.parametrize(
    "mask", [np.ones(5, dtype=bool), np.ones((3, 3)), np.zeros((0, 3), dtype=bool)]
)
def test_invalid_mask(mask):
    with pytest.raises(ValueError):
        digitize_mask(mask, *axes(), max_gap_px=1)


@pytest.mark.parametrize("gap", [0, -1, 1.5, True])
def test_invalid_gap(gap):
    with pytest.raises(ValueError):
        digitize_mask(np.ones((3, 3), dtype=bool), *axes(), max_gap_px=gap)


def test_no_resolvable_branches_cannot_make_closed_card():
    card = digitize_mask(np.zeros((10, 5), dtype=bool), *axes(), max_gap_px=1)
    assert card.excluded_columns == (0, 1, 2, 3, 4)
    with pytest.raises(ValueError):
        card.closed_card()


def test_explicit_pixel_gap_merges_only_requested_runs():
    """Synthetic rows 1,3 bridge at gap=2; mean row=2 => load=80.

    Rows 7,8 remain a second run, mean row=7.5 => load=25.
    At gap=1 there are three runs, so the column is ambiguous.
    """
    mask = np.zeros((10, 5), dtype=bool)
    mask[[1, 3, 7, 8]] = True
    assert not digitize_mask(mask, *axes(), max_gap_px=1).position_in
    card = digitize_mask(mask, *axes(), max_gap_px=2)
    assert card.upstroke_load_lb == (80,) * 5
    assert card.downstroke_load_lb == (25,) * 5


@pytest.mark.parametrize(
    "kwargs",
    [
        {"plot_box": (-1, 0, 5, 10)},
        {"plot_box": (0, 0, 6, 10)},
        {"plot_box": (0, 0, 0, 10)},
        {"rgb_min": (40, 20, 30)},
        {"rgb_max": (256, 30, 40)},
        {"rgb_min": (float("nan"), 0, 0)},
        {"rgb_min": (0, 0)},
        {"allow_envelope": "false"},
    ],
)
def test_invalid_image_settings(tmp_path, kwargs):
    image = tmp_path / "blank.png"
    Image.fromarray(np.zeros((10, 5, 3), dtype=np.uint8)).save(image)
    settings = dict(
        plot_box=(0, 0, 5, 10), rgb_min=(10, 20, 30), rgb_max=(20, 30, 40), max_gap_px=1
    )
    with pytest.raises(ValueError):
        digitize_image(image, *axes(), **{**settings, **kwargs})
