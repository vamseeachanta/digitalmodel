"""Calibrated screenshot traces without inferred grid, colour or branch identity.

Method: two-point affine axis calibration and column run centroids, generalised
from the repository's screenshot ``digitize.py`` prototype for issue #1855
(2026). A larger load is labelled upstroke only under the caller's explicit
upper-load/upstroke interpretation. This cannot identify direction from pixels.
"""

from __future__ import annotations

import csv
import math
from dataclasses import dataclass
from numbers import Real
from pathlib import Path

import numpy as np


@dataclass(frozen=True)
class AxisCalibration:
    """Two measured pixel/value pairs; no assumed axis scale or origin."""

    pixel0: float
    value0: float
    pixel1: float
    value1: float

    def __post_init__(self) -> None:
        if not all(
            isinstance(v, Real) and not isinstance(v, bool) and math.isfinite(v)
            for v in (self.pixel0, self.value0, self.pixel1, self.value1)
        ):
            raise ValueError("axis calibration must contain finite real numbers")
        if self.pixel0 == self.pixel1 or self.value0 == self.value1:
            raise ValueError("axis calibration must have nonzero pixel and value spans")

        scale = (self.value1 - self.value0) / (self.pixel1 - self.pixel0)
        if not math.isfinite(scale) or scale == 0:
            raise ValueError("axis calibration scale must be finite and nonzero")

    def value(self, pixel: float) -> float:
        """Apply the validated affine scale; reject nonfinite results."""
        scale = (self.value1 - self.value0) / (self.pixel1 - self.pixel0)
        value = self.value0 + (pixel - self.pixel0) * scale
        if not math.isfinite(value):
            raise ValueError("calibrated value must be finite")
        return value


@dataclass(frozen=True)
class DigitizedCard:
    """Column centroids; excluded columns remain gaps, never interpolated.

    ``run_counts > 2`` identifies an explicitly accepted outer envelope of
    multiple traces. Such an envelope is not a measured single stroke.
    """

    position_in: tuple[float, ...]
    upstroke_load_lb: tuple[float, ...]
    downstroke_load_lb: tuple[float, ...]
    run_counts: tuple[int, ...]
    excluded_columns: tuple[int, ...]

    def closed_card(self) -> tuple[tuple[float, ...], tuple[float, ...]]:
        """Traverse upper load left-to-right, lower load right-to-left.

        Joining retained columns uses straight segments. Callers must inspect
        gaps and endpoint coverage before interpreting area as full-cycle work.
        """
        if len(self.position_in) < 2:
            raise ValueError("need at least two resolved columns")
        return (
            self.position_in + self.position_in[::-1],
            self.upstroke_load_lb + self.downstroke_load_lb[::-1],
        )

    def write_csv(self, path: str | Path) -> None:
        """Write calibrated branch samples and trace multiplicity.

        Excluded pixel columns are omitted from CSV. Retain ``excluded_columns``
        separately to distinguish extraction gaps from ordinary sample spacing.
        """
        with Path(path).open("w", newline="", encoding="utf-8") as stream:
            writer = csv.writer(stream)
            writer.writerow(
                ("position_in", "upstroke_load_lb", "downstroke_load_lb", "run_count")
            )
            writer.writerows(
                zip(
                    self.position_in,
                    self.upstroke_load_lb,
                    self.downstroke_load_lb,
                    self.run_counts,
                )
            )


def digitize_mask(
    mask: np.ndarray,
    x_axis: AxisCalibration,
    load_axis: AxisCalibration,
    *,
    max_gap_px: int,
    allow_envelope: bool = False,
    pixel_origin: tuple[int, int] = (0, 0),
) -> DigitizedCard:
    """Extract extreme run centroids from a caller-segmented boolean image.

    ``max_gap_px`` is the largest row separation still belonging to one run;
    it must be measured for the image. One means immediate pixel adjacency.
    Blank and single-run columns are excluded. More than two runs require
    explicit envelope consent. Origin is (column, row) in calibration pixels.
    All numeric segmentation settings are required inputs, not tuned defaults.
    """
    mask = np.asarray(mask)
    if mask.ndim != 2 or mask.dtype != np.bool_ or not mask.size:
        raise ValueError("mask must be a nonempty two-dimensional boolean array")
    if type(max_gap_px) is not int or max_gap_px < 1:
        raise ValueError("max_gap_px must be a positive integer")
    if len(pixel_origin) != 2 or any(type(v) is not int for v in pixel_origin):
        raise ValueError("pixel_origin must contain two integers")
    if type(allow_envelope) is not bool:
        raise ValueError("allow_envelope requires explicit boolean consent")
    records = []
    excluded = []
    for col in range(mask.shape[1]):
        rows = np.flatnonzero(mask[:, col])
        runs = np.split(rows, np.flatnonzero(np.diff(rows) > max_gap_px) + 1)
        count = len(runs) if len(rows) else 0
        if count < 2 or (count > 2 and not allow_envelope):
            excluded.append(col + pixel_origin[0])
            continue
        loads = [load_axis.value(float(run.mean()) + pixel_origin[1]) for run in runs]
        records.append(
            (x_axis.value(col + pixel_origin[0]), max(loads), min(loads), count)
        )
    records.sort()
    return DigitizedCard(
        position_in=tuple(r[0] for r in records),
        upstroke_load_lb=tuple(r[1] for r in records),
        downstroke_load_lb=tuple(r[2] for r in records),
        run_counts=tuple(r[3] for r in records),
        excluded_columns=tuple(excluded),
    )


def digitize_image(
    path: str | Path,
    x_axis: AxisCalibration,
    load_axis: AxisCalibration,
    *,
    plot_box: tuple[int, int, int, int],
    rgb_min: tuple[int, int, int],
    rgb_max: tuple[int, int, int],
    max_gap_px: int,
    allow_envelope: bool = False,
) -> DigitizedCard:
    """Read an image with explicit crop and inclusive RGB trace bounds.

    Crop is (left, top, right-exclusive, bottom-exclusive). Calibration uses
    original image coordinates. Grid and labels must be excluded by the crop
    and colour selection; no automatic image interpretation is claimed.
    """
    from PIL import Image

    low, high = np.asarray(rgb_min), np.asarray(rgb_max)
    # RGB channel limits are the image format's unsigned eight-bit range.
    if (
        low.shape != (3,)
        or high.shape != (3,)
        or not np.isfinite(low).all()
        or not np.isfinite(high).all()
        or (low < 0).any()
        or (high > 255).any()
        or (low > high).any()
    ):
        raise ValueError("RGB bounds must be three ordered values in the byte range")
    with Image.open(path) as image:
        if len(plot_box) != 4 or any(type(v) is not int for v in plot_box):
            raise ValueError("plot_box must contain four integers")
        left, top, right, bottom = plot_box
        if not (0 <= left < right <= image.width and 0 <= top < bottom <= image.height):
            raise ValueError("plot_box must be a nonempty crop inside the image")
        pixels = np.asarray(image.convert("RGB").crop(plot_box))
    mask = ((pixels >= low) & (pixels <= high)).all(axis=2)
    return digitize_mask(
        mask,
        x_axis,
        load_axis,
        max_gap_px=max_gap_px,
        allow_envelope=allow_envelope,
        pixel_origin=(left, top),
    )
