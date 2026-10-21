"""Depth conversion and bounded, piecewise-linear tank calibration.

Basis: OpenStax, Precalculus 2e (2021), section 2.1, two-point lines.
Depth complement and interpolation are geometric identities. No empirical
coefficients, density defaults or engineering acceptance limits are supplied.
"""

from __future__ import annotations

import csv
import math
from bisect import bisect_left
from dataclasses import dataclass
from pathlib import Path
from typing import Literal


def finite(value: float, label: str) -> float:
    """Normalize finite scalar inputs; refuse missing values and booleans."""
    if isinstance(value, bool):
        raise ValueError(f"{label} must be a finite number")
    try:
        result = float(value)
    except (TypeError, ValueError, OverflowError) as exc:
        raise ValueError(f"{label} must be a finite number") from exc
    if not math.isfinite(result):
        raise ValueError(f"{label} must be a finite number")
    return result


def positive(value: float, label: str) -> float:
    result = finite(value, label)
    if result <= 0:
        raise ValueError(f"{label} must be positive")
    return result


def bounded_depth(depth_m: float, reference_height_m: float) -> float:
    height = positive(reference_height_m, "reference_height_m")
    depth = finite(depth_m, "depth_m")
    if not 0 <= depth <= height:
        raise ValueError("depth_m must be between zero and reference_height_m")
    return depth


def sounding_to_ullage(sounding_m: float, reference_height_m: float) -> float:
    """u = H - h, for a common datum and a tank-axis reading, all in metres."""
    depth = bounded_depth(sounding_m, reference_height_m)
    return float(reference_height_m) - depth


def ullage_to_sounding(ullage_m: float, reference_height_m: float) -> float:
    """h = H - u; this is the same depth-complement identity."""
    return sounding_to_ullage(ullage_m, reference_height_m)


def _interpolate(query: float, keys: tuple, values: tuple) -> float:
    query = finite(query, "query")
    if not keys[0] <= query <= keys[-1]:
        raise ValueError("query outside calibration range; extrapolation refused")
    hi = bisect_left(keys, query)
    if keys[hi] == query:
        return values[hi]
    lo = hi - 1
    # OpenStax, Precalculus 2e (2021), section 2.1: two-point line.
    fraction = (query - keys[lo]) / (keys[hi] - keys[lo])
    return values[lo] + fraction * (values[hi] - values[lo])


@dataclass(frozen=True)
class CalibrationTable:
    """Rows (depth_m, volume_m3), strictly ordered by increasing depth.

    Volume increases with sounding and decreases with ullage. Strict volume
    monotonicity makes the inverse unique. Partial tables are permitted, but
    queries are bounded by the measured rows, rather than the reference height.
    Calibration is for the reference attitude; corrections are not inferred.
    """

    points: tuple[tuple[float, float], ...]
    reading_kind: Literal["sounding", "ullage"]
    reference_height_m: float

    def __post_init__(self):
        height = positive(self.reference_height_m, "reference_height_m")
        if self.reading_kind not in ("sounding", "ullage"):
            raise ValueError("reading_kind must be sounding or ullage")
        try:
            points = tuple(
                (bounded_depth(depth, height), finite(volume, "volume_m3"))
                for depth, volume in self.points
            )
        except (TypeError, ValueError) as exc:
            raise ValueError("points must contain finite depth/volume pairs") from exc
        if len(points) < 2 or any(volume < 0 for _, volume in points):
            raise ValueError("at least two rows with nonnegative volumes are required")
        direction = 1 if self.reading_kind == "sounding" else -1
        for a, b in zip(points, points[1:]):
            if b[0] <= a[0] or direction * (b[1] - a[1]) <= 0:
                raise ValueError("depth and volume must be strictly monotonic")
        object.__setattr__(self, "points", points)
        object.__setattr__(self, "reference_height_m", height)

    def volume_at(self, depth_m: float) -> float:
        """Interpolate volume in m³ from the table's declared reading kind."""
        return _interpolate(depth_m, self.depths, self.volumes)

    def depth_at_volume(self, volume_m3: float) -> float:
        """Invert to sounding or ullage in metres, without extrapolation."""
        if self.reading_kind == "ullage":
            return _interpolate(volume_m3, self.volumes[::-1], self.depths[::-1])
        return _interpolate(volume_m3, self.volumes, self.depths)

    @property
    def depths(self) -> tuple[float, ...]:
        return tuple(depth for depth, _ in self.points)

    @property
    def volumes(self) -> tuple[float, ...]:
        return tuple(volume for _, volume in self.points)

    @classmethod
    def from_csv(
        cls, path: str | Path, *, reading_kind: str, reference_height_m: float
    ) -> CalibrationTable:
        """Read exactly depth_m,volume_m3 columns; extra columns are refused."""
        with Path(path).open(encoding="utf-8", newline="") as stream:
            reader = csv.DictReader(stream)
            if reader.fieldnames is None or sorted(reader.fieldnames) != [
                "depth_m",
                "volume_m3",
            ]:
                raise ValueError("CSV requires only depth_m and volume_m3 columns")
            points = _read_rows(list(reader))
        return cls(points, reading_kind, reference_height_m)

    @classmethod
    def from_yaml(cls, path: str | Path) -> CalibrationTable:
        """Read safe YAML, refusing duplicate keys and unsupported fields."""
        import yaml

        class UniqueKeyLoader(yaml.SafeLoader):
            def construct_mapping(self, node, deep=False):
                mapping = {}
                for key_node, value_node in node.value:
                    key = self.construct_object(key_node, deep=deep)
                    if not isinstance(key, str) or key in mapping:
                        raise ValueError("YAML keys must be unique strings")
                    mapping[key] = self.construct_object(value_node, deep=deep)
                return mapping

        loader = UniqueKeyLoader(Path(path).read_text(encoding="utf-8"))
        try:
            data = loader.get_single_data()
        except yaml.YAMLError as exc:
            raise ValueError("invalid calibration YAML") from exc
        finally:
            loader.dispose()
        if not isinstance(data, dict) or set(data) != {
            "reading_kind",
            "reference_height_m",
            "rows",
        }:
            raise ValueError("YAML requires reading_kind, reference_height_m and rows")
        return cls(
            _read_rows(data["rows"]), data["reading_kind"], data["reference_height_m"]
        )


def _read_rows(rows: list) -> tuple[tuple[float, float], ...]:
    if not isinstance(rows, list):
        raise ValueError("rows must be a list of depth_m/volume_m3 mappings")
    points = []
    for row in rows:
        if not isinstance(row, dict) or set(row) != {"depth_m", "volume_m3"}:
            raise ValueError("each row requires only depth_m and volume_m3")
        points.append(
            (finite(row["depth_m"], "depth_m"), finite(row["volume_m3"], "volume_m3"))
        )
    return tuple(points)
