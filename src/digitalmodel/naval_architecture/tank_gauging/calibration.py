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
    Reference lookup does not apply corrections. Optional correction columns
    are signed additions to the declared reading at one explicit attitude.
    They are indexed by the raw gauge reading, not the corrected depth.
    """

    points: tuple[tuple[float, float], ...]
    reading_kind: Literal["sounding", "ullage"]
    reference_height_m: float
    trim_corrections_m: tuple[float, ...] | None = None
    list_corrections_m: tuple[float, ...] | None = None
    correction_attitude_deg: tuple[float, float] | None = None

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
        has_corrections = False
        for name in ("trim_corrections_m", "list_corrections_m"):
            values = getattr(self, name)
            if values is None:
                continue
            has_corrections = True
            try:
                values = tuple(finite(value, name) for value in values)
            except TypeError as exc:
                raise ValueError(f"{name} must contain one value per row") from exc
            if len(values) != len(points):
                raise ValueError(f"{name} must contain one value per row")
            object.__setattr__(self, name, values)
        if has_corrections != (self.correction_attitude_deg is not None):
            raise ValueError(
                "correction columns require an explicit trim/list attitude"
            )
        if has_corrections:
            try:
                trim, heel = self.correction_attitude_deg
            except (TypeError, ValueError) as exc:
                raise ValueError(
                    "correction attitude must be a trim/list pair"
                ) from exc
            attitude = (finite(trim, "trim_deg"), finite(heel, "list_deg"))
            # Same single-valued surface domain as centre_sounding:
            # OpenStax Precalculus 2e (2021), §5.4, vertical tangent excluded.
            if any(abs(angle) >= 90 for angle in attitude):
                raise ValueError("correction angles must lie between -90 and 90")
            object.__setattr__(self, "correction_attitude_deg", attitude)

    def corrected_depth_at(
        self, gauge_depth_m: float, *, trim_deg: float, list_deg: float
    ) -> float:
        """Add interpolated row corrections for exactly the qualified attitude.

        d_ref = d_gauge + c_trim(d_gauge) + c_list(d_gauge). This is an
        explicit input convention, not an inferred tank correction law.
        Linear interpolation: OpenStax Precalculus 2e (2021), §2.1.
        A missing component contributes zero. Do not also apply geometric
        gauge-to-centre correction. No interpolation between attitudes or
        corrected inverse is offered. Caller qualifies the table and clipping.
        """
        if self.correction_attitude_deg is None:
            raise ValueError("table has no qualified correction columns")
        attitude = (finite(trim_deg, "trim_deg"), finite(list_deg, "list_deg"))
        if attitude != self.correction_attitude_deg:
            raise ValueError("trim/list does not match the correction-table attitude")
        depth = finite(gauge_depth_m, "gauge_depth_m")
        # Interpolate even a zero column so raw queries remain row-bounded.
        corrected = depth
        for values in (self.trim_corrections_m, self.list_corrections_m):
            if values is not None:
                corrected += _interpolate(depth, self.depths, values)
        return bounded_depth(corrected, self.reference_height_m)

    def volume_at_gauge(
        self, gauge_depth_m: float, *, trim_deg: float, list_deg: float
    ) -> float:
        """Apply table corrections once, then bounded reference-volume lookup."""
        return self.volume_at(
            self.corrected_depth_at(gauge_depth_m, trim_deg=trim_deg, list_deg=list_deg)
        )

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
        cls,
        path: str | Path,
        *,
        reading_kind: str,
        reference_height_m: float,
        correction_attitude_deg: tuple[float, float] | None = None,
    ) -> CalibrationTable:
        """Read depth/volume and optional signed trim/list correction columns."""
        with Path(path).open(encoding="utf-8", newline="") as stream:
            reader = csv.DictReader(stream)
            if reader.fieldnames is None or len(set(reader.fieldnames)) != len(
                reader.fieldnames
            ):
                raise ValueError("CSV requires unique column names")
            points, corrections = _read_rows(list(reader))
        return cls(
            points,
            reading_kind,
            reference_height_m,
            correction_attitude_deg=correction_attitude_deg,
            **corrections,
        )

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
        required = {
            "reading_kind",
            "reference_height_m",
            "rows",
        }
        if (
            not isinstance(data, dict)
            or not required <= set(data)
            or set(data) - (required | {"correction_attitude_deg"})
        ):
            raise ValueError("YAML requires reading_kind, reference_height_m and rows")
        points, corrections = _read_rows(data["rows"])
        return cls(
            points,
            data["reading_kind"],
            data["reference_height_m"],
            correction_attitude_deg=data.get("correction_attitude_deg"),
            **corrections,
        )


def _read_rows(rows: list) -> tuple[tuple[tuple[float, float], ...], dict]:
    if not isinstance(rows, list):
        raise ValueError("rows must be a list of depth_m/volume_m3 mappings")
    points = []
    required = {"depth_m", "volume_m3"}
    optional = {"trim_correction_m", "list_correction_m"}
    columns = set(rows[0]) if rows and isinstance(rows[0], dict) else required
    if not required <= columns or columns - (required | optional):
        raise ValueError("unsupported calibration columns")
    corrections = {column: [] for column in columns & optional}
    for row in rows:
        if not isinstance(row, dict) or set(row) != columns:
            raise ValueError("each row must have the same calibration columns")
        points.append(
            (finite(row["depth_m"], "depth_m"), finite(row["volume_m3"], "volume_m3"))
        )
        for column, values in corrections.items():
            values.append(finite(row[column], column))
    return tuple(points), {
        column.replace("_correction_m", "_corrections_m"): tuple(values)
        for column, values in corrections.items()
    }
