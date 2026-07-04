"""Atlas artifact: load/save + per-physics interpolation with hard safety rails.

Implements Decisions 2 and 4 of docs/plans/parametric-refresh/atlas-spec.md:
the on-disk format and the interpolation taxonomy
(``linear`` / ``log_log`` / ``utilization_threshold``) plus the out-of-range
contract (never clamp, never extrapolate).
"""

from __future__ import annotations

import json
import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd
import yaml
from scipy.interpolate import RegularGridInterpolator

PHYSICS_CLASSES = {"linear", "log_log", "utilization_threshold"}


# Registry of pure derived-axis transforms (raw input fields -> the axis value).
# Keeps the axis a single physically-meaningful coordinate while the query stays
# in the user's raw terms (e.g. collapse tension+area -> stress). No eval().
DERIVED_TRANSFORMS: dict[str, Any] = {
    # stress range (MPa) from a tension range (kN) over a cross-section (mm^2)
    "stress_from_tension_area": lambda tension_range_kN, area_mm2: (
        float(tension_range_kN) * 1000.0 / float(area_mm2)
    ),
}


def _interval_index(grid: list[float], val: float) -> int:
    """Index i of the grid interval [grid[i], grid[i+1]] containing val,
    clamped to [0, len(grid)-2]."""
    import bisect

    i = bisect.bisect_right(grid, val) - 1
    return max(0, min(i, len(grid) - 2))


@dataclass


@dataclass
class Axis:
    """One atlas dimension. Continuous axes carry ``grid`` + ``scale``;
    categorical axes carry ``values`` and are sliced, never interpolated.
    A continuous axis may be ``derived``: its value is computed from raw input
    fields via a registered transform, so the grid is in the derived coordinate
    while the query supplies the raw inputs."""

    name: str
    scale: str = "linear"  # linear | log  (ignored for categorical axes)
    grid: list[float] | None = None
    values: list[Any] | None = None
    derived: dict[str, Any] | None = None  # {"fn": <name>, "inputs": [<field>, ...]}

    @property
    def is_categorical(self) -> bool:
        return self.values is not None

    def derived_value(self, point: dict[str, Any]) -> float:
        """Compute this derived axis's value from the point's raw inputs."""
        fn = DERIVED_TRANSFORMS[self.derived["fn"]]
        return float(fn(*[point[k] for k in self.derived["inputs"]]))

    def to_dict(self) -> dict[str, Any]:
        if self.is_categorical:
            return {"name": self.name, "values": list(self.values)}
        out = {"name": self.name, "scale": self.scale, "grid": list(self.grid)}
        if self.derived is not None:
            out["derived"] = self.derived
        return out

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "Axis":
        if data.get("values") is not None:
            return cls(name=data["name"], values=list(data["values"]))
        return cls(
            name=data["name"],
            scale=data.get("scale", "linear"),
            grid=[float(v) for v in data["grid"]],
            derived=data.get("derived"),
        )


@dataclass
class Prediction:
    """Result of a single atlas lookup. ``in_range`` is the hard gate: when
    False the caller must escalate, never use ``value``."""

    value: float
    in_range: bool
    reason: str = ""


@dataclass
class Atlas:
    basename: str
    atlas_id: str
    physics: str
    response: str
    axes: list[Axis]
    grid: pd.DataFrame
    validation: dict[str, Any] = field(default_factory=dict)
    provenance: dict[str, Any] = field(default_factory=dict)

    # -- axis helpers ----------------------------------------------------
    @property
    def continuous_axes(self) -> list[Axis]:
        return [ax for ax in self.axes if not ax.is_categorical]

    @property
    def categorical_axis(self) -> Axis | None:
        cats = [ax for ax in self.axes if ax.is_categorical]
        if len(cats) > 1:
            raise ValueError("atlas supports at most one categorical axis (pilot)")
        return cats[0] if cats else None

    @property
    def max_rel_error(self) -> float:
        return float(self.validation.get("max_rel_error", 0.0))

    # -- transforms ------------------------------------------------------
    def _axis_coords(self, axis: Axis) -> np.ndarray:
        knots = np.asarray(axis.grid, dtype=float)
        return np.log10(knots) if axis.scale == "log" else knots

    def _transform_response(self, values: np.ndarray) -> np.ndarray:
        return np.log10(values) if self.physics == "log_log" else values

    def _untransform_response(self, value: float) -> float:
        return float(10.0**value) if self.physics == "log_log" else float(value)

    # -- interpolation ---------------------------------------------------
    def _interpolator(self, cat_value: Any) -> RegularGridInterpolator:
        cont = self.continuous_axes
        frame = self.grid
        cat = self.categorical_axis
        if cat is not None:
            frame = frame[frame[cat.name] == cat_value]
        if frame.empty:
            raise ValueError(f"no atlas rows for categorical value {cat_value!r}")
        coords = [self._axis_coords(ax) for ax in cont]
        shape = tuple(len(c) for c in coords)
        ordered = frame.sort_values([ax.name for ax in cont])
        resp = self._transform_response(
            ordered[self.response].to_numpy(dtype=float)
        ).reshape(shape)
        return RegularGridInterpolator(
            coords, resp, method="linear", bounds_error=False, fill_value=np.nan
        )

    def predict(self, point: dict[str, Any]) -> Prediction:
        """Interpolate one point. Out-of-range (any continuous axis outside its
        grid box, or an unknown categorical value) -> in_range=False, no value
        is extrapolated."""
        cat = self.categorical_axis
        cat_value = None
        if cat is not None:
            cat_value = point.get(cat.name)
            if cat_value not in (cat.values or []):
                return Prediction(
                    math.nan, False, f"{cat.name}={cat_value!r} not in atlas"
                )

        query = []
        for ax in self.continuous_axes:
            # direct value if supplied (generation/holdout); else derive it from
            # the point's raw inputs (query path for a derived axis).
            if ax.name in point:
                raw = float(point[ax.name])
            elif ax.derived is not None:
                try:
                    raw = ax.derived_value(point)
                except KeyError as missing:
                    return Prediction(
                        math.nan, False,
                        f"missing input {missing.args[0]} for derived axis {ax.name}")
            else:
                return Prediction(math.nan, False, f"missing axis {ax.name}")
            lo, hi = min(ax.grid), max(ax.grid)
            if raw < lo or raw > hi:
                return Prediction(
                    math.nan,
                    False,
                    f"{ax.name}={raw:g} outside [{lo:g}, {hi:g}]",
                )
            query.append(math.log10(raw) if ax.scale == "log" else raw)

        interp = self._interpolator(cat_value)
        raw_value = float(interp(np.array([query]))[0])
        if math.isnan(raw_value):
            return Prediction(math.nan, False, "interpolation returned NaN")
        return Prediction(self._untransform_response(raw_value), True)

    def local_error(self, point: dict[str, Any]) -> float:
        """Per-point error estimate (#828): the held-out error of the grid
        interval the query falls into, taken over the worst continuous axis.
        Tighter than the global max_rel_error in smooth regions, wider near a
        feature. Falls back to the global error if no local map is present (an
        older atlas) or on any miss — so it is always at least as honest."""
        slice_map = (self.validation.get("local_error_map") or {})
        if not slice_map:
            return self.max_rel_error
        cat = self.categorical_axis
        cat_key = str(point.get(cat.name)) if cat is not None else ""
        per_axis = slice_map.get(cat_key)
        if per_axis is None:
            return self.max_rel_error
        errs = []
        for ax in self.continuous_axes:
            if ax.name in point:
                val = float(point[ax.name])
            elif ax.derived is not None:
                try:
                    val = ax.derived_value(point)
                except Exception:
                    return self.max_rel_error
            else:
                return self.max_rel_error
            intervals = per_axis.get(ax.name)
            if not intervals:
                return self.max_rel_error
            idx = _interval_index([float(k) for k in ax.grid], val)
            errs.append(intervals[min(idx, len(intervals) - 1)])
        return max(errs) if errs else self.max_rel_error

    # -- persistence -----------------------------------------------------
    def surrogate_spec(self) -> dict[str, Any]:
        return {
            "basename": self.basename,
            "atlas_id": self.atlas_id,
            "physics": self.physics,
            "response": self.response,
            "axes": [ax.to_dict() for ax in self.axes],
        }

    def save(self, root: Path) -> Path:
        out = Path(root) / self.basename / self.atlas_id
        out.mkdir(parents=True, exist_ok=True)
        self.grid.to_parquet(out / "grid.parquet", index=False)
        (out / "surrogate.json").write_text(
            json.dumps(self.surrogate_spec(), indent=2)
        )
        (out / "validation.json").write_text(json.dumps(self.validation, indent=2))
        manifest = {
            "atlas_id": self.atlas_id,
            "basename": self.basename,
            "physics": self.physics,
            "response": self.response,
            "axes": [ax.to_dict() for ax in self.axes],
            "validation": {
                "metric": self.validation.get("metric"),
                "max_rel_error": self.max_rel_error,
                "threshold": self.validation.get("threshold"),
                "passes": self.validation.get("passes"),
            },
            "provenance": self.provenance,
        }
        (out / "manifest.yaml").write_text(yaml.safe_dump(manifest, sort_keys=False))
        # default pointer so a query can resolve the live atlas by basename
        (Path(root) / self.basename / "default.txt").write_text(self.atlas_id)
        return out

    @classmethod
    def load(cls, root: Path, basename: str, atlas_id: str | None = None) -> "Atlas":
        base = Path(root) / basename
        if atlas_id is None:
            atlas_id = (base / "default.txt").read_text().strip()
        out = base / atlas_id
        spec = json.loads((out / "surrogate.json").read_text())
        manifest = yaml.safe_load((out / "manifest.yaml").read_text())
        validation = json.loads((out / "validation.json").read_text())
        return cls(
            basename=spec["basename"],
            atlas_id=spec["atlas_id"],
            physics=spec["physics"],
            response=spec["response"],
            axes=[Axis.from_dict(a) for a in spec["axes"]],
            grid=pd.read_parquet(out / "grid.parquet"),
            validation=validation,
            provenance=manifest.get("provenance", {}),
        )
