"""ASCII accelerogram readers for response-spectrum workflows."""

from __future__ import annotations

from dataclasses import dataclass
from hashlib import sha256
from pathlib import Path
import re
from typing import Iterable

import numpy as np

G0_M_S2 = 9.80665


@dataclass(frozen=True)
class AccelerogramRecord:
    """A single-component accelerogram with SI acceleration values."""

    time_s: np.ndarray
    acceleration_m_s2: np.ndarray
    units: str
    source_path: Path
    source_digest: str

    @property
    def npts(self) -> int:
        return int(self.time_s.size)

    @property
    def dt_s(self) -> float:
        if self.npts < 2:
            raise ValueError("AccelerogramRecord requires at least two samples")
        return float(self.time_s[1] - self.time_s[0])

    @property
    def pga_m_s2(self) -> float:
        return float(np.max(np.abs(self.acceleration_m_s2)))

    @property
    def pga_g(self) -> float:
        return self.pga_m_s2 / G0_M_S2

    @property
    def acceleration_g(self) -> np.ndarray:
        return self.acceleration_m_s2 / G0_M_S2


def read_ascii_accelerogram(
    path: str | Path,
    *,
    units: str | None = None,
    dt_s: float | None = None,
) -> AccelerogramRecord:
    """Read two-column or dt-header single-column ASCII accelerogram data."""

    source_path = Path(path)
    raw = source_path.read_bytes()
    metadata, rows = _parse_rows(raw.decode("utf-8"))
    resolved_units = _normalise_units(units or metadata.get("units") or "m_s2")
    if dt_s is None and "dt_s" in metadata:
        dt_s = float(metadata["dt_s"])
    time_s, acceleration = _time_and_acceleration(rows, dt_s)
    _validate_uniform_time_step(time_s)
    acceleration_m_s2 = _to_si(acceleration, resolved_units)
    return AccelerogramRecord(
        time_s=time_s,
        acceleration_m_s2=acceleration_m_s2,
        units=resolved_units,
        source_path=source_path,
        source_digest=sha256(raw).hexdigest(),
    )


def _parse_rows(text: str) -> tuple[dict[str, str], list[list[float]]]:
    metadata: dict[str, str] = {}
    rows: list[list[float]] = []
    for line in text.splitlines():
        stripped = line.strip()
        if not stripped:
            continue
        if stripped.startswith("#"):
            metadata.update(_metadata_from_comment(stripped))
            continue
        rows.append([float(part) for part in stripped.split()])
    if not rows:
        raise ValueError("ASCII accelerogram contains no numeric samples")
    return metadata, rows


def _metadata_from_comment(line: str) -> dict[str, str]:
    match = re.match(r"#\s*([A-Za-z0-9_./-]+)\s*:\s*(.+?)\s*$", line)
    if not match:
        return {}
    key, value = match.groups()
    key = key.strip().lower().replace("-", "_")
    if key in {"dt", "delta_t", "time_step_s"}:
        key = "dt_s"
    return {key: value.strip()}


def _time_and_acceleration(
    rows: list[list[float]],
    dt_s: float | None,
) -> tuple[np.ndarray, np.ndarray]:
    widths = {len(row) for row in rows}
    if widths == {1}:
        if dt_s is None:
            raise ValueError("single-column accelerogram requires dt_s")
        acceleration = np.array([row[0] for row in rows], dtype=float)
        time_s = np.arange(acceleration.size, dtype=float) * float(dt_s)
        return time_s, acceleration
    if widths == {2}:
        time_s = np.array([row[0] for row in rows], dtype=float)
        acceleration = np.array([row[1] for row in rows], dtype=float)
        return time_s, acceleration
    raise ValueError("ASCII accelerogram must be one-column or two-column data")


def _validate_uniform_time_step(time_s: np.ndarray) -> None:
    if time_s.size < 2:
        raise ValueError("accelerogram requires at least two time samples")
    dt = np.diff(time_s)
    if np.any(dt <= 0.0) or not np.allclose(dt, dt[0], rtol=1.0e-6, atol=1.0e-12):
        raise ValueError("accelerogram must use a uniform time step")


def _normalise_units(units: str) -> str:
    value = str(units).strip().lower().replace("/", "_").replace("-", "_")
    aliases = {
        "g": "g",
        "cm_s2": "cm_s2",
        "cm_s_s": "cm_s2",
        "cm_per_s2": "cm_s2",
        "m_s2": "m_s2",
        "m_s_s": "m_s2",
        "m_per_s2": "m_s2",
    }
    if value not in aliases:
        raise ValueError("units must be one of g, cm_s2, or m_s2")
    return aliases[value]


def _to_si(values: Iterable[float], units: str) -> np.ndarray:
    array = np.asarray(list(values), dtype=float)
    if units == "g":
        return array * G0_M_S2
    if units == "cm_s2":
        return array / 100.0
    return array
