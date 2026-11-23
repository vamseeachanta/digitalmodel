"""Telemetry inputs for the drilling-riser digital twin (twin A #1373, epic #1372).

Turns a DP-drilling-vessel telemetry record stream (a live vessel-monitoring feed
or a committed offline fixture) into the inputs the merged operating-envelope engine
(#1279) consumes — mirroring the metocean-input pattern (#1282,
:mod:`digitalmodel.drilling_riser.metocean_inputs`). This is a PURE reducer: no live
client is committed (a vessel feed is session/vessel-specific — there is no public
endpoint), and any future live iterator's network errors are caught by the shared
``tests/metocean/netskip`` guard at the caller.

Of the telemetry channels the twin ingests, the analytical envelope can honestly
consume TWO today:

  * ``snapshot_to_offset_pct`` maps measured vessel offset onto the sweep's
    ``offset_pct`` — the exact inverse of the merged
    :func:`digitalmodel.drilling_riser.operability.watch_circle_radius_m`
    (``radius = offset_pct/100 * water_depth`` ⟹ ``offset_pct = 100*offset/wd``).
  * ``snapshot_to_tension_n`` maps measured top tension onto the envelope's
    ``tension_n`` input (an engine input, not a predicted output).

The other two channels are ingested and typed but deliberately NOT consumed here,
because doing so would fabricate capability the later children own:

  * measured flex-joint angles (:class:`MeasuredResponse`) are *predicted outputs* of
    the physics; the residual (measured − predicted) is twin B's ML seam (#1374);
  * DP thruster / station-keeping state (:class:`DPState`) drives drift-off, which is
    twin C's solver tier (#1375). The static envelope has no thruster term.

SI internally (newtons, metres, degrees). A fixture may report kilonewtons; the
conversion happens once, at the parse boundary. Public-provider / synthetic data
only; no client telemetry, no vessel identity, no credentials are read here.
"""
from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime
from typing import Iterable, Mapping, Optional, Sequence

from digitalmodel.drilling_riser.operability import watch_circle_radius_m

_KN_TO_N = 1000.0


@dataclass(frozen=True)
class DPState:
    """DP thruster / station-keeping state (parked for twin C — drift-off)."""

    thrusters_online: Optional[int] = None
    total_available_thrust_n: Optional[float] = None
    total_commanded_thrust_n: Optional[float] = None

    @property
    def station_keeping_margin(self) -> Optional[float]:
        """``1 - commanded/available`` in [.., 1]; ``None`` when no capacity.

        Guarded: returns ``None`` when available thrust is missing or <= 0 (e.g.
        all thrusters offline) rather than dividing by zero. Not consumed by the
        static envelope; twin C (#1375) uses it.
        """
        avail = self.total_available_thrust_n
        cmd = self.total_commanded_thrust_n
        if avail is None or cmd is None or avail <= 0:
            return None
        return 1.0 - cmd / avail


@dataclass(frozen=True)
class MeasuredResponse:
    """Measured riser response (parked for twin B — ML residual-correction).

    Flex-joint angle is a *predicted output* of the physics; the measured value
    lives here so twin B (#1374) can form the measured − predicted residual. Twin A
    ingests but does not consume it.
    """

    flexjoint_angle_upper_deg: Optional[float] = None
    flexjoint_angle_lower_deg: Optional[float] = None


@dataclass(frozen=True)
class TelemetrySnapshot:
    """One atomic telemetry sample.

    ``timestamp`` is a :class:`datetime` (mirrors the marine-ops MRUReading/buffer
    idiom so a rolling window is well-defined for the twin-D monitoring surface).
    ``vessel_offset_m`` (radial excursion, >= 0) and ``top_tension_n`` are the two
    channels the envelope consumes; heading / ``dp`` / ``measured`` are ingested but
    not consumed by the analytical tier.
    """

    vessel_offset_m: float
    timestamp: Optional[datetime] = None
    vessel_heading_deg: Optional[float] = None
    top_tension_n: Optional[float] = None
    dp: Optional[DPState] = None
    measured: Optional[MeasuredResponse] = None


def _parse_timestamp(value: object) -> Optional[datetime]:
    if isinstance(value, datetime):
        return value
    if isinstance(value, str) and value:
        return datetime.fromisoformat(value)
    return None


def _kn_to_n(value: object) -> Optional[float]:
    return None if value is None else float(value) * _KN_TO_N


def _dp_from(rec: Mapping[str, object]) -> Optional[DPState]:
    online = rec.get("thrusters_online")
    avail = rec.get("total_available_thrust_kn")
    cmd = rec.get("total_commanded_thrust_kn")
    if online is None and avail is None and cmd is None:
        return None
    return DPState(
        thrusters_online=None if online is None else int(online),
        total_available_thrust_n=_kn_to_n(avail),
        total_commanded_thrust_n=_kn_to_n(cmd),
    )


def _measured_from(rec: Mapping[str, object]) -> Optional[MeasuredResponse]:
    upper = rec.get("flexjoint_angle_upper_deg")
    lower = rec.get("flexjoint_angle_lower_deg")
    if upper is None and lower is None:
        return None
    return MeasuredResponse(
        flexjoint_angle_upper_deg=None if upper is None else float(upper),
        flexjoint_angle_lower_deg=None if lower is None else float(lower),
    )


def parse_snapshots(records: Iterable[Mapping[str, object]]) -> list[TelemetrySnapshot]:
    """Type a raw telemetry record stream into :class:`TelemetrySnapshot` objects.

    Records missing ``vessel_offset_m`` (the one always-consumed channel) are
    skipped; heading / tension / dp / measured are optional. ``timestamp`` is parsed
    to :class:`datetime`. ``*_kn`` fields are converted to newtons at this boundary.
    """
    snapshots: list[TelemetrySnapshot] = []
    for rec in records:
        offset = rec.get("vessel_offset_m")
        if offset is None:
            continue
        heading = rec.get("vessel_heading_deg")
        snapshots.append(
            TelemetrySnapshot(
                vessel_offset_m=float(offset),
                timestamp=_parse_timestamp(rec.get("timestamp")),
                vessel_heading_deg=None if heading is None else float(heading),
                top_tension_n=_kn_to_n(rec.get("top_tension_kn")),
                dp=_dp_from(rec),
                measured=_measured_from(rec),
            )
        )
    return snapshots


def snapshot_to_offset_pct(
    snapshot: TelemetrySnapshot, *, water_depth_m: float
) -> float:
    """Measured vessel offset → envelope ``offset_pct`` (% of water depth).

    Exact inverse of :func:`watch_circle_radius_m`. Raises ``ValueError`` on a
    non-positive water depth or a negative offset (radial excursion is >= 0).
    """
    if water_depth_m <= 0:
        raise ValueError(f"water_depth_m must be > 0, got {water_depth_m}")
    if snapshot.vessel_offset_m < 0:
        raise ValueError(
            f"vessel_offset_m must be >= 0 (radial excursion), got {snapshot.vessel_offset_m}"
        )
    return 100.0 * snapshot.vessel_offset_m / water_depth_m


def snapshot_to_tension_n(snapshot: TelemetrySnapshot) -> Optional[float]:
    """Measured top tension [N] for the envelope's ``tension_n`` input.

    ``None`` when no measured tension is present (caller falls back to the assembled
    riser's design top tension).
    """
    return snapshot.top_tension_n


def snapshots_to_offset_track(
    records: Iterable[TelemetrySnapshot], *, water_depth_m: float
) -> list[float]:
    """``offset_pct`` time-series over a telemetry window (twin-D monitoring)."""
    return [snapshot_to_offset_pct(s, water_depth_m=water_depth_m) for s in records]
