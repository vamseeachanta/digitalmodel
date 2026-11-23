"""Drilling-riser DP drift-off screen (twin C #1375, epic #1372).

A CONSERVATIVE quasi-static screen of the vessel's drift-off after thruster /
station-keeping loss: the net environmental force drives the vessel off station
until it reaches the flex-joint-limited watch circle. Reports the **time-to-limit**
and the **point-of-disconnect** (the EDS — emergency-disconnect-sequence — window).

Design (per the #1375 T2 review — conservative in the failure regime):
  * ONE :class:`EnvironmentalConditions` drives BOTH the watch-circle limit
    (``watch_circle_limit`` scans :func:`compute_operating_envelope`) AND the
    environmental force (reusing the public Newman/flat-plate/wind model in
    :mod:`digitalmodel.subsea.mooring_analysis.designer`) — a higher current
    shrinks the circle AND raises the force.
  * SI throughout: the environmental model returns kN; converted to newtons at the
    boundary (``* 1000``) to combine with :class:`DPState` thrust (N).
  * The drift is budgeted from the ACTUAL measured offset ``x0`` (the vessel is
    already off-station when thrusters fail) using the vessel's velocity at EDS
    initiation, NOT the full watch circle from rest.
  * Thrust credit is conservative: :class:`DPState` is scalar (no direction), so
    surviving thrust is credited only through a low ``thrust_efficiency`` (default
    0) and reported separately as a margin — never full opposition.
  * The effective mass uses the LOW end of its band (time-to-limit ``~ sqrt(m)``).
  * Every result self-marks ``tier="quasi_static_screening"`` + a disclaimer; the
    full time-domain (added mass, recoil, thruster ramp-down) is the deferred
    solver tier (a ``solver.licensed=False`` #1346-style stub).

Limits stay the cited getters (:func:`resolve_envelope_criteria`); this module
constructs NO ``Citation`` / ``CitedValue`` — ``m_eff`` / ``t_eds`` /
``thrust_efficiency`` / the environmental coefficients are plain config values,
never a standard.
"""
from __future__ import annotations

import math
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

import numpy as np
import yaml

from digitalmodel.drilling_riser.envelope import (
    EnvelopeCriteria,
    OperatingMode,
    RiserSection,
    SeaState,
    compute_operating_envelope,
)
from digitalmodel.drilling_riser.operability import watch_circle_radius_m
from digitalmodel.drilling_riser.telemetry_inputs import DPState
from digitalmodel.subsea.mooring_analysis.designer import MooringDesigner
from digitalmodel.subsea.mooring_analysis.models import (
    EnvironmentalConditions,
    MooringSystem,
    MooringType,
    VesselParticulars,
)

CONFIG_PATH = Path(__file__).with_name("drift_off_config.yml")

SCREENING_DISCLAIMER = (
    "Quasi-static drift-off screen: a constant-acceleration LOWER BOUND on "
    "time-to-limit (ignores hull drag/damping, riser lateral restoring and the "
    "thruster ramp-down — all conservative). NOT a time-domain dynamic result; "
    "added mass, recoil and coupled vessel-riser dynamics are the deferred "
    "solver tier (a licensed OrcaFlex run)."
)

#: status values
STATION_HELD = "station_held"
DRIFT_OFF = "drift_off"
ESCALATE = "escalate"


def load_config(path: Path = CONFIG_PATH) -> dict:
    return yaml.safe_load(Path(path).read_text())


@dataclass(frozen=True)
class DriftOffResult:
    """A drift-off screen result. ``time_to_limit_s`` is ``inf`` when station is
    held. ``status``: ``station_held`` | ``drift_off`` | ``escalate`` (escalate =
    already at/over the limit, or the EDS window is non-positive)."""

    r_watch_m: float
    governing_limit: Optional[str]
    f_env_n: float
    surviving_thrust_n: float
    f_net_n: float
    time_to_limit_s: float
    point_of_disconnect_m: float
    lead_time_margin_s: float
    status: str
    provenance: dict


def environmental_force_n(
    condition: EnvironmentalConditions, config: dict
) -> tuple[float, dict]:
    """Total environmental force on the vessel [N], reusing the public
    Newman/flat-plate/wind model in ``mooring_analysis.designer`` (kN → N).

    Returns ``(f_env_n, components_kn)``. Raises on a non-positive force (a units
    slip must never silently read as 'station held')."""
    v = config["vessel"]
    vessel = VesselParticulars(
        vessel_type="generic",
        length=float(v["length_m"]),
        beam=float(v["beam_m"]),
        draft=float(v["draft_m"]),
        displacement=float(v["displacement_t"]),
        windage_area=float(v["windage_area_m2"]),
    )
    designer = MooringDesigner(
        MooringSystem(
            system_type=MooringType.SPREAD,
            water_depth=float(config.get("water_depth_ref_m", 1500.0)),
            lines=[],
            vessel=vessel,
        )
    )
    loads = designer.calculate_environmental_loads(condition, vessel)
    f_env_n = float(loads.total_force) * 1000.0  # kN -> N (SI boundary)
    if not (f_env_n > 0):
        raise ValueError(f"environmental force must be > 0 N, got {f_env_n}")
    components_kn = {
        "wave_drift_kn": float(loads.wave_drift_force),
        "current_kn": float(loads.current_force),
        "wind_kn": float(loads.wind_force),
    }
    return f_env_n, components_kn


def watch_circle_limit(
    condition: EnvironmentalConditions,
    *,
    section: RiserSection,
    water_depth_m: float,
    length_m: float,
    tension_n: float,
    criteria: EnvelopeCriteria,
    mode: OperatingMode = OperatingMode.DRILLING,
    offsets_pct=None,
) -> tuple[float, Optional[str]]:
    """The largest allowable vessel offset (governing UC <= 1) at THIS condition,
    as a watch-circle radius [m], plus the governing limit name.

    The current in ``condition`` drives both this limit (via the flex-joint angle
    the offset produces) AND the drift force in ``drift_off_screen`` — one design
    condition. The angle limits come fail-closed from the cited getters through
    ``criteria`` (caller-supplied; this module cites nothing)."""
    if offsets_pct is None:
        offsets_pct = [round(x, 4) for x in np.arange(0.0, 20.0001, 0.25)]
    seastate = SeaState(hs_m=condition.wave_hs, tp_s=condition.wave_tp)
    result = compute_operating_envelope(
        section=section,
        water_depth_m=water_depth_m,
        length_m=length_m,
        tension_n=tension_n,
        criteria=criteria,
        offsets_pct=offsets_pct,
        current_speeds_mps=[condition.current_speed],
        seastates=[seastate],
        mode=mode,
    )
    mask = result.allowable_mask[:, 0, 0]
    allowable_idx = np.nonzero(mask)[0]
    if allowable_idx.size == 0:
        # even zero offset is over the limit at this condition
        gov = str(result.governing_limit[0, 0, 0])
        return 0.0, gov
    last = int(allowable_idx.max())
    r_watch_m = watch_circle_radius_m(float(offsets_pct[last]), water_depth_m)
    # the limit that first bites just outside the allowable region (or the last
    # governing limit if the whole sweep is allowable)
    gov_idx = min(last + 1, len(offsets_pct) - 1)
    governing = str(result.governing_limit[gov_idx, 0, 0])
    return r_watch_m, governing


def drift_off_screen(
    dp_state: DPState,
    condition: EnvironmentalConditions,
    *,
    section: RiserSection,
    water_depth_m: float,
    length_m: float,
    tension_n: float,
    criteria: EnvelopeCriteria,
    config: Optional[dict] = None,
    x0_m: float = 0.0,
    v0_mps: float = 0.0,
    t_now_s: float = 0.0,
    mode: OperatingMode = OperatingMode.DRILLING,
) -> DriftOffResult:
    """Conservative quasi-static drift-off screen. See the module docstring."""
    config = config or load_config()
    m_eff_low = float(config["m_eff_low_kg"])
    m_eff_high = float(config["m_eff_high_kg"])
    t_eds = float(config["t_eds_s"])
    eta = float(config["thrust_efficiency"])

    r_watch_m, governing = watch_circle_limit(
        condition, section=section, water_depth_m=water_depth_m,
        length_m=length_m, tension_n=tension_n, criteria=criteria, mode=mode,
    )
    f_env_n, components_kn = environmental_force_n(condition, config)
    surviving_thrust_n = float(dp_state.total_available_thrust_n or 0.0)
    f_net_n = f_env_n - eta * surviving_thrust_n

    provenance = {
        "tier": "quasi_static_screening",
        "dynamic": False,
        "m_eff_band_kg": [m_eff_low, m_eff_high],
        "m_eff_used_kg": m_eff_low,
        "thrust_efficiency": eta,
        "governing_limit": governing,
        "environmental_force_components_kn": components_kn,
        "standards": [
            {"id": "API-RP-16Q", "note": "flex-joint angle limit (cited getter)"},
            {"id": "API-STD-2RD", "note": "von Mises design factor (cited getter)"},
        ],
        "disclaimer": SCREENING_DISCLAIMER,
    }

    def _result(status, time_to_limit_s, point_of_disconnect_m, lead_time_margin_s):
        return DriftOffResult(
            r_watch_m=r_watch_m, governing_limit=governing, f_env_n=f_env_n,
            surviving_thrust_n=surviving_thrust_n, f_net_n=f_net_n,
            time_to_limit_s=time_to_limit_s,
            point_of_disconnect_m=point_of_disconnect_m,
            lead_time_margin_s=lead_time_margin_s, status=status,
            provenance=provenance,
        )

    # station held: net drift force does not push the vessel off station
    if f_net_n <= 0.0:
        return _result(STATION_HELD, math.inf, r_watch_m, math.inf)

    remaining = r_watch_m - x0_m
    if remaining <= 0.0:
        # already at/over the limit — must disconnect now
        return _result(ESCALATE, 0.0, x0_m, -t_eds - t_now_s)

    a = f_net_n / m_eff_low  # LOW m_eff -> higher accel -> shorter time (conservative)
    # remaining = v0*t + 1/2 a t^2  ->  t_limit
    t_limit = (-v0_mps + math.sqrt(v0_mps**2 + 2.0 * a * remaining)) / a

    t_disc = t_limit - t_eds
    lead_time_margin_s = t_limit - t_eds - t_now_s
    if t_disc <= 0.0:
        # cannot complete an EDS before the limit — escalate (disconnect now)
        return _result(ESCALATE, t_limit, x0_m, lead_time_margin_s)

    # velocity at EDS initiation is v0 + a*t_disc (nonzero) -> disconnect offset
    point_of_disconnect_m = x0_m + v0_mps * t_disc + 0.5 * a * t_disc**2
    return _result(DRIFT_OFF, t_limit, point_of_disconnect_m, lead_time_margin_s)
