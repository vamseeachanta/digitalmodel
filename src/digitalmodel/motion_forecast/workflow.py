"""Durable workflow entry point for motion_forecast (digitalmodel #1358).

Mirrors the repo convention (e.g. ``vessel_seakeeping.workflow``): ``router``
takes the config dict, does the work, writes results back under the module key,
and returns the (mutated) cfg. ``engine.py`` dispatches ``basename ==
"motion_forecast"`` here.

Config shape (under ``cfg['motion_forecast']``)::

    sea:   {hs, tp, gamma?, heading?, n_components?, horizon?, seed?}
    rao:   {source: "file"|"analytic",
            file?: <path>, format?: "aqwa"|"orcaflex",
            preset?: "generic_vessel"}      # analytic fallback
    asset: {location?: [x, y], dt?: <s>}
"""

from __future__ import annotations

from typing import Dict

import numpy as np

from .conventions import RAOSource
from .models import DOF_NAMES
from .rao_adapter import AnalyticRAO, GridRAO
from .reconstruct import reconstruct_motion
from .wave_source import synthesize_forecast


def generic_vessel_rao() -> AnalyticRAO:
    """A simple, documented analytic displacement RAO for smoke/demo use.

    Not a real vessel — a second-order heave/pitch/roll response with a
    short-wave roll-off, in the canonical lead convention. Amplitudes are
    m/m (translations) and deg/m (rotations).
    """
    g = 9.80665

    def _mech(omega: float, tn: float, zeta: float) -> complex:
        wn = 2.0 * np.pi / tn
        r = omega / wn
        return 1.0 / complex(1.0 - r * r, 2.0 * zeta * r)

    def _rolloff(omega: float, length_scale: float) -> float:
        k = omega * omega / g
        return float(np.exp(-((k * length_scale) ** 2)))

    def heave(omega, heading):
        return _mech(omega, 10.0, 0.2) * _rolloff(omega, 12.0)

    def pitch(omega, heading):
        k = omega * omega / g
        return k * _mech(omega, 12.0, 0.15) * _rolloff(omega, 15.0) * 40.0

    def roll(omega, heading):
        k = omega * omega / g
        return k * _mech(omega, 13.0, 0.06) * 60.0

    return AnalyticRAO({"heave": heave, "pitch": pitch, "roll": roll})


def _build_rao(rao_cfg: Dict):
    source = rao_cfg.get("source", "analytic")
    if source == "file":
        fmt = rao_cfg.get("format", "orcaflex").lower()
        src = RAOSource.AQWA if fmt == "aqwa" else RAOSource.ORCAFLEX
        return GridRAO.from_file(rao_cfg["file"], src)
    # analytic
    preset = rao_cfg.get("preset", "generic_vessel")
    if preset != "generic_vessel":
        raise ValueError(f"Unknown analytic RAO preset: {preset}")
    return generic_vessel_rao()


def _build_forecast(sea: Dict):
    """Build the incident-wave forecast per ``sea['forecaster']``.

    ``"synthetic"`` (default) — long-crested JONSWAP with a caller-set horizon
    (``wave_source``, unchanged / backward-compatible). ``"directional"`` — the
    #1357 short-crested forecaster with a physics predictable-zone horizon
    (``coherence_horizon``, or ``dpz_horizon`` when a measurement ``aperture`` is
    given).
    """
    kind = sea.get("forecaster", "synthetic")
    if kind == "synthetic":
        return synthesize_forecast(
            hs=float(sea.get("hs", 2.5)),
            tp=float(sea.get("tp", 9.0)),
            gamma=float(sea.get("gamma", 3.3)),
            heading=float(sea.get("heading", 0.0)),
            n_components=int(sea.get("n_components", 48)),
            horizon=float(sea.get("horizon", 90.0)),
            seed=int(sea.get("seed", 20260704)),
        )
    if kind == "directional":
        from .wave_forecast import synthesize_directional_forecast

        aperture = sea.get("aperture")
        return synthesize_directional_forecast(
            float(sea.get("hs", 2.5)), float(sea.get("tp", 9.0)),
            gamma=float(sea.get("gamma", 3.3)),
            mean_heading=float(sea.get("mean_heading", sea.get("heading", 0.0))),
            spread_s=float(sea.get("spread_s", 10.0)),
            n_freq=int(sea.get("n_freq", sea.get("n_components", 32))),
            n_dir=int(sea.get("n_dir", 7)),
            aperture=(float(aperture) if aperture is not None else None),
            seed=int(sea.get("seed", 20260704)),
        )
    raise ValueError(f"unknown forecaster {kind!r}; use 'synthetic' or 'directional'")


def router(cfg: Dict) -> Dict:
    """Run a motion forecast and write results back into ``cfg``."""
    mf = cfg.setdefault("motion_forecast", {})
    sea = mf.get("sea", {})
    rao_cfg = mf.get("rao", {})
    asset = mf.get("asset", {})

    forecast = _build_forecast(sea)
    rao = _build_rao(rao_cfg)
    location = tuple(asset.get("location", (0.0, 0.0)))
    motion = reconstruct_motion(
        forecast, rao, asset_location=location, dt=float(asset.get("dt", 0.2))
    )

    mf["results"] = {
        "t": motion.t.tolist(),
        "dof": {d: motion.dof[d].tolist() for d in DOF_NAMES},
        "significant": {d: motion.significant(d) for d in DOF_NAMES},
        "horizon": motion.horizon,
        "n_components": len(forecast.components),
    }

    # Criteria loaded once when a named operation is set; reused by the forecast
    # decision (#1359) and the measured status (#1367).
    op = mf.get("operation")
    crits = None
    if op:
        from .criteria import load_criteria

        crits = load_criteria(mf.get("criteria_path"))
        if op not in crits:
            raise ValueError(f"unknown operation {op!r}; have {sorted(crits)}")

    # Optional rolling go/no-go decision for a named operation (#1359).
    if op:
        from .decision import rolling_decision

        dec = rolling_decision(motion, crits[op])
        mf["decision"] = {
            "operation": dec.operation,
            "governing": dec.governing,
            "unit": dec.unit,
            "state": dec.state.value,
            "display": dec.display,
            "current_value": dec.current_value,
            "caution": dec.caution,
            "limit": dec.limit,
            "lead_time_to_caution": dec.lead_time_to_caution,
            "lead_time_to_no_go": dec.lead_time_to_no_go,
        }

    # Optional measured-motion mode: ingest an MMS/MRU feed (#1367).
    meas_cfg = mf.get("measured")
    if meas_cfg:
        from .measured import MeasuredMotion
        from .measured_source import from_csv
        from .reconcile import measured_status, seam_offset

        if isinstance(meas_cfg, MeasuredMotion):
            measured = meas_cfg
        elif isinstance(meas_cfg, dict) and meas_cfg.get("csv"):
            measured = from_csv(meas_cfg["csv"])
        else:
            raise ValueError(
                "motion_forecast.measured must be a MeasuredMotion or {'csv': <path>}"
            )
        if op:
            md = measured_status(measured, crits[op])
            mf["measured_status"] = {
                "operation": md.operation, "governing": md.governing,
                "unit": md.unit, "state": md.state.value, "display": md.display,
                "current_value": md.current_value,
                "caution": md.caution, "limit": md.limit,
            }
        # Reconcile only against the just-built forecast when "now" is inside it
        # (never against an unrelated default sea).
        if float(motion.t[0]) <= measured.now <= float(motion.t[-1]):
            mf["reconciliation"] = {"seam_offset": seam_offset(measured, motion)}

    # Optional forecast-skill aggregation over a batch of records (#1360).
    skill_cfg = mf.get("skill")
    if skill_cfg and skill_cfg.get("records"):
        from .skill import SkillRecord, aggregate_skill

        recs = [r if isinstance(r, SkillRecord) else SkillRecord(*r)
                for r in skill_cfg["records"]]
        agg = aggregate_skill(recs)
        mf["skill_summary"] = {
            d: {"rmse": a.rmse, "bias": a.bias, "n_samples": a.n_samples,
                "correlation_median": a.correlation_median}
            for d, a in agg.items()
        }
    return cfg
