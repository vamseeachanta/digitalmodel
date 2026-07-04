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


def router(cfg: Dict) -> Dict:
    """Run a motion forecast and write results back into ``cfg``."""
    mf = cfg.setdefault("motion_forecast", {})
    sea = mf.get("sea", {})
    rao_cfg = mf.get("rao", {})
    asset = mf.get("asset", {})

    forecast = synthesize_forecast(
        hs=float(sea.get("hs", 2.5)),
        tp=float(sea.get("tp", 9.0)),
        gamma=float(sea.get("gamma", 3.3)),
        heading=float(sea.get("heading", 0.0)),
        n_components=int(sea.get("n_components", 48)),
        horizon=float(sea.get("horizon", 90.0)),
        seed=int(sea.get("seed", 20260704)),
    )
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
    return cfg
