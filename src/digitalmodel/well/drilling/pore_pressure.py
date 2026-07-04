# ABOUTME: PorePressure — multi-method pore-pressure prediction with uncertainty bands + safe drilling window
# ABOUTME: References: Eaton (1975) SPE 5544; Bowers (1995) SPE 27488; Mouchet & Mitchell (1989).

"""
PorePressure
============

Closed-form, license-free pore-pressure prediction with *explicit* uncertainty
and a communicated safe-drilling (mud-weight) window. Inspired by the
Geoprovider/Havtil report *"Pore Pressure Uncertainty & Communication"* which
ties well-control incidents on the NCS to poorly-communicated uncertainty
(issue #1034).

    "Uncertainty is unavoidable. Poorly understood (or poorly communicated)
    uncertainty is not."

Methods
-------
Two independent, industry-standard pore-pressure predictors are implemented and
then *combined into a distribution* (P10 / P50 / P90) rather than a single
deterministic curve.

**Eaton's method** (Eaton, 1975) — empirical, driven by the departure of a
porosity-sensitive log (resistivity or sonic) from its normal-compaction
trend::

    resistivity:  Pp = Sv - (Sv - Pn) * (R_obs   / R_normal)^n     (n ~ 1.2)
    sonic:        Pp = Sv - (Sv - Pn) * (dt_normal / dt_obs)^n      (n ~ 3.0)

where ``Sv`` is the overburden (vertical-stress) gradient, ``Pn`` the normal
(hydrostatic) pore-pressure gradient, and ``n`` the Eaton exponent. In an
overpressured (undercompacted) zone resistivity falls below trend and sonic
transit time rises above trend, so both ratios drop below 1 and ``Pp`` climbs
toward ``Sv``.

**Bowers' method** (Bowers, 1995) — effective-stress / velocity based. The
virgin (loading) curve relates sonic velocity to vertical effective stress::

    Vp = V0 + A * sigma_e^B          ->   sigma_e = ((Vp - V0) / A)^(1/B)
    Pp = Sv - sigma_e

with ``V0`` ~ mudline velocity (~5000 ft/s), and ``A``, ``B`` calibrated
constants. (The unloading branch for centroid/uplift effects is a follow-up;
this screening tier uses the loading curve.)

Distribution & calibration
---------------------------
Method point estimates are combined into a normal distribution whose spread
carries (a) the *between-method* disagreement and (b) an assumed *within-method*
model uncertainty ``model_sigma``. P10/P50/P90 follow from the standard-normal
quantiles (z = +/-1.2816). A simple **calibration** shifts or scales the
distribution to honour a measured point (offset-well RFT/MDT or LOT/FIT).

Safe drilling window
--------------------
The mud-weight window is communicated as ``pore (kick bound) <= MW <= fracture
(loss bound)`` in EMW (ppg), conservatively built from the *high* pore pressure
(P90) and *low* fracture gradient (P10), for both **static** and **dynamic**
(ECD) conditions. This is the bound fed to swab/surge and hydraulics (#1036).

Units: depth ft, mud weight / gradients EMW ppg, pressure psi, sonic transit
time us/ft, velocity ft/s, resistivity ohm.m.
"""

from __future__ import annotations

import json
import math
import statistics
from pathlib import Path
from typing import Any

__all__ = [
    "PorePressureDistribution",
    "SafeDrillingWindow",
    "eaton_pore_pressure",
    "bowers_pore_pressure",
    "combine_methods",
    "calibrate",
    "safe_drilling_window",
    "router",
]

# Pressure / gradient conversion: gradient [psi/ft] = 0.052 * EMW [ppg].
_PSI_PER_PPG_FT = 0.052
# Standard-normal quantile for the 10th / 90th percentile (P10 / P90).
_Z_P90 = 1.2815515594


# ---------------------------------------------------------------------------
# Result objects (plain classes, mirroring swab_surge.py style)
# ---------------------------------------------------------------------------


class PorePressureDistribution:
    """Pore-pressure estimate as a distribution in EMW (ppg).

    ``p10 < p50 < p90`` by construction (low / median / high pore pressure).
    """

    def __init__(
        self,
        *,
        p50: float,
        sigma: float,
        methods: dict[str, float] | None = None,
        between_method_sigma: float = 0.0,
        model_sigma: float = 0.0,
        calibration: dict[str, Any] | None = None,
    ) -> None:
        if sigma < 0:
            raise ValueError("sigma must be >= 0")
        self.p50 = float(p50)
        self.sigma = float(sigma)
        self.p10 = self.p50 - _Z_P90 * self.sigma
        self.p90 = self.p50 + _Z_P90 * self.sigma
        self.methods = dict(methods or {})
        self.between_method_sigma = float(between_method_sigma)
        self.model_sigma = float(model_sigma)
        self.calibration = calibration

    @property
    def mean(self) -> float:
        """Mean of the (symmetric) normal distribution = P50."""
        return self.p50

    def shifted(self, delta: float) -> "PorePressureDistribution":
        """Return a copy translated by ``delta`` ppg (spread unchanged)."""
        out = PorePressureDistribution(
            p50=self.p50 + delta,
            sigma=self.sigma,
            methods={k: v + delta for k, v in self.methods.items()},
            between_method_sigma=self.between_method_sigma,
            model_sigma=self.model_sigma,
        )
        return out

    def scaled(self, factor: float) -> "PorePressureDistribution":
        """Return a copy scaled by ``factor`` about zero (mean and spread)."""
        return PorePressureDistribution(
            p50=self.p50 * factor,
            sigma=self.sigma * abs(factor),
            methods={k: v * factor for k, v in self.methods.items()},
            between_method_sigma=self.between_method_sigma * abs(factor),
            model_sigma=self.model_sigma * abs(factor),
        )

    def as_dict(self) -> dict[str, Any]:
        out: dict[str, Any] = {
            "p10_emw_ppg": self.p10,
            "p50_emw_ppg": self.p50,
            "p90_emw_ppg": self.p90,
            "sigma_ppg": self.sigma,
            "between_method_sigma_ppg": self.between_method_sigma,
            "model_sigma_ppg": self.model_sigma,
            "methods": self.methods,
        }
        if self.calibration is not None:
            out["calibration"] = self.calibration
        return out


class SafeDrillingWindow:
    """Mud-weight (EMW, ppg) window between the kick and loss bounds.

    Conservative bounds use the *high* pore pressure (P90) for the kick floor
    and the *low* fracture gradient (P10) for the loss ceiling. The dynamic
    ceiling is reduced by the circulating ECD so that BHP while pumping stays
    below the fracture gradient.
    """

    def __init__(
        self,
        *,
        static_low_emw_ppg: float,
        static_high_emw_ppg: float,
        dynamic_low_emw_ppg: float,
        dynamic_high_emw_ppg: float,
        nominal_low_emw_ppg: float,
        nominal_high_emw_ppg: float,
        ecd_delta_ppg: float,
        kick_margin_ppg: float,
        loss_margin_ppg: float,
    ) -> None:
        self.static_low_emw_ppg = static_low_emw_ppg
        self.static_high_emw_ppg = static_high_emw_ppg
        self.dynamic_low_emw_ppg = dynamic_low_emw_ppg
        self.dynamic_high_emw_ppg = dynamic_high_emw_ppg
        self.nominal_low_emw_ppg = nominal_low_emw_ppg
        self.nominal_high_emw_ppg = nominal_high_emw_ppg
        self.ecd_delta_ppg = ecd_delta_ppg
        self.kick_margin_ppg = kick_margin_ppg
        self.loss_margin_ppg = loss_margin_ppg

    @property
    def static_width_ppg(self) -> float:
        return self.static_high_emw_ppg - self.static_low_emw_ppg

    @property
    def dynamic_width_ppg(self) -> float:
        return self.dynamic_high_emw_ppg - self.dynamic_low_emw_ppg

    @property
    def static_feasible(self) -> bool:
        """True if a non-empty static mud-weight window exists."""
        return self.static_width_ppg > 0.0

    @property
    def dynamic_feasible(self) -> bool:
        """True if a non-empty dynamic (circulating) window exists."""
        return self.dynamic_width_ppg > 0.0

    def recommended_mud_weight_ppg(self) -> float | None:
        """Centre of the dynamic window, or ``None`` if it is empty."""
        if not self.dynamic_feasible:
            return None
        return 0.5 * (self.dynamic_low_emw_ppg + self.dynamic_high_emw_ppg)

    def as_dict(self) -> dict[str, Any]:
        rec = self.recommended_mud_weight_ppg()
        return {
            "static_low_emw_ppg": self.static_low_emw_ppg,
            "static_high_emw_ppg": self.static_high_emw_ppg,
            "static_width_ppg": self.static_width_ppg,
            "static_feasible": self.static_feasible,
            "dynamic_low_emw_ppg": self.dynamic_low_emw_ppg,
            "dynamic_high_emw_ppg": self.dynamic_high_emw_ppg,
            "dynamic_width_ppg": self.dynamic_width_ppg,
            "dynamic_feasible": self.dynamic_feasible,
            "nominal_low_emw_ppg": self.nominal_low_emw_ppg,
            "nominal_high_emw_ppg": self.nominal_high_emw_ppg,
            "ecd_delta_ppg": self.ecd_delta_ppg,
            "kick_margin_ppg": self.kick_margin_ppg,
            "loss_margin_ppg": self.loss_margin_ppg,
            "recommended_mud_weight_ppg": rec,
        }


# ---------------------------------------------------------------------------
# Methods
# ---------------------------------------------------------------------------


def eaton_pore_pressure(
    *,
    overburden_emw_ppg: float,
    normal_pressure_emw_ppg: float,
    observed: float,
    normal: float,
    log_type: str = "resistivity",
    exponent: float | None = None,
) -> float:
    """Eaton (1975) pore-pressure gradient, EMW ppg.

    Parameters
    ----------
    overburden_emw_ppg : float
        Overburden / vertical-stress gradient ``Sv`` at the depth, EMW ppg.
    normal_pressure_emw_ppg : float
        Normal (hydrostatic) pore-pressure gradient ``Pn``, EMW ppg
        (~8.5-8.9 for typical formation water / seawater).
    observed : float
        Observed log value at the depth: resistivity ``R_obs`` (ohm.m) for
        ``log_type='resistivity'`` or sonic transit time ``dt_obs`` (us/ft)
        for ``log_type='sonic'``.
    normal : float
        Normal-compaction-trend value at the depth (``R_normal`` or
        ``dt_normal``), same units as ``observed``.
    log_type : {'resistivity', 'sonic'}
        Selects the ratio form.
    exponent : float, optional
        Eaton exponent. Defaults to 1.2 (resistivity) or 3.0 (sonic).

    Notes
    -----
    Resistivity ratio is ``(R_obs / R_normal)``; sonic ratio is the inverse
    ``(dt_normal / dt_obs)`` because transit time *rises* with undercompaction
    while resistivity *falls*. In a normally-compacted interval the ratio is 1
    and ``Pp = Pn``.
    """
    if log_type not in ("resistivity", "sonic"):
        raise ValueError("log_type must be 'resistivity' or 'sonic'")
    if observed <= 0 or normal <= 0:
        raise ValueError("observed and normal log values must be > 0")
    if overburden_emw_ppg <= normal_pressure_emw_ppg:
        raise ValueError("overburden must exceed the normal pore pressure")

    if log_type == "resistivity":
        n = 1.2 if exponent is None else float(exponent)
        ratio = observed / normal
    else:  # sonic
        n = 3.0 if exponent is None else float(exponent)
        ratio = normal / observed

    pp = overburden_emw_ppg - (
        overburden_emw_ppg - normal_pressure_emw_ppg
    ) * (ratio ** n)
    return pp


def bowers_pore_pressure(
    *,
    overburden_emw_ppg: float,
    tvd_ft: float,
    velocity_ft_s: float | None = None,
    sonic_dt_us_ft: float | None = None,
    v0_ft_s: float = 5000.0,
    a: float = 9.0,
    b: float = 0.75,
) -> float:
    """Bowers (1995) loading-curve pore-pressure gradient, EMW ppg.

    Uses the virgin compaction relation ``Vp = V0 + A*sigma_e^B`` to recover the
    vertical effective stress ``sigma_e`` from sonic velocity, then
    ``Pp = Sv - sigma_e`` (Terzaghi). Velocity may be supplied directly
    (``velocity_ft_s``) or derived from sonic transit time
    (``Vp = 1e6 / dt`` for ``sonic_dt_us_ft``).

    Parameters
    ----------
    overburden_emw_ppg : float
        Overburden gradient ``Sv`` at the depth, EMW ppg.
    tvd_ft : float
        True vertical depth, ft (needed to convert stress psi <-> gradient).
    velocity_ft_s, sonic_dt_us_ft : float, optional
        Provide exactly one. Compressional velocity (ft/s) or transit time
        (us/ft).
    v0_ft_s : float
        Mudline / fluid velocity intercept ``V0``, ft/s.
    a, b : float
        Bowers loading-curve constants ``A`` and ``B``.
    """
    if tvd_ft <= 0:
        raise ValueError("tvd_ft must be > 0")
    if (velocity_ft_s is None) == (sonic_dt_us_ft is None):
        raise ValueError("provide exactly one of velocity_ft_s or sonic_dt_us_ft")
    if a <= 0 or b <= 0:
        raise ValueError("Bowers a and b must be > 0")

    if velocity_ft_s is None:
        if sonic_dt_us_ft <= 0:
            raise ValueError("sonic_dt_us_ft must be > 0")
        velocity_ft_s = 1.0e6 / sonic_dt_us_ft

    if velocity_ft_s <= v0_ft_s:
        # At/below the fluid velocity there is no skeleton stress; pore
        # pressure rises to the overburden (fully overpressured screen).
        return overburden_emw_ppg

    sigma_e_psi = ((velocity_ft_s - v0_ft_s) / a) ** (1.0 / b)
    overburden_psi = _PSI_PER_PPG_FT * overburden_emw_ppg * tvd_ft
    pp_psi = overburden_psi - sigma_e_psi
    pp_emw = pp_psi / (_PSI_PER_PPG_FT * tvd_ft)
    # Floor at the fluid (normal) line is not enforced here; the caller may
    # clamp. Cap at overburden to keep it physical.
    return min(pp_emw, overburden_emw_ppg)


# ---------------------------------------------------------------------------
# Distribution & calibration
# ---------------------------------------------------------------------------


def combine_methods(
    estimates: dict[str, float],
    *,
    model_sigma_ppg: float = 0.5,
) -> PorePressureDistribution:
    """Combine per-method point estimates into a P10/P50/P90 distribution.

    The P50 is the mean of the method estimates. The total spread combines the
    *between-method* scatter (population std-dev of the estimates) with an
    assumed *within-method* model uncertainty ``model_sigma_ppg`` in
    quadrature::

        sigma = sqrt(between^2 + model_sigma^2)

    A single method therefore yields ``sigma = model_sigma_ppg`` (the model
    uncertainty alone), never zero — honest uncertainty communication.
    """
    if not estimates:
        raise ValueError("estimates must contain at least one method")
    if model_sigma_ppg < 0:
        raise ValueError("model_sigma_ppg must be >= 0")

    values = list(estimates.values())
    p50 = statistics.fmean(values)
    between = statistics.pstdev(values) if len(values) > 1 else 0.0
    sigma = math.hypot(between, model_sigma_ppg)
    return PorePressureDistribution(
        p50=p50,
        sigma=sigma,
        methods=dict(estimates),
        between_method_sigma=between,
        model_sigma=model_sigma_ppg,
    )


def calibrate(
    dist: PorePressureDistribution,
    *,
    measured_emw_ppg: float,
    mode: str = "shift",
) -> PorePressureDistribution:
    """Calibrate a distribution to a measured pore pressure (RFT/MDT, LOT/FIT).

    ``mode='shift'`` translates the distribution so its P50 hits the measured
    value (offset bias). ``mode='scale'`` multiplies by ``measured / P50``
    (proportional correction). ``mode='none'`` returns the input unchanged.
    The calibration delta/factor is recorded on the returned object.
    """
    if mode == "none":
        return dist
    if mode == "shift":
        delta = measured_emw_ppg - dist.p50
        out = dist.shifted(delta)
        out.calibration = {
            "mode": "shift",
            "measured_emw_ppg": measured_emw_ppg,
            "delta_ppg": delta,
        }
        return out
    if mode == "scale":
        if dist.p50 == 0:
            raise ValueError("cannot scale-calibrate a zero P50")
        factor = measured_emw_ppg / dist.p50
        out = dist.scaled(factor)
        out.calibration = {
            "mode": "scale",
            "measured_emw_ppg": measured_emw_ppg,
            "factor": factor,
        }
        return out
    raise ValueError("mode must be 'shift', 'scale', or 'none'")


def safe_drilling_window(
    pore_pressure: PorePressureDistribution,
    fracture: PorePressureDistribution,
    *,
    ecd_delta_ppg: float = 0.0,
    kick_margin_ppg: float = 0.0,
    loss_margin_ppg: float = 0.0,
) -> SafeDrillingWindow:
    """Safe mud-weight window between kick (pore) and loss (fracture) bounds.

    Conservative bounds:
        * floor  = pore-pressure **P90** + ``kick_margin`` (highest credible
          pore pressure -> avoid influx/kick),
        * ceiling = fracture **P10** - ``loss_margin`` (lowest credible
          fracture gradient -> avoid losses).

    The static window applies these directly. The dynamic window lowers the
    ceiling by ``ecd_delta_ppg`` so that BHP while circulating (MW + ECD) stays
    below the fracture gradient; the floor is unchanged because the static MW
    must still kill the well during connections. The nominal window is the
    P50-to-P50 span for reference.

    Parameters
    ----------
    pore_pressure, fracture : PorePressureDistribution
        Pore-pressure and fracture-gradient distributions, EMW ppg.
    ecd_delta_ppg : float
        Equivalent circulating-density rise from annular friction, ppg.
    kick_margin_ppg, loss_margin_ppg : float
        Operational safety margins (trip/kick and loss/riser margins), ppg.
    """
    if ecd_delta_ppg < 0:
        raise ValueError("ecd_delta_ppg must be >= 0")
    if kick_margin_ppg < 0 or loss_margin_ppg < 0:
        raise ValueError("margins must be >= 0")

    static_low = pore_pressure.p90 + kick_margin_ppg
    static_high = fracture.p10 - loss_margin_ppg
    dynamic_low = static_low
    dynamic_high = static_high - ecd_delta_ppg
    nominal_low = pore_pressure.p50
    nominal_high = fracture.p50

    return SafeDrillingWindow(
        static_low_emw_ppg=static_low,
        static_high_emw_ppg=static_high,
        dynamic_low_emw_ppg=dynamic_low,
        dynamic_high_emw_ppg=dynamic_high,
        nominal_low_emw_ppg=nominal_low,
        nominal_high_emw_ppg=nominal_high,
        ecd_delta_ppg=ecd_delta_ppg,
        kick_margin_ppg=kick_margin_ppg,
        loss_margin_ppg=loss_margin_ppg,
    )


# ---------------------------------------------------------------------------
# Config-driven helpers
# ---------------------------------------------------------------------------


def _fracture_distribution(
    frac_cfg: dict[str, Any], model_sigma_ppg: float
) -> PorePressureDistribution:
    """Build the fracture-gradient distribution from config.

    Accepts an explicit ``p50_emw_ppg`` (+ optional ``sigma_ppg``) or, for
    convenience, a single ``emw_ppg`` value.
    """
    if "p50_emw_ppg" in frac_cfg:
        p50 = float(frac_cfg["p50_emw_ppg"])
    elif "emw_ppg" in frac_cfg:
        p50 = float(frac_cfg["emw_ppg"])
    else:
        raise KeyError("fracture config needs 'p50_emw_ppg' or 'emw_ppg'")
    sigma = float(frac_cfg.get("sigma_ppg", model_sigma_ppg))
    return PorePressureDistribution(p50=p50, sigma=sigma, methods={"fracture": p50})


def _estimates_from_config(inputs: dict[str, Any]) -> dict[str, float]:
    """Run the configured methods and return ``{method: pore_pressure_emw}``."""
    obg = float(inputs["overburden_emw_ppg"])
    pn = float(inputs["normal_pressure_emw_ppg"])
    tvd = float(inputs["tvd"])
    estimates: dict[str, float] = {}

    eaton_cfg = inputs.get("eaton")
    if eaton_cfg:
        estimates["eaton"] = eaton_pore_pressure(
            overburden_emw_ppg=obg,
            normal_pressure_emw_ppg=pn,
            observed=float(eaton_cfg["observed"]),
            normal=float(eaton_cfg["normal"]),
            log_type=eaton_cfg.get("method", "resistivity"),
            exponent=(
                float(eaton_cfg["exponent"]) if "exponent" in eaton_cfg else None
            ),
        )

    bowers_cfg = inputs.get("bowers")
    if bowers_cfg:
        estimates["bowers"] = bowers_pore_pressure(
            overburden_emw_ppg=obg,
            tvd_ft=tvd,
            velocity_ft_s=(
                float(bowers_cfg["sonic_velocity_ft_s"])
                if "sonic_velocity_ft_s" in bowers_cfg
                else None
            ),
            sonic_dt_us_ft=(
                float(bowers_cfg["sonic_dt_us_ft"])
                if "sonic_dt_us_ft" in bowers_cfg
                else None
            ),
            v0_ft_s=float(bowers_cfg.get("v0_ft_s", 5000.0)),
            a=float(bowers_cfg.get("a", 9.0)),
            b=float(bowers_cfg.get("b", 0.75)),
        )

    if not estimates:
        raise ValueError("at least one of 'eaton' or 'bowers' must be configured")
    return estimates


def _resolve_dir(cfg: dict[str, Any], value: str | Path) -> Path:
    path = Path(value)
    if path.is_absolute():
        return path
    return Path(cfg.get("_config_dir_path", Path.cwd())) / path


def _write_summary(
    cfg: dict[str, Any],
    output_cfg: dict[str, Any],
    payload: dict[str, Any],
    default_directory: str,
    default_filename: str,
) -> Path:
    directory = _resolve_dir(cfg, output_cfg.get("directory", default_directory))
    directory.mkdir(parents=True, exist_ok=True)
    summary_path = directory / output_cfg.get("summary_json", default_filename)
    summary_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    cfg.setdefault("outputs", {})["directory"] = str(directory)
    cfg["outputs"]["summary_json"] = str(summary_path)
    return summary_path


def router(cfg: dict[str, Any]) -> dict[str, Any]:
    """Engine router for ``basename: pore_pressure``.

    Reads ``cfg['pore_pressure']``, runs the configured methods, builds the
    P10/P50/P90 distribution, optionally calibrates it, derives the safe
    drilling window, writes a JSON summary, and returns ``cfg`` with the result
    under ``cfg['pore_pressure']['result']`` (mirrors ``run_swab_surge``).
    """
    params = cfg.get("pore_pressure", {})
    inputs = params["inputs"]

    ensemble_cfg = params.get("ensemble", {})
    model_sigma = float(ensemble_cfg.get("model_sigma_ppg", 0.5))

    estimates = _estimates_from_config(inputs)
    dist = combine_methods(estimates, model_sigma_ppg=model_sigma)

    calib_cfg = params.get("calibration")
    if calib_cfg and calib_cfg.get("mode", "none") != "none":
        dist = calibrate(
            dist,
            measured_emw_ppg=float(calib_cfg["measured_pore_pressure_emw_ppg"]),
            mode=calib_cfg.get("mode", "shift"),
        )

    result: dict[str, Any] = {"pore_pressure": dist.as_dict()}

    window_cfg = params.get("window")
    frac_cfg = params.get("fracture")
    if frac_cfg is not None:
        frac_dist = _fracture_distribution(frac_cfg, model_sigma)
        result["fracture"] = frac_dist.as_dict()
        wcfg = window_cfg or {}
        window = safe_drilling_window(
            dist,
            frac_dist,
            ecd_delta_ppg=float(wcfg.get("ecd_delta_ppg", 0.0)),
            kick_margin_ppg=float(wcfg.get("kick_margin_ppg", 0.0)),
            loss_margin_ppg=float(wcfg.get("loss_margin_ppg", 0.0)),
        )
        result["drilling_window"] = window.as_dict()

    summary = {
        "calculation": "pore_pressure",
        "inputs": dict(inputs),
        "result": result,
    }
    summary["summary_json"] = str(
        _write_summary(
            cfg,
            params.get("outputs", {}),
            summary,
            "results/pore_pressure",
            "pore_pressure_summary.json",
        )
    )
    cfg["pore_pressure"] = {**params, "result": result, "summary": summary}
    return cfg
