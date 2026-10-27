# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Instrument selection and causal alerts for self-elevating unit demonstrations.

This is an educational screen, not a soil, structural or stability assessment.
All thresholds, persistence counts and permissible data gaps are caller inputs.
Positive settlement is downward; rate and acceleration are signed increases.
Magnitude rules use absolute values, including reversed bending or inclination.
Derivatives use backward differences only, with acceleration evaluated between
adjacent interval midpoints. An alert is timestamped at its confirming sample;
there is no backdating or interpolation. Invalid or gapped data raises ValueError
and must be handled as an unavailable monitor, never as a safe condition.

FAILURE_MODES records proposed instrument mappings inferred from the open sources
below. The sources support the physical mechanisms and observed quantities;
they do not validate these alert algorithms, numerical settings or lead times.
The synthetic examples deliberately provide a precursor. Real punch-through or
other sudden failures can occur without a measurable warning.
"""

from dataclasses import dataclass
from math import exp, isfinite
from typing import Mapping, Sequence


@dataclass(frozen=True)
class Source:
    """Open source consulted for a mapping, with publication terms."""

    title: str
    url: str
    locator: str
    terms: str
    retrieved: str = "2026-10-27"


SOURCES = {
    "penetration": Source(
        "On Cubic Spudcan Deep Penetration in Dense Sand Overlying Non-Uniform Clay",
        "https://mdpi-res.com/d_attachment/jmse/jmse-10-01607/"
        "article_deploy/jmse-10-01607.pdf",
        "Introduction and load-penetration discussion; DOI 10.3390/jmse10111607",
        "CC BY 4.0; https://creativecommons.org/licenses/by/4.0/",
    ),
    "structure": Source(
        "Structural Analysis Procedure and Applicability Review of Spudcan Considering Soil Types",
        "https://mdpi-res.com/d_attachment/jmse/jmse-11-01833/"
        "article_deploy/jmse-11-01833.pdf",
        "Sections 2.1 and 2.2; DOI 10.3390/jmse11091833",
        "CC BY 4.0; https://creativecommons.org/licenses/by/4.0/",
    ),
    "waves": Source(
        "Experimental Investigation of the Coupling Effect of Jackup Offshore Platforms, "
        "Towers, and Seabed Foundations under Waves of Large Wave Height",
        "https://mdpi-res.com/d_attachment/water/water-15-00024/"
        "article_deploy/water-15-00024.pdf",
        "Section 2.3 and Figure 2: acceleration, displacement, wave-height sensors; "
        "DOI 10.3390/w15010024",
        "CC BY 4.0; https://creativecommons.org/licenses/by/4.0/",
    ),
    "transit": Source(
        "Investigation on Seakeeping of WTIVs Considering the Effect of Leg-Spudcan Well",
        "https://mdpi-res.com/d_attachment/applsci/applsci-15-12701/"
        "article_deploy/applsci-15-12701.pdf",
        "Abstract: roll period, damping and motion response; DOI 10.3390/app152312701",
        "CC BY 4.0; https://creativecommons.org/licenses/by/4.0/",
    ),
    "operation": Source(
        "46 CFR Part 134: Added Provisions for Liftboats (2025 edition)",
        "https://www.govinfo.gov/content/pkg/CFR-2025-title46-vol4/"
        "pdf/CFR-2025-title46-vol4-part134.pdf",
        "Sections 134.140 Structural standards, 134.150 Liftboat-jacking systems, "
        "134.170 Operating manual",
        "US federal regulation; public domain under 17 USC 105; "
        "https://www.govinfo.gov/about/policies",
    ),
    "stability": Source(
        "46 CFR Part 174: Specific Vessel Types (2025 edition)",
        "https://www.govinfo.gov/content/pkg/CFR-2025-title46-vol7/"
        "pdf/CFR-2025-title46-vol7-part174.pdf",
        "Sections 174.045 Intact stability requirements and 174.255 Stability",
        "US federal regulation; public domain under 17 USC 105; "
        "https://www.govinfo.gov/about/policies",
    ),
}


@dataclass(frozen=True)
class FailureMode:
    """Trace an instrument proposal to a mechanism and its warning limits."""

    name: str
    instruments: tuple[str, ...]
    precursor: str
    logic: str
    lead_time: str
    sources: tuple[str, ...]


FAILURE_MODES = (
    FailureMode(
        "Punch-through during preload",
        ("Preload and settlement logging per leg", "Leg inclination", "Hull motion IMU"),
        "Preload/settlement logs: increasing penetration rate at held load. "
        "Leg inclination: rotation. Hull IMU: tilt or downward acceleration, "
        "possibly only after support is lost. Soil resistance can drop after a peak.",
        "Input settlement rate or acceleration threshold and persistence during a "
        "confirmed preload hold; cross-check per-leg loads and inclination.",
        "Potentially seconds to minutes if progressive; zero for sudden loss of support. "
        "Synthetic lead only is measured below.",
        ("penetration", "structure"),
    ),
    FailureMode(
        "Uneven settlement",
        ("Settlement logging per leg", "Leg inclination", "Hull motion IMU"),
        "Settlement logs: growing difference between leg penetrations. "
        "Leg inclination: local tilt drift. Hull IMU: persistent heel or trim; "
        "hull attitude alone cannot locate the affected footing.",
        "Input maximum differential settlement, tilt magnitude or their rise rates, "
        "with persistence and a common surveyed reference.",
        "Minutes to hours for slow drift; none guaranteed for abrupt settlement.",
        ("structure", "waves"),
    ),
    FailureMode(
        "Leg overload or bending in an elevated storm",
        ("Opposed leg strain gauges", "Leg inclination", "Metocean: wave, wind, current"),
        "Strain gauges: rising mean axial strain or opposed-gauge difference "
        "(bending). Inclination: leg tilt. Metocean sensors: increasing wave, wind "
        "and current demand, not the resulting structural utilization.",
        "Input strain magnitude, bending-strain difference or tilt threshold below "
        "assessed limits; temperature-compensate and require persistence.",
        "Seconds to minutes for measured response; longer weather preparation time "
        "comes from a qualified forecast, not a strain sensor.",
        ("structure", "waves", "operation"),
    ),
    FailureMode(
        "Sliding or overturning",
        ("Leg inclination", "Hull motion IMU", "Leg strain gauges", "Jacking load readings",
         "Metocean: wave, wind, current"),
        "Inclination: leg rotation. IMU: hull tilt or acceleration. Strain gauges: "
        "load redistribution. Jacking loads: support unloading. Metocean: growing "
        "environmental demand. Pure translation can escape these instruments; "
        "add a position reference.",
        "Input tilt rate, load-imbalance or minimum support-load threshold and "
        "site-assessed metocean envelope. Fuse position data for sliding detection.",
        "Seconds to minutes if progressive; no guaranteed warning of sudden sliding "
        "or overturning. These signals do not measure a stability margin.",
        ("structure", "waves", "stability"),
    ),
    FailureMode(
        "Afloat transit motion and stability",
        ("Hull motion IMU", "Metocean: wave, wind, current"),
        "Hull IMU: increasing roll, pitch, acceleration or persistent heel. "
        "Metocean: worsening waves, wind and current. IMU motion does not "
        "establish intact or damaged stability.",
        "Input roll or pitch magnitude, persistent heel or acceleration threshold "
        "from the approved loading condition and transit assessment.",
        "Seconds for measured motions; weather forecasts can support hours of planning. "
        "Flooding or abrupt wind shifts may give little warning.",
        ("transit", "waves", "operation", "stability"),
    ),
    FailureMode(
        "Jacking system overload",
        ("Jacking load readings per leg or drive", "Leg strain gauges", "Leg inclination"),
        "Jacking readings: increasing drive load or unequal sharing. Strain gauges: "
        "rising axial response. Inclination: out-of-level elevation. Load readings "
        "cannot replace hydraulic pressure and fluid-level alarms.",
        "Input drive-load magnitude, load rise rate or imbalance threshold below "
        "the rated envelope, plus persistence; retain independent protective trips.",
        "Seconds to minutes for a gradual overload; an abrupt drive failure may "
        "have no precursor.",
        ("operation", "structure"),
    ),
)


@dataclass(frozen=True)
class AlertRule:
    """Explicit threshold in the derived metric's unit, with input-quality limits."""

    name: str
    channel: str
    metric: str
    threshold: float
    unit: str
    persistence: int
    max_gap_s: float

    def __post_init__(self):
        """Reject undefined rules rather than silently producing no alert."""
        if self.metric not in {"magnitude", "rise_rate", "acceleration"}:
            raise ValueError("unknown metric")
        if not self.name or not self.channel or not self.unit:
            raise ValueError("name, channel and metric unit are required")
        if not isfinite(self.threshold) or self.threshold <= 0:
            raise ValueError("threshold must be finite and positive")
        if type(self.persistence) is not int or self.persistence < 1:
            raise ValueError("persistence must be a positive integer")
        if not isfinite(self.max_gap_s) or self.max_gap_s <= 0:
            raise ValueError("maximum data gap must be finite and positive")


@dataclass(frozen=True)
class Alert:
    """First confirming sample for one rule, without a predicted failure time."""

    rule: str
    time_s: float
    value: float
    unit: str


def evaluate(time_s: Sequence[float], channels: Mapping[str, Sequence[float]],
             rules: Sequence[AlertRule]) -> tuple[Alert, ...]:
    """Return first persistent alerts using only present and earlier samples.

    Threshold equality counts as a crossing. A false sample resets persistence.
    Caller units must match the rule: for example m, m/s and m/s² when the clock
    is seconds. No conversion, smoothing, extrapolation or preload-hold inference
    is performed. Supply only the phase in which a configured rule is applicable.
    """
    if not time_s or any(not isfinite(t) for t in time_s):
        raise ValueError("a nonempty finite clock is required")
    gaps = [b - a for a, b in zip(time_s, time_s[1:])]
    if any(dt <= 0 for dt in gaps):
        raise ValueError("clock must increase strictly")
    if len({r.name for r in rules}) != len(rules):
        raise ValueError("rule names must be unique")
    alerts = []
    for rule in rules:
        if rule.channel not in channels:
            raise ValueError(f"missing channel: {rule.channel}")
        values = channels[rule.channel]
        if len(values) != len(time_s) or any(not isfinite(v) for v in values):
            raise ValueError(f"invalid measurements: {rule.channel}")
        if any(dt > rule.max_gap_s for dt in gaps):
            raise ValueError(f"data gap exceeds input limit: {rule.channel}")
        count = 0
        for i, t in enumerate(time_s):
            if rule.metric == "magnitude":
                value = abs(values[i])
            elif i == 0:
                continue
            else:
                value = (values[i] - values[i - 1]) / gaps[i - 1]
                if rule.metric == "acceleration":
                    if i < 2:
                        continue
                    previous = (values[i - 1] - values[i - 2]) / gaps[i - 2]
                    value = 2 * (value - previous) / (gaps[i - 1] + gaps[i - 2])
            if not isfinite(value):
                raise ValueError(f"nonfinite derived metric: {rule.channel}")
            count = count + 1 if value >= rule.threshold else 0
            if count >= rule.persistence:
                alerts.append(Alert(rule.name, t, value, rule.unit))
                break
    return tuple(alerts)


@dataclass(frozen=True)
class DemoCase:
    """Synthetic log and separately defined event for retrospective lead time."""

    time_s: tuple[float, ...]
    channels: Mapping[str, tuple[float, ...]]
    rules: tuple[AlertRule, ...]
    event_s: float | None
    limits: Mapping[str, float]


def synthetic_cases() -> dict[str, DemoCase]:
    """Build deterministic educational logs; no measured or earlier project data.

    Preload: 10 s samples; load ramps to 1000 kN at 240 s then holds. Settlement
    is 0.18*(1-exp(-t/75)) m; the accelerating case adds 0.000006*(t-240)² m
    after 240 s and a discontinuous 0.8 m penetration at the imposed 660 s event.
    Storm: 60 s samples, inclination 0.05+0.00006*t deg, strain 80+0.18*t µε.
    Its event is the first sampled crossing of either illustrative response limit.
    Settings below are synthetic inputs, not literature or equipment limits.
    """
    preload_t = tuple(range(0, 901, 10))
    settled = tuple(0.18 * (1 - exp(-t / 75)) for t in preload_t)
    accelerating = tuple(s + 0.000006 * max(0, t - 240)**2 + (0.8 if t >= 660 else 0)
                         for t, s in zip(preload_t, settled))
    load = tuple(1000 * min(t / 240, 1) for t in preload_t)
    preload_rules = (
        AlertRule("settlement acceleration", "settlement_m", "acceleration",
                  0.000004, "m/s²", 3, 20),
        AlertRule("settlement rate", "settlement_m", "rise_rate", 0.0025, "m/s", 3, 20),
    )
    storm_t = tuple(range(0, 7201, 60))
    storm = {
        "inclination_deg": tuple(0.05 + 0.00006*t for t in storm_t),
        "strain_microstrain": tuple(80 + 0.18*t for t in storm_t),
    }
    limits = {"inclination_deg": 0.45, "strain_microstrain": 1200}
    event = next(t for i, t in enumerate(storm_t)
                 if any(storm[c][i] >= limit for c, limit in limits.items()))
    storm_rules = (
        AlertRule("leg inclination", "inclination_deg", "magnitude", 0.3, "deg", 2, 120),
        AlertRule("leg strain", "strain_microstrain", "magnitude", 850, "µε", 2, 120),
    )
    return {
        "settled": DemoCase(preload_t, {"settlement_m": settled, "preload_kn": load},
                            preload_rules, None, {}),
        "accelerating": DemoCase(preload_t,
                                 {"settlement_m": accelerating, "preload_kn": load},
                                 preload_rules, 660, {}),
        "storm": DemoCase(storm_t, storm, storm_rules, event, limits),
    }
