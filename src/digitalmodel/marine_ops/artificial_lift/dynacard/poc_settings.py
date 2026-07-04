# ABOUTME: Pump-off-controller (POC) setpoint recommendations and alarm evaluation.
# ABOUTME: Field-practice rules from rod-pump automation training and POC vendor manuals.
"""Recommend rod-pump controller (POC / VSD) setpoints and evaluate alarms
from a measured dynamometer card.

The rules encode standard beam-pump automation field practice, cross-checked
between operator rod-pump automation training (2016-2017), a POC vendor
user manual (setpoint recipes re-stated in original wording), and
Bommer & Podio, *The Beam Lift Handbook* (UT Austin):

* High-load, two tiers: alarm at PPRL x 1.10-1.12, shutdown at
  PPRL x 1.20-1.25 — never above the unit structure rating.
* Low-load, two tiers: deep wells use fixed offsets (alarm MPRL - 1,000 to
  1,500 lb, shutdown MPRL - 2,000 to 2,500 lb); shallow wells use fractions
  (alarm MPRL x 0.9, shutdown MPRL x 0.8) because fixed offsets can go
  non-positive. (Some operators instead use 50 % of MPRL as the shallow
  shutdown; the fraction recipe here is the two-tier vendor guidance.)
* Load-span malfunction: flag when the stroke's load span collapses below
  0.6-0.75 of the normal span (deep rod parts, dead pump valves, gas-locked
  pump, flumping/flowing wells).
* Consecutive-stroke debounce on alarms; pump-off after 2-5 consecutive
  low-fillage strokes (10-15 for very gassy wells, which are treated with a
  lower setpoint and more strokes, never as fluid pound).
* Card-area surveillance: low area = deep rod part / gas lock / dead pump;
  high area = paraffin / scale / emulsion friction (condition-based
  chemical-treatment trigger).
* VSD: pump-fillage setpoint ~90 % with a +/-5 % deadband (2 % for tight
  control); minimum speed >= 4 SPM without a low-speed gearbox lubrication
  system; polished-rod velocity limit stroke_length x SPM < 1,440 in/min
  (~1,200 preferred for lower failure frequency).
* Automatic idle time: hold total cycle time constant — next idle =
  target cycle - average recent run time, floored at the minimum idle;
  halve the target cycle when run time falls below half the cycle.
"""

from __future__ import annotations

from typing import List, Optional

import numpy as np
from pydantic import BaseModel

from .models import CardData

# Field-practice constants (see module docstring for provenance).
HIGH_LOAD_ALARM_FRACTION = 0.10  # PPRL x 1.10 (1.10-1.12 band)
HIGH_LOAD_SHUTDOWN_FRACTION = 0.20  # PPRL x 1.20 (1.20-1.25 band)
LOW_LOAD_ALARM_OFFSET_LBS = 1500.0  # deep wells: MPRL - 1,000..1,500 lb
LOW_LOAD_SHUTDOWN_OFFSET_LBS = 2500.0  # deep wells: MPRL - 2,000..2,500 lb
LOW_LOAD_ALARM_FRACTION = 0.90  # shallow wells: MPRL x 0.9
LOW_LOAD_SHUTDOWN_FRACTION = 0.80  # shallow wells: MPRL x 0.8
LOAD_SPAN_MALFUNCTION_FRACTION = 0.60  # 0.6-0.75 of normal span
CARD_AREA_LOW_FRACTION = 0.50  # area below half of normal = malfunction
CARD_AREA_HIGH_FRACTION = 1.50  # area 50 % above normal = friction alarm
ALARM_DEBOUNCE_STROKES = 3  # consecutive strokes before alarm action
POC_STROKES_NORMAL = (2, 5)  # consecutive low-fillage strokes before idle
POC_STROKES_GASSY = (10, 15)
FILLAGE_SETPOINT_DEFAULT_PCT = 90.0  # "as high as possible"; avoid 50 %
FILLAGE_DEADBAND_PCT = 5.0  # VSD deadband (2 % for tight control)
VSD_MIN_SPM_NO_LUBE = 4.0  # without low-speed gearbox oiling
PRV_LIMIT_IN_PER_MIN = 1440.0  # stroke_length x SPM hard limit
PRV_PREFERRED_IN_PER_MIN = 1200.0  # lower failure frequency target
BUOYANCY_FACTOR_STEEL = 0.87  # steel rods in fresh water
SEPARATOR_MAX_DOWNWARD_VELOCITY_IN_PER_S = 6.0  # 1/4" bubble rise velocity
SEPARATOR_CAPACITY_BPD_PER_SQIN = 50.0  # 1 in^2 quiet zone @ 6 in/s


class AlarmSetpoints(BaseModel):
    """Recommended POC alarm / shutdown setpoints for one well."""

    peak_load_reference_lbs: float = 0.0  # PPRL the setpoints are based on
    min_load_reference_lbs: float = 0.0  # MPRL the setpoints are based on
    high_load_alarm_lbs: float = 0.0
    high_load_shutdown_lbs: float = 0.0
    structure_rating_capped: bool = False
    low_load_alarm_lbs: float = 0.0
    low_load_shutdown_lbs: float = 0.0
    shallow_low_load_rule_used: bool = False
    load_span_malfunction_lbs: float = 0.0  # span below this = malfunction
    card_area_low_fraction: float = CARD_AREA_LOW_FRACTION
    card_area_high_fraction: float = CARD_AREA_HIGH_FRACTION
    alarm_debounce_strokes: int = ALARM_DEBOUNCE_STROKES
    fillage_setpoint_pct: float = FILLAGE_SETPOINT_DEFAULT_PCT
    fillage_deadband_pct: float = FILLAGE_DEADBAND_PCT
    pump_off_strokes: int = POC_STROKES_NORMAL[0]
    gassy_well: bool = False
    vsd_min_spm: float = VSD_MIN_SPM_NO_LUBE
    vsd_max_spm: Optional[float] = None  # from PRV limit when stroke known
    notes: List[str] = []


class AlarmEvent(BaseModel):
    """One alarm raised while evaluating a card against setpoints."""

    name: str
    severity: str  # "alarm" | "shutdown" | "malfunction"
    observed: float
    threshold: float
    message: str


def recommend_setpoints(
    peak_load_lbs: float,
    min_load_lbs: float,
    structure_rating_lbs: Optional[float] = None,
    stroke_length_in: Optional[float] = None,
    gassy_well: bool = False,
    low_speed_lube: bool = False,
) -> AlarmSetpoints:
    """Recommend POC setpoints from a normal (reference) card's PPRL / MPRL.

    Args:
        peak_load_lbs: peak polished-rod load of the well's normal card.
        min_load_lbs: minimum polished-rod load of the normal card.
        structure_rating_lbs: pumping-unit structure rating; the high-load
            setpoints are capped here when provided.
        stroke_length_in: surface stroke length; enables the VSD max-SPM
            recommendation from the polished-rod-velocity limit.
        gassy_well: use the gassy pump-off stroke count and note the
            gas-handling strategy (lower setpoint + more strokes).
        low_speed_lube: gearbox has a low-speed lubrication system, allowing
            VSD operation below 4 SPM.
    """
    notes: List[str] = []
    high_alarm = peak_load_lbs * (1.0 + HIGH_LOAD_ALARM_FRACTION)
    high_shutdown = peak_load_lbs * (1.0 + HIGH_LOAD_SHUTDOWN_FRACTION)
    capped = False
    if structure_rating_lbs is not None and high_shutdown > structure_rating_lbs:
        high_shutdown = structure_rating_lbs
        capped = True
        notes.append(
            "High-load shutdown capped at the unit structure rating "
            f"({structure_rating_lbs:.0f} lb)."
        )
        high_alarm = min(high_alarm, structure_rating_lbs)
    low_alarm = min_load_lbs - LOW_LOAD_ALARM_OFFSET_LBS
    low_shutdown = min_load_lbs - LOW_LOAD_SHUTDOWN_OFFSET_LBS
    shallow = False
    if low_shutdown <= 0.0 or low_alarm <= 0.0:
        shallow = True
        low_alarm = min_load_lbs * LOW_LOAD_ALARM_FRACTION
        low_shutdown = min_load_lbs * LOW_LOAD_SHUTDOWN_FRACTION
        notes.append(
            "Shallow-well rule: fixed low-load offsets go non-positive, so "
            "fractions of MPRL are used (alarm x0.9, shutdown x0.8)."
        )
    span = peak_load_lbs - min_load_lbs
    strokes = POC_STROKES_GASSY[0] if gassy_well else POC_STROKES_NORMAL[0]
    if gassy_well:
        notes.append(
            "Gassy well: treat gas interference with a lower pump-off "
            "setpoint and more consecutive strokes (10-15), not as fluid "
            "pound."
        )
    vsd_min = 1.0 if low_speed_lube else VSD_MIN_SPM_NO_LUBE
    vsd_max = None
    if stroke_length_in:
        vsd_max = PRV_LIMIT_IN_PER_MIN / stroke_length_in
        notes.append(
            "VSD max speed from the polished-rod-velocity limit "
            f"(stroke x SPM < {PRV_LIMIT_IN_PER_MIN:.0f} in/min; "
            f"~{PRV_PREFERRED_IN_PER_MIN:.0f} preferred)."
        )
    return AlarmSetpoints(
        peak_load_reference_lbs=peak_load_lbs,
        min_load_reference_lbs=min_load_lbs,
        high_load_alarm_lbs=round(high_alarm, 0),
        high_load_shutdown_lbs=round(high_shutdown, 0),
        structure_rating_capped=capped,
        low_load_alarm_lbs=round(low_alarm, 0),
        low_load_shutdown_lbs=round(low_shutdown, 0),
        shallow_low_load_rule_used=shallow,
        load_span_malfunction_lbs=round(span * LOAD_SPAN_MALFUNCTION_FRACTION, 0),
        pump_off_strokes=strokes,
        gassy_well=gassy_well,
        vsd_min_spm=vsd_min,
        vsd_max_spm=None if vsd_max is None else round(vsd_max, 2),
        notes=notes,
    )


def evaluate_alarms(
    card: CardData,
    setpoints: AlarmSetpoints,
    reference_card: Optional[CardData] = None,
) -> List[AlarmEvent]:
    """Evaluate a measured surface card against recommended setpoints.

    In a controller these conditions are debounced over
    ``setpoints.alarm_debounce_strokes`` consecutive strokes; this function
    evaluates a single card and reports the conditions it trips.
    """
    load = np.asarray(card.load, dtype=float)
    peak = float(load.max())
    mn = float(load.min())
    span = peak - mn
    events: List[AlarmEvent] = []
    if peak >= setpoints.high_load_shutdown_lbs:
        events.append(AlarmEvent(
            name="high_load", severity="shutdown", observed=peak,
            threshold=setpoints.high_load_shutdown_lbs,
            message="Peak load at/above shutdown - stuck pump on upstroke, "
                    "flowline blockage or rising friction; stop the unit.",
        ))
    elif peak >= setpoints.high_load_alarm_lbs:
        events.append(AlarmEvent(
            name="high_load", severity="alarm", observed=peak,
            threshold=setpoints.high_load_alarm_lbs,
            message="Peak load above alarm - investigate friction, "
                    "backpressure or falling casing fluid level.",
        ))
    if mn <= setpoints.low_load_shutdown_lbs:
        events.append(AlarmEvent(
            name="low_load", severity="shutdown", observed=mn,
            threshold=setpoints.low_load_shutdown_lbs,
            message="Minimum load at/below shutdown - parted rods or rods "
                    "sticking/floating on the downstroke; stop the unit.",
        ))
    elif mn <= setpoints.low_load_alarm_lbs:
        events.append(AlarmEvent(
            name="low_load", severity="alarm", observed=mn,
            threshold=setpoints.low_load_alarm_lbs,
            message="Minimum load below alarm - check for rod float or a "
                    "developing part.",
        ))
    if span <= setpoints.load_span_malfunction_lbs:
        events.append(AlarmEvent(
            name="load_span", severity="malfunction", observed=span,
            threshold=setpoints.load_span_malfunction_lbs,
            message="Load span collapsed below the malfunction setpoint - "
                    "deep rod part, dead pump valves, gas-locked pump or a "
                    "flumping (flowing) well.",
        ))
    if reference_card is not None:
        change = card_area_change_pct(card, reference_card)
        low_pct = (setpoints.card_area_low_fraction - 1.0) * 100.0
        high_pct = (setpoints.card_area_high_fraction - 1.0) * 100.0
        if change <= low_pct:
            events.append(AlarmEvent(
                name="card_area_low", severity="malfunction", observed=change,
                threshold=low_pct,
                message="Card area collapsed vs the reference - deep rod "
                        "part, gas lock or dead pump valves.",
            ))
        elif change >= high_pct:
            events.append(AlarmEvent(
                name="card_area_high", severity="alarm", observed=change,
                threshold=high_pct,
                message="Card area well above the reference - rising "
                        "friction work (paraffin / scale / emulsion); "
                        "condition-based chemical-treatment trigger.",
            ))
    return events


def card_area_change_pct(card: CardData, reference: CardData) -> float:
    """Percent change of card area vs a healthy reference card.

    A sustained rise is the condition-based trigger for paraffin / scale
    treatment (rising friction work); a collapse toward zero corroborates a
    load-span malfunction or deep rod part.
    """

    def _area(c: CardData) -> float:
        x = np.asarray(c.position, dtype=float)
        y = np.asarray(c.load, dtype=float)
        return float(abs(np.dot(x, np.roll(y, -1)) - np.dot(y, np.roll(x, -1))) / 2.0)

    ref = _area(reference)
    if ref == 0.0:
        return 0.0
    return round((_area(card) - ref) / ref * 100.0, 1)


def automatic_idle_time(
    target_cycle_min: float,
    average_run_min: float,
    minimum_idle_min: float = 2.0,
) -> dict:
    """Cycle-time-stabilization idle time (the pragmatic POC algorithm).

    Hold the total cycle time constant instead of searching the nonlinear
    idle-vs-run response: next idle = target cycle - average recent run
    time, floored at the minimum idle. When the well is over-pumped (run
    time below half the target cycle) the target cycle is halved.

    Returns dict(next_idle_min, target_cycle_min, over_pumped).
    """
    over_pumped = average_run_min < 0.5 * target_cycle_min
    cycle = target_cycle_min / 2.0 if over_pumped else target_cycle_min
    next_idle = max(minimum_idle_min, cycle - average_run_min)
    return dict(
        next_idle_min=round(next_idle, 1),
        target_cycle_min=round(cycle, 1),
        over_pumped=over_pumped,
    )


def estimate_production_bpd(
    plunger_diameter_in: float,
    spm: float,
    downhole_stroke_in: float,
    pump_efficiency: float = 0.85,
    runtime_fraction: float = 1.0,
) -> float:
    """Pump displacement estimate, BFPD = 0.1166 d^2 SPM SL eta %RT.

    Standard beam-lift displacement formula; pump efficiency ~0.85 for a
    pump with moderate wear.
    """
    return round(
        0.1166 * plunger_diameter_in ** 2 * spm * downhole_stroke_in
        * pump_efficiency * runtime_fraction, 1,
    )


def separator_capacity_bpd(quiet_zone_area_sqin: float) -> float:
    """Downhole gas-separator liquid capacity from the quiet-zone area.

    Separation works while the downward liquid velocity in the quiet zone
    stays below the ~6 in/s rise velocity of a 1/4-inch gas bubble; that
    corresponds to ~50 BPD of liquid per square inch of annular quiet-zone
    area. Separator length adds no capacity - it only needs to hold 1.5-2x
    the pump volume so gas cannot short-circuit to the dip tube.
    """
    return round(quiet_zone_area_sqin * SEPARATOR_CAPACITY_BPD_PER_SQIN, 0)


def rods_parted(
    static_peak_load_lbs: float,
    rod_weight_air_lbs: float,
    buoyancy_factor: float = BUOYANCY_FACTOR_STEEL,
) -> bool:
    """Rod-part screening rule.

    Rods are parted when the static peak polished-rod load is below the
    calculated buoyant rod-string weight (steel ~0.87 x air weight in
    fresh water); above it, they are not.
    """
    return static_peak_load_lbs < rod_weight_air_lbs * buoyancy_factor
