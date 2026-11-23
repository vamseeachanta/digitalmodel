# ABOUTME: Real-time drilling dysfunction detector — stick-slip, washout, bit balling, kick
# ABOUTME: Accepts time-series DataFrame; returns list of DysfunctionEvent dataclasses

"""
Drilling Dysfunction Detector
==============================

Analyses a time-series window of surface drilling parameters and flags
active dysfunctions with severity, start time, and recommended mitigation.

Supported dysfunction types
---------------------------
- STICK_SLIP       : torque CV (std/mean) > threshold sustained over N windows
- WASHOUT          : unexplained ROP increase with constant WOB/RPM
- BIT_BALLING      : MSE > 2x baseline at constant WOB/RPM
- WELLBORE_INSTABILITY : torque spike + string weight increase (combo flag)
- KICK             : flow_out − flow_in > threshold OR pit gain > threshold

Inputs (DataFrame columns, all required)
-----------------------------------------
time_s, wob, rpm, torque, rop, flow_in, flow_out, pit_volume, string_weight

Dependencies
------------
pandas (required). rop_models / hydraulics imported with graceful ImportError fallback.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from enum import Enum
from typing import Optional

import pandas as pd

# ---------------------------------------------------------------------------
# Optional sibling imports — graceful stub fallback
# ---------------------------------------------------------------------------

try:
    from digitalmodel.well.drilling.rop_models import WarrenROP  # noqa: F401
    _HAS_ROP_MODELS = True
except ImportError:
    _HAS_ROP_MODELS = False

try:
    from digitalmodel.well.drilling.hydraulics import WellboreHydraulics  # noqa: F401
    _HAS_HYDRAULICS = True
except ImportError:
    _HAS_HYDRAULICS = False


# ---------------------------------------------------------------------------
# Public types
# ---------------------------------------------------------------------------

class DysfunctionType(str, Enum):
    STICK_SLIP = "stick_slip"
    WASHOUT = "washout"
    BIT_BALLING = "bit_balling"
    WELLBORE_INSTABILITY = "wellbore_instability"
    KICK = "kick"


@dataclass
class DysfunctionEvent:
    """
    A detected drilling dysfunction event.

    Fields
    ------
    dysfunction_type : DysfunctionType enum member
    severity         : 'low' | 'medium' | 'high'
    start_time       : earliest time_s value at which the flag was raised
    recommendation   : short human-readable mitigation string
    confidence       : float in [0, 1] — detection confidence (default 1.0)
    """

    dysfunction_type: DysfunctionType
    severity: str
    start_time: float
    recommendation: str
    confidence: float = field(default=1.0)

    def __post_init__(self) -> None:
        if self.severity not in ("low", "medium", "high"):
            raise ValueError(f"severity must be 'low', 'medium', or 'high', got {self.severity!r}")
        if not 0.0 <= self.confidence <= 1.0:
            raise ValueError(f"confidence must be in [0, 1], got {self.confidence}")


# ---------------------------------------------------------------------------
# Required columns
# ---------------------------------------------------------------------------

_REQUIRED_COLS = [
    "time_s", "wob", "rpm", "torque", "rop",
    "flow_in", "flow_out", "pit_volume", "string_weight",
]


# ---------------------------------------------------------------------------
# Detector
# ---------------------------------------------------------------------------

class DysfunctionDetector:
    """
    Stateless drilling dysfunction detector.

    Accepts a rolling time-series window as a pandas DataFrame and returns
    a list of DysfunctionEvent instances for any active dysfunctions.

    All thresholds are configurable via constructor arguments; defaults
    follow industry conventions.

    Parameters
    ----------
    stick_slip_window_s : rolling window size in seconds for torque CV (default 30)
    stick_slip_threshold : CV threshold — std(torque)/mean(torque) (default 0.20)
    stick_slip_consecutive_windows : number of consecutive windows that must exceed
        threshold before flagging (default 3)
    washout_rop_ratio : ratio actual_ROP/baseline_ROP that triggers washout (default 1.30)
    washout_param_tol : fractional tolerance for WOB/RPM "unchanged" check (default 0.05)
    bit_balling_mse_ratio : MSE / baseline_MSE ratio that triggers bit balling (default 2.0)
    bit_balling_baseline_s : initial seconds used to compute baseline MSE (default 60)
    kick_flow_diff_pct : (flow_out-flow_in)/flow_in threshold for kick (default 0.05)
    kick_pit_gain_pct : pit_volume gain / initial_pit_volume threshold (default 0.05)
    bit_dia_in : bit diameter in inches used for MSE calculation (default 8.5)
    """

    def __init__(
        self,
        stick_slip_window_s: int = 30,
        stick_slip_threshold: float = 0.20,
        stick_slip_consecutive_windows: int = 3,
        washout_rop_ratio: float = 1.30,
        washout_param_tol: float = 0.05,
        bit_balling_mse_ratio: float = 2.0,
        bit_balling_baseline_s: int = 60,
        kick_flow_diff_pct: float = 0.05,
        kick_pit_gain_pct: float = 0.05,
        bit_dia_in: float = 8.5,
    ) -> None:
        self.stick_slip_window_s = stick_slip_window_s
        self.stick_slip_threshold = stick_slip_threshold
        self.stick_slip_consecutive_windows = stick_slip_consecutive_windows
        self.washout_rop_ratio = washout_rop_ratio
        self.washout_param_tol = washout_param_tol
        self.bit_balling_mse_ratio = bit_balling_mse_ratio
        self.bit_balling_baseline_s = bit_balling_baseline_s
        self.kick_flow_diff_pct = kick_flow_diff_pct
        self.kick_pit_gain_pct = kick_pit_gain_pct
        self.bit_dia_in = bit_dia_in

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def detect(self, df: pd.DataFrame) -> list[DysfunctionEvent]:
        """
        Analyse a drilling time-series window and return all active dysfunction flags.

        Parameters
        ----------
        df : DataFrame with columns as defined in _REQUIRED_COLS.
             Rows sorted by time_s in ascending order.
             May be empty — returns [] in that case.

        Returns
        -------
        List of DysfunctionEvent (may be empty for normal drilling).
        """
        if df.empty:
            return []

        df = df.reset_index(drop=True)
        n = len(df)
        if n < 2:
            return []

        events: list[DysfunctionEvent] = []

        events.extend(self._detect_stick_slip(df))
        events.extend(self._detect_washout(df))
        events.extend(self._detect_bit_balling(df))
        events.extend(self._detect_wellbore_instability(df))
        events.extend(self._detect_kick(df))

        return events

    # ------------------------------------------------------------------
    # Internal detectors
    # ------------------------------------------------------------------

    def _detect_stick_slip(self, df: pd.DataFrame) -> list[DysfunctionEvent]:
        """
        Stick-slip: rolling std(torque)/mean(torque) > threshold for
        stick_slip_consecutive_windows consecutive non-overlapping windows.
        """
        w = self.stick_slip_window_s
        n = len(df)
        if n < w:
            return []

        torque = df["torque"].to_numpy()
        num_windows = n // w
        if num_windows < self.stick_slip_consecutive_windows:
            return []

        cv_exceeds = []
        for k in range(num_windows):
            segment = torque[k * w : (k + 1) * w]
            mean_t = float(segment.mean()) if hasattr(segment, "mean") else _mean(segment)
            if mean_t == 0.0:
                cv_exceeds.append(False)
                continue
            std_t = _std(segment)
            cv = std_t / abs(mean_t)
            cv_exceeds.append(cv > self.stick_slip_threshold)

        # Find first streak of N consecutive True values
        streak = 0
        first_window = -1
        for idx, flag in enumerate(cv_exceeds):
            if flag:
                streak += 1
                if streak >= self.stick_slip_consecutive_windows:
                    first_window = idx - self.stick_slip_consecutive_windows + 1
                    break
            else:
                streak = 0

        if first_window < 0:
            return []

        start_time = float(df["time_s"].iloc[first_window * w])

        # Severity based on max CV in the flagged windows
        max_cv = max(
            _std(torque[k * w : (k + 1) * w]) / max(abs(_mean(torque[k * w : (k + 1) * w])), 1e-9)
            for k in range(first_window, first_window + self.stick_slip_consecutive_windows)
        )
        severity = _cv_to_severity(max_cv, self.stick_slip_threshold)
        confidence = min(1.0, max_cv / self.stick_slip_threshold)

        return [
            DysfunctionEvent(
                dysfunction_type=DysfunctionType.STICK_SLIP,
                severity=severity,
                start_time=start_time,
                recommendation=(
                    "Reduce WOB by 20-30%, increase RPM 10-20% to break stick-slip cycle. "
                    "Consider surface agitator or downhole vibration tools."
                ),
                confidence=min(1.0, confidence),
            )
        ]

    def _detect_washout(self, df: pd.DataFrame) -> list[DysfunctionEvent]:
        """
        Washout: actual ROP > washout_rop_ratio × rolling baseline ROP,
        while WOB and RPM remain within washout_param_tol of their baseline values.

        Baseline is the mean of the first half of the window (or first 60 rows).
        """
        n = len(df)
        baseline_end = min(n // 2, 60)
        if baseline_end < 5:
            return []

        baseline_wob = float(df["wob"].iloc[:baseline_end].mean())
        baseline_rpm = float(df["rpm"].iloc[:baseline_end].mean())
        baseline_rop = float(df["rop"].iloc[:baseline_end].mean())

        if baseline_rop <= 0.0:
            return []

        events: list[DysfunctionEvent] = []
        for i in range(baseline_end, n):
            wob_i = float(df["wob"].iloc[i])
            rpm_i = float(df["rpm"].iloc[i])
            rop_i = float(df["rop"].iloc[i])

            wob_ok = abs(wob_i - baseline_wob) <= self.washout_param_tol * max(baseline_wob, 1e-9)
            rpm_ok = abs(rpm_i - baseline_rpm) <= self.washout_param_tol * max(baseline_rpm, 1e-9)

            if wob_ok and rpm_ok:
                ratio = rop_i / baseline_rop
                if ratio > self.washout_rop_ratio:
                    start_time = float(df["time_s"].iloc[i])
                    excess = ratio - 1.0
                    severity = "high" if excess > 0.5 else ("medium" if excess > 0.3 else "low")
                    events.append(
                        DysfunctionEvent(
                            dysfunction_type=DysfunctionType.WASHOUT,
                            severity=severity,
                            start_time=start_time,
                            recommendation=(
                                "Suspect bit washout. Monitor standpipe pressure for drop. "
                                "Pull out and inspect bit nozzles if pressure loss confirmed."
                            ),
                            confidence=min(1.0, (ratio - 1.0) / (self.washout_rop_ratio - 1.0)),
                        )
                    )
                    break  # report first occurrence only

        return events

    def _detect_bit_balling(self, df: pd.DataFrame) -> list[DysfunctionEvent]:
        """
        Bit balling: MSE > bit_balling_mse_ratio × baseline MSE,
        with WOB and RPM constant (within washout_param_tol).

        MSE = WOB/A_bit + (2*pi*RPM*Torque) / (A_bit * ROP)
        A_bit = pi/4 * d_bit^2
        """
        n = len(df)
        baseline_end = min(self.bit_balling_baseline_s, n // 2)
        if baseline_end < 5:
            return []

        a_bit = math.pi / 4.0 * self.bit_dia_in ** 2

        def mse_row(wob: float, rpm: float, torque: float, rop: float) -> Optional[float]:
            if rop <= 0.0 or a_bit <= 0.0:
                return None
            # Convert ROP from ft/hr to ft/min for dimensional consistency
            # Both terms use same A_bit units — result is in psi (or same relative units)
            rop_ft_min = rop / 60.0
            if rop_ft_min <= 0.0:
                return None
            term1 = wob / a_bit
            term2 = (2.0 * math.pi * rpm * torque) / (a_bit * rop_ft_min)
            return term1 + term2

        baseline_mses = []
        for i in range(baseline_end):
            m = mse_row(
                float(df["wob"].iloc[i]),
                float(df["rpm"].iloc[i]),
                float(df["torque"].iloc[i]),
                float(df["rop"].iloc[i]),
            )
            if m is not None:
                baseline_mses.append(m)

        if not baseline_mses:
            return []

        baseline_mse = _mean(baseline_mses)
        if baseline_mse <= 0.0:
            return []

        baseline_wob = float(df["wob"].iloc[:baseline_end].mean())
        baseline_rpm = float(df["rpm"].iloc[:baseline_end].mean())

        events: list[DysfunctionEvent] = []
        for i in range(baseline_end, n):
            wob_i = float(df["wob"].iloc[i])
            rpm_i = float(df["rpm"].iloc[i])

            wob_ok = abs(wob_i - baseline_wob) <= self.washout_param_tol * max(baseline_wob, 1e-9)
            rpm_ok = abs(rpm_i - baseline_rpm) <= self.washout_param_tol * max(baseline_rpm, 1e-9)

            if not (wob_ok and rpm_ok):
                continue

            m = mse_row(
                wob_i,
                rpm_i,
                float(df["torque"].iloc[i]),
                float(df["rop"].iloc[i]),
            )
            if m is None:
                continue

            ratio = m / baseline_mse
            if ratio > self.bit_balling_mse_ratio:
                start_time = float(df["time_s"].iloc[i])
                severity = "high" if ratio > 3.0 else ("medium" if ratio > 2.5 else "low")
                events.append(
                    DysfunctionEvent(
                        dysfunction_type=DysfunctionType.BIT_BALLING,
                        severity=severity,
                        start_time=start_time,
                        recommendation=(
                            "Bit balling suspected. Increase flow rate and RPM to clean bit. "
                            "Reduce WOB and consider pill circulation or bit trip."
                        ),
                        confidence=min(1.0, (ratio - 1.0) / (self.bit_balling_mse_ratio - 1.0)),
                    )
                )
                break

        return events

    def _detect_wellbore_instability(self, df: pd.DataFrame) -> list[DysfunctionEvent]:
        """
        Wellbore instability: torque spike AND string weight increase simultaneously.

        Torque spike: instantaneous torque > 1.3 × rolling mean of first half
        String weight increase: > 5% above rolling mean of first half
        Both conditions must be true in the same row.
        """
        n = len(df)
        baseline_end = min(n // 2, 60)
        if baseline_end < 5:
            return []

        baseline_torque = float(df["torque"].iloc[:baseline_end].mean())
        baseline_weight = float(df["string_weight"].iloc[:baseline_end].mean())

        torque_spike_threshold = 1.30 * baseline_torque
        weight_increase_threshold = 1.05 * max(baseline_weight, 1e-9)

        events: list[DysfunctionEvent] = []
        for i in range(baseline_end, n):
            torque_i = float(df["torque"].iloc[i])
            weight_i = float(df["string_weight"].iloc[i])

            torque_spike = torque_i > torque_spike_threshold
            weight_increased = weight_i > weight_increase_threshold

            if torque_spike and weight_increased:
                start_time = float(df["time_s"].iloc[i])
                torque_ratio = torque_i / baseline_torque
                severity = "high" if torque_ratio > 1.6 else "medium"
                events.append(
                    DysfunctionEvent(
                        dysfunction_type=DysfunctionType.WELLBORE_INSTABILITY,
                        severity=severity,
                        start_time=start_time,
                        recommendation=(
                            "Wellbore instability detected. Reduce WOB and ROP. "
                            "Check ECD and consider increasing mud weight if underbalanced. "
                            "Ream slowly if packoff risk."
                        ),
                        confidence=min(1.0, (torque_ratio - 1.0) / 0.5),
                    )
                )
                break

        return events

    def _detect_kick(self, df: pd.DataFrame) -> list[DysfunctionEvent]:
        """
        Kick: flow_out − flow_in > kick_flow_diff_pct × flow_in
              OR pit_volume gain > kick_pit_gain_pct × initial pit_volume.

        Both conditions are checked independently; either triggers a kick flag.
        """
        n = len(df)
        if n < 2:
            return []

        initial_pit = float(df["pit_volume"].iloc[0])
        if initial_pit <= 0.0:
            initial_pit = 1.0  # avoid division by zero

        events: list[DysfunctionEvent] = []
        flag_time: Optional[float] = None

        for i in range(1, n):
            flow_in_i = float(df["flow_in"].iloc[i])
            flow_out_i = float(df["flow_out"].iloc[i])
            pit_i = float(df["pit_volume"].iloc[i])

            flow_in_ref = max(abs(flow_in_i), 1.0)
            flow_diff_ratio = (flow_out_i - flow_in_i) / flow_in_ref
            pit_gain_ratio = (pit_i - initial_pit) / initial_pit

            kick_by_flow = flow_diff_ratio > self.kick_flow_diff_pct
            kick_by_pit = pit_gain_ratio > self.kick_pit_gain_pct

            if kick_by_flow or kick_by_pit:
                flag_time = float(df["time_s"].iloc[i])
                break

        if flag_time is None:
            return []

        events.append(
            DysfunctionEvent(
                dysfunction_type=DysfunctionType.KICK,
                severity="high",
                start_time=flag_time,
                recommendation=(
                    "Kick detected — possible gas influx. Shut in well immediately (close BOP). "
                    "Activate well control procedure. Notify well control supervisor."
                ),
                confidence=1.0,
            )
        )
        return events


# ---------------------------------------------------------------------------
# Private helpers
# ---------------------------------------------------------------------------

def _mean(values: list[float]) -> float:
    """Return arithmetic mean of a list or numpy array slice."""
    if hasattr(values, "mean"):
        return float(values.mean())
    n = len(values)
    if n == 0:
        return 0.0
    return sum(values) / n


def _std(values: list[float]) -> float:
    """Return population standard deviation of a list or numpy array slice."""
    if hasattr(values, "std"):
        return float(values.std())
    n = len(values)
    if n == 0:
        return 0.0
    mu = _mean(values)
    variance = sum((x - mu) ** 2 for x in values) / n
    return math.sqrt(variance)


def _cv_to_severity(cv: float, threshold: float) -> str:
    """Map coefficient of variation ratio to severity label."""
    ratio = cv / threshold
    if ratio >= 2.0:
        return "high"
    if ratio >= 1.5:
        return "medium"
    return "low"
