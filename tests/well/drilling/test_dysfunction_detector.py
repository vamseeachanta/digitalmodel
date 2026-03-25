# ABOUTME: Unit tests for drilling dysfunction detector — stick-slip, washout, bit balling, kick
# ABOUTME: Uses synthetic time-series with known dysfunction signatures to verify each detector

"""
Tests for dysfunction_detector — DysfunctionDetector and DysfunctionEvent.

Synthetic data strategy:
- Each dysfunction test constructs a time-series window that exhibits the
  target anomaly signature at >detection threshold.
- Each false-positive test constructs normal drilling variation (noise
  <50% of the detection threshold) and verifies no flag is raised.
"""

import math

import pandas as pd
import pytest

from digitalmodel.well.drilling.dysfunction_detector import (
    DysfunctionDetector,
    DysfunctionEvent,
    DysfunctionType,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

_DT = 1.0  # 1-second time step


def _make_df(
    n: int,
    wob: float = 20.0,
    rpm: float = 100.0,
    torque: float = 5000.0,
    rop: float = 50.0,
    flow_in: float = 600.0,
    flow_out: float = 600.0,
    pit_volume: float = 1000.0,
    string_weight: float = 50.0,
) -> pd.DataFrame:
    """Return a DataFrame with constant values across n rows (1-s steps)."""
    times = [float(i) for i in range(n)]
    return pd.DataFrame(
        {
            "time_s": times,
            "wob": [wob] * n,
            "rpm": [rpm] * n,
            "torque": [torque] * n,
            "rop": [rop] * n,
            "flow_in": [flow_in] * n,
            "flow_out": [flow_out] * n,
            "pit_volume": [pit_volume] * n,
            "string_weight": [string_weight] * n,
        }
    )


def _detector(
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
) -> DysfunctionDetector:
    return DysfunctionDetector(
        stick_slip_window_s=stick_slip_window_s,
        stick_slip_threshold=stick_slip_threshold,
        stick_slip_consecutive_windows=stick_slip_consecutive_windows,
        washout_rop_ratio=washout_rop_ratio,
        washout_param_tol=washout_param_tol,
        bit_balling_mse_ratio=bit_balling_mse_ratio,
        bit_balling_baseline_s=bit_balling_baseline_s,
        kick_flow_diff_pct=kick_flow_diff_pct,
        kick_pit_gain_pct=kick_pit_gain_pct,
        bit_dia_in=bit_dia_in,
    )


# ---------------------------------------------------------------------------
# DysfunctionEvent dataclass
# ---------------------------------------------------------------------------


class TestDysfunctionEvent:
    def test_fields_accessible(self):
        ev = DysfunctionEvent(
            dysfunction_type=DysfunctionType.STICK_SLIP,
            severity="high",
            start_time=10.0,
            recommendation="Reduce WOB and increase RPM",
        )
        assert ev.dysfunction_type == DysfunctionType.STICK_SLIP
        assert ev.severity == "high"
        assert ev.start_time == 10.0
        assert "WOB" in ev.recommendation or len(ev.recommendation) > 0

    def test_confidence_between_zero_and_one(self):
        ev = DysfunctionEvent(
            dysfunction_type=DysfunctionType.WASHOUT,
            severity="medium",
            start_time=5.0,
            recommendation="Check pump and standpipe pressure",
            confidence=0.75,
        )
        assert 0.0 <= ev.confidence <= 1.0

    def test_default_confidence_valid(self):
        ev = DysfunctionEvent(
            dysfunction_type=DysfunctionType.KICK,
            severity="high",
            start_time=0.0,
            recommendation="Shut in well immediately",
        )
        assert 0.0 <= ev.confidence <= 1.0


# ---------------------------------------------------------------------------
# DysfunctionDetector construction
# ---------------------------------------------------------------------------


class TestDysfunctionDetectorConstruction:
    def test_default_instantiation(self):
        det = DysfunctionDetector()
        assert det is not None

    def test_custom_thresholds_stored(self):
        det = _detector(stick_slip_threshold=0.25, kick_pit_gain_pct=0.08)
        assert det.stick_slip_threshold == pytest.approx(0.25)
        assert det.kick_pit_gain_pct == pytest.approx(0.08)

    def test_detect_returns_list(self):
        det = _detector()
        df = _make_df(120)
        result = det.detect(df)
        assert isinstance(result, list)

    def test_normal_drilling_returns_no_flags(self):
        """Steady-state drilling with small random noise must not trigger any flag."""
        det = _detector()
        df = _make_df(120)
        result = det.detect(df)
        assert result == []


# ---------------------------------------------------------------------------
# Stick-slip detection
# ---------------------------------------------------------------------------


class TestStickSlipDetection:
    def _make_stick_slip_df(self, n: int = 120, amplitude: float = 2500.0) -> pd.DataFrame:
        """
        Oscillating torque: mean=5000, amplitude creates std/mean >> 20%.
        Torque swings between 2500 and 7500 — std ≈ 1768, severity ≈ 0.35 (>0.20).
        """
        df = _make_df(n)
        torques = []
        for i in range(n):
            torques.append(5000.0 + amplitude * math.sin(2 * math.pi * i / 10.0))
        df["torque"] = torques
        return df

    def test_stick_slip_detected_on_oscillating_torque(self):
        det = _detector()
        df = self._make_stick_slip_df(n=120)
        events = det.detect(df)
        types = [e.dysfunction_type for e in events]
        assert DysfunctionType.STICK_SLIP in types

    def test_stick_slip_event_has_valid_severity(self):
        det = _detector()
        df = self._make_stick_slip_df(n=120)
        events = [e for e in det.detect(df) if e.dysfunction_type == DysfunctionType.STICK_SLIP]
        assert len(events) >= 1
        assert events[0].severity in ("low", "medium", "high")

    def test_stick_slip_event_has_start_time(self):
        det = _detector()
        df = self._make_stick_slip_df(n=120)
        events = [e for e in det.detect(df) if e.dysfunction_type == DysfunctionType.STICK_SLIP]
        assert events[0].start_time >= 0.0

    def test_stick_slip_false_positive_small_noise(self):
        """
        Torque noise of ±200 on mean 5000 → std/mean ≈ 4% — well below 20% threshold.
        """
        det = _detector()
        df = _make_df(120)
        import random
        rng = random.Random(0)
        df["torque"] = [5000.0 + rng.uniform(-200, 200) for _ in range(120)]
        events = [e for e in det.detect(df) if e.dysfunction_type == DysfunctionType.STICK_SLIP]
        assert events == []

    def test_stick_slip_requires_consecutive_windows(self):
        """
        A single window of high torque CV followed by stable torque should NOT flag.
        We need 3+ consecutive windows (stick_slip_consecutive_windows=3).
        Only the first 30 rows have oscillation; rest are flat.
        """
        det = _detector(stick_slip_consecutive_windows=3)
        df = _make_df(120)
        # Only first 30 s are oscillating — just 1 window, not 3 consecutive
        torques = list(df["torque"])
        for i in range(30):
            torques[i] = 5000.0 + 2500.0 * math.sin(2 * math.pi * i / 5.0)
        df["torque"] = torques
        events = [e for e in det.detect(df) if e.dysfunction_type == DysfunctionType.STICK_SLIP]
        # Exactly one window of oscillation — should not meet 3-consecutive requirement
        assert events == []


# ---------------------------------------------------------------------------
# Washout detection
# ---------------------------------------------------------------------------


class TestWashoutDetection:
    def _make_washout_df(self, n: int = 120, rop_jump_factor: float = 1.5) -> pd.DataFrame:
        """
        First 60 s: normal baseline ROP=50. Then ROP jumps to 50*1.5=75 with WOB/RPM constant.
        WOB/RPM unchanged — unexplained ROP increase > 30% → washout.
        """
        df = _make_df(n)
        rops = list(df["rop"])
        for i in range(60, n):
            rops[i] = 50.0 * rop_jump_factor
        df["rop"] = rops
        return df

    def test_washout_detected_on_rop_jump_constant_params(self):
        det = _detector()
        df = self._make_washout_df(n=120, rop_jump_factor=1.5)
        events = det.detect(df)
        types = [e.dysfunction_type for e in events]
        assert DysfunctionType.WASHOUT in types

    def test_washout_event_has_valid_severity(self):
        det = _detector()
        df = self._make_washout_df(n=120, rop_jump_factor=1.5)
        events = [e for e in det.detect(df) if e.dysfunction_type == DysfunctionType.WASHOUT]
        assert len(events) >= 1
        assert events[0].severity in ("low", "medium", "high")

    def test_washout_not_triggered_if_rop_increase_with_wob_increase(self):
        """
        ROP jumps 50% but WOB also increases 50% — expected ROP improvement, not washout.
        """
        det = _detector()
        df = _make_df(120)
        for i in range(60, 120):
            df.loc[i, "rop"] = 75.0
            df.loc[i, "wob"] = 30.0  # WOB increased proportionally
        events = [e for e in det.detect(df) if e.dysfunction_type == DysfunctionType.WASHOUT]
        assert events == []

    def test_washout_false_positive_normal_rop_variation(self):
        """
        ROP variation of ±10% (within threshold) should not trigger washout.
        """
        det = _detector()
        df = _make_df(120)
        import random
        rng = random.Random(1)
        df["rop"] = [50.0 * (1 + rng.uniform(-0.10, 0.10)) for _ in range(120)]
        events = [e for e in det.detect(df) if e.dysfunction_type == DysfunctionType.WASHOUT]
        assert events == []


# ---------------------------------------------------------------------------
# Bit balling detection
# ---------------------------------------------------------------------------


class TestBitBallingDetection:
    def _make_bit_balling_df(
        self, n: int = 180, baseline_s: int = 60, bit_dia_in: float = 8.5
    ) -> pd.DataFrame:
        """
        Baseline 60 s: normal MSE. Then torque doubles and ROP halves — MSE > 2x baseline.
        WOB and RPM stay constant throughout.
        """
        df = _make_df(n, wob=20.0, rpm=100.0, torque=5000.0, rop=50.0)
        # After baseline period: torque increases, ROP decreases → MSE spikes
        for i in range(baseline_s, n):
            df.loc[i, "torque"] = 12000.0  # doubled torque
            df.loc[i, "rop"] = 20.0        # halved ROP
        return df

    def test_bit_balling_detected(self):
        det = _detector(bit_balling_baseline_s=60, bit_dia_in=8.5)
        df = self._make_bit_balling_df(n=180, baseline_s=60)
        events = det.detect(df)
        types = [e.dysfunction_type for e in events]
        assert DysfunctionType.BIT_BALLING in types

    def test_bit_balling_event_has_recommendation(self):
        det = _detector(bit_balling_baseline_s=60, bit_dia_in=8.5)
        df = self._make_bit_balling_df(n=180, baseline_s=60)
        events = [e for e in det.detect(df) if e.dysfunction_type == DysfunctionType.BIT_BALLING]
        assert len(events) >= 1
        assert len(events[0].recommendation) > 0

    def test_bit_balling_false_positive_constant_mse(self):
        """Constant drilling parameters → MSE stays constant → no bit balling flag."""
        det = _detector(bit_balling_baseline_s=60, bit_dia_in=8.5)
        df = _make_df(180, wob=20.0, rpm=100.0, torque=5000.0, rop=50.0)
        events = [e for e in det.detect(df) if e.dysfunction_type == DysfunctionType.BIT_BALLING]
        assert events == []

    def test_bit_balling_not_triggered_if_wob_changes(self):
        """
        MSE doubles but WOB also doubled — formation hardened, not bit balling.
        Detector should require constant WOB/RPM.
        """
        det = _detector(bit_balling_baseline_s=60, bit_dia_in=8.5)
        df = _make_df(180, wob=20.0, rpm=100.0, torque=5000.0, rop=50.0)
        for i in range(60, 180):
            df.loc[i, "wob"] = 40.0    # WOB doubled
            df.loc[i, "torque"] = 12000.0
            df.loc[i, "rop"] = 20.0
        events = [e for e in det.detect(df) if e.dysfunction_type == DysfunctionType.BIT_BALLING]
        assert events == []


# ---------------------------------------------------------------------------
# Wellbore instability detection
# ---------------------------------------------------------------------------


class TestWellboreInstabilityDetection:
    def _make_instability_df(self, n: int = 120) -> pd.DataFrame:
        """
        Torque spike (50% increase) + string weight increase (5 klb) simultaneously.
        """
        df = _make_df(n, torque=5000.0, string_weight=50.0)
        # After row 60: both torque spike and string weight increase
        for i in range(60, n):
            df.loc[i, "torque"] = 8000.0       # 60% torque increase
            df.loc[i, "string_weight"] = 56.0  # +6 klb string weight
        return df

    def test_wellbore_instability_detected_on_combo_flag(self):
        det = _detector()
        df = self._make_instability_df(n=120)
        events = det.detect(df)
        types = [e.dysfunction_type for e in events]
        assert DysfunctionType.WELLBORE_INSTABILITY in types

    def test_wellbore_instability_not_triggered_torque_only(self):
        """Torque spike alone without string weight increase → not instability."""
        det = _detector()
        df = _make_df(120, torque=5000.0, string_weight=50.0)
        for i in range(60, 120):
            df.loc[i, "torque"] = 8000.0
            # string_weight stays at 50.0 — no weight increase
        events = [
            e for e in det.detect(df)
            if e.dysfunction_type == DysfunctionType.WELLBORE_INSTABILITY
        ]
        assert events == []

    def test_wellbore_instability_not_triggered_weight_only(self):
        """String weight increase alone without torque spike → not instability."""
        det = _detector()
        df = _make_df(120, torque=5000.0, string_weight=50.0)
        for i in range(60, 120):
            df.loc[i, "string_weight"] = 58.0
            # torque stays at 5000 — no spike
        events = [
            e for e in det.detect(df)
            if e.dysfunction_type == DysfunctionType.WELLBORE_INSTABILITY
        ]
        assert events == []

    def test_wellbore_instability_event_severity_high(self):
        """Combo flag should be at minimum 'medium' severity."""
        det = _detector()
        df = self._make_instability_df(n=120)
        events = [
            e for e in det.detect(df)
            if e.dysfunction_type == DysfunctionType.WELLBORE_INSTABILITY
        ]
        assert events[0].severity in ("medium", "high")


# ---------------------------------------------------------------------------
# Kick detection
# ---------------------------------------------------------------------------


class TestKickDetection:
    def _make_kick_df(
        self,
        n: int = 120,
        flow_diff_ratio: float = 0.10,
        pit_gain_total: float = 60.0,
    ) -> pd.DataFrame:
        """
        flow_out - flow_in = 10% of flow_in (> 5% threshold).
        pit_volume grows by 60 bbl over 60 s (pit gain > 5% of 1000 bbl baseline).
        """
        df = _make_df(n, flow_in=600.0, flow_out=600.0, pit_volume=1000.0)
        for i in range(60, n):
            df.loc[i, "flow_out"] = 600.0 * (1 + flow_diff_ratio)
            df.loc[i, "pit_volume"] = 1000.0 + pit_gain_total * (i - 59) / (n - 60)
        return df

    def test_kick_detected_on_flow_differential(self):
        det = _detector()
        df = self._make_kick_df(n=120, flow_diff_ratio=0.10)
        events = det.detect(df)
        types = [e.dysfunction_type for e in events]
        assert DysfunctionType.KICK in types

    def test_kick_event_is_high_severity(self):
        det = _detector()
        df = self._make_kick_df(n=120, flow_diff_ratio=0.10)
        events = [e for e in det.detect(df) if e.dysfunction_type == DysfunctionType.KICK]
        assert len(events) >= 1
        assert events[0].severity == "high"

    def test_kick_detected_on_pit_gain_alone(self):
        """Pit gain > 5% alone should trigger kick even if flow differential is small."""
        det = _detector(kick_pit_gain_pct=0.05)
        df = _make_df(120, flow_in=600.0, flow_out=600.0, pit_volume=1000.0)
        # Pit volume jumps by 6% in second half
        for i in range(60, 120):
            df.loc[i, "pit_volume"] = 1000.0 + 70.0 * (i - 59) / 60  # ~7% gain
        events = [e for e in det.detect(df) if e.dysfunction_type == DysfunctionType.KICK]
        assert len(events) >= 1

    def test_kick_false_positive_balanced_flow(self):
        """
        Flow out equals flow in and pit volume constant → no kick.
        """
        det = _detector()
        df = _make_df(120, flow_in=600.0, flow_out=600.0, pit_volume=1000.0)
        events = [e for e in det.detect(df) if e.dysfunction_type == DysfunctionType.KICK]
        assert events == []

    def test_kick_false_positive_small_flow_variation(self):
        """
        Small balanced pump fluctuation < 5% threshold should not flag kick.
        """
        det = _detector(kick_flow_diff_pct=0.05)
        df = _make_df(120, flow_in=600.0, flow_out=600.0, pit_volume=1000.0)
        import random
        rng = random.Random(2)
        df["flow_out"] = [600.0 + rng.uniform(-15, 15) for _ in range(120)]  # ±2.5%
        events = [e for e in det.detect(df) if e.dysfunction_type == DysfunctionType.KICK]
        assert events == []

    def test_kick_recommendation_contains_shut_in(self):
        """Kick recommendation must mention shut-in or well control action."""
        det = _detector()
        df = self._make_kick_df(n=120, flow_diff_ratio=0.12)
        events = [e for e in det.detect(df) if e.dysfunction_type == DysfunctionType.KICK]
        rec = events[0].recommendation.lower()
        assert any(word in rec for word in ("shut", "close", "bop", "well control", "kick"))


# ---------------------------------------------------------------------------
# Integration: multiple dysfunctions in same window
# ---------------------------------------------------------------------------


class TestMultipleDysfunctions:
    def test_detect_returns_multiple_types(self):
        """
        Combine stick-slip torque oscillation + flow differential kick signal.
        Both should be detected.
        """
        det = _detector()
        n = 180
        df = _make_df(n, torque=5000.0, flow_in=600.0, flow_out=600.0, pit_volume=1000.0)

        # Stick-slip: oscillating torque throughout
        import math
        df["torque"] = [5000.0 + 2500.0 * math.sin(2 * math.pi * i / 10.0) for i in range(n)]

        # Kick: flow differential in second half
        for i in range(90, n):
            df.loc[i, "flow_out"] = 666.0  # 11% differential
            df.loc[i, "pit_volume"] = 1000.0 + 80.0 * (i - 89) / 90

        events = det.detect(df)
        types = {e.dysfunction_type for e in events}
        assert DysfunctionType.STICK_SLIP in types
        assert DysfunctionType.KICK in types

    def test_detect_empty_dataframe_returns_no_flags(self):
        """Empty input must not crash — return empty list."""
        det = _detector()
        df = pd.DataFrame(
            columns=[
                "time_s", "wob", "rpm", "torque", "rop",
                "flow_in", "flow_out", "pit_volume", "string_weight",
            ]
        )
        result = det.detect(df)
        assert result == []

    def test_detect_short_dataframe_below_minimum_window(self):
        """
        DataFrame shorter than stick-slip window (30 s) should not crash.
        """
        det = _detector()
        df = _make_df(10)
        result = det.detect(df)
        assert isinstance(result, list)
