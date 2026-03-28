"""Tests for real-time feedback data models and state machine."""
from __future__ import annotations

import numpy as np
import pytest
from datetime import datetime, timedelta

from digitalmodel.marine_ops.installation.realtime_feedback import (
    CriteriaExceedance,
    InstallationStateMachine,
    MRUBuffer,
    MRUReading,
)
from digitalmodel.marine_ops.installation.models import (
    GoNoGoState,
    InstallationCriteria,
    InstallationPhase,
)


def _generate_readings(n, heave_amp, roll_amp, pitch_amp, dt_s=1.0):
    """Generate n synthetic MRU readings."""
    t0 = datetime(2026, 3, 27, 10, 0, 0)
    readings = []
    for i in range(n):
        readings.append(MRUReading(
            timestamp=t0 + timedelta(seconds=i * dt_s),
            heave_m=heave_amp * np.sin(0.5 * i),
            roll_deg=roll_amp * np.sin(0.3 * i),
            pitch_deg=pitch_amp * np.sin(0.4 * i),
        ))
    return readings


class TestMRUBuffer:
    """Rolling buffer for MRU readings."""

    def test_add_and_trim(self):
        buf = MRUBuffer(window_seconds=10.0)
        readings = _generate_readings(20, 0.1, 0.5, 0.3, dt_s=1.0)
        for r in readings:
            buf.add(r)
        # Only last 10 seconds should remain
        assert len(buf.readings) <= 11

    def test_significant_heave_calm(self):
        buf = MRUBuffer(window_seconds=120.0)
        for r in _generate_readings(50, heave_amp=0.1, roll_amp=0.5, pitch_amp=0.3):
            buf.add(r)
        sig = buf.significant_heave()
        # 4 * std of 0.1*sin -> small value
        assert sig < 0.5

    def test_significant_heave_rough(self):
        buf = MRUBuffer(window_seconds=120.0)
        for r in _generate_readings(50, heave_amp=2.0, roll_amp=5.0, pitch_amp=4.0):
            buf.add(r)
        sig = buf.significant_heave()
        assert sig > 1.0

    def test_max_roll(self):
        buf = MRUBuffer(window_seconds=120.0)
        for r in _generate_readings(50, heave_amp=0.1, roll_amp=3.0, pitch_amp=0.3):
            buf.add(r)
        assert buf.max_roll() <= 3.0
        assert buf.max_roll() > 0

    def test_max_pitch(self):
        buf = MRUBuffer(window_seconds=120.0)
        for r in _generate_readings(50, heave_amp=0.1, roll_amp=0.5, pitch_amp=4.0):
            buf.add(r)
        assert buf.max_pitch() <= 4.0
        assert buf.max_pitch() > 0

    def test_empty_buffer_returns_zero(self):
        buf = MRUBuffer()
        assert buf.significant_heave() == 0.0
        assert buf.max_roll() == 0.0
        assert buf.max_pitch() == 0.0

    def test_few_readings_returns_zero_heave(self):
        buf = MRUBuffer()
        for r in _generate_readings(5, 1.0, 1.0, 1.0):
            buf.add(r)
        # <10 readings → returns 0
        assert buf.significant_heave() == 0.0


class TestInstallationStateMachine:
    """Go/no-go state machine."""

    def test_calm_gives_go(self):
        sm = InstallationStateMachine(
            criteria=InstallationCriteria(max_crane_tip_heave_m=2.0, max_tilt_deg=5.0)
        )
        buf = MRUBuffer(window_seconds=120.0)
        for r in _generate_readings(50, heave_amp=0.1, roll_amp=0.5, pitch_amp=0.3):
            buf.add(r)
        state = sm.check_criteria(buf)
        assert state == GoNoGoState.GO

    def test_rough_gives_no_go(self):
        sm = InstallationStateMachine(
            criteria=InstallationCriteria(max_crane_tip_heave_m=1.0, max_tilt_deg=3.0)
        )
        buf = MRUBuffer(window_seconds=120.0)
        for r in _generate_readings(50, heave_amp=2.0, roll_amp=5.0, pitch_amp=4.0):
            buf.add(r)
        state = sm.check_criteria(buf)
        assert state == GoNoGoState.NO_GO

    def test_marginal_near_threshold(self):
        sm = InstallationStateMachine(
            criteria=InstallationCriteria(max_crane_tip_heave_m=5.0, max_tilt_deg=10.0)
        )
        buf = MRUBuffer(window_seconds=120.0)
        # Heave that puts sig_heave at ~80-100% of threshold
        for r in _generate_readings(50, heave_amp=1.0, roll_amp=0.5, pitch_amp=0.3):
            buf.add(r)
        state = sm.check_criteria(buf)
        # Should be either GO or MARGINAL (depending on exact std)
        assert state in (GoNoGoState.GO, GoNoGoState.MARGINAL)

    def test_exceedances_recorded(self):
        sm = InstallationStateMachine(
            criteria=InstallationCriteria(max_crane_tip_heave_m=0.5, max_tilt_deg=1.0)
        )
        buf = MRUBuffer(window_seconds=120.0)
        for r in _generate_readings(50, heave_amp=2.0, roll_amp=5.0, pitch_amp=4.0):
            buf.add(r)
        sm.check_criteria(buf)
        assert len(sm.exceedances) > 0
        assert all(isinstance(e, CriteriaExceedance) for e in sm.exceedances)

    def test_state_history_tracked(self):
        """Transition from GO to NO_GO should be logged."""
        sm = InstallationStateMachine(
            criteria=InstallationCriteria(max_crane_tip_heave_m=1.0, max_tilt_deg=3.0)
        )
        # First: calm → GO (transitions from initial NO_GO)
        buf_calm = MRUBuffer(window_seconds=120.0)
        for r in _generate_readings(50, heave_amp=0.05, roll_amp=0.1, pitch_amp=0.1):
            buf_calm.add(r)
        sm.check_criteria(buf_calm)
        # Then: rough → NO_GO
        buf_rough = MRUBuffer(window_seconds=120.0)
        for r in _generate_readings(50, heave_amp=2.0, roll_amp=5.0, pitch_amp=4.0):
            buf_rough.add(r)
        sm.check_criteria(buf_rough)
        # Should have at least 2 transitions: NO_GO→GO, GO→NO_GO
        assert len(sm.state_history) >= 2

    def test_advance_phase(self):
        sm = InstallationStateMachine()
        assert sm.current_phase == InstallationPhase.LIFT_OFF
        sm.advance_phase(InstallationPhase.SPLASH_ZONE)
        assert sm.current_phase == InstallationPhase.SPLASH_ZONE
        sm.advance_phase(InstallationPhase.LOWERING)
        assert sm.current_phase == InstallationPhase.LOWERING

    def test_phase_transitions_logged(self):
        sm = InstallationStateMachine()
        sm.advance_phase(InstallationPhase.IN_AIR)
        sm.advance_phase(InstallationPhase.SPLASH_ZONE)
        assert len(sm.state_history) == 2
