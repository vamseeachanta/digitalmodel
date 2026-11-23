"""Real-time motion feedback data models (Phase 3 — design only).

Defines the data model and criteria checker for real-time MRU data
ingestion and go/no-go decision making during installation operations.
Dashboard UI is deferred — this module provides the computation layer.

References:
    DNV-ST-N001 (2021) -- Marine Operations and Marine Warranty
"""
from __future__ import annotations

import numpy as np
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from .models import GoNoGoState, InstallationPhase, InstallationCriteria


@dataclass
class MRUReading:
    """Single MRU/IMU measurement sample.

    Attributes
    ----------
    timestamp : datetime
        Measurement time.
    heave_m : float
        Heave displacement [m].
    roll_deg : float
        Roll angle [deg].
    pitch_deg : float
        Pitch angle [deg].
    surge_m : float
        Surge displacement [m].
    sway_m : float
        Sway displacement [m].
    yaw_deg : float
        Yaw angle [deg].
    """

    timestamp: datetime
    heave_m: float = 0.0
    roll_deg: float = 0.0
    pitch_deg: float = 0.0
    surge_m: float = 0.0
    sway_m: float = 0.0
    yaw_deg: float = 0.0


@dataclass
class MRUBuffer:
    """Rolling buffer of MRU readings for statistical processing.

    Attributes
    ----------
    window_seconds : float
        Duration of the rolling window [s].
    readings : list[MRUReading]
        Buffered readings within the window.
    """

    window_seconds: float = 1200.0  # 20 minutes default
    readings: list[MRUReading] = field(default_factory=list)

    def add(self, reading: MRUReading) -> None:
        """Add a reading and trim old data outside the window."""
        self.readings.append(reading)
        cutoff = reading.timestamp.timestamp() - self.window_seconds
        self.readings = [
            r for r in self.readings
            if r.timestamp.timestamp() >= cutoff
        ]

    def significant_heave(self) -> float:
        """Compute significant heave from buffer (4 * std)."""
        if len(self.readings) < 10:
            return 0.0
        heaves = np.array([r.heave_m for r in self.readings])
        return 4.0 * np.std(heaves)

    def max_roll(self) -> float:
        """Maximum roll in buffer [deg]."""
        if not self.readings:
            return 0.0
        return max(abs(r.roll_deg) for r in self.readings)

    def max_pitch(self) -> float:
        """Maximum pitch in buffer [deg]."""
        if not self.readings:
            return 0.0
        return max(abs(r.pitch_deg) for r in self.readings)


@dataclass
class CriteriaExceedance:
    """Record of a criteria exceedance event."""

    timestamp: datetime
    criterion: str
    measured_value: float
    allowable_value: float
    phase: InstallationPhase


@dataclass
class InstallationStateMachine:
    """Go/no-go state machine for installation operations.

    Tracks the current installation phase and decision state,
    with history of state transitions and exceedances.
    """

    current_phase: InstallationPhase = InstallationPhase.LIFT_OFF
    current_state: GoNoGoState = GoNoGoState.NO_GO
    criteria: InstallationCriteria = field(default_factory=InstallationCriteria)
    exceedances: list[CriteriaExceedance] = field(default_factory=list)
    state_history: list[tuple[datetime, GoNoGoState, InstallationPhase]] = field(
        default_factory=list
    )

    def check_criteria(self, buffer: MRUBuffer) -> GoNoGoState:
        """Evaluate current motions against criteria.

        Parameters
        ----------
        buffer : MRUBuffer
            Rolling buffer of MRU measurements.

        Returns
        -------
        GoNoGoState
            Current go/no-go state.
        """
        now = datetime.now()
        sig_heave = buffer.significant_heave()
        max_roll = buffer.max_roll()
        max_pitch = buffer.max_pitch()

        exceedance_found = False

        if sig_heave > self.criteria.max_crane_tip_heave_m:
            self.exceedances.append(CriteriaExceedance(
                timestamp=now,
                criterion="significant_heave",
                measured_value=sig_heave,
                allowable_value=self.criteria.max_crane_tip_heave_m,
                phase=self.current_phase,
            ))
            exceedance_found = True

        max_tilt = max(max_roll, max_pitch)
        if max_tilt > self.criteria.max_tilt_deg:
            self.exceedances.append(CriteriaExceedance(
                timestamp=now,
                criterion="tilt",
                measured_value=max_tilt,
                allowable_value=self.criteria.max_tilt_deg,
                phase=self.current_phase,
            ))
            exceedance_found = True

        if exceedance_found:
            new_state = GoNoGoState.NO_GO
        elif sig_heave > 0.8 * self.criteria.max_crane_tip_heave_m:
            new_state = GoNoGoState.MARGINAL
        else:
            new_state = GoNoGoState.GO

        if new_state != self.current_state:
            self.state_history.append((now, new_state, self.current_phase))
            self.current_state = new_state

        return new_state

    def advance_phase(self, next_phase: InstallationPhase) -> None:
        """Advance to the next installation phase."""
        self.current_phase = next_phase
        self.state_history.append(
            (datetime.now(), self.current_state, next_phase)
        )
