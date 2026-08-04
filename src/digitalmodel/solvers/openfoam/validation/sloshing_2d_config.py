#!/usr/bin/env python3
"""
ABOUTME: Configuration contracts for the 2D rectangular-tank sloshing validation
cases (#639): the free-decay natural-frequency case and the forced-roll case,
plus the acceptance tolerance and the out-of-plane slab thickness they share.
Separated from rendering, building and analysis so each can depend on the
contracts without depending on the others.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import List, Tuple

from ..motion import (
    MotionType,
    PrescribedMotion,
)
from ..partial_fill import (
    snap_fill_to_cell_face,
)
from ..spectral_analysis import (
    prismatic_tank_natural_frequency,
)

# Primary gate (#639): measured first-mode sloshing frequency tracks the
# analytical tanh dispersion relation within 5%.
SLOSHING_FREQ_TOLERANCE = 0.05

# Out-of-plane slab thickness for the 2D case (m).
_CASE_DEPTH = 0.01

# ---------------------------------------------------------------------------
# Free-decay natural-frequency configuration (PRIMARY gate)
# ---------------------------------------------------------------------------


@dataclass
class SloshingFreeDecayConfig:
    """Config for the free-decay first-mode sloshing-frequency validation.

    A rectangular tank of breadth ``L`` and height ``tank_height`` filled to
    ``fill_level`` is given a small first-mode cosine perturbation of the free
    surface and released. The clean dimensions (L=1.0 m, half full) put the
    analytical first mode at ~0.76 Hz.

    Attributes:
        breadth: Tank breadth ``L`` in the sloshing (x) direction (m).
        tank_height: Internal tank height (y) (m).
        fill_level: Still-water fill fraction of ``tank_height`` (0-1). Snapped
            onto a cell face by the partial-fill helper (#659).
        cells_per_breadth: Uniform cells across the breadth; the cell size
            ``L/cells_per_breadth`` is used in both x and y.
        perturbation_amplitude: First-mode cosine perturbation amplitude (m);
            small vs the fill depth to stay in the linear regime.
        delta_t: Fixed time step (s) — fixed so the probe sampling is uniform
            for the FFT.
        end_time: Physical end time (s); ~12 first-mode periods.
        sample_every: Probe (interfaceHeight) sampling stride in time steps.
        field_write_interval: Full-field snapshot interval (s) for qualitative
            inspection.
        name: Case directory name.
    """

    breadth: float = 1.0
    tank_height: float = 0.6
    fill_level: float = 0.5
    cells_per_breadth: int = 100
    perturbation_amplitude: float = 0.02
    delta_t: float = 0.002
    end_time: float = 16.0
    sample_every: int = 10
    field_write_interval: float = 0.5
    name: str = "validation_sloshing_free_decay"

    @property
    def cell_size(self) -> float:
        """Uniform cell size ``L / cells_per_breadth`` (m)."""
        return self.breadth / self.cells_per_breadth

    @property
    def nx(self) -> int:
        return self.cells_per_breadth

    @property
    def ny(self) -> int:
        """Vertical cell count (tank_height / cell_size, rounded)."""
        return max(1, round(self.tank_height / self.cell_size))

    @property
    def fill_snap(self):
        """Fill snapped onto a vertical cell face (see partial_fill)."""
        return snap_fill_to_cell_face(self.tank_height, self.ny, self.fill_level)

    @property
    def fill_depth(self) -> float:
        """Snapped still-water fill depth ``h`` (m)."""
        return self.fill_snap.fill_height

    def analytical_frequency(self, mode: int = 1) -> float:
        """Analytical first-mode sloshing frequency (Hz) for the snapped fill."""
        return prismatic_tank_natural_frequency(
            self.breadth, self.fill_depth, mode=mode
        )

    @property
    def probe_x(self) -> float:
        """Wall-probe x location (half a cell off the left wall)."""
        return 0.5 * self.cell_size


# ---------------------------------------------------------------------------
# Forced-roll SPHERIC Test 10 configuration (CORROBORATION)
# ---------------------------------------------------------------------------


@dataclass
class SloshingForcedRollConfig:
    """Config for the SPHERIC Test 10 forced-roll rectangular-tank benchmark.

    Attributes:
        breadth: Tank breadth ``L`` (m) — SPHERIC Test 10 = 0.9 m.
        tank_height: Tank height (m) — 0.508 m.
        fill_depth: Still-water fill depth ``h`` (m) — 0.093 m (18% fill).
        roll_amplitude_deg: Forced-roll amplitude (deg) — ~4 deg.
        roll_period: Forced-roll period (s). Defaults to the benchmark's
            lateral-impact drive 0.85*T1 (T1 = first-mode period).
        cells_per_breadth: Uniform cells across the breadth.
        delta_t: Base time step (s); the solver adapts on Courant number.
        n_cycles: Number of forcing periods to run.
        field_write_interval: Full-field snapshot interval (s).
        name: Case directory name.
    """

    breadth: float = 0.9
    tank_height: float = 0.508
    fill_depth: float = 0.093
    roll_amplitude_deg: float = 4.0
    roll_period: float | None = None
    cells_per_breadth: int = 90
    delta_t: float = 0.001
    n_cycles: float = 6.0
    field_write_interval: float = 0.05
    name: str = "validation_sloshing_spheric_test10"
    # --- Effective-Gravity-Angle (combined sway+roll) excitation (Carette 2023).
    # A real vessel's roll axis sits BELOW the ballast tank, so a rolling tank
    # also feels a lateral acceleration; roll-only about the tank is a partial
    # (conservative) drive. Two equivalent representations:
    #   roll_axis_depth_m > 0 : roll about an axis this far below the tank floor
    #       (exact rigid roll about the lower axis — the origin drops to -depth).
    #   sway_amplitude_m > 0  : superpose an independent lateral SURGE on the roll
    #       about the floor via OpenFOAM multiMotion (general seaway: arbitrary
    #       amplitude + phase). sway_phase_shift_s offsets it from the roll.
    # Set at most one (they overlap physically); the driver picks the renderer.
    roll_axis_depth_m: float = 0.0
    sway_amplitude_m: float = 0.0
    sway_phase_shift_s: float = 0.0

    @property
    def cell_size(self) -> float:
        return self.breadth / self.cells_per_breadth

    @property
    def nx(self) -> int:
        return self.cells_per_breadth

    @property
    def ny(self) -> int:
        return max(1, round(self.tank_height / self.cell_size))

    @property
    def fill_level(self) -> float:
        return self.fill_depth / self.tank_height

    @property
    def fill_snap(self):
        return snap_fill_to_cell_face(self.tank_height, self.ny, self.fill_level)

    def analytical_frequency(self, mode: int = 1) -> float:
        return prismatic_tank_natural_frequency(
            self.breadth, self.fill_depth, mode=mode
        )

    @property
    def first_mode_period(self) -> float:
        """Analytical first-mode period T1 (s)."""
        return 1.0 / self.analytical_frequency()

    @property
    def drive_period(self) -> float:
        """Forced-roll drive period (s) — default 0.85*T1 (lateral-impact case)."""
        if self.roll_period is not None:
            return self.roll_period
        return 0.85 * self.first_mode_period

    @property
    def end_time(self) -> float:
        return self.n_cycles * self.drive_period

    @property
    def roll_origin(self) -> Tuple[float, float, float]:
        """Rotation axis at the centre of the tank floor (m), dropped by
        ``roll_axis_depth_m`` to place it below the tank for an EGA drive."""
        return (0.5 * self.breadth, -self.roll_axis_depth_m or 0.0, 0.0)

    def motion(self) -> PrescribedMotion:
        """Prescribed forced roll = in-plane rotation about z (engine YAW)."""
        return PrescribedMotion(
            MotionType.YAW,
            amplitude=self.roll_amplitude_deg,
            period=self.drive_period,
            origin=self.roll_origin,
        )

    @property
    def is_combined_motion(self) -> bool:
        """True when an independent lateral sway is superposed on the roll
        (Effective-Gravity-Angle drive via multiMotion)."""
        return self.sway_amplitude_m > 0.0

    def motions(self) -> List[Tuple[str, PrescribedMotion]]:
        """Ordered ``(label, motion)`` pairs for the multiMotion superposition:
        roll (about the floor) + an independent lateral SURGE. Only meaningful
        when ``is_combined_motion``."""
        roll = PrescribedMotion(
            MotionType.YAW, amplitude=self.roll_amplitude_deg,
            period=self.drive_period, origin=(0.5 * self.breadth, 0.0, 0.0))
        sway = PrescribedMotion(
            MotionType.SURGE, amplitude=self.sway_amplitude_m,
            period=self.drive_period, phase_shift_s=self.sway_phase_shift_s)
        return [("roll", roll), ("sway", sway)]
