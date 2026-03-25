#!/usr/bin/env python3
"""
ABOUTME: PlaningMotionSolver — RK4 integrator for coupled heave/pitch EOMs
of a planing hull in regular head-sea waves, plus PlaningRAO dataclass and
the compute_rao() convenience entry point.

Physical model (2D+t strip theory, Tavakoli 2026):
- Perturbation motion about the dynamic planing equilibrium.
- Excitation from wave-induced relative velocity and wave surface.
- Wagner water-entry force provides added mass and nonlinear slam.
- Restoring stiffness from waterplane area (heave) and planing lift (pitch).
- Radiation damping via derivative of added mass (proportional to velocity).

Equations of motion (perturbation coordinates):
    (m + A33)*eta3_ddot + B33*eta3_dot + C33*eta3 = F_exc3(t)
    (Iyy + A55)*eta5_ddot + B55*eta5_dot + C55*eta5 = F_exc5(t)

with nonlinear coupling from slam forces.

Wave steepness: eps = k*a. When eps > 0 the wave-entry velocity has
a second-harmonic component that produces nonlinear (2nd harmonic) response.

RAO extraction: FFT over steady-state window (after 3 wave periods).
"""

import math
from dataclasses import dataclass
from typing import Tuple

import numpy as np

from .geometry import PlaningHullGeometry
from .strip_model import PlaningStripModel

# Physical constants
G: float = 9.81  # gravitational acceleration [m/s^2]
RHO_WATER: float = 1025.0  # water density [kg/m^3]


# ---------------------------------------------------------------------------
# Output dataclass
# ---------------------------------------------------------------------------

@dataclass
class PlaningRAO:
    """
    Response amplitude operators and phase angles for a planing hull.

    Amplitudes are non-dimensionalised:
      heave_rao = eta3_amplitude / wave_amplitude  [m/m]
      pitch_rao = eta5_amplitude / (k * wave_amplitude)  [rad/rad]

    Parameters
    ----------
    heave_rao : float
        Non-dimensional heave amplitude [-].
    pitch_rao : float
        Non-dimensional pitch amplitude [-].
    heave_phase : float
        Heave phase relative to wave crest [rad].
    pitch_phase : float
        Pitch phase relative to wave crest [rad].
    second_harmonic_heave : float
        2nd-harmonic heave amplitude / wave_amplitude [-].
    """

    heave_rao: float
    pitch_rao: float
    heave_phase: float
    pitch_phase: float
    second_harmonic_heave: float


# ---------------------------------------------------------------------------
# Solver class
# ---------------------------------------------------------------------------

class PlaningMotionSolver:
    """
    Coupled heave-pitch RK4 solver for a planing hull in regular head seas.

    Solves perturbation motion about the dynamic planing equilibrium using
    Wagner strip theory with linearised restoring and radiation damping.

    Parameters
    ----------
    geometry : PlaningHullGeometry
        Hull principal dimensions.
    dt : float
        RK4 time step [s]. Default 0.005 s.
    n_strips : int
        Number of strips for the strip model. Default 20.
    rho : float
        Water density [kg/m^3]. Default 1025.
    damping_fraction : float
        Fraction of critical damping added as radiation damping. Default 0.1.
    """

    def __init__(
        self,
        geometry: PlaningHullGeometry,
        dt: float = 0.005,
        n_strips: int = 20,
        rho: float = RHO_WATER,
        damping_fraction: float = 0.10,
    ) -> None:
        self.geometry = geometry
        self.dt = dt
        self.rho = rho
        self.damping_fraction = damping_fraction
        self.strip_model = PlaningStripModel(geometry, n_strips=n_strips, rho=rho)

        # Hull inertia parameters (Fridsma-style: uniform mass distribution)
        self._mass = self._estimate_mass()
        self._iyy = self._estimate_iyy()

    # ------------------------------------------------------------------
    # Mass / inertia estimates
    # ------------------------------------------------------------------

    def _estimate_mass(self) -> float:
        """
        Estimate hull displacement mass from hull dimensions.

        Uses a prismatic wedge approximation:
            Volume ≈ (B^2 / (4*tan(beta))) * L * fill_fraction
        fill_fraction ~ 0.6 for typical planing hull.
        """
        geom = self.geometry
        tan_b = math.tan(geom.deadrise_rad)
        vol = (geom.beam * geom.beam / (4.0 * tan_b)) * geom.length * 0.6
        return self.rho * vol

    def _estimate_iyy(self) -> float:
        """
        Pitch moment of inertia about LCG.

        Approximation: Iyy = k_yy^2 * m, k_yy ≈ 0.25 * L.
        """
        k_yy = 0.25 * self.geometry.length
        return self._mass * k_yy * k_yy

    # ------------------------------------------------------------------
    # Added mass and restoring coefficients
    # ------------------------------------------------------------------

    def _total_added_mass(self) -> Tuple[float, float]:
        """
        Compute total vertical added mass A33 and pitch added moment A55.

        Uses maximum half-wetted beam = B/2 for each strip.
        """
        sm = self.strip_model
        geom = self.geometry
        c_max = geom.half_beam

        a33 = 0.0
        a55 = 0.0
        for xi in sm.strip_x:
            x_lcg = geom.lcg - xi
            ma = sm.added_mass_per_length(c_max) * sm.dx
            a33 += ma
            a55 += ma * x_lcg * x_lcg

        return a33, a55

    def _restoring_coefficients(self, speed: float) -> Tuple[float, float, float, float]:
        """
        Linearised heave restoring (C33) and pitch restoring (C55).

        Heave restoring: waterplane area stiffness
            C33 = rho * g * Awp
            Awp ≈ L * B * Cwp  (waterplane area coefficient ~0.75 for planing)

        Pitch restoring: planing lift curve slope + buoyancy metacentric
            C55 = rho * g * V * GM_L  where GM_L ≈ (L^2/12) for uniform
            Simplified: C55 = rho*g*Awp * (L^2/12)

        Also compute radiation damping fractions based on critical damping.
        """
        geom = self.geometry
        c_wp = 0.75  # waterplane area coefficient
        awp = geom.length * geom.beam * c_wp

        c33 = self.rho * G * awp
        c55 = self.rho * G * awp * (geom.length ** 2) / 12.0

        a33, a55 = self._total_added_mass()
        m_eff3 = self._mass + a33
        m_eff5 = self._iyy + a55

        # Radiation damping: fraction of critical damping
        omega_n3 = math.sqrt(max(c33 / m_eff3, 1e-10))
        omega_n5 = math.sqrt(max(c55 / m_eff5, 1e-10))
        b33 = 2.0 * self.damping_fraction * m_eff3 * omega_n3
        b55 = 2.0 * self.damping_fraction * m_eff5 * omega_n5

        return c33, c55, b33, b55

    # ------------------------------------------------------------------
    # Wave kinematics
    # ------------------------------------------------------------------

    @staticmethod
    def _wave_number(omega: float) -> float:
        """Deep-water wave number: k = omega^2 / g."""
        return omega * omega / G

    def _wave_surface(
        self,
        x: float,
        t: float,
        omega: float,
        wave_amp: float,
        k: float,
        speed: float,
    ) -> float:
        """
        Second-order Stokes wave surface elevation at (x, t) in ship-fixed frame.

        In head seas (ship moves in +x, wave comes from +x):
            encounter phase: phi = k*x + omega_e*t
        First-order:  zeta1 = a*cos(phi)
        Second-order: zeta2 = (k*a^2/2)*cos(2*phi)
        """
        omega_e = omega + k * speed
        phi = k * x + omega_e * t
        zeta1 = wave_amp * math.cos(phi)
        zeta2 = (k * wave_amp * wave_amp / 2.0) * math.cos(2.0 * phi)
        return zeta1 + zeta2

    def _wave_vertical_velocity(
        self,
        x: float,
        t: float,
        omega: float,
        wave_amp: float,
        k: float,
        speed: float,
    ) -> float:
        """
        Vertical water-particle velocity at the free surface (z=0).

        w(x, t) = a * omega_e * sin(k*x + omega_e*t)
        """
        omega_e = omega + k * speed
        phi = k * x + omega_e * t
        return wave_amp * omega_e * math.sin(phi)

    # ------------------------------------------------------------------
    # Excitation force computation
    # ------------------------------------------------------------------

    def _excitation_forces(
        self,
        t: float,
        speed: float,
        omega: float,
        wave_amp: float,
        k: float,
    ) -> Tuple[float, float]:
        """
        Compute wave-excitation heave force and pitch moment.

        Uses the relative motion principle: excitation = integral of
        strip slam forces due to wave orbital velocity alone.

        F_exc3 = sum_strips [ F_slam(w_wave) * dx ]
        M_exc5 = sum_strips [ F_slam(w_wave) * (-x_lcg) * dx ]

        The slam force is nonlinear in velocity: F ~ V_n^2.
        This generates 2nd-harmonic content when w_wave has 1st-order amplitude.
        """
        sm = self.strip_model
        geom = self.geometry

        f3 = 0.0
        m5 = 0.0

        for xi in sm.strip_x:
            x_lcg = geom.lcg - xi
            # Wave vertical velocity at strip (only from wave, not hull motion)
            w_wave = self._wave_vertical_velocity(
                xi, t, omega, wave_amp, k, speed
            )
            # Slam force only when wave hits from below (positive w_wave upward)
            f_slam = sm.water_entry_force_per_length(
                v_n=max(w_wave, 0.0), dt=self.dt
            ) * sm.dx

            f3 += f_slam
            m5 += f_slam * (-x_lcg)

        return f3, m5

    # ------------------------------------------------------------------
    # RK4 integrator
    # ------------------------------------------------------------------

    def integrate(
        self,
        speed: float,
        wave_freq: float,
        wave_steepness: float,
        t_total: float,
    ) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
        """
        Integrate coupled heave-pitch EOMs via RK4.

        Model equations (perturbation about equilibrium):
            m_eff3 * eta3_ddot + b33 * eta3_dot + c33 * eta3 = F_exc3(t)
            m_eff5 * eta5_ddot + b55 * eta5_dot + c55 * eta5 = F_exc5(t)

        Parameters
        ----------
        speed : float
            Forward speed [m/s].
        wave_freq : float
            Wave frequency omega [rad/s].
        wave_steepness : float
            Wave steepness epsilon = k * a.
        t_total : float
            Total integration time [s].

        Returns
        -------
        t_arr : ndarray  Time array [s].
        heave_arr : ndarray  Heave time series [m].
        pitch_arr : ndarray  Pitch time series [rad].
        """
        k = self._wave_number(wave_freq)
        wave_amp = wave_steepness / k if k > 0.0 else 0.0

        a33, a55 = self._total_added_mass()
        c33, c55, b33, b55 = self._restoring_coefficients(speed)
        m_eff3 = self._mass + a33
        m_eff5 = self._iyy + a55

        n_steps = max(2, int(t_total / self.dt))
        t_arr = np.zeros(n_steps)
        heave_arr = np.zeros(n_steps)
        pitch_arr = np.zeros(n_steps)

        state = np.zeros(4)  # [eta3, eta3_dot, eta5, eta5_dot]

        def _deriv(s: np.ndarray, ti: float) -> np.ndarray:
            eta3, v3, eta5, v5 = s
            f3, m5 = self._excitation_forces(ti, speed, wave_freq, wave_amp, k)
            dsdt = np.zeros(4)
            dsdt[0] = v3
            dsdt[1] = (f3 - b33 * v3 - c33 * eta3) / m_eff3
            dsdt[2] = v5
            dsdt[3] = (m5 - b55 * v5 - c55 * eta5) / m_eff5
            return dsdt

        for i in range(n_steps):
            t_arr[i] = i * self.dt
            heave_arr[i] = state[0]
            pitch_arr[i] = state[2]

            ti = i * self.dt
            k1 = _deriv(state, ti)
            k2 = _deriv(state + 0.5 * self.dt * k1, ti + 0.5 * self.dt)
            k3 = _deriv(state + 0.5 * self.dt * k2, ti + 0.5 * self.dt)
            k4 = _deriv(state + self.dt * k3, ti + self.dt)
            state = state + (self.dt / 6.0) * (k1 + 2.0 * k2 + 2.0 * k3 + k4)

        return t_arr, heave_arr, pitch_arr


# ---------------------------------------------------------------------------
# High-level entry point
# ---------------------------------------------------------------------------

def compute_rao(
    geometry: PlaningHullGeometry,
    speed: float,
    wave_freq: float,
    wave_steepness: float,
    n_strips: int = 20,
    dt: float = 0.005,
    n_periods: int = 10,
) -> PlaningRAO:
    """
    Compute heave and pitch RAOs for a planing hull in regular head seas.

    Runs the RK4 solver for n_periods wave periods. The first 3 periods are
    discarded as transient; amplitudes and phases are extracted via FFT
    over the remaining window.

    Parameters
    ----------
    geometry : PlaningHullGeometry
        Hull principal dimensions.
    speed : float
        Forward speed [m/s].
    wave_freq : float
        Wave frequency [rad/s].
    wave_steepness : float
        Wave steepness epsilon = k * H/2. Zero → linear (no excitation).
    n_strips : int
        Number of hull strips. Default 20.
    dt : float
        RK4 time step [s]. Default 0.005.
    n_periods : int
        Number of wave periods to integrate. Default 10.

    Returns
    -------
    PlaningRAO
        RAO amplitudes and phase angles.
    """
    # Handle zero steepness: trivial zero-response solution
    if wave_steepness == 0.0:
        return PlaningRAO(
            heave_rao=0.0,
            pitch_rao=0.0,
            heave_phase=0.0,
            pitch_phase=0.0,
            second_harmonic_heave=0.0,
        )

    k = wave_freq * wave_freq / G
    wave_amp = wave_steepness / k
    wave_period = 2.0 * math.pi / wave_freq

    t_total = n_periods * wave_period

    solver = PlaningMotionSolver(geometry, dt=dt, n_strips=n_strips)
    t_arr, heave_arr, pitch_arr = solver.integrate(
        speed=speed,
        wave_freq=wave_freq,
        wave_steepness=wave_steepness,
        t_total=t_total,
    )

    # Discard first 3 wave periods (transient)
    n_skip = int(3.0 * wave_period / dt)
    heave_ss = heave_arr[n_skip:]
    pitch_ss = pitch_arr[n_skip:]

    if len(heave_ss) < 4:
        return PlaningRAO(
            heave_rao=0.0,
            pitch_rao=0.0,
            heave_phase=0.0,
            pitch_phase=0.0,
            second_harmonic_heave=0.0,
        )

    # FFT-based amplitude and phase extraction
    n = len(heave_ss)
    freqs = np.fft.rfftfreq(n, d=dt)
    omega_e = wave_freq + k * speed  # encounter frequency [rad/s]

    heave_fft = np.fft.rfft(heave_ss)
    pitch_fft = np.fft.rfft(pitch_ss)

    idx1 = _nearest_freq_index(freqs, omega_e / (2.0 * math.pi))
    idx2 = _nearest_freq_index(freqs, 2.0 * omega_e / (2.0 * math.pi))

    heave_amp1 = 2.0 * abs(heave_fft[idx1]) / n
    pitch_amp1 = 2.0 * abs(pitch_fft[idx1]) / n
    heave_amp2 = 2.0 * abs(heave_fft[idx2]) / n

    heave_phase = float(np.angle(heave_fft[idx1]))
    pitch_phase = float(np.angle(pitch_fft[idx1]))

    # Non-dimensionalise
    heave_rao = heave_amp1 / wave_amp
    pitch_rao = pitch_amp1 / (k * wave_amp) if (k * wave_amp) > 0.0 else 0.0
    sh2 = heave_amp2 / wave_amp

    return PlaningRAO(
        heave_rao=float(heave_rao),
        pitch_rao=float(pitch_rao),
        heave_phase=float(heave_phase),
        pitch_phase=float(pitch_phase),
        second_harmonic_heave=float(sh2),
    )


def _nearest_freq_index(freqs: np.ndarray, target_hz: float) -> int:
    """Return index of frequency array entry closest to target_hz."""
    if len(freqs) == 0:
        return 0
    idx = int(np.argmin(np.abs(freqs - target_hz)))
    return max(0, min(idx, len(freqs) - 1))
