#!/usr/bin/env python3
"""
ABOUTME: PlaningStripModel — 2D+t strip discretisation and per-strip force
computation using Wagner water-entry theory for planing hull seakeeping.

Theory references:
  - Wagner (1932): water-entry of a wedge.
  - Tavakoli et al. (2026) Phys. Fluids 38, 027129 — Sections II-III.

Key formulae
------------
Half-wetted beam under Wagner impact:
    c(t) = sqrt(2 * V_n * t / tan(beta))   [Wagner 1932, simplified]
    where beta = deadrise angle (rad)

Added mass per unit length (2-D wedge, per strip):
    m_a(c) = (pi/2) * rho_w * c^2

Water-entry force rate per unit length:
    F_WE = d(m_a * V_n) / dt
         = pi * rho_w * c * dc/dt * V_n + (pi/2) * rho_w * c^2 * dV_n/dt

For quasi-steady strip approach (constant V_n within one time step):
    dc/dt = sqrt(V_n / (2 * t * tan(beta)))   (derivative of c(t))
    F_WE ≈ pi * rho_w * c * (dc/dt) * V_n
          = (pi/2) * rho_w * V_n^2 / tan(beta)   [simplified momentum slam]
"""

import math
import numpy as np

from .geometry import PlaningHullGeometry

# Water density [kg/m^3]
RHO_WATER: float = 1025.0


class PlaningStripModel:
    """
    Discretises the hull into N transverse strips and evaluates
    per-strip added mass and water-entry (slam) forces.

    Parameters
    ----------
    geometry : PlaningHullGeometry
        Hull principal dimensions.
    n_strips : int
        Number of strips along the hull length. Default 20.
    rho : float
        Water density [kg/m^3]. Default 1025.
    """

    def __init__(
        self,
        geometry: PlaningHullGeometry,
        n_strips: int = 20,
        rho: float = RHO_WATER,
    ) -> None:
        self.geometry = geometry
        self.n_strips = n_strips
        self.rho = rho

        self.dx: float = geometry.length / n_strips
        # Strip centres: from bow (x=0) to stern (x=L)
        self.strip_x: np.ndarray = np.linspace(
            self.dx / 2.0,
            geometry.length - self.dx / 2.0,
            n_strips,
        )

        # Precompute deadrise tangent (constant deadrise Fridsma series)
        self._tan_beta: float = math.tan(geometry.deadrise_rad)

    # ------------------------------------------------------------------
    # Wagner per-strip computations
    # ------------------------------------------------------------------

    def added_mass_per_length(self, c: float) -> float:
        """
        Added mass per unit length for a 2-D wedge with half-wetted beam c.

        m_a = (pi/2) * rho * c^2

        Parameters
        ----------
        c : float
            Half-wetted beam [m]. Must be >= 0.

        Returns
        -------
        float
            Added mass per unit strip length [kg/m].
        """
        return (math.pi / 2.0) * self.rho * c * c

    def half_wetted_beam(self, v_n: float, dt: float) -> float:
        """
        Instantaneous half-wetted beam from Wagner impact theory.

        c = sqrt(2 * V_n * dt / tan(beta))

        Uses the strip entry time dt as a proxy for the time since first
        contact (quasi-steady assumption per strip).

        Parameters
        ----------
        v_n : float
            Normal (downward) entry velocity of the strip [m/s].
        dt : float
            Time increment representing strip contact duration [s].

        Returns
        -------
        float
            Half-wetted beam [m]. Zero when v_n <= 0.
        """
        if v_n <= 0.0 or dt <= 0.0:
            return 0.0
        return math.sqrt(2.0 * v_n * dt / self._tan_beta)

    def water_entry_force_per_length(self, v_n: float, dt: float) -> float:
        """
        Water-entry (slam) force per unit strip length.

        Uses the simplified momentum approach:
            F_WE / dx = (pi/2) * rho * V_n^2 / tan(beta)

        This is the quasi-steady Wagner result (O(V_n^2)); the sign convention
        is positive upward (opposing downward entry).

        Parameters
        ----------
        v_n : float
            Normal entry velocity (positive downward) [m/s].
        dt : float
            Time step [s] (used for half-wetted beam; not needed for simplified
            form but kept for API consistency).

        Returns
        -------
        float
            Upward water-entry force per unit length [N/m].
        """
        if v_n <= 0.0:
            return 0.0
        return (math.pi / 2.0) * self.rho * v_n * v_n / self._tan_beta

    # ------------------------------------------------------------------
    # Buoyancy per strip (static + dynamic)
    # ------------------------------------------------------------------

    def buoyancy_force_per_length(self, submergence: float) -> float:
        """
        Hydrostatic buoyancy per unit length for a wedge cross-section.

        For a wedge with deadrise beta, the cross-sectional area at
        submergence h (from keel to waterline) is:
            A = h^2 * tan(beta_half)   where beta_half ≈ beta for narrow wedge
        Using full deadrise approximation: A ≈ h^2 / tan(beta).

        Parameters
        ----------
        submergence : float
            Depth of keel below waterplane [m]. Zero when not submerged.

        Returns
        -------
        float
            Upward buoyancy force per unit length [N/m].
        """
        if submergence <= 0.0:
            return 0.0
        area = submergence * submergence / self._tan_beta
        return self.rho * 9.81 * area

    # ------------------------------------------------------------------
    # Lift force per strip (planing lift, Savitsky-style)
    # ------------------------------------------------------------------

    def planing_lift_per_length(
        self,
        speed: float,
        trim_angle: float,
        wetted_length: float,
    ) -> float:
        """
        Simplified steady planing lift per unit length (longitudinal strip).

        Uses Savitsky's 2-D lift coefficient approximation:
            C_L = pi * tau   (for small trim angle tau in radians)
        Lift per unit length: L' = 0.5 * rho * V^2 * B * C_L

        Parameters
        ----------
        speed : float
            Forward speed [m/s].
        trim_angle : float
            Dynamic trim angle [rad].
        wetted_length : float
            Wetted keel length [m] (unused in 2-D approximation).

        Returns
        -------
        float
            Upward planing lift per unit hull length [N/m].
        """
        if speed <= 0.0:
            return 0.0
        # 2-D lift coefficient (Savitsky linearised)
        c_l = math.pi * abs(trim_angle)
        return 0.5 * self.rho * speed * speed * self.geometry.beam * c_l
