# ABOUTME: SwabSurge — tripping-induced swab/surge pressures (Burkhardt clinging constant)
# ABOUTME: Reference: Burkhardt (1961) JPT; API RP 13D. Reuses the Bingham annular friction model.

"""
SwabSurge
=========

Swab and surge pressures induced by moving pipe during tripping, and the
maximum safe tripping speed that keeps bottom-hole pressure inside the
pore/fracture (kick/loss) window.

Physics
-------
Moving pipe drags and displaces mud, producing a frictional pressure change in
the annulus:

    * **Surging** (running in) raises bottom-hole pressure  -> loss / fracture risk.
    * **Swabbing** (pulling out) lowers bottom-hole pressure -> influx / kick risk.

The mud's effective annular velocity from pipe motion uses Burkhardt's mud
**clinging constant** ``Kc`` plus the pipe displacement::

    closed pipe:  V_e = Vp * (Kc + Dod^2 / (Dh^2 - Dod^2))
    open pipe:    V_e = Vp * (Kc + (Dod^2 - Did^2) / (Dh^2 - Dod^2 + Did^2))

``V_e`` is fed into the same Bingham-Plastic annular friction model as
:class:`digitalmodel.well.drilling.hydraulics.WellboreHydraulics`::

    dp = [ PV * V_e / (239400 * D_eq^2) + YP / (200 * D_eq) ] * TVD     [psi]

and converted to an equivalent mud-weight swing ``dEMW = dp / (0.052 * TVD)``.

Closed-form screening tool (API RP 13D class). A transient surge model
(acoustic / wave propagation) is a follow-up; this is the steady screening tier.

Units: diameters in inches, depth in feet, mud weight in ppg, PV in cp,
YP in lbf/100 ft^2, trip speed in ft/min, pressure in psi.
"""

from __future__ import annotations

import math

__all__ = ["SwabSurgeResult", "TripSpeedLimit", "SwabSurge"]

# Friction constants (identical to WellboreHydraulics; SI-derived field units).
_K_VISCOUS = 239400.0
_K_YIELD = 200.0
_PSI_PER_PPG_FT = 0.052


class SwabSurgeResult:
    """Swab/surge result at one tripping speed."""

    def __init__(
        self,
        *,
        trip_speed_ft_min: float,
        effective_velocity_ft_min: float,
        pressure_psi: float,
        surge_emw_ppg: float,
        swab_emw_ppg: float,
    ) -> None:
        self.trip_speed_ft_min = trip_speed_ft_min
        self.effective_velocity_ft_min = effective_velocity_ft_min
        self.pressure_psi = pressure_psi
        self.surge_emw_ppg = surge_emw_ppg  # running in (BHP up)
        self.swab_emw_ppg = swab_emw_ppg  # pulling out (BHP down)

    def as_dict(self) -> dict[str, float]:
        return {
            "trip_speed_ft_min": self.trip_speed_ft_min,
            "effective_velocity_ft_min": self.effective_velocity_ft_min,
            "pressure_psi": self.pressure_psi,
            "surge_emw_ppg": self.surge_emw_ppg,
            "swab_emw_ppg": self.swab_emw_ppg,
        }


class TripSpeedLimit:
    """Maximum safe tripping speeds against the pore/fracture window."""

    def __init__(
        self,
        *,
        swab_limited_ft_min: float,
        surge_limited_ft_min: float,
        pore_pressure_emw_ppg: float,
        fracture_emw_ppg: float,
    ) -> None:
        self.swab_limited_ft_min = swab_limited_ft_min  # pulling-out limit (kick)
        self.surge_limited_ft_min = surge_limited_ft_min  # running-in limit (loss)
        self.pore_pressure_emw_ppg = pore_pressure_emw_ppg
        self.fracture_emw_ppg = fracture_emw_ppg

    @property
    def governing_ft_min(self) -> float:
        """The more restrictive of the swab and surge limits (>= 0)."""
        return max(0.0, min(self.swab_limited_ft_min, self.surge_limited_ft_min))

    def as_dict(self) -> dict[str, float]:
        return {
            "swab_limited_ft_min": self.swab_limited_ft_min,
            "surge_limited_ft_min": self.surge_limited_ft_min,
            "governing_ft_min": self.governing_ft_min,
            "pore_pressure_emw_ppg": self.pore_pressure_emw_ppg,
            "fracture_emw_ppg": self.fracture_emw_ppg,
        }


class SwabSurge:
    """Tripping swab/surge calculator for a single pipe-in-hole section."""

    def __init__(
        self,
        *,
        pipe_od: float,
        pipe_id: float,
        hole_diameter: float,
        mud_weight: float,
        tvd: float,
        plastic_viscosity: float,
        yield_point: float,
        clinging_constant: float = 0.45,
        pipe_config: str = "closed",
    ) -> None:
        if hole_diameter <= pipe_od:
            raise ValueError("hole_diameter must exceed pipe_od")
        if pipe_id >= pipe_od:
            raise ValueError("pipe_id must be less than pipe_od")
        if min(pipe_od, pipe_id, mud_weight, tvd) <= 0:
            raise ValueError("pipe_od, pipe_id, mud_weight, tvd must be > 0")
        if plastic_viscosity < 0 or yield_point < 0:
            raise ValueError("PV and YP must be >= 0")
        if pipe_config not in ("closed", "open"):
            raise ValueError("pipe_config must be 'closed' or 'open'")

        self.pipe_od = float(pipe_od)
        self.pipe_id = float(pipe_id)
        self.hole_diameter = float(hole_diameter)
        self.mud_weight = float(mud_weight)
        self.tvd = float(tvd)
        self.plastic_viscosity = float(plastic_viscosity)
        self.yield_point = float(yield_point)
        self.clinging_constant = float(clinging_constant)
        self.pipe_config = pipe_config

    # -- geometry ----------------------------------------------------------

    def displacement_factor(self) -> float:
        """Dimensionless pipe-displacement contribution to annular velocity."""
        dh2, dod2, did2 = (
            self.hole_diameter**2,
            self.pipe_od**2,
            self.pipe_id**2,
        )
        if self.pipe_config == "closed":
            return dod2 / (dh2 - dod2)
        # open: steel displacement splits between annulus and pipe bore by area
        return (dod2 - did2) / (dh2 - dod2 + did2)

    def effective_velocity(self, trip_speed_ft_min: float) -> float:
        """Effective annular mud velocity from pipe motion, ft/min."""
        return abs(trip_speed_ft_min) * (
            self.clinging_constant + self.displacement_factor()
        )

    # -- pressures ---------------------------------------------------------

    def pressure(self, trip_speed_ft_min: float) -> float:
        """Swab/surge frictional pressure magnitude over TVD, psi."""
        v_e = self.effective_velocity(trip_speed_ft_min)
        if v_e == 0.0:
            return 0.0
        d_eq = self.hole_diameter - self.pipe_od
        dp_per_ft = (
            self.plastic_viscosity * v_e / (_K_VISCOUS * d_eq**2)
            + self.yield_point / (_K_YIELD * d_eq)
        )
        return dp_per_ft * self.tvd

    def _demw(self, pressure_psi: float) -> float:
        return pressure_psi / (_PSI_PER_PPG_FT * self.tvd)

    def evaluate(self, trip_speed_ft_min: float) -> SwabSurgeResult:
        """Swab and surge equivalent mud weights at a tripping speed."""
        p = self.pressure(trip_speed_ft_min)
        demw = self._demw(p)
        return SwabSurgeResult(
            trip_speed_ft_min=abs(trip_speed_ft_min),
            effective_velocity_ft_min=self.effective_velocity(trip_speed_ft_min),
            pressure_psi=p,
            surge_emw_ppg=self.mud_weight + demw,
            swab_emw_ppg=self.mud_weight - demw,
        )

    # -- safe tripping speed ----------------------------------------------

    def _speed_for_pressure(self, target_pressure_psi: float) -> float:
        """Trip speed (ft/min) giving the target swab/surge pressure.

        ``dp(Vp) = a*Vp + b`` with viscous slope ``a`` and yield offset ``b``.
        Returns 0.0 if the yield offset alone already exceeds the target (no
        nonzero speed is safe) or the section has no viscous slope.
        """
        d_eq = self.hole_diameter - self.pipe_od
        factor = self.clinging_constant + self.displacement_factor()
        a = self.plastic_viscosity * factor / (_K_VISCOUS * d_eq**2) * self.tvd
        b = self.yield_point / (_K_YIELD * d_eq) * self.tvd
        if target_pressure_psi <= b or a <= 0.0:
            return 0.0
        return (target_pressure_psi - b) / a

    def max_safe_trip_speed(
        self, pore_pressure_emw_ppg: float, fracture_emw_ppg: float
    ) -> TripSpeedLimit:
        """Max tripping speeds keeping BHP inside the pore/fracture window.

        Swab limit: pulling out must not drop EMW below the pore pressure.
        Surge limit: running in must not raise EMW above the fracture gradient.
        """
        swab_margin_ppg = self.mud_weight - pore_pressure_emw_ppg
        surge_margin_ppg = fracture_emw_ppg - self.mud_weight
        swab_dp = max(0.0, swab_margin_ppg) * _PSI_PER_PPG_FT * self.tvd
        surge_dp = max(0.0, surge_margin_ppg) * _PSI_PER_PPG_FT * self.tvd
        return TripSpeedLimit(
            swab_limited_ft_min=self._speed_for_pressure(swab_dp),
            surge_limited_ft_min=self._speed_for_pressure(surge_dp),
            pore_pressure_emw_ppg=pore_pressure_emw_ppg,
            fracture_emw_ppg=fracture_emw_ppg,
        )
