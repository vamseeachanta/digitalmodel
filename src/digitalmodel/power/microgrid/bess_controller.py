"""BESS controller — SOC management, charge/discharge rate limiting.

Manages battery energy storage system state including state-of-charge
tracking, max rate computation with linear derating near SOC limits,
and island-mode reserve enforcement.

References
----------
IEEE 1547.4-2011 §6 — Energy storage integration requirements.
"""

from __future__ import annotations

DERATING_BAND: float = 0.05
"""Width of SOC derating band above soc_min [fraction]."""

ISLAND_RESERVE: float = 0.20
"""Extra SOC reserve held in island mode [fraction]."""


class BESSController:
    """Battery energy storage system controller.

    Parameters
    ----------
    capacity_kwh : float
        Nameplate energy capacity [kWh].
    max_power_kw : float
        Maximum charge/discharge power [kW].
    soc : float
        Initial state-of-charge [0–1]. Default 0.5.
    soc_min : float
        Minimum allowed SOC [0–1]. Default 0.10.
    soc_max : float
        Maximum allowed SOC [0–1]. Default 0.90.

    Raises
    ------
    ValueError
        If soc_min >= soc_max or soc outside [soc_min, soc_max].
    """

    def __init__(
        self,
        capacity_kwh: float,
        max_power_kw: float,
        soc: float = 0.5,
        soc_min: float = 0.10,
        soc_max: float = 0.90,
    ) -> None:
        if soc_min >= soc_max:
            raise ValueError(
                f"soc_min ({soc_min}) must be less than soc_max ({soc_max})"
            )
        if soc < soc_min or soc > soc_max:
            raise ValueError(
                f"soc ({soc}) must be in [{soc_min}, {soc_max}]"
            )
        self.capacity_kwh = capacity_kwh
        self.max_power_kw = max_power_kw
        self.soc = soc
        self.soc_min = soc_min
        self.soc_max = soc_max

    def max_discharge_kw(self, island_mode: bool = False) -> float:
        """Maximum allowable discharge power [kW].

        Applies linear derating in the last 5% SOC band above the
        effective minimum to prevent control oscillation.

        Parameters
        ----------
        island_mode : bool
            If True, adds 20% island reserve to effective soc_min.

        Returns
        -------
        float
            Available discharge power [kW], >= 0.
        """
        effective_min = self.soc_min + (ISLAND_RESERVE if island_mode else 0.0)
        margin = self.soc - effective_min
        if margin <= 0.0:
            return 0.0
        if margin >= DERATING_BAND:
            return self.max_power_kw
        derating = margin / DERATING_BAND
        return self.max_power_kw * derating

    def max_charge_kw(self) -> float:
        """Maximum allowable charge power [kW].

        Applies linear derating in the last 5% SOC band below soc_max.

        Returns
        -------
        float
            Available charge power [kW], >= 0.
        """
        margin = self.soc_max - self.soc
        if margin <= 0.0:
            return 0.0
        if margin >= DERATING_BAND:
            return self.max_power_kw
        derating = margin / DERATING_BAND
        return self.max_power_kw * derating

    def compute_power_setpoint(
        self,
        requested_kw: float,
        island_mode: bool = False,
    ) -> float:
        """Compute clamped power setpoint.

        Positive = discharge, negative = charge.

        Parameters
        ----------
        requested_kw : float
            Requested power [kW]. Positive = discharge, negative = charge.
        island_mode : bool
            If True, applies island-mode SOC reserve.

        Returns
        -------
        float
            Clamped power setpoint [kW].
        """
        if requested_kw >= 0:
            max_avail = self.max_discharge_kw(island_mode=island_mode)
            return min(requested_kw, max_avail)
        max_avail = self.max_charge_kw()
        return max(requested_kw, -max_avail)

    def update_soc(self, power_kw: float, dt_hours: float) -> None:
        """Update SOC based on power delivered over a time step.

        Parameters
        ----------
        power_kw : float
            Power delivered [kW]. Positive = discharge, negative = charge.
        dt_hours : float
            Time step duration [hours].
        """
        energy_kwh = power_kw * dt_hours
        delta_soc = energy_kwh / self.capacity_kwh
        new_soc = self.soc - delta_soc  # discharge decreases SOC
        self.soc = max(self.soc_min, min(self.soc_max, new_soc))
