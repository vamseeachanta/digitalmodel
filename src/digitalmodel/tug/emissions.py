# ABOUTME: Tug hybrid power sizing and fuel/CO2/NOx with IMO Tier III NOx compliance check
# ABOUTME: Duty-cycle energy drives battery peak-shaving and fuel/emissions totals
"""Tug fuel, emissions, and hybrid power sizing (issue #1198).

A tug's duty cycle is peaky — short, high-power assist bursts between long, low
load transits and standby. That profile is what makes hybrid/battery systems
attractive: a battery shaves the peaks so the engine runs nearer its efficient
band, cutting fuel, emissions, and noise, and giving a path to IMO Tier III /
GHG compliance.

References:
    - IMO MARPOL Annex VI Tier III NOx limits (function of rated engine speed)
    - Riviera tug-market hybrid/alternative-fuel reviews
    - Representative SFOC and fuel carbon factors (see constants.py)
"""

from dataclasses import dataclass

from digitalmodel.tug.constants import FUEL_CO2_FACTOR, FUEL_LHV_MJ_PER_KG

# Reference brake SFOC for marine gas oil, g/kWh. SFOC for another fuel is scaled
# by the inverse LHV ratio (same engine efficiency, different fuel energy density).
SFOC_MGO_G_PER_KWH = 195.0


@dataclass
class OperatingPhase:
    """One phase of a tug operating profile."""

    name: str
    power_fraction: float  # fraction of installed power used in this phase (0..1)
    duration_h: float  # hours spent in this phase per cycle


@dataclass
class HybridSizingResult:
    """Battery sizing for peak-shaving a duty cycle."""

    total_energy_kwh: float
    peak_power_kw: float
    baseline_engine_kw: float
    battery_power_kw: float  # power the battery must deliver to shave peaks
    battery_energy_kwh: float  # usable energy the battery must store
    peak_energy_fraction: float  # share of cycle energy delivered by the battery


@dataclass
class EmissionsResult:
    """Fuel burn, CO2, and IMO Tier III NOx compliance."""

    fuel_mass_t: float
    co2_t: float
    nox_rate_g_per_kwh: float
    tier_iii_nox_limit_g_per_kwh: float
    tier_iii_compliant: bool


def imo_tier_iii_nox_limit(rated_rpm: float) -> float:
    """IMO MARPOL Annex VI Tier III NOx limit, g/kWh, by rated engine speed n.

    n < 130 rpm        : 3.4
    130 <= n < 2000    : 9 * n^(-0.2)
    n >= 2000 rpm      : 2.0
    """
    if rated_rpm <= 0:
        raise ValueError("rated_rpm must be positive")
    if rated_rpm < 130:
        return 3.4
    if rated_rpm < 2000:
        return 9.0 * rated_rpm**-0.2
    return 2.0


def cycle_energy(installed_power_kw: float, profile: list[OperatingPhase]) -> float:
    """Total propulsion energy per operating cycle, kWh."""
    if installed_power_kw <= 0:
        raise ValueError("installed_power_kw must be positive")
    if not profile:
        raise ValueError("profile must contain at least one operating phase")
    return sum(installed_power_kw * p.power_fraction * p.duration_h for p in profile)


def size_hybrid(
    installed_power_kw: float,
    profile: list[OperatingPhase],
    baseline_fraction: float = 0.5,
) -> HybridSizingResult:
    """Size a peak-shaving battery for a tug duty cycle.

    The engine carries a baseline load; the battery supplies the power and
    energy above that baseline during high-power phases.

    Args:
        installed_power_kw: installed propulsion power, kW.
        profile: operating phases (power fractions and durations).
        baseline_fraction: fraction of installed power the engine carries
            continuously; peaks above it are shaved by the battery.

    Returns:
        HybridSizingResult with battery power/energy and the peak energy share.
    """
    if not 0.0 < baseline_fraction < 1.0:
        raise ValueError("baseline_fraction must be in (0, 1)")

    total = cycle_energy(installed_power_kw, profile)
    baseline_kw = baseline_fraction * installed_power_kw
    peak_kw = max(installed_power_kw * p.power_fraction for p in profile)

    battery_power = max(0.0, peak_kw - baseline_kw)
    battery_energy = sum(
        max(0.0, installed_power_kw * p.power_fraction - baseline_kw) * p.duration_h
        for p in profile
    )
    share = battery_energy / total if total > 0 else 0.0

    return HybridSizingResult(
        total_energy_kwh=total,
        peak_power_kw=peak_kw,
        baseline_engine_kw=baseline_kw,
        battery_power_kw=battery_power,
        battery_energy_kwh=battery_energy,
        peak_energy_fraction=share,
    )


def fuel_and_emissions(
    installed_power_kw: float,
    profile: list[OperatingPhase],
    sfoc_g_per_kwh: float | None = None,
    fuel: str = "mgo",
    nox_rate_g_per_kwh: float = 9.0,
    rated_rpm: float = 1600.0,
) -> EmissionsResult:
    """Fuel burn, CO2, and Tier III NOx compliance over an operating cycle.

    fuel_mass = SFOC * energy
    CO2 = fuel_mass * carbon_factor
    Tier III compliant if engine NOx rate <= the speed-dependent limit.

    If ``sfoc_g_per_kwh`` is not given it is derived for the chosen fuel by
    scaling the reference MGO SFOC by the inverse lower-heating-value ratio
    (a lower-energy fuel like methanol burns more mass per kWh), so fuel mass
    is fuel-correct rather than diesel-only.

    Args:
        installed_power_kw: installed propulsion power, kW.
        profile: operating phases.
        sfoc_g_per_kwh: specific fuel-oil consumption, g/kWh; derived from the
            fuel's LHV when omitted.
        fuel: fuel key in FUEL_CO2_FACTOR ('mgo', 'mdo', 'methanol', 'hydrogen').
        nox_rate_g_per_kwh: engine NOx emission rate, g/kWh.
        rated_rpm: engine rated speed for the Tier III limit.

    Returns:
        EmissionsResult with fuel mass, CO2, and Tier III compliance.
    """
    key = fuel.lower()
    if key not in FUEL_CO2_FACTOR:
        raise ValueError(f"unknown fuel {fuel!r}; use one of {list(FUEL_CO2_FACTOR)}")

    if sfoc_g_per_kwh is None:
        sfoc_g_per_kwh = SFOC_MGO_G_PER_KWH * (
            FUEL_LHV_MJ_PER_KG["mgo"] / FUEL_LHV_MJ_PER_KG[key]
        )

    energy_kwh = cycle_energy(installed_power_kw, profile)
    fuel_mass_t = sfoc_g_per_kwh * energy_kwh / 1e6  # g -> tonnes
    co2_t = fuel_mass_t * FUEL_CO2_FACTOR[key]
    limit = imo_tier_iii_nox_limit(rated_rpm)

    return EmissionsResult(
        fuel_mass_t=fuel_mass_t,
        co2_t=co2_t,
        nox_rate_g_per_kwh=nox_rate_g_per_kwh,
        tier_iii_nox_limit_g_per_kwh=limit,
        tier_iii_compliant=nox_rate_g_per_kwh <= limit,
    )
