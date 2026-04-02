"""
Environmental Correction Factors for Fatigue
=============================================

Correction factors to account for the effect of environment on fatigue
life.  Environments include:

- **Air** (baseline, factor = 1.0)
- **Seawater with cathodic protection (CP)** — reduced fatigue life
- **Seawater free corrosion** — significantly reduced life, no endurance limit
- **Temperature derating** — reduced allowable stress at elevated temperature
- **Splash zone / submerged / atmospheric** zone factors

All factors are applied as multipliers on the S-N curve intercept (log_a)
or as divisors on the allowable fatigue life.

References
----------
- DNV-RP-C203 (2021), Section 2.4 — S-N curves in seawater
- DNV-RP-C203 (2021), Section 2.11 — Effect of elevated temperature
- API RP 2A-WSD (2014), Section 5.3 — Environmental effects
- NORSOK N-004 (2013), Annex C.2.6 — Environmental factors
"""

import math
from typing import Optional, Union

import numpy as np
from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# Pydantic models
# ---------------------------------------------------------------------------

class EnvironmentInput(BaseModel):
    """Input specification for environmental correction.

    Attributes
    ----------
    environment : str
        One of ``"air"``, ``"seawater_cp"``, ``"free_corrosion"``.
    zone : str
        Structural zone: ``"atmospheric"``, ``"splash"``, ``"submerged"``.
        Default ``"submerged"``.
    temperature : float
        Operating temperature (°C). Default 20.
    cp_potential : float
        Cathodic protection potential (mV vs Ag/AgCl). Default -800.
        Only relevant for ``"seawater_cp"``.
    """
    environment: str = "air"
    zone: str = "submerged"
    temperature: float = 20.0
    cp_potential: float = -800.0


class EnvironmentResult(BaseModel):
    """Result of environmental correction calculation.

    Attributes
    ----------
    life_factor : float
        Multiplier on fatigue life. < 1.0 means reduced life.
    log_a_adjustment : float
        Additive correction to log10(a) of the S-N curve.
    endurance_limit_factor : float
        Factor on the endurance limit (0 = no limit, 1 = full limit).
    slope_change : bool
        Whether the SN curve changes to single-slope.
    description : str
        Human-readable description of the correction.
    """
    life_factor: float = 1.0
    log_a_adjustment: float = 0.0
    endurance_limit_factor: float = 1.0
    slope_change: bool = False
    description: str = ""


# ---------------------------------------------------------------------------
# Zone factors
# ---------------------------------------------------------------------------

_ZONE_FACTORS = {
    # Zone: (life_factor, note)
    "atmospheric": (1.0, "Atmospheric zone — air conditions apply"),
    "splash":      (0.7, "Splash zone — severe corrosion, reduced life per DNV-RP-C203 Sec 2.4"),
    "submerged":   (1.0, "Submerged zone — use appropriate seawater S-N curve"),
}


def zone_factor(zone: str) -> float:
    """Return the fatigue life factor for a structural zone.

    DNV-RP-C203 (2021), Section 2.4.3.

    Parameters
    ----------
    zone : str
        One of ``"atmospheric"``, ``"splash"``, ``"submerged"``.

    Returns
    -------
    float
        Life factor (≤ 1.0 for penalty zones).
    """
    zone_lower = zone.lower()
    if zone_lower in _ZONE_FACTORS:
        return _ZONE_FACTORS[zone_lower][0]
    raise ValueError(f"Unknown zone '{zone}'. Use 'atmospheric', 'splash', or 'submerged'.")


# ---------------------------------------------------------------------------
# Temperature derating
# ---------------------------------------------------------------------------

def temperature_derating_factor(
    temperature: float,
    standard: str = "DNV",
) -> float:
    """Temperature derating factor for fatigue.

    DNV-RP-C203 (2021) Section 2.11: No reduction needed for T ≤ 100°C.
    For T > 100°C, the allowable stress range is reduced by a factor
    based on the ratio of elastic modulus at temperature to that at 20°C.

    API RP 2A Section 5.3: Similar approach, using ASME yield/UTS derating.

    Parameters
    ----------
    temperature : float
        Operating temperature (°C).
    standard : str
        ``"DNV"`` or ``"API"``. Default ``"DNV"``.

    Returns
    -------
    float
        Factor on allowable stress range (≤ 1.0 at elevated temperatures).
    """
    if temperature <= 100.0:
        return 1.0

    if standard.upper() == "DNV":
        # DNV-RP-C203 Sec 2.11: linear reduction in E beyond 100°C
        # E(T)/E(20) approximation for carbon steel
        # At 200°C: ~0.95, at 300°C: ~0.88, at 400°C: ~0.79
        E_ratio = 1.0 - 0.0007 * (temperature - 100.0)
        return round(max(E_ratio, 0.5), 4)

    elif standard.upper() == "API":
        # API RP 2A: similar to DNV, slightly more conservative
        E_ratio = 1.0 - 0.0008 * (temperature - 100.0)
        return round(max(E_ratio, 0.5), 4)

    else:
        raise ValueError(f"Unknown standard '{standard}'. Use 'DNV' or 'API'.")


# ---------------------------------------------------------------------------
# Main environment correction
# ---------------------------------------------------------------------------

def environment_correction(
    inp: EnvironmentInput,
) -> EnvironmentResult:
    """Calculate comprehensive environmental correction factors.

    Combines environment type, zone, and temperature effects into
    a single set of correction factors.

    Parameters
    ----------
    inp : EnvironmentInput

    Returns
    -------
    EnvironmentResult
    """
    env = inp.environment.lower()
    zone = inp.zone.lower()

    # Base environment correction
    if env == "air":
        life_f = 1.0
        log_a_adj = 0.0
        el_factor = 1.0
        slope_chg = False
        desc = "In-air baseline"

    elif env == "seawater_cp":
        # DNV-RP-C203 Table 2-2: seawater with CP
        # log_a reduced by 0.2–0.4 depending on curve class
        # Using typical 0.2 reduction (factored into S-N curve data)
        life_f = 0.63  # roughly factor 3 on life for class D
        log_a_adj = -0.2
        el_factor = 0.9  # reduced endurance limit
        slope_chg = False

        # Check CP potential
        if inp.cp_potential > -750:
            # Insufficient CP — closer to free corrosion
            life_f *= 0.8
            log_a_adj -= 0.1
            desc = "Seawater with CP (potential above -750 mV — marginal protection)"
        else:
            desc = "Seawater with cathodic protection"

    elif env == "free_corrosion":
        # DNV-RP-C203 Table 2-2: free corrosion
        # Single slope, no endurance limit
        life_f = 0.33  # roughly factor 3 on life vs CP, factor 10 vs air
        log_a_adj = -0.4
        el_factor = 0.0  # no endurance limit
        slope_chg = True
        desc = "Seawater free corrosion (single slope, no endurance limit)"

    else:
        raise ValueError(
            f"Unknown environment '{env}'. "
            "Use 'air', 'seawater_cp', or 'free_corrosion'."
        )

    # Zone correction
    z_factor = zone_factor(zone)
    life_f *= z_factor
    if z_factor < 1.0:
        desc += f"; {zone} zone penalty (×{z_factor})"

    # Temperature derating
    t_factor = temperature_derating_factor(inp.temperature, standard="DNV")
    if t_factor < 1.0:
        # Temperature affects allowable stress range, hence life as S^m
        life_f *= t_factor ** 3.0  # assuming m=3
        desc += f"; temperature derating at {inp.temperature}°C (×{t_factor})"

    return EnvironmentResult(
        life_factor=round(life_f, 4),
        log_a_adjustment=round(log_a_adj, 3),
        endurance_limit_factor=round(el_factor, 3),
        slope_change=slope_chg,
        description=desc,
    )


# ---------------------------------------------------------------------------
# Convenience: apply correction to S-N parameters
# ---------------------------------------------------------------------------

def apply_environment_to_sn(
    log_a: float,
    m: float,
    endurance_limit: Optional[float],
    inp: EnvironmentInput,
) -> dict:
    """Apply environmental correction to S-N curve parameters.

    Parameters
    ----------
    log_a : float
        log10(a) intercept of the S-N curve.
    m : float
        Slope of the S-N curve.
    endurance_limit : float or None
        CAFL in MPa (None if no limit).
    inp : EnvironmentInput

    Returns
    -------
    dict
        Keys: ``"log_a"``, ``"m"``, ``"endurance_limit"``, ``"environment"``.
    """
    result = environment_correction(inp)

    new_log_a = log_a + result.log_a_adjustment

    if endurance_limit is not None:
        new_el = endurance_limit * result.endurance_limit_factor
        if result.endurance_limit_factor <= 0:
            new_el = None
    else:
        new_el = None

    return {
        "log_a": round(new_log_a, 4),
        "m": m,
        "endurance_limit": round(new_el, 2) if new_el is not None else None,
        "environment": inp.environment,
        "life_factor": result.life_factor,
        "description": result.description,
    }
