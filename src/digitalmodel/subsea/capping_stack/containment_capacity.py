"""Capping-stack containment pressure adequacy (API 17W / API STD 53).

This module encodes the *central* deterministic safety check for a subsea
capping stack: can the stack's rated working pressure (RWP) contain the
maximum pressure the un-controlled well can present at the wellhead?

After Macondo, a capping stack (API 17W) is the primary source-control
device lowered onto a flowing wellhead to shut in / cap the well.  The
governing acceptance check is a pressure-containment comparison:

    the stack RWP must equal or exceed the Maximum Anticipated Shut-In
    Wellhead Pressure (SIWHP) that develops once the well is capped.

Equations
---------
Shut-in wellhead pressure for a capped well is the reservoir pressure at
datum less the hydrostatic head of the wellbore column between the datum
and the mudline (API STD 53 / well-control hydrostatics):

    p_hydrostatic = rho * g * h                                     (1)
    SIWHP         = p_res - p_hydrostatic                           (2)

where ``rho`` is the density of the wellbore column fluid that fills the
well once it is shut in (lightest credible column governs — a gas column
gives the highest SIWHP and is the design case), ``h`` is the true
vertical depth from the reservoir datum to the mudline, and ``g`` the
gravitational acceleration.

The containment utilisation and verdict are

    U_containment = SIWHP / RWP                                     (3)
    is_contained  = (SIWHP <= RWP)  i.e.  U_containment <= 1.0      (4)

Notes / assumptions
-------------------
* SIWHP uses the *lightest* credible shut-in column (gas) as the design
  case — this maximises SIWHP and is conservative for sizing the stack.
  Pass ``column_density`` explicitly to model a specific fluid column.
* A single static hydrostatic gradient is assumed (no friction, no
  thermal/compositional gradient).  A compositional gas-gradient or a
  transient flowing-well model is a deferred follow-on (see issue #490
  containment-flow scope).
* RWP is the stack rated working pressure from the API 17W pressure class
  (e.g. 10,000 / 15,000 / 20,000 psi); see ``interface_check`` for the
  class enum.
"""

from __future__ import annotations

from dataclasses import dataclass, field

G = 9.80665  # standard gravity [m/s^2]

# Representative shut-in column densities [kg/m^3].
GAS_COLUMN_DENSITY = 200.0       # dense hydrocarbon gas at high P (design / lightest)
OIL_COLUMN_DENSITY = 800.0       # live-oil column
BRINE_COLUMN_DENSITY = 1030.0    # seawater / completion brine


@dataclass
class ContainmentResult:
    """Result of a capping-stack containment-pressure adequacy check."""

    siwhp: float                 # shut-in wellhead pressure [Pa]
    rated_working_pressure: float  # stack RWP [Pa]
    hydrostatic_head: float      # column hydrostatic pressure [Pa]
    utilisation: float           # SIWHP / RWP  [-]
    is_contained: bool
    column_density: float        # density used for the shut-in column [kg/m^3]
    notes: list = field(default_factory=list)


def shut_in_wellhead_pressure(
    reservoir_pressure: float,
    datum_to_mudline_tvd: float,
    column_density: float = GAS_COLUMN_DENSITY,
) -> float:
    """Maximum shut-in wellhead pressure once the well is capped, Eq. (1)-(2).

    Parameters
    ----------
    reservoir_pressure : float
        Reservoir pressure at datum p_res [Pa].
    datum_to_mudline_tvd : float
        True vertical depth from reservoir datum to the mudline h [m].
    column_density : float
        Density of the shut-in wellbore column rho [kg/m^3].  Defaults to a
        gas column (lightest credible, highest SIWHP -> design case).

    Returns
    -------
    float
        Shut-in wellhead pressure SIWHP [Pa].  Clamped at >= 0.
    """
    p_hydrostatic = column_density * G * datum_to_mudline_tvd
    return max(reservoir_pressure - p_hydrostatic, 0.0)


def check_containment(
    rated_working_pressure: float,
    reservoir_pressure: float,
    datum_to_mudline_tvd: float,
    column_density: float = GAS_COLUMN_DENSITY,
) -> ContainmentResult:
    """Verify a capping stack contains the well shut-in pressure (API 17W).

    Parameters
    ----------
    rated_working_pressure : float
        Stack rated working pressure RWP [Pa] (API 17W pressure class).
    reservoir_pressure : float
        Reservoir pressure at datum [Pa].
    datum_to_mudline_tvd : float
        TVD from reservoir datum to mudline [m].
    column_density : float
        Shut-in column density [kg/m^3]; defaults to a gas column.

    Returns
    -------
    ContainmentResult
        SIWHP, utilisation = SIWHP / RWP, and ``is_contained`` verdict.
    """
    notes: list = []
    p_hydrostatic = column_density * G * datum_to_mudline_tvd
    siwhp = shut_in_wellhead_pressure(
        reservoir_pressure, datum_to_mudline_tvd, column_density
    )
    if rated_working_pressure <= 0.0:
        raise ValueError("rated_working_pressure must be positive")
    utilisation = siwhp / rated_working_pressure
    is_contained = siwhp <= rated_working_pressure

    if column_density > GAS_COLUMN_DENSITY:
        notes.append(
            "Column heavier than gas modelled; gas column gives the highest "
            "SIWHP and is the conservative design case for stack sizing."
        )
    if siwhp <= 0.0:
        notes.append(
            "Hydrostatic head exceeds reservoir pressure: well does not flow "
            "to surface under this column (SIWHP clamped to 0)."
        )

    return ContainmentResult(
        siwhp=siwhp,
        rated_working_pressure=rated_working_pressure,
        hydrostatic_head=p_hydrostatic,
        utilisation=utilisation,
        is_contained=is_contained,
        column_density=column_density,
        notes=notes,
    )
