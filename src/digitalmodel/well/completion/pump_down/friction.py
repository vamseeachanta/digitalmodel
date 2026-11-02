"""Water and optional power-law pipe-friction screens; Darcy factors throughout.

Classical correlations: Swamee & Jain (1976), doi:10.1061/JYCEAJ.0004542;
Dodge & Metzner (1959), AIChE J. 5(2), 189-204, doi:10.1002/aic.690050214;
Metzner & Reed (1955), AIChE J. 1(4), 434-440, doi:10.1002/aic.690010409.
Virk (1975), AIChE J. 21(4), 625-656, doi:10.1002/aic.690210402, supplies
the polymer-drag-reduction context, NOT the concentration saturation fit here.
The latter is an explicitly empirical task assumption requiring measured fits.
"""

import math

from digitalmodel.common.hydraulics import swamee_jain_friction_factor
from digitalmodel.units import Q_

from ._validation import finite, scalar


def reynolds_number(density, velocity, diameter, viscosity) -> float:
    """Return Re=rho*abs(v)*D/mu; quantities or bare SI inputs.

    Source: DOE-HDBK-1012/3-92, Fluid Flow, 'Reynolds Number'.
    """
    rho = scalar(density, "kg/m**3", "density", positive=True)
    speed = scalar(velocity, "m/s", "velocity", minimum=-math.inf)
    d = scalar(diameter, "m", "diameter", positive=True)
    mu = scalar(viscosity, "Pa*s", "dynamic viscosity", positive=True)
    return rho * abs(speed) * d / mu


def annulus_hydraulic_diameter(casing_id, tool_od):
    """Return Dh=4A/P=Dc-Dt [m] for a concentric annulus.

    Source: DOE-HDBK-1012/3-92, Fluid Flow, 'Equivalent Diameter'.
    This does not correct for annular eccentricity or noncircular flow.
    """
    dc = scalar(casing_id, "m", "casing ID", positive=True)
    dt = scalar(tool_od, "m", "tool OD")
    if dt >= dc:
        raise ValueError("tool OD must be less than casing ID")
    return Q_(dc - dt, "m")


def darcy_weisbach_gradient(
    density, velocity, diameter, viscosity, roughness=Q_(0.0018, "inch"), reduction=0.0
):
    """Return f_D*rho*v**2/(2D)*(1-DR) [Pa/m], quantities or bare SI.

    DOE-HDBK-1012/3-92, Fluid Flow, 'Head Loss', Darcy's equation;
    f_D uses Swamee-Jain (citation in shared helper). Roughness 0.0018 in
    is a REPRESENTATIVE task input, not a measured casing roughness. Field
    shortcut dp/dL[psi/ft]=f*rho[ppg]*v[ft/s]**2/(25.81*D[in]) takes the
    FANNING factor (25.81 comes from 2*f_F); with Darcy use 4*25.81. Verify
    with casing condition. DR is a measured fractional reduction in [0,1).
    The signed gradient follows the velocity; zero flow returns zero loss.
    """
    rho = scalar(density, "kg/m**3", "density", positive=True)
    speed = scalar(velocity, "m/s", "velocity", minimum=-math.inf)
    d = scalar(diameter, "m", "diameter", positive=True)
    eps = scalar(roughness, "m", "roughness")
    dr = finite(reduction, "drag reduction", maximum=1)
    if dr == 1 or eps >= d:
        raise ValueError("drag reduction must be < 1 and roughness < diameter")
    re = reynolds_number(density, velocity, diameter, viscosity)
    f_d = swamee_jain_friction_factor(re, eps / d) if re else 0.0
    return Q_(f_d * rho * speed * abs(speed) / (2 * d) * (1 - dr), "Pa/m")


def drag_reduction(concentration, *, dr_max, c50) -> float:
    """Return DRmax*c/(c+c50), a task-specified empirical saturation fit.

    No default DRmax/c50: both require a measured calibration. Concentrations
    must be nonnegative numbers in the SAME caller-selected concentration
    unit. Virk (1975), doi:10.1002/aic.690210402, discusses drag reduction;
    this rational fit is not attributed to Virk or an MDR correlation.
    """
    c = finite(concentration, "concentration")
    maximum = finite(dr_max, "DR maximum", maximum=1)
    half = scalar(c50, "dimensionless", "c50", positive=True)
    if maximum == 1:
        raise ValueError("DR maximum must be < 1")
    return maximum * c / (c + half)


def power_law_reynolds(density, velocity, diameter, consistency, n: float) -> float:
    """Generalized Re=rho*v**(2-n)*D**n/[K*8**(n-1)*((3n+1)/(4n))**n].

    Metzner & Reed (1955), doi:10.1002/aic.690010409, generalized Reynolds
    number for tau=K*gamma_dot**n. K units are Pa*s**n, n is dimensionless.
    This implementation supports shear-thinning through Newtonian, 0.3<=n<=1,
    matching the scope of the Dodge-Metzner experimental abstract (1959).
    """
    index = finite(n, "power-law index", minimum=0.3, maximum=1)
    rho = scalar(density, "kg/m**3", "density", positive=True)
    speed = abs(scalar(velocity, "m/s", "velocity", minimum=-math.inf))
    d = scalar(diameter, "m", "diameter", positive=True)
    k = scalar(consistency, f"Pa*s**{index}", "consistency", positive=True)
    return (
        rho
        * speed ** (2 - index)
        * d**index
        / (k * 8 ** (index - 1) * ((3 * index + 1) / (4 * index)) ** index)
    )


def dodge_metzner_factor(generalized_reynolds: float, n: float) -> float:
    """Return Darcy=4*Fanning using the implicit smooth-pipe resistance law.

    Dodge & Metzner (1959), doi:10.1002/aic.690050214, final resistance law:
    1/sqrt(f_F)=4/n**0.75*log10(Re_g*f_F**(1-n/2))-0.4/n**1.2.
    Laminar Darcy=64/Re_g below 2100; transition is not modelled. Smooth
    round pipe only: annulus use is an uncalibrated hydraulic-diameter screen.
    """
    re = scalar(
        generalized_reynolds, "dimensionless", "generalized Reynolds", positive=True
    )
    index = finite(n, "power-law index", minimum=0.3, maximum=1)
    if re < 2100:
        return 64 / re
    # Solve in x=1/sqrt(f_F); residual is strictly increasing for 0.3<=n<=1.
    low, high = 1e-6, 1e6
    for _ in range(100):
        x = (low + high) / 2
        residual = (
            x
            - 4 / index**0.75 * (math.log10(re) - (2 - index) * math.log10(x))
            + 0.4 / index**1.2
        )
        if residual > 0:
            high = x
        else:
            low = x
    return 4 / ((low + high) / 2) ** 2
