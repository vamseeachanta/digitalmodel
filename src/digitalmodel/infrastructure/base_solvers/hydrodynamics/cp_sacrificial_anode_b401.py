"""
Sacrificial anode design calculations per DNV-RP-B401.

Standalone functions operating on a simple I_c-based interface — suitable for
both offshore fixed platforms and pipelines. Each function validates its inputs
and raises ValueError with a descriptive message for non-physical values.

Reference: DNV-RP-B401 (all editions share the same core sizing equations)

Functions
---------
net_anode_mass          — m_net = I_c * T * 8760 / (u * epsilon)
gross_anode_mass        — m_gross = m_net / u
anode_count             — N = ceil(m_gross / m_anode)
anode_resistance_flush  — Dwight: R = (rho/2piL) * (ln(2L/r) - 1)
anode_resistance_bracelet — Modified: R = 0.315 * rho / sqrt(A)
driving_voltage         — E_drive = E_c - E_a
anode_current_output    — I_a = (E_c - E_a) / R_a
"""

import math


# ---------------------------------------------------------------------------
# Internal validation helpers
# ---------------------------------------------------------------------------

def _positive(value, name):
    """Raise ValueError if value is not strictly positive."""
    if value <= 0.0:
        raise ValueError(
            f"{name} must be > 0, got {value}"
        )


def _unit_interval(value, name):
    """Raise ValueError if value is not in the range (0, 1]."""
    if value <= 0.0 or value > 1.0:
        raise ValueError(
            f"{name} must be in (0, 1.0], got {value}"
        )


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def net_anode_mass(*, I_c, T, u, epsilon):
    """Calculate net anode mass required.

    DNV-RP-B401 formula:
        m_net = I_c * T * 8760 / (u * epsilon)

    Parameters
    ----------
    I_c : float
        Protective current demand in amperes (A).  Must be > 0.
    T : float
        Design life in years.  Must be > 0.
    u : float
        Utilization factor (dimensionless), typically 0.8.  Must be in (0, 1].
    epsilon : float
        Electrochemical capacity of the anode material in A·h/kg.
        Typical values: ~780 for zinc, ~2000 for aluminium alloy.
        Must be > 0.

    Returns
    -------
    float
        Net anode mass in kg.
    """
    _positive(I_c, "I_c")
    _positive(T, "T")
    _unit_interval(u, "u")
    _positive(epsilon, "epsilon")
    return I_c * T * 8760.0 / (u * epsilon)


def gross_anode_mass(*, m_net, u):
    """Calculate gross anode mass (accounting for utilization).

    DNV-RP-B401 formula:
        m_gross = m_net / u

    Parameters
    ----------
    m_net : float
        Net anode mass in kg.  Must be > 0.
    u : float
        Utilization factor (dimensionless), typically 0.8.  Must be in (0, 1].

    Returns
    -------
    float
        Gross anode mass in kg.
    """
    _positive(m_net, "m_net")
    _unit_interval(u, "u")
    return m_net / u


def anode_count(*, m_gross, m_anode):
    """Calculate number of individual anodes required.

    Formula: N = ceil(m_gross / m_anode)

    Parameters
    ----------
    m_gross : float
        Total gross anode mass required in kg.  Must be > 0.
    m_anode : float
        Mass of a single anode in kg.  Must be > 0.

    Returns
    -------
    int
        Number of anodes (ceiling division — always rounds up).
    """
    _positive(m_gross, "m_gross")
    _positive(m_anode, "m_anode")
    return math.ceil(m_gross / m_anode)


def anode_resistance_flush(*, rho, L, r):
    """Anode resistance for flush-mounted or stand-off anodes (Dwight formula).

    DNV-RP-B401:
        R_a = (rho / (2 * pi * L)) * (ln(2L/r) - 1)

    Parameters
    ----------
    rho : float
        Seawater resistivity in ohm·m.  Must be > 0.
    L : float
        Anode length in metres.  Must be > 0.
    r : float
        Anode equivalent radius in metres.  Must be > 0.

    Returns
    -------
    float
        Anode resistance in ohms.

    Raises
    ------
    ValueError
        If geometry is too stubby for the Dwight formula (2L/r <= e), or any
        required parameter is non-positive.
    """
    _positive(rho, "rho")
    _positive(L, "L")
    _positive(r, "r")
    ratio = 2.0 * L / r
    if ratio <= math.e:
        raise ValueError(
            f"Anode geometry too stubby for Dwight formula: "
            f"2L/r = {ratio:.4f} must be > e = {math.e:.4f} (require L/r >= 1.36)"
        )
    return (rho / (2.0 * math.pi * L)) * (math.log(ratio) - 1.0)


def anode_resistance_bracelet(*, rho, A):
    """Anode resistance for bracelet-type anodes.

    DNV-RP-B401 modified formula:
        R_a = 0.315 * rho / sqrt(A)

    Parameters
    ----------
    rho : float
        Seawater resistivity in ohm·m.  Must be > 0.
    A : float
        Exposed surface area of the anode in m².  Must be > 0.

    Returns
    -------
    float
        Anode resistance in ohms.
    """
    _positive(rho, "rho")
    if A <= 0.0:
        raise ValueError(f"A must be > 0, got {A}")
    return 0.315 * rho / math.sqrt(A)


def driving_voltage(*, E_c, E_a):
    """Calculate electrochemical driving voltage.

    Formula:
        E_drive = E_c - E_a

    where E_c is the structure protection potential and E_a is the anode
    open-circuit (equilibrium) potential.  The anode must be more negative
    than the structure for galvanic protection to occur.

    Parameters
    ----------
    E_c : float
        Protective (cathodic) potential of the structure in volts (V vs SCE).
        Typical value: -0.80 V for carbon steel in seawater.
    E_a : float
        Anode open-circuit potential in V vs SCE.
        Typical values: -1.05 V (Al-Zn-In alloy), -1.00 V (Zn).

    Returns
    -------
    float
        Driving voltage in volts (always > 0 for valid inputs).

    Raises
    ------
    ValueError
        If E_a >= E_c (zero or reversed driving voltage — no cathodic protection).
    """
    dV = E_c - E_a
    if dV <= 0.0:
        raise ValueError(
            f"Non-positive driving voltage {dV:.6f} V "
            f"(E_c={E_c}, E_a={E_a}). "
            "Anode potential E_a must be more negative than structure potential E_c."
        )
    return dV


def anode_current_output(*, E_c, E_a, R_a):
    """Calculate current output per anode (Ohm's law for the galvanic circuit).

    Formula:
        I_a = (E_c - E_a) / R_a

    Parameters
    ----------
    E_c : float
        Structure protection potential in V vs SCE.
    E_a : float
        Anode open-circuit potential in V vs SCE.  Must be more negative than E_c.
    R_a : float
        Anode resistance in ohms.  Must be > 0.

    Returns
    -------
    float
        Current output per anode in amperes (A).

    Raises
    ------
    ValueError
        If R_a <= 0, or if driving voltage is non-positive (E_a >= E_c).
    """
    if R_a <= 0.0:
        raise ValueError(f"R_a must be > 0, got {R_a}")
    dV = driving_voltage(E_c=E_c, E_a=E_a)
    return dV / R_a
