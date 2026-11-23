"""S-lay pipelay installation analysis -- API RP 1111 Section 6.

Implements bending strain checks for S-lay installation per API RP 1111
(4th Edition, 2009) Section 6.2 Installation Loads.

Sagbend and overbend bending strains are the primary installation checks.
The sagbend governs when the seabed is deep and the stinger is short;
the overbend (over the stinger) governs for shallow/steep stinger angles.
"""
from __future__ import annotations


#: Allowable sagbend bending strain for S-lay installation (API RP 1111 S6.2.1).
#: Conservative limit of 0.5% is widely used for steel pipelines during
#: installation.
EPSILON_B_ALLOW_INSTALLATION: float = 0.005


def slay_sagbend_bending_strain(D: float, R_sagbend: float) -> float:
    """Sagbend bending strain for S-lay installation.

    API RP 1111 (4th Ed.) S6.2.1::

        epsilon_b = D / (2 * R_sagbend)

    Args:
        D:          Outside diameter [m].
        R_sagbend:  Sagbend radius of curvature [m].

    Returns:
        Sagbend bending strain [-].
    """
    if R_sagbend <= 0:
        raise ValueError(f"R_sagbend must be positive, got {R_sagbend}")
    return D / (2.0 * R_sagbend)


def slay_overbend_bending_strain(D: float, R_overbend: float) -> float:
    """Overbend bending strain over the stinger.

    API RP 1111 (4th Ed.) S6.2.1::

        epsilon_b = D / (2 * R_overbend)

    Args:
        D:           Outside diameter [m].
        R_overbend:  Stinger radius of curvature [m].

    Returns:
        Overbend bending strain [-].
    """
    if R_overbend <= 0:
        raise ValueError(f"R_overbend must be positive, got {R_overbend}")
    return D / (2.0 * R_overbend)


def slay_allowable_bending_strain() -> float:
    """Allowable bending strain during installation per API RP 1111 S6.2.1.

    Returns the conservative 0.5% (0.005) limit commonly applied for
    steel pipeline S-lay installation.

    Returns:
        Allowable bending strain [-].
    """
    return EPSILON_B_ALLOW_INSTALLATION


def slay_bending_check(
    D: float,
    R_sagbend: float,
    R_overbend: float,
) -> dict[str, object]:
    """Full S-lay bending strain check per API RP 1111 S6.2.1.

    Evaluates both sagbend and overbend strains against the allowable
    installation bending strain limit (0.5%).

    Args:
        D:           Outside diameter [m].
        R_sagbend:   Sagbend radius of curvature [m].
        R_overbend:  Overbend (stinger) radius of curvature [m].

    Returns:
        Dict with keys:
        - ``pass`` (bool): True when both checks pass.
        - ``sagbend_strain`` (float): Sagbend bending strain.
        - ``overbend_strain`` (float): Overbend bending strain.
        - ``epsilon_b_allow`` (float): Allowable bending strain.
        - ``sagbend_utilization`` (float): sagbend_strain / epsilon_b_allow.
        - ``overbend_utilization`` (float): overbend_strain / epsilon_b_allow.
    """
    eps_allow = slay_allowable_bending_strain()
    eps_sag = slay_sagbend_bending_strain(D=D, R_sagbend=R_sagbend)
    eps_over = slay_overbend_bending_strain(D=D, R_overbend=R_overbend)
    util_sag = eps_sag / eps_allow
    util_over = eps_over / eps_allow

    return {
        "pass": util_sag <= 1.0 and util_over <= 1.0,
        "sagbend_strain": eps_sag,
        "overbend_strain": eps_over,
        "epsilon_b_allow": eps_allow,
        "sagbend_utilization": util_sag,
        "overbend_utilization": util_over,
    }
