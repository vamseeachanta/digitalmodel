"""
Cathodic disbonding calculations per ASTM G42 (1996).

ASTM G42: Standard Test Method for Cathodic Disbonding of Pipeline Coatings
Subjected to Elevated Temperatures.

ASTM G42 extends ASTM G80 to elevated-temperature testing.  The measured
disbonding is corrected to an equivalent ambient value using the Arrhenius
relationship, enabling comparison of coatings tested at different temperatures.

Key equations
-------------
Disbonding area (same as G80, Section 10):
    A_d = pi * (r_d^2 - r_h^2)   [mm^2]

Arrhenius temperature correction factor (Section 11):
    f_T = exp(-Ea/R * (1/T_test - 1/T_ref))

Corrected net disbonding radius:
    r_corr = r_net / f_T

Pass/fail check (on corrected value):
    r_corr <= acceptance_radius_mm

Constants
---------
R_GAS                   — Universal gas constant, 8.314 J/(mol·K)
DEFAULT_ACTIVATION_ENERGY — 60 000 J/mol (typical for FBE/polyethylene coatings)
"""

import math
from dataclasses import dataclass


R_GAS = 8.314                    # J/(mol·K)
DEFAULT_ACTIVATION_ENERGY = 60_000.0  # J/mol


@dataclass
class G42TestResult:
    """Result from an ASTM G42 elevated-temperature cathodic disbonding evaluation."""

    holiday_radius_mm: float
    disbonding_radius_mm: float
    test_temperature_c: float
    disbonding_area_mm2: float
    net_disbonding_mm: float
    temperature_correction_factor: float
    corrected_net_disbonding_mm: float
    passes: bool
    acceptance_radius_mm: float  # None when no acceptance criterion is specified


# ---------------------------------------------------------------------------
# Standalone calculation functions
# ---------------------------------------------------------------------------

def arrhenius_correction_factor(
    test_temperature_c,
    reference_temperature_c=25.0,
    activation_energy_j_mol=DEFAULT_ACTIVATION_ENERGY,
):
    """Arrhenius temperature correction factor per ASTM G42 Section 11.

    f_T = exp(-Ea/R * (1/T_test - 1/T_ref))

    At elevated temperatures (T_test > T_ref), f_T > 1, meaning the coating
    disbonds more rapidly.  Dividing the measured disbonding by f_T normalises
    the result to the reference temperature.

    Parameters
    ----------
    test_temperature_c : float
        Test temperature [°C].  Valid range: -50 to 200 °C.
    reference_temperature_c : float
        Reference temperature [°C], default 25 °C.
    activation_energy_j_mol : float
        Activation energy [J/mol], default 60 000 J/mol.

    Returns
    -------
    float
        Correction factor f_T (dimensionless).  Equal to 1.0 at reference
        temperature; > 1.0 above reference.
    """
    if test_temperature_c < -50.0 or test_temperature_c > 200.0:
        raise ValueError(
            f"test_temperature_c out of range [-50, 200] °C, got {test_temperature_c}"
        )
    if reference_temperature_c < -50.0 or reference_temperature_c > 200.0:
        raise ValueError(
            "reference_temperature_c out of range [-50, 200] °C, "
            f"got {reference_temperature_c}"
        )
    if activation_energy_j_mol <= 0.0:
        raise ValueError(
            f"activation_energy_j_mol must be > 0, got {activation_energy_j_mol}"
        )
    t_test_k = test_temperature_c + 273.15
    t_ref_k = reference_temperature_c + 273.15
    exponent = -(activation_energy_j_mol / R_GAS) * (1.0 / t_test_k - 1.0 / t_ref_k)
    return math.exp(exponent)


def evaluate_g42(
    holiday_radius_mm,
    disbonding_radius_mm,
    test_temperature_c,
    acceptance_radius_mm=None,
    reference_temperature_c=25.0,
    activation_energy_j_mol=DEFAULT_ACTIVATION_ENERGY,
):
    """Evaluate an ASTM G42 elevated-temperature cathodic disbonding test result.

    The measured net disbonding is temperature-corrected using the Arrhenius
    relationship before being compared to the acceptance criterion.

    Parameters
    ----------
    holiday_radius_mm : float
        Holiday radius [mm].  Must be > 0.
    disbonding_radius_mm : float
        Measured total disbonding radius from holiday centre [mm].
        Must be >= holiday_radius_mm.
    test_temperature_c : float
        Temperature at which the test was conducted [°C].
    acceptance_radius_mm : float or None
        Maximum allowable corrected net disbonding radius [mm].
        When None, no pass/fail check is applied.
    reference_temperature_c : float
        Reference temperature for correction, default 25 °C.
    activation_energy_j_mol : float
        Coating activation energy [J/mol], default 60 000 J/mol.

    Returns
    -------
    G42TestResult
    """
    if holiday_radius_mm <= 0.0:
        raise ValueError(
            f"holiday_radius_mm must be > 0, got {holiday_radius_mm}"
        )
    if disbonding_radius_mm < holiday_radius_mm:
        raise ValueError(
            "disbonding_radius_mm must be >= holiday_radius_mm, "
            f"got {disbonding_radius_mm} < {holiday_radius_mm}"
        )
    if acceptance_radius_mm is not None and acceptance_radius_mm <= 0.0:
        raise ValueError(
            f"acceptance_radius_mm must be > 0, got {acceptance_radius_mm}"
        )

    area = math.pi * (disbonding_radius_mm**2 - holiday_radius_mm**2)
    net_r = disbonding_radius_mm - holiday_radius_mm
    f_t = arrhenius_correction_factor(
        test_temperature_c, reference_temperature_c, activation_energy_j_mol
    )
    corrected_net_r = net_r / f_t

    if acceptance_radius_mm is None:
        passes = True
    else:
        passes = corrected_net_r <= acceptance_radius_mm

    return G42TestResult(
        holiday_radius_mm=holiday_radius_mm,
        disbonding_radius_mm=disbonding_radius_mm,
        test_temperature_c=test_temperature_c,
        disbonding_area_mm2=round(area, 3),
        net_disbonding_mm=round(net_r, 3),
        temperature_correction_factor=round(f_t, 4),
        corrected_net_disbonding_mm=round(corrected_net_r, 3),
        passes=passes,
        acceptance_radius_mm=acceptance_radius_mm,
    )
