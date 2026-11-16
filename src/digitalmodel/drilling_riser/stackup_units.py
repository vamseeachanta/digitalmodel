"""Exact conventional unit definitions at the static stack-up boundary.

These constants define units; they are not engineering factors or site inputs.
The pound-force definition uses conventional standard gravity, independently
of the caller's gravitational acceleration in physical calculations.
"""

from digitalmodel.drilling_riser.stackup_calculation import _finite

_POUND_KG = 0.45359237
_INCH_M = 0.0254
_FOOT_M = 0.3048
_POUND_FORCE_N = _POUND_KG * 9.80665
_US_GALLON_M3 = 231.0 * _INCH_M**3


def _scaled(value: float, factor: float) -> float:
    result = value * factor
    _finite("converted value", result)
    return result


def feet_to_m(length_ft: float) -> float:
    """Convert ft to m using the exact definition 1 ft = 0.3048 m."""
    _finite("length_ft", length_ft)
    return _scaled(length_ft, _FOOT_M)


def inches_to_m(length_in: float) -> float:
    """Convert inches to m using the exact definition 1 in = 0.0254 m."""
    _finite("length_in", length_in)
    return _scaled(length_in, _INCH_M)


def pounds_mass_to_kg(mass_lb: float) -> float:
    """Convert lb mass to kg: 1 lb = 0.45359237 kg exactly."""
    _finite("mass_lb", mass_lb)
    return _scaled(mass_lb, _POUND_KG)


def kips_to_n(force_kip: float) -> float:
    """Convert 1000 lbf to N; 1 lbf = 0.45359237*9.80665 N exactly."""
    _finite("force_kip", force_kip)
    return _scaled(force_kip, 1000.0 * _POUND_FORCE_N)


def n_to_kips(force_n: float) -> float:
    """Convert N to kips, inverse of :func:`kips_to_n`."""
    _finite("force_n", force_n)
    return _scaled(force_n, 1.0 / (1000.0 * _POUND_FORCE_N))


def ppg_to_kg_m3(density_ppg: float) -> float:
    """Convert lb per US liquid gallon to kg/m3; 1 US gal = 231 in3."""
    _finite("density_ppg", density_ppg, 0.0)
    return _scaled(density_ppg, _POUND_KG / _US_GALLON_M3)


def ksi_to_pa(stress_ksi: float) -> float:
    """Convert 1000 lbf/in2 to Pa using exact force and length definitions."""
    _finite("stress_ksi", stress_ksi, 0.0)
    return _scaled(stress_ksi, 1000.0 * _POUND_FORCE_N / _INCH_M**2)
