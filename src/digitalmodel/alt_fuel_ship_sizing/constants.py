# ABOUTME: Published physical constants for the alternative-fuel (LH2) ship-design
# ABOUTME: sizing toolkit, with source citations for every value.
"""Published constants for LH2 / alternative-fuel ship sizing.

All values are public, published data. Sources:

- **Liquid parahydrogen at the normal boiling point (NBP, 20.28 K,
  101.325 kPa)**: McCarty, R. D., Hord, J., and Roder, H. M., *Selected
  Properties of Hydrogen (Engineering Design Data)*, NBS Monograph 168,
  U.S. National Bureau of Standards, 1981; and the NIST Chemistry WebBook
  (thermophysical properties from Leachman, J. W., et al., "Fundamental
  Equations of State for Parahydrogen, Normal Hydrogen, and Orthohydrogen",
  *J. Phys. Chem. Ref. Data* 38(3), 2009):

  - saturated liquid density ~70.85 kg/m3
  - latent heat of vaporization ~446 kJ/kg
  - NBP temperature 20.28 K

- **Hydrogen lower heating value** 119.96 MJ/kg: U.S. DOE Hydrogen Program
  *Hydrogen Analysis Resource Center* / NIST-based lower heating value of
  hydrogen (also quoted as 119.93-120.0 MJ/kg across handbooks; ISO 14687
  and the DOE H2 data book use 119.96 MJ/kg).

- **Unit conversions**: 1 international nautical mile = 1852 m (exact) and
  1 knot = 1852/3600 m/s (exact), per the International System of Units
  (SI) brochure, BIPM, Table 8 (non-SI units accepted for use with the SI).

- **Tonnage coefficients** (:mod:`digitalmodel.alt_fuel_ship_sizing.tonnage`):
  International Convention on Tonnage Measurement of Ships, 1969 (ITC 69),
  Annex I, Regulations 3 (gross tonnage) and 4 (net tonnage).
"""

from __future__ import annotations

# --- Liquid hydrogen (parahydrogen) at NBP -------------------------------
# NBS Monograph 168 (1981); NIST Chemistry WebBook (Leachman et al. 2009).
LH2_DENSITY_KG_PER_M3 = 70.85
"""Saturated liquid parahydrogen density at NBP (20.28 K, 101.325 kPa)."""

LH2_HEAT_OF_VAPORIZATION_KJ_PER_KG = 446.0
"""Latent heat of vaporization of parahydrogen at NBP."""

LH2_NBP_K = 20.28
"""Normal boiling point of parahydrogen at 101.325 kPa."""

H2_LOWER_HEATING_VALUE_MJ_PER_KG = 119.96
"""Hydrogen lower heating value (DOE Hydrogen Analysis Resource Center)."""

# --- Unit conversions (exact definitions, BIPM SI brochure Table 8) ------
NAUTICAL_MILE_M = 1852.0
"""International nautical mile in metres (exact)."""

KNOT_MPS = NAUTICAL_MILE_M / 3600.0
"""One knot in metres per second (exact, = 0.5144...)."""

SECONDS_PER_HOUR = 3600.0
SECONDS_PER_DAY = 86400.0
HOURS_PER_DAY = 24.0
