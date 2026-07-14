# ABOUTME: Published physical constants for the alternative-fuel ship-design
# ABOUTME: sizing toolkit (LH2, NH3, methanol), with source citations per value.
"""Published constants for alternative-fuel ship sizing (LH2, NH3, methanol).

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

- **Anhydrous ammonia, saturated liquid at the NBP (239.82 K,
  101.325 kPa)** -- the *fully refrigerated* storage condition: NIST
  Chemistry WebBook, saturation properties of ammonia (CAS 7664-41-7):

  - NBP temperature 239.82 K (-33.33 degC)
  - saturated liquid density ~682 kg/m3 (published values span
    ~681.9-682.8 kg/m3 across equation-of-state formulations)
  - latent heat of vaporization ~23.33 kJ/mol = ~1370 kJ/kg
    (commonly quoted 1370-1371 kJ/kg)

- **Ammonia lower heating value** 18.6 MJ/kg (gaseous NH3 basis), from
  standard-state enthalpies of formation (NIST Chemistry WebBook /
  NIST-JANAF): NH3(g) + 3/4 O2 -> 1/2 N2 + 3/2 H2O(g), dH = -316.8 kJ/mol
  = 18.60 MJ/kg. FLAGGED: handbooks quote 18.6-18.8 MJ/kg depending on
  reference state (gas vs liquid feed); the gas-basis 18.6 is used here.

- **Methanol** (CAS 67-56-1):

  - liquid density 791.4 kg/m3 at 20 degC (CRC Handbook of Chemistry and
    Physics, 0.7914 g/cm3 at 20 degC; ~786.5 kg/m3 at 25 degC -- the value
    is temperature-sensitive at the 0.5 % level across ambient conditions)
  - lower heating value 19.9 MJ/kg, derived from the NIST Chemistry WebBook
    standard enthalpy of combustion of liquid methanol (dcH ~= -726.1
    kJ/mol, HHV basis) minus the vaporization enthalpy of the 2 mol product
    water: (726.1 - 2 x 44.0) / 32.04 g/mol = 19.92 MJ/kg. FLAGGED:
    published LHVs span ~19.9-20.1 MJ/kg (e.g. the DOE Alternative Fuels
    Data Center Btu/gal figure converts to ~20.1); the thermochemically
    derived 19.9 is used here.
  - latent heat of vaporization ~35.2 kJ/mol = ~1100 kJ/kg at the NBP
    (337.8 K), NIST Chemistry WebBook. FLAGGED: rounded from ~1099 kJ/kg;
    the 25 degC value is higher (~37.4 kJ/mol = ~1170 kJ/kg). Methanol is
    stored as an ambient liquid, so this constant is not exercised by the
    boil-off model in normal use (see the preset validity notes).
  - NBP 337.8 K (64.7 degC), NIST Chemistry WebBook.

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

# --- Anhydrous ammonia, saturated liquid at NBP (fully refrigerated) ------
# NIST Chemistry WebBook, saturation properties of ammonia (CAS 7664-41-7).
NH3_LIQUID_DENSITY_KG_PER_M3 = 682.0
"""Saturated liquid ammonia density at the NBP (239.82 K, 101.325 kPa).

Published values span ~681.9-682.8 kg/m3 across EOS formulations; valid for
*fully refrigerated* storage only -- pressurized ambient-temperature storage
(~8.6 bar(a) at 20 degC) has a markedly lower liquid density (~610 kg/m3,
not provided as a constant here).
"""

NH3_HEAT_OF_VAPORIZATION_KJ_PER_KG = 1370.0
"""Latent heat of vaporization of ammonia at the NBP (~23.33 kJ/mol,
NIST Chemistry WebBook; commonly quoted 1370-1371 kJ/kg)."""

NH3_NBP_K = 239.82
"""Normal boiling point of ammonia at 101.325 kPa (-33.33 degC)."""

NH3_LOWER_HEATING_VALUE_MJ_PER_KG = 18.6
"""Ammonia lower heating value, gaseous-NH3 basis (from NIST standard
enthalpies of formation: -316.8 kJ/mol = 18.60 MJ/kg).

FLAGGED: handbooks quote 18.6-18.8 MJ/kg depending on the reference state.
"""

# --- Methanol (ambient liquid) --------------------------------------------
# CRC Handbook of Chemistry and Physics; NIST Chemistry WebBook (CAS 67-56-1).
METHANOL_DENSITY_KG_PER_M3 = 791.4
"""Liquid methanol density at 20 degC (CRC Handbook, 0.7914 g/cm3;
~786.5 kg/m3 at 25 degC -- temperature-sensitive at the 0.5 % level)."""

METHANOL_HEAT_OF_VAPORIZATION_KJ_PER_KG = 1100.0
"""Latent heat of vaporization of methanol at the NBP (337.8 K), ~35.2
kJ/mol (NIST Chemistry WebBook).

FLAGGED: rounded from ~1099 kJ/kg; ~1170 kJ/kg at 25 degC. Not exercised by
the boil-off model for ambient-liquid storage (see the methanol preset).
"""

METHANOL_NBP_K = 337.8
"""Normal boiling point of methanol at 101.325 kPa (64.7 degC),
NIST Chemistry WebBook."""

METHANOL_LOWER_HEATING_VALUE_MJ_PER_KG = 19.9
"""Methanol lower heating value, derived from the NIST standard enthalpy of
combustion of the liquid (~-726.1 kJ/mol HHV) minus product-water
vaporization: 19.92 MJ/kg.

FLAGGED: published values span ~19.9-20.1 MJ/kg (DOE AFDC converts to
~20.1); the thermochemically derived 19.9 is used here.
"""

AMBIENT_TEMPERATURE_K = 293.15
"""Reference ambient temperature (20 degC) used as the storage temperature
for ambient-liquid fuels (methanol)."""

# --- Unit conversions (exact definitions, BIPM SI brochure Table 8) ------
NAUTICAL_MILE_M = 1852.0
"""International nautical mile in metres (exact)."""

KNOT_MPS = NAUTICAL_MILE_M / 3600.0
"""One knot in metres per second (exact, = 0.5144...)."""

SECONDS_PER_HOUR = 3600.0
SECONDS_PER_DAY = 86400.0
HOURS_PER_DAY = 24.0
