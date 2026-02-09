"""
Dynacard Analysis Constants

This module contains all physical constants, conversion factors, and empirical
values used in dynacard (dynamometer card) analysis for sucker rod pump systems.

References:
    - API RP 11L: Recommended Practice for Design Calculations for Sucker Rod Pumping Systems
    - API 11E: Specification for Pumping Units
    - Gibbs, S.G.: "Predicting the Behavior of Sucker-Rod Pumping Systems" (JPT, 1963)
"""

# =============================================================================
# UNIT CONVERSION FACTORS
# =============================================================================

FEET_TO_INCHES = 12.0
"""Conversion factor from feet to inches."""

CUBIC_FEET_TO_CUBIC_INCHES = 12.0 ** 3  # 1728
"""Conversion factor from cubic feet to cubic inches."""

SECONDS_PER_MINUTE = 60.0
"""Seconds in one minute, used for SPM (strokes per minute) calculations."""

SQUARE_FEET_TO_SQUARE_INCHES = 144.0
"""Conversion factor from square feet to square inches."""


# =============================================================================
# MATERIAL PROPERTIES - STEEL
# =============================================================================

STEEL_YOUNGS_MODULUS_PSI = 30.5e6
"""
Young's modulus (elastic modulus) for steel rod material in psi.
Typical value for carbon steel sucker rods per API 11B.
Range: 29-31 x 10^6 psi depending on grade.
"""

STEEL_DENSITY_LB_PER_FT3 = 490.0
"""
Density of steel in pounds per cubic foot.
Standard value for carbon steel: 490 lb/ft³ (7850 kg/m³).
"""

STEEL_DENSITY_LB_PER_IN3 = STEEL_DENSITY_LB_PER_FT3 / CUBIC_FEET_TO_CUBIC_INCHES
"""Density of steel in pounds per cubic inch (derived)."""

DEFAULT_MODULUS_OF_ELASTICITY = 30_000_000.0
"""Default Young's modulus for rod sections in psi (30 x 10^6)."""

DEFAULT_ROD_DENSITY = 490.0
"""Default rod string density in lb/ft³."""

DEFAULT_DAMPING_FACTOR = 0.05
"""
Default structural damping factor for rod string.
Dimensionless. Typical range: 0.01-0.10 depending on well conditions.
"""


# =============================================================================
# FLUID PROPERTIES
# =============================================================================

DEFAULT_FLUID_DENSITY_LB_PER_FT3 = 62.4
"""
Default fluid density in lb/ft³.
Fresh water at standard conditions: 62.4 lb/ft³.
"""


# =============================================================================
# PHYSICAL CONSTANTS
# =============================================================================

GRAVITATIONAL_ACCELERATION_FT_PER_S2 = 32.2
"""Standard gravitational acceleration in feet per second squared."""

GRAVITATIONAL_ACCELERATION_IN_PER_S2 = GRAVITATIONAL_ACCELERATION_FT_PER_S2 * FEET_TO_INCHES
"""Gravitational acceleration in inches per second squared (derived)."""


# =============================================================================
# WAVE EQUATION PARAMETERS (GIBBS METHOD)
# =============================================================================

WAVE_SPEED_FT_PER_S = 16_300.0
"""
Speed of stress wave propagation in steel rod string in feet per second.
Legacy "gold standard" value from Gibbs analytical method.
Theoretical: a = sqrt(E/rho) ≈ 16,300-16,800 ft/s for steel.
"""

WAVE_SPEED_IN_PER_S = WAVE_SPEED_FT_PER_S * FEET_TO_INCHES
"""Wave speed in inches per second (derived)."""

GIBBS_DAMPING_COEFFICIENT = 0.012
"""
Viscous damping coefficient for Gibbs frequency-domain solver.
Empirical value calibrated against field data for parity with
legacy implementation. Accounts for fluid friction, rod/tubing contact,
and material hysteresis losses.
Dimensionless in normalized form.
"""


# =============================================================================
# EMPIRICAL CORRECTION FACTORS (LEGACY CALIBRATION)
# =============================================================================

BUOYANCY_FACTOR = 0.825
"""
Buoyancy correction factor for rod weight in fluid.
Represents (1 - rho_fluid/rho_steel) for typical oil/water mixtures.
Empirical value from legacy "gold standard" implementation.
Theoretical range: 0.80-0.90 depending on fluid density.
"""

LOAD_ATTENUATION_FACTOR = 0.88
"""
Load attenuation correction factor for frequency-domain solver.
Compensates for energy losses not captured by simplified wave equation.
Empirical calibration value from legacy implementation.
Applied to downhole load calculation for parity alignment.
"""

STROKE_SCALING_FACTOR = 0.96
"""
Stroke position scaling factor for final alignment.
Empirical "tweak" to achieve <2% stroke error versus measured data.
Applied after inverse FFT to synchronize calculated stroke.
"""


# =============================================================================
# DIAGNOSTIC THRESHOLDS
# =============================================================================

PUMP_TAGGING_LOAD_THRESHOLD_LBS = 38_000
"""
Maximum load threshold for pump tagging detection in pounds.
Loads exceeding this indicate mechanical contact between plunger
and standing valve or pump top. API guideline based on typical
pump barrel ratings.
"""

FLUID_POUND_LOAD_DIFF_THRESHOLD_LBS = 5_000
"""
Load differential threshold for fluid pound detection in pounds.
Sharp load drops exceeding this during downstroke indicate
incomplete pump fillage causing fluid pound impact.
"""

GAS_INTERFERENCE_MIN_LOAD_THRESHOLD_LBS = 2_000
"""
Minimum load threshold for gas interference detection in pounds.
Very low loads during downstroke suggest gas compression in
pump barrel reducing fluid load transfer.
"""

BUCKLING_DETECTION_LOAD_THRESHOLD_LBS = -500
"""
Compression load threshold for rod buckling detection in pounds.
Negative loads (compression) below this indicate potential
sinusoidal or helical buckling in the rod string.
"""


# =============================================================================
# DEFAULT ANALYSIS VALUES
# =============================================================================

DEFAULT_PUMP_FILLAGE = 0.85
"""
Default pump fillage (volumetric efficiency) as decimal.
85% is typical for wells operating within normal parameters.
Range: 0.0 (empty) to 1.0 (full stroke).
"""


# =============================================================================
# ROD MATERIAL SPECIFICATIONS
# =============================================================================

DEFAULT_MINIMUM_TENSILE_STRENGTH_PSI = 115_000
"""
Default minimum tensile strength for sucker rods in psi.
API Grade KD rods: 115,000 psi minimum.
Range varies by grade: C (90,000), K (85,000), D (115,000).
"""


# =============================================================================
# SI UNIT CONSTANTS (for unit system flexibility)
# =============================================================================

GRAVITATIONAL_ACCELERATION_SI = 9.81
"""Standard gravitational acceleration in m/s² (SI units)."""

DENSITY_OF_WATER_SI_KG_PER_M3 = 1000.0
"""Density of fresh water in kg/m³ (SI units)."""

DENSITY_OF_STEEL_SI_KG_PER_M3 = 7850.0
"""Density of steel in kg/m³ (SI units)."""


# =============================================================================
# EFFICIENCY FACTORS
# =============================================================================

DEFAULT_PRIME_MOVER_EFFICIENCY = 0.85
"""
Default prime mover (motor) efficiency as decimal.
Typical range: 0.80-0.95 depending on motor type and loading.
"""

DEFAULT_PUMPING_UNIT_EFFICIENCY = 0.90
"""
Default pumping unit mechanical efficiency as decimal.
Accounts for gear reducer, bearing, and structural losses.
Typical range: 0.85-0.95.
"""

DEFAULT_PUMP_EFFICIENCY = 1.0
"""
Default pump volumetric efficiency as decimal.
1.0 assumes ideal pump with no leakage.
Actual range: 0.70-0.95 depending on condition.
"""


# =============================================================================
# FINITE DIFFERENCE SOLVER PARAMETERS
# =============================================================================

DEFAULT_FD_NUM_NODES = 100
"""
Default number of spatial nodes for finite difference solver.
More nodes = higher accuracy but slower computation.
Typical range: 50-200.
"""


# =============================================================================
# FEATURE EXTRACTION PARAMETERS
# =============================================================================

BEZERRA_N_BINS = 8
"""Number of position bins per half-cycle for Bezerra vertical projections."""

N_FAILURE_MODES = 18
"""Total number of diagnostic failure modes in the ML classifier."""
