# ABOUTME: Physical constants and API sucker-rod properties for RP 11L analysis.
# ABOUTME: Every value is cited; nothing here is a tuned or fitted number.
"""Constants for API RP 11L rod-pump analysis.

Sources are named for each value. Where a constant only holds for a particular
material or geometry, that restriction is stated — the analysis entry point
fails closed rather than extrapolating past it.
"""

from typing import Dict, NamedTuple

# Sonic velocity of a stress wave in a steel sucker rod. Fibreglass rods are
# markedly slower (~4,000 ft/s), so this is a parameter everywhere it is used,
# never a hard-coded assumption.
SONIC_VELOCITY_STEEL_FT_S = 16_300.0

# Young's modulus for steel sucker rods, API RP 11L.
YOUNGS_MODULUS_PSI = 3.1e7

# API RP 11L natural-frequency constant: No = NATURAL_FREQUENCY_CONSTANT / L.
# Equivalent to 15 * c / L; the two agree to about 0.2% for steel, which
# `natural_frequency` asserts rather than assumes.
NATURAL_FREQUENCY_CONSTANT = 245_000.0
NATURAL_FREQUENCY_SONIC_MULTIPLIER = 15.0
NATURAL_FREQUENCY_AGREEMENT_TOLERANCE_PCT = 0.5

# Buoyancy coefficient in Wrf = Wr * (1 - BUOYANCY_COEFFICIENT * SG), API RP 11L.
BUOYANCY_COEFFICIENT = 0.128

# Fluid gradient of fresh water, psi/ft.
WATER_GRADIENT_PSI_PER_FT = 0.433

# Pump displacement constant: PD [bfpd] = 0.1484 * Ap * Sp * N.
PUMP_DISPLACEMENT_CONSTANT = 0.1484

# Polished-rod horsepower: work per stroke [ft-lb] * strokes/min / 33,000.
FT_LB_PER_MIN_PER_HP = 33_000.0


class RodProperties(NamedTuple):
    """Published properties of one API sucker-rod size."""

    diameter_in: float
    area_sq_in: float
    weight_lb_per_ft: float  # includes couplings


# API sucker-rod sizes. Areas are the published table values; the weights
# include couplings, which is why they exceed bare steel area * density.
API_ROD_SIZES: Dict[str, RodProperties] = {
    "5/8": RodProperties(0.625, 0.307, 1.135),
    "3/4": RodProperties(0.750, 0.442, 1.634),
    "7/8": RodProperties(0.875, 0.601, 2.224),
    "1": RodProperties(1.000, 0.785, 2.904),
}

# Validity envelope on N/No'. Below the lower bound the surface card is
# wave-dominated (many undulations) but the kinematics remain valid; above the
# upper bound resonance invalidates the RP 11L correlations outright.
ENVELOPE_WAVE_DOMINATED = 0.15
ENVELOPE_MAX_VALID = 0.35

# Pumping-unit geometries the RP 11L (Class I crank-balanced) correlations
# cover. Long-stroke and Mark II geometries have different kinematics.
SUPPORTED_UNIT_GEOMETRIES = frozenset({"conventional", "class_i", "class-i"})
UNSUPPORTED_UNIT_GEOMETRIES = frozenset(
    {"mark_ii", "mark-ii", "markii", "rotaflex", "hydraulic", "long_stroke"}
)
