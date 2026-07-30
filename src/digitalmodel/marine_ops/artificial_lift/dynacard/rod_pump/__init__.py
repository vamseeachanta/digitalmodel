# ABOUTME: API RP 11L rod-pump kinematics, rod-string mechanics and card metrics.
# ABOUTME: Public entry point is analyse(); it fails closed outside the RP 11L envelope.
"""API RP 11L rod-pump surface-card analysis.

Covers the kinematics and rod-string mechanics that sit *upstream* of a
surface-to-downhole transform: natural frequency, card undulations, crank
motion, plunger stroke, pump displacement, and the dimensionless groups the
RP 11L correlations run on.

For the surface-to-downhole card conversion itself, see the sibling
:mod:`..everitt_jennings` package.

Two behaviours are deliberate and worth knowing before use:

- **Fails closed.** ``N/No' > 0.35`` raises rather than extrapolating, as do
  non-Class-I unit geometries and tapered strings without an explicit ``Fc``.
- **Reports what it cannot determine.** Volumetric efficiency returns ``None``
  when runtime or formation volume factor is unknown, instead of assuming 24 h
  and ``Bo = 1.0``.

Example::

    from digitalmodel.marine_ops.artificial_lift.dynacard.rod_pump import analyse

    result = analyse(
        rod_diameter_in=0.75, rod_length_ft=4200.0,
        surface_stroke_in=41.0, strokes_per_minute=6.4,
        plunger_diameter_in=1.25, fluid_level_ft=4300.0,
        specific_gravity=0.85,
        tubing_pressure_psi=150.0, casing_pressure_psi=25.0,
    )
    result.undulations_per_half_stroke   # 4.56
    result.pump_displacement_bpd         # 38.8
"""

from .analysis import RodPumpAnalysis, ValidityError, analyse
from .card_metrics import (
    CardMetrics,
    analyse_card,
    card_area,
    load_datum_check,
    polished_rod_horsepower,
)
from .constants import (
    API_ROD_SIZES,
    ENVELOPE_MAX_VALID,
    ENVELOPE_WAVE_DOMINATED,
    SONIC_VELOCITY_STEEL_FT_S,
    YOUNGS_MODULUS_PSI,
)
from .kinematics import (
    Measurement,
    PeakTrains,
    angular_velocity,
    crank_position,
    crank_velocity,
    divergence_onset,
    intervals_are_distinguishable,
    natural_frequency,
    peak_interval,
    peak_times,
    taper_adjusted_natural_frequency,
    time_at_position,
    time_from_card_position,
    undulations_per_half_stroke,
)
from .rod_string import (
    RodStringAnalysis,
    analyse_rod_string,
    buoyant_rod_weight,
    fluid_load,
    plunger_stroke,
    pump_displacement,
    rod_area,
    rod_elastic_constant,
    spring_rate,
    volumetric_efficiency,
)

__all__ = [
    "API_ROD_SIZES",
    "CardMetrics",
    "ENVELOPE_MAX_VALID",
    "ENVELOPE_WAVE_DOMINATED",
    "Measurement",
    "PeakTrains",
    "RodPumpAnalysis",
    "RodStringAnalysis",
    "SONIC_VELOCITY_STEEL_FT_S",
    "ValidityError",
    "YOUNGS_MODULUS_PSI",
    "analyse",
    "analyse_card",
    "analyse_rod_string",
    "angular_velocity",
    "buoyant_rod_weight",
    "card_area",
    "crank_position",
    "crank_velocity",
    "divergence_onset",
    "fluid_load",
    "intervals_are_distinguishable",
    "load_datum_check",
    "natural_frequency",
    "peak_interval",
    "peak_times",
    "plunger_stroke",
    "polished_rod_horsepower",
    "pump_displacement",
    "rod_area",
    "rod_elastic_constant",
    "spring_rate",
    "taper_adjusted_natural_frequency",
    "time_at_position",
    "time_from_card_position",
    "undulations_per_half_stroke",
    "volumetric_efficiency",
]
