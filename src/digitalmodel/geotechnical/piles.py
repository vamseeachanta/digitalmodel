# ABOUTME: Pile axial capacity calculations per API RP 2GEO.
# ABOUTME: Implements alpha method (clay), beta method (sand), end bearing, total capacity.
"""Pile axial capacity — API RP 2GEO method.

Implements:
- skin_friction_clay: alpha method for cohesive soils (Sec 7.3)
- skin_friction_sand: beta method for cohesionless soils (Sec 8.2)
- end_bearing_clay: Nc * Su for cohesive soils (Sec 7.4)
- end_bearing_sand: Nq * sigma_v' for cohesionless soils (Sec 8.3)
- axial_capacity: total compressive pile capacity (Sec 7.2)
"""
from dataclasses import dataclass
import math
from typing import Any


STANDARD = "API RP 2GEO"
DEFAULT_NC = 9.0
DEFAULT_NQ = 40.0

# Valid input ranges per API RP 2GEO (from dark-intelligence YAML archive)
_RANGE_PILE_DIAMETER_M = (0.3, 3.0)
_RANGE_PILE_LENGTH_M = (10.0, 100.0)
_RANGE_SU_KPA = (10.0, 200.0)
_RANGE_SIGMA_V_KPA = (10.0, 500.0)


def validate_pile_inputs(
    pile_diameter_m: float | None = None,
    pile_length_m: float | None = None,
    su_kpa: float | None = None,
    sigma_v_kpa: float | None = None,
) -> None:
    """Validate pile inputs against API RP 2GEO typical ranges.

    Args:
        pile_diameter_m: Outer pile diameter (m). Valid range [0.3, 3.0].
        pile_length_m: Embedded pile length (m). Valid range [10, 100].
        su_kpa: Undrained shear strength (kPa). Valid range [10, 200].
        sigma_v_kpa: Effective overburden pressure (kPa). Valid range [10, 500].

    Raises:
        ValueError: If any provided value is outside its valid range.
    """
    if pile_diameter_m is not None:
        lo, hi = _RANGE_PILE_DIAMETER_M
        if not (lo <= pile_diameter_m <= hi):
            raise ValueError(
                f"pile_diameter_m={pile_diameter_m} out of API RP 2GEO range [{lo}, {hi}] m"
            )
    if pile_length_m is not None:
        lo, hi = _RANGE_PILE_LENGTH_M
        if not (lo <= pile_length_m <= hi):
            raise ValueError(
                f"pile_length_m={pile_length_m} out of API RP 2GEO range [{lo}, {hi}] m"
            )
    if su_kpa is not None:
        lo, hi = _RANGE_SU_KPA
        if not (lo <= su_kpa <= hi):
            raise ValueError(
                f"su_kpa={su_kpa} out of API RP 2GEO range [{lo}, {hi}] kPa"
            )
    if sigma_v_kpa is not None:
        lo, hi = _RANGE_SIGMA_V_KPA
        if not (lo <= sigma_v_kpa <= hi):
            raise ValueError(
                f"sigma_v_kpa={sigma_v_kpa} out of API RP 2GEO range [{lo}, {hi}] kPa"
            )


@dataclass
class SkinFrictionClayResult:
    """Result of alpha-method skin friction calculation."""

    unit_friction_kpa: float
    alpha: float
    standard: str = STANDARD


@dataclass
class SkinFrictionSandResult:
    """Result of beta-method skin friction calculation."""

    unit_friction_kpa: float
    beta: float
    standard: str = STANDARD


@dataclass
class EndBearingResult:
    """Result of end bearing calculation."""

    unit_bearing_kpa: float
    standard: str = STANDARD


@dataclass
class AxialCapacityResult:
    """Total axial pile capacity breakdown."""

    total_capacity_kn: float
    skin_friction_kn: float
    end_bearing_kn: float
    standard: str = STANDARD


def skin_friction_clay(
    undrained_shear_strength_kpa: float,
    effective_overburden_kpa: float,
    validate: bool = False,
) -> SkinFrictionClayResult:
    """Alpha method for unit skin friction in clay per API RP 2GEO Sec 7.3.

    Alpha = 0.5 * (Su/sigma_v')^(-0.5)  for Su/sigma_v' <= 1.0
    Alpha = 0.5 * (Su/sigma_v')^(-0.25) for Su/sigma_v' > 1.0
    Alpha is capped at 1.0.

    Args:
        undrained_shear_strength_kpa: Undrained shear strength Su (kPa).
        effective_overburden_kpa: Effective vertical overburden stress (kPa).
        validate: If True, raise ValueError if inputs are outside API RP 2GEO ranges.

    Returns:
        SkinFrictionClayResult with unit friction and alpha factor.

    Raises:
        ValueError: If validate=True and inputs are out of valid range.
    """
    if validate:
        validate_pile_inputs(
            su_kpa=undrained_shear_strength_kpa,
            sigma_v_kpa=effective_overburden_kpa,
        )
    psi = undrained_shear_strength_kpa / effective_overburden_kpa
    if psi <= 1.0:
        alpha = 0.5 * psi ** (-0.5)
    else:
        alpha = 0.5 * psi ** (-0.25)
    alpha = min(alpha, 1.0)

    unit_friction = alpha * undrained_shear_strength_kpa
    return SkinFrictionClayResult(
        unit_friction_kpa=unit_friction,
        alpha=alpha,
    )


def skin_friction_sand(
    effective_overburden_kpa: float,
    friction_angle_deg: float,
    k0: float,
) -> SkinFrictionSandResult:
    """Beta method for unit skin friction in sand per API RP 2GEO Sec 8.2.

    f = beta * sigma_v'  where beta = K0 * tan(delta)
    delta is taken as friction_angle - 5 degrees (interface friction).

    Args:
        effective_overburden_kpa: Effective vertical overburden stress (kPa).
        friction_angle_deg: Soil friction angle phi (degrees).
        k0: Lateral earth pressure coefficient.

    Returns:
        SkinFrictionSandResult with unit friction and beta factor.
    """
    delta_rad = math.radians(friction_angle_deg - 5.0)
    beta = k0 * math.tan(delta_rad)
    unit_friction = beta * effective_overburden_kpa
    return SkinFrictionSandResult(
        unit_friction_kpa=unit_friction,
        beta=beta,
    )


def end_bearing_clay(
    undrained_shear_strength_kpa: float,
    nc: float = DEFAULT_NC,
) -> EndBearingResult:
    """Unit end bearing in clay per API RP 2GEO Sec 7.4.

    q = Nc * Su, where Nc is typically 9.

    Args:
        undrained_shear_strength_kpa: Undrained shear strength at pile tip (kPa).
        nc: Bearing capacity factor (default 9.0).

    Returns:
        EndBearingResult with unit bearing pressure.
    """
    return EndBearingResult(
        unit_bearing_kpa=nc * undrained_shear_strength_kpa,
    )


def end_bearing_sand(
    effective_overburden_kpa: float,
    nq: float = DEFAULT_NQ,
) -> EndBearingResult:
    """Unit end bearing in sand per API RP 2GEO Sec 8.3.

    q = Nq * sigma_v'.

    Args:
        effective_overburden_kpa: Effective vertical stress at pile tip (kPa).
        nq: Bearing capacity factor.

    Returns:
        EndBearingResult with unit bearing pressure.
    """
    return EndBearingResult(
        unit_bearing_kpa=nq * effective_overburden_kpa,
    )


def axial_capacity(
    pile_diameter_m: float,
    pile_length_m: float,
    layers: list[dict[str, Any]],
    validate: bool = False,
) -> AxialCapacityResult:
    """Total axial compressive pile capacity per API RP 2GEO Sec 7.2.

    Q = Qf + Qp = sum(f_i * As_i) + q * Ap

    Each layer dict must contain:
        type: "clay" or "sand"
        thickness_m: layer thickness (m)
    For clay: su_kpa, sigma_v_kpa
    For sand: phi_deg, sigma_v_kpa, k0

    Args:
        pile_diameter_m: Outer diameter of pile (m).
        pile_length_m: Embedded length of pile (m).
        layers: List of soil layer dicts.
        validate: If True, raise ValueError if pile dimensions are outside API RP 2GEO ranges.

    Returns:
        AxialCapacityResult with total, skin friction, and end bearing.

    Raises:
        ValueError: If layers is empty, pile dimensions are invalid, or validate=True and
                    inputs are outside API RP 2GEO ranges.
    """
    if not layers:
        raise ValueError("layers must not be empty")
    if pile_diameter_m <= 0:
        raise ValueError("pile_diameter_m must be positive")
    if pile_length_m <= 0:
        raise ValueError("pile_length_m must be positive")

    if validate:
        validate_pile_inputs(
            pile_diameter_m=pile_diameter_m,
            pile_length_m=pile_length_m,
        )

    perimeter = math.pi * pile_diameter_m
    tip_area = math.pi * (pile_diameter_m / 2.0) ** 2

    total_skin_friction_kn = 0.0

    for layer in layers:
        layer_type = layer["type"]
        thickness = layer["thickness_m"]

        if layer_type == "clay":
            sf = skin_friction_clay(
                undrained_shear_strength_kpa=layer["su_kpa"],
                effective_overburden_kpa=layer["sigma_v_kpa"],
            )
            unit_friction = sf.unit_friction_kpa
        elif layer_type == "sand":
            sf = skin_friction_sand(
                effective_overburden_kpa=layer["sigma_v_kpa"],
                friction_angle_deg=layer["phi_deg"],
                k0=layer.get("k0", 0.8),
            )
            unit_friction = sf.unit_friction_kpa
        else:
            raise ValueError(f"Unknown layer type: {layer_type}")

        layer_skin_kn = unit_friction * perimeter * thickness
        total_skin_friction_kn += layer_skin_kn

    # End bearing from the last (tip) layer
    tip_layer = layers[-1]
    if tip_layer["type"] == "clay":
        eb = end_bearing_clay(
            undrained_shear_strength_kpa=tip_layer["su_kpa"],
        )
    else:
        eb = end_bearing_sand(
            effective_overburden_kpa=tip_layer["sigma_v_kpa"],
            nq=tip_layer.get("nq", DEFAULT_NQ),
        )
    end_bearing_kn = eb.unit_bearing_kpa * tip_area

    return AxialCapacityResult(
        total_capacity_kn=total_skin_friction_kn + end_bearing_kn,
        skin_friction_kn=total_skin_friction_kn,
        end_bearing_kn=end_bearing_kn,
    )
