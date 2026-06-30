# ABOUTME: Towline load, required class minimum breaking load, and snap-back recoil energy
# ABOUTME: Excess assisted-vessel speed multiplies towline load 2-5x; parted lines recoil dangerously
"""Towline load and line-safety analysis (issue #1197).

The towline load is driven by the tug's bollard pull amplified by dynamic
effects — chiefly the assisted vessel's through-water speed, which can multiply
the static load several-fold (MAIB *Biter*: 4.6 kn vs the 2-3 kn norm raised
loads 2-5x). The required minimum breaking load (MBL) follows the governing
classification society's safety factor. A parted line stores and releases
elastic energy in a snap-back arc — the dominant tug-crew injury path.

References:
    - Classification-society towing-gear factors (LR ~2.5x, ABS/DNV ~2.0x)
    - MAIB *Biter* (2023) on speed-driven towline load amplification
    - Maritime Mutual / Polestar snap-back loss-prevention guidance
"""

from dataclasses import dataclass

from digitalmodel.tug.constants import CLASS_TOWLINE_SAFETY_FACTOR, GRAVITY_M_S2


@dataclass
class TowlineLoadResult:
    """Towline load and required minimum breaking load."""

    static_load_t: float  # static load (= bollard pull), tonnes-force
    dynamic_factor: float  # amplification from speed/sea state
    design_load_t: float  # static * dynamic
    society: str
    safety_factor: float
    required_mbl_t: float  # design load * safety factor
    selected_mbl_t: float | None
    adequate: bool | None  # selected_mbl >= required_mbl


def speed_amplification_factor(
    assist_speed_kn: float,
    reference_speed_kn: float = 2.5,
    exponent: float = 2.0,
    cap: float = 5.0,
) -> float:
    """Dynamic load amplification from assisted-vessel through-water speed.

    Towline load scales roughly with the square of speed; normalised to a
    recommended reference assist speed and capped to a realistic maximum.

        factor = min(cap, max(1, (V / V_ref)^exponent))

    With V_ref = 2.5 kn (mid of the 2-3 kn norm), an assist at 4.6 kn gives
    (4.6/2.5)^2 = 3.4x — within the reported 2-5x band.

    Args:
        assist_speed_kn: actual assisted-vessel through-water speed, knots.
        reference_speed_kn: recommended assist speed, knots.
        exponent: load-vs-speed exponent (≈2 for hydrodynamic load).
        cap: maximum credible amplification.

    Returns:
        Dynamic amplification factor (>= 1.0).
    """
    if assist_speed_kn < 0 or reference_speed_kn <= 0:
        raise ValueError("invalid speeds")
    raw = (assist_speed_kn / reference_speed_kn) ** exponent
    return min(cap, max(1.0, raw))


def towline_load(
    bollard_pull_t: float,
    society: str = "LR",
    dynamic_factor: float = 1.0,
    selected_mbl_t: float | None = None,
) -> TowlineLoadResult:
    """Required towline minimum breaking load for a given bollard pull.

    design_load = bollard_pull * dynamic_factor
    required_MBL = design_load * class_safety_factor

    Args:
        bollard_pull_t: static towline load, taken as the tug bollard pull, tonnes.
        society: classification society key ('LR', 'ABS', 'DNV').
        dynamic_factor: amplification from speed/sea state (see
            speed_amplification_factor).
        selected_mbl_t: candidate line MBL to check, tonnes (optional).

    Returns:
        TowlineLoadResult with required MBL and adequacy of any selected line.
    """
    if bollard_pull_t <= 0:
        raise ValueError("bollard_pull_t must be positive")
    if dynamic_factor < 1.0:
        raise ValueError("dynamic_factor must be >= 1.0")

    key = society.upper()
    if key not in CLASS_TOWLINE_SAFETY_FACTOR:
        raise ValueError(
            f"unknown society {society!r}; use one of {list(CLASS_TOWLINE_SAFETY_FACTOR)}"
        )
    sf = CLASS_TOWLINE_SAFETY_FACTOR[key]

    design_load = bollard_pull_t * dynamic_factor
    required_mbl = design_load * sf
    adequate = None if selected_mbl_t is None else selected_mbl_t >= required_mbl

    return TowlineLoadResult(
        static_load_t=bollard_pull_t,
        dynamic_factor=dynamic_factor,
        design_load_t=design_load,
        society=key,
        safety_factor=sf,
        required_mbl_t=required_mbl,
        selected_mbl_t=selected_mbl_t,
        adequate=adequate,
    )


def snapback_recoil_energy(
    line_load_t: float,
    line_length_m: float,
    line_mbl_t: float,
    elongation_at_break: float = 0.20,
) -> float:
    """Elastic energy stored in a loaded towline (snap-back energy proxy), kJ.

    Models the line as a linear spring to break: stiffness k = (MBL*g)/(L*eps),
    stored energy = 0.5 * F^2 / k, with F the load at parting. Higher values
    mean a more dangerous recoil; synthetic lines (large elongation) store more.

    Args:
        line_load_t: load in the line at the moment of parting, tonnes-force.
        line_length_m: stretched line length, m.
        line_mbl_t: line minimum breaking load, tonnes-force.
        elongation_at_break: fractional elongation at break (≈0.20 for many
            synthetics, lower for HMPE).

    Returns:
        Stored elastic energy released on parting, kJ.
    """
    if line_length_m <= 0 or line_mbl_t <= 0 or elongation_at_break <= 0:
        raise ValueError(
            "line_length_m, line_mbl_t, elongation_at_break must be positive"
        )

    f = line_load_t * 1000.0 * GRAVITY_M_S2  # N
    mbl_n = line_mbl_t * 1000.0 * GRAVITY_M_S2  # N
    stiffness = mbl_n / (line_length_m * elongation_at_break)  # N/m
    energy_j = 0.5 * f**2 / stiffness
    return energy_j / 1000.0
