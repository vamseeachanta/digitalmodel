"""Fail-closed parsing without physics or verdict defaults in package data."""

import math

from digitalmodel.orcaflex.viv_screening import BeamProperties, VIVScreeningInput


def _mapping(source: dict, name: str) -> dict:
    value = source.get(name)
    if not isinstance(value, dict):
        raise ValueError(f"viv_screening {name} must be a mapping")
    return value


def _number(
    source: dict, name: str, minimum: float = 0.0, allow_equal: bool = False
) -> float:
    value = source.get(name)
    if value is None:
        raise ValueError(f"viv_screening {name} is required")
    try:
        if isinstance(value, bool):
            raise ValueError
        value = float(value)
    except (ValueError, TypeError, OverflowError) as exc:
        raise ValueError(f"viv_screening {name} must be numeric") from exc
    if not math.isfinite(value):
        raise ValueError(f"viv_screening {name} must be finite")
    if value < minimum or (value == minimum and not allow_equal):
        relation = "at least" if allow_equal else "greater than"
        raise ValueError(f"viv_screening {name} must be {relation} {minimum}")
    return value


def _positive_float(source: dict, name: str) -> float:
    return _number(source, name)


def _current_envelope(environment: dict) -> tuple[float, float]:
    current = _mapping(environment, "current")
    scalar, profile = "speed_m_s" in current, "profile" in current
    if scalar == profile:
        raise ValueError("viv_screening current requires either speed_m_s or profile")
    if scalar:
        speeds = [_number(current, "speed_m_s", allow_equal=True)]
    else:
        entries = current["profile"]
        if not isinstance(entries, list) or not entries:
            raise ValueError("viv_screening current profile must be a nonempty list")
        speeds = []
        for entry in entries:
            if not isinstance(entry, dict):
                raise ValueError(
                    "viv_screening current profile entries must be mappings"
                )
            _number(entry, "depth_m", allow_equal=True)
            speeds.append(_number(entry, "speed_m_s", allow_equal=True))
    if max(speeds) == 0:
        raise ValueError("viv_screening current speeds cannot all be zero")
    return min(speeds), max(speeds)


def _beam(settings: dict) -> BeamProperties:
    structure = _mapping(settings, "structure")
    values = {
        "length": _positive_float(structure, "length_m"),
        "outer_diameter": _positive_float(structure, "outer_diameter_m"),
        "inner_diameter": _number(structure, "inner_diameter_m", allow_equal=True),
        "youngs_modulus": _positive_float(structure, "youngs_modulus_Pa"),
        "mass_per_length": _positive_float(structure, "mass_per_length_kg_m"),
        "effective_tension": _number(
            structure, "effective_tension_N", allow_equal=True
        ),
        "boundary": structure.get("boundary"),
    }
    if values["inner_diameter"] >= values["outer_diameter"]:
        raise ValueError(
            "viv_screening inner_diameter_m must be below outer_diameter_m"
        )
    if values["boundary"] is None:
        raise ValueError("viv_screening boundary is required")
    try:
        return BeamProperties(**values)
    except ValueError as exc:
        raise ValueError(f"viv_screening structure: {exc}") from exc


def parse_inputs(cfg: dict) -> tuple[dict, BeamProperties, VIVScreeningInput]:
    settings = _mapping(cfg, "viv_screening").copy()
    settings["design_life_years"] = _positive_float(settings, "design_life_years")
    settings["dff"] = _positive_float(settings, "dff")
    count = settings.get("n_modes", 10)
    if isinstance(count, bool) or not isinstance(count, int) or count < 1:
        raise ValueError("viv_screening n_modes must be a positive integer")
    settings["n_modes"] = count
    beam = _beam(settings)
    environment = _mapping(settings, "environment")
    if "lock_in_vr_range" in settings or "lock_in_vr_range" in environment:
        raise ValueError("viv_screening lock_in_vr_range override is unsupported")
    low, high = _current_envelope(environment)
    settings["current_envelope_m_s"] = {"minimum": low, "maximum": high}
    exposure = _positive_float(environment, "exposure_fraction")
    if exposure > 1:
        raise ValueError("viv_screening exposure_fraction must be in (0, 1]")
    settings["exposure_fraction"] = exposure
    settings["unit_modal_stress_MPa"] = (
        _positive_float(settings, "unit_modal_stress_MPa")
        if settings.get("unit_modal_stress_MPa") is not None
        else None
    )
    curve = settings.get("sn_curve", {})
    if not isinstance(curve, dict):
        raise ValueError("viv_screening sn_curve must be a mapping")
    settings["sn_curve"] = {
        "curve": str(curve.get("curve", "F1")),
        "environment": str(curve.get("environment", "seawater_cp")),
    }
    viv = VIVScreeningInput(
        outer_diameter=beam.outer_diameter,
        current_speed=high,
        kinematic_viscosity=_positive_float(environment, "kinematic_viscosity_m2_s"),
        mass_ratio=_positive_float(environment, "mass_ratio"),
        damping_ratio=_number(environment, "damping_ratio", allow_equal=True),
    )
    return settings, beam, viv
