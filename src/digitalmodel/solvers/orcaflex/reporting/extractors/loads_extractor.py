"""
Extractor for environmental loading conditions from OrcaFlex models.
"""
try:
    import OrcFxAPI as ofx
except ImportError:
    ofx = None

from ..models.loads import EnvironmentData, LoadCaseData, HydroCoeffData


def extract_loads(model: 'ofx.Model') -> EnvironmentData:
    """Extracts environment / loading data from an OrcaFlex model.

    Reads the active wave and current settings from ``model.environment`` and
    returns a single :class:`LoadCaseData` labelled "Active Load Case".  If
    any attribute is unavailable (e.g., no wave defined) the corresponding
    field is left as ``None``.

    Args:
        model: An open OrcaFlex model object.

    Returns:
        EnvironmentData with one load case and an optional current profile.

    Raises:
        ImportError: If OrcFxAPI is not installed.
    """
    if ofx is None:
        raise ImportError("OrcFxAPI is required for extraction.")

    env = model.environment

    hs = _safe_attr(env, "WaveHeight")
    tp = _safe_attr(env, "WavePeriod")
    current_vel = _safe_attr(env, "CurrentVelocity")
    current_dir = _safe_attr(env, "CurrentDirection")

    load_case = LoadCaseData(
        case_id="Active Load Case",
        hs_m=hs,
        tp_s=tp,
        current_velocity_m_s=current_vel,
        current_direction_deg=current_dir,
    )

    # Build current profile if depth/velocity arrays are available
    current_profile = None
    depths = _safe_attr(env, "CurrentDepth")
    velocities = _safe_attr(env, "CurrentVelocity")
    if depths is not None and hasattr(depths, "__len__") and len(depths) > 1:
        current_profile = {
            "depth": list(depths),
            "velocity": list(velocities) if hasattr(velocities, "__len__") else [],
        }

    return EnvironmentData(
        load_cases=[load_case],
        current_profile=current_profile,
    )


def _safe_attr(obj: object, name: str):
    """Return ``getattr(obj, name)`` or ``None`` if attribute is absent."""
    try:
        return getattr(obj, name)
    except Exception:
        return None
