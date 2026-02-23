"""
Extractor for boundary conditions from OrcaFlex line objects.
"""
try:
    import OrcFxAPI as ofx
except ImportError:
    ofx = None

from ..models.boundary_conditions import BCData, BCEndData, SeabedModelData


def extract_boundary_conditions(line: 'ofx.OrcaFlexLineObject') -> BCData:
    """Extracts End A / End B connections and seabed model from an OrcaFlex line.

    The OrcaFlex line end connection may be "Anchored", "Free", a vessel name,
    or another object name.  The extracted type string reflects whatever OrcaFlex
    reports for ``EndAConnection`` / ``EndBConnection``.

    Args:
        line: An OrcaFlex line object.

    Returns:
        BCData with end_a, end_b, and seabed populated where possible.

    Raises:
        ImportError: If OrcFxAPI is not installed.
    """
    if ofx is None:
        raise ImportError("OrcFxAPI is required for extraction.")

    end_a = _extract_end(line, end="A")
    end_b = _extract_end(line, end="B")
    seabed = _extract_seabed(line)

    return BCData(end_a=end_a, end_b=end_b, seabed=seabed)


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

def _extract_end(line: object, end: str) -> BCEndData:
    """Build BCEndData for one end of the line."""
    conn_attr = f"End{end}Connection"
    x_attr = f"End{end}X"
    y_attr = f"End{end}Y"
    z_attr = f"End{end}Z"

    connection = _safe_attr(line, conn_attr) or "Unknown"
    x = _safe_float(line, x_attr) or 0.0
    y = _safe_float(line, y_attr) or 0.0
    z = _safe_float(line, z_attr) or 0.0

    # Classify connection type
    if connection in ("Anchored", "Fixed"):
        conn_type = "Fixed"
    elif connection == "Free":
        conn_type = "Free"
    elif connection and connection not in ("", "Anchored", "Free"):
        conn_type = "Vessel" if _looks_like_vessel(connection) else "Object"
    else:
        conn_type = "Unknown"

    return BCEndData(
        name=f"End {end}",
        type=conn_type,
        x=x,
        y=y,
        z=z,
        connected_to=connection if conn_type not in ("Fixed", "Free") else None,
    )


def _extract_seabed(line: object) -> SeabedModelData:
    """Build SeabedModelData from seabed contact properties."""
    stiffness = _safe_float(line, "SeabedNormalStiffness")
    friction_axial = _safe_float(line, "SeabedFrictionCoefficient")

    seabed_type = "Linear" if stiffness is not None else "Not defined"
    return SeabedModelData(
        type=seabed_type,
        stiffness_kn_m2=stiffness,
        friction_axial=friction_axial,
    )


def _looks_like_vessel(name: str) -> bool:
    """Heuristic: vessel names often contain 'vessel', 'fpso', 'ship', etc."""
    lower = name.lower()
    return any(kw in lower for kw in ("vessel", "fpso", "ship", "buoy", "platform"))


def _safe_attr(obj: object, name: str):
    """Return ``getattr(obj, name)`` or ``None`` if absent."""
    try:
        return getattr(obj, name)
    except Exception:
        return None


def _safe_float(obj: object, name: str):
    """Return ``float(getattr(obj, name))`` or ``None`` if unavailable."""
    val = _safe_attr(obj, name)
    if val is None:
        return None
    try:
        return float(val)
    except (TypeError, ValueError):
        return None
