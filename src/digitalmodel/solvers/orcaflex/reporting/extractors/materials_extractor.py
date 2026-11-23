"""
Extractor for material and line-type properties from OrcaFlex models.
"""
try:
    import OrcFxAPI as ofx
except ImportError:
    ofx = None

from ..models.materials import MaterialData, LineTypeData


def extract_materials(line: 'ofx.OrcaFlexLineObject') -> MaterialData:
    """Extracts line-type (material) properties from an OrcaFlex line.

    Reads the first line type associated with the line.  Properties that are
    not defined in the model (e.g. no inner diameter) fall back to ``0.0``.

    Args:
        line: An OrcaFlex line object.

    Returns:
        MaterialData with one LineTypeData entry per unique line type found.

    Raises:
        ImportError: If OrcFxAPI is not installed.
    """
    if ofx is None:
        raise ImportError("OrcFxAPI is required for extraction.")

    line_types = _collect_line_types(line)
    return MaterialData(line_types=line_types)


def extract_materials_for_lines(
    lines: list,
) -> MaterialData:
    """Extracts deduplicated line-type properties across multiple lines.

    Args:
        lines: Iterable of OrcaFlex line objects.

    Returns:
        MaterialData with unique LineTypeData entries (deduplicated by name).

    Raises:
        ImportError: If OrcFxAPI is not installed.
    """
    if ofx is None:
        raise ImportError("OrcFxAPI is required for extraction.")

    seen: dict = {}
    for line in lines:
        for lt_data in _collect_line_types(line):
            seen.setdefault(lt_data.name, lt_data)
    return MaterialData(line_types=list(seen.values()))


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

def _collect_line_types(line: object) -> list:
    """Return a list of LineTypeData from a single line's section data."""
    results = []

    # LineType attribute may be a single string (homogeneous line) or a list
    lt_names = _safe_attr(line, "LineType")
    if lt_names is None:
        return results
    if isinstance(lt_names, str):
        lt_names = [lt_names]

    model = _safe_attr(line, "model")
    for lt_name in set(lt_names):  # deduplicate within this line
        lt_obj = _find_line_type(model, lt_name)
        results.append(_build_line_type_data(lt_name, lt_obj))
    return results


def _find_line_type(model: object, name: str):
    """Return the LineType OrcaFlex object, or None if not found."""
    if model is None or name is None:
        return None
    try:
        return model[name]
    except Exception:
        return None


def _build_line_type_data(name: str, lt_obj: object) -> LineTypeData:
    """Build a LineTypeData from a line type OrcaFlex object (may be None)."""
    od = _safe_float(lt_obj, "OD") or 0.0
    id_ = _safe_float(lt_obj, "ID") or 0.0
    wt = (od - id_) / 2.0 if od and id_ else 0.0
    density = _safe_float(lt_obj, "MassPerUnitLength") or 0.0

    return LineTypeData(
        name=name,
        od=od,
        id=id_,
        wt=wt,
        density_kg_m3=density,
        youngs_modulus_mpa=_safe_float(lt_obj, "EIx"),
        ea_kn=_safe_float(lt_obj, "EA"),
        ei_knm2=_safe_float(lt_obj, "EIx"),
        mass_per_m_kg_m=_safe_float(lt_obj, "MassPerUnitLength"),
    )


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
