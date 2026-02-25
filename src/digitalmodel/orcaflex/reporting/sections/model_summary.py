"""Section 1: Model summary — object counts, environment, analysis type."""
from __future__ import annotations


def _render_table(rows: list[tuple[str, str]]) -> str:
    trs = "\n".join(
        f"    <tr><td>{label}</td><td><strong>{value}</strong></td></tr>"
        for label, value in rows
    )
    return f'<table class="ofx">\n{trs}\n</table>'


def _safe_get(obj, attr: str, default: str = "N/A") -> str:
    try:
        return str(getattr(obj, attr))
    except Exception:
        return default


def build_model_summary(model, config) -> str:
    """Build model summary section HTML."""
    import OrcFxAPI

    lines = [o for o in model.objects if isinstance(o, OrcFxAPI.Line)]
    vessels = [o for o in model.objects if isinstance(o, OrcFxAPI.Vessel)]
    buoys_3d = [o for o in model.objects if isinstance(o, OrcFxAPI.Buoy) and "3D" in type(o).__name__]
    buoys_6d = [o for o in model.objects if isinstance(o, OrcFxAPI.Buoy) and "6D" in type(o).__name__]

    general = model.general
    env = model.environment

    rows = [
        ("Simulation file", model.simulationFileName if hasattr(model, "simulationFileName") else "—"),
        ("OrcaFlex version", _safe_get(general, "OrcaFlexVersion")),
        ("Units", _safe_get(general, "UnitsSystem")),
        ("Stages", _safe_get(general, "NumberOfStages")),
        ("Water depth (m)", _safe_get(env, "WaterDepth")),
        ("Water density (t/m³)", _safe_get(env, "Density")),
        ("Wave type", _safe_get(env, "WaveType")),
        ("Wave Hs (m)", _safe_get(env, "HsAtOrigin")),
        ("Wave direction (°)", _safe_get(env, "WaveDirection")),
        ("Lines", str(len(lines))),
        ("Vessels", str(len(vessels))),
        ("3D buoys", str(len(buoys_3d))),
        ("6D buoys", str(len(buoys_6d))),
    ]

    return _render_table(rows)
