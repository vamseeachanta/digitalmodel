"""Section 1: Model summary — object counts, environment, analysis type."""
from __future__ import annotations

from ._common import attr_text, buoys, deprecated_alias_getattr, kv_table, lines, vessels

# Deprecated: ``_render_table`` and ``_safe_get`` moved to ``_common`` as
# ``kv_table`` and ``attr_text``; the old names still resolve, with a warning.
__getattr__ = deprecated_alias_getattr(
    __name__, {"_render_table": "kv_table", "_safe_get": "attr_text"}
)


def build_model_summary(model, config) -> str:
    """Build model summary section HTML."""
    line_objs = lines(model)
    vessel_objs = vessels(model)
    buoys_3d = buoys(model, "3D")
    buoys_6d = buoys(model, "6D")

    general = model.general
    env = model.environment

    rows = [
        ("Simulation file", model.simulationFileName if hasattr(model, "simulationFileName") else "—"),
        ("OrcaFlex version", attr_text(general, "OrcaFlexVersion")),
        ("Units", attr_text(general, "UnitsSystem")),
        ("Stages", attr_text(general, "NumberOfStages")),
        ("Water depth (m)", attr_text(env, "WaterDepth")),
        ("Water density (t/m³)", attr_text(env, "Density")),
        ("Wave type", attr_text(env, "WaveType")),
        ("Wave Hs (m)", attr_text(env, "HsAtOrigin")),
        ("Wave direction (°)", attr_text(env, "WaveDirection")),
        ("Lines", str(len(line_objs))),
        ("Vessels", str(len(vessel_objs))),
        ("3D buoys", str(len(buoys_3d))),
        ("6D buoys", str(len(buoys_6d))),
    ]

    return kv_table(rows)
