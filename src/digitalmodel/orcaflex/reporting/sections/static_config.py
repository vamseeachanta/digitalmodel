"""Section 2: Static configuration — line profile summary table."""
from __future__ import annotations

from ._common import deprecated_alias_getattr, kv_table, lines, vessels

# Deprecated: ``_render_table`` moved to ``_common.kv_table``; the old name
# still resolves, with a warning.
__getattr__ = deprecated_alias_getattr(__name__, {"_render_table": "kv_table"})


def build_static_config(model, config) -> str:
    """Build static configuration section HTML."""
    import OrcFxAPI

    line_objs = lines(model)
    if not line_objs:
        return "<p>No line objects found in model.</p>"

    parts = []
    for line in line_objs:
        try:
            top_t = line.StaticResult("Effective Tension", OrcFxAPI.oeEndA)
            bot_t = line.StaticResult("Effective Tension", OrcFxAPI.oeEndB)
            top_x = line.StaticResult("X", OrcFxAPI.oeEndA)
            top_z = line.StaticResult("Z", OrcFxAPI.oeEndA)
            bot_x = line.StaticResult("X", OrcFxAPI.oeEndB)
            bot_z = line.StaticResult("Z", OrcFxAPI.oeEndB)
            rows = [
                ("Line", line.name),
                ("Top tension (kN)", f"{top_t:.2f}"),
                ("Bottom tension (kN)", f"{bot_t:.2f}"),
                ("Top position X (m)", f"{top_x:.2f}"),
                ("Top position Z (m)", f"{top_z:.2f}"),
                ("Bottom position X (m)", f"{bot_x:.2f}"),
                ("Bottom position Z (m)", f"{bot_z:.2f}"),
            ]
            parts.append(kv_table(rows))
        except Exception as exc:
            parts.append(f"<p>Error extracting static results for '{line.name}': {exc}</p>")

    for vessel in vessels(model):
        try:
            vx = vessel.StaticResult("X")
            vy = vessel.StaticResult("Y")
            vz = vessel.StaticResult("Z")
            rows = [
                ("Vessel", vessel.name),
                ("Static X (m)", f"{vx:.3f}"),
                ("Static Y (m)", f"{vy:.3f}"),
                ("Static Z (m)", f"{vz:.3f}"),
            ]
            parts.append(kv_table(rows))
        except Exception as exc:
            parts.append(f"<p>Error extracting vessel static for '{vessel.name}': {exc}</p>")

    return "\n".join(parts)
