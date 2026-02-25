"""Section 2: Static configuration — line profile summary table."""
from __future__ import annotations


def _render_table(rows: list[tuple[str, str]]) -> str:
    trs = "\n".join(
        f"    <tr><td>{label}</td><td><strong>{value}</strong></td></tr>"
        for label, value in rows
    )
    return f'<table class="ofx">\n{trs}\n</table>'


def build_static_config(model, config) -> str:
    """Build static configuration section HTML."""
    import OrcFxAPI

    lines = [o for o in model.objects if isinstance(o, OrcFxAPI.Line)]
    if not lines:
        return "<p>No line objects found in model.</p>"

    parts = []
    for line in lines:
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
            parts.append(_render_table(rows))
        except Exception as exc:
            parts.append(f"<p>Error extracting static results for '{line.name}': {exc}</p>")

    vessels = [o for o in model.objects if isinstance(o, OrcFxAPI.Vessel)]
    for vessel in vessels:
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
            parts.append(_render_table(rows))
        except Exception as exc:
            parts.append(f"<p>Error extracting vessel static for '{vessel.name}': {exc}</p>")

    return "\n".join(parts)
