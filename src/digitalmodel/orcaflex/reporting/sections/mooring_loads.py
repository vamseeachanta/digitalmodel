"""Section 6: Mooring loads — fairlead tensions and watch circle."""
from __future__ import annotations
import json


def build_mooring_loads(model, config) -> str:
    """Build mooring loads section HTML."""
    import OrcFxAPI

    lines = [o for o in model.objects if isinstance(o, OrcFxAPI.Line)]
    if not lines:
        return "<p>No mooring lines found in model.</p>"

    rows_html = []
    for line in lines:
        try:
            static_t = line.StaticResult("Effective Tension", OrcFxAPI.oeEndA)
            try:
                th = line.TimeHistory("Effective Tension", OrcFxAPI.oeEndA)
                min_t = min(th)
                max_t = max(th)
                dyn_str = f"{min_t:.1f} / {max_t:.1f}"
            except Exception:
                dyn_str = "N/A"
            rows_html.append(
                f"<tr><td>{line.name}</td>"
                f"<td>{static_t:.1f}</td>"
                f"<td>{dyn_str}</td></tr>"
            )
        except Exception as exc:
            rows_html.append(f"<tr><td>{line.name}</td><td colspan='2'>Error: {exc}</td></tr>")

    header = "<tr><th>Line</th><th>Static tension (kN)</th><th>Dynamic min/max (kN)</th></tr>"
    return f'<table class="ofx">{header}{"".join(rows_html)}</table>'
