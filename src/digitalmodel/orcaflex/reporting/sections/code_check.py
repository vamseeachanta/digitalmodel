"""Section 5: Code check utilization table."""
from __future__ import annotations


def build_code_check(model, config) -> str:
    """Build code check utilization table HTML."""
    try:
        import OrcFxAPI
        code_checks = getattr(model, "codeChecks", None) or []
        if not code_checks:
            return (
                "<p>No code check results found in .sim file. "
                "Enable code checks in the model and re-run to populate this section.</p>"
            )
        rows = []
        for cc in code_checks:
            util = getattr(cc, "Utilisation", None) or getattr(cc, "Utilization", None)
            name = getattr(cc, "Name", str(cc))
            status = "pass" if util is not None and util <= 1.0 else "fail"
            util_str = f"{util:.3f}" if util is not None else "N/A"
            rows.append(
                f"<tr><td>{name}</td>"
                f'<td class="{status}">{util_str}</td></tr>'
            )
        header = "<tr><th>Code check</th><th>Utilisation</th></tr>"
        return f'<table class="ofx">{header}{"".join(rows)}</table>'
    except Exception as exc:
        return f"<p>Error extracting code check results: {exc}</p>"
