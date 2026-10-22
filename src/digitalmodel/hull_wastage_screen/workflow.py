"""Durable offline workflow and HTML hull-structure wastage analysis."""
from datetime import date
from html import escape
import json
from pathlib import Path

from .screen import assess


def _details(value, field="", ratios=False):
    """Render finite result values with report precision, escaping all text."""
    if isinstance(value, dict):
        return "<ul>" + "".join(
            f"<li>{escape(str(key))}: {_details(item, key, ratios or key == 'modulus_ratios')}</li>"
            for key, item in value.items()
        ) + "</ul>"
    if isinstance(value, list):
        return "<ul>" + "".join(f"<li>{_details(item)}</li>" for item in value) + "</ul>"
    if isinstance(value, float):
        if not ratios and field.endswith(("_m2", "_m3", "_m4")):
            return f"{value:#.4g}"
        return f"{value:.3f}"
    return escape(str(value))


def _report(result):
    rows = "".join(
        f"<tr><td>{escape(row['id'])}</td><td>{row['measured_mm']:.3f}</td>"
        f"<td>{row['lower_bound_mm']:.3f}</td><td>{row['renewal_mm']:.3f}</td>"
        f"<td>{row['monitor_boundary_mm']:.3f}</td><td>{escape(row['status'])}</td></tr>"
        for row in result["elements"]
    )
    return (
        '<!doctype html><html lang="en"><meta charset="utf-8">'
        '<title>Hull-structure wastage analysis</title><body>'
        '<h1>Hull-structure wastage analysis</h1><h2>Summary</h2>'
        f"<p>Disposition: {escape(result['status'])}; "
        f"shared FFS action: {escape(result['decision']['action'])}.</p>"
        '<h2>Revision history</h2><table><tr><th>Revision</th><th>Date</th>'
        '<th>What changed and why</th></tr>'
        f'<tr><td>0</td><td>{date.today().isoformat()}</td>'
        '<td>Initial analysis of the supplied representative thickness inputs.</td></tr></table>'
        '<h2>Method and basis</h2><p>Renewal thickness equals as-built thickness less '
        'corrosion and voluntary additions for CSR. The percentage family uses the '
        'supplied renewal thickness. Measured thickness less the stated uncertainty '
        'is compared with renewal and the selected monitor boundary. Global modulus '
        'and neutral-axis area checks use the gross offered section basis.</p>'
        '<table><tr><th>Element</th><th>Measured (mm)</th><th>Lower bound (mm)</th>'
        '<th>Renewal (mm)</th><th>Monitor boundary (mm)</th><th>Disposition</th></tr>'
        + rows + '</table><p>Table 1. Local representative-item thickness screen.</p>'
        '<h2>Strength and pitting checks</h2>' + _details(result["checks"])
        + '<h2>CSR input deviations</h2>' + _details(result["csr_deviations"])
        + '<h2>Way forward</h2><p>Renewal findings require repair assessment. '
        'Monitor findings require survey follow-up. Every NOT_EVALUATED check '
        'requires the named evidence before overall acceptance can be established. '
        'Class survey review remains required.</p><h2>Limitations</h2>'
        + _details(result["limitations"]) + '<h2>Sources and selected basis</h2>'
        + _details(result["sources"]) + '</body></html>'
    )


def router(cfg):
    """Assess in memory and write the numeric record and escaped HTML analysis.

    The output directory is caller-controlled. Re-issues require retention of
    previous output directories; this router produces an initial issue only.
    """
    block = cfg["hull_wastage_screen"]
    result = assess(block)
    output = Path(block.get("output_dir", "results"))
    if not output.is_absolute():
        if cfg.get("_config_dir_path"):
            config_dir = Path(cfg["_config_dir_path"])
        elif cfg.get("_config_file_path"):
            config_dir = Path(cfg["_config_file_path"]).parent
        else:
            config_dir = Path.cwd()
        output = config_dir / output
    output.mkdir(parents=True, exist_ok=True)
    # Fail before overwriting any retained analysis or revision history.
    paths = [output / "hull-wastage.json", output / "hull-wastage.html"]
    if any(path.exists() for path in paths):
        raise FileExistsError("Use a fresh output directory to retain previous analysis revisions")
    paths[0].write_text(json.dumps(result, indent=2, allow_nan=False) + "\n", encoding="utf-8")
    paths[1].write_text(_report(result), encoding="utf-8")
    block["result"] = result
    return cfg
