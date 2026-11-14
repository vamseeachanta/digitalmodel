"""Offline durable workflow for analytical integral-flange rating."""

import csv
import hashlib
import json
from copy import deepcopy
from pathlib import Path

from digitalmodel.reporting import DataSource

from .calculations import analyze
from .render import render_report


def router(cfg):
    """Calculate and persist three consistent views of one result payload."""
    settings = deepcopy(cfg["flange_analysis"])
    result = analyze(settings)
    result["method"] = "Analytical flange rating (ASME VIII-1 Appendix 2, 2019)"
    result["inputs"] = settings
    options = settings.get("outputs", {})
    config_dir = Path(cfg.get("_config_dir_path", Path.cwd()))
    directory = Path(options.get("directory", "results/flange_analysis"))
    if not directory.is_absolute():
        directory = config_dir / directory
    names = {"summary_json": "flange_analysis_summary.json",
             "checks_csv": "flange_analysis_checks.csv",
             "report_html": "flange_analysis_report.html"}
    paths = {key: directory / options.get(key, name) for key, name in names.items()}
    for key, path in paths.items():
        result[key] = _display_path(path, config_dir)
    canonical_input = json.dumps(settings, sort_keys=True, allow_nan=False)
    source = DataSource(
        kind="spec", identifier=Path(cfg.get("_config_file_path", "input.yml")).name,
        digest="sha256:" + hashlib.sha256(canonical_input.encode()).hexdigest(),
        description="Calculation inputs; " + json.dumps(settings.get("source", {}), sort_keys=True),
    )
    # Render and validate provenance before writing any result files.
    document = render_report(result, inputs=settings, data_source=source)
    directory.mkdir(parents=True, exist_ok=True)
    paths["report_html"].write_text(document, encoding="utf-8")
    fields = ["name", "demand", "allowable", "units", "utilization", "status", "clause"]
    with paths["checks_csv"].open("w", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=fields)
        writer.writeheader()
        for row in result["checks"]:
            writer.writerow({**{key: row[key] for key in fields if key != "status"},
                             "status": "pass" if row["passes"] else "fail"})
    paths["summary_json"].write_text(json.dumps(result, indent=2, allow_nan=False), encoding="utf-8")
    cfg["flange_analysis"] = result
    cfg["screening_status"] = result["screening_status"]
    cfg.setdefault("outputs", {}).update({key: result[key] for key in names})
    return cfg


def _display_path(path, config_dir):
    """Keep machine-specific directories out of exported result metadata."""
    try:
        return path.resolve().relative_to(config_dir.resolve()).as_posix()
    except ValueError:
        return path.name
