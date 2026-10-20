"""Offline flexible-pipe intake workflow using the shared FFS report renderer."""

from __future__ import annotations

from .assessment.ffs_report import FFSReport
from .assessment.screening_report import store_report
from .flexible_pipe import screen_flexible_pipe


def _display_values(values):
    """Format report quantities; preserve full precision in the result payload."""
    displayed = {}
    for key, value in values.items():
        label = key.replace("_", " ")
        if isinstance(value, float):
            if "fraction" in key or key == "flooded_life_factor":
                # Percent is defined as a fraction per hundred, not a code factor.
                value = f"{value * 100:.1f} %"
            else:
                value = f"{value:.1f}"
        displayed[label] = value
    return displayed


def router(cfg):
    """Assess explicit evidence and optionally persist a self-contained report."""
    block = cfg.get("flexible_pipe_assessment", {})
    result = screen_flexible_pipe(block.get("evidence", {}))
    sections = {
        "Revision history": {
            "Initial issue": "2026-10-20: Evidence intake and count arithmetic; quantitative models remain unqualified."
        },
        "Inspection evidence (years where stated; fractions as percentages)": _display_values(
            result["evidence"]
        ),
        "Wire inventory (counts; fractions as percentages)": _display_values(
            result["wire_inventory"]
        ),
        "Unqualified calculations": {
            "Remaining capacity": "NOT_EVALUATED",
            "Remaining fatigue life (years)": "NOT_EVALUATED",
        },
    }
    for row in result["failure_modes"]:
        sections[row["title"]] = {
            **_display_values(row["evidence"]),
            "Assessment status": row["status"],
            "Action": row["verdict"],
            "Missing inputs": ", ".join(row["missing_inputs"]) or "none recorded",
            "Next action": row["next_action"],
            "Citation": row["citation"],
        }
    sections["Public sources"] = {
        source["title"]: f"{source['url']} ({source['locator']})"
        for source in result["sources"]
    }
    result["report_html"] = FFSReport.generate_screening_html(
        block.get("component_id", "FLEXIBLE-PIPE"),
        "Unbonded flexible pipe integrity screen",
        result["decision"],
        sections,
        result["limitations"],
    )
    cfg[cfg["basename"]] = store_report(
        result,
        block,
        "flexible-pipe-integrity-screen.html",
        cfg.get("_config_dir_path"),
    )
    return cfg
