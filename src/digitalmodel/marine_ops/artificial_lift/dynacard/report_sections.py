# ABOUTME: HTML report sections and verdict helpers for dynacard diagnostics.
# ABOUTME: Keeps report rendering separate from the dynacard solver workflow.

from html import escape
from pathlib import Path
from typing import Any

from .models import AnalysisResults, DynacardAnalysisContext
from .poc_settings import evaluate_alarms, recommend_setpoints
from .troubleshooting import guide_for


FAIL_SEVERITIES = {"critical", "failure"}
TROUBLESHOOTING_ALIASES = {"VALVE_LEAK": "VALVE_LEAK_TV"}


def _classification_verdict(classification: str) -> dict[str, str]:
    """Map a dynacard classification to report severity and pass/fail status."""
    try:
        severity = guide_for(_guide_key(classification)).severity
    except KeyError:
        severity = "warning"
    return {
        "classification": classification,
        "severity": severity,
        "screening_status": "fail" if severity in FAIL_SEVERITIES else "pass",
    }


def _build_alarm_block(
    ctx: DynacardAnalysisContext,
    classification: str,
) -> dict[str, Any]:
    """Build POC setpoints and single-card alarm events for report output."""
    peak = max(ctx.surface_card.load)
    minimum = min(ctx.surface_card.load)
    structure_rating = ctx.surface_unit.beam_rating or None
    stroke = ctx.surface_unit.stroke_length or None
    setpoints = recommend_setpoints(
        peak,
        minimum,
        structure_rating_lbs=structure_rating,
        stroke_length_in=stroke,
        gassy_well=classification == "GAS_INTERFERENCE",
    )
    events = evaluate_alarms(ctx.surface_card, setpoints)
    return {
        "reference": "current_card",
        "note": (
            "Setpoints use the current card as the reference; a field "
            "controller should use a known-normal card."
        ),
        "setpoints": setpoints.model_dump(),
        "alarms": [event.model_dump() for event in events],
    }


def build_diagnostic_report_html(
    cfg: dict[str, Any],
    results: AnalysisResults,
    svg: str,
    verdict: dict[str, str],
    alarm_block: dict[str, Any],
) -> str:
    """Build the self-contained dynacard diagnostic report document."""
    ctx = results.ctx
    lines = _report_header()
    lines.extend(["    <h1>Dynacard Diagnostic Report</h1>"])
    lines.extend(_verdict_section(verdict))
    if ctx is not None:
        lines.extend(_input_section(cfg, ctx, results.solver_method))
        lines.extend(_input_checks_section(cfg, ctx))
    lines.extend(_kpi_section(results))
    lines.extend(_setpoints_section(alarm_block))
    lines.extend(_troubleshooting_section(verdict["classification"]))
    lines.extend([svg, "  </main>", "</body>", "</html>"])
    return "\n".join(lines)


def _esc(value: Any) -> str:
    return escape(str(value), quote=True)


def _fmt(value: Any, places: int = 3) -> str:
    if value is None:
        return ""
    try:
        return f"{float(value):.{places}f}"
    except (TypeError, ValueError):
        return str(value)


def _input_stem(cfg: dict[str, Any]) -> str:
    if cfg.get("_config_file_path"):
        return Path(str(cfg["_config_file_path"])).stem
    analysis = cfg.get("Analysis", {})
    if analysis.get("file_name"):
        return Path(str(analysis["file_name"])).stem
    return "input"


def _rod_configs(cfg: dict[str, Any]) -> list[dict[str, Any]]:
    well_cfg = cfg.get("well", {})
    if "rod_string" in well_cfg:
        return list(well_cfg["rod_string"])
    if "rods" in well_cfg:
        return list(well_cfg["rods"])
    if "rod" in well_cfg:
        return [well_cfg["rod"]]
    well_data = cfg.get("well_data", {})
    return list(well_data.get("rod_string", []))


def _input_warnings(
    cfg: dict[str, Any],
    ctx: DynacardAnalysisContext,
) -> list[str]:
    warnings: list[str] = []
    for index, rod in enumerate(_rod_configs(cfg), start=1):
        if float(rod.get("damping_factor", 0.0) or 0.0) == 0.0:
            warnings.append(
                f"Rod section {index}: no damping specified; higher damping "
                "shrinks the downhole card."
            )
        if float(rod.get("weight_per_foot", 0.0) or 0.0) == 0.0:
            warnings.append(
                f"Rod section {index}: weight_per_foot computed from diameter."
            )
    if ctx.surface_unit.stroke_length == 0.0:
        warnings.append(
            "Surface unit stroke_length is 0; stroke-speed checks are not applied."
        )
    if ctx.surface_unit.beam_rating == 0.0:
        warnings.append(
            "Surface unit beam_rating is 0; structure-rating cap not applied."
        )
    return warnings


def _dl_row(label: str, value: Any, suffix: str = "") -> str:
    return f"      <dt>{_esc(label)}</dt><dd>{_esc(value)}{_esc(suffix)}</dd>"


def _report_header() -> list[str]:
    return [
        "<!doctype html>",
        "<html lang=\"en\">",
        "<head>",
        "  <meta charset=\"utf-8\">",
        "  <title>Dynacard Diagnostic Report</title>",
        "  <style>",
        "    body { font-family: Arial, sans-serif; margin: 24px; color: #202124; }",
        "    main { max-width: 1080px; margin: 0 auto; }",
        "    section { margin: 24px 0; }",
        "    dl { display: grid; grid-template-columns: 220px 1fr; gap: 8px; }",
        "    dt { font-weight: 700; }",
        "    table { border-collapse: collapse; width: 100%; margin: 8px 0; }",
        "    th, td { border: 1px solid #d0d7de; padding: 6px 8px; text-align: left; }",
        "    th { background: #f6f8fa; }",
        "    .verdict { border-left: 6px solid #6a737d; padding: 12px 16px; background: #f6f8fa; }",
        "    .verdict.pass { border-left-color: #1a7f37; }",
        "    .verdict.fail { border-left-color: #cf222e; }",
        "    .warning { color: #8a4600; }",
        "  </style>",
        "</head>",
        "<body>",
        "  <main>",
    ]


def _verdict_section(verdict: dict[str, str]) -> list[str]:
    status = verdict["screening_status"]
    return [
        f"    <section class=\"verdict {_esc(status)}\">",
        "      <h2>Verdict</h2>",
        "      <dl>",
        _dl_row("screening_status", status),
        _dl_row("Classification", verdict["classification"]),
        _dl_row("Severity", verdict["severity"]),
        "      </dl>",
        "    </section>",
    ]


def _input_section(
    cfg: dict[str, Any],
    ctx: DynacardAnalysisContext,
    solver_method: str,
) -> list[str]:
    synthetic = cfg.get("synthetic_card", {})
    lines = [
        "    <section>",
        "      <h2>Inputs</h2>",
        "      <dl>",
        _dl_row("Input stem", _input_stem(cfg)),
        _dl_row("Well", ctx.api14),
        _dl_row("SPM", _fmt(ctx.spm, 3)),
        _dl_row("Solver method", solver_method),
    ]
    if synthetic:
        lines.extend([
            _dl_row("Synthetic mode", synthetic.get("mode", "")),
            _dl_row("Synthetic seed", synthetic.get("seed", "")),
        ])
    lines.extend([
        _dl_row("Pump diameter", _fmt(ctx.pump.diameter, 3), " in"),
        _dl_row("Pump depth", _fmt(ctx.pump.depth, 3), " ft"),
        _dl_row("Stroke length", _fmt(ctx.surface_unit.stroke_length, 3), " in"),
        _dl_row("Beam rating", _fmt(ctx.surface_unit.beam_rating, 0), " lb"),
        "      </dl>",
        "      <h3>Rod string</h3>",
        "      <table>",
        "        <tr><th>#</th><th>Diameter</th><th>Length</th><th>Count</th><th>Damping</th><th>Weight/ft</th></tr>",
    ])
    for index, rod in enumerate(ctx.rod_string, start=1):
        lines.append(
            "        <tr>"
            f"<td>{index}</td>"
            f"<td>{_esc(_fmt(rod.diameter, 3))} in</td>"
            f"<td>{_esc(_fmt(rod.length, 3))} ft</td>"
            f"<td>{_esc(rod.count)}</td>"
            f"<td>{_esc(_fmt(rod.damping_factor, 4))}</td>"
            f"<td>{_esc(_fmt(rod.weight_per_foot, 4))} lb/ft</td>"
            "</tr>"
        )
    lines.extend(["      </table>", "    </section>"])
    return lines


def _input_checks_section(
    cfg: dict[str, Any],
    ctx: DynacardAnalysisContext,
) -> list[str]:
    warnings = _input_warnings(cfg, ctx)
    lines = ["    <section>", "      <h2>Input checks</h2>"]
    if warnings:
        lines.append("      <ul>")
        lines.extend(
            f"        <li class=\"warning\">{_esc(warning)}</li>"
            for warning in warnings
        )
        lines.append("      </ul>")
    else:
        lines.append("      <p>No input lint warnings.</p>")
    lines.append("    </section>")
    return lines


def _kpi_section(results: AnalysisResults) -> list[str]:
    fluid_load = None
    if results.fluid_load is not None:
        fluid_load = results.fluid_load.fluid_load
    load_span = results.peak_polished_rod_load - results.minimum_polished_rod_load
    rows = [
        _dl_row("Diagnostic", results.diagnostic_message),
        _dl_row("Pump fillage", _fmt(results.pump_fillage, 6)),
        _dl_row("Production", _fmt(results.inferred_production, 6), " bbl/day"),
        _dl_row("Buckling detected", results.buckling_detected),
        _dl_row("PPRL", _fmt(results.peak_polished_rod_load, 3), " lb"),
        _dl_row("MPRL", _fmt(results.minimum_polished_rod_load, 3), " lb"),
        _dl_row("Load span", _fmt(load_span, 3), " lb"),
    ]
    if fluid_load is not None:
        rows.append(_dl_row("Fluid load", _fmt(fluid_load, 3), " lb"))
    return [
        "    <section>",
        "      <h2>KPIs</h2>",
        "      <dl>",
        *rows,
        "      </dl>",
        "    </section>",
    ]


def _setpoints_section(alarm_block: dict[str, Any]) -> list[str]:
    setpoints = alarm_block["setpoints"]
    lines = [
        "    <section>",
        "      <h2>POC setpoints &amp; alarms</h2>",
        f"      <p>{_esc(alarm_block['note'])}</p>",
        "      <table>",
        "        <tr><th>Setpoint</th><th>Value</th></tr>",
    ]
    for key, value in setpoints.items():
        if key == "notes":
            continue
        lines.append(f"        <tr><td>{_esc(key)}</td><td>{_esc(value)}</td></tr>")
    lines.extend(["      </table>", "      <h3>Tripped alarms</h3>"])
    return [*lines, *_alarm_rows(alarm_block), *_setpoint_notes(setpoints), "    </section>"]


def _alarm_rows(alarm_block: dict[str, Any]) -> list[str]:
    alarms = alarm_block["alarms"]
    if not alarms:
        return ["      <p>No alarms tripped.</p>"]
    lines = [
        "      <table>",
        "        <tr><th>Name</th><th>Severity</th><th>Observed</th><th>Threshold</th><th>Message</th></tr>",
    ]
    for event in alarms:
        lines.append(
            "        <tr>"
            f"<td>{_esc(event['name'])}</td>"
            f"<td>{_esc(event['severity'])}</td>"
            f"<td>{_esc(_fmt(event['observed'], 3))}</td>"
            f"<td>{_esc(_fmt(event['threshold'], 3))}</td>"
            f"<td>{_esc(event['message'])}</td>"
            "</tr>"
        )
    lines.append("      </table>")
    return lines


def _setpoint_notes(setpoints: dict[str, Any]) -> list[str]:
    notes = setpoints.get("notes") or []
    if not notes:
        return []
    return [
        "      <h3>Notes</h3>",
        "      <ul>",
        *(f"        <li>{_esc(note)}</li>" for note in notes),
        "      </ul>",
    ]


def _troubleshooting_section(classification: str) -> list[str]:
    try:
        entry = guide_for(_guide_key(classification))
    except KeyError:
        return [
            "    <section>",
            "      <h2>Troubleshooting</h2>",
            f"      <p>No troubleshooting catalog entry for {_esc(classification)}.</p>",
            "    </section>",
        ]
    return [
        "    <section>",
        "      <h2>Troubleshooting</h2>",
        f"      <h3>{_esc(entry.title)}</h3>",
        f"      <p><strong>Symptom:</strong> {_esc(entry.symptom)}</p>",
        f"      <p><strong>Mechanism:</strong> {_esc(entry.mechanism)}</p>",
        "      <ol>",
        *(f"        <li>{_esc(action)}</li>" for action in entry.actions),
        "      </ol>",
        "    </section>",
    ]


def _guide_key(classification: str) -> str:
    return TROUBLESHOOTING_ALIASES.get(classification, classification)
