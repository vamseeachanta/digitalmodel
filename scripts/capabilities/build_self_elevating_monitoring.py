# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Render the monitoring demonstrator from the instrument table and alert engine.

Run with PYTHONPATH=src python scripts/capabilities/build_self_elevating_monitoring.py.
No network, numerical library or plotting dependency is required.
"""

import argparse
from html import escape
import json
from pathlib import Path

from digitalmodel.marine_ops.self_elevating_monitoring import (
    FAILURE_MODES,
    SOURCES,
    evaluate,
    synthetic_cases,
)


ROOT = Path(__file__).resolve().parents[2]
OUTPUT = "docs/api/hydro/self-elevating-monitoring.html"
TEMPLATE = Path(__file__).with_name("self_elevating_monitoring.html.template")


def demo_payload():
    """Export evaluator results and plot values in readable engineering units."""
    cases = synthetic_cases()
    result = {}
    for name, case in cases.items():
        alerts = evaluate(case.time_s, case.channels, case.rules)
        charts = []
        if name == "storm":
            for channel, title, unit, scale in (
                ("inclination_deg", "Leg inclination", "mdeg", 1000),
                ("strain_microstrain", "Leg strain", "µε", 1),
            ):
                rule = next(r for r in case.rules if r.channel == channel)
                charts.append(dict(title=title, unit=unit,
                                   traces=[dict(label=title, values=[round(v*scale, 1)
                                           for v in case.channels[channel]])],
                                   threshold=rule.threshold*scale,
                                   limit=case.limits[channel]*scale))
        else:
            charts.append(dict(title="Preload settlement: stops versus accelerates", unit="mm",
                               traces=[dict(label=key, values=[round(v*1000, 1) for v in
                                       cases[key].channels["settlement_m"]])
                                       for key in ("settled", "accelerating")]))
            charts.append(dict(title="Applied preload per leg", unit="kN",
                               traces=[dict(label="preload", values=[round(v, 1)
                                       for v in case.channels["preload_kn"]])]))
        result[name] = dict(
            time_s=list(case.time_s), event_s=case.event_s, charts=charts,
            alerts=[dict(rule=a.rule, time_s=a.time_s,
                         lead_min=round((case.event_s-a.time_s)/60, 1)) for a in alerts],
        )
    return result


def render_page():
    """Build a deterministic standalone page with static source and warning tables."""
    mappings = []
    for mode in FAILURE_MODES:
        citations = " ".join(f'<a href="#source-{key}">[{key}]</a>' for key in mode.sources)
        cells = [escape(mode.name), "<br>".join(map(escape, mode.instruments)),
                 escape(mode.precursor), escape(mode.logic), escape(mode.lead_time), citations]
        mappings.append("<tr>" + "".join(f"<td>{c}</td>" for c in cells) + "</tr>")
    sources = []
    for key, source in SOURCES.items():
        sources.append(f'<li id="source-{key}"><a href="{escape(source.url)}">'
                       f'{escape(source.title)}</a>. {escape(source.locator)}. '
                       f'Terms: {escape(source.terms)}. Retrieved {source.retrieved}.</li>')
    rules, summaries = [], []
    for name, case in synthetic_cases().items():
        for rule in case.rules:
            scale, unit = {
                "m/s": (1000, "mm/s"), "m/s²": (1e6, "µm/s²"), "deg": (1000, "mdeg"),
            }.get(rule.unit, (1, rule.unit))
            rules.append("<tr>" + "".join(f"<td>{escape(str(v))}</td>" for v in (
                name, rule.name, rule.metric, f"{rule.threshold*scale:.1f} {unit}",
                rule.persistence, f"{rule.max_gap_s:.1f} s")) + "</tr>")
        alerts = evaluate(case.time_s, case.channels, case.rules)
        for alert in alerts:
            summaries.append(f"<tr><td>{name}</td><td>{escape(alert.rule)}</td>"
                             f"<td>{alert.time_s/60:.1f}</td><td>{case.event_s/60:.1f}</td>"
                             f"<td>{(case.event_s-alert.time_s)/60:.1f}</td></tr>")
    summaries.insert(0, "<tr><td>settled</td><td>No configured alert</td>"
                        "<td>—</td><td>No imposed event</td><td>—</td></tr>")
    replacements = {
        "@@MAPPINGS@@": "\n".join(mappings), "@@SOURCES@@": "\n".join(sources),
        "@@RULES@@": "\n".join(rules), "@@SUMMARY@@": "\n".join(summaries),
        "@@DATA@@": json.dumps(demo_payload(), ensure_ascii=False, separators=(",", ":")),
    }
    page = TEMPLATE.read_text(encoding="utf-8")
    for key, value in replacements.items():
        page = page.replace(key, value)
    return page


def main():
    """Write the documented output or a caller-selected reproduction target."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=ROOT / OUTPUT)
    args = parser.parse_args()
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(render_page(), encoding="utf-8", newline="\n")


if __name__ == "__main__":
    main()
