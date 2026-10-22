"""Build the self-contained tug report from the public regression example.

From the repository root: python docs/reports/modules/tug/build_report.py
Only the Python standard library and source checkout are required.
"""

import argparse
import ast
from dataclasses import asdict
from html import escape
import json
import math
from pathlib import Path
import re
from string import Template
import sys

ROOT = Path(__file__).resolve().parents[4]
HERE = Path(__file__).resolve().parent
EXEMPLAR = "docs/api/cfd/turbulent-flat-plate-verification.html"
FIXTURE = "tests/tug/test_tug_toolkit.py"
sys.path.insert(0, str(ROOT / "src"))

from digitalmodel.tug.emissions import OperatingPhase, cycle_energy, size_hybrid


def read_example():
    """Read literals from the public test without executing its imports."""
    tree = ast.parse((ROOT / FIXTURE).read_text(encoding="utf-8"))
    suite = next(n for n in tree.body
                 if isinstance(n, ast.ClassDef) and n.name == "TestEmissions")
    profile = next(n.value for n in suite.body if isinstance(n, ast.Assign)
                   and n.targets[0].id == "PROFILE")
    rows = [tuple(ast.literal_eval(a) for a in call.args)
            for call in profile.elts]
    case = next(n for n in suite.body if isinstance(n, ast.FunctionDef)
                and n.name == "test_hybrid_sizing_shaves_assist_peak")
    call = next(n for n in ast.walk(case) if isinstance(n, ast.Call)
                and isinstance(n.func, ast.Name) and n.func.id == "size_hybrid")
    power = ast.literal_eval(call.args[0])
    baseline = next(ast.literal_eval(k.value) for k in call.keywords
                    if k.arg == "baseline_fraction")
    counts = {n.name: sum(isinstance(f, ast.FunctionDef)
                         and f.name.startswith("test_") for f in n.body)
              for n in tree.body if isinstance(n, ast.ClassDef)}
    return power, rows, baseline, counts


def validate_profile(power, rows, baseline):
    """Reject nonphysical inputs before the permissive sizing API is called."""
    if not math.isfinite(power) or power <= 0 or not 0 < baseline < 1 or not rows:
        raise ValueError("Positive finite power and a bounded baseline are required")
    for name, fraction, duration in rows:
        if (not name or not math.isfinite(fraction) or not 0 <= fraction <= 1
                or not math.isfinite(duration) or duration <= 0):
            raise ValueError("Finite fractions and positive durations are required")


def calculate():
    power, rows, baseline, counts = read_example()
    validate_profile(power, rows, baseline)
    profile = [OperatingPhase(*row) for row in rows]
    result = asdict(size_hybrid(power, profile, baseline))
    if result["total_energy_kwh"] != cycle_energy(power, profile):
        raise ValueError("Cycle energy and hybrid energy disagree")
    return {
        "input": {"installed_power_kw": power, "baseline_fraction": baseline,
                  "profile": [asdict(p) for p in profile]},
        "result": result,
        "phases": [{"name": p.name,
                    "result": asdict(size_hybrid(power, [p], baseline))}
                   for p in profile],
        "test_inventory": counts,
    }


def lookup(data, key):
    value = data
    for part in key.split("."):
        value = value[int(part)] if isinstance(value, list) else value[part]
    return value


def number(data, key):
    return f'<span data-value="{key}">{lookup(data, key):,.3f}</span>'


def tables(data):
    inputs = '<tr><th>Phase</th><th>Power fraction (–)</th><th>Duration (h)</th><th>Source</th></tr>'
    results = '<tr><th>Quantity</th><th>Value</th><th>Unit</th><th>Source</th></tr>'
    for i, phase in enumerate(data["input"]["profile"]):
        inputs += (f'<tr><td>{escape(phase["name"])}</td><td>'
                   + number(data, f"input.profile.{i}.power_fraction") + '</td><td>'
                   + number(data, f"input.profile.{i}.duration_h")
                   + '</td><td><a href="#source-example">Public example</a></td></tr>')
    labels = [("total_energy_kwh", "Cycle energy", "kWh"),
              ("peak_power_kw", "Peak demand", "kW"),
              ("baseline_engine_kw", "Engine baseline", "kW"),
              ("battery_power_kw", "Peak battery discharge", "kW"),
              ("battery_energy_kwh", "Battery discharge energy", "kWh"),
              ("peak_energy_fraction", "Battery share of cycle energy", "–")]
    for key, label, unit in labels:
        results += (f'<tr><td>{label}</td><td>' + number(data, f"result.{key}")
                    + f'</td><td>{unit}</td><td><a href="#source-api">Sizing calculation</a></td></tr>')
    return inputs, results


def chart(data):
    """SVG is self-contained; titles, keyboard focus and selection expose values."""
    power = data["input"]["installed_power_kw"]
    baseline = data["result"]["baseline_engine_kw"]
    y = lambda value: 260 - 190 * value / power
    parts = ['<svg viewBox="0 0 760 330" style="width:100%;height:auto" role="img" aria-label="Phase power demand and engine baseline">',
             '<path d="M70 45 V260 H720" fill="none" stroke="currentColor"/>',
             '<text x="12" y="30">Power (kW)</text>',
             f'<text x="14" y="264">{0:.0f}</text>',
             f'<text x="8" y="74">{power:.0f}</text>',
             f'<path d="M70 {y(baseline):.3f} H720" stroke="var(--nogo)" stroke-dasharray="8 5"/>',
             f'<text x="440" y="{y(baseline)-10:.3f}">Engine baseline: {baseline:.0f} kW</text>']
    for i, row in enumerate(data["phases"]):
        r, x = row["result"], 150 + 210 * i
        for offset, key, label, colour in [
                (-38, "peak_power_kw", "Propulsion demand", "var(--brand)"),
                (12, "battery_power_kw", "Battery discharge", "var(--caution)")]:
            value = r[key]
            tip = (f'Public example; {row["name"]}; {label}; demand {value:.3f} kW; '
                   f'engine baseline {baseline:.3f} kW; battery rating NOT_EVALUATED')
            parts.append(f'<rect tabindex="0" x="{x+offset}" y="{y(value):.3f}" '
                         f'width="42" height="{max(1,260-y(value)):.3f}" fill="{colour}" '
                         f'data-detail="{escape(tip)}"><title>{escape(tip)}</title></rect>')
        parts.append(f'<text x="{x}" y="290" text-anchor="middle">{escape(row["name"])}</text>')
    parts.append('<text x="220" y="320">Phase (categorical; bar width does not represent duration)</text></svg>')
    return "\n".join(parts)


def changes():
    return {"revision": "A", "status": "Draft for maintainer review", "changes": [
        "Initial issue: ideal duty-cycle energy and peak battery discharge are evaluated using the public example.",
        "Battery rating, recharge, reserve and installation weather limits remain not evaluated."
    ]}


def render(data):
    source = (ROOT / EXEMPLAR).read_text(encoding="utf-8")
    style = re.search(r"<style>.*?</style>", source, re.S).group()
    brand = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    inputs, results = tables(data)
    values = {k: number(data, f"result.{k}") for k in data["result"]}
    values.update(power=number(data, "input.installed_power_kw"),
                  baseline=number(data, "input.baseline_fraction"),
                  input_rows=inputs, result_rows=results, chart=chart(data),
                  test_rows="".join(f'<tr><td>{escape(k.removeprefix("Test"))}</td><td>{v}</td>'
                                    '<td><a href="#source-tests">Test definitions</a></td></tr>'
                                    for k, v in data["test_inventory"].items()),
                  test_count=sum(data["test_inventory"].values()),
                  styles=f"<style>{brand}</style>\n{style}",
                  calculation=json.dumps(data, ensure_ascii=True, sort_keys=True),
                  change_data=json.dumps(changes(), ensure_ascii=True),
                  interactions=(HERE / "interactions.js").read_text(encoding="utf-8"))
    return Template((HERE / "report.html.template").read_text(
        encoding="utf-8")).substitute(values)


def build(output):
    output = Path(output)
    output.mkdir(parents=True, exist_ok=True)
    (output / "index.html").write_text(render(calculate()), encoding="utf-8", newline="\n")
    (output / "changes.json").write_text(
        json.dumps(changes(), indent=2) + "\n", encoding="utf-8", newline="\n")


def check(output):
    """Compare fresh in-memory output with saved files without writing."""
    expected = {"index.html": render(calculate()),
                "changes.json": json.dumps(changes(), indent=2) + "\n"}
    for name, content in expected.items():
        if (Path(output) / name).read_text(encoding="utf-8") != content:
            raise ValueError(f"Stale {name}; run python docs/reports/modules/tug/build_report.py")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=HERE)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    check(args.output) if args.check else build(args.output)
