"""Cross-machine HTML report built from benchmark receipts (#2300).

For every case and variant the report states which machines ran it, how long
the solve took on each, and whether the fingerprints from different machines
agree within the case tolerance. Standard library only: the pack must stay
importable on hosts without the full environment.

Rules the page follows:

* only the most recent receipt (by ``started_utc``) per machine label and
  case/variant is used, and the number supplied is stated;
* results that are not baseline-eligible are shown and marked, never compared;
* receipts of different pack versions are reported in separate sections;
* fingerprint comparison is ``compare.fingerprint_status`` (the ``compare``
  subcommand's rule), so a differing solver version is reported, not failed;
* only the machine label and the receipt's environment block are shown; free
  text goes through ``runner.sanitise`` and everything is HTML-escaped.
"""

from __future__ import annotations

import datetime as _dt
import html as _html
import json
import math
from pathlib import Path

from . import compare, runner

SERIES_COLOURS = ("#2a78d6", "#eb6834", "#1baf7a", "#eda100")
_OVERFLOW_COLOUR = "#8a8f98"  # variants beyond the fixed palette; labelled directly

AGREE = "agree"
DIFFER = "differ"
VERSION = "version"
INPUT = "input"
ONE_MACHINE = "one-machine"
NOT_COMPARED = "not-compared"

STATUS_LABEL = {
    AGREE: "agree within tolerance",
    DIFFER: "differ beyond tolerance",
    VERSION: "different solver version",
    INPUT: "input files differ",
    ONE_MACHINE: "one machine only; no cross-machine comparison",
    NOT_COMPARED: "not compared: fewer than two baseline-eligible results",
}
_STATUS_ORDER = (AGREE, DIFFER, VERSION, INPUT, NOT_COMPARED, ONE_MACHINE)
_ENV_FIELDS = (("os", "Operating system"), ("cpu_model", "CPU"),
               ("logical_cores", "Logical cores"), ("physical_cores", "Physical cores"),
               ("ram_gb", "RAM (GB)"))
_MISSING = "not recorded"


# ------------------------------------------------------------------ the model


def load_receipts(paths) -> list[dict]:
    receipts = []
    for path in paths:
        path = Path(path)
        try:
            receipt = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, ValueError) as exc:
            raise ValueError(f"{path.name} cannot be read as JSON: {exc}") from None
        _check(receipt, path.name)
        receipts.append(receipt)
    return receipts


def _check(receipt, name: str) -> None:
    if (not isinstance(receipt, dict) or not isinstance(receipt.get("results"), list)
            or not receipt.get("machine_label") or "pack_version" not in receipt
            or not all(isinstance(e, dict) and "case" in e and "variant" in e
                       for e in receipt["results"])):
        raise ValueError(f"{name} is not a benchmark receipt (needs pack_version, "
                         f"machine_label and results[] with case and variant)")


def _when(text) -> _dt.datetime:
    try:
        stamp = _dt.datetime.fromisoformat(str(text))
    except ValueError:
        return _dt.datetime.min.replace(tzinfo=_dt.timezone.utc)
    return stamp if stamp.tzinfo else stamp.replace(tzinfo=_dt.timezone.utc)


def _variant_key(variant) -> tuple:
    if isinstance(variant, (int, float)) and not isinstance(variant, bool):
        return (0, variant, "")
    return (1, 0, str(variant))


def _unique(items) -> list:
    return list(dict.fromkeys(items))


def _row(label: str, candidates: list) -> dict:
    """The newest of the supplied (receipt order, receipt, entry) candidates."""
    ranked = sorted(candidates, key=lambda c: (_when(c[1].get("started_utc")), c[0]))
    _, receipt, entry = ranked[-1]
    repeats = receipt.get("repeats")
    n_ok = entry.get("n_ok") or 0
    eligible = bool(entry.get("baseline_eligible"))
    consistent = bool(entry.get("fingerprint_consistent"))
    reasons = []
    if not eligible:
        if entry.get("skipped"):
            reasons.append(f"run skipped: {runner.sanitise(entry['skipped'])}")
        if entry.get("busy_allowed"):
            reasons.append("the host was busy during the run")
        if isinstance(repeats, int) and n_ok < repeats:
            reasons.append(f"only {n_ok} of {repeats} repeats completed")
        if n_ok and not consistent:
            reasons.append("the fingerprint differed between repeats on this machine")
        if not reasons:
            reasons.append("the receipt marks this result as not baseline-eligible")
    return {
        "machine": label,
        "entry": entry,
        "started_utc": receipt.get("started_utc"),
        "solver_version": entry.get("solver_version"),
        "n_ok": n_ok,
        "repeats": repeats,
        "solve_s": entry.get("solve_s") or None,
        "timing_basis": entry.get("timing_basis"),
        "consistent": consistent,
        "eligible": eligible,
        # comparable: eligible and passes compare's own completeness checks
        "comparable": eligible and compare.fingerprint_status(entry, entry) == "MATCH",
        "reasons": reasons,
        "errors": _unique(runner.sanitise(e) for e in entry.get("errors") or []),
        "supplied": len(ranked),
        "older_eligible": sum(bool(c[2].get("baseline_eligible")) for c in ranked[:-1]),
    }


def _tolerance_text(rows: list) -> str:
    rel = max(r["entry"].get("rel_tol", 1e-6) for r in rows)
    ab = max(r["entry"].get("abs_tol", 0.0) for r in rows)
    return f"relative {rel:g}, absolute {ab:g}"


def _agreement(rows: list) -> dict:
    """Compare the comparable rows of one case/variant pairwise."""
    usable = [r for r in rows if r["comparable"]]
    result = {"machines": [r["machine"] for r in usable],
              "excluded": [r["machine"] for r in rows if not r["comparable"]],
              "pairs": [], "differing_keys": [], "key_status": {}}
    if len(rows) < 2:
        result.update(status=ONE_MACHINE, machines=[], excluded=[],
                      detail="Only one machine supplied a receipt for this case and variant.")
        return result
    if len(usable) < 2:
        result.update(status=NOT_COMPARED, machines=[], detail=(
            f"{len(usable)} of the {len(rows)} machines that ran this have a "
            f"baseline-eligible result, so nothing was compared."))
        return result

    sentences, keys, seen = [], [], set()
    for i, a in enumerate(usable):
        for b in usable[i + 1:]:
            status = compare.fingerprint_status(b["entry"], a["entry"])
            seen.add(status)
            result["pairs"].append((a["machine"], b["machine"], status))
            if status == "MATCH":
                continue
            names = f"{a['machine']} and {b['machine']}"
            if status == "INPUT_CHANGED":
                sentences.append(f"The input files differ between {names}, so their "
                                 f"fingerprints were not compared.")
                continue
            differing = compare.differing_keys(b["entry"], a["entry"])
            keys.extend(differing)
            where = ", ".join(map(str, differing))
            if status == "VERSION_CHANGED":
                sentences.append(
                    f"{a['machine']} (solver version {a['solver_version'] or _MISSING}) and "
                    f"{b['machine']} (solver version {b['solver_version'] or _MISSING}) differ "
                    f"in {where}; with different solver versions this is reported, "
                    f"not counted as a disagreement.")
            else:
                sentences.append(f"{names} ran the same solver version and differ in {where}.")
    result["differing_keys"] = _unique(keys)

    compared = ", ".join(result["machines"])
    tolerance = _tolerance_text(usable)
    if "MISMATCH" in seen:
        status = DIFFER
        lead = f"Fingerprints from {compared} were compared ({tolerance})."
    elif "INPUT_CHANGED" in seen:
        status = INPUT
        lead = f"Results from {compared} were considered."
    elif "VERSION_CHANGED" in seen:
        status = VERSION
        lead = f"Fingerprints from {compared} were compared ({tolerance})."
    else:
        status = AGREE
        lead = f"Fingerprints agree within tolerance across {compared} ({tolerance})."
    if result["excluded"]:
        sentences.append("Excluded from this comparison as not baseline-eligible: "
                         + ", ".join(result["excluded"]) + ".")
    result.update(status=status, detail=" ".join([lead, *sentences]))
    if "INPUT_CHANGED" not in seen:
        every = _unique(k for r in usable for k in r["entry"]["fingerprint"])
        result["key_status"] = {k: "differ" if k in result["differing_keys"] else "agree"
                                for k in every}
    return result


def _pack(version: str, receipts: list) -> dict:
    by_label: dict = {}
    candidates: dict = {}
    for order, receipt in receipts:
        label = str(receipt["machine_label"])
        by_label.setdefault(label, []).append((order, receipt))
        for entry in receipt["results"]:
            key = (str(entry["case"]), entry["variant"], label)
            candidates.setdefault(key, []).append((order, receipt, entry))

    machines = []
    for label in sorted(by_label):
        ranked = sorted(by_label[label],
                        key=lambda c: (_when(c[1].get("started_utc")), c[0]))
        latest = ranked[-1][1]
        environment = latest.get("environment")
        machines.append({"label": label, "receipts_supplied": len(ranked),
                         "latest_started_utc": latest.get("started_utc"),
                         "environment": environment if isinstance(environment, dict) else {}})

    cases: dict = {}
    for (case_name, variant, label), found in candidates.items():
        entry = found[-1][2]
        case = cases.setdefault(case_name, {"case": case_name, "solver": entry.get("solver"),
                                            "variant_label": entry.get("variant_label")
                                            or "variant", "by_variant": {}})
        case["by_variant"].setdefault(variant, []).append(_row(label, found))

    summary = dict.fromkeys(_STATUS_ORDER, 0)
    summary.update(variants=0, results=0, ineligible=0)
    out_cases = []
    for name in sorted(cases):
        case = cases[name]
        by_variant = case.pop("by_variant")
        variants = []
        for variant in sorted(by_variant, key=_variant_key):
            rows = sorted(by_variant[variant], key=lambda r: r["machine"])
            agreement = _agreement(rows)
            variants.append({"variant": variant, "rows": rows, "agreement": agreement})
            summary[agreement["status"]] += 1
            summary["variants"] += 1
            summary["results"] += len(rows)
            summary["ineligible"] += sum(not r["eligible"] for r in rows)
        case["variants"] = variants
        case["machines"] = sorted({r["machine"] for v in variants for r in v["rows"]})
        out_cases.append(case)
    return {"pack_version": version, "machines": machines, "cases": out_cases,
            "summary": summary}


def build_report(receipts: list[dict]) -> dict:
    """Group receipts by pack version and work out timings and agreement."""
    if not receipts:
        raise ValueError("no receipts supplied")
    by_version: dict = {}
    for order, receipt in enumerate(receipts):
        _check(receipt, f"receipt {order + 1}")
        by_version.setdefault(str(receipt["pack_version"]), []).append((order, receipt))
    return {
        "generated_utc": _dt.datetime.now(_dt.timezone.utc).isoformat(timespec="seconds"),
        "receipts_supplied": len(receipts),
        "packs": [_pack(v, by_version[v]) for v in sorted(by_version)],
    }


# ------------------------------------------------------------------ rendering


def _e(value) -> str:
    return _html.escape(str(value), quote=True)


def _seconds(value) -> str:
    if not isinstance(value, (int, float)) or not math.isfinite(value):
        return "–"
    if value >= 100:
        return f"{value:.0f}"
    return f"{value:.1f}" if value >= 10 else f"{value:.2f}"


def _number(value) -> str:
    if isinstance(value, list):
        shown = ", ".join(_number(v) for v in value[:8])
        return shown if len(value) <= 8 else f"{shown}, … ({len(value)} values)"
    if isinstance(value, bool) or value is None:
        return "–" if value is None else str(value).lower()
    if isinstance(value, int):
        return str(value)
    if isinstance(value, float):
        return f"{value:.6g}"
    return str(value)


def _variant_name(case: dict, variant) -> str:
    return f"{case['variant_label']} = {variant}"


def _nice_axis(top: float) -> tuple[float, float]:
    """(step, axis maximum) giving three to six round ticks above zero."""
    raw = top / 4
    magnitude = 10 ** math.floor(math.log10(raw))
    step = next(m * magnitude for m in (1, 2, 5, 10) if m * magnitude >= raw)
    return step, step * math.ceil(top / step - 1e-9)


def _bar_path(x: float, y: float, w: float, h: float) -> str:
    r = min(4.0, w / 2, h)
    return (f"M{x:.1f},{y + h:.1f} V{y + r:.1f} Q{x:.1f},{y:.1f} {x + r:.1f},{y:.1f} "
            f"H{x + w - r:.1f} Q{x + w:.1f},{y:.1f} {x + w:.1f},{y + r:.1f} V{y + h:.1f} Z")


def render_chart(case: dict) -> str:
    """Grouped bars: machines on x, median solve time on the single y axis."""
    variants = [v["variant"] for v in case["variants"]]
    machines = case["machines"]
    cells = {(v["variant"], r["machine"]): r for v in case["variants"] for r in v["rows"]}
    timed = [r for r in cells.values() if r["solve_s"]]
    tops = [t for r in timed for t in (r["solve_s"].get("max"), r["solve_s"].get("median"))
            if isinstance(t, (int, float)) and math.isfinite(t) and t > 0]
    if not tops:
        return '<p class="note">No completed solve times to chart for this case.</p>'

    colour = {v: SERIES_COLOURS[i] if i < len(SERIES_COLOURS) else _OVERFLOW_COLOUR
              for i, v in enumerate(variants)}
    step, y_max = _nice_axis(max(tops))
    n_var = len(variants)
    bar_w, gap = 34.0, 2.0
    group_w = max(110.0, n_var * (bar_w + gap) + 40)
    left, right, top, bottom = 56.0, 16.0, 18.0, 58.0
    width = left + right + group_w * len(machines)
    height = 300.0
    plot_h = height - top - bottom
    base_y = top + plot_h

    def y_of(value: float) -> float:
        return base_y - plot_h * value / y_max

    label_values = len(timed) <= 12
    title = f"Median solve time by machine for {case['case']}"
    out = [f'<svg viewBox="0 0 {width:.0f} {height:.0f}" width="{width:.0f}" '
           f'height="{height:.0f}" role="img" aria-label="{_e(title)}">']
    tick = 0.0
    while tick <= y_max + step / 2:
        y = y_of(tick)
        out.append(f'<line class="{"axis" if tick == 0 else "grid"}" x1="{left:.1f}" '
                   f'x2="{width - right:.1f}" y1="{y:.1f}" y2="{y:.1f}"/>')
        out.append(f'<text class="tick" x="{left - 8:.1f}" y="{y + 4:.1f}" '
                   f'text-anchor="end">{tick:g}</text>')
        tick += step
    out.append(f'<text class="tick" x="{left - 8:.1f}" y="{top - 6:.1f}" '
               f'text-anchor="end">s</text>')

    any_ineligible = False
    for m_index, machine in enumerate(machines):
        centre = left + group_w * (m_index + 0.5)
        start = centre - (n_var * bar_w + (n_var - 1) * gap) / 2
        for v_index, variant in enumerate(variants):
            x = start + v_index * (bar_w + gap)
            mid = x + bar_w / 2
            row = cells.get((variant, machine))
            name = _variant_name(case, variant)
            if row is None:
                continue
            out.append(f'<text class="tick" x="{mid:.1f}" y="{base_y + 15:.1f}" '
                       f'text-anchor="middle">{_e(variant)}</text>')
            median = (row["solve_s"] or {}).get("median")
            if not (isinstance(median, (int, float)) and math.isfinite(median) and median > 0):
                out.append(f'<text class="tick" x="{mid:.1f}" y="{base_y - 6:.1f}" '
                           f'text-anchor="middle">–<title>{_e(machine)}, {_e(name)}: '
                           f'no completed repeat</title></text>')
                continue
            low, high = row["solve_s"].get("min"), row["solve_s"].get("max")
            y = y_of(median)
            marked = "" if row["eligible"] else " ineligible"
            any_ineligible = any_ineligible or not row["eligible"]
            tip = (f"{machine}, {name}: median {_seconds(median)} s, min {_seconds(low)} s, "
                   f"max {_seconds(high)} s over {row['n_ok']} successful repeats"
                   + ("" if row["eligible"] else "; not baseline-eligible"))
            style = (f'fill="{colour[variant]}"' if row["eligible"] else
                     f'stroke="{colour[variant]}"')
            out.append(f'<g><title>{_e(tip)}</title>'
                       f'<path class="bar{marked}" {style} '
                       f'd="{_bar_path(x, y, bar_w, base_y - y)}"/>')
            if all(isinstance(t, (int, float)) and math.isfinite(t) for t in (low, high)):
                out.append(f'<line class="whisker" x1="{mid:.1f}" x2="{mid:.1f}" '
                           f'y1="{y_of(high):.1f}" y2="{y_of(low):.1f}"/>')
                label_y = y_of(high) - 5
            else:
                label_y = y - 5
            if label_values:
                out.append(f'<text class="value" x="{mid:.1f}" y="{label_y:.1f}" '
                           f'text-anchor="middle">{_seconds(median)}</text>')
            out.append("</g>")
        out.append(f'<text class="machine" x="{centre:.1f}" y="{base_y + 36:.1f}" '
                   f'text-anchor="middle">{_e(machine)}</text>')
    out.append("</svg>")

    legend = []
    if n_var > 1:
        legend = [f'<span class="key"><span class="swatch" style="background:{colour[v]}">'
                  f'</span>{_e(_variant_name(case, v))}</span>' for v in variants]
    if any_ineligible:
        legend.append('<span class="key"><span class="swatch hollow"></span>dashed outline: '
                      'not baseline-eligible, excluded from cross-machine comparison</span>')
    legend.append('<span class="key"><span class="swatch line"></span>'
                  'thin line: fastest to slowest repeat</span>')
    single = "" if n_var > 1 else f" ({_e(_variant_name(case, variants[0]))})"
    return (f'<figure><figcaption>Median solve time in seconds by machine{single}. '
            f'The number under each bar is the {_e(case["variant_label"])} count.'
            f'</figcaption><div class="{"legend" if n_var > 1 else "chart-key"}">'
            f'{"".join(legend)}</div><div class="scroll">{"".join(out)}</div></figure>')


def _repeat_consistency(row: dict) -> str:
    if not row["n_ok"]:
        return "no completed repeat"
    if row["n_ok"] == 1:
        return "only one repeat completed"
    return "yes" if row["consistent"] else "no"


def _row_notes(row: dict) -> list[str]:
    notes = []
    if not row["eligible"]:
        notes.append("<strong>Not baseline-eligible</strong>: "
                     + _e("; ".join(row["reasons"])) + ". Excluded from cross-machine "
                     "comparison.")
    for error in row["errors"][:5]:
        notes.append(f"Recorded error: <code>{_e(error)}</code>")
    if len(row["errors"]) > 5:
        notes.append(f"{len(row['errors']) - 5} further recorded errors not shown.")
    if row["timing_basis"] not in (None, "solver"):
        notes.append(f"Timing basis: {_e(row['timing_basis'])}.")
    if row["supplied"] > 1:
        older = row["supplied"] - 1
        notes.append(
            f"Uses the most recent of {row['supplied']} receipts supplied for this machine and "
            f"variant (started {_e(row['started_utc'] or _MISSING)}); "
            f"older receipts not shown: {older}, of which baseline-eligible: "
            f"{row['older_eligible']}.")
    return notes


def _timing_table(case: dict) -> str:
    head = ("Variant", "Machine", "Solver version", "Successful repeats", "Median (s)",
            "Min (s)", "Max (s)", "Same fingerprint on every repeat", "Baseline-eligible")
    out = ['<div class="scroll"><table><thead><tr>',
           "".join(f"<th>{h}</th>" for h in head), "</tr></thead>"]
    for variant in case["variants"]:
        out.append("<tbody>")
        name = _e(_variant_name(case, variant["variant"]))
        for row in variant["rows"]:
            solve = row["solve_s"] or {}
            repeats = row["repeats"] if isinstance(row["repeats"], int) else "?"
            out.append(
                f'<tr class="{"" if row["eligible"] else "ineligible"}"><td>{name}</td>'
                f'<td>{_e(row["machine"])}</td>'
                f'<td>{_e(row["solver_version"] or _MISSING)}</td>'
                f'<td class="num">{row["n_ok"]} of {_e(repeats)}</td>'
                f'<td class="num">{_seconds(solve.get("median"))}</td>'
                f'<td class="num">{_seconds(solve.get("min"))}</td>'
                f'<td class="num">{_seconds(solve.get("max"))}</td>'
                f'<td>{_repeat_consistency(row)}</td>'
                f'<td>{"yes" if row["eligible"] else "<strong>NO</strong>"}</td></tr>')
            notes = _row_notes(row)
            if notes:
                out.append(f'<tr class="notes"><td></td><td colspan="{len(head) - 1}">'
                           + "<br>".join(notes) + "</td></tr>")
        agreement = variant["agreement"]
        out.append(
            f'<tr class="across"><td colspan="{len(head)}">Across machines, {name}: '
            f'<span class="status s-{agreement["status"]}">'
            f'{_e(STATUS_LABEL[agreement["status"]])}</span> {_e(agreement["detail"])}'
            f'</td></tr></tbody>')
    out.append("</table></div>")
    return "".join(out)


def _fingerprint_table(case: dict) -> str:
    machines = case["machines"]
    out = ['<div class="scroll"><table><thead><tr><th>Variant</th><th>Quantity</th>',
           "".join(f"<th>{_e(m)}</th>" for m in machines),
           "<th>Across machines</th></tr></thead><tbody>"]
    any_row = False
    for variant in case["variants"]:
        by_machine = {r["machine"]: r for r in variant["rows"]}
        keys = _unique(k for r in variant["rows"]
                       for k in (r["entry"].get("fingerprint") or {}))
        agreement = variant["agreement"]
        for key in keys:
            any_row = True
            values = []
            for machine in machines:
                row = by_machine.get(machine)
                fingerprint = (row["entry"].get("fingerprint") or {}) if row else {}
                if key not in fingerprint:
                    values.append('<td class="num">–</td>')
                    continue
                flag = "" if row["comparable"] else (
                    '<br><span class="flag">not baseline-eligible; first completed '
                    'repeat shown, not compared</span>')
                values.append(f'<td class="num">{_e(_number(fingerprint[key]))}{flag}</td>')
            verdict = agreement["key_status"].get(key) or (
                "not compared" if agreement["status"] != ONE_MACHINE else "one machine only")
            out.append(f"<tr><td>{_e(_variant_name(case, variant['variant']))}</td>"
                       f"<td>{_e(key)}</td>{''.join(values)}<td>{_e(verdict)}</td></tr>")
    if not any_row:
        return '<p class="note">No fingerprint was recorded for this case.</p>'
    out.append("</tbody></table></div>")
    return "".join(out)


def _machine_table(pack: dict) -> str:
    out = ['<div class="scroll"><table><thead><tr><th>Machine label</th>',
           "".join(f"<th>{title}</th>" for _, title in _ENV_FIELDS),
           "<th>Receipts supplied</th><th>Most recent run started (UTC)</th>"
           "</tr></thead><tbody>"]
    for machine in pack["machines"]:
        cells = []
        for field, _ in _ENV_FIELDS:
            value = machine["environment"].get(field)
            cells.append(f"<td>{_e(_MISSING if value is None else _number(value))}</td>")
        out.append(f'<tr><td>{_e(machine["label"])}</td>{"".join(cells)}'
                   f'<td class="num">{machine["receipts_supplied"]}</td>'
                   f'<td>{_e(machine["latest_started_utc"] or _MISSING)}</td></tr>')
    out.append("</tbody></table></div>")
    return "".join(out)


def _summary(pack: dict) -> str:
    summary = pack["summary"]
    items = [f'<li><span class="status s-{status}">{_e(STATUS_LABEL[status])}</span> '
             f'{summary[status]}</li>' for status in _STATUS_ORDER if summary[status]]
    eligible = summary["results"] - summary["ineligible"]
    return (f'<p>{len(pack["machines"])} machine label(s), {len(pack["cases"])} case(s), '
            f'{summary["variants"]} case/variant combination(s). Machine results used: '
            f'{summary["results"]}, of which baseline-eligible: {eligible}, not '
            f'baseline-eligible: {summary["ineligible"]}. Cross-machine outcome, counted '
            f'per case/variant combination:</p><ul class="summary">{"".join(items)}</ul>')


_CSS = """
:root{--surface:#ffffff;--panel:#f6f7f9;--ink:#1c2026;--muted:#5b6370;--rule:#d9dde3;
--grid:#e6e9ee;--warn:#fff4e0}
@media (prefers-color-scheme:dark){:root{--surface:#15181d;--panel:#1d2128;--ink:#e8ebf0;
--muted:#a3abb8;--rule:#39404b;--grid:#2a3038;--warn:#3a2f17}}
body{margin:0;background:var(--surface);color:var(--ink);
font:15px/1.5 system-ui,-apple-system,"Segoe UI",Roboto,sans-serif}
main{max-width:1080px;margin:0 auto;padding:24px 16px 48px}
h1{font-size:26px;margin:0 0 4px}h2{font-size:20px;margin:36px 0 8px;
border-top:1px solid var(--rule);padding-top:20px}h3{font-size:17px;margin:28px 0 6px}
h4{font-size:14px;margin:18px 0 6px;color:var(--muted);text-transform:uppercase;
letter-spacing:.04em}
p,li{max-width:78ch}.note,.meta{color:var(--muted)}
.scroll{overflow-x:auto}
table{border-collapse:collapse;width:100%;font-size:14px;margin:6px 0 10px}
th,td{text-align:left;padding:6px 10px;border-bottom:1px solid var(--rule);
vertical-align:top}
th{font-weight:600;color:var(--muted);white-space:nowrap}
td.num{font-variant-numeric:tabular-nums;white-space:nowrap}
tr.ineligible td{background:var(--warn)}
tr.notes td{font-size:13px;color:var(--muted);border-bottom:1px solid var(--rule)}
tr.across td{background:var(--panel);border-bottom:2px solid var(--rule)}
code{font-size:12.5px;word-break:break-word}
.status{display:inline-block;border:1px solid var(--muted);border-radius:4px;
padding:0 6px;font-weight:600;font-size:13px;margin-right:6px}
.s-differ{border-width:2px}.flag{font-size:12px;color:var(--muted)}
ul.summary{list-style:none;padding:0}ul.summary li{margin:4px 0}
.notice{background:var(--warn);border:1px solid var(--rule);border-radius:6px;
padding:10px 14px;max-width:78ch}
figure{margin:10px 0 14px}figcaption{color:var(--muted);font-size:14px;margin-bottom:6px}
.legend,.chart-key{display:flex;flex-wrap:wrap;gap:4px 18px;font-size:13px;
margin-bottom:4px}
.key{display:inline-flex;align-items:center;gap:6px}
.swatch{width:12px;height:12px;border-radius:3px;display:inline-block}
.swatch.hollow{border:2px dashed var(--muted);width:8px;height:8px}
.swatch.line{width:2px;border-radius:0;background:var(--ink)}
svg text{fill:var(--muted);font:12px system-ui,-apple-system,"Segoe UI",sans-serif}
svg text.machine{fill:var(--ink);font-size:13px}
svg text.value{fill:var(--ink);font-variant-numeric:tabular-nums}
svg .grid{stroke:var(--grid);stroke-width:1}svg .axis{stroke:var(--muted);stroke-width:1}
svg .whisker{stroke:var(--ink);stroke-width:1.5}
svg .bar.ineligible{fill:var(--surface);stroke-width:2;stroke-dasharray:5 3}
@media print{tr.ineligible td,.notice,tr.across td{-webkit-print-color-adjust:exact;
print-color-adjust:exact}}
"""

_INTRO = """
<p>This page summarises receipts written by the solver baseline pack. A receipt
records one machine running a fixed set of solver cases several times. For each
case the page reports two things: how long the solve took on each machine, and
whether different machines returned the same numbers for the same case.</p>
<h4>How to read it</h4>
<ul>
<li><strong>Case and variant.</strong> A case is a fixed solver input. A variant is
the thread or MPI rank count it was run with.</li>
<li><strong>Solve time.</strong> Median, fastest and slowest solve time in seconds
over the successful timed repeats in the receipt. Times apply to these cases on
these machines as they were loaded at the time; they are not a general ranking of
the machines.</li>
<li><strong>Fingerprint.</strong> A few numbers taken from the solver output of a
case (for example a cell count and a force coefficient). Two fingerprints agree
when every number matches within the tolerance stated beside the comparison.
The fingerprint shown for a machine is the one from its first completed repeat.
Agreement means the machines returned the same numbers for this case. It does not
show that those numbers are physically correct, and it says nothing about other
cases.</li>
<li><strong>Baseline-eligible.</strong> Every repeat completed, the fingerprint
was the same on every repeat, and the host was not busy. Results that are not
baseline-eligible are shown and marked, and are left out of every cross-machine
comparison.</li>
<li><strong>Different solver version.</strong> When two machines run different
solver versions and their fingerprints differ, the page says so and does not
count it as a disagreement.</li>
<li><strong>Which receipt.</strong> When several receipts were supplied for the
same machine label, case and variant, only the most recent is used and the row
says how many were supplied.</li>
</ul>
"""


def render_html(model: dict) -> str:
    packs = model["packs"]
    body = ["<h1>Solver baseline: solve times and cross-machine agreement</h1>",
            f'<p class="meta">Generated {_e(model["generated_utc"])} from '
            f'{model["receipts_supplied"]} receipt(s).</p>', _INTRO]
    if len(packs) > 1:
        versions = ", ".join(_e(p["pack_version"]) for p in packs)
        body.append(
            f'<p class="notice">The receipts supplied have different pack versions '
            f'({versions}). Case definitions can change between pack versions, so each '
            f'version is reported in its own section and no result is compared with a '
            f'result from another pack version.</p>')
    for pack in packs:
        body.append(f'<h2>Pack version {_e(pack["pack_version"])}</h2>')
        body.append(_summary(pack))
        body.append("<h4>Machines</h4>" + _machine_table(pack))
        for case in pack["cases"]:
            solver = f' <span class="note">({_e(case["solver"])})</span>' if case["solver"] else ""
            body.append(f'<h3>{_e(case["case"])}{solver}</h3>')
            body.append(render_chart(case))
            body.append("<h4>Solve times and agreement</h4>" + _timing_table(case))
            body.append("<h4>Fingerprint values compared</h4>" + _fingerprint_table(case))
    return ('<!doctype html>\n<html lang="en"><head><meta charset="utf-8">'
            '<meta name="viewport" content="width=device-width, initial-scale=1">'
            "<title>Solver baseline report</title>"
            f"<style>{_CSS}</style></head><body><main>{''.join(body)}</main></body></html>\n")


def write_report(paths, out) -> dict:
    """Read receipt files, write the HTML report to ``out`` and return the model."""
    model = build_report(load_receipts(paths))
    out = Path(out)
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(render_html(model), encoding="utf-8")
    return model
