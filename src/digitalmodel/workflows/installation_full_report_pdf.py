"""Full engineering report with case appendix and embedded monitoring snapshot."""

from collections import Counter
from copy import deepcopy
from io import BytesIO
import hashlib
import json
import re
from pathlib import Path
from xml.sax.saxutils import escape

from pypdf import PdfReader, PdfWriter
from reportlab.lib import colors
from reportlab.lib.pagesizes import A4
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.pdfgen.canvas import Canvas
from reportlab.platypus import (SimpleDocTemplate, Paragraph, Spacer, PageBreak,
                               LongTable, TableStyle, KeepTogether)

from digitalmodel.workflows.installation_envelope_pdf import render_pdf

PAGE_WIDTH, PAGE_HEIGHT = A4
STYLES = getSampleStyleSheet()
STYLES.add(ParagraphStyle("Engineering", fontName="Helvetica", fontSize=9,
                         leading=13, spaceAfter=8))
STYLES.add(ParagraphStyle("Cell", fontName="Helvetica", fontSize=7, leading=9))
STYLES["Heading1"].textColor = colors.HexColor("#17384d")


def _p(text, style="Engineering"):
    return Paragraph(escape(str(text)), STYLES[style])


def _section(story, title, text):
    story.append(KeepTogether([_p(title, "Heading1"), _p(text)]))


def _table(story, headers, rows, widths, caption):
    data = [[_p(v, "Cell") for v in row] for row in [headers] + rows]
    table = LongTable(data, colWidths=widths, repeatRows=1, hAlign="LEFT")
    table.setStyle(TableStyle([
        ("BACKGROUND", (0, 0), (-1, 0), colors.HexColor("#dfedf6")),
        ("VALIGN", (0, 0), (-1, -1), "TOP"),
        ("GRID", (0, 0), (-1, -1), .3, colors.HexColor("#b9c8d2")),
        ("LEFTPADDING", (0, 0), (-1, -1), 5),
        ("RIGHTPADDING", (0, 0), (-1, -1), 5),
        ("TOPPADDING", (0, 0), (-1, -1), 5),
        ("BOTTOMPADDING", (0, 0), (-1, -1), 5),
    ]))
    story.extend([table, Spacer(1, 5), _p(caption), Spacer(1, 10)])


def _mapped_cases(summary, payload):
    source = {c["index"]: c for c in summary["cases"]}
    screened = {c["index"]: c for c in payload["cases"]}
    if len(source) != len(summary["cases"]) or len(screened) != len(payload["cases"]):
        raise ValueError("Duplicate case identifiers")
    if not source or set(source) != set(screened):
        raise ValueError("Source and screening case coverage differ")
    for index, case in screened.items():
        if any(case[key] != source[index][key] for key in ["hs_m", "tp_s"]):
            raise ValueError("Source and screening case coordinates differ")
        if case["status"] != "NOT_EVALUATED" and source[index].get("status") != "VERIFIED":
            raise ValueError("Screened classification requires verified source case")
    return [(source[i], screened[i]) for i in sorted(source)]


def _cover(story, summary, payload):
    config = payload.get("_report_config", {})
    story.extend([Spacer(1, 80), _p(config.get("report_title", "Jumper installation analysis"), "Title"),
                  _p("Vessel capability screening and near-real-time support demonstration", "Heading2"),
                  Spacer(1, 30)])
    rows = [["Revision", config.get("revision", "r7") + " - engineering review draft"],
            ["Issue purpose", config.get("issue_purpose", "Technical review")],
            ["Analysis maturity", "Screening"],
            ["Engineering acceptance", "NOT EVALUATED"],
            ["Criteria basis", "Assumed project criteria; acceptance authority unverified"],
            ["Prepared by", config.get("prepared_by", "Not assigned")],
            ["Reviewed by", config.get("reviewed_by", "Not assigned")],
            ["Approval record", config.get("approved_by", "Not assigned")],
            ["Source snapshot", payload["created_utc"]],
            ["Source cases", str(len(summary["cases"]))]]
    _table(story, ["Document control", "Record"], rows, [145, 362],
           "Table 1. Document control. This draft is not an issued installation authorization.")
    story.extend([_p("The report integrates the retained irregular-wave demand study, assumed-criteria screening, and a simulated wave-preview demonstration. Original source evidence and reusable analysis results remain in their owning private repositories."), PageBreak()])


def _intro_summary(story, summary, payload, cases):
    _section(story, "1. Introduction", "The analysis assesses the response of the selected " + payload.get("_report_config", {}).get("structure_description", "suspended jumper and lifting system") + " over the sampled sea states. The report supports review of vessel installation capability and the proposed near-real-time support workflow.")
    story.append(_p("The current scope is a fixed deep-zone arrangement. Splash-zone passage, continuous lowering, landing, additional vessel headings and additional random seeds require separate assessment. The report does not establish operating release."))
    counts = Counter(c["status"] for _, c in cases)
    _section(story, "2. Summary and conclusions", f"The report contains {len(cases)} source cases. Assumed-criteria classifications are: " + "; ".join(f"{key}: {value}" for key, value in sorted(counts.items())) + ". Engineering acceptance: NOT EVALUATED.")
    governing = max(_evaluated_cases(cases),
                    key=lambda c: c["max_utilization"], default=None)
    if governing:
        story.append(_p(f"The largest recorded screening utilization is {governing['max_utilization']:.3f}, against a utilization criterion of 1.000, for {governing.get('governing_check', 'unrecorded criterion')}, at Hs {governing['hs_m']:g} m and Tp {governing['tp_s']:g} s (case {governing['index']}). This conclusion applies only to the checks included in the assumed-criteria screen."))
    story.append(_p("Passing cells at the upper study edge are censored by the sampled range; they do not locate a physical failure boundary. Intentional sling slack is not rejected solely by a zero crossing. Snap loads, geometric slack and interference remain separate acceptance checks."))
    story.append(_p(_monitoring_statement(payload)))


def _evaluated_cases(cases):
    for _, case in cases:
        if case["status"] == "NOT_EVALUATED":
            continue
        if case.get("checks"):
            checks = [x for x in case["checks"] if x.get("status") in {"PASS", "FAIL"}
                      and x.get("utilization") is not None]
            if checks:
                check = max(checks, key=lambda x: x["utilization"])
                yield dict(case, max_utilization=check["utilization"], governing_check=check["id"])
        elif case.get("max_utilization") is not None:
            yield case


def _design(story, summary, payload, cases):
    _section(story, "3. Design data and assumed criteria", "The retained model is the arrangement authority. The values below are extracted from the source case settings; reproduction does not qualify the underlying design inputs.")
    headings = sorted({str(c.get("heading_degrees", "Not established")) for c, _ in cases})
    rows = [["Heading", ", ".join(headings), "deg", "Source cases"]]
    rows.insert(0, ["Solver version", payload.get("_report_config", {}).get("solver_version", "Not established"), "Version", "Retained report configuration"])
    for key, unit in [("buildup_s", "s"), ("duration_s", "s"), ("sample_interval_s", "s"),
                      ("gamma", "dimensionless"), ("max_time_step_s", "s"),
                      ("components", "count"), ("seed", "identifier")]:
        observed = {c.get(key, c["settings"].get(key, "Not established")) for c, _ in cases}
        rows.append([key, ", ".join(map(str, sorted(observed, key=str))), unit, "Source settings"])
    _table(story, ["Input", "Value", "Units", "Status / source"], rows,
           [133, 145, 80, 149], "Table 2. Analysis configuration retained with the case summaries.")
    _design_extra(story, payload.get("design_data", []))
    story.append(_p("Geometry, material certificates, rigging capacity register, clamp/connector capacities and drawing revision reconciliation remain design-basis qualification items. No missing input is represented as a zero capacity."))
    rows = [[c.get("label", c.get("id", "Criterion")), str(c.get("limit", "Not established")),
             c.get("units", "Not established"), "Assumed project criterion"] for c in payload["criteria"]]
    _table(story, ["Criterion", "Value", "Units", "Status"], rows, [210, 65, 62, 170],
           "Table 3. Screening assumptions. These values do not constitute verified equipment certification.")
    for disclosure in payload.get("_report_config", {}).get("disclosures", []):
        story.append(_p(disclosure))


def _design_extra(story, items):
    rows = []
    for item in items:
        if not isinstance(item, dict):
            story.append(_p(item))
            continue
        rows.append([item.get("parameter", "Input"), str(item.get("value", "Not established")),
                     item.get("unit", item.get("units", "Not established")),
                     f"{item.get('source', 'Source not established')}. {item.get('status', 'Status not established')}",
                     item.get("effect_if_changed", "Not established")])
    if rows:
        _table(story, ["Design input", "Value", "Units", "Source and status", "Effect if changed"],
               rows, [95, 86, 43, 170, 113],
               "Table 2a. Arrangement design inputs. Model readback is not material or drawing certification.")


def _history_only(payload):
    return payload.get("demo", {}).get("default_mode") == "history_only"


def _monitoring_statement(payload):
    if _history_only(payload):
        return ("The monitoring illustration predicts the next two minutes using only samples available at NOW in a SIMULATED record. "
                "Held-out prediction errors do not validate offshore wave prediction or field operating decisions.")
    return ("The monitoring illustration supplies a simulated future irregular-wave record to a load surrogate fitted using past data. "
            "Conditional prediction performance does not validate offshore wave prediction or field operating decisions.")


def _method_statement(payload):
    if _history_only(payload):
        return ("The monitoring illustration separates recorded history from a two-minute causal forecast at the displayed NOW line. "
                "The history-only predictor uses only samples available at NOW; no future wave or load sample is supplied. "
                "Withheld future response is used only for error assessment, with persistence and history-mean comparators.")
    return ("The monitoring illustration separates recorded history from a two-minute conditional forecast at the displayed NOW line. "
            "The simulated future wave trace is supplied input. The load surrogate is fitted to past data. "
            "Withheld future load response is used for error assessment, with persistence and history-mean comparators.")


def _method(story, payload):
    _section(story, "4. Analysis methodology", "The irregular-wave sea-state matrix is simulated in OrcaFlex. Recorded response channels and retained simulation digests provide the analysis evidence. The same model basis is used across the sea-state grid, with case-specific environmental changes.")
    for text in [
        "Effective-tension histories are examined at the recorded line endpoints. Maximum loads are compared with the corresponding assumed component limits. The hoist minimum/static ratio is compared with the assumed minimum ratio. These checks are not a complete line-interior or crane side-lead assessment.",
        "Negative effective tension and nonpositive-tension duration are retained as signed model-response diagnostics. They are not interpreted as physical sling compression capacity or measured geometric slack. A detailed slack assessment requires rigging stiffness, resolved re-tension peaks and interference checks.",
        "The detailed slack-sling method is discussed against the historical DNVGL-ST-N001 June 2016 clauses 16.17.2.6 to 16.17.2.8. Hoist and individual-sling requirements differ. Governing project edition and applicability remain unverified.",
        _method_statement(payload),
        "Numerical completion and digest verification establish retained computational evidence. Mesh/time-step convergence, alternative seeds, forecast validation and all operating acceptance checks remain separately qualified.",
    ]:
        story.append(_p(text))


def _results(story, summary, payload, cases):
    _section(story, "5. Results - conditional screening", "The tables report the governing recorded results and assumed-criteria comparisons. The integrated Hs-Tp envelope and random-wave monitoring charts appear directly in Appendix B. Detailed case classifications are retained in Appendix A.")
    groups = {}
    for _, case in cases:
        if case["status"] == "NOT_EVALUATED":
            continue
        for check in case.get("checks", []):
            if check.get("status") in {"PASS", "FAIL"} and check.get("utilization") is not None:
                previous = groups.get(check["id"])
                if previous is None or check["utilization"] > previous[1]["utilization"]:
                    groups[check["id"]] = (case, check)
    rows = [[key, f"{check['utilization']:.3f}", str(case["index"]),
             f"{case['hs_m']:g} / {case['tp_s']:g}", check.get("governing_channel", "Not recorded")]
            for key, (case, check) in sorted(groups.items())]
    _table(story, ["Assumed check", "Utilization (-)", "Case", "Hs (m) / Tp (s)", "Governing channel"],
           rows, [148, 65, 42, 78, 174], "Table 4. Governing utilization by assumed criterion; unity is the screening threshold.")
    rows = []
    for env in summary.get("envelopes", []):
        if env.get("units") != "kN":
            continue
        peak, low = env["maximum"], env["minimum"]
        rows.append([env["channel"], f"{low['value']:.3f}", f"{peak['value']:.3f}",
                     str(peak["index"]), f"{peak['hs_m']:g} / {peak['tp_s']:g}"])
    _table(story, ["Endpoint channel", "Minimum (kN)", "Maximum (kN)", "Peak case", "Peak Hs (m) / Tp (s)"],
           rows, [192, 75, 75, 55, 110], "Table 5. Recorded endpoint force extrema. Minima and maxima may occur in different cases.")
    for limitation in payload.get("limitations", []):
        story.append(_p(limitation))


def _validation_references(story, summary, payload):
    _section(story, "6. Validation status", "The retained case summaries and simulation digests support computational traceability. Engineering acceptance remains NOT EVALUATED. No issued operating approval, field forecast qualification or validated sling compression allowance is asserted.")
    _table(story, ["Assessment", "Current disposition"], [
        ["Case completion and retained evidence", "Source case status is reported independently from engineering acceptance."],
        ["Equipment capacity and drawing reconciliation", "Pending qualification."],
        ["Slack, snap and interference", "Pending stiffness/model-form, geometric and convergence assessment."],
        ["RAO range and warnings", "Warnings require disposition; completion alone does not establish adequacy."],
        ["Forecast field validation", "Not established; conditional simulated preview only."],
    ], [200, 307], "Table 6. Validation and qualification register.")
    _section(story, "7. Recommendations", "The capacity register and governing edition should be confirmed. Selected governing cases should then receive rigging-stiffness, limited-compression, time-step and mesh sensitivities. Geometric slack, interference, crane off/side lead, clamp/connector forces and pipe-code checks should be completed before an operating envelope is issued.")
    story.append(_p("Additional random seeds and operation phases should be assessed near any emerging boundary. Offshore wave-preview and load-prediction performance should be measured against independent observations before near-real-time guidance is used operationally."))
    _section(story, "8. References and revision history", "The private retained source workbook, model manifest, simulation records and code revision form the evidence chain. Licensed standards remain at their licensed source locations.")
    for key in ["campaign_sha256", "matrix_sha256"]:
        story.append(_p(f"{key}: {summary.get(key, 'Not recorded')}", "Cell"))
    story.append(_p(f"Master SHA-256: {summary.get('campaign_snapshot', {}).get('master_sha256', 'Not recorded')}", "Cell"))
    for key, value in payload.get("provenance", {}).items():
        story.append(_p(f"{key}: {value}", "Cell"))
    _config_references(story, payload.get("_report_config", {}))


def _config_references(story, config):
    source = config.get("design_source", {})
    story.append(_p(f"Design source: {source.get('path', 'Not established')}; SHA-256: {source.get('sha256', 'Not established')}", "Cell"))
    for ref in config.get("references", []):
        story.append(_p(f"{ref.get('label', 'Reference')}: {ref.get('path', 'Not established')}", "Cell"))
    story.append(Spacer(1, 10))
    for rev in config.get("revision_history", []):
        story.append(_p(f"{rev.get('revision', '')}, {rev.get('date', '')}: {rev.get('description', '')}. Status: {rev.get('status', 'Not recorded')}"))
    story.append(_p("Document-control entries do not establish signed operating approval. Engineering acceptance remains NOT EVALUATED."))


def _other_results(story, summary):
    story.append(_p("5.1 Stress, geometry and response diagnostics", "Heading2"))
    rows = []
    for env in summary.get("envelopes", []):
        if env.get("units") == "kN":
            continue
        low, peak = env["minimum"], env["maximum"]
        rows.append([env["channel"], env["units"], f"{low['value']:.3f}",
                     str(low["index"]), f"{peak['value']:.3f}", str(peak["index"])])
    _table(story, ["Channel", "Units", "Minimum", "Min case", "Maximum", "Max case"],
           rows, [202, 43, 77, 45, 95, 45],
           "Table 5a. Recorded non-force extrema. Units are retained from each source channel; material, stress and clearance acceptance remain unqualified.")
    story.append(_p("Unstretched length minus endpoint span is a geometric diagnostic, not a universal allowable slack value. Span rate and signed line response support investigation of re-tension events; they do not replace resolved snap-load and interference assessment."))


def _appendix(story, cases):
    story.append(PageBreak())
    _section(story, "Appendix A. Detailed case results", "Every source case appears below. W = within assumptions, E = exceeds assumptions, N = not evaluated. Classification does not constitute engineering acceptance. Peak tension is the maximum across the recorded source tension channels.")
    labels = {"WITHIN_ASSUMPTIONS": "W", "EXCEEDS_ASSUMPTIONS": "E", "NOT_EVALUATED": "N"}
    rows = []
    for source, case in cases:
        util = case.get("max_utilization")
        rows.append([f"CASE-{case['index']:03d}", f"{case['hs_m']:g}", f"{case['tp_s']:g}",
                     f"{source['peak_tension_kN']:.3f}" if source.get("peak_tension_kN") is not None else "Not evaluated",
                     f"{util:.3f}" if util is not None else "Not evaluated",
                     case.get("governing_check", "Not recorded"), labels[case["status"]]])
    _table(story, ["Case", "Hs (m)", "Tp (s)", "Peak (kN)", "Max util. (-)", "Governing assumed criterion", "Screen"],
           rows, [63, 43, 43, 65, 65, 180, 48], "Table A1. Complete case register, linked by stable case index to the retained simulation evidence.")
    story.extend([Spacer(1, 15), _p("Appendix B. Integrated envelope and monitoring snapshot", "Heading1"),
                  _p("The following three pages contain the same payload-derived Hs-Tp envelope, simulated irregular-wave preview and conditional load response as the interactive report. Global report pagination applies.")])


def _number_pages(body, snapshot, output, revision, title="Jumper installation analysis"):
    writer = PdfWriter()
    for stream in [body, snapshot]:
        writer.append(PdfReader(stream))
    total = len(writer.pages)
    for number, page in enumerate(writer.pages, 1):
        overlay = BytesIO()
        canvas = Canvas(overlay, pagesize=A4)
        canvas.setFillColor(colors.white)
        canvas.rect(0, 0, PAGE_WIDTH, 39, fill=1, stroke=0)
        canvas.setFillColor(colors.HexColor("#17384d"))
        canvas.setFont("Helvetica", 8)
        canvas.drawString(44, 23, f"{title} | {revision} | Engineering review draft")
        canvas.drawRightString(PAGE_WIDTH - 44, 23, f"Page {number} of {total}")
        canvas.save()
        page.merge_page(PdfReader(overlay).pages[0])
    result = BytesIO()
    writer.write(result)
    output.write(result.getvalue()) if hasattr(output, "write") else Path(output).write_bytes(result.getvalue())
    return total


def _verify_summary_source(summary, payload, raw):
    expected = payload.get("provenance", {}).get("summary", {}).get("sha256")
    if not isinstance(expected, str) or not re.fullmatch(r"[0-9a-f]{64}", expected):
        raise ValueError("Valid source summary SHA-256 digest is required")
    if raw is None:
        raise ValueError("Claimed source digest requires summary_bytes")
    if raw is not None:
        if expected and hashlib.sha256(raw).hexdigest() != expected:
            raise ValueError("Source summary digest mismatch")
        if json.loads(raw) != summary:
            raise ValueError("Source bytes differ from supplied summary")


def render_full_pdf(summary, payload, output, config=None, *, summary_bytes=None):
    """Render the full report without modifying source payloads or simulations."""
    _verify_summary_source(summary, payload, summary_bytes)
    payload = deepcopy(payload)
    payload["_report_config"] = config or {}
    if config:
        payload["design_data"] = config.get("design_data", payload.get("design_data", []))
    cases = _mapped_cases(summary, payload)
    story = []
    _cover(story, summary, payload)
    _intro_summary(story, summary, payload, cases)
    _design(story, summary, payload, cases)
    _method(story, payload)
    _results(story, summary, payload, cases)
    _other_results(story, summary)
    _validation_references(story, summary, payload)
    _appendix(story, cases)
    body, snapshot = BytesIO(), BytesIO()
    document = SimpleDocTemplate(body, pagesize=A4, leftMargin=44, rightMargin=44,
                                 topMargin=44, bottomMargin=49,
                                 title=(config or {}).get("report_title", "Jumper installation engineering report"))
    document.build(story)
    reference = payload.get("snapshot") or {}
    identity = render_pdf(payload, snapshot, **{key: reference[key] for key in ("hs_m", "tp_s", "now_s") if key in reference})
    pages = _number_pages(body, snapshot, output, (config or {}).get("revision", "r7"),
                          (config or {}).get("report_title", "Jumper installation analysis"))
    return {"pages": pages, "cases": len(cases), "snapshot": identity}
