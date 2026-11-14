"""Deterministic, self-contained analytical rating report with provenance."""

import html

import plotly.graph_objects as go
import plotly.io as pio

from digitalmodel.common.assumption_ledger import AssumptionLedger, AssumptionSource, Confidence
from digitalmodel.reporting import Provenance, ReportSection, SectionMode, assemble_report


def render_report(result, *, inputs, data_source, output_path=None):
    """Assemble views over the calculated result; missing provenance fails closed."""
    sections = [
        ReportSection("summary", "Summary", SectionMode.ALWAYS, _summary),
        ReportSection("input-echo", "Input echo", SectionMode.ALWAYS, _inputs),
        ReportSection("bolt-loads", "Bolt loads", SectionMode.ALWAYS, _loads),
        ReportSection("stress-checks", "Stress checks", SectionMode.ALWAYS, _checks),
        ReportSection("utilization", "Utilization", SectionMode.ALWAYS, _chart),
        ReportSection("citations", "Citations", SectionMode.ALWAYS, _citations),
    ]
    provenance = Provenance(sources=[] if data_source is None else [data_source])
    return assemble_report(
        "Analytical flange rating (ASME App 2)", provenance=provenance,
        sections=sections, data={"result": result, "inputs": inputs},
        ledger=_ledger(inputs), output_path=output_path,
        css="body{font-family:Arial,sans-serif;margin:2rem;color:#17202a}"
            "table{border-collapse:collapse;width:100%;margin:1rem 0}"
            "th,td{border:1px solid #bbc;padding:.4rem;text-align:left}"
            ".pass{color:#176638}.fail{color:#a11}.section{margin-bottom:2rem}",
    )


def _section(key, title, body):
    return f'<section class="section" id="{key}"><h2>{title}</h2>{body}</section>'


def _table(headers, rows):
    def cell(value):
        return html.escape(str(value))
    return ("<table><thead><tr>" + "".join(f"<th>{cell(h)}</th>" for h in headers)
            + "</tr></thead><tbody>" + "".join("<tr>" + "".join(
                f"<td>{cell(c)}</td>" for c in row) + "</tr>" for row in rows)
            + "</tbody></table>")


def _summary(data, **_):
    result = data["result"]
    status = result["screening_status"]
    return _section("summary", "Summary",
        f'<p class="{status}">Screening status: <strong>{status.upper()}</strong></p>'
        f'<p>Governing check: {html.escape(result["governing_check"])}; '
        f'utilization {result["governing_utilization"]:.6f} against a limit of 1.0.</p>'
        "<p>Integral non-cast flange strength and rigidity screening; geometry is net of corrosion. "
        "The hub limit includes caller-supplied neck allowables and the sketch multiplier. "
        "Material selection, independent neck and weld qualification, external loads, external pressure, "
        "fatigue and leakage assessment are outside this calculation. "
        "Engineer sign-off on the golden values is required before use.</p>")


def _flatten(values, prefix=""):
    for key, value in values.items():
        name = f"{prefix}.{key}" if prefix else key
        if isinstance(value, dict):
            yield from _flatten(value, name)
        else:
            yield name, "Not reported" if value is None else value


def _inputs(data, **_):
    return _section("input-echo", "Input echo",
        "<p>Lengths: in; areas: in²; loads: lbf; moments: lbf·in; stresses and moduli: psi. "
        "Temperature, when reported, is °F. Allowables are supplied directly.</p>"
        + _table(["Quantity", "Supplied value"], _flatten(data["inputs"])))


def _loads(data, **_):
    result = data["result"]
    rows = [(key, value, "in²" if key in {"Am", "Ab"} else "lbf")
            for key, value in result["bolt_loads"].items()]
    rows.extend((key, value, "lbf·in") for key, value in result["moments"].items())
    return _section("bolt-loads", "Bolt loads and areas", _table(["Quantity", "Value", "Units"], rows))


def _checks(data, **_):
    rows = [(c["name"], f'{c["demand"]:.6g}', f'{c["allowable"]:.6g}', c["units"],
             f'{c["utilization"]:.6f}', "pass" if c["passes"] else "fail", c["clause"])
            for c in data["result"]["checks"]]
    return _section("stress-checks", "Stress checks", _table(
        ["Check", "Demand", "Allowable", "Units", "Utilization", "Status", "Clause"], rows))


def _chart(data, **_):
    checks = data["result"]["checks"]
    figure = go.Figure(go.Bar(x=[c["name"] for c in checks],
                             y=[c["utilization"] for c in checks]))
    figure.add_hline(y=1.0, line_color="red")
    figure.update_layout(title="Utilization against the criterion of 1.0", template="plotly_white")
    chart = pio.to_html(figure, include_plotlyjs=True, full_html=False,
                        div_id="flange-utilizations", config={"displaylogo": False})
    return _section("utilization", "Utilization", chart)


def _citations(data, **_):
    rows = [(c["publisher"], c["code_id"], c["revision"], c["section"], c["wiki_path"])
            for c in data["result"]["citations"]]
    return _section("citations", "Citations", _table(
        ["Publisher", "Code", "Edition", "Clause", "Resolved reference"], rows))


def _ledger(inputs):
    ledger = AssumptionLedger()
    bases = {
        "gasket.width_rule": "Table 2-5.2: b0 supplied for the selected facing; US customary width rule and reaction diameter applied",
        "bolts.root_area": inputs["bolts"].get("root_area_source", "Caller-supplied root area; thread table is not embedded"),
        "material.allowables": "Caller-supplied allowables and elastic moduli; no temperature lookup or material qualification",
        "hub.material_bound": ("Hub allowable is the smaller flange/neck bound; "
                               f"caller-selected hub_stress_multiplier = {inputs['design']['hub_stress_multiplier']} "
                               "must match the applicable Figure 2-4 sketch; "
                               "neck allowables are supplied at both conditions"),
        "stress.magnitudes": "Combined averages use absolute stress magnitudes; both conditions are checked",
        "rigidity.factor": "App 2-14 integral rigidity factor KI = 0.3",
        "bolt_spacing": "App 2-6: correction applied only when explicitly requested; lethal-service qualification is outside this screen",
    }
    for field, basis in bases.items():
        ledger.record(field, basis, AssumptionSource.ASSUMED_DEFAULT, basis,
                      Confidence.HIGH, reference="ASME VIII-1 (2019), Appendix 2")
    return ledger
