"""Cost Data Sheet renderer for floating-wind LCOE/TOTEX (issue #1226).

The closing piece of the floating-wind economics epic (#1220): a provenance-aware
**one-page LCOE / TOTEX data sheet**, built on the *shared reporting backbone*
(:mod:`digitalmodel.reporting`, issue #1018) -- the same backbone the concept
data sheet (#1027) uses -- rather than a parallel renderer.

For a :class:`~digitalmodel.floating_wind.economics.ProjectEconomics` and its
:class:`~digitalmodel.floating_wind.economics.LCOEResult` the sheet presents, in
order: the headline LCOE, the project & financial inputs, the CAPEX component
stack, the discounted TOTEX breakdown with the per-MWh split, and -- when a
cost-driver sweep (#1223) is supplied -- a sensitivity table.

References
----------
* digitalmodel reporting block library (#1018) -- shared report backbone.
* Wood plc / A. Whooley (2021); NREL 2019; ORE Catapult -- cost bases.
"""

from __future__ import annotations

import html
from pathlib import Path
from typing import Any

from digitalmodel.floating_wind.economics import LCOEResult, ProjectEconomics
from digitalmodel.floating_wind.sweep import SweepRow
from digitalmodel.reporting import (
    ReportBackbone,
    ReportDataModel,
    ReportRenderer,
    ReportSection,
    SectionMode,
)

__all__ = [
    "LCOEDataSheetData",
    "lcoe_data_sheet",
    "render_lcoe_data_sheet_html",
    "write_lcoe_data_sheet",
    "LCOE_DATA_SHEET_BACKBONE",
]


class LCOEDataSheetData(ReportDataModel):
    """Envelope for the LCOE/TOTEX data sheet."""

    econ: ProjectEconomics
    result: LCOEResult
    sweep_rows: list[SweepRow] = []
    title: str = "Floating-Wind LCOE / TOTEX Data Sheet"
    references: tuple[str, ...] = (
        "Wood plc / A. Whooley (2021), Translating O&G Cost Reduction into Floating Wind",
        "NREL, 2019 Cost of Wind Energy Review (NREL/TP-5000-78471)",
        "ORE Catapult, Floating Offshore Wind: Cost Reduction Pathways",
    )


def _num(x: float, fmt: str = ",.2f") -> str:
    return format(x, fmt)


def _section(title: str, body: str, *, anchor: str) -> str:
    return f'<div class="section" id="{anchor}"><h2>{html.escape(title)}</h2>{body}</div>'


def _sec_header(data: LCOEDataSheetData, **_: Any) -> str:
    e, r = data.econ, data.result
    return (
        '<div class="report-header">'
        f"<h1>{html.escape(data.title)}</h1>"
        f'<div class="subtitle">{_num(e.turbine_rating_mw, ".0f")} MW '
        f"× {e.turbine_count} = {_num(e.farm_capacity_mw, ',.0f')} MW "
        f"· {html.escape(e.fabrication_region)} · "
        f"{e.financial.design_life_years}-yr life</div>"
        f'<div class="meta">LCOE '
        f"<strong>${_num(r.lcoe_usd_per_mwh, ',.1f')}/MWh</strong></div>"
        "</div>"
    )


def _sec_inputs(data: LCOEDataSheetData, **_: Any) -> str:
    e = data.econ
    f = e.financial
    rows = [
        ("Turbine rating", _num(e.turbine_rating_mw, ".1f"), "MW"),
        ("Turbine count", str(e.turbine_count), ""),
        ("Farm capacity", _num(e.farm_capacity_mw, ",.0f"), "MW"),
        ("Capacity factor", _num(e.capacity_factor * 100, ".1f"), "%"),
        ("Water depth", _num(e.water_depth_m, ",.0f"), "m"),
        ("Distance to shore", _num(e.distance_to_shore_km, ",.0f"), "km"),
        ("Discount rate", _num(f.discount_rate * 100, ".1f"), "%"),
        ("Inflation (OPEX esc.)", _num(f.inflation_rate * 100, ".1f"), "%"),
        ("Design life", str(f.design_life_years), "yr"),
    ]
    body = _kv_table(rows, cols=("Input", "Value", "Unit"))
    return _section("Project & Financial Inputs", body, anchor="inputs")


def _sec_capex(data: LCOEDataSheetData, **_: Any) -> str:
    c = data.econ.capex
    kw = data.econ.farm_capacity_kw
    comps = [
        ("Turbine", c.turbine),
        ("Substructure", c.substructure),
        ("Mooring", c.mooring),
        ("Array cable", c.array_cable),
        ("Export cable", c.export_cable),
        ("Installation", c.installation),
        ("Development", c.development),
    ]
    rows = "".join(
        f"<tr><td>{html.escape(name)}</td><td>{_num(v, ',.0f')}</td>"
        f"<td>{_num(v * kw / 1e9, ',.3f')}</td></tr>"
        for name, v in comps
    )
    total = c.total_per_kw
    rows += (
        f'<tr class="total"><td><strong>Total overnight CAPEX</strong></td>'
        f"<td><strong>{_num(total, ',.0f')}</strong></td>"
        f"<td><strong>{_num(total * kw / 1e9, ',.3f')}</strong></td></tr>"
    )
    body = (
        "<table><thead><tr><th>Component</th><th>$/kW</th>"
        f"<th>$ billion</th></tr></thead><tbody>{rows}</tbody></table>"
    )
    return _section("CAPEX Breakdown", body, anchor="capex")


def _sec_totex(data: LCOEDataSheetData, **_: Any) -> str:
    r = data.result
    rows = [
        ("CAPEX (overnight)", r.capex_usd, r.capex_per_mwh),
        ("OPEX (discounted)", r.opex_discounted_usd, r.opex_per_mwh),
        ("DECOMEX (discounted)", r.decomex_discounted_usd, r.decomex_per_mwh),
    ]
    body_rows = "".join(
        f"<tr><td>{html.escape(name)}</td>"
        f"<td>{_num(usd / 1e9, ',.3f')}</td>"
        f"<td>{_num(permwh, ',.1f')}</td></tr>"
        for name, usd, permwh in rows
    )
    body_rows += (
        f'<tr class="total"><td><strong>TOTEX / LCOE</strong></td>'
        f"<td><strong>{_num(r.totex_discounted_usd / 1e9, ',.3f')}</strong></td>"
        f"<td><strong>{_num(r.lcoe_usd_per_mwh, ',.1f')}</strong></td></tr>"
    )
    note = (
        f'<p class="note">Discounted energy over life: '
        f"{_num(r.energy_discounted_mwh / 1e6, ',.2f')} M MWh "
        f"(AEP {_num(r.aep_mwh / 1e6, ',.3f')} M MWh/yr).</p>"
    )
    body = (
        "<table><thead><tr><th>Component</th><th>$ billion (disc.)</th>"
        f"<th>$/MWh</th></tr></thead><tbody>{body_rows}</tbody></table>{note}"
    )
    return _section("Discounted TOTEX → LCOE", body, anchor="totex")


def _sec_sensitivities(data: LCOEDataSheetData, **_: Any) -> str:
    if not data.sweep_rows:
        return ""
    rows = "".join(
        f"<tr><td>{html.escape(row.name)}</td>"
        f"<td>{_num(row.lcoe_usd_per_mwh, ',.1f')}</td>"
        f"<td>{_num(row.delta_vs_base, '+,.1f')}</td>"
        f"<td>{_num(row.pct_vs_base, '+.1f')}</td></tr>"
        for row in data.sweep_rows
    )
    body = (
        "<table><thead><tr><th>Scenario</th><th>LCOE $/MWh</th>"
        "<th>Δ $/MWh</th><th>Δ %</th></tr></thead>"
        f"<tbody>{rows}</tbody></table>"
    )
    return _section("Cost-Driver Sensitivities", body, anchor="sensitivities")


def _sec_provenance(data: LCOEDataSheetData, **_: Any) -> str:
    items = "".join(f"<li>{html.escape(ref)}</li>" for ref in data.references)
    body = (
        "<p>Cost bases from public references only; no client or proprietary "
        f"data.</p><h3>References</h3><ul>{items}</ul>"
    )
    return _section("Provenance", body, anchor="provenance")


def _kv_table(rows: list[tuple[str, str, str]], *, cols: tuple[str, str, str]) -> str:
    head = "".join(f"<th>{html.escape(c)}</th>" for c in cols)
    body = "".join(
        f"<tr><td>{html.escape(a)}</td><td>{html.escape(b)}</td>"
        f"<td>{html.escape(c)}</td></tr>"
        for a, b, c in rows
    )
    return f"<table><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table>"


LCOE_DATA_SHEET_BACKBONE = ReportBackbone(
    title="Floating-Wind LCOE / TOTEX Data Sheet",
    sections=[
        ReportSection("header", "Headline", SectionMode.ALWAYS, _sec_header),
        ReportSection("inputs", "Inputs", SectionMode.ALWAYS, _sec_inputs),
        ReportSection("capex", "CAPEX", SectionMode.ALWAYS, _sec_capex),
        ReportSection("totex", "TOTEX", SectionMode.ALWAYS, _sec_totex),
        ReportSection(
            "sensitivities",
            "Sensitivities",
            SectionMode.COMPACT_SKIP,
            _sec_sensitivities,
        ),
        ReportSection("provenance", "Provenance", SectionMode.ALWAYS, _sec_provenance),
    ],
)


_LCOE_CSS = """\
body{font-family:-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;color:#222;margin:0}
.container{max-width:900px;margin:0 auto;padding:24px}
.report-header h1{margin:0 0 4px;font-size:1.6em}
.report-header .subtitle{color:#555}
.report-header .meta{margin-top:8px;font-size:1.2em}
.section{margin:22px 0}
.section h2{border-bottom:2px solid #1d76db;padding-bottom:4px;font-size:1.15em}
table{border-collapse:collapse;width:100%;margin-top:8px}
th,td{border:1px solid #ddd;padding:6px 10px;text-align:left}
th{background:#f4f7fb}
tr.total td{background:#eef5ff}
td:nth-child(n+2){text-align:right}
.note{color:#555;font-size:0.9em}
"""


def lcoe_data_sheet(
    econ: ProjectEconomics,
    result: LCOEResult,
    *,
    sweep_rows: list[SweepRow] | None = None,
    title: str | None = None,
    mode: str = "full",
) -> list[str]:
    """Render the LCOE/TOTEX data sheet to the backbone's section HTML list."""
    data = LCOEDataSheetData(
        econ=econ,
        result=result,
        sweep_rows=list(sweep_rows or []),
        title=title or "Floating-Wind LCOE / TOTEX Data Sheet",
    )
    return LCOE_DATA_SHEET_BACKBONE.render(data, mode=mode)


def render_lcoe_data_sheet_html(
    econ: ProjectEconomics,
    result: LCOEResult,
    *,
    sweep_rows: list[SweepRow] | None = None,
    title: str | None = None,
    mode: str = "full",
) -> str:
    """Render a complete standalone HTML LCOE/TOTEX data sheet document."""
    sections = lcoe_data_sheet(
        econ, result, sweep_rows=sweep_rows, title=title, mode=mode
    )
    return ReportRenderer().render_html(
        sections,
        title=title or "Floating-Wind LCOE / TOTEX Data Sheet",
        css=_LCOE_CSS,
        plotlyjs_src="",
        container_class="container",
    )


def write_lcoe_data_sheet(
    econ: ProjectEconomics,
    result: LCOEResult,
    output_path: str | Path,
    *,
    sweep_rows: list[SweepRow] | None = None,
    title: str | None = None,
    mode: str = "full",
) -> Path:
    """Render and write the LCOE/TOTEX data sheet HTML file; return its path."""
    doc = render_lcoe_data_sheet_html(
        econ, result, sweep_rows=sweep_rows, title=title, mode=mode
    )
    return ReportRenderer().write_html(doc, Path(output_path))
