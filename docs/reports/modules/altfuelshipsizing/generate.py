# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the self-contained sizing report from the documented example and API.

Use PYTHONPATH=src and run this file with --output-dir pointing to the intended
report directory. No intermediate calculation files are written.
"""

from __future__ import annotations

import argparse
import ast
import json
import math
import re
from dataclasses import asdict
from html import escape
from pathlib import Path

import yaml

from digitalmodel.alt_fuel_ship_sizing import (
    PortLeg,
    SeaLeg,
    TankParameters,
    boil_off,
    fuel_preset,
    size_fuel_chain,
    size_fuel_chain_voyage,
)
from digitalmodel.alt_fuel_ship_sizing.constants import HOURS_PER_DAY

ROOT = Path(__file__).resolve().parents[4]
EXAMPLE = "examples/workflows/alt-fuel-report-pack/sizing_input.yml"
STYLE_EXEMPLAR = "docs/api/cfd/turbulent-flat-plate-verification.html"
CHANGES = {
    "revision": "1",
    "date": "2026-10-22",
    "changes": [
        {"section": "validation", "text": "Hand-worked headline checks and explicit UTF-8 and LF output distinguish arithmetic evidence from API consistency."},
        {"section": "references", "text": "Configured property precision and open-reference differences are quantified without changing the sizing defaults."},
        {"section": "results", "text": "Each route leg supplies its own speed and the thermal summary includes both tank sizes and the consumption boundary."},
        {"section": "summary", "text": "Sea-passage and port-dwell inventories are compared to expose the additional boil-off allowance."},
        {"section": "basis", "text": "Supplied powering, insulation and reserve inputs are distinguished from fuel properties and acceptance criteria."},
        {"section": "results", "text": "The sea-leg boil-off absorption boundary is shown with the action required above it."},
    ],
}


def flatten(value, prefix=""):
    """Collect source-bound numerical leaves without treating flags as numbers."""
    if isinstance(value, dict):
        return {k: v for name, item in value.items()
                for k, v in flatten(item, f"{prefix}.{name}".strip(".")).items()}
    if isinstance(value, list):
        return {k: v for index, item in enumerate(value)
                for k, v in flatten(item, f"{prefix}.{index}").items()}
    if isinstance(value, (int, float)) and not isinstance(value, bool):
        return {prefix: value}
    return {}


def engineering_number(value):
    """Use one decimal, retaining a nonzero value through scientific notation."""
    if not math.isfinite(value):
        raise ValueError("Nonfinite evidence cannot be issued")
    if value != 0 and abs(value) < 1:
        return f"{value:.1e}".replace("-", "−")
    return f"{value:,.1f}".replace("-", "−")


def calculate():
    """Call the public API on the existing public example, without export effects."""
    settings = yaml.safe_load((ROOT / EXAMPLE).read_text(encoding="utf-8"))["alt_fuel_ship_sizing"]
    fuel = fuel_preset(settings["fuel"]["preset"])
    fuel = type(fuel)(**{**asdict(fuel), **{
        k: v for k, v in settings["fuel"].items() if k in asdict(fuel)
    }})
    tank = TankParameters(**settings["tank"])
    common = dict(
        shaft_power_kw=settings["powering"]["shaft_power_kw"],
        hotel_load_kw=settings["powering"]["hotel_load_kw"],
        eta_electric_drivetrain=settings["efficiency"]["electric_drivetrain"],
        eta_fuel_cell_lhv=settings["efficiency"]["fuel_cell_lhv"],
        tank=tank, fuel=fuel, fuel_margin_fraction=settings["fuel"]["margin_fraction"],
    )
    legs = []
    for item in settings["route"]["legs"]:
        if item["type"] == "sea":
            legs.append(SeaLeg(**{k: v for k, v in item.items() if k != "type"}))
        elif item["type"] == "port":
            legs.append(PortLeg(dwell_h=item["dwell_h"]))
        else:
            raise ValueError("unrecognized route leg type")
    profile = size_fuel_chain_voyage(legs=legs, **common)
    # NBS Monograph 168, sections 1.2.15 and 1.2.25, parahydrogen NBP.
    reference_fuel = type(fuel)(**{**asdict(fuel), "density_kg_per_m3": 70.78,
                                 "heat_of_vaporization_kj_per_kg": 445.5,
                                 "storage_temperature_k": 20.268})
    reference = size_fuel_chain_voyage(legs=legs, **{**common, "fuel": reference_fuel})
    single = size_fuel_chain(
        distance_nm=sum(item.distance_nm for item in legs if isinstance(item, SeaLeg)),
        speed_kn=settings["route"]["service_speed_kn"], **common,
    )
    # At fixed consumed-mode inventory the area and demand remain fixed. This
    # boundary is an energy-accounting identity, not an approved insulation limit.
    draw = single.fuel_mass_flow_kg_per_h * HOURS_PER_DAY
    q_boundary = tank.insulation_heat_flux_w_per_m2 * draw / single.boil_off.bog_kg_per_day
    points = []
    for i in range(1, 41):
        q = q_boundary * i / 20
        varied = TankParameters(**{**settings["tank"], "insulation_heat_flux_w_per_m2": q})
        thermal = boil_off(single.tank, varied, fuel)
        points.append({"x": q, "y": thermal.bog_kg_per_day})
    points.append({"x": tank.insulation_heat_flux_w_per_m2,
                   "y": single.boil_off.bog_kg_per_day})
    points.sort(key=lambda point: point["x"])
    boundary = boil_off(single.tank, TankParameters(**{
        **settings["tank"], "insulation_heat_flux_w_per_m2": q_boundary,
    }), fuel)
    counts = {}
    for path in (
        "tests/naval_architecture/test_alt_fuel_ship_sizing.py",
        "tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py",
        "tests/report_pack/test_alt_fuel_report_pack_demo.py",
    ):
        tree = ast.parse((ROOT / path).read_text(encoding="utf-8"))
        counts[path] = sum(isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
                           and node.name.startswith("test_") for node in ast.walk(tree))
    return {
        "input": settings, "fuel": asdict(fuel), "single": asdict(single),
        "profile": asdict(profile), "counts": counts,
        "comparison": {
            "density_change_percent": 100 * (fuel.density_kg_per_m3 / 70.78 - 1),
            "latent_heat_change_percent": 100 * (fuel.heat_of_vaporization_kj_per_kg / 445.5 - 1),
            "reference_gross_volume_m3": reference.gross_tank_volume_m3,
            "inventory_change_percent": 100 * (profile.required_fuel_mass_kg - single.required_fuel_mass_kg) / single.required_fuel_mass_kg,
            "volume_change_percent": 100 * (profile.gross_tank_volume_m3 - single.gross_tank_volume_m3) / single.gross_tank_volume_m3,
        },
        "chart": {"points": points, "limit": draw,
                  "boundary": {"x": q_boundary, "y": boundary.bog_kg_per_day}},
    }


LABELS = {
    "endurance_hours": ("Sea-passage duration", "h"),
    "total_voyage_hours": ("Total voyage duration", "h"),
    "sea_hours": ("Time at sea", "h"),
    "port_dwell_h": ("Port dwell", "h"),
    "total_distance_nm": ("Total sea distance", "nautical miles"),
    "electrical_power_kw": ("Electrical demand", "kW"),
    "fuel_lhv_power_kw": ("Fuel power on a lower-heating-value basis", "kW"),
    "fuel_mass_flow_kg_per_h": ("Sea fuel consumption", "kg/h"),
    "voyage_fuel_kg": ("Sea fuel including reserve", "kg"),
    "bog_lost_kg": ("Boil-off loss allowance", "kg"),
    "required_fuel_mass_kg": ("Required fuel inventory", "kg"),
    "net_tank_volume_m3": ("Net liquid volume", "m³"),
    "gross_tank_volume_m3": ("Gross tank volume", "m³"),
    "gross_volume_m3": ("Cylinder gross volume", "m³"),
    "diameter_m": ("Cylinder diameter", "m"),
    "length_m": ("Cylinder length", "m"),
    "surface_area_m2": ("Cylinder surface area", "m²"),
    "heat_leak_w": ("Tank heat leak", "W"),
    "bog_kg_per_day": ("Boil-off generation", "kg/day"),
    "bog_rate_percent_per_day": ("Boil-off rate relative to full-load liquid mass", "%/day"),
    "full_load_liquid_mass_kg": ("Full-load liquid mass", "kg"),
    "density_kg_per_m3": ("Liquid density", "kg/m³"),
    "lhv_mj_per_kg": ("Lower heating value", "MJ/kg"),
    "heat_of_vaporization_kj_per_kg": ("Latent heat of vaporization", "kJ/kg"),
    "storage_temperature_k": ("Fuel storage temperature", "K"),
    "service_speed_kn": ("Service speed", "knots"),
    "distance_nm": ("Sea distance", "nautical miles"),
    "speed_kn": ("Sea speed", "knots"),
    "dwell_h": ("Port dwell", "h"),
    "shaft_power_kw": ("Shaft demand", "kW"),
    "hotel_load_kw": ("Hotel electrical demand", "kW"),
    "fuel_cell_lhv": ("Fuel-cell efficiency on a lower-heating-value basis", "fraction"),
    "electric_drivetrain": ("Electric drivetrain efficiency", "fraction"),
    "margin_fraction": ("Sea fuel reserve", "fraction"),
    "ullage_fraction": ("Ullage", "fraction"),
    "length_to_diameter": ("Length divided by diameter", "dimensionless"),
    "insulation_heat_flux_w_per_m2": ("Insulation heat flux", "W/m²"),
    "fuel_price_per_kg": ("Illustrative fuel price", "currency/kg"),
    "duration_h": ("Leg duration", "h"),
    "fuel_burned_kg": ("Fuel burned before reserve", "kg"),
    "bog_generated_kg": ("Boil-off generated", "kg"),
    "bog_consumed_kg": ("Boil-off credited as consumed", "kg"),
    "index": ("Leg index", "dimensionless"),
}


def house_style():
    """Inline the existing canonical tokens and report stylesheet unchanged."""
    brand = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    brand = re.sub(r"/\*.*?\*/", "", brand, flags=re.S)
    brand = "\n".join(line.rstrip() for line in brand.splitlines())
    exemplar = (ROOT / STYLE_EXEMPLAR).read_text(encoding="utf-8")
    style = re.search(r"<style>.*?</style>", exemplar, re.S).group()
    return f"<style>{brand}</style>\n{style}"


def number(key, values):
    """Bind each displayed example number to a numeric API/input record."""
    # Preserve configured source precision for the property comparison.
    if key in {"fuel.density_kg_per_m3", "fuel.storage_temperature_k"}:
        text = f"{values[key]:.2f}"
    elif key.startswith("comparison.") and key.endswith("_percent"):
        text = f"{values[key]:.1f}"
    else:
        text = engineering_number(values[key])
    return f'<span data-number="{key}">{text}</span>'


def ledger(prefix, data, values, source):
    """Render complete records with defined quantities, units and source columns."""
    rows = []
    for key in flatten(data, prefix):
        field = key.split(".")[-1]
        if re.fullmatch(r"input\.trade_study\.speeds_kn\.\d+", key):
            label, unit = "Trade-study service speed", "knots"
        elif field in LABELS:
            label, unit = LABELS[field]
        else:
            raise ValueError(f"unrecognized numeric field: {key}")
        if ".legs." in key:
            leg = int(key.split(".legs.")[1].split(".")[0])
            label = f"Leg {leg + 1}: " + label.lower()
        rows.append(f"<tr><td>{escape(label)}</td><td>{escape(unit)}</td>"
                    f"<td>{number(key, values)}</td><td>{source}</td></tr>")
    return ('<table><thead><tr><th>Quantity</th><th>Unit</th><th>Value</th><th>Source</th></tr></thead>'
            f'<tbody>{"".join(rows)}</tbody></table>')


def route_description(settings, values):
    """Describe each configured leg using its own distance, speed or dwell."""
    descriptions = []
    for index, leg in enumerate(settings["route"]["legs"]):
        prefix = f"input.route.legs.{index}"
        if leg["type"] == "sea":
            descriptions.append(f"sea passage of {number(prefix + '.distance_nm', values)} nautical miles "
                                f"at {number(prefix + '.speed_kn', values)} knots")
        elif leg["type"] == "port":
            descriptions.append(f"port dwell of {number(prefix + '.dwell_h', values)} h")
        else:
            raise ValueError("unrecognized route leg type")
    return "The ordered route comprises: " + "; ".join(descriptions) + "."


def tank_schematic(values):
    """Draw the actual flat-ended cylindrical idealisation, not a pressure design."""
    n = lambda key: number(key, values)
    return f'''<figure>
<svg viewBox="0 0 760 280" role="img" aria-label="Longitudinal cutaway of a flat-ended cylindrical tank with liquid, ullage, heat ingress and vapor outlet">
<defs><marker id="arrow" markerWidth="8" markerHeight="8" refX="7" refY="4" orient="auto"><path d="M0 0 L8 4 L0 8 Z" fill="currentColor"/></marker></defs>
<rect x="80" y="70" width="520" height="115" rx="8" fill="var(--panel-2)" stroke="var(--ink)"/>
<path d="M90 102 H590 V175 H90 Z" fill="var(--ground)" stroke="var(--brand)"/>
<path d="M90 100 H590 M90 80 V175 M590 80 V175" fill="none" stroke="var(--ink)"/>
<path d="M220 30 V67 M370 30 V67 M520 30 V67 M570 80 V45 H690" fill="none" stroke="var(--ink)" marker-end="url(#arrow)"/>
<text x="270" y="23">Uniform insulation heat ingress</text><text x="610" y="68">Vapor outlet</text>
<text x="280" y="95">Ullage</text><text x="250" y="140">Liquid hydrogen</text>
<path d="M80 215 H600 M80 205 V225 M600 205 V225 M40 70 V185 M30 70 H50 M30 185 H50" fill="none" stroke="var(--ink)"/>
<text x="240" y="240">Length L</text><text x="10" y="260">Diameter D</text>
</svg>
<figcaption>Figure A. Cylinder idealisation at the port-dwell inventory: length {n('profile.tank.length_m')} m,
diameter {n('profile.tank.diameter_m')} m. The liquid surface is illustrative; the thermal calculation uses the full cylinder surface.
End closures are flat in the calculation. Insulation thickness, supports, piping and pressure containment are not dimensioned.</figcaption></figure>'''


def render():
    """Write the engineering narrative with the full calculation context available."""
    model = calculate()
    values = flatten(model)
    n = lambda key: number(key, values)
    input_table = ledger("input", model["input"], values, '<a href="#source-example">Documented example; assumed</a>')
    fuel_table = ledger("fuel", model["fuel"], values, '<a href="#source-properties">Configured properties</a>')
    single_table = ledger("single", model["single"], values, '<a href="#source-api">Fuel-chain calculation</a>')
    profile_table = ledger("profile", model["profile"], values, '<a href="#source-api">Voyage calculation</a>')
    count_rows = "".join(f'<tr><td><a href="#source-tests">{label}</a></td><td>{count}</td></tr>'
                         for label, count in zip(("Library calculation checks", "Configuration and export checks", "Report example checks"), model["counts"].values()))
    # Full precision is retained only in machine-readable chart data; visible
    # labels and tooltips use the same engineering precision as the tables.
    chart_json = json.dumps(model["chart"], ensure_ascii=False, allow_nan=False)
    changes_json = json.dumps(CHANGES, ensure_ascii=False)
    body = f'''
<header><div class="kicker">Marine engineering · concept design</div>
<h1>Alternative fuel ship sizing analysis</h1>
<p class="sub">Liquid-hydrogen inventory, cylindrical tank volume and voyage boil-off</p>
<p>Draft for maintainer review</p></header>
<nav aria-label="Report sections"><a href="#summary">Summary</a> · <a href="#basis">Design data</a> ·
<a href="#results">Results</a> · <a href="#validation">Validation</a> · <a href="#references">References</a> · <a href="#appendix">Appendix</a></nav>

<section id="executive"><h2>1. Executive Summary</h2>
<p>Alternative fuel ship sizing is a concept-design calculation of fuel inventory and storage volume.
Analysis is performed to determine the effect of supplied propulsion demand and voyage dwell on liquid-hydrogen storage;
key results are summarized below.</p>
<ul><li>A preliminary storage allowance can be selected for the documented synthetic example.
<ul><li>The sea-only inventory requires {n('single.gross_tank_volume_m3')} m³ gross tank volume;
the example with {n('profile.port_dwell_h')} h in port requires {n('profile.gross_tank_volume_m3')} m³ under the constant heat-leak model.</li></ul></li>
<li>The consumed-vapor assumption can be screened against the sea fuel draw.
<ul><li>At the example heat flux, generation is {n('single.boil_off.bog_kg_per_day')} kg/day for the sea-only tank
and {n('profile.boil_off.bog_kg_per_day')} kg/day for the port-dwell tank. Sea fuel draw is {n('chart.limit')} kg/day.
The sea-only tank reaches that draw at {n('chart.boundary.x')} W/m² in Figure B; above that boundary,
venting or reliquefaction is required and the reported inventory is optimistic.</li></ul></li>
<li>A detailed design cannot be released from these calculations.
<ul><li>Tank pressure, structural adequacy, route powering accuracy and physical validation remain NOT_EVALUATED.</li></ul></li></ul>
<h3>Revision history</h3>
<table><thead><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr></thead>
<tbody><tr><td>0</td><td>2026-10-22</td><td>Initial issue: fuel-chain scope, port-dwell inventory and the vapor-absorption criterion are documented for concept screening.</td></tr>
<tr><td>{CHANGES['revision']}</td><td>{CHANGES['date']}</td><td>Arithmetic evidence, portable reconstruction, property comparison and leg-specific reporting are corrected to make the calculation basis reviewable.</td></tr></tbody></table>
</section>

<section id="introduction"><h2>2. Introduction</h2>
<p>The fuel-chain workflow is used to translate supplied shaft demand into fuel mass and gross storage volume.
The electrical drivetrain and fuel-cell plant are represented by fixed efficiencies on a lower-heating-value basis.</p>
<p>Liquid hydrogen is represented by constant liquid density and latent heat. Hydrogen vapor generated by heat ingress is
either credited to the plant at sea or treated as a loss. Port dwell is represented by zero fuel burn with continuous vapor generation.
This report uses a public repository example with synthetic inputs; it is not a measured ship or an externally published operating trial.
The calculation and example records are identified in <a href="#source-example">the source register</a>.</p>
</section>

<section id="summary"><h2>3. Summary and operating controls</h2>
<p>Inventory allowances are determined by the burn reserve and by the periods in which vapor cannot be absorbed.
The available calculation boundary is an average sea-leg vapor balance; an operating approval envelope is not established.</p>
<p>The port-dwell case carries {n('profile.bog_lost_kg')} kg of additional boil-off allowance.
Port hotel demand is not represented, even though hotel demand is included at sea. Shore power and vapor handling therefore require a separate balance.</p>
<p>For the consumed mode, compare heat-leak generation with consumption for each sea leg. A value above the boundary in Figure B requires
a vapor-disposal or reliquefaction design and a revised inventory calculation. An empty warning list does not establish pressure control.
The governing warning and port-loss semantics are documented in <a href="#source-api">the calculation record</a>.</p>
</section>

<section id="basis"><h2>4. Design data and acceptance basis</h2>
<p>The documented public example is adopted as a synthetic calculation basis. Powering, efficiencies, reserve and insulation
are marked ASSUMED - to be confirmed for application to an actual design.</p>
<p>The supplied sea shaft demand is {n('input.powering.shaft_power_kw')} kW and the hotel electrical demand is
{n('input.powering.hotel_load_kw')} kW. The drivetrain and fuel-cell efficiencies are held constant.
The complete numerical input and property registers are provided in the appendix; the source of each value is stated there.</p>
{tank_schematic(values)}
<h3>Criteria and rationale</h3>
<p>The sea consumed-vapor criterion is generation no greater than fuel draw, because surplus vapor has no sink in that model.
Equality is the balance boundary; generation greater than draw sets the module warning. All port vapor is added as lost inventory.
These are software accounting criteria, not class-rule acceptance criteria. Available arrangement volume, maximum pressure,
minimum reserve endurance and relief capacity are required design inputs and remain NOT_EVALUATED.</p>
<p>The numerical stopping criterion is the change in required fuel mass between successive tank estimates.
The API defaults and the exhaustion exception are recorded in the source register. That tolerance controls iteration closure only;
it provides no measure of physical accuracy.</p>
</section>

<section id="method"><h2>5. Methodology and theory</h2>
<p>Energy conservation and cylindrical geometry are applied with the units defined below.
The equations reproduce the <a href="#source-api">documented library method</a> rather than a resistance or pressure-vessel calculation.</p>
<h3>Energy and inventory balance</h3>
<div class="eq">t = s / v; Pₑ = Pₛ / ηₑ + Pₕ; P𝒇 = Pₑ / η𝒇; ṁ = cₕ P𝒇 / H</div>
<p>The route distance s is in nautical miles, speed v in knots and sea duration t in hours. Shaft demand Pₛ,
hotel demand Pₕ, electrical demand Pₑ and fuel power P𝒇 are in kW. The dimensionless efficiencies ηₑ and η𝒇 represent
the drivetrain and the lower-heating-value plant efficiency. H is lower heating value in MJ/kg and ṁ is fuel flow in kg/h.
The unit conversion cₕ is the seconds-per-hour to kilojoules-per-megajoule ratio; it is defined in <a href="#source-api">the unit implementation</a>.</p>
<div class="eq">Mᵥ = (1 + r) Σ(ṁᵢ tᵢ); M = Mᵥ + Mₗ; Vₙ = M / ρ; V𝓰 = Vₙ / (1 − u)</div>
<p>The subscript ᵢ identifies each sea burn period. Reserve fraction r is applied to sea burn only.
Voyage fuel including reserve Mᵥ, lost vapor Mₗ and required inventory M are in kg. Liquid density ρ is in kg/m³,
ullage fraction u is dimensionless, and net liquid volume Vₙ and gross tank volume V𝓰 are in m³.
Lost vapor is added without a further reserve multiplier, as implemented in <a href="#source-api">the voyage balance</a>.</p>
<h3>Heat leak and vapor generation</h3>
<div class="eq">λ = L / D; V𝓰 = πD²L / 4; A = πDL + πD² / 2; Q = qA; ṁᵥ = Q / hᵥ</div>
<p>Tank length L and diameter D are in m; slenderness λ is dimensionless. Surface area A is in m².
Uniform heat flux q is in W/m², heat ingress Q is in W, latent heat hᵥ is expressed in J/kg for this equation,
and vapor generation ṁᵥ is in kg/s. The reported vapor rate is converted to kg/day; its percentage rate is referenced to the full-load liquid mass.
The alternative heat-flux input is q = U(Tₐ − Tₛ), with thermal transmittance U in W/(m² K) and ambient and storage temperatures Tₐ and Tₛ in K.</p>
<p>The heat balance uses constant saturation properties, uniform heat flux and the entire cylinder area throughout the voyage.
The public monograph defines latent heat as the saturated vapor–liquid enthalpy difference; the repository property approximations are retained,
with their external qualification limits stated in <a href="#source-properties">the property source register</a>.</p>
<p>Inventory is iterated when lost vapor increases tank size and hence surface area. In consumed mode, sea vapor is credited without adding inventory;
surplus generation is flagged rather than removed from the credit. Port vapor is always added as a loss. Leg sequence does not change heat leak,
because no depletion-dependent liquid level or pressure state is solved.</p>
</section>

<section id="results"><h2>6. Detailed results and worked example</h2><div id="worked-example">
<p>The documented example is calculated by calling the public fuel-chain and voyage functions.
The sea-only comparison uses the aggregate distance and the configured comparison service speed while omitting port dwell.</p>
<p>{route_description(model['input'], values)} At the sea-only comparison point, electrical demand is {n('single.electrical_power_kw')} kW,
fuel power is {n('single.fuel_lhv_power_kw')} kW and fuel flow is {n('single.fuel_mass_flow_kg_per_h')} kg/h.</p>
<table><thead><tr><th>Quantity</th><th>Unit</th><th>Sea only</th><th>With port dwell</th><th>Source</th></tr></thead><tbody>
<tr><td>Fuel inventory</td><td>kg</td><td>{n('single.required_fuel_mass_kg')}</td><td>{n('profile.required_fuel_mass_kg')}</td><td><a href="#source-api">Calculation</a></td></tr>
<tr><td>Gross tank volume</td><td>m³</td><td>{n('single.gross_tank_volume_m3')}</td><td>{n('profile.gross_tank_volume_m3')}</td><td><a href="#source-api">Calculation</a></td></tr>
<tr><td>Lost vapor allowance</td><td>kg</td><td>{n('single.bog_lost_kg')}</td><td>{n('profile.bog_lost_kg')}</td><td><a href="#source-api">Calculation</a></td></tr>
<tr><td>Vapor generation</td><td>kg/day</td><td>{n('single.boil_off.bog_kg_per_day')}</td><td>{n('profile.boil_off.bog_kg_per_day')}</td><td><a href="#source-api">Calculation</a></td></tr>
</tbody></table>
<p>The inventory change follows from port vapor losses and the resulting increase in tank heat leak.
Inventory increases by {n('comparison.inventory_change_percent')} % and gross volume by {n('comparison.volume_change_percent')} % relative to the sea-only reference.
The complete output records, including geometry and each leg balance, are tabulated in the appendix. Available hold volume remains NOT_EVALUATED.</p>
</div></section>

<section id="validation"><h2>7. Validation evidence and sensitivity</h2>
<p>Repository evidence is separated into arithmetic regression, export consistency and physical validation.
The existing tests exercise the first two categories; comparison with measured fuel or tank data is not established by those records.</p>
<table><thead><tr><th>Existing evidence group</th><th>Defined test functions</th></tr></thead><tbody>{count_rows}</tbody></table>
<p>The library tests cover the energy chain, flat-ended tank geometry, heat leak, consumed and lost vapor,
port dwell, leg power overrides, fuel presets, wind interpolation, speed sweeps and tonnage provisos.
Configuration tests exercise parsing and comma-separated-value exports. The example tests compare checked-in tables with a fresh calculation
and exercise the separate report renderer. Counts denote test definitions, not independent physical experiments.</p>
<figure><svg id="thermal-chart" viewBox="0 0 760 340" role="img" aria-label="Boil-off generation against insulation heat flux with the sea fuel consumption boundary"></svg>
<label for="heat-case">Select heat-flux case</label> <input id="heat-case" type="range" min="0" max="{len(model['chart']['points']) - 1}" value="0" step="1">
<p id="chart-readout" role="status" aria-live="polite"></p>
<figcaption>Figure B. Sea-only consumed-mode thermal sensitivity at fixed inventory. Hover or focus a point to read heat flux,
generation demand and absorption limit. The dashed line is the module's average sea fuel draw, not a relief-system rating.</figcaption></figure>
<p>The legend is read as follows. Green indicates generation below the absorption boundary. Amber indicates the balance boundary.
Red indicates generation above the boundary, requiring venting or reliquefaction and a revised inventory model.
The increased heat flux is a sensitivity variable; it is not a proposed insulation specification.</p>
<p>External calibration, transient tank pressure, fuel-cell turndown and measured route weather remain NOT_EVALUATED.
The published property monograph supports thermodynamic definitions; it does not validate this ship-sizing implementation.</p>
</section>

<section id="limits"><h2>8. Key findings, limitations and recommendations</h2>
<p>The principal inventory effect is the loss of vapor during periods without fuel consumption.
The reported volumes are conditional on the supplied powering and on the constant heat-leak approximation.</p>
<p>A separate vapor-handling balance is required whenever a sea leg exceeds the Figure B boundary or any port dwell is present.
Venting or reliquefaction, pressure protection and port energy supply are to be resolved before the consumed-mode assumption is used for design.
This action follows the module warning and zero-port-burn treatment identified in <a href="#source-api">the calculation record</a>.</p>
<p>Replace the synthetic shaft-demand, efficiency and insulation inputs with qualified route and supplier data before selecting storage capacity.
Obtain arrangement space, tank closure geometry, supports and relief design before treating the calculated cylinder as an installable tank.
Comparison with a measured heat-leak test and a route energy balance is required to establish physical accuracy for the intended storage condition.</p>
<h3>Not covered</h3>
<p>Wind-assist thrust integration, fuel-price comparisons, alternative-fuel property selection and tonnage are available sub-areas but are not engineering
assessments in this report. Supplied wind polars and a route wind distribution are required for a wind assessment. The speed sweep uses an aggregate
sea passage and excludes dwell; it is not a schedule calculation. Explicit leg shaft powers bypass the default wind reduction, so the enriched summary
is not a substitute for checking each leg. These behaviors are identified in <a href="#source-workflow">the workflow source</a>.</p>
<p>Refrigerated ammonia and ambient methanol require separate property and plant bases. The methanol preset does not establish an ambient boil-off model.
Resistance, sea-state limits, heading limits, crane rigging, structural stress and stability are not represented; installation envelopes and forecast controls
are therefore not assigned. General numerical finiteness is not comprehensively checked by the module, so nonfinite inputs require rejection before use.</p>
</section>

<section id="references"><h2>9. References and source register</h2>
<p>The calculation basis is identified by the public repository records and an open government reference.
Quoted example values originate in those records or in direct function returns; no operating limit is inferred from an uncited handbook value.</p>
<h3>9.1. Reading basis</h3>
<p>The documented example is used to demonstrate calculation behavior. The property constants are treated as configured approximations;
the monograph's definition of saturation latent heat is used as the physical basis. No licensed standard values are reproduced.</p>
<h3>9.2. Source locators and editions</h3>
<dl class="refs">
<dt id="source-example">Documented public example</dt><dd>Repository edition accompanying this report:
<code>{EXAMPLE}</code>. Synthetic sea–port–sea route, fuel-chain inputs, speed sweep and illustrative price.
The reserve, ullage, heat flux, efficiencies and powering are example assumptions, not prescribed design coefficients.</dd>
<dt id="source-api">Calculation and unit implementation</dt><dd>Repository edition accompanying this report:
<code>src/digitalmodel/alt_fuel_ship_sizing/lh2_fuel_chain.py</code>,
<code>size_fuel_chain</code>, <code>size_fuel_chain_voyage</code>, <code>boil_off</code>,
<code>cylindrical_tank_geometry</code>, <code>electrical_power_kw</code> and <code>fuel_mass_flow_kg_per_h</code>.
<code>constants.py</code> defines time conversions. The voyage API defaults to fifty iterations and a mass-change tolerance of one milligram;
exhaustion raises an exception. Efficiency is positive and no greater than unity; ullage is nonnegative and less than unity;
distance, speed, property values and the supplied heat-flux magnitude are positive. These are implementation domains.</dd>
<dt id="source-properties">Configured fuel properties</dt><dd>Repository edition accompanying this report:
<code>src/digitalmodel/alt_fuel_ship_sizing/constants.py</code> and the documented example's lower-heating-value override.
The hydrogen density, latent heat and storage temperature in the appendix are retained from the module, not re-fitted to an external table.
The configured density {n('fuel.density_kg_per_m3')} kg/m³ and latent heat {n('fuel.heat_of_vaporization_kj_per_kg')} kJ/kg
are higher than the monograph's normal-boiling entries by {n('comparison.density_change_percent')} % and
{n('comparison.latent_heat_change_percent')} %, respectively. Using both reference properties gives
{n('comparison.reference_gross_volume_m3')} m³ rather than {n('profile.gross_tank_volume_m3')} m³ for the port-dwell tank,
with all other inputs retained. This is a property sensitivity, not physical validation.</dd>
<dt id="source-monograph">Open thermodynamic reference</dt><dd><a href="https://nvlpubs.nist.gov/nistpubs/Legacy/MONO/nbsmonograph168.pdf">Selected Properties of Hydrogen (Engineering Design Data),
National Bureau of Standards Monograph 168, 1981</a>, section 1.2.15, Fixed Points, and section 1.2.25, Heat of Vaporization.
The parahydrogen normal-boiling entries are 70.78 kg/m³ at 20.268 K and 445.5 J/g (equivalently kJ/kg).
These published entries are compared directly with the configured approximations; the configured storage temperature is {n('fuel.storage_temperature_k')} K.
The open government copy was accessible on the report issue date. The latent-heat definition uses saturated vapor and liquid at the same temperature and pressure.</dd>
<dt id="source-tests">Existing test evidence</dt><dd>Repository edition accompanying this report:
<code>tests/naval_architecture/test_alt_fuel_ship_sizing.py</code>,
<code>tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py</code> and
<code>tests/report_pack/test_alt_fuel_report_pack_demo.py</code>. The first contains hand-arithmetic comparisons;
the last also compares the retained example tables with fresh exports. Test-definition counts are extracted from these files.</dd>
<dt id="source-workflow">Workflow and published usage</dt><dd>Repository edition accompanying this report:
<code>src/digitalmodel/alt_fuel_ship_sizing/workflow.py</code> and
<code>docs/domains/ship-design/alt-fuel-lh2-sizing.md</code>. The documented callable interface is
<code>digitalmodel.alt_fuel_ship_sizing</code>; <code>workflow.router</code> accepts the configuration mapping and writes the summary,
voyage-leg, optional wind-bin, speed-study and tonnage tables under the supplied output directory.
The returned mapping contains the same result records. File names are derived from the configuration stem.</dd>
<dt id="source-rebuild">Report reconstruction</dt><dd>Repository edition accompanying this report:
<code>docs/reports/modules/altfuelshipsizing/generate.py</code> and
<code>tests/docs/test_altfuelshipsizing_report.py</code>. From the repository root, set the source import path and execute
<code>python docs/reports/modules/altfuelshipsizing/generate.py --output-dir &lt;report directory&gt;</code>.
The output directory receives the HTML and change panel data only. Intermediate exports are not needed.
The layout and inlined styles are reused from <code>{STYLE_EXEMPLAR}</code> and <code>docs/api/_assets/brand.css</code>.
The document body uses <code>hs-doc</code>.</dd>
</dl></section>

<section id="appendix"><h2>10. Appendix: data register and quality evidence</h2>
<p>The complete input and numerical output records are retained below to permit an independent review of the example.
Values are calculated afresh; display rounding does not change the inputs used by the module.
Nonzero magnitudes below unity are printed in scientific notation to retain significant digits.</p>
<h3>Data register</h3>
<p>ASSUMED - to be confirmed: route, powering, efficiencies, reserve, ullage, slenderness, insulation flux and fuel price are copied from the synthetic example.
The lower heating value is its deliberate round-number override. The price and speed-sweep inputs are recorded for completeness but are not used in the reported inventory comparison.
Storage properties are configured approximations with the source limits stated above. Actual arrangement volume, pressure limits, supplier guarantees,
port loads and measured validation data are missing; the affected design decisions remain NOT_EVALUATED.</p>
{input_table}<p class="sub">Table A. All numerical example inputs; source and assumption status are retained.</p>
{fuel_table}<p class="sub">Table B. Effective fuel properties used by the calculation, including the example override.</p>
<h3>Complete calculation records</h3>
{single_table}<p class="sub">Table C. Sea-only comparison, including geometry and full-load-reference vapor rate.</p>
{profile_table}<p class="sub">Table D. Documented voyage, including each leg's duration, burn and vapor allocation.</p>
<p>The consumed mode is used in both cases. The example sea legs do not set the excess-generation flag; the port leg is treated as lost vapor.
Sea-only loss is a true zero. A port-leg zero burn is the model definition, not a missing datum.</p>
<h3>Quality pre-flight evidence</h3>
<p>The reconstruction check compares both delivered files byte for byte with a fresh build. Each displayed example number carries a source key;
the API consistency test calls the same public functions as the builder and compares every key and displayed value, including all numeric result fields and leg records.
Separate hand-worked literal assertions check the sea flow, reserve inventory, port-dwell inventory, gross volume,
lost vapor, inventory increase and heat-flux boundary without using the sizing functions as the expected-value oracle.
The export check also compares the configuration workflow with the voyage calculation in a temporary output directory.</p>
<p>The chart check compares every plotted generation value with the module heat-leak function and checks that the absorption boundary is crossed.
Document checks cover section count, reference targets, inlined house styles, public wording, precision and local comment controls.
These checks establish reconstruction consistency; physical validation and browser interaction testing remain NOT_EVALUATED.</p>
<details id="changes"><summary>Engineering changes since the previous revision</summary><div id="change-list"></div></details>
<details id="comments"><summary>Section comments</summary>
<p>Comments are kept in this browser until saved. Select Save comments and place the comments file beside this report.
Load comments restores a saved file. Browsers without direct file saving download the file; select the report folder as the destination.</p>
<label for="comment-section">Section</label><select id="comment-section"></select>
<label for="comment-text">Comment</label><textarea id="comment-text" rows="4"></textarea>
<button id="add-comment" type="button">Add comment</button>
<button id="save-comments" type="button">Save comments</button>
<label for="load-comments">Load comments</label><input id="load-comments" type="file" accept="application/json,.json">
<p id="comment-status" role="status" aria-live="polite"></p><ol id="comment-list"></ol></details>
</section>
<footer>Concept-design screening. Numerical consistency does not establish physical accuracy or design approval.</footer>
'''
    return f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Alternative fuel ship sizing analysis</title>
{house_style()}
<style>
svg{{width:100%;height:auto}} svg text{{font:13px sans-serif;fill:var(--ink)}}
textarea{{display:block;width:100%;font:inherit}} button,input,select{{font:inherit;margin:6px}}
details{{border:1px solid var(--line);padding:12px;margin:16px 0}} td{{vertical-align:top}}
dt{{margin-top:14px}} dd{{margin-left:0}} a{{color:var(--brand)}}
</style></head><body class="hs-doc"><div class="wrap">{body}</div>
<script id="chart-data" type="application/json">{chart_json}</script>
<script id="changes-data" type="application/json">{changes_json}</script>
<script>{INTERACTIONS}</script>
</body></html>
'''


INTERACTIONS = r'''
(() => {
  'use strict';
  const data = JSON.parse(document.getElementById('chart-data').textContent);
  const chart = document.getElementById('thermal-chart');
  const ns = 'http://www.w3.org/2000/svg';
  function element(tag, attrs, text) {
    const node = document.createElementNS(ns, tag);
    Object.entries(attrs).forEach(([key, value]) => node.setAttribute(key, value));
    if (text !== undefined) node.textContent = text;
    chart.appendChild(node); return node;
  }
  const xmax = Math.max(...data.points.map(point => point.x));
  const ymax = Math.max(...data.points.map(point => point.y));
  const x = value => 90 + value / xmax * 620;
  const y = value => 280 - value / ymax * 240;
  const fmt = value => value.toLocaleString('en', {minimumFractionDigits: 1, maximumFractionDigits: 1});
  element('path', {d: 'M90 40 V280 H710', fill: 'none', stroke: 'var(--ink)'});
  element('text', {x: 260, y: 330}, 'Insulation heat flux (W/m²)');
  element('text', {x: 90, y: 22}, 'Boil-off generation (kg/day)');
  [0, 0.5, 1].forEach(fraction => {
    element('text', {x: x(xmax * fraction), y: 300, 'text-anchor': 'middle'}, fmt(xmax * fraction));
    element('text', {x: 80, y: y(ymax * fraction), 'text-anchor': 'end'}, fmt(ymax * fraction));
  });
  element('line', {x1: 90, x2: 710, y1: y(data.limit), y2: y(data.limit),
    stroke: 'var(--caution)', 'stroke-width': 2, 'stroke-dasharray': '6 4'});
  element('text', {x: 100, y: y(data.limit) - 10}, 'Sea draw boundary: ' + fmt(data.limit) + ' kg/day');
  const readout = document.getElementById('chart-readout');
  const circles = [];
  function describe(point) {
    return 'Sea-only case; tank vapor; heat flux ' + fmt(point.x) + ' W/m²; generation demand ' +
      fmt(point.y) + ' kg/day; absorption limit ' + fmt(data.limit) + ' kg/day.';
  }
  function show(index) {
    circles.forEach((circle, i) => circle.setAttribute('r', i === index ? 7 : 4));
    readout.textContent = describe(data.points[index]);
    document.getElementById('heat-case').value = index;
  }
  data.points.forEach((point, index) => {
    const boundary = Math.abs(point.y - data.limit) < data.limit * Number.EPSILON * 8;
    const color = boundary ? 'var(--caution)' : point.y < data.limit ? 'var(--go)' : 'var(--nogo)';
    const circle = element('circle', {cx: x(point.x), cy: y(point.y), r: 4, fill: color,
      tabindex: 0, role: 'img', 'aria-label': describe(point)});
    const title = document.createElementNS(ns, 'title'); title.textContent = describe(point); circle.appendChild(title);
    circle.addEventListener('mouseenter', () => show(index));
    circle.addEventListener('focus', () => show(index)); circles.push(circle);
  });
  document.getElementById('heat-case').addEventListener('input', event => show(Number(event.target.value)));
  show(0);

  const changes = JSON.parse(document.getElementById('changes-data').textContent);
  const panel = document.getElementById('change-list');
  changes.changes.forEach(change => {
    const paragraph = document.createElement('p');
    const link = document.createElement('a'); link.href = '#' + change.section;
    link.textContent = change.text; paragraph.appendChild(link); panel.appendChild(paragraph);
  });
  const key = 'alternative-fuel-sizing-comments';
  let comments = [], fileHandle;
  const status = document.getElementById('comment-status');
  try { comments = JSON.parse(localStorage.getItem(key) || '[]'); } catch (_) { comments = []; }
  const sections = [...document.querySelectorAll('section[id]')];
  const select = document.getElementById('comment-section');
  sections.forEach(section => {
    const option = document.createElement('option'); option.value = section.id;
    option.textContent = section.querySelector('h2').textContent; select.appendChild(option);
  });
  function valid(items) {
    return Array.isArray(items) && items.every(item => item && typeof item.text === 'string' &&
      sections.some(section => section.id === item.section));
  }
  if (!valid(comments)) comments = [];
  function display() {
    const list = document.getElementById('comment-list'); list.replaceChildren();
    comments.forEach(comment => {
      const row = document.createElement('li');
      const heading = sections.find(section => section.id === comment.section).querySelector('h2').textContent;
      row.textContent = heading + ': ' + comment.text; list.appendChild(row);
    });
    try { localStorage.setItem(key, JSON.stringify(comments)); }
    catch (_) { status.textContent = 'Browser storage is unavailable. Save comments to retain them.'; }
  }
  document.getElementById('add-comment').addEventListener('click', () => {
    const text = document.getElementById('comment-text').value.trim();
    if (!text) return;
    comments.push({section: select.value, text}); display();
    document.getElementById('comment-text').value = '';
    status.textContent = 'Comment added. Save comments to retain a file beside the report.';
  });
  document.getElementById('save-comments').addEventListener('click', async () => {
    const payload = JSON.stringify({revision: changes.revision, comments}, null, 2) + '\n';
    try {
      if (window.showSaveFilePicker) {
        fileHandle = fileHandle || await window.showSaveFilePicker({suggestedName: 'alternative-fuel-sizing.comments.json',
          types: [{description: 'Section comments', accept: {'application/json': ['.json']}}]});
        const stream = await fileHandle.createWritable(); await stream.write(payload); await stream.close();
        status.textContent = 'Comments saved to the selected file.';
      } else {
        const url = URL.createObjectURL(new Blob([payload], {type: 'application/json'}));
        const link = document.createElement('a'); link.href = url; link.download = 'alternative-fuel-sizing.comments.json';
        link.click(); setTimeout(() => URL.revokeObjectURL(url), 1000);
        status.textContent = 'Comments downloaded. Save the file beside the report.';
      }
    } catch (error) {
      status.textContent = error.name === 'AbortError' ? 'Save cancelled; comments remain in the browser.' :
        'Comments were not saved. Check browser file permissions and retry.';
    }
  });
  document.getElementById('load-comments').addEventListener('change', async event => {
    const file = event.target.files[0]; if (!file) return;
    try {
      const saved = JSON.parse(await file.text());
      if (!valid(saved.comments)) throw new Error('Invalid comment file');
      comments = saved.comments; display();
      status.textContent = saved.revision === changes.revision ? 'Comments loaded.' :
        'Comments loaded from another revision. Recheck their section context.';
    } catch (_) { status.textContent = 'The selected file is not a supported comment file.'; }
  });
  display();
})();
'''


def build(output_dir):
    """Write only the two requested deliverables to the explicit destination."""
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "index.html").write_text(render(), encoding="utf-8", newline="\n")
    (output_dir / "changes.json").write_text(
        json.dumps(CHANGES, indent=2, ensure_ascii=False) + "\n", encoding="utf-8", newline="\n",
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    build(parser.parse_args().output_dir)
