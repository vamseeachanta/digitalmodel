# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build the self-contained production chemistry module analysis.

Usage: PYTHONPATH=src uv run python scripts/production_chemistry/build_module_report.py
The published explorer supplies the example inputs; chemistry is evaluated only
by the public module. No solver or remote service is used during rebuilding.
"""

from __future__ import annotations

import argparse
import ast
from dataclasses import asdict
import hashlib
import importlib.util
import json
import math
from pathlib import Path
import re
from types import ModuleType
from typing import Any, Iterable, Sequence

from digitalmodel.production_chemistry import (
    default_coefficients, saturation_indices, si_profile,
)
from digitalmodel.production_chemistry.scale_si import ION_PROPERTIES, SCALE_FAMILIES

ROOT = Path(__file__).resolve().parents[2]
DESTINATION = ROOT / "docs/reports/modules/productionchemistry"
EXAMPLE = "scripts/production_chemistry/build_scale_si_explorer.py"
IMPLEMENTATION = "src/digitalmodel/production_chemistry/scale_si.py"
TESTS = "tests/production_chemistry/test_scale_si.py"
HOUSE = "src/digitalmodel/reporting/assets/calc_report_format.html"
ENGINE_CSS = "src/digitalmodel/reporting/assets/standard_report.css"
SCRIPT = "scripts/production_chemistry/module_report.js"
REVISION = "02"
DATE = "2026-10-25"
CHANGES = {
    "report": "productionchemistry", "revision": REVISION,
    "revisions": [{"revision": "00", "date": "2026-10-21",
        "changes": [
            "Initial analysis of mineral-scale screening along a production string.",
            "Synthetic public-example results are separated from field design acceptance.",
            "Coefficient calibration and independent physical comparison remain required.",
        ]}, {"revision": "01", "date": "2026-10-22",
        "changes": [
            "Numeric serialization and text records are normalized for reproducible builds.",
            "Coefficient precision and unevaluated-result wording are corrected for independent readback.",
            "The method reference and shared report output name are added for consistent discovery.",
        ]}, {"revision": REVISION, "date": DATE,
        "changes": [
            "Independent endpoint arithmetic is recorded in a literal hand-calculation worksheet.",
            "The method is described as a conditional-solubility framework and cited by title and identifier.",
        ]}],
}
TITLES = (
    "Executive summary", "Introduction", "Design data", "Analysis methodology",
    "Detailed results", "Key findings", "Limitations", "Recommendations",
    "References", "Appendix",
)
ION_NAMES = {
    "na": "Sodium", "k": "Potassium", "mg": "Magnesium", "ca": "Calcium",
    "sr": "Strontium", "ba": "Barium", "fe": "Ferrous iron", "cl": "Chloride",
    "so4": "Sulfate", "hco3": "Bicarbonate",
}


def read_example() -> ModuleType:
    """Load the repository's documented public example without rendering it."""
    spec = importlib.util.spec_from_file_location("scale_example", ROOT / EXAMPLE)
    if spec is None or spec.loader is None:
        raise RuntimeError("Public example loader is unavailable")
    example = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(example)
    return example


def portable_numbers(value: Any) -> Any:
    """Retain twelve significant figures before serializing or hashing results."""
    if isinstance(value, float):
        return float(f"{value:.12g}")
    if isinstance(value, dict):
        return {key: portable_numbers(item) for key, item in value.items()}
    if isinstance(value, list):
        return [portable_numbers(item) for item in value]
    return value


def build_data() -> dict[str, Any]:
    """Evaluate the fixed-composition workflow through the public API."""
    example = read_example()
    brine = example.FORMATION_WATER
    points = [
        (example.T_BH + (example.T_WH - example.T_BH) * i / (example.N_STEPS - 1),
         example.P_BH + (example.P_WH - example.P_BH) * i / (example.N_STEPS - 1))
        for i in range(example.N_STEPS)
    ]
    profile = si_profile(brine, points).to_dict(orient="records")
    states = []
    for temperature, pressure in points:
        states.append({family: {
            "si": result.si, "log10_iap": result.log10_iap,
            "pk_cond": result.pk_cond, "details": dict(result.details),
        } for family, result in saturation_indices(brine, temperature, pressure).items()})
    tree = ast.parse((ROOT / TESTS).read_text(encoding="utf-8"))
    tests = [n.name for n in tree.body if isinstance(n, ast.FunctionDef)
             and n.name.startswith("test_")]
    data = {
        "inputs": {"brine": asdict(brine)}, "profile": profile, "states": states,
        "ionic_strength": brine.ionic_strength(),
        "criterion": math.log10(1.0), "families": list(SCALE_FAMILIES),
        "coefficients": {k: asdict(v) for k, v in default_coefficients().items()},
        "ions": {ion: {"molecular_mass": mass, "charge": charge}
                 for ion, (mass, charge) in ION_PROPERTIES.items()},
        "evidence": {"test_count": len(tests), "tests": tests},
        "sources": {path: hashlib.sha256((ROOT / path).read_text(encoding="utf-8").encode("utf-8")).hexdigest()
                    for path in (EXAMPLE, IMPLEMENTATION, TESTS, HOUSE, ENGINE_CSS, SCRIPT)},
    }
    data["numbers"] = {}
    return {key: portable_numbers(value) for key, value in data.items()}


def resolve(data: dict[str, Any], path: str) -> float:
    """Resolve a numeric document locator without a second calculation."""
    value: Any = data
    for key in path.split("."):
        value = value[int(key)] if isinstance(value, list) else value[key]
    return float(value)


def number(data: dict[str, Any], key: str, places: int = 1, tag: str = "span") -> str:
    """Emit a numeric value with its machine-checkable origin and precision."""
    value = resolve(data, key)
    data["numbers"][key] = value
    text = f"{value:.{places}f}" if places >= 0 else f"{value:.5e}"
    text = text.replace("-", "−")
    return f'<{tag} data-number="{key}" data-places="{places}">{text}</{tag}>'


def table(headers: Sequence[str], rows: Iterable[Sequence[str]], caption: str) -> str:
    """Keep data in real table columns, with the caption below the table."""
    head = "".join(f"<th scope=\"col\">{h}</th>" for h in headers)
    body = "".join("<tr>" + "".join(f"<td>{v}</td>" for v in row) + "</tr>" for row in rows)
    return (f'<div class="tbl-wrap"><table><thead><tr>{head}</tr></thead>'
            f'<tbody>{body}</tbody></table></div><p class="fig-cap">{caption}</p>')


def section(index: int, body: str) -> str:
    """Use the existing calculation-report section grammar."""
    return (f'<section id="section-{index}"><div class="l1head">'
            f'<span class="secnum"></span><h2>{TITLES[index - 1]}</h2></div>{body}</section>')


def schematic(data: dict[str, Any]) -> str:
    """Draw the production-string boundary conditions without invented dimensions."""
    n = lambda key: number(data, key, 1, "tspan")
    last = len(data["profile"]) - 1
    return f'''<figure class="card"><svg viewBox="0 0 660 380" role="img"
aria-label="Production tubing from reservoir to wellhead choke and separator">
<defs><marker id="flow-arrow" markerWidth="8" markerHeight="8" refX="6" refY="3"
orient="auto"><path d="M0,0 L6,3 L0,6" fill="var(--cfd)"/></marker></defs>
<path d="M60 95H600 M60 315H310" stroke="var(--hairline)" fill="none"/>
<path d="M140 95V305H220V95 M158 95V290H202V95" fill="none"
stroke="var(--ink-muted)" stroke-width="3"/>
<path d="M180 285V68H410" fill="none" stroke="var(--cfd)" stroke-width="3"
marker-end="url(#flow-arrow)"/>
<path d="M282 54L304 82V54L282 82Z" fill="var(--surface-2)" stroke="var(--ink)"/>
<rect x="420" y="40" width="180" height="80" rx="32" fill="var(--surface-2)"
stroke="var(--ink-muted)" stroke-width="2"/>
<text x="440" y="85">Separator</text><text x="285" y="35">Choke</text>
<text x="65" y="140">Casing</text><text x="230" y="195">Production tubing</text>
<text x="65" y="345">Reservoir / bottomhole inlet</text>
<text x="260" y="240">Bottomhole: {n('profile.0.t_f')} °F;
{n('profile.0.p_psia')} psia</text>
<text x="260" y="270">Wellhead: {n(f'profile.{last}.t_f')} °F;
{n(f'profile.{last}.p_psia')} psia</text>
<text x="260" y="300">Fixed water and gas composition</text>
</svg><figcaption class="fig-cap">Figure A. Production-string boundary conditions from the public
synthetic example [1]. The drawing identifies the flow route and equipment;
geometry, depth and downstream separator conditions are not supplied. Dimensions
and hydraulic performance are not evaluated.</figcaption></figure>'''


def render_body(data: dict[str, Any]) -> list[str]:
    """Write the technical narrative as one document with the module in context."""
    n = lambda key, places=1: number(data, key, places)
    last = len(data["profile"]) - 1
    start = "states.0.calcite.si"
    end = f"states.{last}.calcite.si"
    rows = []
    for family in data["families"]:
        index = data["states"][-1][family]["si"]
        disposition = ("NOT_EVALUATED." if not math.isfinite(index) else
                       "Supersaturation is indicated." if index > data["criterion"] else
                       "Supersaturation is not indicated.")
        rows.append((family.capitalize(), n(f"states.0.{family}.si"),
                     n(f"states.{last}.{family}.si"),
                     n(f"profile.{last}.dsi_{family}", 2),
                     disposition, "[2], public-example calculation"))
    result_table = table(
        ("Mineral", "Bottomhole index (dimensionless)", "Wellhead index (dimensionless)",
         "Change in index (dimensionless; two decimals)", "Wellhead disposition", "Source"), rows,
        "Table A. Computed endpoint saturation indices; the saturation boundary is zero [2, 4]. "
        "A positive index is a thermodynamic screening result, not a deposition-rate prediction.")
    summary = f'''<p>Production chemistry is a mineral-scale screening module for produced water.
Analysis is performed to determine the change in mineral saturation along a specified
production-string temperature and pressure path. The key results are summarized below.</p>
<ul><li>The public synthetic example can be reproduced through the documented interface.
<ul><li>Calcite saturation index changes from {n(start)} at the bottomhole to {n(end)} at the wellhead,
against the saturation boundary of {n('criterion')} [1, 2, 4].</li></ul></li>
<li>The screening results can guide the selection of conditions for further chemical assessment.
<ul><li>Quantitative design suitability is not established because the supplied coefficients
are not calibrated against an independent physical dataset [2].</li></ul></li></ul>
<div class="revhist"><h3>Revision history</h3>
{table(('Revision', 'Date', 'What changed and why'), [
('00', '2026-10-21', 'Initial mineral-scale screening analysis; coefficient and physical-comparison gaps are identified before design use.'),
('01', '2026-10-22', 'Portable rebuilds, coefficient readback and unevaluated-result reporting are corrected; the method reference and shared output name are added.'),
(REVISION, DATE, 'Independent endpoint arithmetic is recorded; the method citation and framework description are corrected.')],
'Revision record. Each issue retains its original entry and records the reason for the change.')}</div>'''
    introduction = '''<p>Mineral-scale tendency is assessed from water composition and local
thermal and pressure conditions. The calculation is used to identify supersaturation
along the specified path; deposited mass and treatment dosage are not calculated [2].</p>
<p>The main workflow comprises composition entry, a single-state calculation and an
ordered production-string profile. Calcite, gypsum, hemihydrate, anhydrite, barite,
celestite and halite are returned. The public example is a synthetic published
repository example, not a measured operating case or an external experimental benchmark [1–3].</p>
<h3>Not covered</h3><p>Ideal volumetric water mixing and compatibility sweeps are available and
are addressed by existing tests [3]. Their calculated results are outside this report.
Mixing is not followed by carbonate re-equilibration or precipitation. In particular,
when gas carbon dioxide is defined for only one endmember, the gas route is dropped
at interior mixture fractions; endpoint and interior calcite indices can then follow
different carbonate routes [2]. Inhibitor selection, corrosion, organic deposits,
reactive transport and fluid hydraulics are outside the implemented screening method.</p>'''
    design = f'''<p>The water composition and path are taken directly from the public
example [1]. The composition is held fixed while the stated pressure and temperature
are applied; depletion of dissolved ions is not calculated [2].</p>
<p>The bottomhole is specified at {n('profile.0.t_f')} °F and {n('profile.0.p_psia')} psia.
The wellhead is specified at {n(f'profile.{last}.t_f')} °F and
{n(f'profile.{last}.p_psia')} psia, with linear intermediate states [1].
The gas carbon dioxide mole fraction is {n('inputs.brine.co2_mole_fraction_gas', 3)}
(dimensionless). The supplied pH is {n('inputs.brine.ph')},
but the positive gas fraction selects the gas route throughout this path [2].</p>
{schematic(data)}
<p>Ion concentrations are entered in milligrams per litre. Temperature is entered in
degrees Fahrenheit and pressure in pounds per square inch absolute (psia).
The full input register is given in Appendix 10.1. pH is the negative decimal
logarithm of hydrogen-ion activity; the mixing approximation instead uses
hydrogen-ion concentration [2]. The synthetic composition is marked
ASSUMED - to be confirmed for transfer to any actual service.</p>'''
    methodology = '''<p>Saturation is evaluated from a conditional solubility correlation.
Dissolved concentrations are used in place of explicit activities, with activity
effects assigned to the conditional constants [2]. The physical definition uses
activities; this distinction limits direct comparison with full speciation models [4, 5].</p>
<h3>Calculation basis and criteria</h3>
<div class="eq"><div class="m">SI = log<sub>10</sub>(Q / K<sub>sp</sub>)</div></div>
<p>SI denotes saturation index, Q the ion activity product and K<sub>sp</sub> the
solubility product for the stated mineral reaction. The ratio is dimensionless.
The boundary SI = 0 corresponds to equality of the product and solubility constant.
A positive index indicates supersaturation; a negative index indicates undersaturation.
This is a thermodynamic boundary, not an operating alarm limit [2, 4].</p>
<div class="eq"><div class="m">c<sub>i</sub> = C<sub>i</sub> / (1000 M<sub>i</sub>);
I = ½ Σ c<sub>i</sub> z<sub>i</sub>²</div></div>
<p>C<sub>i</sub> is an entered ion concentration in mg/L, M<sub>i</sub> is its
molecular mass in g/mol, c<sub>i</sub> is molar concentration in mol/L,
z<sub>i</sub> is signed ionic charge; ionic strength is denoted by I (mol/L).
The unit conversion and one-half weighting are implemented directly [2].
The molecular masses and charges actually used are listed in Appendix 10.1.</p>
<div class="eq"><div class="m">F(T,P,I) = a₀ + a<sub>T</sub>T + a<sub>T²</sub>T²
+ a<sub>T³</sub>T³ + a<sub>P</sub>P + a<sub>√I</sub>√I + a<sub>I</sub>I</div></div>
<p>F is a conditional logarithmic constant; T is temperature in °F and P is pressure
in psia. Each a denotes the coefficient of its associated term, with reciprocal
units required to make F dimensionless. Distinct coefficient sets are used for each
mineral and carbonate route. Their values are read from the implementation in
Appendix 10.1; no coefficient is represented as a value transcribed from a publication.
The implementation uses the conditional-solubility framework [2, 6].</p>
<div class="eq"><div class="m">SI<sub>carbonate,gas</sub> =
log<sub>10</sub>(c<sub>Ca</sub> c<sub>HCO₃</sub>² / f<sub>CO₂</sub>) + F<sub>gas</sub>;
f<sub>CO₂</sub> = y<sub>CO₂</sub>P</div></div>
<p>c<sub>Ca</sub> and c<sub>HCO₃</sub> are calcium and bicarbonate concentrations
in mol/L. f<sub>CO₂</sub> is carbon dioxide fugacity approximated by partial pressure
in psia; y<sub>CO₂</sub> is its gas mole fraction. The unit-specific scaling is
absorbed into F<sub>gas</sub>; pressure units cannot be substituted without recalibration [2].</p>
<div class="eq"><div class="m">pH<sub>estimated</sub> =
log<sub>10</sub>(c<sub>HCO₃</sub> / f<sub>CO₂</sub>) + F<sub>pH-from-gas</sub></div></div>
<p>The estimated in-situ pH uses a separate conditional constant F<sub>pH-from-gas</sub>.
It is a model output, not a measurement at the stated conditions [2].</p>
<div class="eq"><div class="m">SI<sub>carbonate,pH</sub> =
log<sub>10</sub>(c<sub>Ca</sub> c<sub>HCO₃</sub>) + pH − F<sub>pH</sub>;<br>
SI<sub>salt</sub> = log<sub>10</sub>(c<sub>M</sub> c<sub>X</sub>) + F<sub>salt</sub></div></div>
<p>The measured-pH route is used only when a positive gas fraction is unavailable.
For the salt expression, M denotes the mineral cation and X the anion: calcium,
barium or strontium with sulfate, or sodium with chloride. Water activity and
hydration effects are absorbed into the constants [2].</p>
<h3>Inputs and outputs</h3><p>A <code>BrineComposition</code> instance accepts the listed ions, optional total
dissolved solids, measured pH and gas carbon dioxide mole fraction. A carbonate route
is required even when only sulfate results are of interest. Negative concentrations,
non-positive pressure and out-of-range gas or mixing fractions are rejected; finite
values, charge balance and coefficient applicability require separate checks [2, 3].</p>
<p>The <code>saturation_indices</code> single-state output gives the index, logarithmic concentration product,
conditional constant, method basis and route details for every mineral. The <code>si_profile</code>
output gives an ordered table of temperature, pressure, each mineral index and its
change from the first state. A missing participating ion produces a non-finite index,
which means not evaluated rather than a zero tendency [2].</p>'''
    results = f'''<p>The endpoint results are calculated with the supplied default
coefficients and the public synthetic water. Every result in Table A is obtained by
calling the module; a field acceptance decision is not established [1, 2].</p>
<p>The computed ionic strength is {n('ionic_strength', 3)} mol/L.
Calcite saturation index changes from {n(start)} to {n(end)}, against the saturation
boundary of {n('criterion')}. The wellhead change relative to the bottomhole is
{n(f'profile.{last}.dsi_calcite')} index units. These are screening results from
uncalibrated coefficients [2].</p>
<p>At the wellhead, the carbon dioxide partial pressure used by the module is
{n(f'states.{last}.calcite.details.f_co2_psia')} psia.
The logarithmic concentration term is {n(f'states.{last}.calcite.log10_iap')},
and the conditional term is {n(f'states.{last}.calcite.pk_cond')}.
Their unrounded sum gives the calcite index {n(end)}. The estimated in-situ pH is
{n(f'states.{last}.calcite.details.estimated_ph')}; it is derived from the same
uncalibrated model, not an independent measurement [2].</p>
{result_table}
<figure class="card"><label for="mineral">Mineral <select id="mineral"></select></label>
<div id="profile-chart"></div><p id="chart-detail" aria-live="polite"></p>
<figcaption class="fig-cap">Figure B. Saturation index versus absolute pressure along
the specified thermal path [1, 2]. The saturation boundary SI = 0 is drawn. Each point
reports mineral, pressure, temperature, demand and boundary; keyboard focus and hover
show the same information. Lines connect sampled states and do not locate a continuous
crossing. Temperature changes with pressure, so the curve does not isolate pressure effects.</figcaption></figure>
<p>The complete path results are tabulated in Appendix 10.2. The selected points do
not establish a maximum between samples or an operating pressure limit.</p>'''
    findings = f'''<p>The calcite index at the synthetic wellhead is {n(end)}, above the
saturation boundary of {n('criterion')}. The conditional model therefore indicates
supersaturation at that state; deposited quantity, onset time and treatment demand
are not established [2, 4].</p>
<p>The existing suite contains {n('evidence.test_count', 0)} automated checks [3].
Arithmetic checks cover ionic strength, coefficient overrides, profile order,
changes from the first state and mixing endpoint identity. Qualitative checks cover
temperature trends, depressurization, calcium sulfate phase ordering, halite tendency
and an interior barite mixing peak. Other checks address input rejection and result
contracts. These checks support calculation consistency and expected trends; they
provide no measured error bound for the default coefficients [2, 3].</p>
<p>The published explorer provides a reproducible example [1]. An independent
experimental benchmark dataset, calibration residuals and a demonstrated domain of
quantitative accuracy are not supplied by the inspected chemistry module, its tests
or its example. Quantitative design suitability remains not established.</p>'''
    limitations = '''<p>Use is restricted by coefficient provenance and the simplified
chemical representation. The default coefficient sets are described by the
implementation as configurable approximations rather than verified published
correlation values; calibration is required before quantitative use [2].</p>
<p>Molar concentration is treated as interchangeable with molality. Density,
ion pairing, explicit activity coefficients, charge-balance correction and water
activity are not resolved. The high-salinity public example therefore carries an
unquantified chemical-model error. The supplied total dissolved solids does not
replace the individual ions in the ionic-strength calculation [2].</p>
<p>The gas mole fraction is fixed, fugacity is approximated by partial pressure,
and composition is unchanged along the path. Gas liberation, nucleation, precipitation,
redissolution, residence time and deposition kinetics are not solved. Neither an
inhibitor dose nor an operating alarm limit is available from this result [2].</p>
<p>Temperature, pressure and salinity applicability envelopes are NOT_EVALUATED:
no calibrated bounds are supplied. Uncertainty, physical dimensions, hydraulic
conditions and observed deposit records are also NOT_EVALUATED. Those gaps are
recorded with their consequences in Appendix 10.4.</p>'''
    recommendations = '''<p>Further assessment is directed at the evidence gaps that
prevent quantitative design use. The following actions address the supersaturation
screen and the limitations of its chemical basis [2, 5].</p>
<p>For calcite and every other mineral with a positive wellhead result in Table A,
obtain representative water and gas analyses at the relevant conditions and assess
the thermal and pressure path with a qualified speciation basis. Use those results
to define laboratory conditions for scale and inhibitor assessment before setting
a treatment requirement. The current positive index alone does not size a treatment [2, 5].</p>
<p>Replace the default coefficients with a documented calibration dataset and a
stated temperature, pressure and composition envelope. Retain independent reference
cases outside the fit and record errors against an agreed quantitative criterion.
The implementation permits coefficient replacement, but that capability does not
establish calibration or accuracy [2, 3].</p>
<p>Define operating triggers from qualified chemistry and observed deposit or
performance evidence. No numerical pressure restriction, sampling interval or
inhibitor dosage is specified here because its required basis is missing.
For concentrated brines, select an activity model and thermodynamic dataset suited
to the intended composition, with their own limitations assessed [2, 5].</p>'''
    references = f'''<p>The equations are tied to the implementation and the cited public
geochemical definition. Numeric example inputs are taken solely from the existing
public repository example; external sources do not qualify the shipped coefficients.</p>
<h3>9.1 Technical sources</h3>
<ol><li id="ref-1">[1] Public mineral-scale explorer, repository example, revision at
preparation. The formation water and production path are explicitly synthetic.
The source locator and content record are given in Section 9.2.</li>
<li id="ref-2">[2] Production chemistry implementation, revision at preparation:
composition, conditional coefficients, carbonate routes, state and profile functions,
and documented assumptions. Numeric provenance is the implementation, not an
independently qualified solubility dataset. See Section 9.2.</li>
<li id="ref-3">[3] Production chemistry tests, revision at preparation. Derivation
and behaviour checks are distinguished in the source. See Section 9.2.</li>
<li id="ref-4">[4] <a href="https://pubs.usgs.gov/publication/sir20225094/full">Groundwater
quality and geochemistry of the western wet gas part of the Marcellus Shale Oil and Gas
Play in West Virginia</a>, Scientific Investigations Report 2022–5094 (2022),
“Geochemical Modeling”, saturation-index definition. The activity-product definition
supports the sign criterion, not the coefficient values used here.</li>
<li id="ref-5">[5] <a href="https://water.usgs.gov/water-resources/software/PHREEQC/documentation/phreeqc3-html/phreeqc3-2.htm">Description
of Input and Examples for PHREEQC Version 3</a>, Techniques and Methods, book 6,
chapter A43 (2013), “Introduction”, “Capabilities” and “Program Limitations”.
Aqueous activities, speciation and database consistency are relevant to qualification.
The referenced program has not been executed for this analysis.</li>
<li id="ref-6">[6] <a href="https://doi.org/10.2118/21710-PA">Why Scale Forms in the Oil Field and Methods To Predict It</a>,
SPE Production &amp; Facilities (1994), SPE-21710-PA.
The method is cited by the implementation as its conditional-solubility framework.
The supplied coefficient values are not transcribed from this paper [2].</li></ol>
<h3>9.2 Reproduction records</h3>
<p>The authoritative calculation and example records are listed below. Each content
digest is retained inside this HTML's machine-readable data record. Source digests
cover UTF-8 text with LF-normalized line endings. Embedded numerical values retain
twelve significant figures before serialization and document hashing. The report is
reproducible without network access once its Python dependencies are installed.</p>
{table(('Record', 'Repository locator', 'Used for'), [
('[1]', f'<code>{EXAMPLE}</code>', 'Synthetic composition and thermal-pressure path'),
('[2]', f'<code>{IMPLEMENTATION}</code>', 'Equations, molecular masses, coefficients and outputs'),
('[3]', f'<code>{TESTS}</code>', 'Existing calculation evidence'),
('Presentation', '<code>docs/domains/cathodic_protection/reports/cases/S01/report.html</code>', 'House calculation-report layout exemplar'),
('Stylesheet', f'<code>{HOUSE}</code> and <code>{ENGINE_CSS}</code>', 'Canonical inlined house presentation'),
('Rebuild', '<code>scripts/production_chemistry/build_module_report.py</code>', 'Deterministic document and change panel'),
('Numerical check', '<code>tests/production_chemistry/test_module_report.py</code>', 'Rebuild comparison and direct module readback'),
], 'Table B. Source locators; the chemistry evidence is limited to the records listed.')}
<div class="prose"><pre><code>PYTHONPATH=src uv run python scripts/production_chemistry/build_module_report.py
PYTHONPATH=src uv run python -m pytest tests/production_chemistry</code></pre></div>'''
    return [summary, introduction, design, methodology, results, findings,
            limitations, recommendations, references, appendix(data)]


def appendix(data: dict[str, Any]) -> str:
    """Keep large data tables, limitations and quality evidence in the appendix."""
    n = lambda key, places=3: number(data, key, places)
    ion_rows = [(ION_NAMES[ion], n(f"inputs.brine.{ion}"), "[1], synthetic")
                for ion in ION_PROPERTIES]
    ion_rows.append(("Total dissolved solids (supplied)", n("inputs.brine.tds"), "[1], synthetic"))
    constants = [(ION_NAMES[ion], n(f"ions.{ion}.molecular_mass"),
                  n(f"ions.{ion}.charge", 0), "[2], ion properties") for ion in ION_PROPERTIES]
    terms = tuple(next(iter(data["coefficients"].values())))
    coefficient_rows = [(key, *(n(f"coefficients.{key}.{term}", -1) for term in terms),
                         "[2], default coefficients") for key in data["coefficients"]]
    profile_rows = [(n(f"profile.{i}.t_f", 1), n(f"profile.{i}.p_psia", 1),
                     *(n(f"profile.{i}.si_{family}", 2) for family in data["families"]))
                    for i in range(len(data["profile"]))]
    return f'''<p>The calculation inputs, coefficient provenance and unresolved data are
recorded below. The data register distinguishes assumptions from quantities that
have not been evaluated.</p>
<h3>10.1 Input and constant register</h3>
{table(('Ion or property', 'Concentration (mg/L)', 'Source'), ion_rows,
'Table C. Synthetic public-example inputs; each value is read from the example [1].')}
{table(('Ion', 'Molecular mass (g/mol)', 'Charge (dimensionless)', 'Source'), constants,
'Table D. Molecular masses and ionic charges used by the implementation [2]. Their source here is code; an independent reference audit is not supplied.')}
{table(('Correlation', *terms, 'Source'), coefficient_rows,
'Table E. Implementation coefficient values [2], shown to six significant figures in scientific notation. '
'Column terms are defined in Section 4; their units cancel their temperature, pressure '
'and ionic-strength factors. These values are not qualified publication values.')}
<h3>10.2 Complete profile</h3>
{table(('Temperature (°F)', 'Pressure (psia)', *(f'{f.capitalize()} index (dimensionless)' for f in data['families'])),
profile_rows, 'Table F. Computed sampled profile [1, 2]. Every value is read from the module profile output. Indices are printed to two decimals to retain non-zero values near the zero saturation boundary [2, 4].')}
<h3>10.3 Calculation evidence and quality pre-flight</h3>
<p>The existing suite comprises {n('evidence.test_count', 0)} checks [3].
The arithmetic and behavioural evidence is described in Section 6.
The rebuild check compares the complete document with its generated form,
compares all state and profile values with direct module calls, and reads back
every annotated printed number against its computed origin.
The literal worksheet at
<code>tests/production_chemistry/fixtures/module_report_hand_calculation.json</code>
records separate decimal arithmetic for all seven minerals at both endpoints,
including ionic strength, conditional constants, logarithmic products and the
gas carbon dioxide route. Endpoint and printed-coefficient tests use these fixed
expectations without calculating expected values through the chemistry interface.
This establishes numerical reproduction and independent arithmetic agreement; physical validation is
NOT_EVALUATED.</p>
<p>Quality pre-flight covers section and reference integrity, input units, the
saturation boundary, source records, reproducibility and the distinction between
synthetic inputs and measured data. The chart uses the same embedded profile as
Table F. Table precision is selected for readable screening results; arrays retain
twelve significant figures for numerical readback. The saturation criterion is not an
acceptance tolerance for the chemical model.</p>
<h3>10.4 Data register and symbols</h3>
{table(('Item', 'Status and consequence', 'Required evidence or disposition'), [
('Water composition, gas fraction and path', 'ASSUMED - to be confirmed. Synthetic public example; no service-specific decision is established.', 'Representative samples and operating-condition records [1, 2].'),
('Default coefficients', 'ASSUMED - to be confirmed. Quantitative errors and applicability are unbounded.', 'Calibrated coefficients, independent cases and stated error criteria [2].'),
('Pressure, temperature and salinity envelope', 'NOT_EVALUATED. No quantitative operating domain is established.', 'Documented calibration bounds and extrapolation rules [2].'),
('Charge balance, density and species activities', 'NOT_EVALUATED. Composition consistency and activity-model errors are unquantified.', 'Complete analysis, density and a qualified chemical representation [2, 5].'),
('Deposited mass, kinetics and inhibitor dosage', 'NOT_EVALUATED. Treatment quantities cannot be calculated.', 'Qualified deposition and treatment assessment [2, 5].'),
('Equipment dimensions and flow conditions', 'NOT_EVALUATED. Hydraulic and deposition-location decisions are unavailable.', 'Geometry, flow rates and thermal-pressure records [2].'),
('Independent physical benchmark', 'NOT_EVALUATED. Numerical reproduction does not establish physical accuracy.', 'Independent reference observations and a comparison criterion [2, 3].'),
], 'Table G. Missing data, governing consequences and evidence required for quantitative use.')}
<p>SI denotes saturation index; I, ionic strength; Q, activity product;
K<sub>sp</sub>, solubility product; C, mass concentration; c, molar concentration;
M, molecular mass or, as a subscript in the salt equation, the mineral cation;
z, ionic charge; T, temperature; P, absolute pressure; F, conditional logarithmic
constant; a, polynomial coefficient; f, gas fugacity; y, gas mole fraction;
CO₂, carbon dioxide; Ca, calcium; HCO₃, bicarbonate. Indices are dimensionless.
The species and unit conventions are those used by the implementation [2].</p>'''


def build(output_dir: Path) -> None:
    """Build only the two named report outputs in the requested directory."""
    data = build_data()
    bodies = render_body(data)
    body = "\n".join(section(i, b) for i, b in enumerate(bodies, 1))
    house = (ROOT / HOUSE).read_text(encoding="utf-8")
    match = re.search(r"<style>(.*?)</style>", house, re.S)
    if match is None:
        raise ValueError("The canonical report stylesheet is missing")
    style = match.group(1)
    style += (ROOT / ENGINE_CSS).read_text(encoding="utf-8")
    # Functional overflow and interaction rules retain the house tokens and layout.
    style += '''\n.tbl-wrap{max-width:100%;overflow-x:auto}.main{min-width:0}
    .hs-doc svg{max-width:100%}.hs-doc label{display:block;margin:12px 0}
    .hs-doc textarea{display:block;width:100%;min-height:90px}
    .hs-doc button,.hs-doc select,.hs-doc input{max-width:100%;font:inherit}
    .hs-doc svg .point:focus{outline:3px solid var(--proj)}
    @media print{.comment-controls,#change-panel{display:none}}'''
    digest = hashlib.sha256((style + body + json.dumps(data, sort_keys=True)).encode("utf-8")).hexdigest()
    data["document_digest"] = digest
    toc = "".join(f'<li><a href="#section-{i}">{title}</a></li>'
                  for i, title in enumerate(TITLES, 1))
    script = (ROOT / SCRIPT).read_text(encoding="utf-8")
    output = f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="description" content="Production chemistry mineral-scale screening analysis and limits of quantitative use">
<title>Production chemistry module analysis</title><style>{style}</style></head>
<body class="doc hs-doc"><header class="masthead"><div class="wrap">
<span class="brand">digitalmodel</span><span class="docno">Revision {REVISION}</span>
</div></header><div class="hero"><div class="wrap"><div class="kicker">Engineering analysis</div>
<h1>Production chemistry module analysis</h1><p class="lede">Mineral-scale screening along a production string</p>
<p>Draft for maintainer review</p></div></div>
<div class="wrap shell"><nav class="toc" aria-label="Contents"><p class="tt">Contents</p>
<ol>{toc}</ol><details id="change-panel"><summary>Changes</summary><div id="changes"></div></details></nav>
<main class="main">{body}
<div class="card comment-controls"><h2>Document comments</h2>
<p>Comments remain in this document until saved. Save the comments file beside this HTML.
Where direct file saving is unavailable, a download is offered; place that file beside the report.</p>
<label>Section <select id="comment-section"></select></label>
<button id="capture-quote" type="button">Capture selected text</button>
<label>Quoted text <textarea id="comment-quote" readonly></textarea></label>
<label>Comment <textarea id="comment-text" maxlength="10000"></textarea></label>
<button id="add-comment" type="button">Add comment</button>
<button id="save-comments" type="button">Save comments beside report</button>
<label>Load saved comments <input id="load-comments" type="file" accept="application/json"></label>
<p id="comment-message" role="status"></p><ol id="comment-list"></ol></div>
</main></div>
<script type="application/json" id="report-data">{json.dumps(data, sort_keys=True, allow_nan=False)}</script>
<script type="application/json" id="report-changes">{json.dumps(CHANGES, sort_keys=True)}</script>
<script>{script}</script></body></html>\n'''
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "index.html").write_text(output, encoding="utf-8", newline="\n")
    (output_dir / "changes.json").write_text(json.dumps(CHANGES, indent=2) + "\n", encoding="utf-8", newline="\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=DESTINATION)
    build(parser.parse_args().output_dir)
