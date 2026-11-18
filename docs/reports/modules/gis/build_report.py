# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the self-contained GIS coordinate transformation module report.

Rebuild from the repository root with PYTHONPATH=src and the GIS dependencies:
    python docs/reports/modules/gis/build_report.py
"""
from __future__ import annotations

import argparse
import html
import json
import re
from pathlib import Path

from digitalmodel.gis import CoordinateTransformer
from digitalmodel.gis.core.crs import CRSDefinition

ROOT = Path(__file__).resolve().parents[4]
HERE = Path(__file__).resolve().parent
REVISION = "00"
DATE = "2026-10-21"
EXEMPLAR = "docs/reports/pipeline-ffs-assessment.html"
REBUILD = "PYTHONPATH=src python docs/reports/modules/gis/build_report.py"

CHANGES = {
    "report": "gis-coordinate-transformation",
    "revision": REVISION,
    "date": DATE,
    "changes": [
        "Established the geographic-to-map coordinate example and its inverse so that axis order and units are explicit.",
        "Added analytic controls, published reference coordinates and the polar singularity to separate numerical agreement from suitability for engineering measurement.",
    ],
}


def calculate() -> dict:
    """Call the public module, keeping inputs, outputs and plot samples together."""
    forward = CoordinateTransformer("EPSG:4326", "EPSG:3857")
    inverse = CoordinateTransformer("EPSG:3857", "EPSG:4326")
    longitude, latitude = 2, 49  # PROJ 9.5, Web Mercator, Usage example.
    easting, northing = forward.transform_point(longitude, latitude)
    recovered_lon, recovered_lat = inverse.transform_point(easting, northing)
    quarter_easting, _ = forward.transform_point(90, 0)
    _, mid_northing = forward.transform_point(0, 45)
    # PROJ 9.5 Ellipsoids / Built-in ellipsoids gives this same semi-major axis.
    radius = CRSDefinition.from_epsg(3857).to_pyproj().ellipsoid.semi_major_metre
    # Integer-degree samples are a plotting choice, not a design criterion.
    latitudes = list(range(90))
    _, northings = forward.transform_points([longitude] * len(latitudes), latitudes)
    return {
        "basis": {"radius_m": radius, "polar_limit_deg": 90},
        "example": {
            "longitude_deg": longitude,
            "latitude_deg": latitude,
            "easting_m": easting,
            "northing_m": northing,
            "recovered_longitude_deg": recovered_lon,
            "recovered_latitude_deg": recovered_lat,
        },
        "anchors": {"quarter_longitude_deg": 90, "quarter_latitude_deg": 0,
                    "quarter_easting_m": quarter_easting,
                    "mid_longitude_deg": 0, "mid_latitude_deg": 45,
                    "mid_northing_m": mid_northing},
        "curve": [{"latitude_deg": lat, "northing_m": round(y, 2)}
                  for lat, y in zip(latitudes, northings)],
    }


def rounded(value):
    """Embed calculated coordinates at centimetres and inverse angles at degrees."""
    if isinstance(value, dict):
        return {key: (round(item, 8) if key.endswith("_deg") else rounded(item))
                for key, item in value.items()}
    if isinstance(value, list):
        return [rounded(item) for item in value]
    if isinstance(value, float):
        return round(value, 2)
    return value


def lookup(record: dict, locator: str):
    """Resolve a dotted numeric binding."""
    value = record
    for key in locator.split("."):
        value = value[int(key)] if isinstance(value, list) else value[key]
    return value


def display(value, digits: int) -> str:
    """Use fixed displayed precision and a true minus sign."""
    return f"{value:.{digits}f}".replace("-", "−")


def table(headers, rows, caption="") -> str:
    """Render structured columns; callers escape prose cells."""
    return ('<div class="table-wrap"><table><thead><tr>'
            + "".join(f'<th scope="col">{html.escape(h)}</th>' for h in headers)
            + "</tr></thead><tbody>"
            + "".join("<tr>" + "".join(f"<td>{cell}</td>" for cell in row) + "</tr>" for row in rows)
            + "</tbody>" + (f"<caption>{caption}</caption>" if caption else "")
            + "</table></div>")


def chart(record: dict) -> str:
    """Draw module samples with pointer and keyboard-accessible hover details."""
    width, height = 700, 330
    left, top, plot_width, plot_height = 90, 25, 560, 245
    max_northing = max(c["northing_m"] for c in record["curve"])
    def position(case):
        return (left + plot_width * case["latitude_deg"] / record["basis"]["polar_limit_deg"],
                top + plot_height * (1 - case["northing_m"] / max_northing))
    points = " ".join(f"{x:.2f},{y:.2f}" for x, y in map(position, record["curve"]))
    circles = []
    for case in record["curve"]:
        x, y = position(case)
        label = (f'Case: latitude {case["latitude_deg"]}°; Component: projection; '
                 f'Northing: {display(case["northing_m"], 2)} m; '
                 f'Limit: pole at {record["basis"]["polar_limit_deg"]}°, no finite northing.')
        circles.append(f'<circle cx="{x:.2f}" cy="{y:.2f}" r="3" tabindex="0" '
                       f'aria-label="{label}" data-detail="{label}"><title>{label}</title></circle>')
    ticks = []
    for lat in (0, 30, 60, record["basis"]["polar_limit_deg"]):
        x = left + plot_width * lat / record["basis"]["polar_limit_deg"]
        ticks.append(f'<text x="{x:.1f}" y="290" text-anchor="middle">{lat}</text>')
    for fraction in (0, 0.5, 1):
        y = top + plot_height * (1 - fraction)
        ticks.append(f'<text x="82" y="{y:.1f}" text-anchor="end">{display(max_northing * fraction, 0)}</text>')
    return f'''<figure id="northing-figure"><svg viewBox="0 0 {width} {height}"
 role="img" aria-labelledby="chart-title chart-description">
<title id="chart-title">Map northing against latitude</title>
<desc id="chart-description">Northing increases without bound towards the pole.
Focus or hover over a sample for its case, component, northing, limit and units.</desc>
<path d="M90 25 V270 H650" fill="none" stroke="currentColor"/>
<polyline points="{points}" fill="none" stroke="var(--report-accent)" stroke-width="2"/>
<g fill="var(--report-accent)">{''.join(circles)}</g>
<path id="polar-limit" d="M650 25 V270" stroke="var(--ink)" stroke-dasharray="6 4"/>
<g font-size="12">{''.join(ticks)}
<text x="370" y="321" text-anchor="middle">Latitude (degrees north)</text>
<text x="25" y="150" text-anchor="middle" transform="rotate(-90 25 150)">Northing (m)</text></g>
</svg><figcaption>Figure 2. Coordinate samples are calculated by the module.
The dashed line marks the pole, where no finite northing exists [<a href="#ref-1">1</a>].
It is a mathematical boundary, not an accuracy or operating limit.</figcaption>
<p id="chart-readout" aria-live="polite">Focus or hover over a point to read its coordinates.</p></figure>'''


INTERACTION = r'''
<script>
"use strict";
for (const point of document.querySelectorAll("circle[data-detail]")) {
  const show = () => { document.getElementById("chart-readout").textContent = point.dataset.detail; };
  point.addEventListener("pointerenter", show);
  point.addEventListener("focus", show);
}
(() => {
  const el = id => document.getElementById(id);
  const reportId = "gis-coordinate-transformation";
  const revision = "00";
  let directory = null;
  let state = {report: reportId, revision, comments: []};
  const message = text => { el("comment-status").textContent = text; };
  const valid = value => value && value.report === reportId && value.revision === revision &&
    Array.isArray(value.comments) && value.comments.every(c => c &&
      typeof c.text === "string" && typeof c.section === "string" &&
      Boolean(document.getElementById(c.section)));
  const render = () => {
    const list = el("comment-list");
    list.replaceChildren();
    for (const comment of state.comments) {
      const item = document.createElement("li");
      item.textContent = comment.section.replace("section-", "Section ") + ": " + comment.text;
      list.appendChild(item);
    }
  };
  const mergeComments = comments => [...new Map(comments.map(comment =>
    [JSON.stringify([comment.section, comment.text]), comment])).values()];
  el("select-folder").addEventListener("click", async () => {
    try {
      if (!window.showDirectoryPicker) {
        message("Folder saving is unavailable. Export comments and save the file beside this report.");
        return;
      }
      const selected = await window.showDirectoryPicker({mode: "readwrite"});
      const reportHandle = await selected.getFileHandle("report.html");
      const reportText = await (await reportHandle.getFile()).text();
      if (!reportText.includes('id="gis-coordinate-transformation" data-revision="00"'))
        throw new Error("Select the folder containing this report revision.");
      let loaded = {report: reportId, revision, comments: []};
      try {
        const handle = await selected.getFileHandle("report.comments.json");
        loaded = JSON.parse(await (await handle.getFile()).text());
        if (!valid(loaded)) throw new Error("Comments do not match this report revision.");
      } catch (error) { if (error.name !== "NotFoundError") throw error; }
      directory = selected;
      state = {...loaded, comments: mergeComments([...loaded.comments, ...state.comments])};
      render();
      message("Report folder selected. Save writes comments beside the report.");
    } catch (error) { message(error.message); }
  });
  el("add-comment").addEventListener("click", () => {
    const text = el("comment-text").value.trim();
    if (!text) { message("Enter a comment before adding it."); return; }
    state.comments.push({section: el("comment-section").value, text});
    el("comment-text").value = "";
    render();
    message("Comment added in memory. Save or export before closing.");
  });
  el("save-comments").addEventListener("click", async () => {
    try {
      if (!directory) { message("Select the report folder first, or export comments."); return; }
      const handle = await directory.getFileHandle("report.comments.json", {create: true});
      const writer = await handle.createWritable();
      await writer.write(JSON.stringify(state, null, 2) + "\n");
      await writer.close();
      message("Comments saved beside the report.");
    } catch (error) { message(error.message); }
  });
  el("export-comments").addEventListener("click", () => {
    const url = URL.createObjectURL(new Blob([JSON.stringify(state, null, 2) + "\n"], {type: "application/json"}));
    const link = document.createElement("a");
    link.href = url; link.download = "report.comments.json"; link.click();
    setTimeout(() => URL.revokeObjectURL(url), 1000);
    message("Save the exported comments beside the report.");
  });
  el("import-comments").addEventListener("change", async event => {
    try {
      const file = event.target.files[0];
      if (!file) return;
      const incoming = JSON.parse(await file.text());
      if (!valid(incoming)) throw new Error("Comments do not match this report revision.");
      state.comments = mergeComments([...state.comments, ...incoming.comments]); render();
      message("Comments loaded. Save or export before closing.");
    } catch (error) { message(error.message); }
  });
})();
</script>
'''


def build(record: dict) -> str:
    """Write the complete engineering narrative using one calculation record."""
    def n(key, digits=2):
        return (f'<span data-value="{key}" data-digits="{digits}">'
                f'{display(lookup(record, key), digits)}</span>')
    def ref(number):
        return f'[<a href="#ref-{number}">{number}</a>]'
    exemplar = (ROOT / EXEMPLAR).read_text(encoding="utf-8")
    style = re.search(r"<style>(.*?)</style>", exemplar, re.S)[1]
    revisions = table(["Revision", "Date", "What changed and why"], [[REVISION, DATE,
        "First issue: documented coordinate transformation, analytic controls and limits for map use."]])
    worked = table(["Quantity", "Value", "Unit", "Source"], [
        ["Input longitude", n("example.longitude_deg"), "degrees east", ref(2)],
        ["Input latitude", n("example.latitude_deg"), "degrees north", ref(2)],
        ["Calculated easting", n("example.easting_m"), "m", "Module calculation; " + ref(2)],
        ["Calculated northing", n("example.northing_m"), "m", "Module calculation; " + ref(2)],
        ["Inverse longitude", n("example.recovered_longitude_deg"), "degrees east", "Module inverse calculation"],
        ["Inverse latitude", n("example.recovered_latitude_deg"), "degrees north", "Module inverse calculation"],
    ], "Table 1. Public example inputs and calculated outputs.")
    anchors = table(["Control", "Longitude (degrees)", "Latitude (degrees)", "Result (m)", "Source"], [
        ["Quarter-equator easting", n("anchors.quarter_longitude_deg", 0), n("anchors.quarter_latitude_deg", 0), n("anchors.quarter_easting_m"), ref(1)],
        ["Midlatitude northing", n("anchors.mid_longitude_deg", 0), n("anchors.mid_latitude_deg", 0), n("anchors.mid_northing_m"), ref(1)],
    ], "Table 2. Analytic controls calculated through the same public entry point.")
    locators = [
        ("Public coordinate interface and docstrings", "src/digitalmodel/gis/core/coordinate_transformer.py"),
        ("Coordinate reference definitions", "src/digitalmodel/gis/core/crs.py"),
        ("Point distances and bounding boxes", "src/digitalmodel/gis/core/geometry.py"),
        ("Tabular feature interface", "src/digitalmodel/gis/layers/feature_layer.py"),
        ("Existing coordinate checks", "tests/gis/test_coordinates.py"),
        ("Existing geometry and reference-system checks", "tests/gis/test_crs_and_spatial.py"),
        ("Existing point-file round trips", "tests/gis/test_geojson_roundtrip.py"),
        ("Existing spatial-query checks", "tests/gis/test_spatial_queries.py"),
        ("Existing feature and well-layer checks", "tests/gis/test_feature_layer.py"),
        ("Report anchors and consistency checks", "tests/gis/test_module_report.py"),
        ("Report generator and documented rebuild", "docs/reports/modules/gis/build_report.py"),
        ("Public-source access record", "docs/reports/modules/gis/sources.json"),
        ("Layout exemplar", EXEMPLAR),
    ]
    locator_table = table(["Record", "Repository locator"], [[html.escape(title),
        f'<a href="../../../../{path}">{path}</a>'] for title, path in locators])
    register = table(["Item", "Basis", "Status and consequence", "Source"], [
        ["Example location", "Published projection input; no physical asset is represented.", "Documented public example.", ref(2)],
        ["Projection radius", n("basis.radius_m", 0) + " m; ellipsoid semi-major axis used as the projection sphere radius.", "Defined projection parameter, not a mean Earth radius.", ref(3)],
        ["Coordinate epoch and positional uncertainty", "Required input for a site survey.", "NOT_EVALUATED. No survey acceptance is assigned.", "No site data supplied."],
        ["Vertical datum and height transformation", "Required input for elevations.", "NOT_EVALUATED. Only horizontal coordinates are evaluated.", "No vertical reference supplied."],
        ["Engineering distance tolerance", "Required input tied to the intended measurement.", "NOT_EVALUATED. Map metres are not accepted as ground metres.", ref(1) + " " + ref(2)],
        ["Chart sampling", "Integer-degree samples below the pole.", "Plotting choice; no interpolation accuracy is claimed.", "Calculated sample record embedded in this document."],
    ], "Table 3. Data register. No substitute site values are assumed.")
    contents = ["Executive Summary", "Introduction", "Data Basis", "Theory and Equations", "Worked Example",
                "Validation Evidence", "Limits of Use", "Recommendations", "References", "Appendix"]
    nav = "".join(f'<li><a href="#section-{i}">{title}</a></li>' for i, title in enumerate(contents, 1))
    comment_options = "".join(f'<option value="section-{i}">{title}</option>' for i, title in enumerate(contents, 1))
    changes = "".join(f"<li>{html.escape(change)}</li>" for change in CHANGES["changes"])
    return f'''<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>GIS module analysis: coordinate transformation</title>
<style>{style}</style>
<style>
button,input,select,textarea{{font:inherit;max-width:100%}}label{{display:block;margin:12px 0}}
textarea{{display:block;width:100%;min-height:100px}}button{{margin:5px;padding:8px}}
:focus-visible{{outline:3px solid var(--report-accent);outline-offset:3px}}
circle[data-detail]:focus,circle[data-detail]:hover{{r:6;stroke:var(--ink);stroke-width:2}}
pre{{white-space:pre-wrap;overflow-wrap:anywhere}}.formula{{font-family:monospace}}
@media print{{button,input,select,textarea,#comment-list{{display:none}}}}
</style></head>
<body class="hs-doc" id="gis-coordinate-transformation" data-revision="{REVISION}">
<header><h1>GIS module analysis: coordinate transformation</h1>
<p>Draft for maintainer review</p><p>Revision {REVISION} · {DATE}</p></header>
<div class="layout"><nav aria-label="Contents"><ol>{nav}</ol></nav><main>
<section id="section-1"><h2>1. Executive Summary</h2>
<p>The geographic information system (GIS) module is a set of coordinate, geometry, layer and exchange tools.
Analysis is performed to determine whether its horizontal coordinate interface reproduces a documented map example;
key results are summarized below.</p>
<ul><li>A published map coordinate is reproduced at centimetre display precision.
<ul><li>The input at {n("example.longitude_deg", 0)}° east and {n("example.latitude_deg", 0)}° north
is mapped to easting {n("example.easting_m")} m and northing {n("example.northing_m")} m {ref(2)}.</li></ul></li>
<li>The example supports map preparation within the stated horizontal-coordinate scope.
<ul><li>The pole is excluded: no finite northing exists at {n("basis.polar_limit_deg", 0)}° latitude {ref(1)}.
A survey reference and measurement tolerance remain required before engineering distances are accepted.</li></ul></li></ul>
</section>
<div id="revision-history"><h3>Revision history</h3>{revisions}</div>
<section id="section-2"><h2>2. Introduction</h2>
<p>Geographic positions are transformed into map coordinates by the public coordinate interface.
The main workflow is demonstrated from an explicit source reference through forward and inverse transformation.</p>
<p>The package also provides point geometry, spatial searches, tabular feature and well layers, raster and temporal layers,
file exchange, mapping exports and imagery preparation. These facilities support locating and exchanging engineering data;
structural capacity, asset condition and survey quality are not calculated by this example.</p>
<figure><svg viewBox="0 0 700 280" role="img" aria-labelledby="schematic-title schematic-description">
<title id="schematic-title">Geographic input and map-plane output</title>
<desc id="schematic-description">A curved meridian and an equator represent the geographic surface.
The input is passed to a rectangular map plane with easting and northing axes. The drawing is not to scale.</desc>
<ellipse cx="160" cy="125" rx="95" ry="95" fill="none" stroke="currentColor"/>
<ellipse cx="160" cy="125" rx="35" ry="95" fill="none" stroke="var(--report-accent)"/>
<path d="M65 125 Q160 160 255 125 M160 30 V220" fill="none" stroke="currentColor" stroke-dasharray="4 4"/>
<circle cx="184" cy="65" r="5" fill="var(--report-accent)"/>
<text x="35" y="250">Geographic surface: longitude, latitude</text>
<text x="180" y="26">North pole</text><text x="35" y="116">Equator</text>
<path d="M280 125 H395 L382 116 M395 125 L382 134 M440 45 V220 H660" fill="none" stroke="currentColor"/>
<path d="M440 90 H540 V220" fill="none" stroke="var(--report-accent)" stroke-dasharray="4 4"/>
<circle cx="540" cy="90" r="5" fill="var(--report-accent)"/>
<text x="450" y="30">Northing</text><text x="600" y="245">Easting</text>
<text x="464" y="115">Mapped input</text></svg>
<figcaption>Figure 1. Coordinate model and axis convention. Geometry is schematic, not a site plan.
The projection sphere is based on the ellipsoid semi-major axis {ref(2)} {ref(3)}.</figcaption></figure>
</section>
<section id="section-3"><h2>3. Data Basis</h2>
<p>The input basis is taken from the public Web Mercator example and its reference definition.
Numerical agreement is assessed against the precision printed in that example; engineering suitability is assessed separately.</p>
<p>Source positions use the World Geodetic System 1984 geographic reference. Target positions use its Web Mercator map reference.
The European Petroleum Survey Group registry identifiers are given in the interface record in Section 9.2.
The interface fixes horizontal order as longitude then latitude on input, and easting then northing on output.</p>
<p>The projection radius is {n("basis.radius_m", 0)} m {ref(3)}. The central meridian and the false coordinate offsets are zero {ref(2)}.
The acceptance tolerance for comparison with the published coordinates is half a centimetre, derived from their centimetre rounding;
it is not a survey or map-scale accuracy criterion. The data register in Table 3 identifies inputs needed for site use.</p>
</section>
<section id="section-4"><h2>4. Theory and Equations</h2>
<p>The horizontal mapping is evaluated with spherical Mercator equations using the ellipsoid semi-major axis as radius.
The geographic latitude is used directly, as defined for Web Mercator {ref(2)}.</p>
<p class="formula">x = a(λ − λ₀); y = a ln[tan(π/4 + φ/2)] {ref(1)}</p>
<p>Here x is easting in metres, y is northing in metres, a is the projection radius in metres,
λ is longitude, λ₀ is the central meridian and φ is latitude. Angular arguments are in radians;
π denotes the circular constant. Degree inputs are converted by multiplying by π/180 {ref(1)}.</p>
<p class="formula">λ = x/a + λ₀; φ = π/2 − 2 arctan[exp(−y/a)] {ref(1)}</p>
<p>The inverse recovers longitude and latitude in radians before conversion to degrees.
The equations use zero false offsets for this example {ref(2)}.
The logarithm diverges at either pole; the northern boundary is drawn in Figure 2 {ref(1)}.</p>
<p class="formula">k = 1/cos φ {ref(1)}</p>
<p>The symbol k is local linear scale on the reference sphere. It increases with latitude.
This spherical relation explains map distortion; Web Mercator on an ellipsoidal Earth is not a conformal ground-distance model {ref(2)}.
No ground-distance correction is applied or accepted here.</p>
</section>
<section id="section-5"><h2>5. Worked Example</h2>
<p>The published geographic input is passed to the module and the resulting map position is passed to its inverse.
Every numerical entry in Table 1 is obtained from the calculation record; input coordinates are those of the public example.</p>
{worked}
<p>The easting and northing agree with the published coordinate pair at its printed precision {ref(2)}.
The inverse position is reported in geographic degrees. Agreement of an inverse with its forward calculation is a consistency check;
it does not establish the accuracy of an external survey.</p>
</section>
<section id="section-6"><h2>6. Validation Evidence</h2>
<p>The coordinate calculation is checked against a public example and analytic controls.
Existing repository tests are identified as software evidence for specific behaviours.</p>
<p>The report tests assert the example easting from the degree form of the forward equation,
the quarter-equator easting from a quarter circumference, and the midlatitude northing from the logarithmic equation.
Table 2 displays the supplementary controls; their literal arithmetic and tolerances are recorded in the report test cited in Section 9.2.</p>
{anchors}
<p>Existing coordinate tests cover reference validation, transformation and automatic transverse-Mercator zone selection.
Geometry tests cover zero and known distances, bounding boxes and invalid methods. Spatial-query tests cover radius searches,
ordering, polygon membership and box overlap. Feature tests cover required columns, filtering and point-file round trips.
Those checks do not constitute a survey benchmark or qualify every map reference.</p>
<p>The public example provides a published numerical comparison {ref(2)}.
No measured survey-control dataset or field accuracy campaign is supplied for this report.
The numerical rebuild checks compare displayed coordinates and rounded chart samples with fresh module calls.</p>
{chart(record)}
</section>
<section id="section-7"><h2>7. Limits of Use</h2>
<p>The calculation scope is restricted to horizontal map coordinates and the documented example.
No acceptance of site accuracy, height, distance, area or operational clearance is assigned.</p>
<p>Map metres describe the projected plane. They are unsuitable as uncorrected ground distances away from the projection origin;
the spherical scale relation and the ellipsoidal caveat in Section 4 govern this limitation {ref(1)} {ref(2)}.
The poles remain outside the finite mapping domain. A finite library return value at a pole is not evidence that the singularity is removed.</p>
<p>Source-reference labels, coordinate order and units are caller responsibilities.
The low-level transformer does not request strict error handling from the projection library;
non-finite output and out-of-area inputs require explicit checks in an application.
A horizontal transformation with an added height does not establish a vertical datum transformation.</p>
<p>The geometry and spatial-query interfaces also need care: Euclidean distances use coordinate units,
spherical searches expect geographic degrees, and bounding boxes are axis-aligned.
A box spanning the date line requires an explicit geographic treatment.
Automatic transverse-Mercator zone selection uses regular longitude bands; special-zone and polar use requires a qualified reference choice.
These implementation limits are located in Section 9.2.</p>
</section>
<section id="section-8"><h2>8. Recommendations</h2>
<p>The coordinate interface is recommended for the demonstrated map-preparation calculation with explicit reference labels.
Additional input qualification is required before the outputs are used for survey, clearance or engineering measurement.</p>
<p>For each engineering dataset, record the horizontal reference, axis order, units, coordinate epoch and positional uncertainty.
Supply a vertical reference when height matters. Check the reference area and reject non-finite results before export.
These controls address the reference and domain limitations in Sections 3 and 7.</p>
<p>For a distance or area decision, select a qualified local projection or an ellipsoidal geodesic calculation,
and assess its error against the supplied engineering tolerance. The projection distortion in Section 4 is the reason for this action {ref(1)} {ref(2)}.
At a pole, select a polar reference instead of accepting Web Mercator output {ref(1)}.</p>
<p>Extend the present numerical controls with dated survey-control coordinates, explicit area checks,
vertical-reference tests and date-line cases before extending the report's acceptance scope.
The missing data are listed in Table 3; no numerical survey tolerance is assigned without that basis.</p>
</section>
<section id="section-9"><h2>9. References</h2>
<p>The calculation basis is cited to open technical publications and the versioned projection manual.
Repository evidence is located separately so that implementation records are not substituted for theory.</p>
<h3>9.1 Public technical sources</h3>
<ol><li id="ref-1"><a href="https://pubs.usgs.gov/pp/1395/report.pdf">Map Projections—A Working Manual</a>,
United States Geological Survey Professional Paper 1395, 1987 edition, Chapter 7, equations (7-1), (7-1a), (7-2), (7-3), (7-4) and (7-5), pages 41–43.
The spherical equations, inverse, scale and polar singularity are used.</li>
<li id="ref-2"><a href="https://proj.org/en/9.5/operations/projections/webmerc.html">PROJ 9.5 manual: Web Mercator / Pseudo Mercator</a>,
Usage, Parameters and Mathematical definition. The public example and the spherical projection of ellipsoidal coordinates are used.</li>
<li id="ref-3"><a href="https://proj.org/en/9.5/usage/ellipsoids.html">PROJ 9.5 manual: Ellipsoids</a>,
Built-in ellipsoids, World Geodetic System 1984 row. The semi-major axis is used; the flattening is not used in the spherical projection equation.</li></ol>
<h3>9.2 Interface and repository records</h3>
<p>The public entry point is <code>CoordinateTransformer</code> with <code>transform_point</code> or <code>transform_points</code>.
The source identifier is <code>EPSG:4326</code> and the map identifier is <code>EPSG:3857</code>.
The complete example is calculated by the generator; the following minimal call shows its order.</p>
<pre><code>from digitalmodel.gis import CoordinateTransformer
forward = CoordinateTransformer("EPSG:4326", "EPSG:3857")
easting, northing = forward.transform_point(2, 49)
inverse = CoordinateTransformer("EPSG:3857", "EPSG:4326")
longitude, latitude = inverse.transform_point(easting, northing)</code></pre>
{locator_table}
</section>
<section id="section-10"><h2>10. Appendix</h2>
<p>Missing inputs, evidence boundaries and review controls are recorded here.
The appendices distinguish numerical software checks from engineering acceptance.</p>
<h3>10.1 Data register</h3>{register}
<h3>10.2 Quality pre-flight evidence</h3>
<p>The report test checks three literal hand-calculated anchors, the published coordinate pair, every bound result,
the plotted samples and the rebuilt artifact at displayed precision. A headline coordinate is recomputed from the displayed inputs.
Links and section targets are checked; the stylesheet is compared with the layout exemplar.</p>
<p>Text is read and written explicitly as Unicode, with fixed line endings. The report tests are executed with the implicit Unicode mode unset.
Browser interaction, print rendering, external survey data and imagery retrieval are not tested by this report.
Comment saving, export, reload and chart focus events are exercised with simulated file handles and document elements.
Their operation in a particular browser remains untested.</p>
<h3>10.3 Not covered</h3>
<p>Raster operations, temporal data, well-specific metadata, external mapping exports, imagery retrieval and rendering,
and environmental-source selection are outside the worked calculation. The repository contains additional tests for those areas;
no execution or numerical acceptance is claimed for them here.</p>
<h3>10.4 Engineering changes and comments</h3>
<details id="engineering-changes"><summary>Engineering changes in this revision</summary><ul>{changes}</ul></details>
<p>Comments are saved as a separate file beside this report. Select the report folder, add a section comment,
then save. Where folder access is unavailable, export the comments and place the downloaded file beside the report.
Existing comments are loaded from that folder or imported before editing. Unsaved comments exist only while the page is open.</p>
<label>Section <select id="comment-section">{comment_options}</select></label>
<label>Comment <textarea id="comment-text" maxlength="10000"></textarea></label>
<button id="select-folder" type="button">Select report folder</button>
<button id="add-comment" type="button">Add comment</button>
<button id="save-comments" type="button">Save beside report</button>
<button id="export-comments" type="button">Export comments</button>
<label>Import comments <input id="import-comments" type="file" accept="application/json,.json"></label>
<p id="comment-status" role="status">No comment folder selected.</p><ul id="comment-list"></ul>
</section></main></div>
<script type="application/json" id="calculation-record">{json.dumps(rounded(record), ensure_ascii=False, sort_keys=True)}</script>
{INTERACTION}</body></html>
'''


def main() -> None:
    """Generate both deliverables in the selected directory without network access."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=HERE)
    args = parser.parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    (args.output_dir / "report.html").write_text(build(calculate()), encoding="utf-8", newline="\n")
    (args.output_dir / "changes.json").write_text(
        json.dumps(CHANGES, ensure_ascii=False, indent=2) + "\n", encoding="utf-8", newline="\n")


if __name__ == "__main__":
    main()
