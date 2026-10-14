# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Reference-jacket presentation from an attributed three-dimensional schedule."""

from __future__ import annotations

from collections.abc import Mapping
from typing import Any
import re


from digitalmodel.cathodic_protection.report_elevation import _svg, percentage_text, validate_geometry
from digitalmodel.reporting.spec import FigureBlock

ATTRIBUTION = (
    "[2] Mroczek, 2023, revision da77a393; Apache License 2.0. "
    "[1] Mroczek, Arwade and Lackner, 2023; CC BY 4.0."
)


def reference_projection(model: Mapping[str, Any]) -> dict[str, Any]:
    """Project every tubular axis onto the vertical y–z plane."""
    joints = {j["id"]: j["coordinates_m"] for j in model["joints"]}
    props = {p["id"]: p for p in model["property_sets"]}
    legs: list[dict[str, Any]] = []
    diagonals: list[dict[str, Any]] = []
    for member in model["members"]:
        a, b = (joints[i] for i in member["joint_ids"])
        item = {"name": f"member-{member['id']}", "start": [a[1], a[2]],
                "end": [b[1], b[2]],
                "diameter_m": props[member["property_set_id"]]["diameter_m"],
                "true_length_m": member["true_length_m"], "count": member["count"]}
        (legs if member["section_class"] <= 5 else diagonals).append(item)
    return {
        "basis": "openly_licensed_reference", "datum": "still-water level",
        "waterline_m": 0, "mudline_m": -model["water_depth_m"],
        "zones": [
            {"name": "Submerged and splash, undivided", "calculation_zone": "submerged",
             "lower_m": -model["water_depth_m"], "upper_m": 0},
            {"name": "Splash and atmospheric, undivided", "calculation_zone": "atmospheric",
             "lower_m": 0, "upper_m": max(j[2] for j in joints.values())}],
        "legs": legs, "diagonals": diagonals, "brace_levels": [],
        "citation": "[1]; [2]", "attribution": ATTRIBUTION,
        "licence": "Apache License 2.0; CC BY 4.0",
    }


def _dimensioned_svg(model: Mapping[str, Any], geometry: Any) -> str:
    """Append coordinate-derived principal dimensions to the scaled figure."""
    svg = _svg(geometry)
    match = re.search(r'viewBox="0 0 ([^ ]+) ([^"]+)"', svg)
    if match is None:
        raise ValueError("drawing extent is not established")
    width, height = map(float, match.groups())
    joints = [j["coordinates_m"] for j in model["joints"]]
    top = max(j[2] for j in joints)
    bottom = min(j[2] for j in joints)
    top_span = max(j[1] for j in joints if j[2] == top) - min(j[1] for j in joints if j[2] == top)
    base_span = max(j[1] for j in joints if j[2] == bottom) - min(j[1] for j in joints if j[2] == bottom)
    labels = [f"Axis height above mudline: {top-bottom:.1f} m; water depth: {-bottom:.1f} m",
              f"Projected top span: {top_span:.3f} m; mudline span: {base_span:.3f} m"]
    svg = svg.replace(match.group(), f'viewBox="0 0 {width:g} {height+45:g}"')
    text = "".join(f'<text x="45" y="{height+15+i*14:g}" font-size="10">{label}</text>' for i, label in enumerate(labels))
    return svg.replace("</svg>", text + "</svg>")


def reference_figure(model: Mapping[str, Any]) -> FigureBlock:
    """Draw the complete coordinate-derived jacket at one scale."""
    geometry = validate_geometry(reference_projection(model))
    return FigureBlock(
        title="Reference jacket elevation", figure_id="fig-reference-jacket-elevation",
        svg=_dimensioned_svg(model, geometry),
        caption=(
            "Projection onto the vertical y–z plane of all 63 tubular members; "
            "axis true lengths are computed in three dimensions. The source inventory "
            "contains no horizontal plan-bracing members. Joint offsets and can lengths "
            "are not deducted. Water depth is 60 m; tubular height above mudline is 74.1 m. "
            "The projected top and mudline spans are distinct from triangle side widths; "
            "both measures are listed in Published values and comparison. "
            "Splash separation is not evaluated: water-level and lower splash-limit "
            "evidence are required. Values were transcribed and converted in form; "
            "the figure is drawn from coordinates, with no source figure adapted. "
            + ATTRIBUTION
        ),
    )


def _axis_overlap(member: Mapping[str, Any], joints: Mapping[int, Any],
                  lower: float, upper: float) -> float:
    """Calculate the true axis length between two elevation planes."""
    a, b = (joints[i][2] for i in member["joint_ids"])
    low, high = sorted((float(a), float(b)))
    if low == high:
        fraction = float(lower <= low < upper)
    else:
        fraction = max(0.0, min(high, upper) - max(low, lower)) / (high - low)
    return float(member["true_length_m"]) * int(member["count"]) * fraction


def reference_zone_rows(model: Mapping[str, Any],
                        piles: Mapping[str, Any]) -> list[dict[str, Any]]:
    """Derive class areas in resolved elevation bins and estimated piles."""
    import math

    joints = {j["id"]: j["coordinates_m"] for j in model["joints"]}
    top = max(float(j[2]) for j in joints.values())
    intervals = [("Water-column steel, undivided", -float(model["water_depth_m"]), 0.0),
                 ("Above-water steel, undivided", 0.0, top)]
    rows = []
    for zone, lower, upper in intervals:
        for cls in model["section_classes"]:
            members = [m for m in model["members"] if m["section_class"] == cls["section_class"]]
            lengths = [_axis_overlap(m, joints, lower, upper) for m in members]
            length = sum(lengths)
            rows.append({"zone": zone, "section_class": cls["section_class"],
                         "count": sum(int(m["count"]) for m, length in zip(members, lengths) if length > 0),
                         "length_m": length, "diameter_m": cls["diameter_m"],
                         "area_m2": math.pi * float(cls["diameter_m"]) * length,
                         "locator": "[2], joints, members and circular tubular properties"})
    count = len([j for j in joints.values() if j[2] == -float(model["water_depth_m"])])
    length = count * float(piles["embedment_m"])
    diameter = float(piles["diameter_m"])
    rows.append({"zone": "Buried piles, initial estimate", "section_class": "Pile estimate",
                 "count": count, "length_m": length, "diameter_m": diameter,
                 "area_m2": math.pi * diameter * length,
                 "locator": "[1], " + str(piles["locator"])})
    return rows


def reference_zone_blocks(model: Mapping[str, Any],
                          piles: Mapping[str, Any]) -> list[Any]:
    """Present every class area, arithmetic total and unresolved division."""
    from digitalmodel.reporting.spec import TableBlock, TextBlock

    rows = reference_zone_rows(model, piles)
    lengths = sum(r["length_m"] for r in rows if r["zone"] != "Buried piles, initial estimate")
    missing = max(0.0, float(model["total_true_length_m"]) - lengths)
    table = TableBlock(
        title="External area derivation by elevation interval and section class",
        columns=["Elevation interval", "Section class", "Count intersecting", "Axis length",
                 "Outside diameter", "External area", "Locator"],
        units=["-", "-", "-", "m", "m", "m2", "-"],
        rows=[[r["zone"], r["section_class"], r["count"], f"{r['length_m']:.3f}",
               f"{r['diameter_m']:.3f}", f"{r['area_m2']:.3f}", r["locator"]] for r in rows])
    zones = dict.fromkeys(r["zone"] for r in rows)
    totals = TableBlock(title="External area totals", columns=["Elevation interval", "Area", "Locator"],
                        units=["-", "m2", "-"], rows=[
                            [z, f"{sum(r['area_m2'] for r in rows if r['zone'] == z):.3f}",
                             "[1], initial pile estimate" if z == "Buried piles, initial estimate" else "[2], complete member schedule"] for z in zones])
    return [TextBlock(markdown=(
        "External area = π × outside diameter × axis length in the elevation interval. "
        "Crossing axes are split at each interval boundary. Joint offsets, can lengths, "
        "end caps, member intersections and internal surfaces are not deducted or added. "
        f"Jacket interval lengths sum to {lengths:.3f} m against {model['total_true_length_m']:.3f} m; "
        f"axis length outside the reported intervals: {missing:.3f} m. "
        "Splash separation is not evaluated: a defined splash boundary, "
        "water-level range and applicable wave data are required. The printed maximum "
        "crest does not establish the lower splash boundary [1], p. 811, Figure 3. "
        "Above-water steel remains undivided between splash and atmospheric exposure. "
        "The jacket axes end at the mudline. Buried area uses the paper's initial pile "
        "estimate, one pile per leg; the final installed pile geometry is not established "
        "[1], pp. 810–811, Soil–structure interaction.")), table, totals]


def reference_arrangements(cfg: Mapping[str, Any]) -> dict[str, Any]:
    """Evaluate the stated arrangement and recheck a required count increase."""
    from copy import deepcopy
    from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection

    evaluated = run_cathodic_protection(deepcopy(dict(cfg)))
    stated = evaluated["results"]
    if stated["status"]["result"] == "PASS":
        return {"stated": stated, "rechecked": None}
    candidate = deepcopy(dict(cfg))
    candidate["inputs"]["anode"]["count"] = stated["current_output_verification"]["recommended_anode_count"]
    checked = run_cathodic_protection(candidate)["results"]
    if checked["status"]["result"] != "PASS":
        raise ValueError("a passing aggregate arrangement is not established")
    return {"stated": stated, "rechecked": checked}


def reference_checks(cfg: Mapping[str, Any], arrangements: Mapping[str, Any]) -> Any:
    """Bind each aggregate conclusion to its demand, capacity and margin."""
    from digitalmodel.cathodic_protection import _kernels as kernel
    from digitalmodel.cathodic_protection.report_verification import _model_outputs
    from digitalmodel.reporting.spec import TableBlock

    rows = []
    for name, result in (("Stated arrangement", arrangements["stated"]),
                         ("Rechecked arrangement", arrangements["rechecked"])):
        if result is None:
            continue
        count = result["current_output_verification"]["anode_count"]
        req = result["anode_requirements"]
        initial, final = _model_outputs(cfg["inputs"], result)
        demands = result["current_demand_A"]
        mass = demands["total_mean_A"] * kernel.HOURS_PER_YEAR * result["design_life_years"] / (req["electrochemical_capacity_Ah_kg"] * req["utilization_factor"])
        values = [("Initial current", demands["total_initial_A"], count * initial, "A"),
                  ("Final current", demands["total_final_A"], count * final, "A"),
                  ("Mass", mass, count * cfg["inputs"]["anode"]["individual_anode_mass_kg"], "kg")]
        for check, demand, capacity, unit in values:
            decimals = 0 if unit == "kg" else 3
            rows.append([name, check, f"{demand:.{decimals}f}", f"{capacity:.{decimals}f}",
                         f"{100 * (capacity / demand - 1):+.1f}%",
                         "PASS" if capacity >= demand else "FAIL", unit,
                         "calculated from declared inputs"])
    return TableBlock(title="Aggregate mass and current checks",
                      columns=["Arrangement", "Check", "Demand", "Capacity", "Margin", "Result", "Unit", "Locator"], rows=rows)


def reference_sections(cfg: Mapping[str, Any]) -> list[Any]:
    """Build an analysis with source checks separated from CP adequacy."""
    from digitalmodel.reporting.spec import Section, TextBlock

    model = cfg["structural_model"]
    try:
        arrangements = reference_arrangements(cfg)
    except (KeyError, TypeError, ValueError, OverflowError):
        arrangements = {"stated": cfg["results"], "rechecked": None}
    count = arrangements["stated"]["current_output_verification"]["anode_count"]
    passed = arrangements["stated"]["status"]["result"] == "PASS"
    finding = ("All evaluated aggregate checks PASS for the stated arrangement. "
               if passed else "The stated arrangement fails an aggregate check; the increased arrangement is rechecked below. ")
    if not passed:
        revised = arrangements["rechecked"]
        finding = (f"The stated arrangement fails an aggregate check. Increasing to {revised['current_output_verification']['anode_count']} anodes is rechecked and passes the aggregate mass and current checks. "
                   if revised is not None else "Rechecked arrangement is not evaluated: complete demand, output, mass and arrangement data are required. ")
    qualified = arrangements["stated"] if passed else arrangements["rechecked"]
    minimum = qualified is not None and qualified["current_output_verification"]["anode_count"] == qualified["current_output_verification"]["recommended_anode_count"]
    kind = "minimum aggregate arrangement" if minimum else "evaluated aggregate arrangement"
    recommendation = (f"The {kind} is recommended at {qualified['current_output_verification']['anode_count']} anodes on the stated basis. "
                      if qualified is not None else "A passing aggregate quantity is not evaluated. ")
    qualification = (f"The recommended option is a {kind}, with no selected anode product; anode size requires confirmation. "
                     if qualified is not None else "No selected anode product is established; anode size requires confirmation. ")
    mass_difference = abs(next(c["difference_percent"] for c in model["comparisons"] if c["quantity"] == "Tubular jacket steel mass"))
    angle_difference = next(c["difference_percent"] for c in model["comparisons"] if c["quantity"] == "Leg 1 inclination in 3D")
    summary = (f"The {count}-anode arrangement on the reference jacket is analysed against "
               "aggregate mass and initial and final current requirements. The paper provides "
               "structural comparisons; published CP outputs are not available. " + finding +
               f"Tubular steel mass is {mass_difference:.1f}% below the paper's printed jacket mass on the stated "
               f"exclusion basis; the mass discrepancy and 3D leg inclination difference ({angle_difference:+.1f}%) remain unresolved. "
               + qualification + recommendation + "Confirmation of the mass basis, "
               "splash boundaries, installed piles and spatial protection is recommended before "
               "a final design is established. Structural comparisons, area derivation and "
               "the limitations below identify the evidence required.")
    sections = [Section(key="summary", title="Summary", blocks=[TextBlock(markdown=summary)]),
                Section(key="design-basis", title="Design basis", blocks=reference_basis(cfg)),
                Section(key="elevation", title="Reference jacket geometry", blocks=[reference_figure(model)]),
                Section(key="member-schedule", title="Member schedule", blocks=reference_member_tables(model)),
                Section(key="structural-comparison", title="Published values and comparison", blocks=reference_comparison(model)),
                Section(key="zone-areas", title="External area derivation", blocks=reference_zone_blocks(model, cfg["initial_pile_estimate"])),
                Section(key="adequacy", title="Aggregate adequacy", blocks=[reference_checks(cfg, arrangements)]),
                Section(key="limitations", title="Limitations and way forward", blocks=[TextBlock(markdown="\n\n".join(cfg["cp_limitations"]) + "\n\n" + model["mass_limitation"])]),
                Section(key="references", title="References", blocks=reference_references(model))]
    if not passed:
        from digitalmodel.cathodic_protection.report_adapters import _report_options, _verification_presentation
        options = Section(key="options", title="Options", blocks=_report_options(cfg["inputs"], arrangements["stated"]))
        _verification_presentation([options])
        for block in options.blocks:
            if isinstance(block, TextBlock):
                block.markdown = block.markdown.replace("Design increase and recheck", "Increase at design and recheck")
        options.blocks.append(TextBlock(markdown="A recurring monitoring interval is not evaluated; elapsed service, remaining anode condition and spatial survey evidence are required to set a date."))
        for block in options.blocks:
            if isinstance(block, TextBlock):
                block.markdown = block.markdown.replace("[1]", "[3]")
            if hasattr(block, "rows"):
                block.rows = [[v.replace("[1]", "[3]") if isinstance(v, str) else v for v in row] for row in block.rows]
        sections[-3].blocks.extend(options.blocks)
    return sections


def reference_basis(cfg: Mapping[str, Any]) -> list[Any]:
    """State the calculation assumptions once and identify standard values."""
    from digitalmodel.reporting.spec import TableBlock, TextBlock

    inputs = cfg["inputs"]
    env, anode = inputs["environment"], inputs["anode"]
    result = cfg["results"]
    req = result["anode_requirements"]
    count = result["current_output_verification"]["count_by_mass"]
    return [TextBlock(markdown=(
        "Environment, coating, design life and cylinder dimensions are declared "
        "example inputs (source: illustrative input), stated in Design input sources. "
        "Net alloy mass is derived from the cylinder "
        f"dimensions: π × radius² × length × 2750 kg/m³ = {anode['individual_anode_mass_kg']:.0f} kg; "
        "no core is specified or deducted. Fabrication and attachment are not evaluated. "
        f"The stated arrangement is {result['current_output_verification']['anode_count']} anodes, compared with the mass-only requirement of {count} anodes: "
        "mean current × 8760 hours/year × design life / (capacity × utilisation × net anode mass), rounded upward. "
        f"Capacity is {req['electrochemical_capacity_Ah_kg']:g} Ah/kg and utilisation is "
        f"{req['utilization_factor']:g}; these inputs, electrode potentials, current densities "
        "and coating breakdown are based on previous project experience. "
        "Water-column current areas are divided at the 30 m depth boundary; the upper and "
        "lower seawater rows and the buried row are evaluated. Above-water steel is excluded "
        "from current demand because electrolyte contact is not established. "
        "No measured potentials are available for this structure.")),
        TableBlock(title="Design input sources", columns=["Quantity", "Value", "Unit", "Source"], rows=[
            ["Seawater temperature", env["seawater_temperature_C"], "°C", "illustrative input"],
            ["Seawater resistivity", env["seawater_resistivity_ohm_m"], "Ω m", "illustrative input"],
            ["Coating category", "III", "-", "illustrative input"],
            ["Design life", inputs["design_data"]["design_life"], "yr", "illustrative input"],
            ["Anode length", anode["length_m"], "m", "illustrative input"],
            ["Anode radius", anode["radius_m"], "m", "illustrative input"],
            ["Electrochemical capacity", req["electrochemical_capacity_Ah_kg"], "Ah/kg", "based on previous project experience"],
            ["Utilisation factor", req["utilization_factor"], "-", "based on previous project experience"],
        ]),
        TextBlock(markdown="Governing practice: DNV-RP-B401, Cathodic protection design, May 2021. "
                  "Design current densities; Coating breakdown factors; Anode materials; "
                  "Anode resistance; Anode current output [3].")]


def reference_member_tables(model: Mapping[str, Any]) -> list[Any]:
    """Show class totals and every member with its row locator."""
    from digitalmodel.reporting.spec import TableBlock

    return [TableBlock(title="Section classes and true axis lengths",
                       columns=["Class", "Count", "Diameter (m)", "Wall (m)", "Total true length (m)", "Locator"],
                       rows=[[s["section_class"], s["count"], f"{s['diameter_m']:.3f}", f"{s['wall_thickness_m']:.3f}", f"{s['total_true_length_m']:.3f}", "[2], circular tubular properties"] for s in model["section_classes"]]),
            TableBlock(title="Complete member inventory", columns=["Member", "Joint A", "Joint B", "Class", "True length (m)", "Locator"],
                       rows=[[m["id"], *m["joint_ids"], m["section_class"], f"{m['true_length_m']:.3f}", f"[2], members, row {m['id']}, revision da77a393"] for m in model["members"]])]


def reference_comparison(model: Mapping[str, Any]) -> list[Any]:
    """Print each sourced comparison without tuning geometric differences."""
    from digitalmodel.reporting.spec import TableBlock, TextBlock

    rows = []
    for c in model["comparisons"]:
        digits = 0 if c["unit"] == "kg" else 3
        rows.append([c["quantity"], f"{c['printed']:.{digits}f}", f"{c['model']:.{digits}f}",
                     ("not comparable" if "y-projection" in c["quantity"] or "y projection" in c["quantity"] else percentage_text(c["difference_percent"])), c["unit"], "[1], " + c["locator"]])
    return [TableBlock(title="Structural dimensions and mass compared with the paper",
                       columns=["Quantity", "Paper", "Structural model", "Difference", "Unit", "Locator"], rows=rows),
            TextBlock(markdown=model["mass_limitation"])]


def reference_references(model: Mapping[str, Any]) -> list[Any]:
    """Attribute public sources and record changes and licence conditions."""
    from digitalmodel.reporting.spec import TextBlock

    return [TextBlock(markdown=(
        "[1] Mroczek, Arwade and Lackner (2023), Design optimization of offshore wind jacket piles "
        "by assessing support structure orientation relative to metocean conditions, Wind Energy Science "
        "8, 807–817. https://doi.org/10.5194/wes-8-807-2023. CC BY 4.0, p. 807.\n\n"
        "[2] Mroczek (2023), IEA-15-240-RWT jacket structural model, OpenFAST, "
        "IEA-15-240-RWT-Jacket_SubDyn.dat. "
        "https://github.com/mmrocze2/IEA-15-240-RWT/tree/da77a393569b6c5ddf7aa424e5b0a92d37eba833/"
        "OpenFAST/IEA-15-240-RWT-Jacket_60m_water_depth. Revision da77a393569b6c5ddf7aa424e5b0a92d37eba833. "
        "Apache License 2.0: https://github.com/mmrocze2/IEA-15-240-RWT/blob/da77a393569b6c5ddf7aa424e5b0a92d37eba833/LICENSE. "
        "Values were transcribed and converted in form; lengths, areas and mass were calculated from "
        "the coordinate and section schedule. Original attribution is retained; a verbatim licence "
        "copy accompanies the case input. No source figure is reproduced or adapted.\n\n"
        "[3] DNV-RP-B401, Cathodic protection design, May 2021. Design current densities; "
        "Coating breakdown factors; Anode materials; Anode mass; Anode resistance; "
        "Anode current output; Output adequacy; Fresh/depleted resistance; Buried surfaces. "
        "Clause titles identify the calculation basis; splash boundaries are not established."))]
