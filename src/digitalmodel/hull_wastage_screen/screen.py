"""Hull wastage screen; all rule limits are explicit, source-tagged inputs.

Method: IACS CSR BC&OT, 01 July 2025, Pt 1 Ch 13 Sec 2, renewal criteria.
Percentage alternative: IACS UR Z7 Rev.21 (2014), 1.2.8.
No built-in corrosion allowance, survey applicability or pitting acceptance.
"""
from __future__ import annotations

import math

from digitalmodel.asset_integrity.assessment.ffs_decision import Verdict, action_for
from digitalmodel.naval_architecture.hull_girder_screening import (
    SectionElement,
    section_properties,
)

CSR_SOURCE = (
    "IACS CSR BC&OT, 01 July 2025, Pt 1 Ch 13 Sec 2 [2.1, 2.2]; "
    "https://rulesexplorer-docs.bureauveritas.com/documents/nr606/jan2025/606-NR_2025-01.pdf"
)
Z7_SOURCE = (
    "IACS UR Z7 Rev.21 (2014), 1.2.8; "
    "https://www.turkloydu.org/pdf-files/iacs-karar-ve-csr-degisimleri/iacs-es-gereklilikleri/UR_Z7.pdf"
)


def number(data, key, *, positive=False):
    """Require a finite numeric input; booleans and numeric strings are rejected."""
    value = data.get(key)
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{key} must be a finite number")
    if not math.isfinite(value) or (value <= 0 if positive else value < 0):
        raise ValueError(f"{key} must be {'positive' if positive else 'nonnegative'} and finite")
    return float(value)


def source(data):
    value = data.get("source")
    if not isinstance(value, str) or not value.strip():
        raise ValueError("source must identify the selected limits and edition")
    return value


def fraction(data, key):
    value = number(data, key, positive=True)
    if value > 1:
        raise ValueError(f"{key} must be <= 1")
    return value


def not_evaluated(reason):
    return {"status": "NOT_EVALUATED", "reason": reason}


def local_screen(item, family, limits):
    built = number(item, "as_built_mm", positive=True)
    measured = number(item, "measured_mm")
    uncertainty = number(item, "uncertainty_mm")
    lower = max(0., measured - uncertainty)
    if family == "csr":
        # CSR BC&OT 2025 Pt1 Ch13 Sec2 [2.1.1]: tren = tas-built - tc - tvol_add.
        renewal = built - number(item, "corrosion_mm") - number(item, "voluntary_mm")
        # [2.1.3] is lower-inclusive, upper-exclusive; reserve is caller-sourced.
        boundary = renewal + number(limits, "reserve_mm", positive=True)
        monitor = lower < boundary
    else:
        renewal = number(item, "renewal_mm", positive=True)
        # UR Z7 Rev.21 (2014) [1.2.8]: strict excess of selected allowable margin.
        boundary = built - fraction(limits, "substantial_fraction") * (built - renewal)
        monitor = lower < boundary
    if renewal <= 0 or renewal > built:
        raise ValueError("renewal thickness must lie in (0, as_built_mm]")
    status = "RENEW" if lower < renewal else "MONITOR" if monitor else "ACCEPT"
    return {"id": item["id"], "measured_mm": measured, "lower_bound_mm": lower,
            "renewal_mm": renewal, "monitor_boundary_mm": boundary,
            "diminution_mm": built - measured, "status": status}


def _member(member, item, thickness):
    span = number(member, "span_m", positive=True)
    z = number(member, "z_m")
    orientation = member.get("orientation")
    # SI prefix definition: 1000 mm = 1 m (exact unit conversion).
    t = thickness / 1000.
    if orientation == "horizontal":
        return SectionElement.horizontal(item["id"], span, t, z)
    if orientation == "vertical":
        return SectionElement.vertical(item["id"], span, t, z)
    raise ValueError("orientation must be horizontal or vertical")


def global_screen(block, items, local, family):
    if block is None:
        return not_evaluated("Complete section geometry and gross offered basis required")
    if family != "csr":
        return not_evaluated("Percentage-family global criteria are not established")
    if block.get("complete_section") is not True:
        return not_evaluated("Contributing section completeness must be confirmed")
    source(block)
    depth = number(block, "depth_m", positive=True)
    modulus_fraction = fraction(block, "modulus_fraction")
    corrosion_fraction = fraction(block, "na_corrosion_fraction")
    gross, measured, na_current, na_required = [], [], 0., 0.
    zones, seen = set(), set()
    for member in block.get("members", []):
        key = member.get("element_id")
        if key not in items or key in seen:
            raise ValueError("section members require unique known element_id values")
        seen.add(key)
        zone = member.get("zone")
        if zone not in {"deck", "bottom", "neutral_axis", "other"}:
            raise ValueError("unknown section zone")
        zones.add(zone)
        item = items[key]
        offered = number(member, "gross_offered_mm", positive=True)
        if offered > item["as_built_mm"]:
            raise ValueError("gross offered thickness exceeds as-built thickness")
        current = local[key]["lower_bound_mm"]
        g, m = _member(member, item, offered), _member(member, item, current)
        if member["z_m"] > depth:
            raise ValueError("member centroid exceeds section depth")
        if member["orientation"] == "vertical" and (
            member["z_m"] - member["span_m"] / 2 < 0
            or member["z_m"] + member["span_m"] / 2 > depth
        ):
            raise ValueError("vertical member extends beyond section depth")
        gross.append(g)
        measured.append(m)
        if zone == "neutral_axis":
            # CSR 2025 Ch13 Sec2 [2.2.2 b]: area at gross - selected fraction * tc.
            reference = offered - corrosion_fraction * number(item, "corrosion_mm")
            if reference <= 0:
                raise ValueError("neutral-axis reference thickness must be positive")
            na_current += m.area_m2
            na_required += _member(member, item, reference).area_m2
    if not {"deck", "bottom", "neutral_axis"} <= zones:
        return not_evaluated("Deck, bottom and neutral-axis zones must all be represented")
    if any(local[key]["lower_bound_mm"] == 0 for key in seen):
        return {"status": "RENEW", "reason": "Section contains a fully consumed member"}
    g = section_properties(gross, depth_m=depth)
    m = section_properties(measured, depth_m=depth)
    # CSR 2025 Ch13 Sec2 [2.2.2 a]: both extreme-fibre moduli must pass.
    ratios = {key: m[key] / g[key] for key in ("sm_deck_m3", "sm_keel_m3")}
    passes = min(ratios.values()) >= modulus_fraction and na_current >= na_required
    return {"status": "ACCEPT" if passes else "RENEW", "gross": g,
            "measured": m, "modulus_ratios": ratios,
            "modulus_fraction": modulus_fraction, "na_area_m2": na_current,
            "na_required_m2": na_required, "source": block["source"]}


def assess(data):
    """Return a partial screen with explicit missing-check dispositions.

    Elements must be representative item readings selected by the survey basis;
    this function does not infer representative averages from raw TM reports.
    """
    family = data.get("rule_family")
    if family not in {"csr", "percentage"}:
        raise ValueError("rule_family must be csr or percentage")
    limits = data["limits"]
    source(limits)
    elements = data.get("elements")
    if not isinstance(elements, list) or not elements:
        raise ValueError("elements must be a nonempty list")
    items, rows = {}, []
    for item in elements:
        key = item.get("id")
        if not isinstance(key, str) or not key.strip() or key in items:
            raise ValueError("element id values must be nonempty and unique")
        items[key] = item
        rows.append(local_screen(item, family, limits))
    from .buckling import plate_screen

    local = {r["id"]: r for r in rows}
    checks = {
        "global": global_screen(data.get("global"), items, {r["id"]: r for r in rows}, family),
        "buckling": plate_screen(data.get("buckling"), items, local),
        "pitting": not_evaluated(
            "Hull survey pitting limits and Rec.84 applicability are not established; "
            "API 579 pressure-equipment pitting acceptance is not substituted"),
    }
    statuses = [r["status"] for r in rows] + [c["status"] for c in checks.values()]
    status = next(s for s in ("RENEW", "NOT_EVALUATED", "MONITOR", "ACCEPT") if s in statuses)
    # Shared FFS vocabulary and hull action map; no pressure derating heuristics.
    verdict = {"RENEW": Verdict.REPAIR, "NOT_EVALUATED": Verdict.ESCALATE,
               "MONITOR": Verdict.MONITOR, "ACCEPT": Verdict.ACCEPT}[status]
    return {"status": status, "rule_family": family, "elements": rows, "checks": checks,
            "decision": {"verdict": verdict.value, "action": action_for(verdict, "hull_plating")},
            "sources": [CSR_SOURCE if family == "csr" else Z7_SOURCE, limits["source"]],
            "limitations": ["Screening only; class survey acceptance is not established",
                            "Zone membership and representative readings require survey review"]}
