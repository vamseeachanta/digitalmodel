"""Mooring-chain degradation intake, dimensional screening and evidence gaps.

No chain strength or fatigue coefficient is embedded. Source-qualified local
geometry is delegated to mooring_chain_dimensions; unresolved mechanisms force
shared FFS escalation, rather than treating an area ratio as residual MBL.
"""

from __future__ import annotations

from dataclasses import asdict

from digitalmodel.asset_integrity.applicability import Applicability, merge
from digitalmodel.asset_integrity.assessment.ffs_decision import decide
from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport
from digitalmodel.asset_integrity.mooring_chain_dimensions import (
    ChainInspection,
    DiameterReading,
    VisualInspection,
    screen_dimensions,
)

WORKFLOW_ID = "mooring-chain-degradation-screen"

# Evidence gaps, not normative coefficients. Locator descriptions are based on
# IACS Rec 38 rev 2 (July 2020), sections 5.1-5.4, and the existing method note.
GAPS = {
    "remaining_mbl": (
        "Remaining MBL",
        "Validated degraded-link capacity relation and break-test benchmarks; "
        "nominal MBL with source and chain identity. Area ratio is not MBL.",
    ),
    "pitting": (
        "Pitting classes and stress concentration",
        "Opened Chain FEARS small/medium/mega definitions, pit map, "
        "validated area-loss and stress-concentration method with bounds.",
    ),
    "interlink_wear": (
        "Interlink wear",
        "Individual contact-zone readings and a qualified method; a double "
        "diameter alone cannot resolve the separate link diameters "
        "(BSEE mooring integrity study, 2015, Table 7).",
    ),
    "fatigue": (
        "Remaining fatigue life",
        "Chain-specific S-N curve qualified for corrosion, mean load and "
        "section loss; load history, prior damage and fatigue design factor. "
        "Existing mooring_fatigue welded-detail curves are not qualified "
        "for this chain assessment.",
    ),
    "ovalization": (
        "Ovalization tolerance",
        "Measured link geometry, applicable tolerance and qualification; "
        "a visual anomaly flag does not establish numerical acceptance.",
    ),
}


def _inspection(raw: dict) -> ChainInspection:
    """Build typed records; reject missing, misspelled and unsupported inputs."""
    if not isinstance(raw, dict):
        raise ValueError("inspection must be a mapping")
    try:
        values = dict(raw)
        values["readings"] = tuple(DiameterReading(**item) for item in raw["readings"])
        values["visual"] = VisualInspection(**raw["visual"])
        return ChainInspection(**values)
    except (KeyError, TypeError) as exc:
        raise ValueError("inspection requires the documented complete schema") from exc


def _result(inspection: ChainInspection) -> dict:
    """Retain local comparisons and force escalation on unqualified mechanisms."""
    dimensions = screen_dimensions(inspection)
    flags = [f"{name.upper()}_NOT_EVALUATED" for name in GAPS]
    notes = [evidence for _, evidence in GAPS.values()]
    exceeded = any(s.status == "EXCEEDS_LIMITS" for s in dimensions.samples)
    if exceeded:
        flags.append("DIMENSIONAL_LIMIT_EXCEEDED")
        notes.append("An inspected location exceeds the supplied operator limits.")
    if any(s.status == "INDETERMINATE" for s in dimensions.samples):
        flags.append("DIMENSIONAL_LIMIT_INDETERMINATE")
        notes.append("Measurement bounds straddle an operator limit; remeasure.")
    app = merge(dimensions.applicability, Applicability(False, flags, notes))
    # Unknown margin/life force the applicability branch of the shared decision
    # engine. No default decision band or assumed chain capacity is evaluated.
    decision = decide(
        float("nan"), float("nan"), float("nan"), "mooring_chain", applicability=app
    ).to_dict()
    for key in ("margin", "margin_allowable", "rsf", "rsf_a", "remaining_life_yr"):
        decision[key] = None  # JSON null means unknown, never zero or acceptance.
    decision["governing_criterion"] = (
        "Chain strength margin and remaining fatigue life are not established. "
        "Unresolved mechanism evidence and inspection applicability require "
        "engineering review before a fitness verdict."
    )
    mechanisms = {
        name: {"status": "NOT_EVALUATED", "required_evidence": evidence}
        for name, (_, evidence) in GAPS.items()
    }
    action = (
        "WITHDRAW the affected segment pending competent engineering disposition."
        if exceeded or inspection.visual.crack is True
        else "Obtain the listed evidence and competent engineering review before "
        "a continued-service decision."
    )
    return {
        "workflow_id": WORKFLOW_ID,
        "assessment_level": "T1 intake; T2 strength and fatigue not established",
        "dimensions": asdict(dimensions),
        "mechanisms": mechanisms,
        "remaining_mbl_kn": None,
        "remaining_fatigue_life_years": None,
        "decision": decision,
        "next_action": action,
        "validation": "Independent geometry checks only; no published degraded-link "
        "break-test or fatigue benchmark validation established.",
    }


def router(cfg: dict) -> dict:
    """Run the offline workflow without writes or mutation of caller inputs.

    The basename is mooring_chain_degradation; the durable workflow identifier
    is mooring-chain-degradation-screen. The caller may persist report_html.
    """
    block = cfg.get("mooring_chain_degradation")
    if not isinstance(block, dict) or set(block) - {"inspection", "component_id"}:
        raise ValueError(
            "mooring_chain_degradation requires inspection and component_id"
        )
    component_id = block.get("component_id")
    if not isinstance(component_id, str) or not component_id.strip():
        raise ValueError("component_id must be a nonempty string")
    inspection = _inspection(block.get("inspection"))
    result = _result(inspection)
    sections = {
        "Revision history": {
            "Method revision": "Dimensional intake, 2026-10-20",
            "Change and reason": "Dimensional intake and explicit "
            "evidence gaps to prevent unsupported FFS acceptance.",
        },
        "Inputs and criteria": {
            "Nominal diameter (mm)": inspection.nominal_diameter_mm,
            "Maximum diameter loss (%)": f"{inspection.max_diameter_loss_fraction * 100:.1f}",
            "Minimum area ratio (%)": f"{inspection.min_area_ratio * 100:.1f}",
            "Operator limit reference": inspection.operator_limit_reference,
            "Comparison basis": "Supplied operator limits; geometric intervals "
            "do not establish capacity or clearance when applicability is flagged.",
        },
        "Disposition": {
            "Next action": result["next_action"],
            "Remaining MBL (kN)": None,
            "Remaining fatigue life (years)": None,
        },
    }
    for reading, sample in zip(inspection.readings, result["dimensions"]["samples"]):
        sections[f"Location: {sample['location_id']}"] = {
            "Region": reading.region,
            "Diameter a (mm)": reading.diameter_a_mm,
            "Diameter b (mm)": reading.diameter_b_mm,
            "Uncertainty (mm)": reading.uncertainty_mm,
            "Status": sample["status"],
            "Mean diameter interval (mm)": _interval(sample["mean_diameter_range_mm"]),
            "Diameter loss interval (%)": _interval(
                sample["diameter_loss_range_fraction"], scale=100
            ),
            "Area ratio interval (%)": _interval(sample["area_ratio_range"], scale=100),
        }
    sections["Mechanism evidence"] = {
        name: f"NOT_EVALUATED: {evidence}" for name, (_, evidence) in GAPS.items()
    }
    sections["Sources"] = {
        item["document"]: f"{item['revision']}; {item['locator']}; {item['url']}"
        for item in result["dimensions"]["citations"]
    }
    report = FFSReport.generate_screening_html(
        component_id,
        "Mooring-chain degradation analysis",
        result["decision"],
        sections,
        result["decision"]["applicability"]["notes"],
    )
    return {
        **cfg,
        "mooring_chain_degradation": {
            **block,
            "result": result,
            "report_html": report,
        },
    }


def _interval(values, *, scale=1) -> str | None:
    """Presentation precision only; raw result intervals retain full precision."""
    return (
        None
        if values is None
        else f"{values[0] * scale:.1f} to {values[1] * scale:.1f}"
    )
