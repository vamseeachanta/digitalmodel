"""Manual dent geometry adapter, preserving all existing screening criteria."""
from dataclasses import asdict

from digitalmodel.asset_integrity.assessment.ffs_decision import decide
from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport
from digitalmodel.asset_integrity.assessment.screening_report import finite_number, store_report
from .dent_assessment import (
    assess_dent, PLAIN_DENT_DEPTH_LIMIT, WELD_DENT_DEPTH_LIMIT, PLAIN_DENT_STRAIN_LIMIT,
)


def _geometry(block):
    names = ("od_in", "wt_in", "dent_depth_in", "dent_length_axial_in",
             "dent_length_circ_in")
    geometry = {name: finite_number(block, name) for name in names}
    for name in ("on_weld", "restrained", "with_gouge_or_metal_loss"):
        if type(block.get(name)) is not bool:
            raise ValueError(f"{name} must be an explicit boolean; unknown features cannot be accepted")
        geometry[name] = block[name]
    limits = {"plain_dent_depth_limit": PLAIN_DENT_DEPTH_LIMIT,
              "weld_dent_depth_limit": WELD_DENT_DEPTH_LIMIT,
              "strain_limit": PLAIN_DENT_STRAIN_LIMIT}
    for name, limit in limits.items():
        if name in block:
            geometry[name] = finite_number(block, name)
            if geometry[name] > limit:
                raise ValueError(f"{name} cannot relax the existing engine limit {limit}")
    return geometry


def _decision(assessment, geometry):
    """Map engine dispositions through shared tree; tokens are not physical margins."""
    status = assessment.verdict
    if status not in {"ACCEPT", "MONITOR", "REJECT", "NEEDS_ASSESSMENT"}:
        raise ValueError(f"Unknown dent engine verdict {status}")
    unreliable = any(geometry["dent_depth_in"] / geometry[key] > 0.5
                     for key in ("dent_length_axial_in", "dent_length_circ_in"))
    if status == "REJECT":
        decision = decide(1.0, 1.0, 0.0, "pipeline", screening_pass=False)
    elif status == "NEEDS_ASSESSMENT" or unreliable:
        decision = decide(float("nan"), 1.0, 0.0, "pipeline")
    else:
        token = 1.0 if status == "MONITOR" else 2.0
        decision = decide(token, 1.0, float("inf"), "pipeline", screening_pass=True)
    result = decision.to_dict()
    for key in ("rsf", "rsf_a", "margin", "margin_allowable", "remaining_life_yr",
                "rerated_mawp_psi", "derated", "derated_units"):
        result.pop(key)
    result["governing_criterion"] = assessment.governing_criterion
    if status == "REJECT":
        result["governing_criterion"] += "; repair or higher-level assessment required"
    if unreliable:
        result["governing_criterion"] += "; parabolic strain estimate unreliable; depth-based rejection remains governing" if status == "REJECT" else "; parabolic profile unreliable: ESCALATE"
    return result


def router(cfg):
    """Manual geometry only; wall-thickness grids do not supply dent curvature."""
    block = cfg["dent_assessment"]
    geometry = _geometry(block)
    assessment = assess_dent(**geometry)
    decision = _decision(assessment, geometry)
    result = {"assessment": asdict(assessment), "decision": decision}
    sections = {"Manual geometry (inches)": {
        "OD (in)": assessment.od_in, "WT (in)": assessment.wt_in,
        "Depth (in)": assessment.dent_depth_in, "Depth / OD (1)": assessment.depth_ratio,
        "Axial length (in)": geometry["dent_length_axial_in"],
        "Circumferential length (in)": geometry["dent_length_circ_in"],
        "On weld": assessment.on_weld, "Restrained": assessment.restrained,
        "With gouge / metal loss": assessment.with_gouge_or_metal_loss},
        "ASME B31.8 Appendix R strain estimate (dimensionless)": asdict(assessment.strain),
        "Screening thresholds (ratios)": assessment.thresholds}
    limits = assessment.citations + assessment.validity_notes + [
        "API 579-1 Part 12 published worked examples: not evaluated; this is a geometric/strain screen.",
        "Dent detection from UT grids is not implemented: manual entry of dent geometry and feature flags is required.",
        "Parabolic apex radii assume reversed curvature in both planes; measure actual radii for sharp profiles.",
        "Gas-pipeline geometry screening only: fatigue, pressure cycling and quantitative dent-gouge fracture are not assessed.",
        "RSF, pressure rerating and remaining life are not calculated from dent strain.",
        "Validation record: docs/domains/asset-integrity/dent-validation-2026-10-09.md",
    ]
    result["report_html"] = FFSReport.generate_screening_html(
        block.get("component_id", "DENT"), "Dent screen", decision, sections, limits)
    cfg[cfg["basename"]] = store_report(result, block, "dent-report.html", cfg.get("_config_dir_path"))
    return cfg
