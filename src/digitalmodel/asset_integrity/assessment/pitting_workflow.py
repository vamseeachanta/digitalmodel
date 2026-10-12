"""Grid-only Part 6 philosophy screen and Part 5 equivalent-LTA estimate (inches)."""
from dataclasses import replace

import numpy as np
import pandas as pd

from digitalmodel.asset_integrity.applicability import Applicability, merge
from .ffs_decision import decide
from .ffs_report import FFSReport
from .pitting import (
    DEFAULT_PIT_THRESHOLD_FRACTION, DEFAULT_RT_MIN, characterize_pit_field,
    screen_pitting_level1, assess_pitting_level2_equivalent_lta,
)
from .screening_report import finite_number, store_report


def _read_input(block):
    od = finite_number(block, "nominal_od_in")
    wt = finite_number(block, "nominal_wt_in")
    minimum = finite_number(block, "t_min_in")
    spacing = finite_number(block, "row_spacing_in")
    fca = finite_number(block, "fca_in", default=0.0, positive=False)
    rsfa = finite_number(block, "rsf_a", default=0.9)
    rt_min = finite_number(block, "rt_min", default=DEFAULT_RT_MIN)
    threshold = finite_number(block, "pit_threshold_fraction",
                              default=DEFAULT_PIT_THRESHOLD_FRACTION)
    if od <= 2 * wt or minimum > wt or fca >= wt:
        raise ValueError("Require OD > 2 WT, t_min <= WT and FCA < WT")
    if rsfa < 0.9 or rt_min < DEFAULT_RT_MIN:
        raise ValueError("Workflow cannot relax the existing RSFa or deepest-ligament defaults")
    if max(rsfa, rt_min, threshold) > 1:
        raise ValueError("RSFa, rt_min and pit threshold must be in (0, 1]")
    if threshold != DEFAULT_PIT_THRESHOLD_FRACTION:
        raise ValueError("Workflow pit population threshold is fixed at the existing default")
    raw = np.asarray(block["grid"])
    if raw.dtype.kind not in "iuf":
        raise ValueError("grid readings must be numeric measurements, not strings or booleans")
    values = raw.astype(float)
    if (values.ndim != 2 or not values.size or not np.isfinite(values).all()
            or np.any(values <= 0) or np.any(values > wt)):
        raise ValueError("grid must be rectangular finite remaining thickness in (0, WT]")
    grid = pd.DataFrame(values, index=np.arange(values.shape[0]) * spacing,
                        columns=np.arange(values.shape[1]) * spacing)
    return grid, od, wt, minimum, fca, rsfa, rt_min, threshold


def _levels(char, od, wt, minimum, fca, rsfa, rt_min):
    if char.pit_count == 0:
        note = "No pits identified: use the metal-loss workflow on measured wall."
        return ({"verdict": "not evaluated", "basis": note},
                {"rsf": None, "verdict": "not evaluated", "assessment_basis": note})
    l1 = screen_pitting_level1(char, t_min_in=minimum, fca_in=fca, rt_min=rt_min)
    effective = replace(char, t_avg_pit_in=char.t_avg_pit_in - fca,
                        t_min_pit_in=char.t_min_pit_in - fca,
                        avg_pit_depth_in=char.avg_pit_depth_in + fca)
    if effective.t_min_pit_in <= 0:
        return l1, {"rsf": None, "verdict": "not evaluated",
                    "assessment_basis": "FCA consumes the deepest ligament; strength is not evaluated."}
    if effective.pitted_region_row_count == 1:
        # Two half-pitch rows represent the same uniform region and preserve
        # its physical length despite the engine's single-index spacing default.
        effective = replace(effective, pitted_region_row_count=2,
                            row_spacing_in=effective.row_spacing_in / 2)
    l2 = assess_pitting_level2_equivalent_lta(
        effective, nominal_od_in=od, nominal_wt_in=wt, t_min_in=minimum, rsf_a=rsfa)
    l2["assessment_basis"] = (
        "Uniform synthetic pit field represented by an equivalent local thin area "
        "using mean net pit wall and axial extent, evaluated with the existing "
        "simplified Part 5 engine. A bound on actual pitting failure mechanisms "
        "and published-case qualification are not established."
    )
    return l1, l2


def _applicability(grid, char, minimum, fca, rt_min, l2):
    result = l2.get("applicability", Applicability())
    checks = [
        (char.pit_count == 0, "pitting.no_pits",
         "No pits identified; a pitting disposition is not evaluated. Use measured-wall metal-loss assessment."),
        ((char.t_min_pit_in - fca) / char.nominal_wt_in < rt_min,
         "pitting.deep_ligament", "Remaining deepest ligament after FCA is below rt_min; detailed assessment required."),
    ]
    values = grid.to_numpy()
    background = values[values >= char.pit_threshold_in]
    checks.extend([
        (not background.size, "pitting.no_background",
         "No background wall readings establish the equivalent-LTA reference; use metal-loss assessment."),
        (bool(background.size) and float(background.min()) - fca < minimum,
         "pitting.background_below_tmin",
         "Background wall after FCA is below the t_min reference; use metal-loss assessment."),
    ])
    pits = values[values < char.pit_threshold_in]
    checks.append((pits.size > 0 and np.ptp(pits) > 0, "pitting.nonuniform_depth",
                   "Nonuniform pit depths: mean-depth equivalent-LTA conservatism is not established."))
    for failed, flag, note in checks:
        if failed:
            result = merge(result, Applicability(ok=False, flags=[flag], notes=[note]))
    return result


def _assess(block):
    grid, od, wt, minimum, fca, rsfa, rt_min, threshold = _read_input(block)
    char = characterize_pit_field(grid, wt, pit_threshold_fraction=threshold)
    spacing = float(block["row_spacing_in"])
    char = replace(char, row_spacing_in=spacing,
                   pitted_region_length_in=char.pitted_region_row_count * spacing)
    if char.pit_count == 0:
        char = replace(char, t_min_pit_in=float(grid.to_numpy().min()),
                       t_avg_pit_in=float(grid.to_numpy().mean()))
    l1, l2 = _levels(char, od, wt, minimum, fca, rsfa, rt_min)
    applicability = _applicability(grid, char, minimum, fca, rt_min, l2)
    margin = l2["rsf"] if l2["rsf"] is not None else float("nan")
    # Level 2 supersedes the Level 1 mean-wall screen on RSF; independent
    # ligament and method applicability gates still override the disposition.
    passed = np.isfinite(margin) and margin >= rsfa
    # Failed/unsupported screens have no established rerating or life basis:
    # a non-finite margin invokes the shared ESCALATE path before its bands.
    decision = decide(margin if passed else float("nan"), rsfa, float("nan"),
                      "pipeline", screening_pass=passed, applicability=applicability).to_dict()
    strength = l2["rsf"] if l2["rsf"] is not None else "not evaluated"
    decision["governing_criterion"] = (
        f"Part 5 equivalent-LTA estimate: RSF={strength}, RSFa={rsfa}; "
        f"mean effective pit wall={char.t_avg_pit_in - fca:.3f} in, required={minimum:.3f} in; "
        f"screening disposition={decision['verdict']}. "
        "Synthetic formula screening only; published-case qualification and "
        "measured-component acceptance are not established. "
        "Life and pressure rerating are not evaluated; Part 6 coupled-pit charts are not evaluated."
    )
    if not applicability.ok:
        decision["governing_criterion"] += " " + " ".join(applicability.notes)
    for key in ("remaining_life_yr", "rsf", "margin"):
        if not np.isfinite(decision[key]):
            decision[key] = None
    if "applicability" in l2:
        l2["applicability"] = l2["applicability"].to_dict()
    return char, l1, l2, decision


def router(cfg):
    """Engine cfg adapter for the committed grid-only offline example."""
    block = cfg["pitting_assessment"]
    char, l1, l2, decision = _assess(block)
    result = {"characterization": char.to_dict(), "level1": l1,
              "level2": l2, "decision": decision,
              "qualification": "synthetic_formula_anchor_only"}
    sections = {"Pit characterization": char.to_dict(),
                "Level 1 closed-form screen (Part 6 philosophy; no charts)": l1,
                "Level 2 Part 5 equivalent-LTA estimate": l2}
    limits = [
        "API 579-1/ASME FFS-1 Part 6 charts and coupled-pit examples: not evaluated.",
        "The equivalent LTA is an assumed representation of uniform synthetic pits; "
        "a bound on actual pitting failure mechanisms and published-case qualification are not established.",
        "Inputs are remaining wall thickness and axial pitch in inches; FCA is deducted once for Level 2.",
        "Deepest effective ligament / nominal WT must meet rt_min; Level 2 requires RSF >= RSFa. "
        "The Level 1 mean-wall criterion is not repeated as a Level 2 acceptance gate.",
        "Measured background wall after FCA must meet the t_min reference; "
        "missing or thinner background requires metal-loss assessment.",
        "Required wall t_min and FCA are user-supplied engineering inputs; pressure capacity is not calculated.",
        "Grid rows are axial; pit spacing assumes equal axial/circumferential pitch. Circumferential extent check: not evaluated.",
        "Absolute ligament and distance-to-discontinuity checks are not implemented; code-qualified acceptance is not established.",
        "Uniform synthetic pit fields only; readings above nominal WT are rejected, not clipped. "
        "Pit-pair input is not implemented.",
        "Validation record: docs/domains/asset-integrity/pitting-validation-2026-10-09.md",
    ]
    result["report_html"] = FFSReport.generate_screening_html(
        block.get("component_id", "PITTING"), "Pitting screen", decision, sections, limits)
    cfg[cfg["basename"]] = store_report(result, block, "pitting-report.html", cfg.get("_config_dir_path"))
    return cfg
