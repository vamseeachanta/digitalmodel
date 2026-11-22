# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Prepare an in-memory tank handoff without evaluating structural capacity."""

from copy import deepcopy

from digitalmodel.asset_integrity.assessment.level3_escalation import Level3Handoff
from digitalmodel.asset_integrity.tank_settlement_workflow import assess


def prepare_tank_handoff(block: dict) -> Level3Handoff:
    """Validate screening intake and retain independent input/result snapshots.

    Args:
        block: Raw mapping accepted by ``tank_settlement_workflow.assess``.

    Returns:
        Triggered specialist handoff with unevaluated engineering checks.
        Triggering records unresolved screening, not a structural failure.

    Raises:
        ValueError: When the existing screening intake validation fails.
    """
    screening = assess(block)
    evidence = {
        "strain_limit": [
            "Qualified material law and limiting triaxial strain basis",
            "Validated local strain and stress-triaxiality result readback",
        ],
        "buckling": [
            "Governing buckling criteria and capacity-reduction basis",
            "Qualified bifurcation or imperfection-sensitive collapse results",
        ],
        "plastic_collapse": [
            "Verified load combinations, load factors and collapse criteria",
            "Qualified nonlinear collapse result and displacement-control interpretation",
        ],
        "weld_strain": [
            "Shell-to-bottom weld geometry and governing service strain criteria",
            "Validated weld strain result readback",
        ],
    }
    return Level3Handoff(
        triggered=True,
        triggers=list(screening["limitations"]),
        recommended_analysis=[
            "Qualify shell, bottom, annular ring, roof and stiffener geometry and loads",
            "Assess strain limit, buckling, plastic collapse and shell-to-bottom weld strain",
        ],
        data_package={
            "original_inputs": deepcopy(block),
            "screening": deepcopy(screening),
            "proposed_checks": {
                name: {"status": "NOT_EVALUATED", "value": None, "margin": None,
                       "required_evidence": required}
                for name, required in evidence.items()
            },
        },
        open_questions=[
            "Confirm survey datum, repeatability and reference condition; measured elevation "
            "is not an imposed displacement. Supply a qualified displacement derivation.",
            "Confirm angle origin and orientation. Screening uses degrees in [0, 360) "
            "and positive-up elevations relative to a common datum.",
            "Confirm units: tank lengths and survey distances in m; course thicknesses "
            "and elevations in mm. Raw survey values are retained without conversion.",
            "Supply course heights, shell-to-bottom junction, bottom and annular details, "
            "roof, top angle, wind girder and weld geometry with boundary conditions.",
            "Qualify material basis, constitutive law and limiting strain evidence.",
            "Supply product density, hydrostatic head, self-weight, optional wind and "
            "governing load combinations; fill height alone does not establish loads.",
            "Verify governing criteria and edition crosswalk for API 579-1/ASME FFS-1 "
            "Annex B1 (2007) and Annex 2D (2016/2021); no clause equivalence is established.",
            "Supply a published tank-settlement comparator, qualified native results "
            "and a justified comparison tolerance.",
        ],
        notes=(
            "Diagnostic screening only. No solver execution or availability probe is performed. "
            "No structural capacity, engineering margin, acceptance or remaining life is "
            "established. Measured elevation is not imposed displacement; diagnostic harmonics "
            "are not converted into a load. Plastic collapse requires interpretation as a "
            "secondary check for displacement-controlled settlement. Missing optional surveys "
            "remain missing. Resolve the open questions before structural assessment."
        ),
    )
