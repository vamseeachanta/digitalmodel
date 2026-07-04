# ABOUTME: FFS Level 3 escalation interface — decides WHEN to escalate beyond
# ABOUTME: L1/L2 screening and builds a structured handoff package for the office.
"""Level 3 (advanced assessment) escalation interface for FFS.

The Phase-1 FFS pipeline gives a fast field/desk answer:

* :mod:`...assessment.level1_screener` — minimum-thickness screening,
* :mod:`...assessment.level2_engine` — closed-form RSF (returns ``rsf``,
  ``rsf_a``, ``t_mm_in``, ``assessment_type``, ``folias_factor``),
* :mod:`...assessment.ffs_decision` — a verdict (ACCEPT / MONITOR / RE_RATE /
  REPAIR / REPLACE),
* :mod:`...assessment.measurement_sufficiency` — is the field data adequate.

Those Level 1/2 methods are *closed-form metal-loss* assessments (API 579-1 /
ASME FFS-1 Parts 4 & 5). They deliberately do **not** cover crack-like (planar)
flaws, strongly interacting defect colonies, or geometry that the closed-form
Folias/RSF treatment cannot represent. For those, API 579-1 / ASME FFS-1
directs the assessor to a **Level 3** advanced assessment: numerical (FEA)
stress analysis and, for crack-like flaws, a BS 7910 Failure Assessment Diagram
(FAD) / fracture-mechanics evaluation.

This module is the *escalation interface* between quick screening and that
advanced work. It does **two** things and no more:

1. :func:`should_escalate_to_level3` — the decision logic for *when* a case
   must leave the L1/L2 screening track.
2. :func:`escalate_to_level3` — assembles a complete, structured
   :class:`Level3Handoff` package (geometry, full measurement summary, loads,
   material, and the L1/L2 results) plus the recommended analyses and the
   specific open questions the specialist must close out.

**Honesty note.** Level 3 *here* is an INTERFACE and a DATA CONTRACT, not the
advanced computation itself. This module does not run FEA and does not solve a
FAD; it decides escalation and produces the handoff so that fast L1/L2
screening never silently substitutes for the detailed analysis a flaw needs.
The Level-2 fracture tier for a single surface flaw is implemented in
:mod:`digitalmodel.asset_integrity.assessment.crack_fad` (#1270) — run it
on the handoff's flaw dimensions before commissioning FEA; the legacy
config-driven path remains at
:func:`digitalmodel.asset_integrity.fracture_mechanics.fracture_mechanics`.
"""

from __future__ import annotations

from dataclasses import dataclass, field

# RSF at/below which the Level-2 closed-form re-rating is no longer applicable;
# the component is in repair/replace territory and needs a Level-3 / engineering
# assessment regardless of further screening. Mirrors the severe-loss floor used
# by the measurement-sufficiency engine.
_DEFAULT_RSF_FLOOR = 0.50


@dataclass
class Level3Handoff:
    """A complete, structured handoff to a Level 3 specialist / the office.

    Attributes:
        triggered: Whether escalation to Level 3 is required.
        triggers: Human-readable reasons escalation was (or was not) triggered.
        recommended_analysis: Advanced analyses to perform, e.g.
            ``"nonlinear FEA"``, ``"BS7910 FAD fracture"``,
            ``"interacting-defect FEA"``.
        data_package: Echo of every input so the specialist has a complete
            record — ``geometry``, ``grid_summary``, ``loads``, ``material``,
            ``level1_result`` and ``level2_result``.
        open_questions: Specific questions the specialist must resolve.
        notes: Free-text framing / caveats for the handoff.
    """

    triggered: bool
    triggers: list = field(default_factory=list)
    recommended_analysis: list = field(default_factory=list)
    data_package: dict = field(default_factory=dict)
    open_questions: list = field(default_factory=list)
    notes: str = ""


def should_escalate_to_level3(
    level1_result: dict,
    level2_result: dict,
    *,
    has_crack_like_flaw: bool = False,
    interacting_defects: bool = False,
    complex_geometry: bool = False,
    rsf_floor: float = _DEFAULT_RSF_FLOOR,
) -> tuple[bool, list]:
    """Decide whether a case must escalate from L1/L2 screening to Level 3.

    Escalation is triggered when **any** of the following hold:

    * **Severe metal loss** — the remaining-strength factor is below the
      Level-2 applicability / re-rating floor (``rsf < rsf_floor``); the
      closed-form re-rating no longer applies.
    * **Crack-like (planar) flaw** — outside the metal-loss L1/L2 scope; this is
      BS 7910 / fracture-mechanics territory.
    * **Interacting defects** — a colony the closed-form single-flaw RSF cannot
      represent.
    * **Complex geometry** — discontinuities/shapes the closed-form Folias/RSF
      treatment cannot represent.
    * **Level 2 fails** — ``rsf < rsf_a`` with no viable re-rate (the Level-2
      assessment itself does not pass).

    Args:
        level1_result: dict from the Level 1 screener (uses ``verdict`` if
            present, informational only here).
        level2_result: dict from the Level 2 engine; needs ``rsf`` and
            ``rsf_a``.
        has_crack_like_flaw: Flaw is crack-like / planar rather than volumetric
            metal loss.
        interacting_defects: Multiple flaws interact (a colony).
        complex_geometry: Geometry the closed-form method cannot represent.
        rsf_floor: RSF below which Level-2 re-rating is inapplicable.

    Returns:
        ``(triggered, reasons)`` where ``triggered`` is a Python ``bool`` and
        ``reasons`` is a list of human-readable trigger strings (empty when not
        triggered).
    """
    rsf = float(level2_result.get("rsf", 0.0))
    rsf_a = float(level2_result.get("rsf_a", 0.0))

    reasons: list = []

    if rsf < float(rsf_floor):
        reasons.append(
            f"RSF={rsf:.3f} is below the Level-2 re-rating floor of "
            f"{float(rsf_floor):.2f} — the closed-form re-rating no longer "
            "applies; severe metal loss requires a Level 3 / detailed "
            "assessment."
        )

    if has_crack_like_flaw:
        reasons.append(
            "A crack-like (planar) flaw is present — this is outside the "
            "metal-loss scope of Level 1/2 and requires a BS 7910 "
            "fracture-mechanics (FAD) assessment."
        )

    if interacting_defects:
        reasons.append(
            "Interacting defects (a flaw colony) are present — the closed-form "
            "single-flaw RSF cannot represent their interaction; a numerical "
            "(FEA) assessment is required."
        )

    if complex_geometry:
        reasons.append(
            "Complex geometry / structural discontinuity is present that the "
            "closed-form Folias/RSF treatment cannot represent; a numerical "
            "(FEA) assessment is required."
        )

    # Level 2 itself fails with no viable re-rate. Only meaningful when an RSFa
    # is supplied (rsf_a > 0); guard against a still-passing case that already
    # tripped the floor above.
    if rsf_a > 0.0 and rsf < rsf_a:
        reasons.append(
            f"Level 2 fails: RSF={rsf:.3f} is below RSFa={rsf_a:.2f} with no "
            "viable re-rate — a Level 3 assessment is required to qualify "
            "continued service."
        )

    return bool(len(reasons) > 0), reasons


def escalate_to_level3(
    geometry: dict,
    loads: dict,
    material: dict,
    level1_result: dict,
    level2_result: dict,
    *,
    grid_summary: dict | None = None,
    has_crack_like_flaw: bool = False,
    interacting_defects: bool = False,
    complex_geometry: bool = False,
    rsf_floor: float = _DEFAULT_RSF_FLOOR,
) -> Level3Handoff:
    """Build a complete Level 3 handoff package from the screening inputs.

    This assembles the data contract a Level 3 specialist needs. It does not
    perform the advanced analysis; it records the case, decides escalation via
    :func:`should_escalate_to_level3`, selects the recommended analyses from the
    triggers, and lists the specific open questions to close out.

    Args:
        geometry: Component geometry (OD, wall thickness, flaw dims, ...).
        loads: Applied loads (pressure, temperature, axial/bending, ...).
        material: Material data (grade, SMYS/SMTS, toughness if available, ...).
        level1_result: dict from the Level 1 screener.
        level2_result: dict from the Level 2 engine (``rsf``, ``rsf_a``, ...).
        grid_summary: Optional summary of the full measurement grid (counts,
            min thickness, scatter, ...). Echoed into the data package.
        has_crack_like_flaw: See :func:`should_escalate_to_level3`.
        interacting_defects: See :func:`should_escalate_to_level3`.
        complex_geometry: See :func:`should_escalate_to_level3`.
        rsf_floor: RSF below which Level-2 re-rating is inapplicable.

    Returns:
        A fully-populated :class:`Level3Handoff`.
    """
    rsf = float(level2_result.get("rsf", 0.0))
    rsf_a = float(level2_result.get("rsf_a", 0.0))

    triggered, triggers = should_escalate_to_level3(
        level1_result,
        level2_result,
        has_crack_like_flaw=has_crack_like_flaw,
        interacting_defects=interacting_defects,
        complex_geometry=complex_geometry,
        rsf_floor=rsf_floor,
    )

    severe_metal_loss = (rsf < float(rsf_floor)) or (rsf_a > 0.0 and rsf < rsf_a)

    # --- Recommended analyses, selected from the active triggers -------------
    recommended_analysis: list = []
    if has_crack_like_flaw:
        # Crack-like flaw -> BS 7910 FAD via the fracture-mechanics router.
        recommended_analysis.append(
            "BS7910 FAD fracture assessment (crack-like flaw) — via "
            "digitalmodel.asset_integrity.fracture_mechanics.fracture_mechanics"
        )
    if interacting_defects:
        recommended_analysis.append(
            "interacting-defect FEA (flaw-colony stress interaction)"
        )
    if complex_geometry:
        recommended_analysis.append(
            "numerical FEA stress analysis for complex geometry / discontinuity"
        )
    if severe_metal_loss:
        recommended_analysis.append(
            "nonlinear FEA / detailed RSF (elastic-plastic) for severe metal loss"
        )

    # --- Specific open questions for the specialist --------------------------
    open_questions: list = []
    if has_crack_like_flaw:
        open_questions.append(
            "Confirm the flaw is planar (crack-like) vs volumetric metal loss."
        )
        open_questions.append(
            "Provide CVN / fracture toughness (Kmat or J) for the FAD assessment."
        )
        open_questions.append(
            "Characterise flaw type (surface vs embedded) and depth/length (a, 2c)."
        )
    if interacting_defects:
        open_questions.append(
            "Provide a full 3D scan of the interacting flaw colony with "
            "ligament spacings to confirm/deny interaction."
        )
    if complex_geometry:
        open_questions.append(
            "Provide as-built geometry / discontinuity detail and the local "
            "stress state (SCF or measured strains) at the feature."
        )
    if severe_metal_loss:
        open_questions.append(
            "Confirm the operating/design loads and temperature for an "
            "elastic-plastic (nonlinear) RSF assessment."
        )
        open_questions.append(
            "Confirm the future corrosion allowance / remaining-life basis for "
            "re-rate vs repair vs replace."
        )

    # --- Complete record: echo every input so the specialist has it all ------
    data_package = {
        "geometry": geometry,
        "grid_summary": grid_summary if grid_summary is not None else {},
        "loads": loads,
        "material": material,
        "level1_result": level1_result,
        "level2_result": level2_result,
    }

    if triggered:
        notes = (
            "Level 3 escalation REQUIRED. This package is an interface and data "
            "contract for an advanced (FEA / BS 7910 FAD) assessment — the "
            "computation is not performed here. Quick L1/L2 screening does not "
            "eliminate the need for the detailed analysis listed above."
        )
    else:
        notes = (
            "Level 3 escalation NOT required by the screening triggers; the "
            "Level 1/2 closed-form result stands. This package is retained as a "
            "complete record of the assessed case."
        )

    return Level3Handoff(
        triggered=bool(triggered),
        triggers=triggers,
        recommended_analysis=recommended_analysis,
        data_package=data_package,
        open_questions=open_questions,
        notes=notes,
    )
