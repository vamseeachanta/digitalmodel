"""Unbonded flexible-pipe evidence screen; qualified dispositions remain open.

Sources are public regulator guidance, not a reproduction of licensed API
criteria. No count-based remaining-capacity or generic flooded-life model is
qualified by this module. ESCALATE means obtain a pipe-specific assessment.
"""

from __future__ import annotations

from typing import Literal

from pydantic import BaseModel, ConfigDict, Field, model_validator

FIELD_EXPERIENCE = (
    "Un-bonded Flexible Risers - Recent Field Experience and Actions for "
    "Increased Robustness (2013), revision 5"
)
FIELD_EXPERIENCE_URL = (
    "https://www.havtil.no/contentassets/c2a5bd00e8214411ad5c4966009d6ade/"
    "un-bonded-flexible-risers--recent-field-experience-and-actions--for-increased-robustness.pdf"
)
HSE_URL = "https://www.hse.gov.uk/pipelines/resources/flexiblepipe.htm"


class FlexiblePipeEvidence(BaseModel):
    """Inspection observations; None/unknown differs from zero/absent damage.

    Fraction bounds below are definitions of count fractions/multipliers,
    not material coefficients or API acceptance limits. Allowables and life
    factors have no defaults; each supplied value requires its own locator.
    """

    model_config = ConfigDict(
        extra="forbid", strict=True, frozen=True, allow_inf_nan=False
    )
    outer_sheath_breach_location: Literal[
        "unknown", "none", "above_waterline", "submerged"
    ] = "unknown"
    annulus_test: Literal[
        "unknown", "dry", "flooded", "leak_indicated", "inconclusive"
    ] = "unknown"
    vent_status: Literal["unknown", "clear", "blocked"] = "unknown"
    total_armour_wires: int | None = Field(default=None, gt=0)
    broken_armour_wires: int | None = Field(default=None, ge=0)
    allowable_broken_fraction: float | None = Field(default=None, ge=0, le=1)
    allowable_fraction_reference: str | None = None
    end_fitting_age_years: float | None = Field(default=None, ge=0)
    load_history_available: bool | None = None
    carcass_collapse_indicated: bool | None = None
    pressure_sheath_damage_indicated: bool | None = None
    design_fatigue_life_years: float | None = Field(default=None, gt=0)
    flooded_life_factor: float | None = Field(default=None, gt=0, le=1)
    flooded_factor_reference: str | None = None

    @model_validator(mode="after")
    def check_consistency(self):
        """Reject impossible counts and untraceable assessment inputs."""
        if (
            self.total_armour_wires is not None
            and self.broken_armour_wires is not None
            and self.broken_armour_wires > self.total_armour_wires
        ):
            raise ValueError("broken_armour_wires cannot exceed total_armour_wires")
        for value, reference in (
            (self.allowable_broken_fraction, self.allowable_fraction_reference),
            (self.flooded_life_factor, self.flooded_factor_reference),
        ):
            if (value is None) != (reference is None):
                raise ValueError(
                    "assessment value and document/section reference must be paired"
                )
            if reference is not None and not reference.strip():
                raise ValueError(
                    "assessment reference must name a document and locator"
                )
        return self


# Grouped evidence taxonomy: field-experience report (2013), revision 5,
# sections 4.3-4.6 and 5.4.1. Grouping is for intake, not an API verdict table.
_MODES = (
    (
        "outer_sheath_annulus",
        "Outer sheath damage / annulus flooding",
        ("outer_sheath_breach_location", "annulus_test", "vent_status"),
        "4.4; 5.6; 5.7",
        "Locate sheath damage, establish the annulus environment and check vent function.",
    ),
    (
        "tensile_armour",
        "Tensile armour corrosion / rupture",
        ("total_armour_wires", "broken_armour_wires", "annulus_test"),
        "4.4; 4.6; 5.5",
        "Obtain qualified damage sizing, wire load sharing and local capacity assessment.",
    ),
    (
        "end_fitting_fatigue",
        "End-fitting fatigue",
        ("end_fitting_age_years", "load_history_available"),
        "5.4.1",
        "Assess termination stresses using load history and environment-specific fatigue data.",
    ),
    (
        "carcass_collapse",
        "Carcass collapse",
        ("carcass_collapse_indicated",),
        "4.3; 4.6",
        "Obtain pressure history, liner/carcass geometry and a qualified collapse assessment.",
    ),
    (
        "pressure_sheath",
        "Pressure-sheath creep / rupture",
        ("pressure_sheath_damage_indicated",),
        "4.3; 4.6; 5.4.1",
        "Obtain polymer condition, temperature/pressure exposure and material qualification.",
    ),
)

LIMITATIONS = (
    "NOT_EVALUATED: no qualified mapping to MONITOR, REPAIR or REPLACE is supplied; "
    "ESCALATE is an evidence-gap action, not an API disposition criterion.",
    "NOT_EVALUATED: remaining armour capacity requires geometry, corrosion sizing, "
    "loads, stress concentration and qualified load-sharing analysis; count is not capacity.",
    "NOT_EVALUATED: flooded-annulus fatigue life requires environment-specific fatigue "
    "data, load history and accumulated damage; an input multiplier alone is insufficient.",
    "A below-limit broken-wire count does not establish fitness for service. "
    "An input limit and its locator are recorded without asserting qualification.",
    "Published numerical capacity/life validation: NOT_EVALUATED. "
    "Published incident evidence is used only to check intake routing.",
)


def _wire_inventory(evidence):
    """Count identities only; no structural model is inferred (2013, sec. 5.5)."""
    total, broken = evidence.total_armour_wires, evidence.broken_armour_wires
    fraction = intact = None
    comparison = "NOT_EVALUATED"
    if total is not None and broken is not None:
        # Definition of count fraction b/N and intact count N-b; independent
        # hand calculation is written out in test_flexible_pipe.py. Neither
        # quantity represents remaining area or load capacity (2013, sec. 5.5).
        fraction, intact = broken / total, total - broken
        if evidence.allowable_broken_fraction is not None:
            limit = evidence.allowable_broken_fraction
            comparison = (
                "ABOVE_INPUT_LIMIT"
                if fraction > limit
                else "AT_INPUT_LIMIT"
                if fraction == limit
                else "BELOW_INPUT_LIMIT"
            )
    return {
        "intact_wire_count": intact,
        "broken_fraction": fraction,
        "allowable_broken_fraction": evidence.allowable_broken_fraction,
        "allowable_fraction_reference": evidence.allowable_fraction_reference,
        "input_limit_comparison": comparison,
        "capacity_status": "NOT_EVALUATED",
    }


def screen_flexible_pipe(evidence: dict | FlexiblePipeEvidence) -> dict:
    """Return cited evidence rows, count statistics and explicit assessment gaps."""
    model = FlexiblePipeEvidence.model_validate(evidence)
    inputs = model.model_dump()
    rows = []
    for key, title, fields, sections, action in _MODES:
        observed = {field: inputs[field] for field in fields}
        missing = [
            field
            for field, value in observed.items()
            if value is None
            or value in ("unknown", "inconclusive")
            or (field == "load_history_available" and value is False)
        ]
        rows.append(
            {
                "mode": key,
                "title": title,
                "evidence": observed,
                "missing_inputs": missing,
                "status": "NOT_EVALUATED",
                "verdict": "ESCALATE",
                "next_action": action,
                "citation": f"{FIELD_EXPERIENCE}, sections {sections}",
            }
        )
    return {
        "evidence": inputs,
        "failure_modes": rows,
        "wire_inventory": _wire_inventory(model),
        "remaining_capacity": None,
        "remaining_fatigue_life_years": None,
        "decision": {
            "verdict": "ESCALATE",
            "governing_criterion": "Qualified pipe-specific capacity, life and disposition criteria are missing.",
        },
        "limitations": list(LIMITATIONS),
        "sources": [
            {
                "title": FIELD_EXPERIENCE,
                "url": FIELD_EXPERIENCE_URL,
                "locator": "sections 4.3-4.6, 5.4.1, 5.5-5.7; Figure 3-7",
            },
            {
                "title": "HSE, Unbonded flexible pipe inspection issues and recommendations (2008)",
                "url": HSE_URL,
                "locator": "Good practice guidance; Recommendations",
            },
        ],
    }
