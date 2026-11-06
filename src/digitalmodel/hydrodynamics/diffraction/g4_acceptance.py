"""Separate AQG G4 evidence from advisory mesh-screen verdicts.

Requirement inventory and mass-residual definition: repository diffraction
knowledge pack, revision 4, section 5 (AQG r0, section 8, G4.1a--G4.8e).
The primary AQG document is not bundled here. No numerical acceptance limit is
inferred from a mesh checker or hardcoded: a criterion and its source are inputs.

Only free-flotation mass arithmetic is evaluated. Other checks retain explicit
missing-evidence records, so this module cannot return an overall G4 PASS.
Source references are declarations, not verified files, digests or native output.
The existing packaging policy and legacy displaced-mass screen are unchanged.
This module uses only the standard library and never imports a solver interface.
"""

from dataclasses import asdict, dataclass
from math import isfinite
from numbers import Real


REQUIREMENT_SOURCE = (
    "src/digitalmodel/hydrodynamics/diffraction/docs/agent-pack.md#5-qa-gates"
    " (revision 4; AQG r0 section 8)"
)

# Inventory only: the cited matrix supplies these IDs and evidence categories,
# not authority to turn generic geometry screens into numerical acceptance.
_REQUIRED_EVIDENCE = {
    "G4.1a": "Free-flotation input mass, independent volume, density and criterion",
    "G4.1b": "Restrained preload balance and common density/gravity evidence",
    "G4.2a": "Independent and native displaced-volume comparison",
    "G4.2b": "Independent axis-volume integrals and their agreement criterion",
    "G4.3a": "Native/independent buoyancy centres, waterline length and draft",
    "G4.3b": "Mass-model gravity centre and equilibrium attitude reconciliation",
    "G4.3c": "Independent transverse/longitudinal GM and native comparison",
    "G4.4a": "Panel count and installed-program version limit with its source",
    "G4.4b": "Panel aspects and any justified waterline exception",
    "G4.4c": "Maximum panel dimension and shortest analysed wavelength",
    "G4.4d": "Overlap/gap diagnostics against native program criteria",
    "G4.4e": "Native memory estimate and available memory at assessment time",
    "G4.5a": "Wetted boundary heights against waterline criterion",
    "G4.5b": "Waterline boundary-loop closure evidence",
    "G4.5c": "Mesh draft and design draft with their comparison criterion",
    "G4.5d": "Native clipping semantics and wetted/interior-lid classification",
    "G4.6a": "Outward panel orientation, signed volume and adjacency evidence",
    "G4.6b": "Panel area distribution and zero-area diagnostics",
    "G4.6c": "Symmetry declarations and full-body reconstruction evidence",
    "G4.7a": "Irregular-frequency estimate, analysed range and interior-lid evidence",
    "G4.7b": "Matched enabled removal established from both native program inputs",
    "G4.8a": "Isometric, waterline and panel-type views with verified digests",
    "G4.8b": "Longest-period heave criterion and finite-depth applicability",
    "G4.8c": "Diagonal damping signs in declared units",
    "G4.8d": "Added-mass symmetry criterion, units and zero-term treatment",
    "G4.8e": "Response spikes near irregular frequency and refinement evidence",
}


def _finite_number(value: object, name: str, *, positive: bool) -> float:
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f"{name} must be a real number")
    try:
        value = float(value)
    except (OverflowError, ValueError) as error:
        raise ValueError(f"{name} must be finite") from error
    if not isfinite(value) or value < 0 or (positive and value == 0):
        domain = "positive" if positive else "non-negative"
        raise ValueError(f"{name} must be finite and {domain}")
    return value


@dataclass(frozen=True)
class FreeFlotationMassEvidence:
    """Declared SI inputs for one unrestrained free-flotation assessment.

    All inputs shall refer to the same case and environment. Supplying this
    type declares free-flotation applicability; restrained/preloaded systems
    require G4.1b, which remains NOT_RUN. Sources shall identify the mass model,
    independent volume calculation, environment and governing criterion.
    Their contents and applicability are not authenticated by this arithmetic.
    """

    mass_kg: float
    displacement_m3: float
    water_density_kg_m3: float
    relative_tolerance: float
    criterion_source: str
    mass_source: str
    displacement_source: str
    density_source: str

    def __post_init__(self) -> None:
        for name in (
            "mass_kg",
            "displacement_m3",
            "water_density_kg_m3",
            "relative_tolerance",
        ):
            value = _finite_number(
                getattr(self, name),
                name,
                positive=name != "relative_tolerance",
            )
            object.__setattr__(self, name, value)
        for name in (
            "criterion_source",
            "mass_source",
            "displacement_source",
            "density_source",
        ):
            value = getattr(self, name)
            if not isinstance(value, str) or not value.strip():
                raise ValueError(f"{name} must be a non-empty source reference")


def _mass_record(evidence: FreeFlotationMassEvidence, record: dict) -> None:
    displaced = _finite_number(
        evidence.water_density_kg_m3 * evidence.displacement_m3,
        "displaced mass",
        positive=True,
    )
    # AQG r0 section 8 G4.1a, as transcribed in knowledge pack revision 4:
    # |M - rho V| / M. Common gravity cancels for free flotation only.
    residual = _finite_number(
        abs(evidence.mass_kg - displaced) / evidence.mass_kg,
        "relative residual",
        positive=False,
    )
    record.update(
        status="PASS" if residual <= evidence.relative_tolerance else "FAIL",
        applicability="APPLICABLE",
        criterion={
            "expression": "abs(M - rho V) / M <= relative_tolerance",
            "limit": evidence.relative_tolerance,
            "unit": "1",
            "source": evidence.criterion_source,
        },
        measurement={"value": residual, "unit": "1"},
        evidence=asdict(evidence) | {"displaced_mass_kg": displaced},
        reason=(
            "Free-flotation arithmetic against the supplied criterion only; "
            "source contents and AQG criterion qualification remain unverified."
        ),
    )


def assess_g4(
    *,
    mesh_screen_status: str | None = None,
    mass_balance: FreeFlotationMassEvidence | None = None,
) -> dict:
    """Return a fresh, JSON-ready G4 record, independently of mesh screening.

    Missing evidence/applicability is explicit, never NOT_APPLICABLE or PASS.
    A failed evaluated criterion gives FAIL even while other checks are NOT_RUN;
    otherwise the overall status is NOT_RUN because unsupported checks remain.
    ``mesh_screen_status`` is retained as context only. A mesh FAIL still blocks
    existing packaging through its own policy, outside this assessment.
    """
    if mesh_screen_status not in (None, "PASS", "WARNING", "FAIL", "SKIPPED"):
        raise ValueError("mesh_screen_status must be PASS, WARNING, FAIL or SKIPPED")
    if mass_balance is not None and not isinstance(
        mass_balance, FreeFlotationMassEvidence
    ):
        raise ValueError("mass_balance must be FreeFlotationMassEvidence or None")
    checks: dict[str, dict] = {
        check_id: {
            "status": "NOT_RUN",
            "applicability": "NOT_ASSESSED",
            "requirement_source": REQUIREMENT_SOURCE,
            "required_evidence": requirement,
            "criterion": None,
            "measurement": None,
            "evidence": {},
            "reason": "Applicability, qualified criterion and required evidence are missing.",
        }
        for check_id, requirement in _REQUIRED_EVIDENCE.items()
    }
    if mass_balance is not None:
        _mass_record(mass_balance, checks["G4.1a"])
    return {
        "assessment_scope": "AQG_G4_evidence",
        "status": "FAIL"
        if any(c["status"] == "FAIL" for c in checks.values())
        else "NOT_RUN",
        "mesh_screen_status": mesh_screen_status,
        "checks": checks,
        "limitations": [
            "Only free-flotation mass arithmetic is implemented; no overall G4 PASS is available.",
            "References are declared sources; source contents and digests are not verified.",
            "No native program was loaded and no solver response was evaluated.",
            "Mesh packaging policy and its tolerance defaults remain independent.",
        ],
    }
