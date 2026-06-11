"""DNV-RP-F106 factory-applied external pipeline coating helpers.

Implements a bounded coating-selection and inspection subset from
DNV-RP-F106 (2003) Sec. 5 and Annex 1 coating data sheets.
"""

from __future__ import annotations

from enum import Enum

from pydantic import BaseModel, ConfigDict, Field

from digitalmodel.citations import Citation, validate_citation


_F106_WIKI_PATH = "wikis/engineering-standards/wiki/standards/dnv-rp-f106.md"

_COATING_ALIASES = {
    "3LPE": "three_layer_pe",
    "3LPP": "three_layer_pp",
    "PE": "three_layer_pe",
    "PP": "three_layer_pp",
    "FBE": "fbe",
    "FUSION_BONDED_EPOXY": "fbe",
    "ASPHALT": "asphalt_enamel",
    "ASPHALT_ENAMEL": "asphalt_enamel",
    "COAL_TAR": "coal_tar_enamel",
    "COAL_TAR_ENAMEL": "coal_tar_enamel",
    "POLYCHLOROPRENE": "polychloroprene",
    "NEOPRENE": "polychloroprene",
}


class CoatingType(str, Enum):
    """Factory-applied external pipeline coating families."""

    FBE = "fbe"
    THREE_LAYER_PE = "three_layer_pe"
    THREE_LAYER_PP = "three_layer_pp"
    ASPHALT_ENAMEL = "asphalt_enamel"
    COAL_TAR_ENAMEL = "coal_tar_enamel"
    POLYCHLOROPRENE = "polychloroprene"


class CoatingProperties(BaseModel):
    """F106 coating-property envelope used by selection and inspection checks."""

    nominal_thickness_mm: float = Field(..., gt=0.0)
    min_thickness_mm: float | None = Field(default=None, ge=0.0)
    holiday_detection_voltage_v: float | None = Field(default=None, ge=0.0)
    service_temp_min_c: float
    service_temp_max_c: float
    thickness_is_project_specific: bool = False
    holiday_detection_is_project_specific: bool = False
    holiday_detection_is_thickness_dependent: bool = False
    source_note: str = ""


class SelectionResult(BaseModel):
    """Result of an F106 coating-family selection with citation metadata."""

    model_config = ConfigDict(arbitrary_types_allowed=True)

    coating_type: CoatingType
    service_temp_c: float
    mechanical_protection_required: bool
    expected_life_years: float = Field(..., gt=0.0)
    citation: Citation
    rationale: str


class ValidationResult(BaseModel):
    """Result of an F106 coating-thickness acceptance check."""

    model_config = ConfigDict(arbitrary_types_allowed=True)

    coating_type: CoatingType
    measured_thickness_mm: float = Field(..., ge=0.0)
    minimum_thickness_mm: float = Field(..., ge=0.0)
    is_valid: bool
    citation: Citation
    message: str


class HolidayDetectionResult(BaseModel):
    """Holiday-detection voltage result with its F106 citation sidecar."""

    model_config = ConfigDict(arbitrary_types_allowed=True)

    coating_type: CoatingType
    thickness_mm: float = Field(..., gt=0.0)
    voltage_v: float = Field(..., gt=0.0)
    citation: Citation


def _f106_citation(section: str, note: str = "") -> Citation:
    citation = Citation(
        code_id="dnv-rp-f106",
        publisher="DNV",
        revision="2003",
        section=section,
        wiki_path=_F106_WIKI_PATH,
        note=note,
    )
    validate_citation(citation)
    return citation


def _pe_pp_voltage(thickness_mm: float) -> float:
    return min(10000.0 * thickness_mm, 25000.0)


COATING_LIBRARY: dict[CoatingType, CoatingProperties] = {
    CoatingType.FBE: CoatingProperties(
        nominal_thickness_mm=0.5,
        min_thickness_mm=None,
        holiday_detection_voltage_v=None,
        service_temp_min_c=-45.0,
        service_temp_max_c=100.0,
        thickness_is_project_specific=True,
        holiday_detection_is_project_specific=True,
        source_note="Annex 1 CDS No.1: typical FBE thickness; min/max and holiday detection by project ITP.",
    ),
    CoatingType.THREE_LAYER_PE: CoatingProperties(
        nominal_thickness_mm=3.0,
        min_thickness_mm=2.0,
        holiday_detection_voltage_v=None,
        service_temp_min_c=-45.0,
        service_temp_max_c=110.0,
        holiday_detection_is_thickness_dependent=True,
        source_note="Annex 1 CDS No.2: total thickness minimum 2.0 mm.",
    ),
    CoatingType.THREE_LAYER_PP: CoatingProperties(
        nominal_thickness_mm=3.0,
        min_thickness_mm=2.5,
        holiday_detection_voltage_v=None,
        service_temp_min_c=-20.0,
        service_temp_max_c=140.0,
        holiday_detection_is_thickness_dependent=True,
        source_note="Annex 1 CDS No.3: total thickness minimum 2.5 mm.",
    ),
    CoatingType.ASPHALT_ENAMEL: CoatingProperties(
        nominal_thickness_mm=7.0,
        min_thickness_mm=5.0,
        holiday_detection_voltage_v=15000.0,
        service_temp_min_c=-10.0,
        service_temp_max_c=70.0,
        source_note="Annex 1 CDS No.5: asphalt enamel thickness 5-9 mm.",
    ),
    CoatingType.COAL_TAR_ENAMEL: CoatingProperties(
        nominal_thickness_mm=7.0,
        min_thickness_mm=5.0,
        holiday_detection_voltage_v=15000.0,
        service_temp_min_c=-10.0,
        service_temp_max_c=70.0,
        source_note="Annex 1 CDS No.6: coal-tar enamel thickness 5-9 mm.",
    ),
    CoatingType.POLYCHLOROPRENE: CoatingProperties(
        nominal_thickness_mm=1.0,
        min_thickness_mm=None,
        holiday_detection_voltage_v=None,
        service_temp_min_c=-20.0,
        service_temp_max_c=90.0,
        thickness_is_project_specific=True,
        holiday_detection_is_project_specific=True,
        source_note="Annex 1 CDS No.7: thickness and holiday detection per project ITP.",
    ),
}


def _coerce_coating_type(coating_type: CoatingType | str) -> CoatingType:
    if isinstance(coating_type, CoatingType):
        return coating_type
    normalized = str(coating_type).strip()
    alias_key = normalized.replace("-", "_").replace(" ", "_").upper()
    normalized = _COATING_ALIASES.get(alias_key, normalized.lower())
    try:
        return CoatingType(normalized)
    except ValueError as exc:
        raise ValueError(f"unknown coating_type: {coating_type!r}") from exc


def select_coating(
    service_temp_c: float,
    mechanical_protection_required: bool,
    expected_life_years: float,
) -> SelectionResult:
    """Select a conservative F106 coating family for the service envelope.

    Uses Sec. 5 material-selection factors and the Annex 1 data-sheet
    temperature envelopes. Temperatures at or above 110 deg C route to 3LPP.
    """
    if expected_life_years <= 0.0:
        raise ValueError("expected_life_years must be positive")
    if service_temp_c >= 110.0:
        candidate = CoatingType.THREE_LAYER_PP
        rationale = "service temperature at or above the 3LPE/3LPP selection boundary"
    elif service_temp_c > COATING_LIBRARY[CoatingType.FBE].service_temp_max_c:
        candidate = CoatingType.THREE_LAYER_PE
        rationale = "service temperature above the FBE service envelope"
    elif mechanical_protection_required or expected_life_years >= 30.0:
        candidate = CoatingType.THREE_LAYER_PE
        rationale = "mechanical protection or long design life favors 3LPE"
    else:
        candidate = CoatingType.FBE
        rationale = "service envelope permits the default FBE coating family"
    props = COATING_LIBRARY[candidate]
    if not props.service_temp_min_c <= service_temp_c <= props.service_temp_max_c:
        raise ValueError(
            f"service_temp_c={service_temp_c!r} outside {candidate.value} envelope"
        )
    return SelectionResult(
        coating_type=candidate,
        service_temp_c=service_temp_c,
        mechanical_protection_required=mechanical_protection_required,
        expected_life_years=expected_life_years,
        citation=_f106_citation(
            "§5 / Annex 1",
            "Coating-family selection factors and service-temperature envelopes.",
        ),
        rationale=rationale,
    )


def validate_thickness(
    coating_type: CoatingType | str,
    measured_thickness_mm: float,
    *,
    project_min_thickness_mm: float | None = None,
) -> ValidationResult:
    """Check coating thickness against F106 Sec. 5.4 / 5.6.5 acceptance basis."""
    coating = _coerce_coating_type(coating_type)
    if measured_thickness_mm < 0.0:
        raise ValueError("measured_thickness_mm must be non-negative")
    props = COATING_LIBRARY[coating]
    if props.thickness_is_project_specific:
        if project_min_thickness_mm is None or project_min_thickness_mm <= 0.0:
            raise ValueError(
                "project_min_thickness_mm is required and must be positive"
            )
        minimum = project_min_thickness_mm
    elif props.min_thickness_mm is None:
        raise ValueError(f"minimum thickness for {coating.value} is not configured")
    else:
        minimum = props.min_thickness_mm
    citation = _f106_citation(
        "§5.4 / §5.6.5", "Coating material and thickness control."
    )
    is_valid = measured_thickness_mm >= minimum
    return ValidationResult(
        coating_type=coating,
        measured_thickness_mm=measured_thickness_mm,
        minimum_thickness_mm=minimum,
        is_valid=is_valid,
        citation=citation,
        message="accepted" if is_valid else "below minimum coating thickness",
    )


def holiday_detection_result(
    coating_type: CoatingType | str,
    thickness_mm: float,
) -> HolidayDetectionResult:
    """Return F106 holiday-detection voltage with citation metadata."""
    coating = _coerce_coating_type(coating_type)
    if thickness_mm <= 0.0:
        raise ValueError("thickness_mm must be positive")
    props = COATING_LIBRARY[coating]
    if props.holiday_detection_is_project_specific:
        raise ValueError(
            f"holiday detection voltage for {coating.value} is project-specific"
        )
    if not validate_thickness(coating, thickness_mm).is_valid:
        raise ValueError("thickness_mm is below the F106 minimum for this coating")
    if coating in (CoatingType.THREE_LAYER_PE, CoatingType.THREE_LAYER_PP):
        voltage = _pe_pp_voltage(thickness_mm)
    elif coating in (CoatingType.ASPHALT_ENAMEL, CoatingType.COAL_TAR_ENAMEL):
        voltage = 15000.0
    else:
        raise ValueError(
            f"holiday detection voltage for {coating.value} is not configured"
        )
    return HolidayDetectionResult(
        coating_type=coating,
        thickness_mm=thickness_mm,
        voltage_v=voltage,
        citation=_f106_citation("§6.3 / Annex 1", "Holiday detection data-sheet rule."),
    )


def holiday_detection_voltage(
    coating_type: CoatingType | str, thickness_mm: float
) -> float:
    """Calculate the F106 holiday-detection voltage [V]."""
    return holiday_detection_result(coating_type, thickness_mm).voltage_v
