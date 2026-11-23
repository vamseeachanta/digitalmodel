"""API RP 2SM synthetic fiber rope mooring helpers."""

from __future__ import annotations

import csv
from enum import Enum
from pathlib import Path
from typing import Any

import yaml
from pydantic import BaseModel, ConfigDict, Field

from digitalmodel.citations import Citation, CitedValue, validate_citation
from digitalmodel.orcaflex.mooring_design import (
    MOORING_MATERIAL_LIBRARY,
    SegmentMaterial,
    SyntheticRopeProperties,
    SyntheticRopeStiffness,
)


_API_RP_2SM_WIKI_PATH = "wikis/engineering-standards/wiki/standards/api-rp-2sm.md"
_LOAD_HISTORY_COLUMNS = {
    "mean_load_pct_mbl",
    "dynamic_tension_range_pct_mbl",
    "low_tension_event",
}
_LOW_TENSION_TRUE_TOKENS = {"1", "true", "yes", "y"}
_LOW_TENSION_FALSE_TOKENS = {"", "0", "false", "no", "n"}


def _profile_config_path() -> Path:
    return Path(__file__).resolve().parent / "data" / "synthetic_rope_profiles.yml"


def _load_profile_config() -> dict[str, Any]:
    return yaml.safe_load(_profile_config_path().read_text(encoding="utf-8"))


_PROFILE_CONFIG = _load_profile_config()
_SYNTHETIC_DEFAULT_PROFILE_KEYS = {
    SegmentMaterial(material): profile_key
    for material, profile_key in _PROFILE_CONFIG["default_profile_keys"].items()
}
_FATIGUE_EVENT_THRESHOLDS = {
    resistance: (int(thresholds["watch"]), int(thresholds["critical"]))
    for resistance, thresholds in _PROFILE_CONFIG["fatigue_event_thresholds"].items()
}


class LoadHistory(BaseModel):
    """Condensed API RP 2SM §5.4 synthetic-rope load-history statistics."""

    mean_load_pct_mbl: float = Field(..., ge=0.0, le=100.0)
    dynamic_tension_range_pct_mbl: float = Field(..., ge=0.0, le=100.0)
    low_tension_events: int = Field(default=0, ge=0)

    @classmethod
    def from_csv(cls, path: str | Path) -> "LoadHistory":
        reader = csv.DictReader(Path(path).read_text(encoding="utf-8").splitlines())
        missing = _LOAD_HISTORY_COLUMNS - set(reader.fieldnames or [])
        if missing:
            missing_list = ", ".join(sorted(missing))
            raise ValueError(f"load-history CSV missing columns: {missing_list}")
        rows = list(reader)
        if not rows:
            raise ValueError("load-history CSV must contain at least one row")
        mean_loads: list[float] = []
        dynamic_ranges: list[float] = []
        low_tension_events = 0
        for row_number, row in enumerate(rows, start=2):
            try:
                mean_load = float(row["mean_load_pct_mbl"])
                dynamic_range = float(row["dynamic_tension_range_pct_mbl"])
            except (TypeError, ValueError) as exc:
                raise ValueError(
                    f"load-history CSV row {row_number} contains non-numeric load data"
                ) from exc
            if mean_load < 0.0:
                raise ValueError("mean_load_pct_mbl must be non-negative")
            if mean_load > 100.0:
                raise ValueError("mean_load_pct_mbl must not exceed 100")
            if dynamic_range < 0.0:
                raise ValueError("dynamic_tension_range_pct_mbl must be non-negative")
            if dynamic_range > 100.0:
                raise ValueError("dynamic_tension_range_pct_mbl must not exceed 100")
            mean_loads.append(mean_load)
            dynamic_ranges.append(dynamic_range)
            low_tension_token = str(row.get("low_tension_event", "")).strip().lower()
            if low_tension_token in _LOW_TENSION_TRUE_TOKENS:
                low_tension_events += 1
            elif low_tension_token not in _LOW_TENSION_FALSE_TOKENS:
                raise ValueError(
                    f"low_tension_event has invalid token on row {row_number}"
                )
        return cls(
            mean_load_pct_mbl=sum(mean_loads) / len(mean_loads),
            dynamic_tension_range_pct_mbl=max(dynamic_ranges),
            low_tension_events=low_tension_events,
        )


class CompressionFatigueSeverity(str, Enum):
    """Synthetic-rope axial compression fatigue verdict."""

    OK = "ok"
    WATCH = "watch"
    CRITICAL = "critical"


class CompressionFatigueVerdict(BaseModel):
    """Axial-compression fatigue check result with citation sidecar."""

    model_config = ConfigDict(arbitrary_types_allowed=True)

    material: SegmentMaterial
    low_tension_events: int = Field(..., ge=0)
    severity: CompressionFatigueSeverity
    citation: Citation
    message: str


class ManufacturerSpec(BaseModel):
    """Manufacturing and installation QA evidence supplied by a rope vendor."""

    manufacturer: str
    material: SegmentMaterial
    has_prototype_test: bool = False
    has_splice_qualification: bool = False
    has_traceability: bool = False
    installation_tension_recorded: bool = False


class QAItem(BaseModel):
    """API RP 2SM manufacturing or installation QA checklist item."""

    model_config = ConfigDict(arbitrary_types_allowed=True)

    stage: str
    requirement: str
    is_satisfied: bool
    citation: Citation


class RopeSelectionResult(BaseModel):
    """API RP 2SM §4 synthetic-rope material selection with citation sidecar."""

    model_config = ConfigDict(arbitrary_types_allowed=True)

    material: SegmentMaterial
    citation: Citation
    basis: str


def _api_rp_2sm_citation(section: str, note: str = "") -> Citation:
    citation = Citation(
        code_id="api-rp-2sm",
        publisher="API",
        revision="2001+2007",
        section=section,
        wiki_path=_API_RP_2SM_WIKI_PATH,
        note=note,
    )
    validate_citation(citation)
    return citation


def _coerce_material(material: SegmentMaterial | str) -> SegmentMaterial:
    if isinstance(material, SegmentMaterial):
        return material
    normalized = str(material).strip().replace("-", "_").lower()
    try:
        return SegmentMaterial(normalized)
    except ValueError as exc:
        raise ValueError(f"unsupported synthetic rope material: {material!r}") from exc


def _synthetic_profile_entry(material: SegmentMaterial | str):
    if isinstance(material, str) and material in MOORING_MATERIAL_LIBRARY:
        props = MOORING_MATERIAL_LIBRARY[material]
        if props.synthetic_stiffness and props.synthetic_properties:
            return props
        raise ValueError(f"library profile is not synthetic: {material}")
    mat = _coerce_material(material)
    profile_key = _SYNTHETIC_DEFAULT_PROFILE_KEYS.get(mat)
    if profile_key:
        props = MOORING_MATERIAL_LIBRARY[profile_key]
        if props.synthetic_stiffness and props.synthetic_properties:
            return props
    raise ValueError(f"no synthetic rope profile configured for {mat.value}")


def _synthetic_profile(
    material: SegmentMaterial | str,
) -> tuple[SyntheticRopeStiffness, SyntheticRopeProperties]:
    props = _synthetic_profile_entry(material)
    return props.synthetic_stiffness, props.synthetic_properties


def _low_tension_event_count(envelope: dict[str, Any]) -> int:
    raw = envelope.get("low_tension_event_count", 0)
    if isinstance(raw, bool) or raw is None:
        raise ValueError("low_tension_event_count must be a non-negative integer")
    if isinstance(raw, int):
        value = raw
    elif isinstance(raw, str) and raw.strip().isdigit():
        value = int(raw)
    else:
        raise ValueError("low_tension_event_count must be a non-negative integer")
    if value < 0:
        raise ValueError("low_tension_event_count must be non-negative")
    return value


def select_rope_material(
    service: str,
    water_depth_m: float,
    target_life_years: float,
    motion_envelope: dict[str, Any] | None = None,
) -> SegmentMaterial:
    """Return only the API RP 2SM §4 material enum for legacy callers.

    Use select_rope_material_result() when the citation sidecar must be retained.
    """
    return select_rope_material_result(
        service,
        water_depth_m,
        target_life_years,
        motion_envelope,
    ).material


def select_rope_material_result(
    service: str,
    water_depth_m: float,
    target_life_years: float,
    motion_envelope: dict[str, Any] | None = None,
) -> RopeSelectionResult:
    """Select a material and retain the API RP 2SM §4 design-basis citation."""
    if water_depth_m <= 0.0:
        raise ValueError("water_depth_m must be positive")
    if target_life_years <= 0.0:
        raise ValueError("target_life_years must be positive")
    envelope = motion_envelope or {}
    low_tension_events = _low_tension_event_count(envelope)
    service_key = service.strip().lower().replace("-", "_")

    if "temporary" in service_key:
        material = SegmentMaterial.NYLON
        basis = "temporary service screened to nylon default"
    elif low_tension_events >= 100_000:
        material = SegmentMaterial.HMPE
        basis = "high low-tension cycling screened away from polyester default"
    elif water_depth_m >= 1000.0:
        material = SegmentMaterial.POLYESTER
        basis = "deepwater mooring screened to polyester default"
    elif "permanent" in service_key and target_life_years >= 25.0:
        material = SegmentMaterial.POLYESTER
        basis = "long-life permanent mooring screened to polyester default"
    elif "taut" in service_key:
        material = SegmentMaterial.POLYESTER
        basis = "taut mooring service screened to polyester default"
    else:
        material = SegmentMaterial.NYLON
        basis = "bounded shallow-water fallback for preliminary screening"

    return RopeSelectionResult(
        material=material,
        citation=_api_rp_2sm_citation(
            "§4",
            "Synthetic fiber rope design-basis and material selection screening.",
        ),
        basis=basis,
    )


def axial_stiffness_for_load_history(
    material: SegmentMaterial | str,
    load_history: LoadHistory,
) -> CitedValue:
    """Return API RP 2SM §5.4 mean-load adjusted axial stiffness."""
    stiffness, _ = _synthetic_profile(material)
    uplift = 1.0 + (
        (stiffness.mean_load_factor - 1.0)
        * max(load_history.mean_load_pct_mbl, 0.0)
        / 10.0
    )
    mean_adjusted = stiffness.post_installation_stiffness_kn * uplift
    mean_adjusted = min(mean_adjusted, stiffness.dynamic_storm_stiffness_kn)
    if load_history.dynamic_tension_range_pct_mbl >= 50.0:
        value = stiffness.dynamic_storm_stiffness_kn
    elif load_history.dynamic_tension_range_pct_mbl > 35.0:
        storm_fraction = (load_history.dynamic_tension_range_pct_mbl - 35.0) / 15.0
        value = (
            mean_adjusted
            + (stiffness.dynamic_storm_stiffness_kn - mean_adjusted) * storm_fraction
        )
    else:
        value = mean_adjusted
    return CitedValue(
        value=round(value, 3),
        units="kN",
        citation=_api_rp_2sm_citation(
            "§5.4",
            "Mean-load adjusted synthetic-rope axial stiffness.",
        ),
    )


def creep_elongation(
    material: SegmentMaterial | str,
    mean_load_pct_mbl: float,
    service_years: float,
) -> CitedValue:
    """Estimate API RP 2SM §5.5 long-term creep for bounded screening.

    The local coefficient is a per-10-calendar-year screening rate, calibrated
    to the issue #584 polyester example rather than a licensed standard table.
    """
    if mean_load_pct_mbl < 0.0:
        raise ValueError("mean_load_pct_mbl must be non-negative")
    if service_years <= 0.0:
        raise ValueError("service_years must be positive")
    _, properties = _synthetic_profile(material)
    value = (
        properties.creep_rate_pct_per_decade
        * (service_years / 10.0)
        * (mean_load_pct_mbl / 20.0)
    )
    return CitedValue(
        value=round(value, 6),
        units="%",
        citation=_api_rp_2sm_citation("§5.5", "Synthetic-rope creep screening."),
    )


def axial_compression_fatigue_check(
    material: SegmentMaterial | str,
    low_tension_events: int,
) -> CompressionFatigueVerdict:
    """Flag API RP 2SM §5.6 low-tension axial-compression fatigue risk."""
    props = _synthetic_profile_entry(material)
    mat = props.material
    if low_tension_events < 0:
        raise ValueError("low_tension_events must be non-negative")
    resistance = props.synthetic_properties.axial_compression_fatigue_resistance
    if resistance not in _FATIGUE_EVENT_THRESHOLDS:
        raise ValueError(f"unknown axial compression fatigue resistance: {resistance}")
    watch_threshold, critical_threshold = _FATIGUE_EVENT_THRESHOLDS[resistance]
    if low_tension_events >= critical_threshold:
        severity = CompressionFatigueSeverity.CRITICAL
        message = f"{mat.value} low-tension cycling requires redesign or qualification"
    elif low_tension_events >= watch_threshold:
        severity = CompressionFatigueSeverity.WATCH
        message = "low-tension cycling requires project fatigue review"
    else:
        severity = CompressionFatigueSeverity.OK
        message = "low-tension cycling is inside the screening envelope"
    return CompressionFatigueVerdict(
        material=mat,
        low_tension_events=low_tension_events,
        severity=severity,
        citation=_api_rp_2sm_citation(
            "§5.6",
            "Axial-compression fatigue screening for low-tension cycling.",
        ),
        message=message,
    )


def qa_program(
    material: SegmentMaterial | str,
    manufacturer_spec: ManufacturerSpec,
) -> list[QAItem]:
    """Build a bounded API RP 2SM §7/§8 manufacturing and installation checklist."""
    mat = _synthetic_profile_entry(material).material
    if manufacturer_spec.material != mat:
        raise ValueError("manufacturer_spec.material must match material")
    manufacturing = _api_rp_2sm_citation(
        "§7",
        "Manufacturing qualification and traceability checklist.",
    )
    installation = _api_rp_2sm_citation(
        "§8",
        "Installation handling and as-installed tension records.",
    )
    return [
        QAItem(
            stage="manufacturing",
            requirement="prototype rope qualification test is documented",
            is_satisfied=manufacturer_spec.has_prototype_test,
            citation=manufacturing,
        ),
        QAItem(
            stage="manufacturing",
            requirement="splice qualification is documented",
            is_satisfied=manufacturer_spec.has_splice_qualification,
            citation=manufacturing,
        ),
        QAItem(
            stage="manufacturing",
            requirement="rope traceability records are complete",
            is_satisfied=manufacturer_spec.has_traceability,
            citation=manufacturing,
        ),
        QAItem(
            stage="installation",
            requirement="installation tension records are available",
            is_satisfied=manufacturer_spec.installation_tension_recorded,
            citation=installation,
        ),
    ]
