# ABOUTME: Composite repair candidate screening for FFS REPAIR verdicts.
# ABOUTME: Maps service/defect context to ISO 24817 / ASME PCC-2 repair classes.
"""Composite repair recommendation helper.

This module screens candidate composite repair classes for an FFS result that
already requires physical repair. It intentionally does not perform laminate
sizing or installation-provider selection; those remain project-specific
engineering tasks.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Mapping

BASIS_NOTES = (
    "ISO 24817 (composite repairs for pipework)",
    "ASME PCC-2, non-metallic composite repair articles",
)
EM_DASH = "\u2014"
REPAIR_CLASSES: dict[str, str] = {
    "A": "through-wall capable",
    "B": "axial reinforcement",
    "C": "hoop/pressure reinforcement",
    "D": "axial and structural reinforcement",
    "E": "structural-only reinforcement",
}
REPAIR_CLASS_ORDER = tuple(REPAIR_CLASSES)


@dataclass(frozen=True)
class CompositeRepairParameters:
    """Practice defaults for conservative candidate screening.

    These are operator-practice screening defaults, not values claimed from
    ISO 24817 or ASME PCC-2 tables.
    """

    default_temperature_f: float = 100.0
    default_repair_life_yr: float = 20.0
    reinspection_fraction_of_repair_life: float = 0.5
    max_pressure_psi_by_class: Mapping[str, float] = field(
        default_factory=lambda: {
            "A": 2500.0,
            "B": 2500.0,
            "C": 2500.0,
            "D": 2500.0,
            "E": 0.0,
        }
    )
    max_temperature_f_by_class: Mapping[str, float] = field(
        default_factory=lambda: {klass: 180.0 for klass in REPAIR_CLASS_ORDER}
    )
    allow_steam_service: bool = False

    def __post_init__(self) -> None:
        for name in (
            "default_temperature_f",
            "default_repair_life_yr",
            "reinspection_fraction_of_repair_life",
        ):
            if getattr(self, name) <= 0:
                raise ValueError(f"{name} must be positive")
        if self.reinspection_fraction_of_repair_life > 1.0:
            raise ValueError("reinspection_fraction_of_repair_life must be <= 1")
        _validate_class_limits(
            "max_pressure_psi_by_class", self.max_pressure_psi_by_class
        )
        _validate_class_limits(
            "max_temperature_f_by_class", self.max_temperature_f_by_class
        )

    def basis_notes(self) -> list[str]:
        """Document-level standards basis for public payloads."""
        return list(BASIS_NOTES)


@dataclass(frozen=True)
class RepairContext:
    """Operating and defect context used to screen repair classes."""

    active_leak: bool = False
    through_wall_now: bool = False
    through_wall_within_repair_life: bool = False
    defect_type: str = "external_metal_loss"
    service: str = "oil"
    pressure_psi: float = 0.0
    temperature_f: float = 100.0
    repair_life_yr: float = 20.0
    axial_load: bool = False

    @classmethod
    def from_mapping(
        cls,
        value: "RepairContext | Mapping[str, Any] | None",
        params: CompositeRepairParameters,
    ) -> "RepairContext":
        """Build a context from optional caller overrides."""
        if isinstance(value, cls):
            return value
        data = dict(value or {})
        data.setdefault("temperature_f", params.default_temperature_f)
        data.setdefault("repair_life_yr", params.default_repair_life_yr)
        return cls(**data)

    def __post_init__(self) -> None:
        if self.service.lower() not in {"water", "oil", "gas", "steam"}:
            raise ValueError(f"unsupported service {self.service!r}")
        if self.pressure_psi < 0:
            raise ValueError("pressure_psi must be non-negative")
        if self.temperature_f <= 0:
            raise ValueError("temperature_f must be positive")
        if self.repair_life_yr <= 0:
            raise ValueError("repair_life_yr must be positive")


@dataclass(frozen=True)
class CompositeRepairRecommendation:
    """Candidate classes and screening reasons for a composite repair."""

    candidate_classes: list[str]
    excluded: dict[str, str]
    iso_defect_type: str
    standards_basis: list[str]
    reinspection_interval_yr: float
    summary: str

    def to_dict(self) -> dict[str, Any]:
        """Return a JSON-friendly payload for FFS reports and APIs."""
        return {
            "candidate_classes": list(self.candidate_classes),
            "excluded": dict(self.excluded),
            "iso_defect_type": self.iso_defect_type,
            "standards_basis": list(self.standards_basis),
            "reinspection_interval_yr": float(self.reinspection_interval_yr),
            "summary": self.summary,
        }


def recommend_from_ffs_result(
    result: Any,
    repair_context: RepairContext | Mapping[str, Any] | None = None,
    *,
    params: CompositeRepairParameters | None = None,
) -> CompositeRepairRecommendation | None:
    """Return a recommendation only when the FFS verdict is ``REPAIR``."""
    if str(getattr(result, "verdict", "")).upper() != "REPAIR":
        return None
    params = params or CompositeRepairParameters()
    return recommend_composite_repair(repair_context, params=params)


def recommend_composite_repair(
    repair_context: RepairContext | Mapping[str, Any] | None = None,
    *,
    params: CompositeRepairParameters | None = None,
) -> CompositeRepairRecommendation:
    """Screen candidate repair classes from defect and operating context."""
    params = params or CompositeRepairParameters()
    context = RepairContext.from_mapping(repair_context, params)
    iso_type = _iso_defect_type(context)
    pool, excluded = _class_pool(context)
    candidates: list[str] = []

    for repair_class in pool:
        reason = _screening_exclusion(repair_class, context, params)
        if reason:
            excluded[repair_class] = reason
        else:
            candidates.append(repair_class)

    reinspection_interval = (
        context.repair_life_yr * params.reinspection_fraction_of_repair_life
    )
    summary = _summary(candidates, reinspection_interval)
    return CompositeRepairRecommendation(
        candidate_classes=candidates,
        excluded={k: excluded[k] for k in REPAIR_CLASS_ORDER if k in excluded},
        iso_defect_type=iso_type,
        standards_basis=params.basis_notes(),
        reinspection_interval_yr=reinspection_interval,
        summary=summary,
    )


def _iso_defect_type(context: RepairContext) -> str:
    if (
        context.active_leak
        or context.through_wall_now
        or context.through_wall_within_repair_life
    ):
        return "Type B"
    return "Type A"


def _class_pool(context: RepairContext) -> tuple[list[str], dict[str, str]]:
    if _is_through_wall(context):
        return ["A"], {
            "B": "load-transfer class does not seal active through-wall loss",
            "C": "pressure-only class does not seal active through-wall loss",
            "D": "load-transfer class does not seal active through-wall loss",
            "E": "structural class does not seal pressure boundary leaks",
        }
    if context.defect_type.lower() == "structural_only":
        return ["E"], {
            "A": "through-wall leak repair class not required",
            "B": "axial pressure-boundary reinforcement not required",
            "C": "pressure-only reinforcement not required",
            "D": "combined pressure/axial reinforcement not required",
        }
    if context.axial_load:
        return ["B", "D"], {
            "A": "through-wall leak repair class not required",
            "C": "pressure-only class does not address axial load",
            "E": "structural-only class does not restore pressure boundary",
        }
    return ["C", "D"], {
        "A": "through-wall leak repair class not required",
        "B": "axial load-transfer class not required",
        "E": "structural-only class does not restore pressure boundary",
    }


def _is_through_wall(context: RepairContext) -> bool:
    return (
        context.active_leak
        or context.through_wall_now
        or context.through_wall_within_repair_life
    )


def _screening_exclusion(
    repair_class: str,
    context: RepairContext,
    params: CompositeRepairParameters,
) -> str | None:
    service = context.service.lower()
    if service == "steam" and not params.allow_steam_service:
        return "steam service requires project-specific laminate design"
    max_pressure = params.max_pressure_psi_by_class[repair_class]
    max_temperature = params.max_temperature_f_by_class[repair_class]
    if context.pressure_psi > max_pressure:
        return f"pressure {context.pressure_psi:g} psi exceeds screening default"
    if context.temperature_f > max_temperature:
        return f"temperature {context.temperature_f:g} F exceeds screening default"
    return None


def _summary(candidates: list[str], reinspection_interval_yr: float) -> str:
    classes = ", ".join(candidates) if candidates else "none"
    return (
        f"REPAIR {EM_DASH} candidate repair class(es): {classes}, "
        f"re-inspection interval {reinspection_interval_yr:.1f} yr "
        "per repair-life design"
    )


def _validate_class_limits(name: str, values: Mapping[str, float]) -> None:
    missing = set(REPAIR_CLASS_ORDER) - set(values)
    if missing:
        raise ValueError(f"{name} missing classes: {sorted(missing)}")
    for repair_class, value in values.items():
        if repair_class not in REPAIR_CLASSES:
            raise ValueError(f"{name} has unknown class {repair_class!r}")
        if value < 0:
            raise ValueError(f"{name}[{repair_class!r}] must be non-negative")
