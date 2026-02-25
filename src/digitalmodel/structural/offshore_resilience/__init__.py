"""
digitalmodel.structural.offshore_resilience
============================================

Offshore resilience design framework — modular platforms, lifecycle planning,
and structural health monitoring.

Modules
-------
minimum_facility     : MinimumFacilityPlatform, ModularCostComparison,
                       BSEE classification, lifecycle value index
installation_checklist: Installation-centric engineering checklists for
                       fixed/floating structures and offshore wind
structural_health    : StructuralHealthRecord, sensor config templates,
                       alert threshold framework
lifecycle_planning   : Decommissioning-by-design flags, tieback accommodation
                       scoring, marginal field lifecycle score

WRK
---
WRK-221 — Offshore resilience design framework
Related: WRK-220 (decommissioning analytics), WRK-157 (fatigue budget)
"""

from .minimum_facility import (
    MinimumFacilityPlatform,
    ModularCostComparison,
    BseePlatformClass,
    InstallationMethod,
    classify_bsee_platform,
    lifecycle_value_index,
)

from .installation_checklist import (
    InstallationChecklist,
    ChecklistItem,
    ChecklistStatus,
    HammerType,
    FoundationType,
    build_structure_checklist,
    build_wind_foundation_checklist,
)

from .structural_health import (
    StructuralHealthRecord,
    AlertSeverity,
    MeasurementType,
    SensorConfig,
    AssetType,
    SensorTemplate,
    AlertThresholdConfig,
    build_sensor_template,
    build_alert_thresholds,
)

from .lifecycle_planning import (
    DecomFlag,
    DecomFlagSeverity,
    PipelineLayout,
    decom_flags_for_design_choices,
    score_tieback_accommodation,
    marginal_field_lifecycle_score,
)

__all__ = [
    # minimum_facility
    "MinimumFacilityPlatform",
    "ModularCostComparison",
    "BseePlatformClass",
    "InstallationMethod",
    "classify_bsee_platform",
    "lifecycle_value_index",
    # installation_checklist
    "InstallationChecklist",
    "ChecklistItem",
    "ChecklistStatus",
    "HammerType",
    "FoundationType",
    "build_structure_checklist",
    "build_wind_foundation_checklist",
    # structural_health
    "StructuralHealthRecord",
    "AlertSeverity",
    "MeasurementType",
    "SensorConfig",
    "AssetType",
    "SensorTemplate",
    "AlertThresholdConfig",
    "build_sensor_template",
    "build_alert_thresholds",
    # lifecycle_planning
    "DecomFlag",
    "DecomFlagSeverity",
    "PipelineLayout",
    "decom_flags_for_design_choices",
    "score_tieback_accommodation",
    "marginal_field_lifecycle_score",
]

__version__ = "1.0.0"
