"""Cathodic protection calculations — API RP 1632, ISO 15589-2, DNV-RP-B401
(2005-2021) and DNV-RP-F103 cited tables and bracelet design, the shared
formula kernel, and impressed current fuel system CP design."""

from typing import Any

from digitalmodel.cathodic_protection._edition import (
    DEFAULT_EDITION,
    DEFAULT_F103_EDITION,
    Edition,
    F103Edition,
    f103_standard_for_edition,
    normalize_edition,
    normalize_f103_edition,
    standard_for_edition,
)

# --- Cited, edition-keyed DNV-RP-B401 / DNV-RP-F103 table lookups (#2207) ---

from digitalmodel.cathodic_protection.b401_tables import (
    AnodeEnvironment as B401AnodeEnvironment,
    AnodeMaterial as B401AnodeMaterial,
    AnodeShape as B401AnodeShape,
    Climate as B401Climate,
    DepthBand as B401DepthBand,
    DesignPhase as B401DesignPhase,
    PaintCategory as B401PaintCategory,
    anode_capacity as b401_anode_capacity,
    anode_closed_circuit_potential as b401_anode_closed_circuit_potential,
    buried_current_density as b401_buried_current_density,
    citation_label,
    climate_from_temperature as b401_climate_from_temperature,
    coating_breakdown_constants as b401_coating_breakdown_constants,
    depth_band as b401_depth_band,
    design_current_density as b401_design_current_density,
    design_driving_voltage as b401_design_driving_voltage,
    edition_provenance,
    protection_potential as b401_protection_potential,
    reinforcement_current_density as b401_reinforcement_current_density,
    utilisation_factor as b401_utilisation_factor,
)
from digitalmodel.cathodic_protection.b401_tables import (
    edition_provenance as b401_edition_provenance,
)
from digitalmodel.cathodic_protection.f103_tables import (
    Exposure as F103Exposure,
    FieldJointCoating as F103FieldJointCoating,
    FluidTemperatureBand as F103FluidTemperatureBand,
    LinepipeCoating as F103LinepipeCoating,
    b401_edition_for_f103 as f103_b401_edition_for_f103,
    bracelet_utilisation_factor as f103_bracelet_utilisation_factor,
    edition_provenance as f103_edition_provenance,
    field_joint_coating_constants as f103_field_joint_coating_constants,
    fluid_temperature_band as f103_fluid_temperature_band,
    linepipe_coating_constants as f103_linepipe_coating_constants,
    mean_current_density as f103_mean_current_density,
)

# --- Shared formula kernel (#2211) ---

from digitalmodel.cathodic_protection._kernels import (
    anode_count as kernel_anode_count,
    anode_current_output as kernel_anode_current_output,
    anode_mass as kernel_anode_mass,
    anodes_for_current as kernel_anodes_for_current,
    coating_breakdown_final as kernel_coating_breakdown_final,
    coating_breakdown_linear as kernel_coating_breakdown_linear,
    coating_breakdown_mean as kernel_coating_breakdown_mean,
    current_demand as kernel_current_demand,
    equivalent_radius_from_mass as kernel_equivalent_radius_from_mass,
    equivalent_radius_from_periphery as kernel_equivalent_radius_from_periphery,
    long_flush as kernel_long_flush,
    long_slender_standoff as kernel_long_slender_standoff,
    mass_consumed as kernel_mass_consumed,
    resistance_proximity_factor as kernel_resistance_proximity_factor,
    short_flush_or_bracelet as kernel_short_flush_or_bracelet,
    short_slender_standoff as kernel_short_slender_standoff,
    slender_standoff as kernel_slender_standoff,
)

# --- DNV-RP-F103 bracelet anode design (#2211) ---

from digitalmodel.cathodic_protection.dnv_rp_f103 import (
    BraceletDesignInput as F103BraceletDesignInput,
    BraceletDesignResult as F103BraceletDesignResult,
    design_bracelet_cp as f103_design_bracelet_cp,
    protected_length as f103_protected_length,
)

from digitalmodel.cathodic_protection.api_rp_1632 import (
    anode_driving_voltage,
    anode_life_years,
    anode_resistance_vertical_rod,
    check_protection_potential,
    current_demand,
    current_per_anode,
    number_of_anodes,
)
from digitalmodel.cathodic_protection.iso_15589_2 import (
    anode_mass_requirement,
    anode_output_current,
    anode_resistance,
    coating_breakdown_factor,
    initial_current_density,
    pipeline_current_demand,
)
from digitalmodel.cathodic_protection.iso_15589_2 import (
    check_protection_potential as iso_check_protection_potential,
)
from digitalmodel.cathodic_protection.dnv_rp_b401 import (
    anode_current_output as dnv_anode_current_output,
    anode_mass_requirement as dnv_anode_mass_requirement,
    anode_resistance_slender_standoff,
    coating_breakdown_factor as dnv_coating_breakdown_factor,
    current_demand as dnv_current_demand,
    equivalent_radius_from_mass,
    flush_anode_resistance,
    number_of_anodes as dnv_number_of_anodes,
    protected_length,
)
from digitalmodel.cathodic_protection.dnv_rp_f106 import (
    COATING_LIBRARY as F106_COATING_LIBRARY,
    CoatingProperties as F106CoatingProperties,
    CoatingType as F106CoatingType,
    HolidayDetectionResult as F106HolidayDetectionResult,
    SelectionResult as F106SelectionResult,
    ValidationResult as F106ValidationResult,
    holiday_detection_result as f106_holiday_detection_result,
    holiday_detection_voltage as f106_holiday_detection_voltage,
    select_coating as f106_select_coating,
    validate_thickness as f106_validate_thickness,
)

from digitalmodel.cathodic_protection.fuel_system_cp import (
    CoatingType,
    FuelPipeSegment,
    ImpressedCurrentGroundBed,
    RectifierOutput,
    check_protection,
    current_demand_segment,
    design_ground_bed,
    design_rectifier,
    effective_bare_area,
    pipe_surface_area,
    total_current_demand,
)

# --- New modules ---

from digitalmodel.cathodic_protection.coating import (
    CoatingCategory,
    CoatingBreakdownResult,
    CoatingConstants,
    CoatingLifeResult,
    coating_breakdown_factors,
    coating_constants,
    coating_life_estimate,
    effective_bare_area_coated,
)

from digitalmodel.cathodic_protection.pipeline_cp import (
    PipelineEnvironment,
    PipelineCPInput,
    PipelineCPResult,
    AnodeSpacingResult,
    pipeline_current_demand as pipeline_cp_current_demand,
    anode_spacing as pipeline_anode_spacing,
    holiday_detection_voltage,
)

from digitalmodel.cathodic_protection.marine_structure_cp import (
    ExposureZone,
    ClimateRegion,
    DesignLoopResult,
    StructuralZone,
    MarineCPResult,
    RetrofitAssessment,
    marine_structure_current_demand,
    standoff_anode_design_loop,
    anode_distribution,
    retrofit_assessment,
)

from digitalmodel.cathodic_protection.iccp_design import (
    AnodeBedType,
    AnodeMaterial,
    RectifierSizingInput,
    RectifierSizingResult,
    AnodeBedResult,
    rectifier_sizing,
    anode_bed_design,
    cable_sizing,
)

from digitalmodel.cathodic_protection.cp_survey import (
    CISSurveyPoint,
    CISAnalysisResult,
    DCVGIndication,
    AttenuationResult,
    analyze_cis_survey,
    classify_dcvg_indication,
    attenuation_analysis,
)

from digitalmodel.cathodic_protection.corrosion_rate import (
    CO2CorrosionInput,
    CO2CorrosionResult,
    GalvanicCorrosionInput,
    GalvanicCorrosionResult,
    de_waard_milliams_co2,
    norsok_m506_co2,
    galvanic_corrosion,
    pitting_rate_estimate,
)

from digitalmodel.cathodic_protection.anode_depletion import (
    AnodeStatus,
    DepletionResult,
    DepletionProfile,
    InspectionRecommendation,
    calculate_remaining_life,
    generate_depletion_profile,
    recommend_inspection_interval,
)

from digitalmodel.cathodic_protection.cp_monitoring import (
    ReferenceElectrodeType,
    MonitoringEnvironment,
    ReferenceElectrodeRecommendation,
    MonitoringSystemSpec,
    AlarmThresholds,
    select_reference_electrode,
    design_monitoring_system,
    set_alarm_thresholds,
)

from digitalmodel.cathodic_protection.stray_current import (
    InterferenceType,
    MitigationType,
    StrayCurrentInput,
    StrayCurrentResult,
    MitigationDesign,
    assess_stray_current,
    design_drainage_bond,
)

from digitalmodel.cathodic_protection.cp_reporting import (
    ComplianceStatus,
    RecommendationPriority,
    ComplianceCheck,
    Recommendation,
    RemainingLifeSummary,
    CPAssessmentReport,
    compliance_check_potential,
    generate_assessment_report,
    remaining_life_summary,
)

from digitalmodel.cathodic_protection.anode_sizing import (
    AnodeType,
    AnodeSizingInput,
    AnodeSizingResult,
    calculate_current_demand as sizing_current_demand,
    calculate_anode_mass as sizing_anode_mass,
    calculate_anode_resistance as sizing_anode_resistance,
    depleted_equivalent_radius as sizing_depleted_equivalent_radius,
    design_cp_system,
)

# marine_cp is a deprecated facade over marine_structure_cp (#2211);
# ``design_marine_cp`` is resolved lazily so that the DeprecationWarning
# fires on first access, not on package import.
from digitalmodel.cathodic_protection import marine_cp as _marine_cp
from digitalmodel.cathodic_protection.marine_cp import (
    ZoneType as MarineZoneType,
    Zone as MarineZone,
    MarineCPInput,
    MarineCPResult as MarineCPDesignResult,
    get_seawater_current_density,
    calculate_zone_demand,
)

from digitalmodel.cathodic_protection.pipeline_cp import (
    CriteriaResult,
    PipelineDesignResult,
    calculate_pipeline_current_demand,
    calculate_anode_spacing,
    check_potential_criteria,
    design_pipeline_cp,
    soil_resistivity_correction,
)

__all__ = [
    "DEFAULT_EDITION",
    "DEFAULT_F103_EDITION",
    "Edition",
    "F103Edition",
    "normalize_edition",
    "normalize_f103_edition",
    "standard_for_edition",
    "f103_standard_for_edition",
    "edition_provenance",
    # b401_tables
    "B401AnodeEnvironment",
    "B401AnodeMaterial",
    "B401AnodeShape",
    "B401Climate",
    "B401DepthBand",
    "B401DesignPhase",
    "B401PaintCategory",
    "b401_anode_capacity",
    "b401_anode_closed_circuit_potential",
    "b401_buried_current_density",
    "b401_climate_from_temperature",
    "b401_coating_breakdown_constants",
    "b401_depth_band",
    "b401_design_current_density",
    "b401_design_driving_voltage",
    "b401_edition_provenance",
    "b401_protection_potential",
    "b401_reinforcement_current_density",
    "b401_utilisation_factor",
    "citation_label",
    # f103_tables
    "F103Exposure",
    "F103FieldJointCoating",
    "F103FluidTemperatureBand",
    "F103LinepipeCoating",
    "f103_b401_edition_for_f103",
    "f103_bracelet_utilisation_factor",
    "f103_edition_provenance",
    "f103_field_joint_coating_constants",
    "f103_fluid_temperature_band",
    "f103_linepipe_coating_constants",
    "f103_mean_current_density",
    # _kernels
    "kernel_anode_count",
    "kernel_anode_current_output",
    "kernel_anode_mass",
    "kernel_anodes_for_current",
    "kernel_coating_breakdown_final",
    "kernel_coating_breakdown_linear",
    "kernel_coating_breakdown_mean",
    "kernel_current_demand",
    "kernel_equivalent_radius_from_mass",
    "kernel_equivalent_radius_from_periphery",
    "kernel_long_flush",
    "kernel_long_slender_standoff",
    "kernel_mass_consumed",
    "kernel_resistance_proximity_factor",
    "kernel_short_flush_or_bracelet",
    "kernel_short_slender_standoff",
    "kernel_slender_standoff",
    # dnv_rp_f103
    "F103BraceletDesignInput",
    "F103BraceletDesignResult",
    "f103_design_bracelet_cp",
    "f103_protected_length",
    "anode_driving_voltage",
    "anode_resistance_vertical_rod",
    "current_demand",
    "current_per_anode",
    "number_of_anodes",
    "anode_life_years",
    "check_protection_potential",
    "initial_current_density",
    "coating_breakdown_factor",
    "pipeline_current_demand",
    "anode_resistance",
    "anode_output_current",
    "anode_mass_requirement",
    "iso_check_protection_potential",
    "dnv_current_demand",
    "dnv_anode_mass_requirement",
    "dnv_coating_breakdown_factor",
    "anode_resistance_slender_standoff",
    "dnv_anode_current_output",
    "equivalent_radius_from_mass",
    "flush_anode_resistance",
    "dnv_number_of_anodes",
    "protected_length",
    "F106_COATING_LIBRARY",
    "F106CoatingProperties",
    "F106CoatingType",
    "F106HolidayDetectionResult",
    "F106SelectionResult",
    "F106ValidationResult",
    "f106_holiday_detection_result",
    "f106_holiday_detection_voltage",
    "f106_select_coating",
    "f106_validate_thickness",
    "CoatingType",
    "FuelPipeSegment",
    "ImpressedCurrentGroundBed",
    "RectifierOutput",
    "check_protection",
    "current_demand_segment",
    "design_ground_bed",
    "design_rectifier",
    "effective_bare_area",
    "pipe_surface_area",
    "total_current_demand",
    # coating
    "CoatingCategory",
    "CoatingBreakdownResult",
    "CoatingConstants",
    "CoatingLifeResult",
    "coating_breakdown_factors",
    "coating_constants",
    "coating_life_estimate",
    "effective_bare_area_coated",
    # pipeline_cp
    "PipelineEnvironment",
    "PipelineCPInput",
    "PipelineCPResult",
    "AnodeSpacingResult",
    "pipeline_cp_current_demand",
    "pipeline_anode_spacing",
    "holiday_detection_voltage",
    # marine_structure_cp
    "ExposureZone",
    "ClimateRegion",
    "DesignLoopResult",
    "StructuralZone",
    "MarineCPResult",
    "RetrofitAssessment",
    "marine_structure_current_demand",
    "standoff_anode_design_loop",
    "anode_distribution",
    "retrofit_assessment",
    # iccp_design
    "AnodeBedType",
    "AnodeMaterial",
    "RectifierSizingInput",
    "RectifierSizingResult",
    "AnodeBedResult",
    "rectifier_sizing",
    "anode_bed_design",
    "cable_sizing",
    # cp_survey
    "CISSurveyPoint",
    "CISAnalysisResult",
    "DCVGIndication",
    "AttenuationResult",
    "analyze_cis_survey",
    "classify_dcvg_indication",
    "attenuation_analysis",
    # corrosion_rate
    "CO2CorrosionInput",
    "CO2CorrosionResult",
    "GalvanicCorrosionInput",
    "GalvanicCorrosionResult",
    "de_waard_milliams_co2",
    "norsok_m506_co2",
    "galvanic_corrosion",
    "pitting_rate_estimate",
    # anode_depletion
    "AnodeStatus",
    "DepletionResult",
    "DepletionProfile",
    "InspectionRecommendation",
    "calculate_remaining_life",
    "generate_depletion_profile",
    "recommend_inspection_interval",
    # cp_monitoring
    "ReferenceElectrodeType",
    "MonitoringEnvironment",
    "ReferenceElectrodeRecommendation",
    "MonitoringSystemSpec",
    "AlarmThresholds",
    "select_reference_electrode",
    "design_monitoring_system",
    "set_alarm_thresholds",
    # stray_current
    "InterferenceType",
    "MitigationType",
    "StrayCurrentInput",
    "StrayCurrentResult",
    "MitigationDesign",
    "assess_stray_current",
    "design_drainage_bond",
    # cp_reporting
    "ComplianceStatus",
    "RecommendationPriority",
    "ComplianceCheck",
    "Recommendation",
    "RemainingLifeSummary",
    "CPAssessmentReport",
    "compliance_check_potential",
    "generate_assessment_report",
    "remaining_life_summary",
    # anode_sizing
    "AnodeType",
    "AnodeSizingInput",
    "AnodeSizingResult",
    "sizing_current_demand",
    "sizing_anode_mass",
    "sizing_anode_resistance",
    "sizing_depleted_equivalent_radius",
    "design_cp_system",
    # marine_cp
    "MarineZoneType",
    "MarineZone",
    "MarineCPInput",
    "MarineCPDesignResult",
    "get_seawater_current_density",
    "calculate_zone_demand",
    "design_marine_cp",
    # pipeline_cp extensions
    "CriteriaResult",
    "PipelineDesignResult",
    "calculate_pipeline_current_demand",
    "calculate_anode_spacing",
    "check_potential_criteria",
    "design_pipeline_cp",
    "soil_resistivity_correction",
]


def __getattr__(name: str) -> Any:
    """Lazily resolve the deprecated ``design_marine_cp`` (warns on access)."""
    if name == "design_marine_cp":
        return _marine_cp.design_marine_cp
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")
