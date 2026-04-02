"""Cathodic protection calculations — API RP 1632, ISO 15589-2, DNV-RP-B401,
and impressed current fuel system CP design."""

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
    CoatingLifeResult,
    coating_breakdown_factors,
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
    StructuralZone,
    MarineCPResult,
    RetrofitAssessment,
    marine_structure_current_demand,
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
    design_cp_system,
)

from digitalmodel.cathodic_protection.marine_cp import (
    ZoneType as MarineZoneType,
    Zone as MarineZone,
    MarineCPInput,
    MarineCPResult as MarineCPDesignResult,
    get_seawater_current_density,
    calculate_zone_demand,
    design_marine_cp,
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
    "CoatingLifeResult",
    "coating_breakdown_factors",
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
    "StructuralZone",
    "MarineCPResult",
    "RetrofitAssessment",
    "marine_structure_current_demand",
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
