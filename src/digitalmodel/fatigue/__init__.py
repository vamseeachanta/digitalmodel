"""
digitalmodel.fatigue — Fatigue analysis module

DNV-RP-C203 S-N curves with pyLife WoehlerCurve backend,
Miner's rule damage accumulation, and thickness correction.

Extended with 8 additional modules (v2.0):
- sn_library: 221 S-N curves from 17 international standards
- scf_library: Stress concentration factor equations
- hotspot_stress: Hotspot stress extrapolation
- spectral_fatigue: Frequency-domain fatigue (Dirlik, etc.)
- weld_classification: Automated weld detail classification
- fatigue_reporting: Assessment report generation
- multiaxial_fatigue: Multiaxial fatigue criteria
- environmental_correction: Environment correction factors
"""

from .sn_curves import get_sn_curve, DNV_CURVES
from .damage import miner_damage, design_life_check, thickness_correction
from .crack_growth import paris_law_life, stress_intensity_factor, inspection_interval
from .woehler_fitting import fit_woehler_curve, design_curve
from .rainflow import rainflow_count, stress_histogram, fatigue_life

# New modules (v2.0)
from .sn_library import (
    get_catalog, get_library_curve, search_curves, list_standards,
    curve_count, summary_table, SNCurveRecord, SNCatalog,
)
from .scf_library import (
    efthymiou_ty_axial, efthymiou_ty_ipb, efthymiou_ty_opb,
    efthymiou_k_axial, scf_butt_weld_misalignment,
    scf_cruciform_joint, scf_fillet_weld_toe,
    scf_shoulder_fillet, scf_circumferential_groove,
    TubularJointGeometry, SCFResult, PlateGeometry,
)
from .hotspot_stress import (
    extrapolate_hotspot_linear, extrapolate_hotspot_quadratic,
    extrapolate_hotspot, through_thickness_linearisation,
    recommended_readout_distances, HotspotInput, HotspotResult,
)
from .spectral_fatigue import (
    compute_spectral_moments, narrow_band_damage,
    wirsching_light_damage, dirlik_damage, benasciutti_tovo_damage,
    SpectralMoments, SpectralFatigueResult,
)
from .weld_classification import (
    classify_weld_detail, list_dnv_detail_categories,
    WeldDetail, ClassificationResult,
)
from .fatigue_reporting import (
    generate_report, report_to_markdown, report_to_dict,
    damage_barchart_data, inspection_recommendations,
    FatigueCheckLocation, FatigueReport,
)
from .multiaxial_fatigue import (
    von_mises_equivalent, principal_stress_range,
    findley_critical_plane, shear_stress_correction,
    multiaxial_damage_interaction, StressState, MultiaxialResult,
)
from .environmental_correction import (
    environment_correction, zone_factor, temperature_derating_factor,
    apply_environment_to_sn, EnvironmentInput, EnvironmentResult,
)

__all__ = [
    # Original modules
    "get_sn_curve",
    "DNV_CURVES",
    "miner_damage",
    "design_life_check",
    "thickness_correction",
    "paris_law_life",
    "stress_intensity_factor",
    "inspection_interval",
    "fit_woehler_curve",
    "design_curve",
    "rainflow_count",
    "stress_histogram",
    "fatigue_life",
    # sn_library
    "get_catalog",
    "get_library_curve",
    "search_curves",
    "list_standards",
    "curve_count",
    "summary_table",
    "SNCurveRecord",
    "SNCatalog",
    # scf_library
    "efthymiou_ty_axial",
    "efthymiou_ty_ipb",
    "efthymiou_ty_opb",
    "efthymiou_k_axial",
    "scf_butt_weld_misalignment",
    "scf_cruciform_joint",
    "scf_fillet_weld_toe",
    "scf_shoulder_fillet",
    "scf_circumferential_groove",
    "TubularJointGeometry",
    "SCFResult",
    "PlateGeometry",
    # hotspot_stress
    "extrapolate_hotspot_linear",
    "extrapolate_hotspot_quadratic",
    "extrapolate_hotspot",
    "through_thickness_linearisation",
    "recommended_readout_distances",
    "HotspotInput",
    "HotspotResult",
    # spectral_fatigue
    "compute_spectral_moments",
    "narrow_band_damage",
    "wirsching_light_damage",
    "dirlik_damage",
    "benasciutti_tovo_damage",
    "SpectralMoments",
    "SpectralFatigueResult",
    # weld_classification
    "classify_weld_detail",
    "list_dnv_detail_categories",
    "WeldDetail",
    "ClassificationResult",
    # fatigue_reporting
    "generate_report",
    "report_to_markdown",
    "report_to_dict",
    "damage_barchart_data",
    "inspection_recommendations",
    "FatigueCheckLocation",
    "FatigueReport",
    # multiaxial_fatigue
    "von_mises_equivalent",
    "principal_stress_range",
    "findley_critical_plane",
    "shear_stress_correction",
    "multiaxial_damage_interaction",
    "StressState",
    "MultiaxialResult",
    # environmental_correction
    "environment_correction",
    "zone_factor",
    "temperature_derating_factor",
    "apply_environment_to_sn",
    "EnvironmentInput",
    "EnvironmentResult",
]
