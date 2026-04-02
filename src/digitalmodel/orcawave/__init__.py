"""OrcaWave frequency-domain diffraction analysis utilities.

This package provides:
- **reporting**: HTML report generation from OrcaWave .owr result files
- **rao_processing**: RAO data I/O, interpolation, format conversion, comparison
- **hydro_coefficients**: Added mass, damping, excitation force storage & conversion
- **panel_mesh**: WAMIT GDF panel mesh I/O, quality metrics, volume/area calculation
- **wave_spectrum**: JONSWAP, PM, Bretschneider, ISSC, Ochi-Hubble, Torsethaugen spectra
- **motion_statistics**: Short/long-term response statistics, MSI
- **drift_forces**: Mean/slowly-varying drift forces, Newman approximation, wind/current
- **vessel_database**: Common vessel parameters and representative RAO data
"""

from digitalmodel.orcawave.rao_processing import (
    RAOEntry,
    RAOTable,
    RAOComparisonResult,
    amplitude_phase_to_complex,
    complex_to_amplitude_phase,
    read_rao_csv,
    interpolate_rao,
    combine_raos_multi_body,
    compare_raos,
    rao_table_to_csv,
)
from digitalmodel.orcawave.hydro_coefficients import (
    HydroMatrix6x6,
    ExcitationForceVector,
    HydroCoefficients,
    interpolate_matrix_at_frequency,
    interpolate_excitation_at_frequency,
    to_wamit_added_mass,
    to_wamit_damping,
    from_wamit_added_mass,
    create_hydrostatic_restoring,
)
from digitalmodel.orcawave.panel_mesh import (
    PanelQuad,
    MeshQualityMetrics,
    MeshSummary,
    GDFMesh,
    read_gdf,
    write_gdf,
    compute_panel_quality,
    compute_mesh_summary,
    compute_waterplane_area,
    compute_displaced_volume,
)
from digitalmodel.orcawave.wave_spectrum import (
    SpectrumParameters,
    SpectrumResult,
    SpectralMoments,
    pierson_moskowitz,
    jonswap,
    bretschneider,
    issc_spectrum,
    ochi_hubble,
    torsethaugen,
    generate_spectrum,
    compute_spectral_moments,
    spectral_periods,
)
from digitalmodel.orcawave.motion_statistics import (
    ResponseSpectrum,
    ShortTermStatistics,
    ScatterCell,
    LongTermResult,
    MSIResult,
    compute_response_spectrum,
    short_term_statistics,
    rayleigh_exceedance,
    rayleigh_quantile,
    long_term_extreme,
    motion_sickness_incidence,
)
from digitalmodel.orcawave.drift_forces import (
    MeanDriftCoefficients,
    QTFMatrix,
    DriftForceResult,
    WindCurrentDrift,
    compute_mean_drift_force,
    newman_approximation,
    full_qtf_slowly_varying,
    compute_wind_current_drift,
)
from digitalmodel.orcawave.vessel_database import (
    VesselParameters,
    VesselRAOSet,
    ParametricHull,
    list_vessels,
    get_vessel,
    get_vessels_by_type,
    get_representative_raos,
    generate_parametric_hull,
)

__all__ = [
    # rao_processing
    "RAOEntry",
    "RAOTable",
    "RAOComparisonResult",
    "amplitude_phase_to_complex",
    "complex_to_amplitude_phase",
    "read_rao_csv",
    "interpolate_rao",
    "combine_raos_multi_body",
    "compare_raos",
    "rao_table_to_csv",
    # hydro_coefficients
    "HydroMatrix6x6",
    "ExcitationForceVector",
    "HydroCoefficients",
    "interpolate_matrix_at_frequency",
    "interpolate_excitation_at_frequency",
    "to_wamit_added_mass",
    "to_wamit_damping",
    "from_wamit_added_mass",
    "create_hydrostatic_restoring",
    # panel_mesh
    "PanelQuad",
    "MeshQualityMetrics",
    "MeshSummary",
    "GDFMesh",
    "read_gdf",
    "write_gdf",
    "compute_panel_quality",
    "compute_mesh_summary",
    "compute_waterplane_area",
    "compute_displaced_volume",
    # wave_spectrum
    "SpectrumParameters",
    "SpectrumResult",
    "SpectralMoments",
    "pierson_moskowitz",
    "jonswap",
    "bretschneider",
    "issc_spectrum",
    "ochi_hubble",
    "torsethaugen",
    "generate_spectrum",
    "compute_spectral_moments",
    "spectral_periods",
    # motion_statistics
    "ResponseSpectrum",
    "ShortTermStatistics",
    "ScatterCell",
    "LongTermResult",
    "MSIResult",
    "compute_response_spectrum",
    "short_term_statistics",
    "rayleigh_exceedance",
    "rayleigh_quantile",
    "long_term_extreme",
    "motion_sickness_incidence",
    # drift_forces
    "MeanDriftCoefficients",
    "QTFMatrix",
    "DriftForceResult",
    "WindCurrentDrift",
    "compute_mean_drift_force",
    "newman_approximation",
    "full_qtf_slowly_varying",
    "compute_wind_current_drift",
    # vessel_database
    "VesselParameters",
    "VesselRAOSet",
    "ParametricHull",
    "list_vessels",
    "get_vessel",
    "get_vessels_by_type",
    "get_representative_raos",
    "generate_parametric_hull",
]
