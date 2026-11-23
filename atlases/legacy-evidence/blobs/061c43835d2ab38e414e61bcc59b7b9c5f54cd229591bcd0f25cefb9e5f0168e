"""
Data models for DNV RP F105 free-spanning pipeline VIV assessment.

All physical quantities in SI units unless noted in field names.
"""
from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum


class BoundaryConditionF105(Enum):
    """Span end conditions (F105 Table 6-1)."""
    PINNED_PINNED = "pinned-pinned"
    FIXED_FIXED = "fixed-fixed"
    FIXED_PINNED = "fixed-pinned"


class EnvironmentType(Enum):
    """Corrosion environment for S-N fatigue (DNV-RP-C203)."""
    IN_AIR = "in_air"
    SEAWATER_CP = "seawater_cp"   # seawater with cathodic protection


@dataclass
class PipeSpanInput:
    """All inputs for an F105 free-span VIV assessment.

    Geometry
    --------
    od_m, wt_m         pipe outer diameter and wall thickness [m]
    span_length_m       free span length L [m]
    sag_m               static mid-span sag δ [m] (0 = horizontal)

    Material
    --------
    e_modulus_pa        Young's modulus [Pa]  (steel default: 207 GPa)
    steel_density_kgm3  pipe wall steel density [kg/m³]

    Contents
    --------
    content_density_kgm3  internal fluid density [kg/m³]

    Marine environment
    ------------------
    water_density_kgm3   seawater density [kg/m³]
    current_velocity_ms  current speed Uc [m/s]
    wave_velocity_ms     wave-induced velocity Uw [m/s]
    seabed_gap_m         gap e between pipe bottom and seabed [m]

    Damping
    -------
    structural_damping    ζ_s  (—) typically 0.005
    hydrodynamic_damping  ζ_h  (—) typically 0.01

    Fatigue / S-N
    -------------
    sn_curve_class   DNV-RP-C203 class, e.g. "F" for girth welds
    environment      in_air or seawater_cp

    Partial safety factors (F105 Sec 3.4)
    ----------------------------------------
    gamma_on_IL   IL onset safety factor (default 1.1)
    gamma_on_CF   CF onset safety factor (default 1.3)
    gamma_k       stability parameter safety factor (default 1.15)
    """
    # --- geometry ---
    od_m: float
    wt_m: float
    span_length_m: float

    # --- material ---
    e_modulus_pa: float = 207e9
    steel_density_kgm3: float = 7850.0

    # --- contents ---
    content_density_kgm3: float = 900.0

    # --- marine environment ---
    water_density_kgm3: float = 1025.0
    current_velocity_ms: float = 0.8
    wave_velocity_ms: float = 0.0
    seabed_gap_m: float = 0.5

    # --- boundary condition ---
    bc: BoundaryConditionF105 = BoundaryConditionF105.PINNED_PINNED
    sag_m: float = 0.0

    # --- damping ---
    structural_damping: float = 0.005
    hydrodynamic_damping: float = 0.010

    # --- S-N fatigue ---
    sn_curve_class: str = "F"
    environment: EnvironmentType = EnvironmentType.SEAWATER_CP

    # --- axial force (F105 Eq 6.8-1 complete form) ---
    # Se > 0 = tensile (increases fn); Se < 0 = compressive (reduces fn)
    # Includes thermal contraction, internal pressure end-cap, residual tension.
    # Default 0 is conservative (no tension to raise fn).
    effective_axial_force_N: float = 0.0

    # --- partial safety factors ---
    gamma_on_IL: float = 1.1
    gamma_on_CF: float = 1.3
    gamma_k: float = 1.15


@dataclass
class NaturalFrequencyResult:
    """Output of SpanNaturalFrequency.compute()."""
    fn_IL_hz: float          # in-line natural frequency [Hz]
    fn_CF_hz: float          # cross-flow natural frequency [Hz]
    m_effective_kgm: float   # effective mass per unit length [kg/m]
    EI_Nm2: float            # bending stiffness [N·m²]


@dataclass
class OnsetScreeningResult:
    """Output of SpanOnsetScreening.screening_flags()."""
    Ks: float            # stability parameter (with γ_k applied)
    Ur_IL: float         # in-line reduced velocity
    Ur_CF: float         # cross-flow reduced velocity
    Ur_onset_IL: float   # IL onset threshold (with γ_on applied)
    Ur_onset_CF: float   # CF onset threshold (with γ_on applied)
    il_viv_onset: bool   # True if Ur_IL ≥ Ur_onset_IL
    cf_viv_onset: bool   # True if Ur_CF ≥ Ur_onset_CF


@dataclass
class SpanVIVResult:
    """Consolidated output of FreespanVIVFatigue.assess()."""
    # Natural frequencies
    fn_IL_hz: float
    fn_CF_hz: float
    # Onset screening
    Ks: float
    Ur_IL: float
    Ur_CF: float
    Ur_onset_IL: float
    Ur_onset_CF: float
    il_viv_onset: bool
    cf_viv_onset: bool
    # Amplitude / stress
    il_A_over_D: float
    cf_A_over_D: float
    il_stress_mpa: float
    cf_stress_mpa: float
    # Fatigue
    damage_per_year: float
    fatigue_life_years: float
    # Allowable span
    allowable_span_m: float
    span_length_m: float
    span_utilization: float   # span_length_m / allowable_span_m
