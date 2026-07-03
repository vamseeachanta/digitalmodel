"""
ABOUTME: Foundational CFD validation subpackage (#1161 Phase 2). Each
validation case ships a *known-answer regression*: a pure analytical reference
(no OpenFOAM dependency) plus a case builder that reuses the existing
``solvers/openfoam`` case-building stack. On solver-capable hosts the case is
solved and compared to the reference within tolerance; on dry-run-only hosts
the analytical + build checks still run green.

Cases:
- Flat plate (Blasius), #1167 — laminar boundary layer; Cf, delta99, plate Cd.
- Cylinder Re=100, #1166 — laminar Karman vortex street; mean Cd, Strouhal St.
- Dam break (Martin & Moyce 1952), #1165 — interFoam VOF free surface; surge
  front Z(T) + mass conservation.
- Regular-wave tank (StokesII NWT), #1170 — wave height, dispersion,
  reflection vs wave theory.
- Floating-body heave decay, #1169 — interFoam + sixDoFRigidBodyMotion;
  Archimedes draft + hydrostatic heave period.
- Kleefsman green-water impact, #1172 — 3D dam break onto an instrumented
  box; impact pressures + water heights vs the official MARIN data.
- Wave-excited floating body, #1302 — overInterDyMFoam 2D overset; heave
  RAO vs the long-wave (wave-follower) limit.
- Wave-excited body heave-RAO sweep, #1324 — the #1302 case swept across
  wave period (long-wave -> resonance -> roll-off); CFD heave RAO vs a frozen
  potential-flow (capytaine BEM) reference at finite depth.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable, Dict

from .cylinder import (
    CYLINDER_RE100_CD,
    CYLINDER_RE100_CD_RANGE,
    CYLINDER_RE100_STROUHAL,
    CYLINDER_TOLERANCE,
    CylinderConfig,
    build_cylinder_case,
    cylinder_reference_cd,
    cylinder_reference_strouhal,
    shedding_frequency,
    strouhal_number,
)
from .dam_break import (
    DAM_BREAK_FRONT_TOLERANCE,
    DAM_BREAK_MASS_TOLERANCE,
    DAM_BREAK_RAW_FRONT_TOLERANCE,
    DAM_BREAK_SOLVE_TOLERANCE,
    GATE_RELEASE_TIME_SHIFT,
    MARTIN_MOYCE_N2_FRONT,
    MARTIN_MOYCE_N2_TIME,
    DamBreakConfig,
    build_dam_break_case,
    dimensionless_front,
    dimensionless_time,
    front_deviation,
    martin_moyce_front,
)
from .flat_plate import (
    BLASIUS_TOLERANCE,
    FlatPlateConfig,
    blasius_cf,
    blasius_delta99,
    blasius_plate_cd,
    build_flat_plate_case,
)
from .floating_body import (
    ADDED_MASS_BAND,
    DRAFT_TOLERANCE,
    PERIOD_RATIO_BAND,
    FloatingBodyConfig,
    analyze_decay,
    build_floating_body_case,
    equilibrium_draft,
    extract_heave_history,
    heave_stiffness,
    hydrostatic_heave_period,
)
from .kleefsman import (
    HEIGHT_MAE_TOLERANCE,
    PRESSURE_ARRIVAL_TOLERANCE,
    PRESSURE_PEAK_TOLERANCE,
    KleefsmanConfig,
    build_kleefsman_case,
    extract_impact_metrics,
    load_experiment,
)
from .sloshing_2d import (
    ROLL_MOMENT_FO_NAME,
    ROLL_MOMENT_PATCHES,
    SLOSHING_FREQ_TOLERANCE,
    SloshingForcedRollConfig,
    SloshingFreeDecayConfig,
    analyze_free_decay,
    build_forced_roll_case,
    build_free_decay_case,
    cosine_mode_setfields_body,
    measure_natural_frequency,
    parse_interface_height,
    parse_roll_moment,
    roll_moment_function_object,
)
from .sloshing_sweep import (
    CONTRACT_FIELDS,
    SloshingSweep,
    SloshingSweepConfig,
    reduce_roll_moment,
)
from .wave_excited_body import (
    PERIOD_TOLERANCE,
    RAO_TOLERANCE,
    WaveExcitedBodyConfig,
    analyze_excited_heave,
    build_wave_excited_body_case,
    extract_heave_from_log,
    harmonic_amplitude,
    incident_wave_split,
    response_period,
)
from .wave_excited_body_rao import (
    RAO_SWEEP_BAND,
    RAO_SWEEP_BAND_RESONANCE,
    SWEEP_PERIODS,
    analyze_sweep_point,
    build_sweep_config,
    load_reference_curve,
    reference_peak,
    reference_rao_at,
    summarize_sweep,
)
from .wave_excited_body_decay import (
    analyze_ringdown,
    attribution,
    build_decay_config,
    predict_peak_rao,
    reference_damping_ratio,
)
from .wave_tank import (
    DISPERSION_TOLERANCE,
    REFLECTION_TOLERANCE,
    WAVE_DECAY_TOLERANCE,
    WAVE_HEIGHT_TOLERANCE,
    WaveTankConfig,
    build_wave_tank_case,
    celerity,
    dispersion_wavenumber,
    extract_wave_quality,
    reflection_coefficient,
    wavelength,
)


@dataclass
class ValidationCase:
    """A foundational validation case = build + known-answer reference.

    Attributes:
        name: Human-readable case identifier.
        build: Callable that generates the case directory (``(config, parent)``
            -> ``Path``); defaults baked into the underlying builder.
        reference: Mapping of quantity name -> analytical reference value.
        tolerance: Relative tolerance for the known-answer gate (fraction).
        domain: Physical domain tag (e.g. ``"external-aero (boundary layer)"``).
    """

    name: str
    build: Callable[..., Any]
    reference: Dict[str, float]
    tolerance: float
    domain: str
    metadata: Dict[str, Any] = field(default_factory=dict)

    def relative_error(self, quantity: str, measured: float) -> float:
        """Relative error of ``measured`` against the stored reference value."""
        ref = self.reference[quantity]
        if ref == 0.0:
            return abs(measured)
        return abs(measured - ref) / abs(ref)

    def within_tolerance(self, quantity: str, measured: float) -> bool:
        """True iff ``measured`` matches the reference within ``tolerance``."""
        return self.relative_error(quantity, measured) <= self.tolerance


# Registry of the foundational cases (Phase 2).
FLAT_PLATE_CASE = ValidationCase(
    name="flat_plate_blasius",
    build=build_flat_plate_case,
    reference={
        "cf_at_re_1e5": blasius_cf(1.0e5),
        "delta99_at_x1_re_1e5": blasius_delta99(1.0, 1.0e5),
        "plate_cd_re_1e5": blasius_plate_cd(1.0e5),
    },
    tolerance=BLASIUS_TOLERANCE,
    domain="external-aero (boundary layer)",
    metadata={"issue": "#1167", "reference": "Blasius (1908)"},
)

CYLINDER_CASE = ValidationCase(
    name="cylinder_re100",
    build=build_cylinder_case,
    reference={
        "mean_cd": CYLINDER_RE100_CD,
        "strouhal": CYLINDER_RE100_STROUHAL,
    },
    tolerance=CYLINDER_TOLERANCE,
    domain="external-aero (bluff body) / marine VIV",
    metadata={"issue": "#1166", "reference": "Williamson (1996); Henderson (1997)"},
)

DAM_BREAK_CASE = ValidationCase(
    name="dam_break_martin_moyce",
    build=build_dam_break_case,
    reference={
        # Digitized experimental surge-front spot values Z(T), n^2=2.
        "front_z_at_T1p97": 2.33,
        "front_z_at_T2p96": 3.67,
    },
    tolerance=DAM_BREAK_FRONT_TOLERANCE,
    domain="marine (free-surface VOF)",
    metadata={
        "issue": "#1165",
        "reference": "Martin & Moyce (1952), doi:10.1098/rsta.1952.0006",
    },
)

WAVE_TANK_CASE = ValidationCase(
    name="wave_tank_stokes2",
    build=build_wave_tank_case,
    reference={
        # dispersion for the default wave (T=3 s, d=0.4 m)
        "wavenumber": dispersion_wavenumber(3.0, 0.4),
        "wave_height": 0.05,
    },
    tolerance=DISPERSION_TOLERANCE,
    domain="marine (wave generation / NWT)",
    metadata={
        "issue": "#1170",
        "reference": "linear dispersion; stokesII tutorial (ESI v2312)",
    },
)

FLOATING_BODY_CASE = ValidationCase(
    name="floating_body_decay",
    build=build_floating_body_case,
    reference={
        "draft": FloatingBodyConfig().draft,
        "hydrostatic_period": FloatingBodyConfig().hydrostatic_period,
    },
    tolerance=DRAFT_TOLERANCE,
    domain="marine (floating body, 6-DOF)",
    metadata={
        "issue": "#1169",
        "reference": "Archimedes + waterplane-stiffness heave period",
    },
)

KLEEFSMAN_CASE = ValidationCase(
    name="kleefsman_impact",
    build=build_kleefsman_case,
    reference={
        # official experimental P2 peak (Pa) over the primary impact
        "p2_peak_pa": 8127.0,
    },
    tolerance=PRESSURE_PEAK_TOLERANCE,
    domain="marine (green-water / wave impact)",
    metadata={
        "issue": "#1172",
        "reference": "Kleefsman et al. (2005) MARIN experiment; SPHERIC Test 2",
    },
)

WAVE_EXCITED_BODY_CASE = ValidationCase(
    name="wave_excited_body",
    build=build_wave_excited_body_case,
    reference={
        # long-wave (wave-follower) limit
        "heave_rao": 1.0,
        "draft": WaveExcitedBodyConfig().draft,
    },
    tolerance=RAO_TOLERANCE,
    domain="marine (wave-body interaction, overset)",
    metadata={
        "issue": "#1302",
        "reference": "Newman (1977) ch. 6 — quasi-static long-wave response",
    },
)

WAVE_EXCITED_BODY_RAO_CASE = ValidationCase(
    name="wave_excited_body_rao",
    build=build_wave_excited_body_case,   # same case, swept over wave period
    reference={
        # spot values from the frozen potential-flow (capytaine BEM) curve:
        # long-wave -> 1, flank at T = 1 s, near-resonance peak (~T = 0.87 s)
        "heave_rao_long_wave": 1.0,
        "heave_rao_at_T1": 1.52,
        "heave_rao_peak": 2.18,
    },
    tolerance=RAO_SWEEP_BAND,
    domain="marine (wave-body interaction, frequency response / overset)",
    metadata={
        "issue": "#1324",
        "reference": "capytaine linear BEM (Froude-Krylov + diffraction), "
                     "finite depth 0.4 m, frozen in rao_reference_capytaine.csv; "
                     "reproduces the #1302 CFD long-wave point",
        "gate": "band gate applies OFF resonance (inviscid reference is ground "
                "truth there — CFD passes within tolerance). The resonance-region "
                "reduction was RESOLVED by the free-decay test #1332: the body's "
                "natural frequency and damping are verified (near-inviscid), so "
                "the reduction is a forced-wave delivery / measurement artifact, "
                "not body damping.",
    },
)

WAVE_EXCITED_BODY_DECAY_CASE = ValidationCase(
    name="wave_excited_body_decay",
    build=build_wave_excited_body_case,   # same case, still water + heave offset
    reference={
        # from the frozen potential-flow curve at the heave resonance:
        "omega_n_rad_s": 7.476,           # reference resonance frequency
        "zeta_radiation": 0.104,          # potential-flow radiation damping ratio
    },
    tolerance=0.05,                       # natural-frequency match tolerance
    domain="marine (overset floating body, free-decay damping)",
    metadata={
        "issue": "#1332",
        "resolves": "#1324 near-resonance open finding",
        "result": "omega_n within ~0.4% of reference; zeta ~ 0.13 "
                  "(amplitude-independent, ~20% above radiation) -> body dynamics "
                  "verified; #1324 reduction is a forced-wave measurement artifact.",
    },
)

SLOSHING_2D_CASE = ValidationCase(
    name="sloshing_2d_free_decay",
    build=build_free_decay_case,
    reference={
        # analytical first-mode sloshing frequency (Hz) for the default tank
        "natural_frequency": SloshingFreeDecayConfig().analytical_frequency(),
    },
    tolerance=SLOSHING_FREQ_TOLERANCE,
    domain="marine (internal tank sloshing / TLD)",
    metadata={
        "issue": "#639",
        "reference": (
            "linear tanh dispersion; SPHERIC Test 10 (Delorme et al. 2009) "
            "forced-roll corroboration"
        ),
    },
)

FOUNDATIONAL_CASES = (
    FLAT_PLATE_CASE,
    CYLINDER_CASE,
    DAM_BREAK_CASE,
    WAVE_TANK_CASE,
    FLOATING_BODY_CASE,
    KLEEFSMAN_CASE,
    WAVE_EXCITED_BODY_CASE,
    WAVE_EXCITED_BODY_RAO_CASE,
    WAVE_EXCITED_BODY_DECAY_CASE,
    SLOSHING_2D_CASE,
)

__all__ = [
    # Shared
    "ValidationCase",
    "FOUNDATIONAL_CASES",
    "FLAT_PLATE_CASE",
    "CYLINDER_CASE",
    "DAM_BREAK_CASE",
    "WAVE_TANK_CASE",
    "FLOATING_BODY_CASE",
    "KLEEFSMAN_CASE",
    "WAVE_EXCITED_BODY_CASE",
    "WAVE_EXCITED_BODY_RAO_CASE",
    "WAVE_EXCITED_BODY_DECAY_CASE",
    "SLOSHING_2D_CASE",
    # Wave-excited body heave-RAO sweep (#1324)
    "RAO_SWEEP_BAND",
    "RAO_SWEEP_BAND_RESONANCE",
    "SWEEP_PERIODS",
    "analyze_sweep_point",
    "build_sweep_config",
    "load_reference_curve",
    "reference_peak",
    "reference_rao_at",
    "summarize_sweep",
    # Free-decay heave damping (#1332)
    "analyze_ringdown",
    "attribution",
    "build_decay_config",
    "predict_peak_rao",
    "reference_damping_ratio",
    # Sloshing 2D (#639)
    "SLOSHING_FREQ_TOLERANCE",
    "SloshingForcedRollConfig",
    "SloshingFreeDecayConfig",
    "analyze_free_decay",
    "build_forced_roll_case",
    "build_free_decay_case",
    "cosine_mode_setfields_body",
    "measure_natural_frequency",
    "parse_interface_height",
    # Sloshing roll-moment + fill/frequency sweep (#641)
    "ROLL_MOMENT_FO_NAME",
    "ROLL_MOMENT_PATCHES",
    "parse_roll_moment",
    "roll_moment_function_object",
    "CONTRACT_FIELDS",
    "SloshingSweep",
    "SloshingSweepConfig",
    "reduce_roll_moment",
    # Wave-excited floating body (#1302)
    "PERIOD_TOLERANCE",
    "RAO_TOLERANCE",
    "WaveExcitedBodyConfig",
    "analyze_excited_heave",
    "build_wave_excited_body_case",
    "extract_heave_from_log",
    "harmonic_amplitude",
    "incident_wave_split",
    "response_period",
    # Kleefsman impact (#1172)
    "HEIGHT_MAE_TOLERANCE",
    "PRESSURE_ARRIVAL_TOLERANCE",
    "PRESSURE_PEAK_TOLERANCE",
    "KleefsmanConfig",
    "build_kleefsman_case",
    "extract_impact_metrics",
    "load_experiment",
    # Flat plate (#1167)
    "BLASIUS_TOLERANCE",
    "FlatPlateConfig",
    "blasius_cf",
    "blasius_delta99",
    "blasius_plate_cd",
    "build_flat_plate_case",
    # Cylinder (#1166)
    "CYLINDER_RE100_CD",
    "CYLINDER_RE100_CD_RANGE",
    "CYLINDER_RE100_STROUHAL",
    "CYLINDER_TOLERANCE",
    "CylinderConfig",
    "build_cylinder_case",
    "cylinder_reference_cd",
    "cylinder_reference_strouhal",
    "shedding_frequency",
    "strouhal_number",
    # Dam break (#1165)
    "DAM_BREAK_FRONT_TOLERANCE",
    "DAM_BREAK_MASS_TOLERANCE",
    "DAM_BREAK_RAW_FRONT_TOLERANCE",
    "DAM_BREAK_SOLVE_TOLERANCE",
    "GATE_RELEASE_TIME_SHIFT",
    "MARTIN_MOYCE_N2_FRONT",
    "MARTIN_MOYCE_N2_TIME",
    "DamBreakConfig",
    "build_dam_break_case",
    "dimensionless_front",
    "dimensionless_time",
    "front_deviation",
    "martin_moyce_front",
    # Wave tank (#1170)
    "DISPERSION_TOLERANCE",
    "REFLECTION_TOLERANCE",
    "WAVE_DECAY_TOLERANCE",
    "WAVE_HEIGHT_TOLERANCE",
    "WaveTankConfig",
    "build_wave_tank_case",
    "celerity",
    "dispersion_wavenumber",
    "extract_wave_quality",
    "reflection_coefficient",
    "wavelength",
    # Floating body (#1169)
    "ADDED_MASS_BAND",
    "DRAFT_TOLERANCE",
    "PERIOD_RATIO_BAND",
    "FloatingBodyConfig",
    "analyze_decay",
    "build_floating_body_case",
    "equilibrium_draft",
    "extract_heave_history",
    "heave_stiffness",
    "hydrostatic_heave_period",
]
