"""
digitalmodel.subsea.pipeline.global_buckling
============================================
Closed-form global (lateral / snaking) buckling of heated submarine pipelines,
after Hobbs (1984).

This complements the existing config-driven :mod:`digitalmodel.subsea.pipeline.
lateral_buckling` runner, which builds the effective-axial-force profile along
a route and applies a single susceptibility screen.  This package answers the
other half of the question: *given* that the line is susceptible, what is the
buckle length, amplitude, bending moment and combined stress, and at what
temperature does each mode become possible?

Public API
----------
PipeSection             Section, weight and thermal properties (SI)
SoilResistance          Axial and lateral Coulomb friction coefficients
HobbsMode               Modes 1-4 and the periodic ("infinite") mode
LateralBuckleState      One point on an equilibrium path
lateral_equilibrium     Evaluate the path at a chosen buckle length
critical_state          Minimum-P0 turning point (snap-through force)
equilibria_at_temperature   Roots of P0 = EA alpha dT (0, 1 or 2)
governing_mode          Mode with the lowest critical force
screen_modes            Utilisation of a driving force against every mode
effective_driving_force Fully-restrained S_eff from dT, pressure and lay tension

Quick-start::

    from digitalmodel.subsea.pipeline.global_buckling import (
        PipeSection, SoilResistance, critical_state,
    )

    pipe = PipeSection.from_dimensions(
        e_modulus_pa=207e9, od_m=0.3239, wt_m=0.0159,
        submerged_weight_N_m=900.0,
    )
    soil = SoilResistance(axial_friction=0.5, lateral_friction=0.7)
    state = critical_state(pipe, soil, mode=3)
    print(f"L = {state.buckle_length_m:.1f} m, dT = {state.temperature_rise_K:.1f} C")

All quantities are SI and axial force is positive in compression.  Results are
elastic small-slope equilibrium paths, not design acceptance checks; see
DNV-RP-F110 for the design framework.
"""
from __future__ import annotations

from .hobbs_lateral import (
    critical_state,
    effective_driving_force,
    equilibria_at_temperature,
    governing_mode,
    lateral_equilibrium,
    screen_modes,
)
from .models import (
    MODE_CONSTANTS,
    HobbsConstants,
    HobbsMode,
    LateralBuckleState,
    ModeSusceptibility,
    PipeSection,
    SoilResistance,
)

__all__ = [
    "MODE_CONSTANTS",
    "HobbsConstants",
    "HobbsMode",
    "LateralBuckleState",
    "ModeSusceptibility",
    "PipeSection",
    "SoilResistance",
    "critical_state",
    "effective_driving_force",
    "equilibria_at_temperature",
    "governing_mode",
    "lateral_equilibrium",
    "screen_modes",
]
