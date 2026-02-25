#!/usr/bin/env python3
"""
ABOUTME: Pre-configured marine solver setups for OpenFOAM CFD simulations.
Provides ready-to-use configurations for wave loading, current loading,
greenwater, sloshing, and vortex-induced vibration applications.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Optional

from .models import (
    BoundaryCondition,
    BoundaryType,
    CaseType,
    DomainConfig,
    OpenFOAMCase,
    SolverConfig,
    TurbulenceModel,
    TurbulenceType,
)


# ============================================================================
# WaveLoadingSetup
# ============================================================================


@dataclass
class WaveLoadingSetup:
    """Pre-configured interFoam setup for wave-structure interaction.

    Uses VOF (Volume of Fluid) method with interFoam to model wave
    generation, propagation, and interaction with marine structures.
    k-omega SST turbulence model is selected as appropriate for
    offshore wave loading simulations.

    Attributes:
        wave_height: Incident wave height H (m).
        wave_period: Incident wave period T (s).
        water_depth: Water depth (m).
        current_speed: Superimposed uniform current speed (m/s).
        name: Case directory name.
    """

    wave_height: float = 2.0
    wave_period: float = 10.0
    water_depth: float = 50.0
    current_speed: float = 0.0
    name: str = "wave_loading"

    def __post_init__(self) -> None:
        cfg = SolverConfig(
            solver_name="interFoam",
            is_multiphase=True,
            delta_t=0.005,
            end_time=50.0,
            write_interval=100,
            adjustable_time_step=True,
            max_co=0.5,
        )
        tm = TurbulenceModel(TurbulenceType.K_OMEGA_SST)
        self.case = OpenFOAMCase(
            name=self.name,
            case_type=CaseType.WAVE_LOADING,
            solver_config=cfg,
            turbulence_model=tm,
        )


# ============================================================================
# CurrentLoadingSetup
# ============================================================================


@dataclass
class CurrentLoadingSetup:
    """Pre-configured simpleFoam or pimpleFoam current-loading setup.

    For steady current simulations simpleFoam (RANS) is selected.
    For transient current (or unsteady forcing) pimpleFoam is used.

    Attributes:
        steady: True for steady-state RANS; False for transient RANS.
        current_speed: Approach current speed (m/s).
        name: Case directory name.
    """

    steady: bool = True
    current_speed: float = 1.0
    name: str = "current_loading"

    def __post_init__(self) -> None:
        solver_name = "simpleFoam" if self.steady else "pimpleFoam"
        cfg = SolverConfig(
            solver_name=solver_name,
            is_multiphase=False,
            delta_t=1.0 if self.steady else 0.01,
            end_time=1000.0 if self.steady else 100.0,
            write_interval=100 if self.steady else 50,
        )
        tm = TurbulenceModel(TurbulenceType.K_OMEGA_SST)
        self.case = OpenFOAMCase(
            name=self.name,
            case_type=CaseType.CURRENT_LOADING,
            solver_config=cfg,
            turbulence_model=tm,
        )


# ============================================================================
# GreenWaterSetup
# ============================================================================


@dataclass
class GreenWaterSetup:
    """Pre-configured interFoam setup for deck overtopping (greenwater).

    Models the VOF free surface and wave-body interaction that leads to
    greenwater events on decks of offshore structures.

    Attributes:
        name: Case directory name.
    """

    name: str = "greenwater"

    def __post_init__(self) -> None:
        cfg = SolverConfig(
            solver_name="interFoam",
            is_multiphase=True,
            delta_t=0.005,
            end_time=30.0,
            write_interval=50,
            adjustable_time_step=True,
            max_co=0.5,
        )
        tm = TurbulenceModel(TurbulenceType.K_OMEGA_SST)
        self.case = OpenFOAMCase(
            name=self.name,
            case_type=CaseType.GREENWATER,
            solver_config=cfg,
            turbulence_model=tm,
        )


# ============================================================================
# SloshingSetup
# ============================================================================


@dataclass
class SloshingSetup:
    """Pre-configured interFoam setup for internal tank sloshing.

    Models free-surface sloshing inside LNG cargo tanks or ballast tanks
    using the VOF method with mesh motion capability.

    Attributes:
        fill_level: Tank fill level as fraction of tank height (0-1).
        name: Case directory name.
    """

    fill_level: float = 0.5
    name: str = "sloshing"

    def __post_init__(self) -> None:
        cfg = SolverConfig(
            solver_name="interFoam",
            is_multiphase=True,
            delta_t=0.001,
            end_time=20.0,
            write_interval=50,
            adjustable_time_step=True,
            max_co=0.5,
        )
        tm = TurbulenceModel(TurbulenceType.K_OMEGA_SST)
        self.case = OpenFOAMCase(
            name=self.name,
            case_type=CaseType.SLOSHING,
            solver_config=cfg,
            turbulence_model=tm,
        )


# ============================================================================
# VIVSetup
# ============================================================================


@dataclass
class VIVSetup:
    """Pre-configured pimpleFoam setup for vortex-induced vibration.

    Simulates transient flow past a flexible riser or cylinder with
    the 6DOF solver for structural response. Single-phase flow is used
    since VIV analysis typically ignores the free surface.

    Attributes:
        current_speed: Approach current speed (m/s).
        name: Case directory name.
    """

    current_speed: float = 1.0
    name: str = "viv"

    def __post_init__(self) -> None:
        cfg = SolverConfig(
            solver_name="pimpleFoam",
            is_multiphase=False,
            delta_t=0.005,
            end_time=100.0,
            write_interval=50,
            adjustable_time_step=True,
            max_co=0.8,
        )
        tm = TurbulenceModel(TurbulenceType.K_OMEGA_SST)
        self.case = OpenFOAMCase(
            name=self.name,
            case_type=CaseType.VIV,
            solver_config=cfg,
            turbulence_model=tm,
        )
