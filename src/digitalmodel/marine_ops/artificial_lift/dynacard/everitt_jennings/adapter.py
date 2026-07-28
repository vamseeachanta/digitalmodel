# ABOUTME: Bridges the oilfield-unit DynacardAnalysisContext to the SI solver.
# ABOUTME: Every unit crossing happens here and nowhere else.
"""Adapter between :class:`DynacardAnalysisContext` and the SI solver.

The rest of the dynacard package speaks oilfield units (inches, pounds, feet,
psi, lb/ft^3). The Everitt-Jennings solver speaks SI. Confining every
conversion to this one module means a unit error has exactly one place to hide.
"""

from __future__ import annotations

from typing import Optional

import numpy as np

from ..constants import BUCKLING_DETECTION_LOAD_THRESHOLD_LBS
from ..models import AnalysisResults, CardData, DynacardAnalysisContext
from . import units as U
from .solver import (
    DownholeCard,
    EverittJenningsSolver,
    RodString,
    Survey,
)

# 2-7/8 in tubing is the common default for a rod-pumped well; its drift ID is
# 2.441 in. Only the damping estimate depends on this, and it is a weak
# dependence, but the value should be supplied whenever it is known.
DEFAULT_TUBING_ID_IN = 2.441

# Steel properties in oilfield units, matching the package defaults.
STEEL_MODULUS_PSI = 30_000_000.0
STEEL_DENSITY_LB_FT3 = 490.0


def rod_string_from_context(ctx: DynacardAnalysisContext) -> RodString:
    """Build the SI rod string from the context's oilfield-unit sections.

    Args:
        ctx: The analysis context. ``ctx.rod_string`` is ordered top-first.

    Returns:
        A :class:`RodString` in metres, kilograms and pascals.

    Raises:
        ValueError: If the context has no rod sections. The rod string is the
            one input the solver cannot default — without the taper there is
            no wave speed profile to march through.
    """
    if not ctx.rod_string:
        raise ValueError(
            "rod_string is empty; the surface-to-downhole conversion needs "
            "the rod taper (diameter and length per section)"
        )

    diameters_m = np.array(
        [s.diameter * U.IN2M for s in ctx.rod_string], dtype=np.float64
    )
    lengths_m = np.array(
        [s.length * U.FT2M for s in ctx.rod_string], dtype=np.float64
    )
    densities = np.array(
        [
            (s.density if s.density else STEEL_DENSITY_LB_FT3) * U.LBPFT32KGPM3
            for s in ctx.rod_string
        ],
        dtype=np.float64,
    )
    moduli = np.array(
        [
            (s.modulus_of_elasticity if s.modulus_of_elasticity else STEEL_MODULUS_PSI)
            * U.PSI2PA
            for s in ctx.rod_string
        ],
        dtype=np.float64,
    )
    return RodString(
        diameters=diameters_m,
        lengths=lengths_m,
        densities=densities,
        moduli=moduli,
    )


def survey_from_context(ctx: DynacardAnalysisContext, rods: RodString) -> Survey:
    """Build the trajectory, falling back to vertical when no survey exists."""
    survey = ctx.survey
    if (
        survey is None
        or not survey.measured_depth
        or len(survey.measured_depth) < 2
    ):
        return Survey.vertical(rods.total_length)

    return Survey(
        measured_depth=np.asarray(survey.measured_depth, dtype=np.float64) * U.FT2M,
        inclination=np.asarray(survey.inclination, dtype=np.float64) * U.DEG2RAD,
        azimuth=np.asarray(survey.azimuth, dtype=np.float64) * U.DEG2RAD,
    )


def solve_downhole_card(
    ctx: DynacardAnalysisContext,
    tubing_id_in: float = DEFAULT_TUBING_ID_IN,
    viscosity_cp: float = 10.0,
    n_nodes: int = 100,
    damping: Optional[np.ndarray] = None,
) -> CardData:
    """Convert the context's surface card to a downhole card in oilfield units.

    Args:
        ctx: Analysis context carrying the surface card, rod string, pump and
            speed.
        tubing_id_in: Tubing inner diameter, inches. Affects the damping
            estimate only.
        viscosity_cp: Produced-fluid viscosity in centipoise. 10 cP is a
            reasonable default for a light crude; heavy oil is far higher and
            materially increases damping.
        n_nodes: Spatial nodes down the rod string.
        damping: Explicit damping coefficients, bypassing the estimator.

    Returns:
        A :class:`CardData` with position in inches and load in pounds.

    Raises:
        ValueError: If required inputs are missing or physically inconsistent.
    """
    rods = rod_string_from_context(ctx)
    survey = survey_from_context(ctx, rods)

    position_m = np.asarray(ctx.surface_card.position, dtype=np.float64) * U.IN2M
    load_n = np.asarray(ctx.surface_card.load, dtype=np.float64) * U.LB2N

    # centipoise -> Pa-s
    viscosity_pa_s = viscosity_cp * 1.0e-3

    solver = EverittJenningsSolver(
        n_nodes=n_nodes,
        viscosity=viscosity_pa_s,
        friction_coefficient=ctx.calc_params.coulomb_friction_coefficient,
    )
    card: DownholeCard = solver.solve(
        position=position_m,
        load=load_n,
        rods=rods,
        strokes_per_minute=ctx.spm,
        pump_diameter=ctx.pump.diameter * U.IN2M,
        tubing_id=tubing_id_in * U.IN2M,
        fluid_density=ctx.fluid_density * U.LBPFT32KGPM3,
        survey=survey,
        damping=damping,
    )

    return CardData(
        position=(card.position * U.M2IN).tolist(),
        load=(card.load * U.N2LB).tolist(),
    )


class EverittJenningsContextSolver:
    """Workflow-compatible wrapper around the Everitt-Jennings solver.

    :class:`DynacardWorkflow` expects a solver object constructed from a
    context, exposing ``solve()`` and ``detect_buckling()`` and returning an
    :class:`AnalysisResults`. This adapts the SI solver to that contract so it
    can be selected alongside the existing two.
    """

    def __init__(
        self,
        context: DynacardAnalysisContext,
        tubing_id_in: float = DEFAULT_TUBING_ID_IN,
        viscosity_cp: float = 10.0,
        n_nodes: int = 200,
    ):
        self.ctx = context
        self.tubing_id_in = tubing_id_in
        self.viscosity_cp = viscosity_cp
        self.n_nodes = n_nodes
        self.results = AnalysisResults()

    def solve(self) -> AnalysisResults:
        """Convert the surface card and populate the results object."""
        downhole = solve_downhole_card(
            self.ctx,
            tubing_id_in=self.tubing_id_in,
            viscosity_cp=self.viscosity_cp,
            n_nodes=self.n_nodes,
        )
        surface_load = np.asarray(self.ctx.surface_card.load, dtype=np.float64)
        self.results.downhole_card = downhole
        self.results.peak_polished_rod_load = float(np.max(surface_load))
        self.results.minimum_polished_rod_load = float(np.min(surface_load))
        self.results.ctx = self.ctx
        self.results.solver_method = "everitt_jennings"
        return self.results

    def detect_buckling(self) -> bool:
        """Flag compression in the rod string at the pump.

        Buckling shows up as the downhole load going meaningfully negative —
        the string is being pushed rather than pulled.
        """
        if not self.results.downhole_card:
            return False
        loads = np.asarray(self.results.downhole_card.load, dtype=np.float64)
        if float(np.min(loads)) < BUCKLING_DETECTION_LOAD_THRESHOLD_LBS:
            self.results.buckling_detected = True
        return self.results.buckling_detected
