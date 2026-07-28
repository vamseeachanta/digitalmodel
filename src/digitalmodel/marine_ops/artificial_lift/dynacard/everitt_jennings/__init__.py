# ABOUTME: Everitt-Jennings finite-difference surface-to-downhole card solver.
# ABOUTME: Public entry point is solve_downhole_card(ctx).
"""Everitt-Jennings surface-to-downhole dynamometer card conversion.

Unlike a load-rescaling approximation, this solver rebuilds the downhole load
from the strain field (``F = EA du/dx``) after marching the damped wave
equation down the rod string. That is what removes rod-string vibration
harmonics from the card and makes a healthy pump card rectangular.

Typical use goes through the adapter, which handles oilfield/SI conversion::

    from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings import (
        solve_downhole_card,
    )

    downhole = solve_downhole_card(ctx, tubing_id_in=2.441, viscosity_cp=10.0)

Reference: Everitt, T.A. and Jennings, J.W., "An Improved Finite-Difference
Calculation of Downhole Dynamometer Cards for Sucker-Rod Pumps", SPE 18189.
"""

from .adapter import (
    EverittJenningsContextSolver,
    rod_string_from_context,
    solve_downhole_card,
    survey_from_context,
)
from .damping import estimate_damping_coeff, estimate_damping_profile
from .solver import (
    Coefficients,
    DownholeCard,
    EverittJenningsSolver,
    RodString,
    Simulation,
    Survey,
    build_coefficients,
    build_simulation,
)

__all__ = [
    "Coefficients",
    "EverittJenningsContextSolver",
    "DownholeCard",
    "EverittJenningsSolver",
    "RodString",
    "Simulation",
    "Survey",
    "build_coefficients",
    "build_simulation",
    "estimate_damping_coeff",
    "estimate_damping_profile",
    "rod_string_from_context",
    "solve_downhole_card",
    "survey_from_context",
]
