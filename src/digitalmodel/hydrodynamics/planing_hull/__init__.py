#!/usr/bin/env python3
"""
ABOUTME: Planing hull 2D+t strip theory seakeeping module.

Provides heave and pitch RAO computation for high-speed planing vessels
using the nonlinear Wagner water-entry strip theory approach.

Usage
-----
    from digitalmodel.hydrodynamics.planing_hull import (
        PlaningHullGeometry,
        PlaningStripModel,
        PlaningMotionSolver,
        PlaningRAO,
        compute_rao,
    )

    hull = PlaningHullGeometry(
        length=0.6096, beam=0.1219, deadrise=20.0,
        chine_height=0.04, lcg=0.3048,
    )
    rao = compute_rao(hull, speed=2.91, wave_freq=6.28, wave_steepness=0.10)
"""

from .geometry import PlaningHullGeometry
from .strip_model import PlaningStripModel
from .solver import PlaningMotionSolver, PlaningRAO, compute_rao

__all__ = [
    "PlaningHullGeometry",
    "PlaningStripModel",
    "PlaningMotionSolver",
    "PlaningRAO",
    "compute_rao",
]
