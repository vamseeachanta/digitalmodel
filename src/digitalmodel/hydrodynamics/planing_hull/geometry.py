#!/usr/bin/env python3
"""
ABOUTME: PlaningHullGeometry dataclass for the 2D+t strip theory motion model.

Stores hull principal dimensions following the Fridsma series convention.
Lengths in metres, angles in degrees (stored), radians available via property.
"""

import math
from dataclasses import dataclass


@dataclass
class PlaningHullGeometry:
    """
    Principal dimensions of a planing hull for 2D+t strip theory analysis.

    Parameters
    ----------
    length : float
        Hull length between perpendiculars [m]. Must be > 0.
    beam : float
        Maximum chine beam [m]. Must be > 0.
    deadrise : float
        Deadrise angle at mid-ship [degrees]. Range: 0 < beta < 90.
    chine_height : float
        Height of chine above keel at mid-ship [m]. Must be >= 0.
    lcg : float
        Longitudinal centre of gravity measured from bow [m].
        Must satisfy 0 < lcg < length.
    """

    length: float
    beam: float
    deadrise: float
    chine_height: float
    lcg: float

    def __post_init__(self) -> None:
        self._validate()

    def _validate(self) -> None:
        if self.length <= 0.0:
            raise ValueError(
                f"length must be positive, got {self.length}"
            )
        if self.beam <= 0.0:
            raise ValueError(
                f"beam must be positive, got {self.beam}"
            )
        if not (0.0 < self.deadrise < 90.0):
            raise ValueError(
                f"deadrise must be in (0, 90) degrees, got {self.deadrise}"
            )
        if self.chine_height < 0.0:
            raise ValueError(
                f"chine_height must be >= 0, got {self.chine_height}"
            )
        if not (0.0 < self.lcg < self.length):
            raise ValueError(
                f"lcg must be in (0, length)=(0, {self.length}), got {self.lcg}"
            )

    @property
    def deadrise_rad(self) -> float:
        """Deadrise angle in radians."""
        return math.radians(self.deadrise)

    @property
    def aspect_ratio(self) -> float:
        """Length-to-beam ratio L/B."""
        return self.length / self.beam

    @property
    def half_beam(self) -> float:
        """Half-beam B/2 [m]."""
        return self.beam / 2.0
