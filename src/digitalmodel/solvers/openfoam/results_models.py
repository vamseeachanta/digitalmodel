#!/usr/bin/env python3
"""
ABOUTME: Data models for OpenFOAM post-processing results including force
time series, free surface elevation, and probe point data.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional

import numpy as np
from numpy.typing import NDArray


# ============================================================================
# ForceTimeSeries
# ============================================================================


@dataclass
class ForceTimeSeries:
    """Force and moment time series extracted from OpenFOAM forces function.

    Attributes:
        times: Time array (s).
        fx: Pressure force component in x (N).
        fy: Pressure force component in y (N).
        fz: Pressure force component in z (N).
        mx: Moment about x axis (N·m). Optional.
        my: Moment about y axis (N·m). Optional.
        mz: Moment about z axis (N·m). Optional.
        fx_viscous: Viscous force x component (N). Optional.
        fy_viscous: Viscous force y component (N). Optional.
        fz_viscous: Viscous force z component (N). Optional.
        patch_name: Name of the force patch.
    """

    times: NDArray[np.float64]
    fx: NDArray[np.float64]
    fy: NDArray[np.float64]
    fz: NDArray[np.float64]
    mx: Optional[NDArray[np.float64]] = None
    my: Optional[NDArray[np.float64]] = None
    mz: Optional[NDArray[np.float64]] = None
    fx_viscous: Optional[NDArray[np.float64]] = None
    fy_viscous: Optional[NDArray[np.float64]] = None
    fz_viscous: Optional[NDArray[np.float64]] = None
    patch_name: str = ""

    @property
    def total_fx(self) -> NDArray[np.float64]:
        """Total x force = pressure + viscous."""
        if self.fx_viscous is not None:
            return self.fx + self.fx_viscous
        return self.fx

    @property
    def total_fy(self) -> NDArray[np.float64]:
        """Total y force = pressure + viscous."""
        if self.fy_viscous is not None:
            return self.fy + self.fy_viscous
        return self.fy

    @property
    def total_fz(self) -> NDArray[np.float64]:
        """Total z force = pressure + viscous."""
        if self.fz_viscous is not None:
            return self.fz + self.fz_viscous
        return self.fz

    @property
    def peak_fx(self) -> float:
        """Maximum absolute total x force (N)."""
        return float(np.max(np.abs(self.total_fx)))

    @property
    def peak_fy(self) -> float:
        """Maximum absolute total y force (N)."""
        return float(np.max(np.abs(self.total_fy)))

    @property
    def peak_fz(self) -> float:
        """Maximum absolute total z force (N)."""
        return float(np.max(np.abs(self.total_fz)))

    @property
    def mean_fx(self) -> float:
        """Mean total x force (N)."""
        return float(np.mean(self.total_fx))

    @property
    def mean_fy(self) -> float:
        """Mean total y force (N)."""
        return float(np.mean(self.total_fy))

    @property
    def mean_fz(self) -> float:
        """Mean total z force (N)."""
        return float(np.mean(self.total_fz))

    def to_dict(self) -> Dict[str, Any]:
        """Serialise to a plain dictionary."""
        return {
            "times": self.times.tolist(),
            "fx": self.fx.tolist(),
            "fy": self.fy.tolist(),
            "fz": self.fz.tolist(),
            "peak_fx": self.peak_fx,
            "peak_fy": self.peak_fy,
            "peak_fz": self.peak_fz,
            "mean_fx": self.mean_fx,
            "mean_fy": self.mean_fy,
            "mean_fz": self.mean_fz,
            "patch_name": self.patch_name,
        }


# ============================================================================
# ProbeTimeSeries
# ============================================================================


@dataclass
class ProbeTimeSeries:
    """Time series data at OpenFOAM probe points.

    Attributes:
        times: Time array (s).
        probe_coords: List of probe coordinates [[x,y,z], ...].
        values: Array shaped (n_timesteps, n_probes).
        field_name: Name of the sampled field (e.g. 'p', 'U').
    """

    times: NDArray[np.float64]
    probe_coords: List[List[float]]
    values: NDArray[np.float64]
    field_name: str = ""

    @property
    def n_probes(self) -> int:
        """Number of probe locations."""
        return len(self.probe_coords)

    @property
    def n_timesteps(self) -> int:
        """Number of time steps."""
        return len(self.times)

    def at_probe(self, probe_index: int) -> NDArray[np.float64]:
        """Return the time series at a specific probe.

        Args:
            probe_index: Zero-based index into probe_coords.

        Returns:
            1-D array of field values at that probe.
        """
        return self.values[:, probe_index]


# ============================================================================
# FreeSurfaceElevation
# ============================================================================


@dataclass
class FreeSurfaceElevation:
    """Free surface elevation extracted from alpha.water field.

    Attributes:
        times: Time array (s).
        x_positions: Probe x-positions along a transect (m).
        elevations: Array shaped (n_timesteps, n_positions) of eta (m).
    """

    times: NDArray[np.float64]
    x_positions: NDArray[np.float64]
    elevations: NDArray[np.float64]

    @property
    def n_positions(self) -> int:
        """Number of spatial positions."""
        return len(self.x_positions)

    @property
    def n_timesteps(self) -> int:
        """Number of time steps."""
        return len(self.times)

    @property
    def max_elevation(self) -> float:
        """Maximum free surface elevation across all times and positions."""
        return float(np.max(self.elevations))

    @property
    def min_elevation(self) -> float:
        """Minimum free surface elevation across all times and positions."""
        return float(np.min(self.elevations))
