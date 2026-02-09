"""
ABOUTME: Hull catalog and analysis chain linking hull profiles to the full
hydrodynamic pipeline: hull -> mesh -> RAOs -> motion response -> accelerations.

Provides a registry of hull profiles with methods to generate meshes, compute
motion responses from RAO data, and derive point-specific accelerations using
rigid-body kinematics and spectral analysis.
"""

from __future__ import annotations

from pathlib import Path
from typing import Optional

import numpy as np
from pydantic import BaseModel, ConfigDict, Field
from scipy.interpolate import interp1d

from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
    HullMeshGenerator,
    MeshGeneratorConfig,
)
from digitalmodel.hydrodynamics.hull_library.profile_schema import (
    HullProfile,
)
from digitalmodel.hydrodynamics.models import (
    RAOData,
    WaveParameters,
    WaveSpectrumType,
)
from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra


# ---------------------------------------------------------------------------
# Numpy integration helper
# ---------------------------------------------------------------------------

def _trapz(y: np.ndarray, x: np.ndarray) -> float:
    """Integrate using trapezoidal rule, compatible with old and new numpy."""
    if hasattr(np, "trapezoid"):
        return float(np.trapezoid(y, x))
    return float(np.trapz(y, x))


# ---------------------------------------------------------------------------
# Data models
# ---------------------------------------------------------------------------


class SeaStateDefinition(BaseModel):
    """Sea state for motion response calculation."""

    significant_height: float = Field(
        ..., gt=0, description="Significant wave height Hs (m)"
    )
    peak_period: float = Field(
        ..., gt=0, description="Peak spectral period Tp (s)"
    )
    spectrum_type: str = Field(
        default="jonswap",
        description="Spectrum type matching WaveSpectrumType values",
    )
    gamma: float = Field(
        default=3.3, gt=0, description="JONSWAP peakedness parameter"
    )
    heading: float = Field(
        default=0.0, description="Wave heading in degrees, 0 = head seas"
    )


class HullVariation(BaseModel):
    """A parametric variation of a hull."""

    variation_id: str
    scale_factors: dict[str, float] = Field(
        default_factory=lambda: {"length": 1.0, "beam": 1.0, "draft": 1.0}
    )
    mesh_config: MeshGeneratorConfig = Field(
        default_factory=MeshGeneratorConfig
    )


class MotionResponse(BaseModel):
    """Motion response for a hull in a specific sea state."""

    model_config = ConfigDict(arbitrary_types_allowed=True)

    sea_state: SeaStateDefinition
    significant_values: dict[str, float] = Field(
        default_factory=dict,
        description="Significant response per DOF (e.g. heave_sig)",
    )
    accelerations: dict[str, dict[str, float]] = Field(
        default_factory=dict,
        description=(
            "Point accelerations: "
            "{point_label: {vertical, lateral, longitudinal}}"
        ),
    )


class HullCatalogEntry(BaseModel):
    """A hull in the catalog with its profile and variations."""

    hull_id: str
    profile: HullProfile
    variations: list[HullVariation] = Field(default_factory=list)


# ---------------------------------------------------------------------------
# DOF labels in standard naval architecture order
# ---------------------------------------------------------------------------

_DOF_NAMES = ["surge", "sway", "heave", "roll", "pitch", "yaw"]


# ---------------------------------------------------------------------------
# Catalog
# ---------------------------------------------------------------------------


class HullCatalog:
    """Registry linking hull profiles to the full analysis chain.

    Supports loading hull profiles from YAML, generating panel meshes,
    computing motion responses via spectral analysis, and deriving
    point-specific accelerations using rigid-body kinematics.
    """

    def __init__(self, profiles_dir: Optional[Path] = None):
        """Initialise the catalog, optionally loading profiles from disk.

        Args:
            profiles_dir: Directory containing hull profile YAML files.
                          All ``*.yaml`` files in the directory are loaded.
        """
        self._entries: dict[str, HullCatalogEntry] = {}
        self._mesh_generator = HullMeshGenerator()
        self._wave_spectra = WaveSpectra()

        if profiles_dir is not None and profiles_dir.exists():
            self._load_profiles(profiles_dir)

    # ------------------------------------------------------------------
    # Public API -- registry
    # ------------------------------------------------------------------

    def list_hulls(self) -> list[str]:
        """Return sorted list of registered hull IDs."""
        return sorted(self._entries.keys())

    def get_hull(self, hull_id: str) -> HullCatalogEntry:
        """Retrieve a catalog entry by hull ID.

        Raises:
            KeyError: If *hull_id* is not in the catalog.
        """
        if hull_id not in self._entries:
            raise KeyError(
                f"Hull '{hull_id}' not found. "
                f"Available: {self.list_hulls()}"
            )
        return self._entries[hull_id]

    def register_hull(self, profile: HullProfile) -> HullCatalogEntry:
        """Register a hull profile and return its catalog entry.

        The hull ID is derived from ``profile.name``.
        """
        entry = HullCatalogEntry(hull_id=profile.name, profile=profile)
        self._entries[profile.name] = entry
        return entry

    # ------------------------------------------------------------------
    # Public API -- mesh generation
    # ------------------------------------------------------------------

    def generate_mesh(
        self,
        hull_id: str,
        config: Optional[MeshGeneratorConfig] = None,
    ):
        """Generate a panel mesh for the given hull.

        Args:
            hull_id: Registered hull identifier.
            config: Mesh generation settings. Uses defaults when *None*.

        Returns:
            A ``PanelMesh`` instance.

        Raises:
            KeyError: If *hull_id* is not registered.
        """
        entry = self.get_hull(hull_id)
        return self._mesh_generator.generate(entry.profile, config)

    # ------------------------------------------------------------------
    # Public API -- motion response
    # ------------------------------------------------------------------

    def compute_motions(
        self,
        hull_id: str,
        sea_state: SeaStateDefinition,
        rao_data: RAOData,
    ) -> MotionResponse:
        """Compute motion response using RAO x wave spectrum.

        Algorithm:
            1. Generate wave spectrum from *sea_state*.
            2. For each DOF, find RAO amplitudes at the nearest heading.
            3. Interpolate RAOs to spectrum frequency grid.
            4. S_response(w) = |RAO(w)|^2 * S_wave(w)
            5. m0 = integral of S_response dw
            6. significant = 2 * sqrt(m0)

        Args:
            hull_id: Registered hull identifier (used for validation).
            sea_state: Environmental sea state definition.
            rao_data: RAO data for the vessel.

        Returns:
            ``MotionResponse`` with significant values for each DOF.
        """
        # Validate hull exists
        self.get_hull(hull_id)

        # 1. Build wave spectrum
        omega, s_wave = self._generate_spectrum(sea_state)

        # 2. Find nearest heading direction index in RAO data
        dir_idx = int(np.argmin(np.abs(rao_data.directions - sea_state.heading)))

        significant_values: dict[str, float] = {}

        for dof_idx, dof_name in enumerate(_DOF_NAMES):
            # 3. Extract RAO amplitudes for this DOF at the selected heading
            rao_amp = rao_data.amplitudes[:, dir_idx, dof_idx]

            # 4. Interpolate RAOs to spectrum frequency grid
            rao_interp = self._interpolate_rao(
                rao_data.frequencies, rao_amp, omega
            )

            # 5. Response spectrum: S_resp = |RAO|^2 * S_wave
            s_response = rao_interp**2 * s_wave

            # 6. Zeroth moment and significant value
            m0 = _trapz(s_response, omega)
            sig = 2.0 * np.sqrt(max(m0, 0.0))
            significant_values[f"{dof_name}_sig"] = float(sig)

        return MotionResponse(
            sea_state=sea_state,
            significant_values=significant_values,
        )

    # ------------------------------------------------------------------
    # Public API -- point accelerations
    # ------------------------------------------------------------------

    def compute_accelerations(
        self,
        hull_id: str,
        sea_state: SeaStateDefinition,
        rao_data: RAOData,
        point: tuple[float, float, float],
        cog: tuple[float, float, float] = (0.0, 0.0, 0.0),
    ) -> dict[str, float]:
        """Compute point-specific accelerations via rigid-body transfer.

        Algorithm:
            1. Compute lever arms dx, dy, dz from COG to point.
            2. For each translational direction, combine direct translational
               RAOs with rotational DOF contributions via cross-product
               kinematics, then convert displacement RAO to acceleration
               RAO (multiply by omega^2).
            3. Compute acceleration response spectrum and integrate for
               significant acceleration.

        Args:
            hull_id: Registered hull identifier.
            sea_state: Environmental sea state definition.
            rao_data: RAO data for the vessel.
            point: (x, y, z) coordinates of the point of interest.
            cog: (x, y, z) coordinates of the centre of gravity.

        Returns:
            Dict with keys ``vertical``, ``lateral``, ``longitudinal``
            and significant acceleration values in m/s^2.
        """
        self.get_hull(hull_id)

        # 1. Generate wave spectrum
        omega, s_wave = self._generate_spectrum(sea_state)

        # 2. Lever arms
        dx = point[0] - cog[0]
        dy = point[1] - cog[1]
        dz = point[2] - cog[2]

        # 3. Heading index
        dir_idx = int(np.argmin(np.abs(rao_data.directions - sea_state.heading)))

        # Extract and interpolate all 6 DOF RAOs to spectrum frequencies
        rao_interp = np.zeros((len(omega), 6))
        for dof_idx in range(6):
            rao_amp = rao_data.amplitudes[:, dir_idx, dof_idx]
            rao_interp[:, dof_idx] = self._interpolate_rao(
                rao_data.frequencies, rao_amp, omega
            )

        # Name aliases for clarity
        surge_rao = rao_interp[:, 0]  # x-translation
        sway_rao = rao_interp[:, 1]   # y-translation
        heave_rao = rao_interp[:, 2]  # z-translation
        roll_rao = rao_interp[:, 3]   # rotation about x (rad/m)
        pitch_rao = rao_interp[:, 4]  # rotation about y (rad/m)
        yaw_rao = rao_interp[:, 5]    # rotation about z (rad/m)

        # 4. Acceleration RAOs at the point (displacement -> acceleration: * w^2)
        # Rigid-body transfer: translational acceleration at point =
        #   direct_translation * w^2
        #   + cross-product terms from rotational DOFs * lever arm * w^2
        #
        # Longitudinal (x): surge + pitch*dz - yaw*dy
        # Lateral (y):      sway - roll*dz + yaw*dx
        # Vertical (z):     heave - pitch*dx + roll*dy
        #
        # All multiplied by w^2 to convert displacement to acceleration.

        omega_sq = omega**2

        acc_rao_x = (surge_rao + pitch_rao * dz - yaw_rao * dy) * omega_sq
        acc_rao_y = (sway_rao - roll_rao * dz + yaw_rao * dx) * omega_sq
        acc_rao_z = (heave_rao - pitch_rao * dx + roll_rao * dy) * omega_sq

        # 5. Acceleration response spectra and significant values
        results: dict[str, float] = {}
        for label, acc_rao in [
            ("longitudinal", acc_rao_x),
            ("lateral", acc_rao_y),
            ("vertical", acc_rao_z),
        ]:
            s_acc = acc_rao**2 * s_wave
            m0 = _trapz(s_acc, omega)
            a_sig = 2.0 * np.sqrt(max(m0, 0.0))
            results[label] = float(a_sig)

        return results

    # ------------------------------------------------------------------
    # Private helpers
    # ------------------------------------------------------------------

    def _load_profiles(self, profiles_dir: Path) -> None:
        """Load all YAML hull profiles from *profiles_dir*."""
        for yaml_path in sorted(profiles_dir.glob("*.yaml")):
            try:
                profile = HullProfile.load_yaml(yaml_path)
                self.register_hull(profile)
            except Exception:
                # Skip files that cannot be parsed
                continue

    def _generate_spectrum(
        self, sea_state: SeaStateDefinition
    ) -> tuple[np.ndarray, np.ndarray]:
        """Generate a wave spectrum from a ``SeaStateDefinition``."""
        spectrum_type = WaveSpectrumType(sea_state.spectrum_type)
        params = WaveParameters(
            spectrum_type=spectrum_type,
            significant_height=sea_state.significant_height,
            peak_period=sea_state.peak_period,
            gamma=sea_state.gamma,
        )
        return self._wave_spectra.generate_spectrum(params)

    @staticmethod
    def _interpolate_rao(
        rao_frequencies: np.ndarray,
        rao_amplitudes: np.ndarray,
        target_frequencies: np.ndarray,
    ) -> np.ndarray:
        """Interpolate RAO amplitudes onto *target_frequencies*.

        Uses linear interpolation with zero fill outside the RAO range.
        """
        interp_fn = interp1d(
            rao_frequencies,
            rao_amplitudes,
            kind="linear",
            bounds_error=False,
            fill_value=0.0,
        )
        return interp_fn(target_frequencies)


__all__ = [
    "SeaStateDefinition",
    "HullVariation",
    "MotionResponse",
    "HullCatalogEntry",
    "HullCatalog",
]
