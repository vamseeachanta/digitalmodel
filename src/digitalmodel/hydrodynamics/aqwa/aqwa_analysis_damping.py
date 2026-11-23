# Standard library imports
import math
import pathlib
from typing import Dict, Iterable, Optional, Sequence

# Third party imports
from assetutilities.common.update_deep import AttributeDict
import numpy as np
from .damping_physics import (
    citation_sidecar, critical_dof, frequency_grid, motion_amplitudes, positive,
)

# Reader imports
try:  # pragma: no cover - compatibility shim for environments without custom package
    from digitalmodel.hydrodynamics.aqwa.aqwa_analysis_raos import AqwaRAOs  # type: ignore
except ModuleNotFoundError:  # pragma: no cover
    from digitalmodel.hydrodynamics.aqwa.aqwa_analysis_raos import AqwaRAOs

aq_raos = AqwaRAOs()


class AqwaDamping:
    def __init__(self) -> None:
        pass

    def router(self, cfg: dict) -> None:
        self.run_all_files(cfg)

    def run_all_files(self, cfg: dict) -> None:
        input_files = cfg["file_management"]["input_files"]["DAT"]

        damping_output = []
        for file in input_files:
            filename_pattern = pathlib.Path(file).stem
            cfg_temp = AttributeDict(cfg.copy())

            analysis_settings_key = "damping"
            cfg_temp.analysis_settings[analysis_settings_key]["output"][
                "filename"
            ] = filename_pattern
            cfg_temp, damping_cfg = aq_raos.derive_damping(
                cfg_temp, analysis_settings_key
            )
            cfg_temp, additional_damping_values = aq_raos.prepare_damping(
                cfg_temp, damping_cfg, analysis_settings_key
            )

            damping_output.append(
                {
                    filename_pattern: cfg_temp.analysis_settings[analysis_settings_key][
                        "output"
                    ]["damping"]
                }
            )

        cfg["analysis_settings"][analysis_settings_key]["output"][
            "damping"
        ] = damping_output

    # ------------------------------------------------------------------
    # New viscous damping utilities
    # ------------------------------------------------------------------

    def compute_heading_rms(self, amplitudes: np.ndarray, axis: int = 1) -> np.ndarray:
        """Compute RMS of RAO amplitudes across headings."""

        values = np.asarray(amplitudes, dtype=float)
        if values.size == 0:
            return values
        if values.ndim == 1:
            values = values[:, np.newaxis]
        return np.sqrt(np.nanmean(np.square(values), axis=axis))

    def derive_viscous_damping_statistical(
        self, frequencies, raos, drag_properties, fluid_density=1025.0,
        *, wave_amplitude=None,
    ):
        """Harmonic energy-equivalent damping (legacy method name), E1-E3.

        Wave amplitude is {"value": positive peak amplitude, "unit": "m"}.
        RAOs carry amplitude arrays and explicit m/m, rad/m or deg/m units.
        """
        omega = frequency_grid(frequencies)
        if not isinstance(wave_amplitude, dict) or wave_amplitude.get("unit") != "m":
            raise ValueError("wave_amplitude must declare value and unit 'm'")
        amplitude = positive(wave_amplitude.get("value", 0), "wave amplitude")
        constant = (4.0 / (3.0 * math.pi)) * positive(fluid_density, "fluid density")
        viscous = {}
        for dof, key, axis in (("heave", "B33", None), ("roll", "B44", "x"), ("pitch", "B55", "y")):
            members = list(drag_properties.get(dof, []))
            motion = motion_amplitudes(raos, dof, len(omega), amplitude, bool(members))
            viscous[key] = self._aggregate_viscous_force(omega, motion, members, constant, axis)
        viscous["frequencies"] = omega
        viscous["linearisation"] = {"method": "harmonic", "wave_amplitude": dict(wave_amplitude),
                                    "heading_reduction": "quadratic mean of harmonic amplitudes"}
        viscous["citation_sidecar"] = citation_sidecar()
        return viscous

    def _resolve_rao_amplitudes(self, entry: Optional[np.ndarray], length: int) -> np.ndarray:
        if isinstance(entry, dict):
            entry = entry.get("amplitude")
        if entry is None:
            return np.zeros((length, 1), dtype=float)
        return np.asarray(entry, dtype=float)

    def _aggregate_viscous_force(
        self,
        omega: np.ndarray,
        rao_rms: np.ndarray,
        members: Iterable[Dict[str, float]],
        constant: float,
        moment_axis: Optional[str],
    ) -> np.ndarray:
        """Aggregate viscous contributions from Morison members."""

        if omega.size == 0:
            return omega

        total = np.zeros_like(omega, dtype=float)

        for member in members:
            cd = float(member.get("cd", 0.0))
            area = float(member.get("area", 0.0))
            lever = float(member.get("lever_arm", 0.0))

            if not all(math.isfinite(v) for v in (cd, area, lever)) or cd < 0 or area < 0:
                raise ValueError("Drag coefficient/area must be finite and nonnegative; arm finite")
            force = constant * cd * area * omega * rao_rms
            if moment_axis:
                total += force * abs(lever) ** 3  # damping-basis.md E3
            else:
                total += force

        return total

    def combine_damping(
        self,
        frequencies: Sequence[float],
        radiation: Dict[str, np.ndarray],
        viscous: Dict[str, np.ndarray],
        external: Optional[Dict[str, np.ndarray]] = None,
    ) -> Dict[str, Dict[str, np.ndarray]]:
        """Combine damping sources and compute percentage contributions."""

        omega = np.asarray(frequencies, dtype=float)
        external = external or {}

        totals: Dict[str, np.ndarray] = {}
        percentages: Dict[str, Dict[str, np.ndarray]] = {}

        for dof_key in ("B33", "B44", "B55"):
            rad = np.asarray(radiation.get(dof_key, np.zeros_like(omega)))
            visc = np.asarray(viscous.get(dof_key, np.zeros_like(omega)))
            ext = np.asarray(external.get(dof_key, np.zeros_like(omega)))

            total = rad + visc + ext
            totals[dof_key] = total

            with np.errstate(divide="ignore", invalid="ignore"):
                pct_rad = np.where(total != 0, rad / total * 100.0, 0.0)
                pct_visc = np.where(total != 0, visc / total * 100.0, 0.0)
                pct_ext = np.where(total != 0, ext / total * 100.0, 0.0)

            percentages[dof_key] = {
                "radiation": pct_rad,
                "viscous": pct_visc,
                "external": pct_ext,
            }

        return {
            "frequencies": omega,
            "total": totals,
            "components": {
                "radiation": radiation,
                "viscous": viscous,
                "external": external,
            },
            "percentages": percentages,
        }

    def compute_critical_damping_ratios(
        self,
        frequencies: Sequence[float],
        total_damping: Dict[str, np.ndarray],
        inertia: Dict[str, float],
        restoring: Dict[str, float],
        added_mass_frequency: Optional[Dict[str, np.ndarray]] = None,
        added_mass_infinite: Optional[Dict[str, float]] = None,
        use_frequency_dependent: bool = False,
    ) -> Dict[str, Dict[str, np.ndarray]]:
        """Compute critical damping ratios using provided mass and stiffness data."""

        omega = frequency_grid(frequencies)
        results = {}
        for dof, suffix in (("heave", "33"), ("roll", "44"), ("pitch", "55")):
            mass_source = added_mass_frequency if use_frequency_dependent else added_mass_infinite
            required = ((inertia, "I"), (restoring, "C"), (total_damping, "B"), (mass_source or {}, "A"))
            for source, prefix in required:
                if prefix + suffix not in source:
                    raise ValueError(f"{dof}: missing required coefficient {prefix + suffix}")
            results[dof] = critical_dof(
                omega, total_damping["B" + suffix], inertia["I" + suffix],
                restoring["C" + suffix], mass_source["A" + suffix], dof,
            )
        return results
