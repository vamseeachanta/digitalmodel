# Standard library imports
import math
import pathlib
from typing import Dict, Iterable, Optional, Sequence

# Third party imports
from assetutilities.common.update_deep import AttributeDict
import numpy as np

# Reader imports
try:  # pragma: no cover - compatibility shim for environments without custom package
    from digitalmodel.custom.aqwa.aqwa_analysis_raos import AqwaRAOs  # type: ignore
except ModuleNotFoundError:  # pragma: no cover
    from digitalmodel.modules.aqwa.aqwa_analysis_raos import AqwaRAOs

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
        self,
        frequencies: Sequence[float],
        raos: Dict[str, np.ndarray],
        drag_properties: Dict[str, Iterable[Dict[str, float]]],
        fluid_density: float = 1025.0,
    ) -> Dict[str, np.ndarray]:
        """Derive equivalent viscous damping using statistical linearization."""

        omega = np.asarray(frequencies, dtype=float)
        viscous: Dict[str, np.ndarray] = {
            "B33": np.zeros_like(omega, dtype=float),
            "B44": np.zeros_like(omega, dtype=float),
            "B55": np.zeros_like(omega, dtype=float),
        }

        heave_rms = self.compute_heading_rms(self._resolve_rao_amplitudes(raos.get("heave"), len(omega)))
        roll_rms = self.compute_heading_rms(self._resolve_rao_amplitudes(raos.get("roll"), len(omega)))
        pitch_rms = self.compute_heading_rms(self._resolve_rao_amplitudes(raos.get("pitch"), len(omega)))

        constant = (8.0 / (3.0 * math.pi)) * fluid_density

        viscous["B33"] = self._aggregate_viscous_force(
            omega,
            heave_rms,
            drag_properties.get("heave", []),
            constant,
            moment_axis=None,
        )

        roll_components = self._aggregate_viscous_force(
            omega,
            roll_rms,
            drag_properties.get("roll", []),
            constant,
            moment_axis="x",
        )
        viscous["B44"] = roll_components

        pitch_components = self._aggregate_viscous_force(
            omega,
            pitch_rms,
            drag_properties.get("pitch", []),
            constant,
            moment_axis="y",
        )
        viscous["B55"] = pitch_components

        viscous["frequencies"] = omega
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

            force = constant * cd * area * omega * rao_rms
            if moment_axis:
                total += force * lever
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

        omega = np.asarray(frequencies, dtype=float)
        results: Dict[str, Dict[str, np.ndarray]] = {}

        dof_map = {
            "heave": ("B33", "I33", "C33", "A33"),
            "roll": ("B44", "I44", "C44", "A44"),
            "pitch": ("B55", "I55", "C55", "A55"),
        }

        for dof, (b_key, inertia_key, restoring_key, added_mass_key) in dof_map.items():
            damping_values = np.asarray(total_damping.get(b_key, np.zeros_like(omega)))
            inertia_value = float(inertia.get(inertia_key, 0.0))
            restoring_value = float(restoring.get(restoring_key, 0.0))

            if use_frequency_dependent:
                added_mass_series = np.asarray(
                    (added_mass_frequency or {}).get(added_mass_key, np.zeros_like(omega))
                )
                denominator = inertia_value + added_mass_series
            else:
                added_mass_scalar = float((added_mass_infinite or {}).get(added_mass_key, 0.0))
                denominator = inertia_value + added_mass_scalar

            with np.errstate(divide="ignore", invalid="ignore"):
                natural_frequency = np.sqrt(
                    np.where(denominator > 0, restoring_value / denominator, 0.0)
                )
                critical_ratio = np.where(
                    denominator * natural_frequency * 2.0 > 0,
                    damping_values / (2.0 * denominator * natural_frequency),
                    0.0,
                )
                natural_period = np.where(
                    natural_frequency > 0,
                    (2.0 * math.pi) / natural_frequency,
                    math.inf,
                )

            results[dof] = {
                "critical_damping_ratio": critical_ratio,
                "natural_frequency_rad_s": natural_frequency,
                "natural_period_s": natural_period,
                "damping": damping_values,
                "denominator": denominator,
            }

        return results
