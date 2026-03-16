"""
ABOUTME: Design table batch studies -- enumerate parametric hull variations from
YAML config, run hydrostatics in parallel, and produce comparison results.
"""

from __future__ import annotations

import copy
import itertools
import multiprocessing
from pathlib import Path
from typing import ClassVar

import yaml

from digitalmodel.hydrodynamics.hull_library.profile_schema import HullProfile
from digitalmodel.visualization.design_tools.hull_hydrostatics import (
    HullHydrostatics,
)

# Parameters that can be varied and their HullProfile field names
_SCALABLE_PARAMS: dict[str, str] = {
    "length_bp": "length_bp",
    "beam": "beam",
    "draft": "draft",
    "depth": "depth",
}


def _compute_hydrostatics_worker(profile_dict: dict) -> dict:
    """Worker function for multiprocessing -- takes a serialized profile."""
    profile = HullProfile.from_yaml_dict(profile_dict)
    hydro = HullHydrostatics(profile)
    return hydro.compute_all()


class DesignTable:
    """Parametric design table for hull variation studies.

    Enumerates combinations of hull parameters, generates modified
    hull profiles, runs batch hydrostatics, and exports comparison
    results to YAML.
    """

    def __init__(self, base_profile: HullProfile) -> None:
        self._base = base_profile
        self._parameters: dict[str, list[float]] = {}
        self._variations: list[HullProfile] = []
        self._results: list[dict] = []

    @classmethod
    def from_yaml(
        cls, config_path: Path, base_profile: HullProfile
    ) -> DesignTable:
        """Load parameter variations from a YAML config file.

        Expected format::

            parameters:
              length_bp: [90.0, 100.0, 110.0]
              beam: [18.0, 20.0]
        """
        with open(config_path) as f:
            config = yaml.safe_load(f)
        dt = cls(base_profile)
        for name, values in config.get("parameters", {}).items():
            dt.add_parameter(name, values)
        return dt

    def add_parameter(self, name: str, values: list[float]) -> None:
        """Add a parameter variation axis.

        Parameters
        ----------
        name : str
            Parameter name (must be a scalable hull dimension).
        values : list[float]
            List of values to enumerate. All must be positive.

        Raises
        ------
        ValueError
            If any value is not positive or name is not recognized.
        """
        if name not in _SCALABLE_PARAMS and name != "block_coefficient":
            raise ValueError(
                f"Unknown parameter '{name}'. "
                f"Valid: {list(_SCALABLE_PARAMS.keys()) + ['block_coefficient']}"
            )
        for v in values:
            if v <= 0:
                raise ValueError(
                    f"All parameter values must be positive, got {v}"
                )
        self._parameters[name] = list(values)

    def generate_variations(self) -> list[HullProfile]:
        """Generate cartesian product of all parameter axes.

        Returns a list of modified HullProfile instances. Stations are
        proportionally rescaled when dimensions change.
        """
        if not self._parameters:
            self._variations = [copy.deepcopy(self._base)]
            return self._variations

        param_names = list(self._parameters.keys())
        param_values = [self._parameters[n] for n in param_names]
        combos = list(itertools.product(*param_values))

        variations: list[HullProfile] = []
        for combo in combos:
            overrides = dict(zip(param_names, combo))
            profile = self._apply_overrides(overrides)
            variations.append(profile)

        self._variations = variations
        return variations

    def run_batch_hydrostatics(
        self, parallel: bool = True
    ) -> list[dict]:
        """Compute hydrostatics for all variations.

        Parameters
        ----------
        parallel : bool
            If True, use multiprocessing.Pool. If False, run sequentially.
        """
        if not self._variations:
            self.generate_variations()

        if parallel and len(self._variations) > 1:
            serialized = [
                v.to_yaml_dict() for v in self._variations
            ]
            with multiprocessing.Pool() as pool:
                self._results = pool.map(
                    _compute_hydrostatics_worker, serialized
                )
        else:
            self._results = []
            for v in self._variations:
                hydro = HullHydrostatics(v)
                self._results.append(hydro.compute_all())

        return self._results

    def export_results_yaml(self, output_path: Path) -> Path:
        """Write comparison results to YAML.

        Returns the output path.
        """
        if not self._results:
            raise RuntimeError(
                "No results to export. Run run_batch_hydrostatics() first."
            )

        variations_data: list[dict] = []
        for i, (var, res) in enumerate(
            zip(self._variations, self._results)
        ):
            params: dict = {
                "length_bp": var.length_bp,
                "beam": var.beam,
                "draft": var.draft,
                "depth": var.depth,
            }
            if var.block_coefficient is not None:
                params["block_coefficient"] = var.block_coefficient
            variations_data.append(
                {
                    "index": i,
                    "parameters": params,
                    "hydrostatics": {
                        k: float(v) for k, v in res.items()
                    },
                }
            )

        output = {
            "base_profile": self._base.name,
            "n_variations": len(self._variations),
            "variations": variations_data,
        }

        with open(output_path, "w") as f:
            yaml.dump(output, f, default_flow_style=False, sort_keys=False)
        return output_path

    def get_results_dataframe(self) -> list[dict]:
        """Return results as a list of dicts (one per variation)."""
        rows: list[dict] = []
        for var, res in zip(self._variations, self._results):
            row = {
                "length_bp": var.length_bp,
                "beam": var.beam,
                "draft": var.draft,
                "depth": var.depth,
            }
            row.update(res)
            rows.append(row)
        return rows

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _apply_overrides(self, overrides: dict) -> HullProfile:
        """Create a modified profile with scaled stations."""
        base_dict = self._base.to_yaml_dict()

        # Compute scale factors
        length_scale = overrides.get(
            "length_bp", self._base.length_bp
        ) / self._base.length_bp
        beam_scale = overrides.get(
            "beam", self._base.beam
        ) / self._base.beam
        draft_scale = overrides.get(
            "draft", self._base.draft
        ) / self._base.draft
        depth_scale = overrides.get(
            "depth", self._base.depth
        ) / self._base.depth

        # Apply dimension overrides
        for param, field in _SCALABLE_PARAMS.items():
            if param in overrides:
                base_dict[field] = overrides[param]

        if "block_coefficient" in overrides:
            base_dict["block_coefficient"] = overrides["block_coefficient"]

        # Scale stations proportionally
        for station in base_dict["stations"]:
            station["x_position"] *= length_scale
            scaled_offsets = []
            for z, y in station["waterline_offsets"]:
                new_z = z * depth_scale
                new_y = y * beam_scale
                scaled_offsets.append([new_z, new_y])
            station["waterline_offsets"] = scaled_offsets

        return HullProfile.from_yaml_dict(base_dict)
