"""
ABOUTME: Design table batch studies -- enumerate parametric hull/FEM variations
from YAML config, run hydrostatics or FEM in batch, and produce comparison results.
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

# FEM parameters that can be varied in plate-with-hole studies
_FEM_PARAMS: dict[str, str] = {
    "hole_radius": "hole_radius",
    "plate_width": "plate_width",
    "plate_height": "plate_height",
    "thickness": "thickness",
    "element_size": "element_size",
    "youngs_modulus": "youngs_modulus",
}

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

    # ------------------------------------------------------------------
    # FEM batch mode
    # ------------------------------------------------------------------

    @classmethod
    def for_fem(cls) -> DesignTable:
        """Create a DesignTable configured for FEM parameter studies.

        No base hull profile is needed — FEM mode operates on
        plate-with-hole geometry via FEMChain.
        """
        dt = cls.__new__(cls)
        dt._base = None
        dt._parameters = {}
        dt._variations = []
        dt._results = []
        dt._fem_parameters: dict[str, list[float]] = {}
        dt._fem_results: list[dict] = []
        return dt

    def add_fem_parameter(self, name: str, values: list[float]) -> None:
        """Add a FEM parameter variation axis.

        Parameters
        ----------
        name : str
            One of: hole_radius, plate_width, plate_height, thickness,
            element_size, youngs_modulus.
        values : list[float]
            Values to enumerate. All must be positive.
        """
        if name not in _FEM_PARAMS:
            raise ValueError(
                f"Unknown FEM parameter '{name}'. "
                f"Valid: {list(_FEM_PARAMS.keys())}"
            )
        for v in values:
            if v <= 0:
                raise ValueError(
                    f"All parameter values must be positive, got {v}"
                )
        if not hasattr(self, "_fem_parameters"):
            self._fem_parameters = {}
        self._fem_parameters[name] = list(values)

    def run_batch_fem(
        self,
        work_dir: Path | None = None,
        sigma_applied: float = 100.0,
    ) -> list[dict]:
        """Run FEM analysis for all parameter combinations.

        Each combination creates a plate-with-hole model via FEMChain,
        solves it, and extracts the stress concentration factor Kt.
        """
        from digitalmodel.solvers.calculix.fem_chain import FEMChain

        if not hasattr(self, "_fem_parameters"):
            raise RuntimeError("No FEM parameters defined")

        defaults = {
            "hole_radius": 10.0,
            "plate_width": 200.0,
            "plate_height": 200.0,
            "thickness": 1.0,
            "element_size": 8.0,
            "youngs_modulus": 210000.0,
        }

        param_names = list(self._fem_parameters.keys())
        param_values = [self._fem_parameters[n] for n in param_names]
        combos = list(itertools.product(*param_values))

        self._fem_results = []
        for i, combo in enumerate(combos):
            params = dict(defaults)
            params.update(dict(zip(param_names, combo)))

            sub_dir = None
            if work_dir is not None:
                sub_dir = work_dir / f"fem_var_{i}"
                sub_dir.mkdir(parents=True, exist_ok=True)

            chain = FEMChain(work_dir=sub_dir)
            stats = chain.create_plate_with_hole(
                plate_w=params["plate_width"],
                plate_h=params["plate_height"],
                hole_r=params["hole_radius"],
                thickness=params["thickness"],
                element_size=params["element_size"],
            )
            # Distribute applied stress as equivalent nodal forces
            # Total force on quarter-model far edge = σ × t × (H/2)
            n_load = len(chain._node_sets.get("LOAD", [1]))
            half_h = params["plate_height"] / 2.0
            force_per_node = (
                sigma_applied * params["thickness"] * half_h / n_load
            )
            chain.setup_analysis(
                material={
                    "name": "STEEL",
                    "E": params["youngs_modulus"],
                    "nu": 0.3,
                },
                loads=[{
                    "type": "cload",
                    "node_set": "LOAD",
                    "dof": 1,
                    "magnitude": force_per_node,
                    "direction": (1, 0, 0),
                }],
                boundary_conditions=[
                    {"node_set": "SYM_X", "dof_start": 1, "dof_end": 1},
                    {"node_set": "SYM_Y", "dof_start": 2, "dof_end": 2},
                    {"node_set": "FIX_Z", "dof_start": 3, "dof_end": 3},
                ],
            )
            status = chain.solve()
            if not status["success"]:
                self._fem_results.append({
                    "parameters": params,
                    "success": False,
                    "error": status["stderr"],
                })
                continue

            results = chain.extract_results()
            kt = results["hole_sxx"] / sigma_applied
            self._fem_results.append({
                "parameters": params,
                "success": True,
                "kt": kt,
                **results,
                **stats,
            })

        return self._fem_results

    def export_fem_results_yaml(self, output_path: Path) -> Path:
        """Write FEM batch results to YAML."""
        if not hasattr(self, "_fem_results") or not self._fem_results:
            raise RuntimeError(
                "No FEM results. Run run_batch_fem() first."
            )
        variations = []
        for i, r in enumerate(self._fem_results):
            entry: dict = {"index": i}
            entry["parameters"] = {
                k: float(v) for k, v in r["parameters"].items()
            }
            if r["success"]:
                entry["kt"] = float(r["kt"])
                entry["max_von_mises"] = float(r["max_von_mises"])
            else:
                entry["error"] = r.get("error", "unknown")
            variations.append(entry)

        output = {
            "mode": "fem_batch",
            "n_variations": len(variations),
            "variations": variations,
        }
        with open(output_path, "w") as f:
            yaml.dump(output, f, default_flow_style=False, sort_keys=False)
        return Path(output_path)
