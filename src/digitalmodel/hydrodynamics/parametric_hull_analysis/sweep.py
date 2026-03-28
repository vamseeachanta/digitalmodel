"""
ABOUTME: Core parametric hull sweep — generates hull variants via hull_library,
runs BEM analysis via Capytaine, and collects RAO results into a queryable
RAODatabase.  Orchestration layer only; no solver logic duplicated.
"""

from __future__ import annotations

import logging
import time
from pathlib import Path
from typing import Optional

import numpy as np
import pandas as pd

from .models import SweepConfig, SweepResultEntry, classify_depth

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def run_parametric_sweep(
    config: SweepConfig,
    catalog: object,
    output_dir: Optional[Path] = None,
) -> tuple[list[SweepResultEntry], object]:
    """Execute a full parametric hull sweep.

    Parameters
    ----------
    config : SweepConfig
        Sweep configuration (hull ranges, wave conditions, solver settings).
    catalog : HullCatalog
        Hull catalog with the base hull registered.
    output_dir : Path, optional
        Directory for mesh files and results.  Defaults to a temporary
        directory if not provided.

    Returns
    -------
    results : list[SweepResultEntry]
        One entry per hull variation with full BEM + RAO results.
    rao_db : RAODatabase
        Database of RAO results indexed by variation_id.

    Raises
    ------
    KeyError
        If ``config.base_hull_id`` is not registered in the catalog.
    ImportError
        If Capytaine is not available.
    """
    from digitalmodel.hydrodynamics.hull_library import (
        HullParametricSpace,
        HullMeshGenerator,
        MeshGeneratorConfig,
        RAODatabase,
        RAODatabaseEntry,
        export_scaled_gdf,
    )
    from digitalmodel.hydrodynamics.capytaine import (
        run_bem_analysis,
        compute_rao,
    )

    # Build parametric space
    space = HullParametricSpace(
        base_hull_id=config.base_hull_id,
        ranges=config.ranges,
        fixed_params=config.fixed_params,
    )

    # Set up output directory
    if output_dir is None:
        import tempfile
        output_dir = Path(tempfile.mkdtemp(prefix="hull_sweep_"))
    mesh_dir = output_dir / "meshes"
    mesh_dir.mkdir(parents=True, exist_ok=True)

    # Build wave conditions once (shared across all variations)
    wave_conditions = _build_wave_conditions(config)

    # Solver config
    solver_config = _build_solver_config(config)

    # Mesh generator config
    mesh_config = MeshGeneratorConfig(
        target_panels=config.target_panels,
        symmetry=config.symmetry,
    )
    mesh_gen = HullMeshGenerator()

    rao_db = RAODatabase()
    results: list[SweepResultEntry] = []

    for variation_id, profile in space.generate_profiles(catalog):
        t0 = time.monotonic()
        logger.info(
            "Processing %s (L=%.1f B=%.1f T=%.1f)",
            variation_id, profile.length_bp, profile.beam, profile.draft,
        )

        # 1. Generate panel mesh
        panel_mesh = mesh_gen.generate(profile, mesh_config)
        logger.debug("  Mesh: %d panels", panel_mesh.n_panels)

        # 2. Export mesh to GDF
        gdf_path = mesh_dir / f"{variation_id}.gdf"
        export_scaled_gdf(panel_mesh, gdf_path)

        # 3. Build body config
        body_config = _profile_to_body_config(profile, gdf_path)

        # 4. Run BEM analysis
        bem_result = run_bem_analysis(
            body_config, wave_conditions, solver_config,
            compute_hydrostatics=True,
        )

        # 5. Compute RAO
        rao_result = compute_rao(bem_result)

        # 6. Classify depth
        depth_class = classify_depth(config.water_depth, profile.draft)

        # 7. Build result entry
        hull_params = {
            k: v for combo in [space.fixed_params]
            for k, v in combo.items()
        }
        # Extract hull params from variation_id
        hull_params = _extract_hull_params(variation_id, space)

        entry = SweepResultEntry(
            variation_id=variation_id,
            hull_params=hull_params,
            length_bp=profile.length_bp,
            beam=profile.beam,
            draft=profile.draft,
            block_coefficient=profile.block_coefficient,
            bem_result=bem_result,
            rao_result=rao_result,
            depth_class=depth_class,
            forward_speed_ms=config.forward_speed_ms or 0.0,
            metadata={
                "n_panels": panel_mesh.n_panels,
                "gdf_path": str(gdf_path),
                "solve_time_s": time.monotonic() - t0,
            },
        )
        results.append(entry)

        # 8. Store in RAO database
        rao_db_entry = _to_rao_db_entry(entry, rao_result, profile)
        rao_db.store(rao_db_entry)

        logger.info(
            "  Completed in %.1fs", time.monotonic() - t0,
        )

    # Save database to disk if output_dir provided
    if output_dir is not None:
        db_path = output_dir / "rao_database.parquet"
        try:
            rao_db.save_to_disk(db_path)
            logger.info("RAO database saved: %s", db_path)
        except Exception:
            logger.warning("Could not save RAO database to disk", exc_info=True)

    logger.info(
        "Sweep complete: %d variations processed", len(results),
    )
    return results, rao_db


def sweep_to_dataframe(results: list[SweepResultEntry]) -> pd.DataFrame:
    """Build a comparison DataFrame with one row per hull variation.

    Columns include hull parameters, dimensions, depth classification,
    and peak RAO values for each DOF.
    """
    rows = []
    for entry in results:
        row: dict = {
            "variation_id": entry.variation_id,
            "length_bp": entry.length_bp,
            "beam": entry.beam,
            "draft": entry.draft,
            "block_coefficient": entry.block_coefficient,
            "depth_class": entry.depth_class.value,
            "forward_speed_ms": entry.forward_speed_ms,
        }
        # Add hull params
        for k, v in entry.hull_params.items():
            row[f"param_{k}"] = v

        # Extract peak RAO per DOF
        rao = entry.rao_result
        if rao is not None and rao.rao_amplitude is not None:
            dof_names = rao.dof_names or []
            for i, dof in enumerate(dof_names):
                if i < rao.rao_amplitude.shape[-1]:
                    # Peak across all frequencies and headings
                    peak = float(np.nanmax(rao.rao_amplitude[..., i]))
                    row[f"peak_rao_{dof.lower()}"] = peak

        # Add metadata
        row["n_panels"] = entry.metadata.get("n_panels")
        row["solve_time_s"] = entry.metadata.get("solve_time_s")

        rows.append(row)

    return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# Private helpers
# ---------------------------------------------------------------------------


def _build_wave_conditions(config: SweepConfig) -> object:
    """Build WaveConditions from sweep config."""
    from digitalmodel.hydrodynamics.capytaine import WaveConditions

    return WaveConditions.from_period_range(
        t_min=config.t_min,
        t_max=config.t_max,
        n_periods=config.n_periods,
        headings_deg=config.headings_deg,
        water_depth=config.water_depth,
        rho=config.rho,
    )


def _build_solver_config(config: SweepConfig) -> object:
    """Build SolverConfig from sweep config."""
    from digitalmodel.hydrodynamics.capytaine import SolverConfig

    return SolverConfig(n_jobs=config.n_jobs)


def _profile_to_body_config(profile: object, gdf_path: Path) -> object:
    """Convert a hull_library HullProfile + GDF path into a BodyConfig."""
    from digitalmodel.hydrodynamics.capytaine import (
        BodyConfig, MeshConfig, MeshFormat, DOF,
    )

    mesh_config = MeshConfig(
        path=gdf_path,
        format=MeshFormat.GDF,
        name=profile.name,
    )

    return BodyConfig(
        mesh=mesh_config,
        dofs=DOF.all_dofs(),
        center_of_mass=(profile.length_bp / 2.0, 0.0, -profile.draft / 2.0),
    )


def _extract_hull_params(
    variation_id: str, space: object,
) -> dict[str, float]:
    """Extract hull parameter dict for a variation by iterating combinations.

    Falls back to parsing the variation_id string if iteration is impractical.
    """
    # Parse from variation_id: format is "base__key1=val1_key2=val2"
    params: dict[str, float] = {}
    if "__" in variation_id:
        suffix = variation_id.split("__", 1)[1]
        for part in suffix.split("_"):
            if "=" in part:
                key, val_str = part.split("=", 1)
                try:
                    params[key] = float(val_str)
                except ValueError:
                    pass
    return params


def _to_rao_db_entry(
    entry: SweepResultEntry,
    rao_result: object,
    profile: object,
) -> object:
    """Convert sweep results into an RAODatabaseEntry for storage."""
    from digitalmodel.hydrodynamics.hull_library import RAODatabaseEntry
    from digitalmodel.hydrodynamics.models import RAOData

    # Convert capytaine RAOResult → hull_library RAOData
    rao_data = RAOData(
        frequencies=rao_result.omegas,
        directions=np.rad2deg(rao_result.headings) if rao_result.headings is not None else np.array([0.0]),
        amplitudes=rao_result.rao_amplitude if rao_result.rao_amplitude is not None else np.zeros((1, 1, 6)),
        phases=rao_result.rao_phase if rao_result.rao_phase is not None else np.zeros((1, 1, 6)),
        vessel_name=profile.name,
    )

    return RAODatabaseEntry(
        variation_id=entry.variation_id,
        hull_params=entry.hull_params,
        rao_data=rao_data,
        metadata=entry.metadata,
    )


__all__ = [
    "run_parametric_sweep",
    "sweep_to_dataframe",
]
