"""
ABOUTME: Hull analysis setup skill — chains hull selection, mesh scaling, refinement, and RAO linking.
ABOUTME: Single invocation from target dimensions to (scaled_mesh, rao_data) ready for diffraction analysis.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

from .lookup import HullLookup, HullLookupTarget, HullMatch

logger = logging.getLogger(__name__)

SKILL_NAME = "hull_analysis_setup"


# ---------------------------------------------------------------------------
# Input / Output data classes
# ---------------------------------------------------------------------------


@dataclass
class HullAnalysisInput:
    """Input dimensions and options for hull analysis setup.

    Args:
        loa_m: Length overall in metres (required, must be positive).
        beam_m: Breadth/beam in metres (required, must be positive).
        draft_m: Design draft in metres (required, must be positive).
        displacement_t: Displacement in tonnes (optional).
        mesh_refinement_levels: Number of subdivision levels to apply after
            scaling (0 = no refinement, 1 = one subdivision giving 4x panels).
        include_rao: When True, attempt to fetch RAO data from the registry;
            falls back gracefully if no data exists.
    """

    loa_m: float
    beam_m: float
    draft_m: float
    displacement_t: Optional[float] = None
    mesh_refinement_levels: int = 1
    include_rao: bool = True


@dataclass
class HullAnalysisResult:
    """Full result of the hull analysis setup chain.

    Attributes:
        hull_id: Identifier of the selected hull form.
        similarity_score: Normalised similarity in [0, 1]; 1.0 = exact match.
        scaling_factors: Per-axis scale ratios {"loa": float, "beam": float,
            "draft": float} to resize the reference hull to the target.
        mesh_quality: Quality metrics dict (panel count, aspect ratios, etc.)
            if a PanelMesh was available and processed; None otherwise.
        rao_available: True if at least one RAO dataset was found.
        rao_data: RAO data dict if available, else None.
        summary: Human-readable dict with at least a "hull_id" key plus
            key dimensions and quality highlights.
        source: Constant skill identifier string.
    """

    hull_id: str
    similarity_score: float
    scaling_factors: dict
    mesh_quality: Optional[dict]
    rao_available: bool
    rao_data: Optional[dict]
    summary: dict
    source: str = "skill:hull_analysis_setup"


# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------


def _validate_input(inp: HullAnalysisInput) -> None:
    """Raise ValueError for invalid HullAnalysisInput fields."""
    if inp.loa_m is None or inp.loa_m <= 0:
        raise ValueError(f"loa_m must be a positive number, got {inp.loa_m!r}")
    if inp.beam_m is None or inp.beam_m <= 0:
        raise ValueError(f"beam_m must be a positive number, got {inp.beam_m!r}")
    if inp.draft_m is None or inp.draft_m <= 0:
        raise ValueError(f"draft_m must be a positive number, got {inp.draft_m!r}")


# ---------------------------------------------------------------------------
# Mesh helpers (optional — graceful fallback when PanelMesh unavailable)
# ---------------------------------------------------------------------------


def _attempt_mesh_pipeline(
    match: HullMatch,
    inp: HullAnalysisInput,
) -> Optional[dict]:
    """Try to scale and optionally refine a PanelMesh for the matched hull.

    Returns a quality metrics dict on success, None on any failure (missing
    mesh, catalog-only entry, import errors).  All exceptions are caught so
    the skill degrades gracefully rather than aborting.
    """
    # Only PanelCatalogEntry objects carry an actual mesh file reference;
    # builtin dict entries and HullCatalogEntry objects don't.
    try:
        from .panel_catalog import PanelCatalogEntry
    except ImportError:
        logger.warning("panel_catalog not importable — skipping mesh pipeline")
        return None

    entry = match.matched_entry
    if not isinstance(entry, PanelCatalogEntry):
        logger.debug(
            "Hull %s: matched_entry is %s (no PanelMesh) — skipping mesh pipeline",
            match.hull_id,
            type(entry).__name__,
        )
        return None

    try:
        from .mesh_scaler import ScaleDimensions, scale_mesh_to_target
        from .mesh_refiner import compute_quality_metrics, refine_mesh
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )
        from .catalog import HullCatalog
    except ImportError as exc:
        logger.warning("Mesh pipeline import failed (%s) — skipping", exc)
        return None

    # PanelCatalogEntry carries dimensions but no in-memory mesh directly.
    # We need a HullProfile to generate one; if we can't get one, skip.
    try:
        catalog = HullCatalog()
        hull_entry = catalog.get_hull(match.hull_id)
        profile = hull_entry.profile
    except Exception as exc:  # noqa: BLE001
        logger.debug(
            "Hull %s: could not retrieve profile from HullCatalog (%s) — skipping",
            match.hull_id,
            exc,
        )
        return None

    try:
        generator = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=200)
        source_mesh = generator.generate(profile, config)
    except Exception as exc:  # noqa: BLE001
        logger.warning(
            "Hull %s: mesh generation failed (%s) — skipping", match.hull_id, exc
        )
        return None

    try:
        source_dims = ScaleDimensions(
            length_m=profile.length_bp,
            beam_m=profile.beam,
            draft_m=profile.draft,
        )
        target_dims = ScaleDimensions(
            length_m=inp.loa_m,
            beam_m=inp.beam_m,
            draft_m=inp.draft_m,
        )
        scale_result = scale_mesh_to_target(source_mesh, source_dims, target_dims)
        mesh = scale_result.mesh
    except Exception as exc:  # noqa: BLE001
        logger.warning(
            "Hull %s: mesh scaling failed (%s) — skipping", match.hull_id, exc
        )
        return None

    if inp.mesh_refinement_levels > 0:
        try:
            mesh = refine_mesh(mesh, levels=inp.mesh_refinement_levels)
        except Exception as exc:  # noqa: BLE001
            logger.warning(
                "Hull %s: mesh refinement failed (%s) — using unrefined mesh",
                match.hull_id,
                exc,
            )

    try:
        metrics = compute_quality_metrics(mesh)
        return {
            "panel_count": metrics.panel_count,
            "vertex_count": metrics.vertex_count,
            "min_area": metrics.min_area,
            "max_area": metrics.max_area,
            "mean_area": metrics.mean_area,
            "total_area": metrics.total_area,
            "min_aspect_ratio": metrics.min_aspect_ratio,
            "max_aspect_ratio": metrics.max_aspect_ratio,
            "mean_aspect_ratio": metrics.mean_aspect_ratio,
            "degenerate_count": metrics.degenerate_count,
        }
    except Exception as exc:  # noqa: BLE001
        logger.warning(
            "Hull %s: quality metrics failed (%s) — returning None", match.hull_id, exc
        )
        return None


# ---------------------------------------------------------------------------
# RAO helpers (optional — graceful fallback when registry unavailable)
# ---------------------------------------------------------------------------


def _attempt_rao_lookup(
    hull_id: str,
    draft_m: float,
    raos_dir: Optional[Path] = None,
) -> tuple[bool, Optional[dict]]:
    """Try to fetch RAO data for the hull.

    Returns ``(rao_available, rao_data)``.  On any failure or missing data,
    returns ``(False, None)`` without raising.
    """
    try:
        from .rao_registry import RaoRegistry
    except ImportError as exc:
        logger.warning("rao_registry not importable (%s) — RAO lookup skipped", exc)
        return False, None

    if raos_dir is None:
        logger.debug("No raos_dir provided — RAO lookup skipped for hull %s", hull_id)
        return False, None

    if not raos_dir.exists():
        logger.debug(
            "raos_dir %s does not exist — RAO lookup skipped for hull %s",
            raos_dir,
            hull_id,
        )
        return False, None

    try:
        registry = RaoRegistry(raos_dir)
        refs = registry.get_raos(hull_id, draft_m=draft_m)
        if not refs:
            logger.debug("No RAO data found for hull %s at draft %.2fm", hull_id, draft_m)
            return False, None

        # Load the first matching dataset
        data = registry.load_rao_data(refs[0])
        if data is None:
            logger.warning(
                "RAO reference found for %s but file is missing — returning no data",
                hull_id,
            )
            return False, None

        return True, data
    except Exception as exc:  # noqa: BLE001
        logger.warning("RAO lookup failed for hull %s: %s", hull_id, exc)
        return False, None


# ---------------------------------------------------------------------------
# Main entry point
# ---------------------------------------------------------------------------


def setup_hull_analysis(
    inp: HullAnalysisInput,
    raos_dir: Optional[Path] = None,
) -> HullAnalysisResult:
    """Chain hull selection, mesh scaling, optional refinement, and RAO linking.

    Steps
    -----
    1. Validate input dimensions (raises ValueError on invalid input).
    2. ``HullLookup.get_hull_form()`` → nearest-neighbour ``HullMatch``.
    3. If a ``PanelCatalogEntry`` is in ``matched_entry`` and a ``HullProfile``
       is accessible: scale the generated mesh to target dims with
       ``scale_mesh_to_target()``, then optionally refine with
       ``refine_mesh()``.  If the mesh pipeline fails at any step, proceed
       with ``mesh_quality=None`` (no exception raised).
    4. If ``include_rao=True`` and ``raos_dir`` is provided: query
       ``RaoRegistry`` for matching RAO data.  Fallback to
       ``rao_available=False, rao_data=None`` on any failure or absence.
    5. Return populated ``HullAnalysisResult``.

    Args:
        inp: Target dimensions and processing options.
        raos_dir: Optional path to a RAO registry directory.  When *None*,
            the RAO step is skipped entirely.

    Returns:
        ``HullAnalysisResult`` with all fields populated.

    Raises:
        ValueError: If required dimensions are missing or non-positive.
    """
    _validate_input(inp)

    # Step 1: Nearest-neighbour hull lookup
    target = HullLookupTarget(
        loa_m=inp.loa_m,
        beam_m=inp.beam_m,
        draft_m=inp.draft_m,
        displacement_t=inp.displacement_t,
    )
    lookup = HullLookup()
    match: HullMatch = lookup.get_hull_form(target)

    logger.info(
        "Hull lookup: selected %s (score=%.4f) for target L=%.1f B=%.1f T=%.1f",
        match.hull_id,
        match.similarity_score,
        inp.loa_m,
        inp.beam_m,
        inp.draft_m,
    )

    # Step 2: Optional mesh pipeline
    if inp.mesh_refinement_levels > 0:
        mesh_quality = _attempt_mesh_pipeline(match, inp)
    else:
        # No refinement requested — still attempt scaling but skip refine
        _no_refine_inp = HullAnalysisInput(
            loa_m=inp.loa_m,
            beam_m=inp.beam_m,
            draft_m=inp.draft_m,
            displacement_t=inp.displacement_t,
            mesh_refinement_levels=0,
            include_rao=inp.include_rao,
        )
        mesh_quality = _attempt_mesh_pipeline(match, _no_refine_inp)

    # Step 3: Optional RAO lookup
    rao_available = False
    rao_data: Optional[dict] = None
    if inp.include_rao:
        rao_available, rao_data = _attempt_rao_lookup(
            match.hull_id, inp.draft_m, raos_dir
        )
    else:
        logger.debug("include_rao=False — RAO lookup skipped")

    # Step 4: Build summary
    summary: dict = {
        "hull_id": match.hull_id,
        "similarity_score": round(match.similarity_score, 4),
        "target_loa_m": inp.loa_m,
        "target_beam_m": inp.beam_m,
        "target_draft_m": inp.draft_m,
        "scaling_factors": match.scaling_factors,
        "mesh_available": mesh_quality is not None,
        "rao_available": rao_available,
    }
    if inp.displacement_t is not None:
        summary["displacement_t"] = inp.displacement_t
    if mesh_quality is not None:
        summary["panel_count"] = mesh_quality.get("panel_count")
        summary["max_aspect_ratio"] = mesh_quality.get("max_aspect_ratio")

    return HullAnalysisResult(
        hull_id=match.hull_id,
        similarity_score=match.similarity_score,
        scaling_factors=match.scaling_factors,
        mesh_quality=mesh_quality,
        rao_available=rao_available,
        rao_data=rao_data,
        summary=summary,
    )


__all__ = [
    "SKILL_NAME",
    "HullAnalysisInput",
    "HullAnalysisResult",
    "setup_hull_analysis",
]
