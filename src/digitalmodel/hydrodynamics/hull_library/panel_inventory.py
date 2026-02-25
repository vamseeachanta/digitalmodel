"""Hull panel inventory scanners.

ABOUTME: Scans directories for hull panel mesh files (GDF, AQWA DAT, OrcaFlex
YAML, metadata-only), extracts geometry metadata, and builds a consolidated
PanelCatalog. Each scanner handles one format family and returns catalog entries.
"""

from __future__ import annotations

import logging
import re
from pathlib import Path
from typing import Optional

import yaml

from .panel_catalog import PanelCatalog, PanelCatalogEntry, PanelFormat
from .profile_schema import HullType

logger = logging.getLogger(__name__)

# Mapping from filename keywords to hull types
_HULL_TYPE_HINTS: dict[str, HullType] = {
    "barge": HullType.BARGE,
    "box": HullType.BARGE,
    "tanker": HullType.TANKER,
    "aframax": HullType.TANKER,
    "ship": HullType.SHIP,
    "vessel": HullType.SHIP,
    "spar": HullType.SPAR,
    "cylinder": HullType.CYLINDER,
    "sphere": HullType.SPHERE,
    "ellipsoid": HullType.ELLIPSOID,
    "fpso": HullType.FPSO,
    "lngc": HullType.LNGC,
    "semi": HullType.SEMI_PONTOON,
    "semisub": HullType.SEMI_PONTOON,
    "column": HullType.SEMI_PONTOON,
    "pontoon": HullType.SEMI_PONTOON,
    "keystone": HullType.SEMI_PONTOON,
    "pyramid": HullType.CUSTOM,
    "fst": HullType.FPSO,
}

# File extensions recognized as hull-related metadata
_METADATA_EXTENSIONS = {".3dm", ".xlsx", ".xls", ".pdf", ".doc", ".docx", ".rao", ".hst"}


def _infer_hull_type(name: str) -> HullType:
    """Infer hull type from filename or name string."""
    lower = name.lower()
    for keyword, hull_type in _HULL_TYPE_HINTS.items():
        if keyword in lower:
            return hull_type
    return HullType.CUSTOM


def _make_hull_id(source_id: str, filename: str) -> str:
    """Generate a unique hull_id from source and filename."""
    stem = Path(filename).stem
    clean = re.sub(r"[^a-z0-9_]", "_", stem.lower())
    clean = re.sub(r"_+", "_", clean).strip("_")
    return f"{source_id}_{clean}"


def scan_gdf_directory(
    base_dir: str | Path, source_id: str
) -> list[PanelCatalogEntry]:
    """Scan a directory for GDF files and extract panel metadata.

    Uses the GDFHandler parser to get panel count, vertex count, and
    bounding box dimensions for each file.

    Args:
        base_dir: Directory to scan (recursively).
        source_id: Source identifier for catalog entries.

    Returns:
        List of PanelCatalogEntry for each parseable GDF file.
    """
    base_dir = Path(base_dir)
    if not base_dir.is_dir():
        logger.warning("GDF scan directory does not exist: %s", base_dir)
        return []

    entries: list[PanelCatalogEntry] = []
    gdf_files = sorted(base_dir.rglob("*.gdf"))

    for gdf_path in gdf_files:
        try:
            entry = _parse_gdf_to_entry(gdf_path, source_id)
            if entry is not None:
                entries.append(entry)
        except Exception as exc:
            logger.warning("Failed to parse GDF %s: %s", gdf_path, exc)

    return entries


def _parse_gdf_to_entry(
    gdf_path: Path, source_id: str
) -> Optional[PanelCatalogEntry]:
    """Parse a single GDF file into a catalog entry."""
    from digitalmodel.hydrodynamics.bemrosetta.mesh.gdf_handler import GDFHandler

    handler = GDFHandler()
    mesh = handler.read(gdf_path)

    # Extract bounding box for dimensions
    length_m = beam_m = draft_m = None
    if mesh.vertices is not None and len(mesh.vertices) > 0:
        bbox = mesh.bounding_box
        if bbox is not None:
            mins, maxs = bbox
            length_m = float(maxs[0] - mins[0])
            beam_m = float(maxs[1] - mins[1])
            draft_m = float(abs(maxs[2] - mins[2]))

    symmetry = mesh.symmetry_plane if mesh.symmetry_plane else None

    # Use relative path if possible
    try:
        rel_path = str(gdf_path.relative_to(Path.cwd()))
    except ValueError:
        rel_path = str(gdf_path)

    return PanelCatalogEntry(
        hull_id=_make_hull_id(source_id, gdf_path.name),
        hull_type=_infer_hull_type(gdf_path.stem),
        name=gdf_path.stem,
        source=source_id,
        panel_format=PanelFormat.GDF,
        file_path=rel_path,
        panel_count=int(mesh.n_panels),
        vertex_count=int(mesh.n_vertices),
        symmetry=symmetry,
        length_m=round(length_m, 3) if length_m is not None else None,
        beam_m=round(beam_m, 3) if beam_m is not None else None,
        draft_m=round(draft_m, 3) if draft_m is not None else None,
        tags=["gdf", source_id],
    )


def scan_aqwa_dat_directory(
    base_dir: str | Path, source_id: str
) -> list[PanelCatalogEntry]:
    """Scan a directory for AQWA DAT/DECK files and extract panel metadata.

    Args:
        base_dir: Directory to scan (recursively).
        source_id: Source identifier for catalog entries.

    Returns:
        List of PanelCatalogEntry for each parseable file.
    """
    base_dir = Path(base_dir)
    if not base_dir.is_dir():
        logger.warning("AQWA scan directory does not exist: %s", base_dir)
        return []

    entries: list[PanelCatalogEntry] = []

    for ext in ("*.dat", "*.deck"):
        for dat_path in sorted(base_dir.rglob(ext)):
            try:
                entry = _parse_aqwa_to_entry(dat_path, source_id)
                if entry is not None:
                    entries.append(entry)
            except Exception as exc:
                logger.warning("Failed to parse AQWA %s: %s", dat_path, exc)

    return entries


def _parse_aqwa_to_entry(
    dat_path: Path, source_id: str
) -> Optional[PanelCatalogEntry]:
    """Parse a single AQWA DAT/DECK file into a catalog entry."""
    from digitalmodel.hydrodynamics.bemrosetta.mesh.dat_handler import DATHandler

    handler = DATHandler()
    mesh = handler.read(dat_path)

    length_m = beam_m = draft_m = None
    if mesh.vertices is not None and len(mesh.vertices) > 0:
        bbox = mesh.bounding_box
        if bbox is not None:
            mins, maxs = bbox
            length_m = float(maxs[0] - mins[0])
            beam_m = float(maxs[1] - mins[1])
            draft_m = float(abs(maxs[2] - mins[2]))

    return PanelCatalogEntry(
        hull_id=_make_hull_id(source_id, dat_path.name),
        hull_type=_infer_hull_type(dat_path.stem),
        name=dat_path.stem,
        source=source_id,
        panel_format=PanelFormat.AQWA_DAT,
        file_path=dat_path.name,
        panel_count=int(mesh.n_panels) if mesh.n_panels > 0 else None,
        vertex_count=int(mesh.n_vertices) if mesh.n_vertices > 0 else None,
        length_m=round(length_m, 3) if length_m is not None else None,
        beam_m=round(beam_m, 3) if beam_m is not None else None,
        draft_m=round(draft_m, 3) if draft_m is not None else None,
        tags=["aqwa", source_id],
    )


def scan_orcaflex_vessels(
    base_dir: str | Path, source_id: str
) -> list[PanelCatalogEntry]:
    """Scan a directory for OrcaFlex vessel YAML files.

    Extracts vessel dimensions (Length, Breadth/Beam, Draught/Draft) from
    OrcaFlex YAML model files.

    Args:
        base_dir: Directory to scan (recursively).
        source_id: Source identifier for catalog entries.

    Returns:
        List of PanelCatalogEntry for each vessel found.
    """
    base_dir = Path(base_dir)
    if not base_dir.is_dir():
        logger.warning("OrcaFlex scan directory does not exist: %s", base_dir)
        return []

    entries: list[PanelCatalogEntry] = []

    for yml_path in sorted(base_dir.rglob("*.yml")):
        try:
            parsed = _parse_orcaflex_yaml(yml_path, source_id)
            if parsed:
                entries.extend(parsed)
        except Exception as exc:
            logger.warning("Failed to parse OrcaFlex YAML %s: %s", yml_path, exc)

    for yaml_path in sorted(base_dir.rglob("*.yaml")):
        try:
            parsed = _parse_orcaflex_yaml(yaml_path, source_id)
            if parsed:
                entries.extend(parsed)
        except Exception as exc:
            logger.warning("Failed to parse OrcaFlex YAML %s: %s", yaml_path, exc)

    return entries


def _parse_orcaflex_yaml(
    yaml_path: Path, source_id: str
) -> list[PanelCatalogEntry]:
    """Parse an OrcaFlex YAML file for vessel type definitions."""
    with open(yaml_path) as f:
        data = yaml.safe_load(f)

    if not isinstance(data, dict):
        return []

    entries: list[PanelCatalogEntry] = []

    # Check if this is a vessel type definition
    obj_type = data.get("ObjectType", "")
    if "vessel" in str(obj_type).lower():
        name = data.get("Name", yaml_path.stem)
        length_m = _try_float(data.get("Length"))
        beam_m = _try_float(data.get("Breadth")) or _try_float(data.get("Beam"))
        draft_m = _try_float(data.get("Draught")) or _try_float(data.get("Draft"))

        entries.append(
            PanelCatalogEntry(
                hull_id=_make_hull_id(source_id, yaml_path.stem),
                hull_type=_infer_hull_type(name),
                name=name,
                source=source_id,
                panel_format=PanelFormat.ORCAFLEX_YAML,
                file_path=yaml_path.name,
                length_m=length_m,
                beam_m=beam_m,
                draft_m=draft_m,
                description="OrcaFlex vessel type definition",
                tags=["orcaflex", source_id],
            )
        )

    return entries


def _try_float(val) -> Optional[float]:
    """Try to convert a value to float, returning None on failure."""
    if val is None:
        return None
    try:
        return float(val)
    except (ValueError, TypeError):
        return None


def scan_metadata_hulls(
    base_dir: str | Path, source_id: str
) -> list[PanelCatalogEntry]:
    """Catalog non-parseable hull files by filename metadata.

    For formats like .3dm (Rhino), .xlsx, .pdf that can't be parsed for
    panel geometry, creates catalog entries with available metadata from
    filenames.

    Args:
        base_dir: Directory to scan (recursively).
        source_id: Source identifier for catalog entries.

    Returns:
        List of PanelCatalogEntry with panel_count=None.
    """
    base_dir = Path(base_dir)
    if not base_dir.is_dir():
        logger.warning("Metadata scan directory does not exist: %s", base_dir)
        return []

    entries: list[PanelCatalogEntry] = []

    for f in sorted(base_dir.rglob("*")):
        if not f.is_file():
            continue
        if f.suffix.lower() not in _METADATA_EXTENSIONS:
            continue

        fmt = _extension_to_format(f.suffix.lower())
        entries.append(
            PanelCatalogEntry(
                hull_id=_make_hull_id(source_id, f.name),
                hull_type=_infer_hull_type(f.stem),
                name=f.stem,
                source=source_id,
                panel_format=fmt,
                file_path=f.name,
                description=f"Metadata-only entry ({f.suffix})",
                tags=["metadata", source_id],
            )
        )

    return entries


def _extension_to_format(ext: str) -> PanelFormat:
    """Map file extension to PanelFormat."""
    mapping = {
        ".3dm": PanelFormat.RHINO_3DM,
        ".obj": PanelFormat.OBJ,
        ".rao": PanelFormat.YAML_PROFILE,
        ".hst": PanelFormat.YAML_PROFILE,
    }
    return mapping.get(ext, PanelFormat.YAML_PROFILE)


def build_full_catalog(source_config: dict) -> PanelCatalog:
    """Build a complete hull panel catalog from multiple sources.

    Args:
        source_config: Dict with keys for each scanner type, each mapping to
            a list of {path, source_id} dicts:
            {
                "gdf_dirs": [{"path": "/path", "source_id": "src_a"}],
                "aqwa_dirs": [{"path": "/path", "source_id": "src_d"}],
                "orcaflex_dirs": [{"path": "/path", "source_id": "src_e"}],
                "metadata_dirs": [{"path": "/path", "source_id": "src_b"}],
            }

    Returns:
        PanelCatalog with entries from all sources.
    """
    all_entries: list[PanelCatalogEntry] = []

    scanner_map = {
        "gdf_dirs": scan_gdf_directory,
        "aqwa_dirs": scan_aqwa_dat_directory,
        "orcaflex_dirs": scan_orcaflex_vessels,
        "metadata_dirs": scan_metadata_hulls,
    }

    for key, scanner_fn in scanner_map.items():
        for item in source_config.get(key, []):
            path = item.get("path", "")
            source_id = item.get("source_id", key)
            entries = scanner_fn(path, source_id)
            all_entries.extend(entries)
            logger.info(
                "Source %s (%s): %d entries", source_id, key, len(entries)
            )

    # Deduplicate hull_ids by appending suffix if needed
    seen: dict[str, int] = {}
    for entry in all_entries:
        if entry.hull_id in seen:
            seen[entry.hull_id] += 1
            entry.hull_id = f"{entry.hull_id}_{seen[entry.hull_id]}"
        else:
            seen[entry.hull_id] = 0

    return PanelCatalog(entries=all_entries)


__all__ = [
    "scan_gdf_directory",
    "scan_aqwa_dat_directory",
    "scan_orcaflex_vessels",
    "scan_metadata_hulls",
    "build_full_catalog",
]
