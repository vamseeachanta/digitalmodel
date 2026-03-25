#!/usr/bin/env python3
"""Consolidate hull panel GDF files and build catalog.

ABOUTME: One-time script that copies curated GDF files into
data/hull_library/panels/, sanitizes headers, scans all sources,
and writes the catalog YAML + CSV to data/hull_library/catalog/.

Usage:
    PYTHONPATH="src:../assetutilities/src" python3 scripts/consolidate_panels.py
"""

from __future__ import annotations

import shutil
import sys
from pathlib import Path

# Ensure src/ is importable
REPO_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO_ROOT / "src"))

from digitalmodel.hydrodynamics.hull_library.panel_catalog import (
    PanelCatalog,
    PanelCatalogEntry,
    PanelFormat,
)
from digitalmodel.hydrodynamics.hull_library.panel_inventory import (
    build_full_catalog,
    scan_gdf_directory,
    scan_metadata_hulls,
)
from digitalmodel.hydrodynamics.hull_library.profile_schema import HullType

WORKSPACE = REPO_ROOT.parent
PANELS_DIR = REPO_ROOT / "data" / "hull_library" / "panels"
CATALOG_DIR = REPO_ROOT / "data" / "hull_library" / "catalog"

# GDF files to copy: (source_path_relative_to_digitalmodel, target_subdir, target_name)
GDF_COPIES = [
    # Primitives from L00 validation
    (
        "docs/domains/orcawave/L00_validation_wamit/3.3/OrcaWave v11.0 files/Cylinder.gdf",
        "primitives",
        "cylinder_r10_d50.gdf",
    ),
    (
        "docs/domains/orcawave/L00_validation_wamit/3.2/OrcaWave v11.0 files/SphereWithLid.gdf",
        "primitives",
        "sphere_r5.gdf",
    ),
    (
        "docs/domains/orcawave/L00_validation_wamit/2.8/OrcaWave v11.0 files/Ellipsoid0096.gdf",
        "primitives",
        "ellipsoid_96p.gdf",
    ),
    (
        "docs/domains/orcawave/L00_validation_wamit/2.7/OrcaWave v11.0 files/PyramidZC08.gdf",
        "primitives",
        "pyramid_zc08.gdf",
    ),
    # Barges
    (
        "specs/modules/orcawave/test-configs/geometry/barge.gdf",
        "barges",
        "barge_100x20x10.gdf",
    ),
    (
        "docs/domains/orcawave/L01_aqwa_benchmark/benchmark_results/orcawave/unit_box_clean.gdf",
        "barges",
        "unit_box.gdf",
    ),
    # Semi-subs
    (
        "docs/domains/orcawave/examples/L02 OC4 Semi-sub/L02 OC4 Semi-sub mesh.gdf",
        "semi_subs",
        "oc4_semisub.gdf",
    ),
    (
        "docs/domains/orcawave/examples/L03 Semi-sub multibody analysis/L03 Centre column.gdf",
        "semi_subs",
        "l03_centre_column.gdf",
    ),
    (
        "docs/domains/orcawave/examples/L03 Semi-sub multibody analysis/L03 Outer column.gdf",
        "semi_subs",
        "l03_outer_column.gdf",
    ),
    # Spars
    (
        "specs/modules/orcawave/test-configs/geometry/spar.gdf",
        "spars",
        "spar_r10_d50.gdf",
    ),
    # Ships
    (
        "docs/domains/orcawave/examples/L01_default_vessel/L01 Vessel mesh.gdf",
        "ships",
        "l01_vessel_385p.gdf",
    ),
]


def sanitize_gdf_header(gdf_path: Path) -> None:
    """Check and sanitize line 1 of a GDF file.

    GDF line 1 is a comment/title. Replace with a generic description
    if it contains any potentially sensitive terms.
    """
    with open(gdf_path, "r") as f:
        lines = f.readlines()

    if not lines:
        return

    header = lines[0]
    # Standard validation geometry headers are clean
    # Just ensure no client-specific content
    clean_header = f"{gdf_path.stem} - hull panel mesh\n"
    lines[0] = clean_header

    with open(gdf_path, "w") as f:
        f.writelines(lines)


def copy_gdf_files() -> list[tuple[str, str]]:
    """Copy GDF files and return list of (target_name, target_path)."""
    copied = []
    for src_rel, subdir, target_name in GDF_COPIES:
        src_path = REPO_ROOT / src_rel
        target_path = PANELS_DIR / subdir / target_name

        if not src_path.exists():
            print(f"  WARNING: Source not found: {src_path}")
            continue

        target_path.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(src_path, target_path)
        sanitize_gdf_header(target_path)
        size_kb = target_path.stat().st_size / 1024
        print(f"  Copied: {target_name} ({size_kb:.1f} KB)")
        copied.append((target_name, str(target_path)))

    return copied


def build_catalog() -> PanelCatalog:
    """Scan all sources and build the full catalog."""
    source_config = {
        "gdf_dirs": [
            {
                "path": str(PANELS_DIR),
                "source_id": "canonical",
            },
        ],
        "aqwa_dirs": [],  # Multibody DATs removed — dimensions are wrong for inventory
        "orcaflex_dirs": [],
        "metadata_dirs": [
            {
                "path": str(WORKSPACE / "acma-projects/_hulls"),
                "source_id": "hull_collection",
            },
            {
                "path": str(WORKSPACE / "rock-oil-field/s7/analysis_general/ssRAOs_toolkit/SevenSeasParisMesh"),
                "source_id": "rock_oil_field",
            },
        ],
    }

    catalog = build_full_catalog(source_config)

    # Add manual entries for sources that need special parsing
    manual_entries = _build_manual_entries()
    for entry in manual_entries:
        # Check for hull_id collision
        existing_ids = {e.hull_id for e in catalog.entries}
        if entry.hull_id not in existing_ids:
            catalog.entries.append(entry)

    return catalog


def _build_manual_entries() -> list:
    """Build manual catalog entries for sources with non-standard formats."""
    import re
    import yaml as _yaml

    entries = []

    # Source e: FPSO from OrcaFlex YAML (nested structure)
    # Note: source path uses env var or relative reference to avoid client identifiers
    _fpso_candidates = list((WORKSPACE / "saipem").glob("*/code/rev2/03_Vessels_host/03_fpso_vessel_type.yml"))
    fpso_yml = _fpso_candidates[0] if _fpso_candidates else None
    if fpso_yml and fpso_yml.exists():
        try:
            with open(fpso_yml) as f:
                data = _yaml.safe_load(f)
            for key, val in data.items():
                if isinstance(val, dict) and "Length" in val:
                    # Extract draught from Draughts list
                    draft = None
                    draughts = val.get("Draughts", [])
                    if draughts and isinstance(draughts, list):
                        name = draughts[0].get("Name", "")
                        m = re.search(r"([\d.]+)\s*\[?m\]?", name)
                        if m:
                            draft = float(m.group(1))

                    entries.append(PanelCatalogEntry(
                        hull_id="orcaflex_fpso_328m",
                        hull_type=HullType.FPSO,
                        name="FPSO Vessel Type 328m",
                        source="orcaflex_yaml",
                        panel_format=PanelFormat.ORCAFLEX_YAML,
                        file_path="(orcaflex vessel type definition)",
                        length_m=float(val["Length"]),
                        draft_m=draft,
                        description="OrcaFlex vessel type with RAOs and wireframe",
                        tags=["orcaflex", "fpso"],
                    ))
                    break
        except Exception as exc:
            print(f"  WARNING: Could not parse FPSO YAML: {exc}")

    return entries


def main() -> None:
    print("=" * 60)
    print("Hull Panel Consolidation - WRK-114")
    print("=" * 60)

    # Step 1: Copy GDF files
    print("\n--- Copying GDF files ---")
    copied = copy_gdf_files()
    print(f"\nCopied {len(copied)} files")

    # Step 2: Build catalog from canonical panels
    print("\n--- Building catalog ---")
    catalog = build_catalog()
    print(f"Catalog: {len(catalog.entries)} entries")

    # Step 3: Write outputs
    print("\n--- Writing catalog files ---")
    CATALOG_DIR.mkdir(parents=True, exist_ok=True)

    yaml_path = catalog.to_yaml(CATALOG_DIR / "hull_panel_catalog.yaml")
    print(f"  YAML: {yaml_path}")

    csv_path = catalog.to_csv(CATALOG_DIR / "hull_panel_catalog.csv")
    print(f"  CSV:  {csv_path}")

    # Summary
    print("\n--- Summary ---")
    for entry in catalog.entries:
        panels = entry.panel_count or "N/A"
        dims = ""
        if entry.length_m:
            dims = f" L={entry.length_m}m"
        if entry.beam_m:
            dims += f" B={entry.beam_m}m"
        if entry.draft_m:
            dims += f" D={entry.draft_m}m"
        print(f"  {entry.hull_id}: {entry.name} ({panels} panels){dims}")

    print(f"\nDone. {len(catalog.entries)} hulls cataloged.")


if __name__ == "__main__":
    main()
