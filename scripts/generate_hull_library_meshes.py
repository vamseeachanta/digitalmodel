# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Generate GDF panel meshes from hull profile YAMLs in the hull library.

Loads each YAML profile, creates a HullProfile, generates a panel mesh via
HullMeshGenerator with target_panels=2500, and exports as GDF to the
appropriate subdirectory under data/hull_library/panels/.

Usage:
    PYTHONPATH="src" python3 scripts/generate_hull_library_meshes.py
"""

from __future__ import annotations

from pathlib import Path

from digitalmodel.hydrodynamics.hull_library.profile_schema import HullProfile
from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
    HullMeshGenerator,
    MeshGeneratorConfig,
)
from digitalmodel.hydrodynamics.bemrosetta.mesh.gdf_handler import GDFHandler


# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

PROJECT_ROOT = Path(__file__).resolve().parent.parent
PROFILES_DIR = PROJECT_ROOT / "data" / "hull_library" / "profiles"
PANELS_DIR = PROJECT_ROOT / "data" / "hull_library" / "panels"

# Map each profile filename to its output subdirectory
PROFILE_OUTPUT_MAP: dict[str, str] = {
    "fst_barge_250m.yaml": "fpso",
    "fst_ship_330m.yaml": "fpso",
    "lngc_qflex_315m.yaml": "lngc",
    "lngc_qmax_345m.yaml": "lngc",
}

TARGET_PANELS = 2500


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------


def main() -> None:
    config = MeshGeneratorConfig(target_panels=TARGET_PANELS)
    generator = HullMeshGenerator()
    gdf_handler = GDFHandler()

    print(f"Mesh generation config: target_panels={config.target_panels}, "
          f"symmetry={config.symmetry}, "
          f"waterline_refinement={config.waterline_refinement}")
    print(f"Profiles directory: {PROFILES_DIR}")
    print("-" * 72)

    for yaml_name, subdir in PROFILE_OUTPUT_MAP.items():
        yaml_path = PROFILES_DIR / yaml_name
        if not yaml_path.exists():
            print(f"SKIP: {yaml_path} not found")
            continue

        # Load profile from YAML
        profile = HullProfile.load_yaml(yaml_path)

        # Generate panel mesh
        mesh = generator.generate(profile, config)

        # Determine output path
        output_dir = PANELS_DIR / subdir
        output_dir.mkdir(parents=True, exist_ok=True)
        gdf_name = yaml_path.stem + ".gdf"
        gdf_path = output_dir / gdf_name

        # Write GDF
        gdf_handler.write(mesh, gdf_path)

        # Compute bounding box extents
        bb_min, bb_max = mesh.bounding_box
        extent = bb_max - bb_min

        print(f"Hull: {profile.name}")
        print(f"  Type:       {profile.hull_type.value}")
        print(f"  Panels:     {mesh.n_panels}")
        print(f"  Vertices:   {mesh.n_vertices}")
        print(f"  BBox extent (L x B x D): "
              f"{extent[0]:.1f} x {extent[1]:.1f} x {extent[2]:.1f} m")
        print(f"  Total area: {mesh.total_area:.1f} m2")
        print(f"  Output:     {gdf_path}")
        print("-" * 72)

    print("Done.")


if __name__ == "__main__":
    main()
