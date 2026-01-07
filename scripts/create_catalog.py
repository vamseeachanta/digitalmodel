#!/usr/bin/env python
"""
ABOUTME: Script to create/update the master data catalog
Discovers data files and creates catalog.yml with metadata
"""

from pathlib import Path
from digitalmodel.data.catalog import create_master_catalog, DataCatalog, CatalogEntry

def main():
    """Create master catalog for digitalmodel project"""

    # Project root
    project_root = Path(__file__).parent.parent
    data_dir = project_root / "data"

    print("Creating master data catalog...")
    print(f"Data directory: {data_dir}")

    # Create catalog with auto-discovery
    catalog = create_master_catalog(
        data_dir=data_dir,
        output_file=data_dir / "catalog.yml",
        discover=True
    )

    # Manually add schema references for known datasets
    known_schemas = {
        "204_subsea_shackles_g2100": "schemas/shackles.schema.json",
        "204_subsea_shackles_g2110": "schemas/shackles.schema.json",
        "26_shackles_bolt_type_anchor_g2130": "schemas/shackles.schema.json",
        "134_masterlinks_oblong_a342": "schemas/masterlinks.schema.json",
    }

    for dataset_name, schema_file in known_schemas.items():
        if dataset_name in catalog.datasets:
            entry = catalog.datasets[dataset_name]
            entry.schema = schema_file

            # Add tags based on name
            if "shackle" in dataset_name.lower():
                entry.tags = ["rigging", "shackles", "crosby", "subsea"]
                entry.source = "Crosby Catalog"
                entry.engineering_standard = "API 2C"
            elif "masterlink" in dataset_name.lower():
                entry.tags = ["rigging", "masterlinks", "crosby"]
                entry.source = "Crosby Catalog"
                entry.engineering_standard = "API 2C"

            # Update description
            entry.description = f"Crosby {dataset_name.replace('_', ' ').title()}"

    # Save catalog
    catalog.save()

    print(f"\n[OK] Catalog created: {catalog.catalog_file}")
    print(f"  Total datasets: {len(catalog.datasets)}")
    print(f"\nDatasets:")
    for name in sorted(catalog.datasets.keys()):
        entry = catalog.datasets[name]
        print(f"  - {name:40} ({entry.format:8}) {entry.version}")

    print(f"\n[OK] Master catalog saved to: {catalog.catalog_file}")


if __name__ == "__main__":
    main()
