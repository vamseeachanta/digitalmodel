#!/usr/bin/env python
"""
ABOUTME: Example usage of the data catalog system
Demonstrates loading, searching, and validating datasets
"""

from pathlib import Path
from digitalmodel.data import DataCatalog

def main():
    """Demonstrate catalog usage"""

    project_root = Path(__file__).parent.parent
    catalog_file = project_root / "data" / "catalog.yml"

    print("=" * 70)
    print("Data Catalog System - Example Usage")
    print("=" * 70)

    # Load catalog
    print(f"\n1. Loading catalog from: {catalog_file}")
    catalog = DataCatalog.load_catalog(
        catalog_file,
        base_path=project_root,
        schema_dir=project_root / "data" / "schemas"
    )

    print(f"   Loaded {len(catalog.datasets)} datasets")

    # List all datasets
    print("\n2. All datasets in catalog:")
    for name in sorted(catalog.list_datasets()):
        entry = catalog.datasets[name]
        print(f"   - {name:35} ({entry.format:8}) v{entry.version}")

    # Search by format
    print("\n3. Searching for Excel files:")
    excel_files = catalog.search(format="excel")
    print(f"   Found {len(excel_files)} Excel datasets:")
    for entry in excel_files:
        print(f"   - {entry.name}")

    # Search by tags
    print("\n4. Searching for datasets with tags:")
    if any(entry.tags for entry in catalog.datasets.values()):
        tagged = catalog.search(tags=["rigging"])
        if tagged:
            print(f"   Found {len(tagged)} datasets with 'rigging' tag:")
            for entry in tagged:
                print(f"   - {entry.name}: {entry.tags}")
    else:
        print("   No tagged datasets found (run create_catalog.py to add tags)")

    # Get specific entry details
    print("\n5. Dataset details:")
    if "agent_metrics" in catalog.datasets:
        entry = catalog.datasets["agent_metrics"]
        print(f"   Name: {entry.name}")
        print(f"   File: {entry.file}")
        print(f"   Format: {entry.format}")
        print(f"   Version: {entry.version}")
        print(f"   Hash: {entry.hash[:16]}...")
        print(f"   Created: {entry.created_at}")

        # Load the data
        print("\n6. Loading data:")
        try:
            data = catalog.load("agent_metrics")
            print(f"   Loaded successfully!")
            if hasattr(data, 'shape'):
                print(f"   Shape: {data.shape}")
                print(f"   Columns: {list(data.columns)}")
        except Exception as e:
            print(f"   Error loading: {e}")

    # Version checking
    print("\n7. Version checking:")
    if "agent_metrics" in catalog.datasets:
        version = catalog.check_version("agent_metrics", auto_bump=False)
        print(f"   Current version: {version}")
        print(f"   File hash: {catalog.datasets['agent_metrics'].hash[:16]}...")

    print("\n" + "=" * 70)
    print("Example complete!")
    print("=" * 70)
    print("\nNext steps:")
    print("  - Add schemas for your datasets in data/schemas/")
    print("  - Add tags to datasets for better organization")
    print("  - Use catalog.load(name, validate=True) to validate data")
    print("  - See docs/DATA_CATALOG_USAGE.md for full documentation")


if __name__ == "__main__":
    main()
