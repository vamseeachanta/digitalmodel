"""Regenerate data/rao_database.parquet from the five in-repository fixtures.

Run: PYTHONPATH=src uv run --no-sources python scripts/populate_rao_database.py
     --retrieved-on 2026-10-11
"""

import argparse
from pathlib import Path

from digitalmodel.hydrodynamics.hull_library.rao_fixture_population import (
    regenerate_fixture_database,
)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--retrieved-on", required=True, help="Fixture observation date, ISO format")
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[1]
    db = regenerate_fixture_database(
        root, root / "data/rao_database.parquet",
        root / "data/rao_database.manifest.json", args.retrieved_on,
    )
    print("Fixture RAO database entries:")
    for entry in db.query({}):
        print(f"  {entry.variation_id}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
