#!/usr/bin/env python3
"""Validate all manifest.yaml files against the ModuleManifest schema.

Usage: uv run python scripts/validate_manifests.py [--dir src/digitalmodel/]
Exit code: 0 if all valid, 1 if any invalid or none found.
"""

import argparse
import sys
from pathlib import Path

from digitalmodel.specs.manifest_schema import validate_manifest_file


def main():
    parser = argparse.ArgumentParser(
        description="Validate manifest.yaml files against ModuleManifest schema"
    )
    parser.add_argument(
        "--dir",
        default="src/digitalmodel/",
        help="Root directory to search for manifest.yaml files",
    )
    args = parser.parse_args()

    root = Path(args.dir)
    manifests = sorted(root.rglob("manifest.yaml"))

    if not manifests:
        print(f"No manifest.yaml files found under {root}")
        sys.exit(1)

    errors = []
    for mf in manifests:
        try:
            result = validate_manifest_file(mf)
            print(f"  OK   {mf} ({len(result.functions)} functions)")
        except ValueError as e:
            print(f"  FAIL {mf}: {e}")
            errors.append(mf)

    total = len(manifests)
    valid = total - len(errors)
    print(f"\nSummary: {total} manifests, {valid} valid, {len(errors)} invalid")
    sys.exit(1 if errors else 0)


if __name__ == "__main__":
    main()
