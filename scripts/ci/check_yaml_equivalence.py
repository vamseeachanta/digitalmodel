#!/usr/bin/env python3
# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Compare parsed YAML configurations, preserving scalar and key types.

Covered by the unconditional CI harness instead of the domain matrix.
"""

import argparse
from pathlib import Path
import sys

import yaml


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("before", type=Path)
    parser.add_argument("after", type=Path)
    args = parser.parse_args()
    try:
        values = [yaml.safe_dump(yaml.safe_load(path.read_text(encoding="utf-8")),
                                sort_keys=True)
                  for path in (args.before, args.after)]
    except (OSError, UnicodeError, yaml.YAMLError, TypeError, ValueError):
        print("Invalid YAML configuration input", file=sys.stderr)
        return 2
    equal = values[0] == values[1]
    print("YAML values match" if equal else "YAML values differ")
    return 0 if equal else 1


if __name__ == "__main__":
    sys.exit(main())
