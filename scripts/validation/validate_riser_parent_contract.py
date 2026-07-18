#!/usr/bin/env python3
# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Validate and compose the approved #1602 parent contract."""

from __future__ import annotations

import argparse
from pathlib import Path

from digitalmodel.contract_validation.riser_parent_contract import (
    ContractValidationError,
    validate_parent_contract,
)


def main() -> int:
    """Run the fail-closed validator from the command line."""

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("manifest", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    try:
        validate_parent_contract(args.manifest, args.output)
    except ContractValidationError as exc:
        parser.exit(1, f"contract validation failed: {exc}\n")
    print(f"composed contract written to {args.output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
