# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Convert an external cached workbook using an explicit external cell map."""

import argparse
import hashlib
from pathlib import Path

from digitalmodel.subsea.on_bottom_stability.e305_fixture import convert_workbook


def main() -> None:
    """Convert and print only the resulting fixture digest."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('workbook', type=Path)
    parser.add_argument('--map', dest='cell_map', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    convert_workbook(args.workbook, args.cell_map, args.output,
                     repository_root=Path(__file__).resolve().parents[2])
    print(hashlib.sha256(args.output.read_bytes()).hexdigest())


if __name__ == '__main__':
    main()
