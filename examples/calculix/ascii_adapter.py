# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Parse invented ASCII results in a temporary folder without running a solver.

All physical values are synthetic: node 1 has axial stress 120 MPa, zero
transverse/shear stress and displacement (3, 4, 12) mm. Independently,
sigma_vm = abs(120) = 120 MPa and norm(u) = sqrt(9 + 16 + 144) = 13 mm.
These fields are a parsing demonstration, not a mechanically solved state.
Record flags and field widths follow the existing parser in
src/digitalmodel/solvers/calculix/result_parser.py; see the catalog at
docs/adapters/simulation-matrix.md for its accepted subset and limitations.
"""

import json
from pathlib import Path
from tempfile import TemporaryDirectory

from digitalmodel.solvers.calculix.result_parser import CalculiXResultParser


# Minimal synthetic result blocks, not a complete solver-produced mesh file.
# DISP/STRESS headers, component descriptors and terminators follow the public
# parser's ASCII contract; the node identifier and physical values are invented.
SYNTHETIC_FRD = (
    " -4  DISP        3    1\n"
    " -5  D1\n"
    " -5  D2\n"
    " -5  D3\n"
    " -1         1 3.00000E+00 4.00000E+00 1.20000E+01\n"
    " -3\n"
    " -4  STRESS      6    1\n"
    " -5  SXX\n"
    " -5  SYY\n"
    " -5  SZZ\n"
    " -5  SXY\n"
    " -5  SYZ\n"
    " -5  SZX\n"
    " -1         1 1.20000E+02 0.00000E+00 0.00000E+00"
    " 0.00000E+00 0.00000E+00 0.00000E+00\n"
    " -3\n"
)


def run_example(directory: Path) -> dict:
    """Write the synthetic fixture into an existing folder and parse its fields.

    Args:
        directory: Temporary folder supplied by the caller.

    Returns:
        Public parser outputs with caller-supplied unit labels. No conversion
        or inference of units is performed.
    """
    (directory / "synthetic.frd").write_text(SYNTHETIC_FRD, encoding="ascii")
    parser = CalculiXResultParser("synthetic", directory)
    fields = parser.parse_frd_file()
    return {
        "units": {"stress": "MPa", "displacement": "mm"},
        "stress_at_node": parser.get_stress_at_node(1),
        "displacement_at_node": fields["displacements"][1],
        "max_von_mises": parser.get_max_von_mises(),
        "max_displacement": parser.get_max_displacement(),
    }


def main() -> None:
    """Print extracted quantities from an isolated temporary ASCII fixture."""
    with TemporaryDirectory(prefix="calculix_ascii_") as directory:
        print(json.dumps(run_example(Path(directory)), sort_keys=True))


if __name__ == "__main__":
    main()
