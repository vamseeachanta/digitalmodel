# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Create a synthetic DXF, read it, and report its length in metres."""

import argparse
import json
from pathlib import Path

import ezdxf

from digitalmodel.cad_readers import DxfGeometry, read_cad


def run(output_directory: Path) -> dict[str, float | str]:
    """Hand calculation: 2000 mm * (1 m / 1000 mm) = 2 m.

    NIST SP 811 (2008), Appendix B.8 and SI prefix milli. This is a
    synthetic input, not a design dimension or an acceptance limit.
    """
    output_directory.mkdir(parents=True, exist_ok=True)
    path = output_directory / "synthetic_line.dxf"
    if path.exists():
        raise FileExistsError("Synthetic example output already exists")
    doc = ezdxf.new()
    doc.units = ezdxf.units.MM
    doc.modelspace().add_line((0, 0, 0), (2000, 0, 0))
    doc.saveas(path)
    model = read_cad(path)
    assert isinstance(model.geometry, DxfGeometry)
    line = next(iter(model.geometry.document.modelspace()))
    length = (line.dxf.end - line.dxf.start).magnitude
    return {
        "input_length": length,
        "input_unit": model.units.coordinate_unit,
        "length_m": length * model.units.metres_per_unit,
        "units_source": model.units.source,
    }


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-directory", required=True, type=Path)
    args = parser.parse_args()
    print(json.dumps(run(args.output_directory), indent=2))
