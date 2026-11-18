"""Generate offline native inputs for a manufactured half-box.

Usage: PYTHONPATH=src python examples/hydrodynamics/diffraction_symmetry.py OUTPUT
No solver is imported or executed by this example's conversion calls.
The directory is supplied by the caller; existing example files are refused.
"""

import argparse
import json
from pathlib import Path

from digitalmodel.hydrodynamics.diffraction.aqwa_backend import AQWABackend
from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend


def build_example(output_dir):
    """Return both preflight records after generating matching native inputs.

    Hand calculation: L=4 m, B=2 m, draft=1 m gives V=8 m3. The stored
    positive-y half encloses 4 m3. The chosen density 1000 kg/m3 gives a
    floating mass of 8000 kg. Zero relative tolerance requests equality for
    this exact manufactured arithmetic; it is not an engineering limit.
    """
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    if any(output_dir.iterdir()):
        raise FileExistsError("Example requires an empty output directory")
    # Four outward wetted quads: bottom, positive-y side and the two ends.
    panels = [
        [(-2, 0, -1), (-2, 1, -1), (2, 1, -1), (2, 0, -1)],
        [(2, 1, -1), (-2, 1, -1), (-2, 1, 0), (2, 1, 0)],
        [(-2, 1, -1), (-2, 0, -1), (-2, 0, 0), (-2, 1, 0)],
        [(2, 0, -1), (2, 1, -1), (2, 1, 0), (2, 0, 0)],
    ]
    # WAMIT v7.4, section 6.1: ISY=1 is the y=0 (xz) cut.
    records = ["Manufactured half-box", "1 1", "0 1", str(len(panels))]
    records.extend(" ".join(map(str, vertex)) for panel in panels for vertex in panel)
    with (output_dir / "box.gdf").open("x") as stream:
        stream.write("\n".join(records) + "\n")
    spec = DiffractionSpec.model_validate(
        {
            "vessel": {
                "name": "synthetic",
                "type": "barge",
                "geometry": {
                    "mesh_file": "box.gdf",
                    "symmetry": "xz",
                    "buoyancy_relative_tolerance": 0,
                },
                "inertia": {
                    "mass": 8000,
                    "centre_of_gravity": [0, 0, -0.5],
                    "radii_of_gyration": [1, 1, 1],
                },
            },
            # Density, depth, inertia and sampling are synthetic example inputs.
            "environment": {"water_depth": 50, "water_density": 1000},
            "frequencies": {"values": [1]},
            "wave_headings": {"values": [0]},
        }
    )
    spec._source_dir = output_dir
    spec_path = output_dir / "spec.yml"
    if spec_path.exists():
        raise FileExistsError("Example specification already exists")
    spec.to_yaml(spec_path)
    reports = {}
    for name, backend in [("aqwa", AQWABackend()), ("orcawave", OrcaWaveBackend())]:
        native_dir = output_dir / name
        native_dir.mkdir()
        # Keep the referenced mesh beside each native input for later use.
        (native_dir / "box.gdf").write_bytes((output_dir / "box.gdf").read_bytes())
        backend.generate_single(spec, native_dir)
        reports[name] = backend.preflight_reports
    return reports


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output_dir", type=Path)
    print(json.dumps(build_example(parser.parse_args().output_dir), indent=2))
