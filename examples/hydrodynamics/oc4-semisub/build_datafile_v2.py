# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Versioned extended-band datafile builder; historical inputs are untouched."""

import argparse
import json
from pathlib import Path

import yaml

HERE = Path(__file__).resolve().parent


def datafile_model(mesh_name, *, specification=None, expected=None):
    """Construct SI data in solver tonne/kN units without importing a solver."""
    if Path(mesh_name).name != mesh_name or not mesh_name.endswith(".gdf"):
        raise ValueError("Mesh must be a local GDF basename")
    specification = specification or json.loads((HERE / "input-specification-v2.json").read_text())
    expected = expected or json.loads((HERE / "expected-v2.json").read_text())
    mass = expected["mass_properties"]
    body = {
        "BodyName": "Public semisubmersible", "BodyMeshPosition": [0, 0, 0],
        "BodyMeshAttitude": [0, 0, 0], "BodyIncludedInAnalysis": "Yes",
        "BodyMeshFileName": mesh_name, "BodyMeshFormat": "Wamit gdf",
        "BodyMeshLengthUnits": "m", "BodyMeshSymmetry": "None",
        "BodyOriginType": "Free surface", "BodyAddInteriorSurfacePanels": "Yes",
        "BodyHydrostaticIntegralMethod": "Standard",
        "BodyHydrostaticStiffnessMethod": "Displacement",
        "BodyInertiaSpecifiedBy": "Matrix (for a general body)",
        "BodyCentreOfMass": mass["centre_of_mass_m"], "BodyMass": mass["mass_kg"] / 1000,
        "BodyInertiaTensorRx, BodyInertiaTensorRy, BodyInertiaTensorRz":
            [[v / 1000 for v in row] for row in mass["inertia_about_com_kg_m2"]],
        "BodyInertiaTensorOriginType": "Centre of mass",
        "BodyConnectionParent": "Free", "BodyIncreaseRollDampingToTarget": "No",
    }
    axes = ("x", "y", "z", "Rx", "Ry", "Rz")
    for kind in ("Stiffness", "Damping"):
        key = ", ".join(f"BodyExternal{kind}Matrix{axis}" for axis in axes)
        body[key] = [[0] * 6 for _ in axes]
        body[f"BodyExternal{kind}MatrixOriginType"] = "Body origin"
    body.update({f"BodyFixedDOF{axis}": "No" for axis in axes})
    deck = {
        "UnitsSystem": "SI", "SolveType": "Potential formulation only",
        "HasResonanceDampingLid": "No", "OutputPanelPressures": "No",
        "OutputBodyWireFrames": "Yes", "OutputIntermediateResults": "No",
        "ComputationStrategy": "Optimised for run time",
        "LoadRAOCalculationMethod": "Both", "PreferredLoadRAOCalculationMethod": "Haskind",
        "QuadraticLoadControlSurface": "No", "QuadraticLoadMomentumConservation": "No",
        "LengthTolerance": 100e-9, "WaterlineZTolerance": 1e-6, "WaterlineGapTolerance": 1e-6,
        "DivideNonPlanarPanels": "No", "LinearSolverMethod": "Direct LU",
        "ValidatePanelArrangement": "Yes", "BodyVolumeWarningLevel": 1e-12,
        "PanelAspectRatioWarningLevel": 25, "PanelsPerWavelengthWarningLevel": 6,
        "WaterDepth": specification["depth_m"],
        "WaterDensity": specification["density_kg_m3"] / 1000,
        "WavesReferredToBy": "frequency (rad/s)",
        "HasWaveSpectrumForDragLinearisation": "No", "MorisonFluidVelocity": "Undisturbed incident wave",
        "PeriodOrFrequency": specification["frequencies_rad_s"],
        "WaveHeading": specification["headings_deg"], "EnableMultibodyConstraints": "No",
        "Bodies": [body], "DetectAndSkipFieldPointsInsideBodies": "Yes",
    }
    return deck


def build_datafile(output, mesh_name, *, specification=None, expected=None):
    """Serialize the versioned deck without changing historical records."""
    deck = datafile_model(mesh_name, specification=specification, expected=expected)
    Path(output).write_text(yaml.safe_dump(deck, sort_keys=False), encoding="utf-8")
    return deck


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path)
    parser.add_argument("mesh", help="local GDF basename")
    args = parser.parse_args()
    build_datafile(args.output, args.mesh)


if __name__ == "__main__":
    main()
