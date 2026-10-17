# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Generate and merge two self-contained OrcaFlex catenary inputs.

Intermediate specifications and includes stay in --work-dir (or TMPDIR).
The repository generator, converter and YAML writer own all model serialization.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path
import tempfile

from digitalmodel.solvers.orcaflex.format_converter.modular_to_single import (
    ModularToSingleConverter,
)
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.post_validator import (
    PostGenerationValidator,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import (
    GenericLine,
    GenericLineType,
    GenericModel,
    ProjectInputSpec,
)
from digitalmodel.solvers.orcaflex.yaml_utils import orcaflex_load


ROOT = Path(__file__).resolve().parents[2]
BENCHMARK = ROOT / "docs/benchmarks/catenary_closed_form"
COMMAND = "PYTHONPATH=src uv run --no-sources python scripts/benchmark/build_catenary_closed_form_inputs.py"


def specification(case):
    """Express one fixed-position, freely rotating line through GenericModel.

    Zero friction coefficients switch off both components of seabed friction.
    Fixed connections express global z directly, without an anchored end's
    seabed-relative elevation. Zero connection stiffness permits rotation.
    The recorded normal stiffness bounds penetration; see the README Limits.
    """
    inputs = case["inputs"]
    name = f"catenary_{case['case'].lower()}"
    type_name = f"{name}_type"
    return ProjectInputSpec(
        metadata={
            "name": name, "description": case["description"],
            "structure": "generic", "operation": "statics",
        },
        environment={
            "water": {"depth": 150, "density": 1.025},
            "seabed": {
                "slope": 0,
                "stiffness": {
                    "normal": inputs["seabed_normal_stiffness_kN_per_m_per_m2"]
                },
                "friction_coefficient": 0,
            },
            # Zero-height Airy matches the repository still-water form in
            # docs/domains/pipeline-installation/slay-campaign/models/
            # d0-t0/includes/03_environment.yml. The native load record in
            # docs/domains/pipeline-installation/slay-campaign/
            # native-qa-followup.html records all six S-lay models loading
            # with this include form; it does not establish B3 statics clearance.
            # "No waves" is not a native enum.
            "waves": {"trains": [{"type": "Airy", "height": 0, "period": 8}]},
            "current": {"speed": 0}, "wind": {"speed": 0},
        },
        generic=GenericModel(
            general_properties={"UnitsSystem": "SI", "g": 9.80665},
            line_types=[GenericLineType(
                name=type_name, category="General",
                outer_diameter=inputs["outer_diameter_m"], inner_diameter=0,
                mass_per_length=inputs["mass_te_per_m"],
                axial_stiffness=inputs["EA_kN"], bending_stiffness=0,
                properties={
                    "SeabedLateralFrictionCoefficient": 0,
                    "SeabedAxialFrictionCoefficient": 0,
                },
            )],
            lines=[GenericLine(name=name, line_type_refs=[type_name], properties={
                "IncludeTorsion": "No",
                "TopEnd": "End B",
                "LengthAndEndOrientations": "Explicit",
                "Representation": "Finite element",
                "EndAConnection": "Fixed", "EndAX": 0, "EndAY": 0, "EndAZ": -150,
                "EndBConnection": "Fixed", "EndBX": inputs["X_m"], "EndBY": 0, "EndBZ": 0,
                "ConnectionxBendingStiffness, ConnectionyBendingStiffness": [[0, None], [0, None]],
                "LineType, Length, TargetSegmentLength": [[type_name, 600, 1]],
                "ContentsMethod": "Uniform", "ContentsDensity": 0,
                "IncludedInStatics": "Yes", "StaticsStep1": "Catenary",
                "StaticsStep2": "Full statics",
            })],
        ),
    )


def build(output_dir, work_dir):
    """Generate includes, validate references and merge through the converter.

    Args:
        output_dir: Destination for the two model files and manifest.
        work_dir: Scratch parent for intermediate includes; retained for audit.
    """
    output_dir.mkdir(parents=True, exist_ok=True)
    work_dir.mkdir(parents=True, exist_ok=True)
    scratch = Path(tempfile.mkdtemp(prefix="catenary-build-", dir=work_dir))
    cases = json.loads((BENCHMARK / "benchmark.json").read_text())["cases"]
    manifest = []
    for case in cases:
        generated = scratch / case["case"]
        generator = ModularModelGenerator.from_spec(specification(case))
        generator.generate(generated)
        findings = PostGenerationValidator().validate_directory(generated / "includes")
        if findings:
            raise ValueError(f"Generator findings: {findings}")
        target = output_dir / Path(case["model_file"]).name
        report = ModularToSingleConverter().convert(generated / "master.yml", target)
        if not report.success or report.warnings:
            raise ValueError(f"Merge findings: {report.warnings}")
        merged, _ = orcaflex_load(target)
        findings = PostGenerationValidator().validate_data({target.name: merged})
        if findings:
            raise ValueError(f"Merged model findings: {findings}")
        payload = target.read_bytes()
        manifest.append({
            "path": case["model_file"], "bytes": len(payload),
            "sha256": hashlib.sha256(payload).hexdigest(), "command": COMMAND,
        })
    (output_dir / "MANIFEST.json").write_text(json.dumps({"files": manifest}, indent=2) + "\n")
    for entry in manifest:
        print(f"Generated {Path(entry['path']).name}: {entry['bytes']} bytes; references valid")


def main():
    """Parse output locations and rebuild the committed text inputs."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=BENCHMARK / "inputs")
    parser.add_argument("--work-dir", type=Path, default=os.environ.get("TMPDIR"))
    args = parser.parse_args()
    if args.work_dir is None:
        parser.error("Set TMPDIR to scratch storage or supply --work-dir")
    build(args.output_dir, args.work_dir)


if __name__ == "__main__":
    main()
