"""Basic usage example for digitalmodel.hydrodynamics.diffraction.

Dry-runs the canonical unit-box example (``unit_box_rao/``) through the
OrcaWave runner: parses the high-level spec, generates the OrcaWave-native
input, and copies the mesh into a self-contained output package. On a
licensed Windows host, set ``dry_run=False`` for a full solve with automatic
result validation.

Run with::

    uv run python examples/hydrodynamics/diffraction/basic_usage.py

Equivalent CLI (preferred for day-to-day use)::

    uv run diffraction run-orcawave examples/hydrodynamics/diffraction/unit_box_rao/spec.yml \
        --dry-run -o output/
"""

from pathlib import Path

from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
    OrcaWaveRunner,
    RunConfig,
)

EXAMPLE_DIR = Path(__file__).parent / "unit_box_rao"


def main() -> None:
    spec_path = EXAMPLE_DIR / "spec.yml"
    spec = DiffractionSpec.from_yaml(spec_path)
    runner = OrcaWaveRunner(
        RunConfig(
            output_dir=Path("output"),
            dry_run=True,  # set False on a licensed host for a real solve
        )
    )
    result = runner.run(spec, spec_path=spec_path)

    print(f"Status     : {result.status}")
    print(f"Input file : {result.input_file}")
    print(f"Mesh files : {[p.name for p in result.mesh_files]}")
    print(f"Validation : {result.validation_verdict}")
    if result.owr_path:
        print(f"Results    : {result.owr_path}")


if __name__ == "__main__":
    main()
