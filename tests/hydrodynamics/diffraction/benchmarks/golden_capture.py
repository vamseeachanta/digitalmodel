"""Golden-file capture for OrcaWave byte-identity regression (#501 Sub-task 0).

The corpus is every canonical spec under the L00 WAMIT validation suite plus
the L02 barge and L03 ship benchmarks. Goldens are captured from the
pre-#501 tree and committed BEFORE any schema change; the parametrized
byte-identity test then fails on any emission drift — token-level, no
numeric tolerance.

Regenerate (only when an intentional emission change lands):

    uv run python -m tests.hydrodynamics.diffraction.benchmarks.golden_capture
"""

from __future__ import annotations

import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[4]
BENCHMARKS_DIR = Path(__file__).resolve().parent
GOLDEN_DIR = BENCHMARKS_DIR / "golden"


def enumerate_byte_identity_fixtures() -> list[Path]:
    """Return the frozen corpus: L00 sub-specs + L02 family + L03."""
    root = REPO_ROOT / "docs" / "domains" / "orcawave"
    fixtures = sorted(
        [
            *root.glob("L00_validation_wamit/*/spec.yml"),
            *root.glob("L02_*/spec.yml"),
            *root.glob("L03_ship_benchmark/spec.yml"),
        ]
    )
    names = [golden_path_for(p).name for p in fixtures]
    assert len(names) == len(set(names)), f"golden name collision: {names}"
    return fixtures


def golden_path_for(spec_path: Path) -> Path:
    """Stable 1:1 mapping from a spec.yml to its committed golden file."""
    if spec_path.parent.parent.name == "L00_validation_wamit":
        name = f"L00_{spec_path.parent.name}"
    else:
        name = spec_path.parent.name
    return GOLDEN_DIR / f"{name}.yml"


def render_orcawave_bytes(spec_path: Path) -> bytes:
    """Generate the single OrcaWave input for *spec_path* and return its bytes."""
    from digitalmodel.hydrodynamics.diffraction.input_schemas import (
        DiffractionSpec,
    )
    from digitalmodel.hydrodynamics.diffraction.orcawave_backend import (
        OrcaWaveBackend,
    )

    spec = DiffractionSpec.from_yaml(spec_path)
    with tempfile.TemporaryDirectory() as tmp:
        generated = OrcaWaveBackend().generate_single(spec, Path(tmp))
        return generated.read_bytes()


def capture_golden(spec_path: Path) -> Path:
    """Capture (overwrite) the golden for one fixture. Pre-change tree only."""
    GOLDEN_DIR.mkdir(parents=True, exist_ok=True)
    golden = golden_path_for(spec_path)
    golden.write_bytes(render_orcawave_bytes(spec_path))
    return golden


def main() -> None:
    for spec_path in enumerate_byte_identity_fixtures():
        golden = capture_golden(spec_path)
        print(f"captured {golden.relative_to(BENCHMARKS_DIR)}")


if __name__ == "__main__":
    main()
