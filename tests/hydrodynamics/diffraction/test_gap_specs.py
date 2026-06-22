"""OrcaWave gap-spec use-case templates convert offline (issue #953).

Each of the three example workflows under ``examples/workflows/`` exercises one
partial OrcaWave option (mean drift, field points, control surface). This suite
proves they are real, valid ``DiffractionSpec`` files that convert to an
OrcaWave model fully offline (no license): ``SpecConverter.validate()`` returns
no issues, ``convert(solver="orcawave")`` produces a ``.yml``, and the
option-specific token actually appears in the generated OrcaWave YAML.
"""

from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

EXAMPLES_DIR = Path(__file__).parents[3] / "examples" / "workflows"

MEAN_DRIFT_SPEC = EXAMPLES_DIR / "orcawave-mean-drift" / "spec.yml"
FIELD_POINTS_SPEC = EXAMPLES_DIR / "orcawave-field-points" / "spec.yml"
CONTROL_SURFACE_SPEC = EXAMPLES_DIR / "orcawave-control-surface" / "spec.yml"

ALL_SPECS = [MEAN_DRIFT_SPEC, FIELD_POINTS_SPEC, CONTROL_SURFACE_SPEC]


def _convert_offline(spec_path: Path, output_dir: Path) -> str:
    """Validate + convert a spec to OrcaWave; return the generated YAML text."""
    converter = SpecConverter(spec_path)
    assert converter.validate() == []
    generated = converter.convert(solver="orcawave", output_dir=output_dir)
    assert generated.suffix == ".yml"
    assert generated.is_file()
    return generated.read_text()


@pytest.mark.parametrize("spec_path", ALL_SPECS, ids=lambda p: p.parent.name)
def test_gap_spec_exists_validates_and_converts(spec_path, tmp_path):
    """Every gap-spec template exists, validates clean, and converts offline."""
    assert spec_path.is_file(), f"missing example spec: {spec_path}"
    text = _convert_offline(spec_path, tmp_path)
    assert "SolveType" in text


def test_mean_drift_spec_emits_mean_drift_solve_type(tmp_path):
    text = _convert_offline(MEAN_DRIFT_SPEC, tmp_path)
    assert (
        "SolveType: Potential and source + mean drift "
        "(momentum conservation)" in text
    )


def test_field_points_spec_emits_field_point_group(tmp_path):
    text = _convert_offline(FIELD_POINTS_SPEC, tmp_path)
    # Combined-key convention used by the OrcaWave backend (#501).
    assert "FieldPointX, FieldPointY, FieldPointZ" in text
    assert "DetectAndSkipFieldPointsInsideBodies" in text


def test_control_surface_spec_emits_control_surface_mesh(tmp_path):
    text = _convert_offline(CONTROL_SURFACE_SPEC, tmp_path)
    # control_surface removal turns interior panels off and references the
    # control-surface mesh by basename.
    assert "BodyAddInteriorSurfacePanels: No" in text
    assert "BodyControlSurfaceMeshFileName: control_surface.gdf" in text
