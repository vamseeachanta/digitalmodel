"""Body-level control surface resolution (#609).

``BodySpec.control_surface`` must never be silently ignored: backend
emission, mesh packaging, and preflight all share one resolution rule —
body-level overrides vessel-level.
"""

from __future__ import annotations

import shutil
from pathlib import Path

import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.input_schemas import (
    BodySpec,
    ControlSurfaceSpec,
    DiffractionSpec,
    VesselSpec,
)
from digitalmodel.hydrodynamics.diffraction.mesh_packaging import (
    iter_mesh_references,
)
from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

FIXTURES_DIR = Path(__file__).parent / "fixtures"
MESH_FIXTURE = (
    Path(__file__).parents[1] / "bemrosetta" / "fixtures" / "sample_box.gdf"
)

BODY_CS = """\
    control_surface:
      type: mesh
      mesh_file: "cs_body.gdf"
"""

VESSEL_CS = """\
      control_surface:
        type: mesh
        mesh_file: "cs_vessel.gdf"
"""


def _multibody_spec(
    tmp_path: Path, body_level: bool = True, vessel_level: bool = False
) -> Path:
    """FPSO+turret fixture in tmp_path with control surfaces injected.

    Body-level CS goes on the Turret body; vessel-level CS on the Turret
    vessel. Body meshes are localized so the spec is self-contained.
    """
    text = (FIXTURES_DIR / "spec_fpso_turret.yml").read_text()
    text = text.replace(
        'mesh_file: "../../bemrosetta/fixtures/sample_box.gdf"',
        'mesh_file: "hull.gdf"',
    )
    text = text.replace(
        'mesh_file: "../../bemrosetta/fixtures/sample_box.dat"',
        'mesh_file: "turret.gdf"',
    ).replace("mesh_format: dat", "mesh_format: gdf")
    if vessel_level:
        text = text.replace(
            "      fixed_dofs: [surge, sway, yaw]",
            VESSEL_CS + "      fixed_dofs: [surge, sway, yaw]",
        )
    if body_level:
        text = text.replace(
            '    connection_parent: "FPSO_Hull"',
            BODY_CS + '    connection_parent: "FPSO_Hull"',
        )
    spec_path = tmp_path / "spec.yml"
    spec_path.write_text(text)
    for name in ("hull.gdf", "turret.gdf", "cs_body.gdf", "cs_vessel.gdf"):
        shutil.copy2(MESH_FIXTURE, tmp_path / name)
    return spec_path


def _bodies_from(result: Path) -> list[dict]:
    documents = [d for d in yaml.safe_load_all(result.read_text()) if d]
    return documents[0]["Bodies"]


class TestResolutionRule:
    def _vessel(self, control_surface=None) -> VesselSpec:
        data = DiffractionSpec.from_yaml(
            FIXTURES_DIR / "spec_ship_raos.yml"
        ).vessel.model_dump()
        data["control_surface"] = control_surface
        return VesselSpec.model_validate(data)

    def test_body_level_wins(self):
        body = BodySpec(
            vessel=self._vessel({"type": "mesh", "mesh_file": "vessel.gdf"}),
            control_surface=ControlSurfaceSpec(type="mesh", mesh_file="body.gdf"),
        )
        assert body.resolve_control_surface().mesh_file == "body.gdf"

    def test_falls_back_to_vessel_level(self):
        body = BodySpec(
            vessel=self._vessel({"type": "mesh", "mesh_file": "vessel.gdf"})
        )
        assert body.resolve_control_surface().mesh_file == "vessel.gdf"

    def test_none_when_neither(self):
        assert BodySpec(vessel=self._vessel()).resolve_control_surface() is None


class TestBackendEmission:
    def test_body_level_cs_emitted_for_its_body_only(self, tmp_path):
        spec_path = _multibody_spec(tmp_path, body_level=True)
        out = tmp_path / "out"
        result = SpecConverter(spec_path).convert(
            solver="orcawave", output_dir=out, quality_gates=False
        )
        hull, turret = _bodies_from(result)
        assert "BodyControlSurfaceMeshFileName" not in hull
        assert turret["BodyControlSurfaceMeshFileName"] == "cs_body.gdf"
        assert turret["BodyControlSurfaceType"] == "Defined by mesh file"

    def test_body_level_overrides_vessel_level(self, tmp_path):
        spec_path = _multibody_spec(tmp_path, body_level=True, vessel_level=True)
        result = SpecConverter(spec_path).convert(
            solver="orcawave", output_dir=tmp_path / "out", quality_gates=False
        )
        _, turret = _bodies_from(result)
        assert turret["BodyControlSurfaceMeshFileName"] == "cs_body.gdf"

    def test_vessel_level_still_works(self, tmp_path):
        spec_path = _multibody_spec(tmp_path, body_level=False, vessel_level=True)
        result = SpecConverter(spec_path).convert(
            solver="orcawave", output_dir=tmp_path / "out", quality_gates=False
        )
        _, turret = _bodies_from(result)
        assert turret["BodyControlSurfaceMeshFileName"] == "cs_vessel.gdf"

    def test_general_section_flag_sees_body_level(self, tmp_path):
        spec_path = _multibody_spec(tmp_path, body_level=True)
        result = SpecConverter(spec_path).convert(
            solver="orcawave", output_dir=tmp_path / "out", quality_gates=False
        )
        documents = [d for d in yaml.safe_load_all(result.read_text()) if d]
        # PyYAML (YAML 1.1) parses OrcaWave's "Yes" as boolean True
        assert documents[0]["QuadraticLoadControlSurface"] is True


class TestPackagingAndPreflight:
    def test_body_level_cs_collected_and_packaged(self, tmp_path):
        spec_path = _multibody_spec(tmp_path, body_level=True)
        spec = DiffractionSpec.from_yaml(spec_path)
        files = [r.mesh_file for r in iter_mesh_references(spec)]
        assert "cs_body.gdf" in files

        out = tmp_path / "out"
        SpecConverter(spec_path).convert(
            solver="orcawave", output_dir=out, quality_gates=False
        )
        assert (out / "cs_body.gdf").is_file()

    def test_missing_body_level_cs_fails_preflight(self, tmp_path):
        spec_path = _multibody_spec(tmp_path, body_level=True)
        (tmp_path / "cs_body.gdf").unlink()
        converter = SpecConverter(spec_path)
        with pytest.raises(FileNotFoundError, match="cs_body.gdf"):
            converter.convert(
                solver="orcawave",
                output_dir=tmp_path / "out",
                quality_gates=False,
            )
        issue = next(i for i in converter.validate() if "cs_body.gdf" in i)
        assert "Turret" in issue
