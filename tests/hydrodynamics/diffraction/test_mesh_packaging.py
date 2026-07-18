"""Tests for shared mesh packaging (#605/#606/#609 review sweep).

Covers:
- WAMIT .csf control-surface passthrough (native OrcaWave control-surface
  format; L00_validation_wamit 2.7/2.8 golden specs reference .csf files)
- .csf stays invalid for body panel meshes
- mesh_format recorded on packaged control surfaces (ControlSurfaceSpec's
  optional format previously skipped the rewrite entirely)
- destination basename collisions fail loud instead of silently overwriting
"""

from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.mesh_packaging import (
    package_spec_meshes,
)


def _spec_data(mesh_file: str = "hull.gdf") -> dict:
    return {
        "vessel": {
            "name": "TestVessel",
            "geometry": {"mesh_file": mesh_file},
            "inertia": {
                "mass": 1000.0,
                "centre_of_gravity": [0.0, 0.0, 0.0],
                "radii_of_gyration": [1.0, 2.0, 3.0],
            },
        },
        "environment": {"water_depth": 100.0},
        "frequencies": {"input_type": "frequency", "values": [0.5, 1.0]},
        "wave_headings": {"values": [0.0, 90.0]},
    }


def _multibody_spec_data(mesh_a: str, mesh_b: str) -> dict:
    def body(name: str, mesh: str) -> dict:
        return {
            "vessel": {
                "name": name,
                "geometry": {"mesh_file": mesh},
                "inertia": {
                    "mass": 1000.0,
                    "centre_of_gravity": [0.0, 0.0, 0.0],
                    "radii_of_gyration": [1.0, 2.0, 3.0],
                },
            }
        }

    return {
        "bodies": [body("HullA", mesh_a), body("HullB", mesh_b)],
        "environment": {"water_depth": 100.0},
        "frequencies": {"input_type": "frequency", "values": [0.5, 1.0]},
        "wave_headings": {"values": [0.0, 90.0]},
    }


class TestCsfControlSurfacePassthrough:
    def test_csf_control_surface_packages_unchanged(self, tmp_path: Path):
        """A .csf control surface is a valid native OrcaWave input and must
        package (previously hard-rejected with 'Unsupported mesh format')."""
        (tmp_path / "hull.gdf").write_text("hull")
        (tmp_path / "cs.csf").write_text("csf data")
        data = _spec_data()
        data["vessel"]["control_surface"] = {
            "type": "mesh",
            "mesh_file": "cs.csf",
        }
        spec = DiffractionSpec.model_validate(data)
        out = tmp_path / "out"

        packaged_spec, packaged, _ = package_spec_meshes(
            spec, out, spec_dir=tmp_path, solver="orcawave", quality_gates=False
        )

        names = {p.name for p in packaged}
        assert "cs.csf" in names
        assert (out / "cs.csf").read_text() == "csf data"
        cs = packaged_spec.vessel.control_surface
        assert cs.mesh_file == "cs.csf"
        # The packaged format is recorded so the backend emits 'Wamit csf'.
        assert cs.mesh_format == "csf"

    def test_csf_body_mesh_still_rejected(self, tmp_path: Path):
        """.csf is control-surface-only: a .csf BODY panel mesh has no valid
        OrcaWave interpretation and must keep failing loud."""
        (tmp_path / "hull.csf").write_text("csf")
        spec = DiffractionSpec.model_validate(_spec_data(mesh_file="hull.csf"))
        with pytest.raises(ValueError, match="Unsupported mesh format"):
            package_spec_meshes(
                spec,
                tmp_path / "out",
                spec_dir=tmp_path,
                solver="orcawave",
                quality_gates=False,
            )


class TestControlSurfaceFormatRewrite:
    def test_gdf_control_surface_format_recorded(self, tmp_path: Path):
        """Packaging a .gdf control surface must record mesh_format='gdf' on
        the packaged spec (previously left None, so the backend emitted
        'Wamit csf' for GDF content)."""
        (tmp_path / "hull.gdf").write_text("hull")
        (tmp_path / "cs.gdf").write_text("cs")
        data = _spec_data()
        data["vessel"]["control_surface"] = {
            "type": "mesh",
            "mesh_file": "cs.gdf",
        }
        spec = DiffractionSpec.model_validate(data)

        packaged_spec, _, _ = package_spec_meshes(
            spec,
            tmp_path / "out",
            spec_dir=tmp_path,
            solver="orcawave",
            quality_gates=False,
        )

        assert packaged_spec.vessel.control_surface.mesh_format == "gdf"

    def test_packaged_yaml_declares_wamit_gdf(self, tmp_path: Path):
        """End-to-end: packaged .gdf control surface generates
        BodyControlSurfaceMeshFormat 'Wamit gdf' (native pairing in the L03
        Semi-sub multibody example), not 'Wamit csf'."""
        import yaml

        from digitalmodel.hydrodynamics.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        (tmp_path / "hull.gdf").write_text("hull")
        (tmp_path / "cs.gdf").write_text("cs")
        data = _spec_data()
        data["vessel"]["control_surface"] = {
            "type": "mesh",
            "mesh_file": "cs.gdf",
        }
        spec = DiffractionSpec.model_validate(data)
        out = tmp_path / "out"
        packaged_spec, _, _ = package_spec_meshes(
            spec, out, spec_dir=tmp_path, solver="orcawave", quality_gates=False
        )
        generated = OrcaWaveBackend().generate_single(packaged_spec, out)
        body = yaml.safe_load(generated.read_text())["Bodies"][0]
        assert body["BodyControlSurfaceMeshFormat"] == "Wamit gdf"


class TestDestinationCollisions:
    def test_same_basename_different_dirs_disambiguated(self, tmp_path: Path):
        """Two distinct meshes sharing a basename must not silently overwrite
        each other: each body keeps its own (renamed) mesh."""
        (tmp_path / "hull_a").mkdir()
        (tmp_path / "hull_b").mkdir()
        (tmp_path / "hull_a" / "mesh.gdf").write_text("A")
        (tmp_path / "hull_b" / "mesh.gdf").write_text("B")
        spec = DiffractionSpec.model_validate(
            _multibody_spec_data("hull_a/mesh.gdf", "hull_b/mesh.gdf")
        )
        out = tmp_path / "out"

        packaged_spec, packaged, _ = package_spec_meshes(
            spec, out, spec_dir=tmp_path, solver="orcawave", quality_gates=False
        )

        # Both meshes survive with distinct names and correct contents.
        assert (out / "mesh.gdf").read_text() == "A"
        assert (out / "mesh_2.gdf").read_text() == "B"
        # Each spec slot points at its own body's mesh.
        assert packaged_spec.bodies[0].vessel.geometry.mesh_file == "mesh.gdf"
        assert packaged_spec.bodies[1].vessel.geometry.mesh_file == "mesh_2.gdf"
        assert {p.name for p in packaged} == {"mesh.gdf", "mesh_2.gdf"}

    def test_same_file_referenced_twice_is_fine(self, tmp_path: Path):
        """Two references to the SAME file are not a collision."""
        (tmp_path / "mesh.gdf").write_text("shared")
        spec = DiffractionSpec.model_validate(
            _multibody_spec_data("mesh.gdf", "mesh.gdf")
        )
        _, packaged, _ = package_spec_meshes(
            spec,
            tmp_path / "out",
            spec_dir=tmp_path,
            solver="orcawave",
            quality_gates=False,
        )
        assert (tmp_path / "out" / "mesh.gdf").read_text() == "shared"
