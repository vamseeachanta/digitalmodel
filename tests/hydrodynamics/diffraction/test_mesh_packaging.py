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



@pytest.mark.parametrize("route", ["runner", "implicit_runner", "converter", "programmatic", "loaded"])
@pytest.mark.parametrize("partial", [False, True])
def test_symmetric_packaged_mesh_resolves_from_caller_folder(tmp_path, route, partial):
    """A decoy beside the spec must never replace the packaged mesh check."""
    import yaml
    from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend
    from digitalmodel.hydrodynamics.diffraction.orcawave_runner import OrcaWaveRunner, RunConfig
    from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

    case = tmp_path / "case"
    meshes = case / "meshes"
    meshes.mkdir(parents=True)
    source = Path(__file__).resolve().parents[3] / "examples/hydrodynamics/diffraction/unit_box_rao/unit_box.gdf"
    lines = source.read_text(encoding="utf-8").splitlines()
    for index in range(4, len(lines)):
        x, y, z = map(float, lines[index].split())
        lines[index] = f"{x:.6f} {y - 0.5:.6f} {z:.6f}"
    assert {float(line.split()[1]) for line in lines[4:]} == {-0.5, 0.5}
    if partial:
        # Omit the panel on x = 0 so reflection does not duplicate an interior wall.
        lines = lines[:-4]
        lines[3] = "4"
    lines[2] = "1 0" if partial else "0 0"
    (meshes / "hull.gdf").write_text("\n".join(lines) + "\n", encoding="utf-8")
    lines[2] = "0 0" if partial else "1 1"
    (case / "hull.gdf").write_text("\n".join(lines) + "\n", encoding="utf-8")
    data = _spec_data("meshes/hull.gdf")
    data["vessel"]["geometry"]["symmetry"] = "yz" if partial else "none"
    spec_path = case / "spec.yml"
    spec_path.write_text(yaml.safe_dump(data), encoding="utf-8")
    output = tmp_path / "out"

    def generate():
        if route == "converter":
            return SpecConverter(spec_path).convert(solver="orcawave", output_dir=output)
        if route in {"runner", "implicit_runner"}:
            runner = OrcaWaveRunner(RunConfig(output_dir=output, generate_modular=True))
            result = runner.prepare(
                DiffractionSpec.from_yaml(spec_path),
                spec_path=spec_path if route == "runner" else None,
            )
            assert result.modular_files
            return result.input_file
        if route == "loaded":
            spec = DiffractionSpec.from_yaml(spec_path)
            packaged, _, _ = package_spec_meshes(spec, output)
            return OrcaWaveBackend().generate_single(packaged, output)
        spec = DiffractionSpec.model_validate(data)
        assert spec._source_dir is None
        packaged, _, _ = package_spec_meshes(spec, output, spec_dir=case)
        assert spec._source_dir is None
        return OrcaWaveBackend().generate_single(packaged, output)

    emitted = yaml.safe_load(generate().read_text(encoding="utf-8"))
    assert emitted["Bodies"][0]["BodyMeshSymmetry"] == ("yz plane" if partial else "None")
    assert emitted["Bodies"][0]["BodyMeshFileName"] == "hull.gdf"


class TestCsfControlSurfacePassthrough:
    def test_csf_control_surface_packages_unchanged(self, tmp_path: Path):
        """A .csf control surface is a valid native OrcaWave input and must
        package (previously hard-rejected with 'Unsupported mesh format')."""
        source = Path(__file__).parent.parent / "bemrosetta/fixtures/sample_box.gdf"
        (tmp_path / "hull.gdf").write_bytes(source.read_bytes())
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
        source = Path(__file__).parent.parent / "bemrosetta/fixtures/sample_box.gdf"
        (tmp_path / "hull.gdf").write_bytes(source.read_bytes())
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

        source = Path(__file__).parent.parent / "bemrosetta/fixtures/sample_box.gdf"
        (tmp_path / "hull.gdf").write_bytes(source.read_bytes())
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
