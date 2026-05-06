"""Named multi-body OrcaWave benchmark regression tests (#2458)."""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml

from digitalmodel.benchmarks.inventory import ModelCategory, build_model_inventory
from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend
from digitalmodel.hydrodynamics.diffraction.reverse_parsers import OrcaWaveInputParser

BENCHMARK_ID = "multibody_fpso_turret_v1"
DIFFRACTION_DIR = Path(__file__).resolve().parents[1]
REPO_ROOT = Path(__file__).resolve().parents[4]
BENCHMARK_DIR = DIFFRACTION_DIR / "fixtures" / "benchmarks" / BENCHMARK_ID
SPEC_PATH = BENCHMARK_DIR / "spec.yml"
MANIFEST_PATH = BENCHMARK_DIR / "manifest.yaml"


def _load_spec() -> DiffractionSpec:
    return DiffractionSpec.from_yaml(SPEC_PATH)


def _load_manifest() -> dict:
    return yaml.safe_load(MANIFEST_PATH.read_text(encoding="utf-8"))


def _generate_orcawave_yml(spec: DiffractionSpec, tmp_path: Path) -> Path:
    return OrcaWaveBackend().generate_single(spec, tmp_path)


def _roundtrip(tmp_path: Path) -> tuple[DiffractionSpec, DiffractionSpec]:
    original = _load_spec()
    yml_path = _generate_orcawave_yml(original, tmp_path)
    parsed = OrcaWaveInputParser().parse(yml_path)
    assert parsed.bodies is not None
    return original, parsed


def _body_map(spec: DiffractionSpec):
    assert spec.bodies is not None
    return {body.vessel.name: body for body in spec.bodies}


def _expected_inertia_from_radii(body) -> dict[str, float]:
    inertia = body.vessel.inertia
    assert inertia.radii_of_gyration is not None
    return {
        "Ixx": inertia.mass * inertia.radii_of_gyration[0] ** 2,
        "Iyy": inertia.mass * inertia.radii_of_gyration[1] ** 2,
        "Izz": inertia.mass * inertia.radii_of_gyration[2] ** 2,
    }


class TestMultibodyFpsoTurretBenchmark:
    def test_benchmark_manifest_exists(self) -> None:
        assert MANIFEST_PATH.is_file()
        assert _load_manifest()

    def test_benchmark_id_in_manifest_matches_folder(self) -> None:
        assert _load_manifest()["benchmark_id"] == BENCHMARK_ID
        assert BENCHMARK_DIR.name == BENCHMARK_ID

    def test_benchmark_spec_loads_as_diffraction_spec(self) -> None:
        assert isinstance(_load_spec(), DiffractionSpec)

    def test_benchmark_has_two_bodies_with_expected_names(self) -> None:
        spec = _load_spec()
        assert spec.bodies is not None
        assert [body.vessel.name for body in spec.bodies] == ["FPSO_Hull", "Turret"]

    def test_mesh_files_referenced_by_spec_exist_on_disk(self) -> None:
        spec = _load_spec()
        for body in spec.get_bodies():
            mesh_path = (SPEC_PATH.parent / body.vessel.geometry.mesh_file).resolve()
            assert mesh_path.is_file(), mesh_path

    def test_forward_generation_does_not_raise(self, tmp_path: Path) -> None:
        yml_path = _generate_orcawave_yml(_load_spec(), tmp_path)
        assert yml_path.is_file()

    def test_roundtrip_preserves_body_count(self, tmp_path: Path) -> None:
        original, parsed = _roundtrip(tmp_path)
        assert parsed.bodies is not None
        assert original.bodies is not None
        assert len(parsed.bodies) == len(original.bodies)

    def test_roundtrip_preserves_body_names(self, tmp_path: Path) -> None:
        original, parsed = _roundtrip(tmp_path)
        assert [body.vessel.name for body in parsed.bodies] == [
            body.vessel.name for body in original.bodies
        ]

    def test_roundtrip_preserves_fixed_dofs_on_turret(self, tmp_path: Path) -> None:
        original, parsed = _roundtrip(tmp_path)
        original_turret = _body_map(original)["Turret"]
        parsed_turret = _body_map(parsed)["Turret"]
        assert sorted(parsed_turret.vessel.fixed_dofs or []) == sorted(
            original_turret.vessel.fixed_dofs or []
        )

    def test_roundtrip_preserves_connection_parent(self, tmp_path: Path) -> None:
        _, parsed = _roundtrip(tmp_path)
        assert _body_map(parsed)["Turret"].connection_parent == "FPSO_Hull"

    def test_roundtrip_preserves_body_position(self, tmp_path: Path) -> None:
        original, parsed = _roundtrip(tmp_path)
        parsed_bodies = _body_map(parsed)
        for name, original_body in _body_map(original).items():
            assert parsed_bodies[name].position == pytest.approx(
                original_body.position, abs=1e-6
            )

    def test_roundtrip_preserves_body_attitude(self, tmp_path: Path) -> None:
        original, parsed = _roundtrip(tmp_path)
        parsed_bodies = _body_map(parsed)
        for name, original_body in _body_map(original).items():
            assert parsed_bodies[name].attitude == pytest.approx(
                original_body.attitude, abs=1e-6
            )

    def test_roundtrip_preserves_body_mass(self, tmp_path: Path) -> None:
        original, parsed = _roundtrip(tmp_path)
        parsed_bodies = _body_map(parsed)
        for name, original_body in _body_map(original).items():
            assert parsed_bodies[name].vessel.inertia.mass == pytest.approx(
                original_body.vessel.inertia.mass, rel=1e-6
            )

    def test_roundtrip_preserves_centre_of_gravity_per_body(
        self, tmp_path: Path
    ) -> None:
        original, parsed = _roundtrip(tmp_path)
        parsed_bodies = _body_map(parsed)
        for name, original_body in _body_map(original).items():
            assert (
                parsed_bodies[name].vessel.inertia.centre_of_gravity
                == pytest.approx(
                    original_body.vessel.inertia.centre_of_gravity,
                    rel=1e-6,
                )
            )

    def test_roundtrip_preserves_radii_of_gyration_derived_inertia(
        self, tmp_path: Path
    ) -> None:
        original, parsed = _roundtrip(tmp_path)
        parsed_bodies = _body_map(parsed)
        for name, original_body in _body_map(original).items():
            expected = _expected_inertia_from_radii(original_body)
            parsed_tensor = parsed_bodies[name].vessel.inertia.inertia_tensor
            assert parsed_tensor is not None
            for key, expected_value in expected.items():
                assert parsed_tensor[key] == pytest.approx(expected_value, rel=1e-4)

    def test_benchmark_declares_handoff_bridge_candidate_in_manifest(self) -> None:
        bridge_text = " ".join(_load_manifest().get("bridge_candidates", []))
        assert "future OrcaWave -> OrcaFlex handoff validation" in bridge_text

    def test_benchmark_registered_in_model_inventory(self) -> None:
        entries = {entry.name: entry for entry in build_model_inventory()}
        entry = entries[BENCHMARK_ID]
        assert entry.category is ModelCategory.FREQUENCY_DOMAIN
        assert entry.path == Path(
            "tests/hydrodynamics/diffraction/fixtures/benchmarks/"
            "multibody_fpso_turret_v1/spec.yml"
        )
        assert set(entry.tags) >= {
            "orcawave",
            "multibody",
            "fpso",
            "turret",
            "benchmark",
            "roundtrip",
        }
        assert (REPO_ROOT / entry.path).is_file()
