"""Semantic proof: non-catenary riser spec.yml -> native OrcaFlex YAML (#2456).

Locks in forward-path evidence for lazy-wave and steep-wave riser variants:

    canonical spec.yml
        --> ProjectInputSpec validation
        --> ModularModelGenerator.from_spec(spec).generate(tmpdir)
        --> includes/*.yml

The assertions are deterministic YAML inspections and do not require OrcFxAPI.
They focus on analysis-significant fields that distinguish these variants from a
simple catenary baseline: buoyancy/floats line types, multiple line sections,
BSR/steel-root support line types for the steep-wave family, environment/current
coupling, and cross-reference closure from Lines to LineTypes.
"""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

DOMAIN_ROOT = Path(__file__).resolve().parents[4] / "docs" / "domains" / "orcaflex"
LAZY_SPEC = DOMAIN_ROOT / "library" / "model_library" / "a01_lazy_wave_riser" / "spec.yml"
STEEP_SPEC = DOMAIN_ROOT / "library" / "model_library" / "a01_steep_wave_riser" / "spec.yml"


def _generate_case(spec_path: Path, tmp_path: Path) -> dict[str, dict]:
    """Generate a modular OrcaFlex model and return loaded YAML includes."""
    with spec_path.open(encoding="utf-8") as f:
        data = yaml.safe_load(f)
    spec = ProjectInputSpec(**data)
    assert spec.is_generic(), f"{spec_path} should remain on the generic pass-through track"

    out_dir = tmp_path / spec_path.parent.name
    ModularModelGenerator.from_spec(spec).generate(out_dir)

    result: dict[str, dict] = {}
    for yml in sorted((out_dir / "includes").glob("*.yml")):
        with yml.open(encoding="utf-8") as f:
            result[yml.name] = yaml.safe_load(f) or {}
    with (out_dir / "inputs" / "parameters.yml").open(encoding="utf-8") as f:
        result["_parameters"] = yaml.safe_load(f) or {}
    result["_master"] = {"text": (out_dir / "master.yml").read_text(encoding="utf-8")}
    return result


@pytest.fixture(scope="module")
def lazy_case(tmp_path_factory: pytest.TempPathFactory) -> dict[str, dict]:
    return _generate_case(LAZY_SPEC, tmp_path_factory.mktemp("lazy_wave_proof"))


@pytest.fixture(scope="module")
def steep_case(tmp_path_factory: pytest.TempPathFactory) -> dict[str, dict]:
    return _generate_case(STEEP_SPEC, tmp_path_factory.mktemp("steep_wave_proof"))


def _generic(generated: dict[str, dict], key: str):
    generic = generated.get("20_generic_objects.yml", {})
    assert generic, "20_generic_objects.yml missing — generic builder did not run"
    return generic.get(key) or []


def _environment(generated: dict[str, dict]) -> dict:
    env = generated.get("03_environment.yml", {}).get("Environment")
    assert env, "03_environment.yml missing Environment section"
    return env


def _assert_line_type_references_resolve(generated: dict[str, dict]) -> None:
    line_type_names = {lt.get("Name") for lt in _generic(generated, "LineTypes") if lt.get("Name")}
    assert line_type_names, "No generated LineTypes found"

    unresolved: list[tuple[str, str]] = []
    for line in _generic(generated, "Lines"):
        line_name = line.get("Name", "<unnamed>")
        for key, rows in line.items():
            if not (isinstance(key, str) and key.startswith("LineType,")):
                continue
            for row in rows or []:
                if row and row[0] not in line_type_names:
                    unresolved.append((line_name, row[0]))
    assert not unresolved, f"Unresolved LineType references: {unresolved[:10]}"


class TestLazyWaveRiserSemanticProof:
    def test_lazy_wave_preserves_float_line_type_properties(self, lazy_case: dict[str, dict]) -> None:
        by_name = {lt.get("Name"): lt for lt in _generic(lazy_case, "LineTypes")}
        assert {"10\" flexible", "10\"+Floats"}.issubset(by_name)

        floats = by_name["10\"+Floats"]
        assert floats["OD"] == pytest.approx(0.8073851720213842)
        assert floats["MassPerUnitLength"] == pytest.approx(0.3592915903022746)
        assert floats["OuterContactDiameter"] == pytest.approx(1.2)
        assert floats["Cd"][0] == '10"+Floats drag'

    def test_lazy_wave_preserves_two_riser_line_variants(self, lazy_case: dict[str, dict]) -> None:
        line_names = {line.get("Name") for line in _generic(lazy_case, "Lines")}
        assert {"10\" Lazy Wave Distributed", "10\" Lazy Wave Discrete"}.issubset(line_names)
        _assert_line_type_references_resolve(lazy_case)

    def test_lazy_wave_environment_and_current_profile_survive(self, lazy_case: dict[str, dict]) -> None:
        env = _environment(lazy_case)
        assert env["SeabedOriginDepth"] == pytest.approx(100.0)
        assert env["WaveTrains"][0]["WaveType"] == "Dean stream"
        assert env["WaveTrains"][0]["WaveHeight"] == pytest.approx(6.0)
        assert env["WaveTrains"][0]["WaveDirection"] == pytest.approx(90.0)
        assert env["CurrentDepth, CurrentFactor, CurrentRotation"] == [
            [0.0, 1.0, 0],
            [70.0, 0.9, 0],
            [100.0, 0.3, 0],
        ]


class TestSteepWaveRiserSemanticProof:
    def test_steep_wave_preserves_bsr_and_steel_root_line_types(self, steep_case: dict[str, dict]) -> None:
        by_name = {lt.get("Name"): lt for lt in _generic(steep_case, "LineTypes")}
        expected = {
            "10\" flexible",
            "10\"+Floats",
            "BSR Cone",
            "Steel Root",
            "Topside BSR Properties",
            "Seabed BSR Properties",
        }
        assert expected.issubset(by_name)
        assert by_name["Steel Root"]["OD"] == pytest.approx(0.5)
        assert by_name["Steel Root"]["MassPerUnitLength"] == pytest.approx(0.60359026954339)
        assert by_name["BSR Cone"]["Category"] == "Homogeneous pipe"

    def test_steep_wave_preserves_line_family_and_contact_data(self, steep_case: dict[str, dict]) -> None:
        line_names = {line.get("Name") for line in _generic(steep_case, "Lines")}
        assert {"10\" Steep Wave1", "TopEnd BSR", "Seabed End BSR", "10\" Steep Wave"}.issubset(
            line_names
        )
        assert _generic(steep_case, "LineContactData"), "Steep-wave contact data not emitted"
        _assert_line_type_references_resolve(steep_case)

    def test_steep_wave_environment_and_simulation_survive(self, steep_case: dict[str, dict]) -> None:
        env = _environment(steep_case)
        params = steep_case["_parameters"]
        assert env["SeabedOriginDepth"] == pytest.approx(100.0)
        assert env["WaveTrains"][0]["WaveHeight"] == pytest.approx(3.5)
        assert env["WaveTrains"][0]["WavePeriod"] == pytest.approx(17.0)
        assert env["RefCurrentSpeed"] == pytest.approx(0.5)
        assert env["RefCurrentDirection"] == pytest.approx(75.0)
        assert params["stage_durations"] == [17, 85]
        assert params["time_step"] == pytest.approx(0.1)
