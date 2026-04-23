"""Semantic roundtrip regression tests for OrcaWave strict YAML (#521, #2457)."""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.benchmark_input_comparison import (
    build_semantic_equivalence_html,
)
from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend
from digitalmodel.hydrodynamics.diffraction.reverse_parsers import OrcaWaveInputParser

FIXTURES_DIR = Path(__file__).parent / "fixtures"
L03_SPEC_PATH = (
    Path(__file__).resolve().parents[3]
    / "docs"
    / "domains"
    / "orcawave"
    / "L03_ship_benchmark"
    / "spec.yml"
)


def _load_ship_spec() -> DiffractionSpec:
    return DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_ship_raos.yml")


def _load_multibody_spec() -> DiffractionSpec:
    return DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_fpso_turret.yml")


def _load_semisub_spec() -> DiffractionSpec:
    return DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_semisub.yml")


def _load_l03_ship_benchmark_spec() -> DiffractionSpec:
    return DiffractionSpec.from_yaml(L03_SPEC_PATH)


def _generate_orcawave_yml(spec: DiffractionSpec, tmp_path: Path) -> Path:
    backend = OrcaWaveBackend()
    return backend.generate_single(spec, tmp_path)


class TestOrcaWaveSemanticRoundTripSingleBody:
    def test_preserves_frequency_values(self, tmp_path: Path) -> None:
        original = _load_ship_spec()
        yml_path = _generate_orcawave_yml(original, tmp_path)
        parsed = OrcaWaveInputParser().parse(yml_path)

        assert sorted(parsed.frequencies.to_frequencies_rad_s()) == pytest.approx(
            sorted(original.frequencies.to_frequencies_rad_s()), rel=1e-4
        )

    def test_preserves_heading_values(self, tmp_path: Path) -> None:
        original = _load_ship_spec()
        yml_path = _generate_orcawave_yml(original, tmp_path)
        parsed = OrcaWaveInputParser().parse(yml_path)

        assert parsed.wave_headings.to_heading_list() == pytest.approx(
            original.wave_headings.to_heading_list(), abs=1e-6
        )

    def test_preserves_centre_of_gravity(self, tmp_path: Path) -> None:
        original = _load_semisub_spec()
        yml_path = _generate_orcawave_yml(original, tmp_path)
        parsed = OrcaWaveInputParser().parse(yml_path)

        assert parsed.vessel is not None
        assert parsed.vessel.inertia.centre_of_gravity == pytest.approx(
            original.vessel.inertia.centre_of_gravity, rel=1e-6
        )

    def test_preserves_inertia_tensor(self, tmp_path: Path) -> None:
        """Ship spec uses radii_of_gyration; forward backend derives tensor.

        Round-trip should produce inertia tensor I_ii = mass * radius_i^2.
        """
        original = _load_ship_spec()
        yml_path = _generate_orcawave_yml(original, tmp_path)
        parsed = OrcaWaveInputParser().parse(yml_path)

        assert parsed.vessel is not None
        assert parsed.vessel.inertia.inertia_tensor is not None

        mass = original.vessel.inertia.mass
        radii = original.vessel.inertia.radii_of_gyration
        expected = {
            "Ixx": mass * radii[0] ** 2,
            "Iyy": mass * radii[1] ** 2,
            "Izz": mass * radii[2] ** 2,
        }
        rt = parsed.vessel.inertia.inertia_tensor
        for key in ("Ixx", "Iyy", "Izz"):
            assert rt[key] == pytest.approx(expected[key], rel=1e-2)

    def test_preserves_remove_irregular_frequencies(self, tmp_path: Path) -> None:
        original = _load_ship_spec()
        yml_path = _generate_orcawave_yml(original, tmp_path)
        parsed = OrcaWaveInputParser().parse(yml_path)

        assert parsed.solver_options.remove_irregular_frequencies is original.solver_options.remove_irregular_frequencies


class TestOrcaWaveSemanticRoundTripMultiBody:
    def test_preserves_body_count_and_names(self, tmp_path: Path) -> None:
        original = _load_multibody_spec()
        yml_path = _generate_orcawave_yml(original, tmp_path)
        parsed = OrcaWaveInputParser().parse(yml_path)

        assert parsed.bodies is not None
        assert len(parsed.bodies) == len(original.bodies)
        assert [b.vessel.name for b in parsed.bodies] == [b.vessel.name for b in original.bodies]

    def test_preserves_fixed_dofs_on_turret(self, tmp_path: Path) -> None:
        original = _load_multibody_spec()
        yml_path = _generate_orcawave_yml(original, tmp_path)
        parsed = OrcaWaveInputParser().parse(yml_path)

        assert parsed.bodies is not None
        turret = next(body for body in parsed.bodies if body.vessel.name == "Turret")
        original_turret = next(body for body in original.bodies if body.vessel.name == "Turret")
        assert sorted(turret.vessel.fixed_dofs or []) == sorted(original_turret.vessel.fixed_dofs or [])

    def test_preserves_connection_parent(self, tmp_path: Path) -> None:
        original = _load_multibody_spec()
        yml_path = _generate_orcawave_yml(original, tmp_path)
        parsed = OrcaWaveInputParser().parse(yml_path)

        assert parsed.bodies is not None
        turret = next(body for body in parsed.bodies if body.vessel.name == "Turret")
        assert turret.connection_parent == "FPSO_Hull"


class TestOrcaWaveL03ShipBenchmarkRoundTrip:
    """Flagship L03 ship benchmark proof for #2457.

    The L03 fixture is the richest single-body OrcaWave benchmark in the
    repository: ship mesh, finite water depth, explicit inertia tensor, roll
    damping, dense heading/period grid, and QTF-enabled solve intent.  These
    tests lock the engineering-significant canonical-spec contract without
    claiming byte-for-byte OrcaWave YAML identity.
    """

    def test_l03_spec_file_exists_and_is_ship_benchmark(self) -> None:
        original = _load_l03_ship_benchmark_spec()

        assert L03_SPEC_PATH.exists()
        assert original.vessel.name == "Body1"
        assert original.vessel.type == "ship"
        assert "L03" in original.metadata.tags
        assert original.environment.water_depth == pytest.approx(500.0)

    def test_l03_roundtrip_preserves_frequency_and_heading_grid(self, tmp_path: Path) -> None:
        original = _load_l03_ship_benchmark_spec()
        yml_path = _generate_orcawave_yml(original, tmp_path)
        parsed = OrcaWaveInputParser().parse(yml_path)

        assert parsed.frequencies.to_periods_s() == pytest.approx(
            original.frequencies.to_periods_s(), rel=1e-6
        )
        assert parsed.wave_headings.to_heading_list() == pytest.approx(
            original.wave_headings.to_heading_list(), abs=1e-9
        )

    def test_l03_roundtrip_preserves_inertia_cog_and_roll_damping(self, tmp_path: Path) -> None:
        original = _load_l03_ship_benchmark_spec()
        yml_path = _generate_orcawave_yml(original, tmp_path)
        parsed = OrcaWaveInputParser().parse(yml_path)

        assert parsed.vessel is not None
        assert parsed.vessel.inertia.centre_of_gravity == pytest.approx(
            original.vessel.inertia.centre_of_gravity, rel=1e-9
        )
        for key, expected in original.vessel.inertia.inertia_tensor.items():
            assert parsed.vessel.inertia.inertia_tensor[key] == pytest.approx(
                expected, rel=1e-9
            )
        assert parsed.vessel.external_damping[3][3] == pytest.approx(36010.0)

    def test_l03_generated_native_yaml_preserves_qtf_solve_intent(self, tmp_path: Path) -> None:
        """QTF min/max are native backend settings, not currently reverse-parsed.

        The forward native YAML must still carry the analysis-significant QTF
        solve controls from the canonical L03 spec.  This keeps the #2457 claim
        boundary explicit: native forward fidelity for QTF intent, roundtrip for
        the core engineering grid/inertia/damping fields.
        """
        original = _load_l03_ship_benchmark_spec()
        yml_path = _generate_orcawave_yml(original, tmp_path)
        native = yaml.safe_load(yml_path.read_text(encoding="utf-8"))
        parsed = OrcaWaveInputParser().parse(yml_path)

        assert original.solver_options.qtf_calculation is True
        assert native["SolveType"] == "Full QTF calculation"
        assert native["QuadraticLoadPressureIntegration"] is True
        assert native["QuadraticLoadControlSurface"] is False
        assert native["QTFFrequencyTypes"] == "Sum frequencies"
        assert native["QTFMinPeriodOrFrequency"] == pytest.approx(2.0, rel=1e-6)
        assert native["QTFMaxPeriodOrFrequency"] == pytest.approx(10.0, rel=1e-6)
        assert parsed.solver_options.qtf_calculation is True
        assert parsed.solver_options.remove_irregular_frequencies is True


class TestOrcaWaveReverseParserHardening:
    def test_missing_bodies_raises_instead_of_placeholder(self, tmp_path: Path) -> None:
        yml_path = tmp_path / "empty.yml"
        yml_path.write_text(
            "WaterDepth: 100\nWaterDensity: 1.025\nPeriodOrFrequency: [10.0]\nWaveHeading: [0.0]\nBodies: []\n",
            encoding="utf-8",
        )

        with pytest.raises(ValueError, match="No bodies found"):
            OrcaWaveInputParser().parse(yml_path)

    def test_missing_frequencies_raise_instead_of_fallback(self, tmp_path: Path) -> None:
        yml_path = tmp_path / "missing-frequencies.yml"
        yml_path.write_text(
            "WaterDepth: 100\nWaterDensity: 1.025\nWaveHeading: [0.0]\nBodies:\n  - BodyName: Hull\n    BodyMeshFileName: hull.gdf\n    BodyMeshFormat: Wamit gdf\n    BodyMeshSymmetry: None\n    BodyMeshLengthUnits: m\n    BodyMass: 1000.0\n    BodyCentreOfMass: [0.0, 0.0, 0.0]\n",
            encoding="utf-8",
        )

        with pytest.raises(ValueError, match="No frequency data found"):
            OrcaWaveInputParser().parse(yml_path)

    def test_free_floating_mode_roundtrip_is_preserved_if_present(self, tmp_path: Path) -> None:
        original = _load_ship_spec()
        data = original.model_dump(mode="json", exclude_none=True)
        data["vessel"]["inertia"] = {
            "mode": "free_floating",
            "mass": 1.0,
            "centre_of_gravity": [0.0, 0.0, 0.0],
            "radii_of_gyration": [15.0, 50.0, 50.0],
            "cog_z": -2.5,
        }
        free_spec = DiffractionSpec.model_validate(data)
        yml_path = _generate_orcawave_yml(free_spec, tmp_path)
        parsed = OrcaWaveInputParser().parse(yml_path)

        assert parsed.vessel is not None
        assert parsed.vessel.inertia.mode == "free_floating"
        assert parsed.vessel.inertia.radii_of_gyration == pytest.approx([15.0, 50.0, 50.0], rel=1e-6)
        assert parsed.vessel.inertia.cog_z == pytest.approx(-2.5, rel=1e-6)


class TestSemanticTaxonomyPresentation:
    def test_semantic_html_supports_explicit_orcawave_taxonomy(self) -> None:
        html = build_semantic_equivalence_html(
            ["OrcaWave (.owd)", "OrcaWave (spec.yml)"],
            {
                "OrcaWave (.owd)": {
                    "_semantic_equivalence": {
                        "match_count": 10,
                        "cosmetic_count": 1,
                        "convention_count": 1,
                        "significant_count": 1,
                        "diffs": [
                            {
                                "key": "WaterDensity",
                                "level": "convention",
                                "category": "representation_normalization_only",
                                "owd": "1.025 t/m^3",
                                "spec": "1025 kg/m^3",
                            },
                            {
                                "key": "OutputPanelPressures",
                                "level": "cosmetic",
                                "category": "output_only",
                                "owd": "Yes",
                                "spec": "No",
                            },
                            {
                                "key": "ComputationStrategy",
                                "level": "cosmetic",
                                "category": "known_non_configurable_in_spec",
                                "owd": "PanelBased",
                                "spec": "default",
                            },
                            {
                                "key": "DivideNonPlanarPanels",
                                "level": "significant",
                                "category": "solver_mode_significant",
                                "owd": "Yes",
                                "spec": "No",
                            },
                        ],
                    }
                },
                "OrcaWave (spec.yml)": {},
            },
        )

        # Category labels from diffs should appear in HTML
        assert "solver_mode_significant" in html
        assert "representation_normalization_only" in html
        assert "output_only" in html
        assert "known_non_configurable_in_spec" in html
