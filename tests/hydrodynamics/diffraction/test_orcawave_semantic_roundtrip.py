"""Semantic roundtrip regression tests for OrcaWave strict YAML (#521, #2457)."""

from __future__ import annotations

from math import pi
from pathlib import Path

import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.benchmark_input_comparison import (
    build_semantic_equivalence_html,
)
from digitalmodel.hydrodynamics.diffraction.input_schemas import (
    DiffractionSpec,
    FrequencyInputType,
)
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


@pytest.fixture
def synthetic_spec() -> DiffractionSpec:
    """Small offline input for value-level roundtrip coverage (#524).

    All inputs are synthetic assumptions: supplied mass 12500 kg, density
    1000 kg/m^3, infinite depth, zero centre of gravity, unit gyration radii
    in metres, and headings 0, 90, 225 degrees. The mesh is a reference only;
    the text backend does not need mesh contents for symmetry 'none'.

    One cycle is 2*pi radians, so omega = 2*pi/T. Choosing T = 8, 2, 4 s
    gives the unsorted, nonuniform explicit grid pi/4, pi, pi/2 rad/s.
    These integer periods are exact at the export precision.
    """
    return DiffractionSpec.model_validate(
        {
            "vessel": {
                "name": "body",
                "geometry": {"mesh_file": "mesh.gdf", "mesh_format": "gdf"},
                "inertia": {
                    "mode": "explicit",
                    "mass": 12500.0,
                    "centre_of_gravity": [0.0, 0.0, 0.0],
                    "radii_of_gyration": [1.0, 1.0, 1.0],
                },
            },
            "environment": {"water_depth": "infinite", "water_density": 1000.0},
            "frequencies": {
                "input_type": "frequency",
                "values": [pi / 4, pi, pi / 2],
            },
            "wave_headings": {"values": [0.0, 90.0, 225.0]},
        }
    )


class TestOrcaWaveSemanticRoundTripSingleBody:
    def test_roundtrip_preserves_supplied_mass_and_density(
        self, synthetic_spec: DiffractionSpec, tmp_path: Path
    ) -> None:
        """Check each unit conversion before and after parsing (#524)."""
        yml_path = _generate_orcawave_yml(synthetic_spec, tmp_path)
        native = yaml.safe_load(yml_path.read_text(encoding="utf-8"))
        parsed = OrcaWaveInputParser().parse(yml_path)

        # Definition: 1 tonne = 1000 kg. Hand calculations, independently:
        # mass: 12500 / 1000 = 12.5 t; 12.5 * 1000 = 12500 kg.
        # density: 1000 / 1000 = 1 t/m^3; 1 * 1000 = 1000 kg/m^3.
        # Checking native values prevents reciprocal conversion errors hiding.
        assert native["UnitsSystem"] == "SI"
        assert native["Bodies"][0]["BodyMass"] == 12.5
        assert native["WaterDensity"] == 1.0
        assert parsed.vessel is not None
        assert parsed.vessel.inertia.mode == "explicit"
        assert parsed.vessel.inertia.mass == 12500.0
        assert parsed.environment.water_density == 1000.0

    def test_roundtrip_preserves_infinite_depth_and_explicit_grid_values(
        self, synthetic_spec: DiffractionSpec, tmp_path: Path
    ) -> None:
        """Assert all grid values in sorted-period canonical order (#524)."""
        yml_path = _generate_orcawave_yml(synthetic_spec, tmp_path)
        native = yaml.safe_load(yml_path.read_text(encoding="utf-8"))
        parsed = OrcaWaveInputParser().parse(yml_path)

        # _periods_from_spec documents ascending periods. From T = 2*pi/omega:
        # pi/4 -> 8 s, pi -> 2 s, pi/2 -> 4 s; sorting gives [2, 4, 8] s.
        # In that order, omega = [2*pi/2, 2*pi/4, 2*pi/8]
        # = [pi, pi/2, pi/4] rad/s, not the original sequence order.
        # Integer periods and binary divisions of pi permit exact assertions.
        expected_periods = [2.0, 4.0, 8.0]
        expected_frequencies = [pi, pi / 2, pi / 4]
        # Headings are the synthetic degree inputs, with no unit conversion.
        expected_headings = [0.0, 90.0, 225.0]

        assert native["WaterDepth"] == "Infinity"
        assert parsed.environment.water_depth == "infinite"
        assert native["WavesReferredToBy"] == "period (s)"
        assert native["PeriodOrFrequency"] == expected_periods
        assert native["WaveHeading"] == expected_headings
        assert parsed.frequencies.input_type == FrequencyInputType.FREQUENCY
        assert parsed.frequencies.range is None
        assert parsed.frequencies.values == expected_frequencies
        assert parsed.frequencies.to_frequencies_rad_s() == expected_frequencies
        assert parsed.frequencies.to_periods_s() == expected_periods
        assert parsed.wave_headings.to_heading_list() == expected_headings

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
        # Native example uses kN; canonical SI uses N (Data: Model / Units).
        assert parsed.vessel.external_damping[3][3] == pytest.approx(36010000.0)

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
