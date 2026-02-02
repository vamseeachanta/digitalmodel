"""Tests for SpecConverter and CLI convert-spec / validate-spec commands (WRK-061).

TDD tests: written before implementation.

Uses real fixture YAML files and verifies that the SpecConverter
delegates correctly to AQWABackend and OrcaWaveBackend, producing
the expected output files or directories.
"""

from __future__ import annotations

from pathlib import Path

import pytest
from click.testing import CliRunner


# ---------------------------------------------------------------------------
# Fixtures (shared)
# ---------------------------------------------------------------------------

FIXTURES_DIR = Path(__file__).parent / "fixtures"


@pytest.fixture
def ship_spec_path() -> Path:
    """Single-body ship spec fixture."""
    return FIXTURES_DIR / "spec_ship_raos.yml"


@pytest.fixture
def fpso_spec_path() -> Path:
    """Multi-body FPSO+turret spec fixture."""
    return FIXTURES_DIR / "spec_fpso_turret.yml"


# ---------------------------------------------------------------------------
# SpecConverter unit tests
# ---------------------------------------------------------------------------


class TestSpecConverterConvertAQWA:
    """SpecConverter.convert() for AQWA backend."""

    def test_convert_aqwa_single_produces_dat_file(
        self, ship_spec_path: Path, tmp_path: Path
    ) -> None:
        from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

        converter = SpecConverter(ship_spec_path)
        result = converter.convert(
            solver="aqwa", format="single", output_dir=tmp_path
        )

        assert result.exists()
        assert result.suffix == ".dat"
        content = result.read_text()
        assert len(content) > 0

    def test_convert_aqwa_modular_produces_directory(
        self, ship_spec_path: Path, tmp_path: Path
    ) -> None:
        from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

        converter = SpecConverter(ship_spec_path)
        output_dir = tmp_path / "aqwa_modular"
        result = converter.convert(
            solver="aqwa", format="modular", output_dir=output_dir
        )

        assert result.is_dir()
        dat_files = list(result.glob("*.dat"))
        assert len(dat_files) > 0


class TestSpecConverterConvertOrcaWave:
    """SpecConverter.convert() for OrcaWave backend."""

    def test_convert_orcawave_single_produces_yml_file(
        self, ship_spec_path: Path, tmp_path: Path
    ) -> None:
        from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

        converter = SpecConverter(ship_spec_path)
        result = converter.convert(
            solver="orcawave", format="single", output_dir=tmp_path
        )

        assert result.exists()
        assert result.suffix == ".yml"
        content = result.read_text()
        assert len(content) > 0

    def test_convert_orcawave_modular_produces_directory(
        self, ship_spec_path: Path, tmp_path: Path
    ) -> None:
        from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

        converter = SpecConverter(ship_spec_path)
        output_dir = tmp_path / "orcawave_modular"
        result = converter.convert(
            solver="orcawave", format="modular", output_dir=output_dir
        )

        # generate_modular returns the master.yml path
        assert result.exists()
        assert result.suffix == ".yml"
        # The directory should contain multiple section files
        yml_files = list(result.parent.glob("*.yml"))
        assert len(yml_files) > 1


class TestSpecConverterConvertAll:
    """SpecConverter.convert_all() generates both solver outputs."""

    def test_convert_all_returns_both_solvers(
        self, ship_spec_path: Path, tmp_path: Path
    ) -> None:
        from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

        converter = SpecConverter(ship_spec_path)
        results = converter.convert_all(output_dir=tmp_path)

        assert "aqwa" in results
        assert "orcawave" in results
        assert results["aqwa"].exists()
        assert results["orcawave"].exists()

    def test_convert_all_creates_solver_subdirectories(
        self, ship_spec_path: Path, tmp_path: Path
    ) -> None:
        from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

        converter = SpecConverter(ship_spec_path)
        results = converter.convert_all(output_dir=tmp_path)

        # AQWA output should be under tmp_path/aqwa/
        assert "aqwa" in str(results["aqwa"])
        # OrcaWave output should be under tmp_path/orcawave/
        assert "orcawave" in str(results["orcawave"])


class TestSpecConverterValidate:
    """SpecConverter.validate() method."""

    def test_validate_valid_spec_returns_no_issues(
        self, ship_spec_path: Path
    ) -> None:
        from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

        converter = SpecConverter(ship_spec_path)
        issues = converter.validate()

        assert isinstance(issues, list)
        assert len(issues) == 0

    def test_validate_invalid_spec_returns_issues(
        self, tmp_path: Path
    ) -> None:
        """An invalid YAML should raise during construction or return issues."""
        import yaml

        bad_spec_path = tmp_path / "bad_spec.yml"
        bad_data = {
            "version": "1.0",
            "analysis_type": "diffraction",
            # Missing vessel/bodies, environment, frequencies, wave_headings
        }
        with open(bad_spec_path, "w") as f:
            yaml.dump(bad_data, f)

        from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

        with pytest.raises(Exception):
            # Should raise a validation error on construction
            SpecConverter(bad_spec_path)


class TestSpecConverterMultiBody:
    """SpecConverter with multi-body FPSO+turret fixture."""

    def test_convert_aqwa_multibody(
        self, fpso_spec_path: Path, tmp_path: Path
    ) -> None:
        from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

        converter = SpecConverter(fpso_spec_path)
        result = converter.convert(
            solver="aqwa", format="single", output_dir=tmp_path
        )
        assert result.exists()
        content = result.read_text()
        # Multi-body spec should reference both bodies
        assert "FPSO_Hull" in content or "STRC" in content

    def test_convert_orcawave_multibody(
        self, fpso_spec_path: Path, tmp_path: Path
    ) -> None:
        from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

        converter = SpecConverter(fpso_spec_path)
        result = converter.convert(
            solver="orcawave", format="single", output_dir=tmp_path
        )
        assert result.exists()
        content = result.read_text()
        assert "FPSO_Hull" in content


# ---------------------------------------------------------------------------
# CLI tests (Click CliRunner)
# ---------------------------------------------------------------------------


class TestCLIConvertSpec:
    """CLI convert-spec command tests."""

    def test_convert_spec_aqwa_exit_0(
        self, ship_spec_path: Path, tmp_path: Path
    ) -> None:
        from digitalmodel.hydrodynamics.diffraction.cli import cli

        runner = CliRunner()
        result = runner.invoke(
            cli,
            [
                "convert-spec",
                str(ship_spec_path),
                "--solver", "aqwa",
                "--format", "single",
                "--output", str(tmp_path),
            ],
        )
        assert result.exit_code == 0, result.output

    def test_convert_spec_orcawave_exit_0(
        self, ship_spec_path: Path, tmp_path: Path
    ) -> None:
        from digitalmodel.hydrodynamics.diffraction.cli import cli

        runner = CliRunner()
        result = runner.invoke(
            cli,
            [
                "convert-spec",
                str(ship_spec_path),
                "--solver", "orcawave",
                "--format", "single",
                "--output", str(tmp_path),
            ],
        )
        assert result.exit_code == 0, result.output

    def test_convert_spec_all_generates_both(
        self, ship_spec_path: Path, tmp_path: Path
    ) -> None:
        from digitalmodel.hydrodynamics.diffraction.cli import cli

        runner = CliRunner()
        result = runner.invoke(
            cli,
            [
                "convert-spec",
                str(ship_spec_path),
                "--solver", "all",
                "--output", str(tmp_path),
            ],
        )
        assert result.exit_code == 0, result.output
        # Should mention both solvers in output
        assert "aqwa" in result.output.lower()
        assert "orcawave" in result.output.lower()

    def test_convert_spec_help(self) -> None:
        from digitalmodel.hydrodynamics.diffraction.cli import cli

        runner = CliRunner()
        result = runner.invoke(cli, ["convert-spec", "--help"])
        assert result.exit_code == 0
        assert "Convert" in result.output or "convert" in result.output


class TestCLIValidateSpec:
    """CLI validate-spec command tests."""

    def test_validate_spec_valid_exit_0(
        self, ship_spec_path: Path
    ) -> None:
        from digitalmodel.hydrodynamics.diffraction.cli import cli

        runner = CliRunner()
        result = runner.invoke(
            cli,
            ["validate-spec", str(ship_spec_path)],
        )
        assert result.exit_code == 0, result.output

    def test_validate_spec_invalid_exit_nonzero(
        self, tmp_path: Path
    ) -> None:
        import yaml

        bad_spec_path = tmp_path / "bad_spec.yml"
        bad_data = {"version": "1.0"}
        with open(bad_spec_path, "w") as f:
            yaml.dump(bad_data, f)

        from digitalmodel.hydrodynamics.diffraction.cli import cli

        runner = CliRunner()
        result = runner.invoke(
            cli,
            ["validate-spec", str(bad_spec_path)],
        )
        assert result.exit_code != 0

    def test_validate_spec_help(self) -> None:
        from digitalmodel.hydrodynamics.diffraction.cli import cli

        runner = CliRunner()
        result = runner.invoke(cli, ["validate-spec", "--help"])
        assert result.exit_code == 0
        assert "Validate" in result.output or "validate" in result.output
