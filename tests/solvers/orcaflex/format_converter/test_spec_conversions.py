"""Tests for spec conversion modules."""

from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.format_converter.spec_to_modular import (
    SpecToModularConverter,
)
from digitalmodel.solvers.orcaflex.format_converter.spec_to_single import (
    SpecToSingleConverter,
)
from digitalmodel.solvers.orcaflex.format_converter.modular_to_spec import (
    ModularToSpecConverter,
)
from digitalmodel.solvers.orcaflex.format_converter.single_to_spec import (
    SingleToSpecConverter,
)


class TestSpecToModularConverter:
    """Test spec -> modular conversion."""

    def test_convert_creates_modular_structure(self, spec_file: Path, tmp_path: Path):
        """Spec conversion creates modular directory structure."""
        output = tmp_path / "modular_output"
        converter = SpecToModularConverter()
        report = converter.convert(source=spec_file, target=output)

        assert report.success
        assert (output / "master.yml").exists()
        assert (output / "includes").is_dir()

    def test_convert_generates_includes(self, spec_file: Path, tmp_path: Path):
        """Spec conversion generates include files."""
        output = tmp_path / "modular_output"
        converter = SpecToModularConverter()
        converter.convert(source=spec_file, target=output)

        includes = list((output / "includes").glob("*.yml"))
        assert len(includes) > 0

    def test_convert_invalid_spec_returns_failure(self, tmp_path: Path):
        """Invalid spec returns failure report."""
        bad_spec = tmp_path / "bad_spec.yml"
        bad_spec.write_text("not_a_valid: spec\n")

        output = tmp_path / "output"
        converter = SpecToModularConverter()
        report = converter.convert(source=bad_spec, target=output)

        assert not report.success
        assert len(report.warnings) > 0

    def test_report_metadata(self, spec_file: Path, tmp_path: Path):
        """Report has correct format metadata."""
        output = tmp_path / "output"
        converter = SpecToModularConverter()
        report = converter.convert(source=spec_file, target=output)

        assert report.source_format == "spec"
        assert report.target_format == "modular"

    def test_no_source_raises(self):
        """ValueError when no source provided."""
        converter = SpecToModularConverter()
        with pytest.raises(ValueError, match="No source path"):
            converter.convert()

    def test_init_with_spec_path(self, spec_file: Path, tmp_path: Path):
        """Constructor-provided spec_path is used when source not passed."""
        output = tmp_path / "output"
        converter = SpecToModularConverter(spec_path=spec_file)
        report = converter.convert(target=output)

        assert report.success

    def test_supported_formats(self):
        """Returns correct format pair."""
        converter = SpecToModularConverter()
        assert converter.supported_formats() == ("spec", "modular")


class TestSpecToSingleConverter:
    """Test spec -> single conversion."""

    def test_convert_creates_single_file(self, spec_file: Path, tmp_path: Path):
        """Spec -> single creates output YAML file."""
        output = tmp_path / "output.yml"
        converter = SpecToSingleConverter()
        report = converter.convert(source=spec_file, target=output)

        assert report.success
        assert output.exists()

    def test_output_has_orcaflex_sections(self, spec_file: Path, tmp_path: Path):
        """Output single file has OrcaFlex sections."""
        output = tmp_path / "output.yml"
        converter = SpecToSingleConverter()
        converter.convert(source=spec_file, target=output)

        with open(output) as f:
            data = yaml.safe_load(f)

        assert isinstance(data, dict)
        # Should have at least General and Environment
        assert "General" in data

    def test_report_metadata(self, spec_file: Path, tmp_path: Path):
        """Report has correct format metadata."""
        output = tmp_path / "output.yml"
        converter = SpecToSingleConverter()
        report = converter.convert(source=spec_file, target=output)

        assert report.source_format == "spec"
        assert report.target_format == "single"

    def test_no_source_raises(self):
        """ValueError when no source provided."""
        converter = SpecToSingleConverter()
        with pytest.raises(ValueError, match="No source path"):
            converter.convert()

    def test_supported_formats(self):
        """Returns correct format pair."""
        converter = SpecToSingleConverter()
        assert converter.supported_formats() == ("spec", "single")

    def test_invalid_spec_propagates_failure(self, tmp_path: Path):
        """Invalid spec propagates failure from first stage."""
        bad_spec = tmp_path / "bad.yml"
        bad_spec.write_text("invalid: data\n")

        output = tmp_path / "output.yml"
        converter = SpecToSingleConverter()
        report = converter.convert(source=bad_spec, target=output)

        assert not report.success
        assert report.source_format == "spec"
        assert report.target_format == "single"


class TestModularToSpecConverter:
    """Test modular -> spec conversion (best-effort)."""

    def test_extract_environment_fields(self, a01_master_file: Path, tmp_path: Path):
        """Extracts environment fields from modular format."""
        output = tmp_path / "spec.yml"
        converter = ModularToSpecConverter()
        report = converter.convert(source=a01_master_file, target=output)

        assert report.success
        assert output.exists()

        with open(output) as f:
            spec = yaml.safe_load(f)

        assert "environment" in spec
        assert "water" in spec["environment"]
        assert "depth" in spec["environment"]["water"]

    def test_extract_simulation_fields(self, a01_master_file: Path, tmp_path: Path):
        """Extracts simulation fields from modular format."""
        output = tmp_path / "spec.yml"
        converter = ModularToSpecConverter()
        converter.convert(source=a01_master_file, target=output)

        with open(output) as f:
            spec = yaml.safe_load(f)

        assert "simulation" in spec
        assert "time_step" in spec["simulation"]

    def test_confidence_score(self, a01_master_file: Path, tmp_path: Path):
        """Reports confidence score between 0 and 1."""
        output = tmp_path / "spec.yml"
        converter = ModularToSpecConverter()
        report = converter.convert(source=a01_master_file, target=output)

        assert 0 <= report.confidence <= 1.0
        assert report.confidence > 0  # Should extract at least some fields

    def test_unmapped_sections_reported(self, a01_master_file: Path, tmp_path: Path):
        """Reports unmapped OrcaFlex sections."""
        output = tmp_path / "spec.yml"
        converter = ModularToSpecConverter()
        report = converter.convert(source=a01_master_file, target=output)

        # A01 has sections like VesselTypes, LineTypes that don't map to spec
        assert len(report.unmapped_sections) > 0

    def test_metadata_placeholder(self, a01_master_file: Path, tmp_path: Path):
        """Output includes metadata placeholder."""
        output = tmp_path / "spec.yml"
        converter = ModularToSpecConverter()
        converter.convert(source=a01_master_file, target=output)

        with open(output) as f:
            spec = yaml.safe_load(f)

        assert "metadata" in spec
        assert "name" in spec["metadata"]

    def test_confidence_header_in_file(self, a01_master_file: Path, tmp_path: Path):
        """Output file has confidence header comment."""
        output = tmp_path / "spec.yml"
        converter = ModularToSpecConverter()
        converter.convert(source=a01_master_file, target=output)

        text = output.read_text()
        assert "# Confidence:" in text
        assert "# Review before use" in text

    def test_from_directory(self, a01_modular_dir: Path, tmp_path: Path):
        """Can pass directory instead of master.yml."""
        output = tmp_path / "spec.yml"
        converter = ModularToSpecConverter()
        report = converter.convert(source=a01_modular_dir, target=output)

        assert report.success

    def test_extract_wave_fields(self, tmp_path: Path):
        """Extracts wave info from WaveTrains."""
        # Create modular structure with WaveTrains
        modular = tmp_path / "modular"
        includes = modular / "includes"
        includes.mkdir(parents=True)

        env_data = {
            "Environment": {
                "WaterDepth": 100,
                "Density": 1.025,
                "WaveTrains": [
                    {
                        "WaveHeight": 6,
                        "WavePeriod": 7,
                        "WaveDirection": 180,
                        "WaveType": "Dean stream",
                    }
                ],
            }
        }
        with open(includes / "03_environment.yml", "w") as f:
            yaml.dump(env_data, f)

        master = modular / "master.yml"
        master.write_text("- includefile: includes/03_environment.yml\n")

        output = tmp_path / "spec.yml"
        converter = ModularToSpecConverter()
        report = converter.convert(source=master, target=output)

        with open(output) as f:
            spec = yaml.safe_load(f)

        assert spec["environment"]["waves"]["height"] == 6
        assert spec["environment"]["waves"]["period"] == 7
        assert spec["environment"]["waves"]["direction"] == 180

    def test_no_source_raises(self):
        """ValueError when no source provided."""
        converter = ModularToSpecConverter()
        with pytest.raises(ValueError, match="No source path"):
            converter.convert()

    def test_supported_formats(self):
        """Returns correct format pair."""
        converter = ModularToSpecConverter()
        assert converter.supported_formats() == ("modular", "spec")

    def test_empty_modular_returns_failure(self, tmp_path: Path):
        """Empty modular files return failure report."""
        modular = tmp_path / "empty_modular"
        modular.mkdir()
        master = modular / "master.yml"
        master.write_text("# empty\n")

        output = tmp_path / "spec.yml"
        converter = ModularToSpecConverter()
        report = converter.convert(source=master, target=output)

        assert not report.success


class TestModularToSpecBoundaryReporting:
    """Issue #520: explicit boundary reporting for reverse extraction.

    All tests here are self-contained (no external fixture files).
    They verify that ModularToSpecConverter categorizes unmapped
    sections as expected vs actionable and makes the non-lossless
    nature explicit in both the report and the output file.
    """

    @staticmethod
    def _make_master(tmp_path: Path, extra_sections: dict | None = None) -> Path:
        """Create a minimal modular structure with optional extra OrcaFlex sections."""
        includes = tmp_path / "modular" / "includes"
        includes.mkdir(parents=True)
        data: dict = {
            "Environment": {
                "WaterDepth": 100,
                "Density": 1.025,
                "WaveTrains": [
                    {
                        "WaveHeight": 6,
                        "WavePeriod": 7,
                        "WaveDirection": 180,
                        "WaveType": "Airy",
                    }
                ],
            },
            "General": {
                "ImplicitConstantTimeStep": 0.1,
                "StageDuration": [7, 35],
            },
        }
        if extra_sections:
            data.update(extra_sections)
        with open(includes / "data.yml", "w") as f:
            yaml.dump(data, f)
        master = tmp_path / "modular" / "master.yml"
        master.write_text("- includefile: includes/data.yml\n")
        return master

    def test_is_best_effort_true(self, tmp_path: Path):
        """ModularToSpecConverter always marks report as best-effort."""
        master = self._make_master(tmp_path)
        report = ModularToSpecConverter().convert(
            source=master, target=tmp_path / "s.yml"
        )
        assert report.is_best_effort is True

    def test_summary_includes_best_effort_label(self, tmp_path: Path):
        """summary() labels reverse extraction as best-effort."""
        master = self._make_master(tmp_path, {"VesselTypes": [{"Name": "FPSO"}]})
        report = ModularToSpecConverter().convert(
            source=master, target=tmp_path / "s.yml"
        )
        assert "best-effort" in report.summary().lower()

    def test_expected_gaps_classified(self, tmp_path: Path):
        """VesselTypes, Vessels, Groups are expected (out-of-scope) gaps."""
        master = self._make_master(
            tmp_path,
            {
                "VesselTypes": [{"Name": "FPSO"}],
                "Vessels": [{"Name": "V1"}],
                "Groups": [{"Name": "G1"}],
            },
        )
        report = ModularToSpecConverter().convert(
            source=master, target=tmp_path / "s.yml"
        )
        assert "VesselTypes" in report.expected_gaps
        assert "Vessels" in report.expected_gaps
        assert "Groups" in report.expected_gaps
        assert "VesselTypes" not in report.actionable_gaps

    def test_actionable_gaps_classified(self, tmp_path: Path):
        """Lines and LineTypes are actionable (partial extraction possible)."""
        master = self._make_master(
            tmp_path,
            {
                "Lines": [{"Name": "Riser1", "Length": [100, 200]}],
                "LineTypes": [{"Name": "Steel", "OD": 0.3}],
            },
        )
        report = ModularToSpecConverter().convert(
            source=master, target=tmp_path / "s.yml"
        )
        assert "Lines" in report.actionable_gaps
        assert "LineTypes" in report.actionable_gaps
        assert "Lines" not in report.expected_gaps

    def test_unknown_sections_classified_as_actionable(self, tmp_path: Path):
        """Sections not in any known set default to actionable."""
        master = self._make_master(tmp_path, {"UnknownSection": {"foo": "bar"}})
        report = ModularToSpecConverter().convert(
            source=master, target=tmp_path / "s.yml"
        )
        assert "UnknownSection" in report.actionable_gaps
        assert "UnknownSection" not in report.expected_gaps

    def test_all_unmapped_are_categorized(self, tmp_path: Path):
        """Every unmapped section appears in expected_gaps or actionable_gaps."""
        master = self._make_master(
            tmp_path,
            {
                "VesselTypes": [{"Name": "FPSO"}],
                "Lines": [{"Name": "R1"}],
                "UnknownSection": {"foo": "bar"},
            },
        )
        report = ModularToSpecConverter().convert(
            source=master, target=tmp_path / "s.yml"
        )
        categorized = set(report.expected_gaps) | set(report.actionable_gaps)
        assert set(report.unmapped_sections) == categorized

    def test_file_header_not_lossless_warning(self, tmp_path: Path):
        """Output file header explicitly warns that extraction is not lossless."""
        master = self._make_master(tmp_path)
        out = tmp_path / "spec.yml"
        ModularToSpecConverter().convert(source=master, target=out)
        text = out.read_text()
        assert "not a lossless" in text.lower()


class TestSingleToSpecBoundaryReporting:
    """Issue #520: boundary reporting propagates through single2spec chain."""

    def test_is_best_effort_true(self, sample_single_data: dict, tmp_path: Path):
        """SingleToSpecConverter propagates is_best_effort=True from modular stage."""
        src = tmp_path / "model.yml"
        src.write_text(yaml.dump(sample_single_data))
        out = tmp_path / "spec.yml"
        report = SingleToSpecConverter().convert(source=src, target=out)
        assert report.is_best_effort is True


class TestSingleToSpecConverter:
    """Test single -> spec conversion."""

    def test_convert_real_a01(self, a01_single_file: Path, tmp_path: Path):
        """Extract spec from real A01 single file."""
        output = tmp_path / "spec.yml"
        converter = SingleToSpecConverter()
        report = converter.convert(source=a01_single_file, target=output)

        assert report.success
        assert output.exists()
        assert report.confidence > 0

    def test_extracted_spec_has_environment(
        self, a01_single_file: Path, tmp_path: Path
    ):
        """Extracted spec has environment data."""
        output = tmp_path / "spec.yml"
        converter = SingleToSpecConverter()
        converter.convert(source=a01_single_file, target=output)

        with open(output) as f:
            spec = yaml.safe_load(f)

        assert "environment" in spec
        assert "water" in spec["environment"]

    def test_report_metadata(self, a01_single_file: Path, tmp_path: Path):
        """Report has correct format metadata."""
        output = tmp_path / "spec.yml"
        converter = SingleToSpecConverter()
        report = converter.convert(source=a01_single_file, target=output)

        assert report.source_format == "single"
        assert report.target_format == "spec"
        assert 0 <= report.confidence <= 1.0

    def test_no_source_raises(self):
        """ValueError when no source provided."""
        converter = SingleToSpecConverter()
        with pytest.raises(ValueError, match="No source path"):
            converter.convert()

    def test_supported_formats(self):
        """Returns correct format pair."""
        converter = SingleToSpecConverter()
        assert converter.supported_formats() == ("single", "spec")
