"""Tests for single_to_modular converter."""

from pathlib import Path

import pytest
import yaml

from digitalmodel.modules.orcaflex.format_converter.single_to_modular import (
    SingleToModularConverter,
)


class TestSingleToModularConverter:
    """Test single -> modular conversion."""

    def test_convert_creates_directory_structure(
        self, tmp_path: Path, sample_single_data: dict
    ):
        """Conversion creates master.yml, includes/, inputs/."""
        source = tmp_path / "input.yml"
        with open(source, "w") as f:
            yaml.dump(sample_single_data, f)

        output = tmp_path / "output"
        converter = SingleToModularConverter(source, output)
        report = converter.convert()

        assert report.success
        assert (output / "master.yml").exists()
        assert (output / "includes").is_dir()
        assert (output / "inputs").is_dir()

    def test_convert_splits_sections_to_files(
        self, tmp_path: Path, sample_single_data: dict
    ):
        """Each OrcaFlex section gets its own include file."""
        source = tmp_path / "input.yml"
        with open(source, "w") as f:
            yaml.dump(sample_single_data, f)

        output = tmp_path / "output"
        converter = SingleToModularConverter(source, output)
        converter.convert()

        includes = output / "includes"
        assert (includes / "01_general.yml").exists()
        assert (includes / "03_environment.yml").exists()
        assert (includes / "05_line_types.yml").exists()

    def test_include_file_contains_section_data(
        self, tmp_path: Path, sample_single_data: dict
    ):
        """Include files contain the correct section data."""
        source = tmp_path / "input.yml"
        with open(source, "w") as f:
            yaml.dump(sample_single_data, f)

        output = tmp_path / "output"
        converter = SingleToModularConverter(source, output)
        converter.convert()

        with open(output / "includes" / "01_general.yml") as f:
            general = yaml.safe_load(f)

        assert "General" in general
        assert general["General"]["UnitsSystem"] == "SI"

    def test_master_file_has_include_directives(
        self, tmp_path: Path, sample_single_data: dict
    ):
        """Master.yml contains includefile directives."""
        source = tmp_path / "input.yml"
        with open(source, "w") as f:
            yaml.dump(sample_single_data, f)

        output = tmp_path / "output"
        converter = SingleToModularConverter(source, output)
        converter.convert()

        master_text = (output / "master.yml").read_text()
        assert "- includefile: includes/01_general.yml" in master_text
        assert "- includefile: includes/03_environment.yml" in master_text

    def test_parameters_extracted(
        self, tmp_path: Path, sample_single_data: dict
    ):
        """Parameters.yml gets created with extracted values."""
        source = tmp_path / "input.yml"
        with open(source, "w") as f:
            yaml.dump(sample_single_data, f)

        output = tmp_path / "output"
        converter = SingleToModularConverter(source, output)
        converter.convert()

        params_path = output / "inputs" / "parameters.yml"
        assert params_path.exists()
        with open(params_path) as f:
            params = yaml.safe_load(f)
        assert "hs" in params
        assert params["hs"] == 6

    def test_convert_real_a01_file(self, a01_single_file: Path, tmp_path: Path):
        """Test conversion of real A01 OrcaFlex file."""
        output = tmp_path / "a01_modular"
        converter = SingleToModularConverter(a01_single_file, output)
        report = converter.convert()

        assert report.success
        assert (output / "master.yml").exists()
        assert (output / "includes" / "01_general.yml").exists()
        assert (output / "includes" / "03_environment.yml").exists()

    def test_report_format(self, tmp_path: Path, sample_single_data: dict):
        """ConversionReport has correct metadata."""
        source = tmp_path / "input.yml"
        with open(source, "w") as f:
            yaml.dump(sample_single_data, f)

        output = tmp_path / "output"
        converter = SingleToModularConverter(source, output)
        report = converter.convert()

        assert report.source_format == "single"
        assert report.target_format == "modular"
        assert report.source_path == source
        assert report.target_path == output

    def test_empty_file_returns_failure(self, tmp_path: Path):
        """Empty source file returns failure report."""
        source = tmp_path / "empty.yml"
        source.write_text("")

        output = tmp_path / "output"
        converter = SingleToModularConverter(source, output)
        report = converter.convert()

        assert not report.success

    def test_header_preserved_in_master(self, tmp_path: Path):
        """Original YAML header is preserved in master.yml."""
        source = tmp_path / "input.yml"
        source.write_text(
            "%YAML 1.1\n"
            "# Type: Model\n"
            "# Program: OrcaFlex 11.5e\n"
            "---\n"
            "General:\n"
            "  UnitsSystem: SI\n"
        )

        output = tmp_path / "output"
        converter = SingleToModularConverter(source, output)
        converter.convert()

        master_text = (output / "master.yml").read_text()
        assert "%YAML 1.1" in master_text
        assert "# Program: OrcaFlex 11.5e" in master_text

    def test_multiple_sections_same_file(self, tmp_path: Path):
        """Sections mapping to the same file are grouped together."""
        data = {
            "6DBuoys": [{"Name": "Buoy1"}],
            "Buoys": [{"Name": "Buoy2"}],
            "3DBuoys": [{"Name": "Buoy3"}],
        }
        source = tmp_path / "input.yml"
        with open(source, "w") as f:
            yaml.dump(data, f)

        output = tmp_path / "output"
        converter = SingleToModularConverter(source, output)
        converter.convert()

        # All three should be in 08_buoys.yml
        includes = output / "includes"
        buoys_path = includes / "08_buoys.yml"
        assert buoys_path.exists()

        with open(buoys_path) as f:
            buoys_data = yaml.safe_load(f)

        assert "6DBuoys" in buoys_data
        assert "Buoys" in buoys_data
        assert "3DBuoys" in buoys_data

    def test_unknown_sections_go_to_general(self, tmp_path: Path):
        """Sections not in SECTION_MAPPING are placed in 01_general.yml."""
        data = {
            "General": {"UnitsSystem": "SI"},
            "CustomSection": {"key": "value"},
        }
        source = tmp_path / "input.yml"
        with open(source, "w") as f:
            yaml.dump(data, f)

        output = tmp_path / "output"
        converter = SingleToModularConverter(source, output)
        converter.convert()

        with open(output / "includes" / "01_general.yml") as f:
            general = yaml.safe_load(f)

        assert "General" in general
        assert "CustomSection" in general
