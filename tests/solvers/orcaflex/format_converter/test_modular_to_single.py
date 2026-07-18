"""Tests for modular_to_single converter."""

from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.format_converter.modular_to_single import (
    ModularToSingleConverter,
)


class TestModularToSingleConverter:
    """Test modular -> single conversion."""

    def test_convert_creates_output_file(self, tmp_path: Path):
        """Conversion creates a single output YAML file."""
        modular_dir = tmp_path / "modular"
        includes_dir = modular_dir / "includes"
        includes_dir.mkdir(parents=True)

        with open(includes_dir / "01_general.yml", "w") as f:
            yaml.dump({"General": {"UnitsSystem": "SI"}}, f)
        with open(includes_dir / "03_environment.yml", "w") as f:
            yaml.dump({"Environment": {"WaterDepth": 100}}, f)

        master = modular_dir / "master.yml"
        master.write_text(
            "- includefile: includes/01_general.yml\n"
            "- includefile: includes/03_environment.yml\n"
        )

        output = tmp_path / "output.yml"
        converter = ModularToSingleConverter()
        report = converter.convert(source=master, target=output)

        assert report.success
        assert output.exists()

    def test_merged_output_contains_all_sections(self, tmp_path: Path):
        """Output contains merged content from all include files."""
        modular_dir = tmp_path / "modular"
        includes_dir = modular_dir / "includes"
        includes_dir.mkdir(parents=True)

        with open(includes_dir / "01_general.yml", "w") as f:
            yaml.dump({"General": {"UnitsSystem": "SI", "TimeStep": 0.1}}, f)
        with open(includes_dir / "03_environment.yml", "w") as f:
            yaml.dump(
                {"Environment": {"WaterDepth": 100, "Density": 1.025}}, f
            )

        master = modular_dir / "master.yml"
        master.write_text(
            "- includefile: includes/01_general.yml\n"
            "- includefile: includes/03_environment.yml\n"
        )

        output = tmp_path / "output.yml"
        converter = ModularToSingleConverter()
        converter.convert(source=master, target=output)

        with open(output) as f:
            data = yaml.safe_load(f)

        assert "General" in data
        assert data["General"]["UnitsSystem"] == "SI"
        assert "Environment" in data
        assert data["Environment"]["WaterDepth"] == 100

    def test_deep_merge_dicts(self):
        """Deep merge recursively merges nested dicts."""
        base = {"A": {"x": 1, "y": 2}}
        override = {"A": {"y": 3, "z": 4}}
        result = ModularToSingleConverter._deep_merge(base, override)
        assert result == {"A": {"x": 1, "y": 3, "z": 4}}

    def test_deep_merge_lists_concatenate(self):
        """Deep merge concatenates lists."""
        base = {"items": [1, 2]}
        override = {"items": [3, 4]}
        result = ModularToSingleConverter._deep_merge(base, override)
        assert result == {"items": [1, 2, 3, 4]}

    def test_deep_merge_override_non_dict(self):
        """Non-dict values are replaced by override."""
        base = {"key": "old"}
        override = {"key": "new"}
        result = ModularToSingleConverter._deep_merge(base, override)
        assert result == {"key": "new"}

    def test_deep_merge_adds_new_keys(self):
        """New keys from override are added."""
        base = {"a": 1}
        override = {"b": 2}
        result = ModularToSingleConverter._deep_merge(base, override)
        assert result == {"a": 1, "b": 2}

    def test_missing_include_warns(self, tmp_path: Path):
        """Missing include files generate warnings."""
        modular_dir = tmp_path / "modular"
        modular_dir.mkdir()

        master = modular_dir / "master.yml"
        master.write_text("- includefile: includes/nonexistent.yml\n")

        output = tmp_path / "output.yml"
        converter = ModularToSingleConverter()
        report = converter.convert(source=master, target=output)

        assert report.success  # Still succeeds with warnings
        assert any("not found" in w for w in report.warnings)

    def test_convert_real_a01_modular(
        self, a01_master_file: Path, tmp_path: Path
    ):
        """Test conversion of real A01 modular files."""
        output = tmp_path / "a01_single.yml"
        converter = ModularToSingleConverter()
        report = converter.convert(source=a01_master_file, target=output)

        assert report.success
        assert output.exists()

        with open(output) as f:
            data = yaml.safe_load(f)

        assert "General" in data
        assert "Environment" in data

    def test_convert_from_directory(self, tmp_path: Path):
        """Can pass directory instead of master.yml path."""
        modular_dir = tmp_path / "modular"
        includes_dir = modular_dir / "includes"
        includes_dir.mkdir(parents=True)

        with open(includes_dir / "01_general.yml", "w") as f:
            yaml.dump({"General": {"UnitsSystem": "SI"}}, f)

        master = modular_dir / "master.yml"
        master.write_text("- includefile: includes/01_general.yml\n")

        output = tmp_path / "output.yml"
        converter = ModularToSingleConverter()
        report = converter.convert(source=modular_dir, target=output)

        assert report.success

    def test_merge_to_dict(self, tmp_path: Path):
        """merge_to_dict returns dict without writing file."""
        modular_dir = tmp_path / "modular"
        includes_dir = modular_dir / "includes"
        includes_dir.mkdir(parents=True)

        with open(includes_dir / "01_general.yml", "w") as f:
            yaml.dump({"General": {"UnitsSystem": "SI"}}, f)

        master = modular_dir / "master.yml"
        master.write_text("- includefile: includes/01_general.yml\n")

        converter = ModularToSingleConverter()
        data = converter.merge_to_dict(master)

        assert data == {"General": {"UnitsSystem": "SI"}}

    def test_report_format(self, tmp_path: Path):
        """ConversionReport has correct metadata."""
        modular_dir = tmp_path / "modular"
        includes_dir = modular_dir / "includes"
        includes_dir.mkdir(parents=True)

        with open(includes_dir / "01_general.yml", "w") as f:
            yaml.dump({"General": {"UnitsSystem": "SI"}}, f)

        master = modular_dir / "master.yml"
        master.write_text("- includefile: includes/01_general.yml\n")

        output = tmp_path / "output.yml"
        converter = ModularToSingleConverter()
        report = converter.convert(source=master, target=output)

        assert report.source_format == "modular"
        assert report.target_format == "single"

    def test_header_preserved(self, tmp_path: Path):
        """YAML header from master.yml is preserved in output."""
        modular_dir = tmp_path / "modular"
        includes_dir = modular_dir / "includes"
        includes_dir.mkdir(parents=True)

        with open(includes_dir / "01_general.yml", "w") as f:
            yaml.dump({"General": {"UnitsSystem": "SI"}}, f)

        master = modular_dir / "master.yml"
        master.write_text(
            "%YAML 1.1\n"
            "# Type: Model\n"
            "# Program: OrcaFlex 11.5e\n"
            "---\n"
            "- includefile: includes/01_general.yml\n"
        )

        output = tmp_path / "output.yml"
        converter = ModularToSingleConverter()
        converter.convert(source=master, target=output)

        text = output.read_text()
        assert "%YAML 1.1" in text
        assert "# Program: OrcaFlex 11.5e" in text

    def test_missing_master_returns_failure(self, tmp_path: Path):
        """Missing master.yml returns failure report."""
        output = tmp_path / "output.yml"
        converter = ModularToSingleConverter()
        report = converter.convert(
            source=tmp_path / "nonexistent" / "master.yml", target=output
        )

        assert not report.success
        assert any("not found" in w.lower() for w in report.warnings)

    def test_no_source_raises_error(self):
        """No source path raises ValueError."""
        converter = ModularToSingleConverter()
        with pytest.raises(ValueError, match="No source path"):
            converter.convert()

    def test_dict_master_preserves_content_and_section_scope(
        self, tmp_path: Path
    ):
        """Dict-format master keeps its own sections and scopes includes.

        Regression: master content was dropped entirely (merged started
        empty) and section-scoped include contents were merged at the TOP
        level instead of inside their section.
        """
        modular_dir = tmp_path / "modular"
        includes_dir = modular_dir / "includes"
        includes_dir.mkdir(parents=True)

        # Fragment with bare Environment properties (section context)
        with open(includes_dir / "03_environment.yml", "w") as f:
            yaml.dump({"WaterDepth": 100, "RefCurrentSpeed": 0.5}, f)

        master = modular_dir / "master.yml"
        master.write_text(
            "General:\n"
            "  StageDuration:\n"
            "  - 8\n"
            "  - 16\n"
            "Environment:\n"
            "  includefile: includes/03_environment.yml\n"
        )

        output = tmp_path / "output.yml"
        converter = ModularToSingleConverter()
        report = converter.convert(source=master, target=output)
        assert report.success

        with open(output) as f:
            data = yaml.safe_load(f)

        # Master's own General section is preserved
        assert data["General"]["StageDuration"] == [8, 16]
        # Include content lands INSIDE Environment, not at top level
        assert data["Environment"]["WaterDepth"] == 100
        assert data["Environment"]["RefCurrentSpeed"] == 0.5
        assert "WaterDepth" not in data
        assert "RefCurrentSpeed" not in data

    def test_dict_master_section_include_wrapped_fragment(
        self, tmp_path: Path
    ):
        """Section-scoped include tolerates an already-wrapped fragment."""
        modular_dir = tmp_path / "modular"
        includes_dir = modular_dir / "includes"
        includes_dir.mkdir(parents=True)

        with open(includes_dir / "03_environment.yml", "w") as f:
            yaml.dump({"Environment": {"WaterDepth": 250}}, f)

        master = modular_dir / "master.yml"
        master.write_text(
            "Environment:\n"
            "  includefile: includes/03_environment.yml\n"
        )

        data = ModularToSingleConverter().merge_to_dict(master)
        assert data == {"Environment": {"WaterDepth": 250}}

    def test_dict_master_missing_section_include_warns(self, tmp_path: Path):
        """Missing section-scoped include produces a warning."""
        modular_dir = tmp_path / "modular"
        modular_dir.mkdir()

        master = modular_dir / "master.yml"
        master.write_text(
            "General:\n"
            "  StageDuration: [8, 16]\n"
            "Environment:\n"
            "  includefile: includes/nonexistent.yml\n"
        )

        output = tmp_path / "output.yml"
        report = ModularToSingleConverter().convert(
            source=master, target=output
        )
        assert report.success
        assert any("not found" in w for w in report.warnings)

        with open(output) as f:
            data = yaml.safe_load(f)
        # Master content still preserved
        assert data["General"]["StageDuration"] == [8, 16]
