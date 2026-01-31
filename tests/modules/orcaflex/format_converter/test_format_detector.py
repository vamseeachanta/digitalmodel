"""Tests for format_detector module."""

from pathlib import Path

import pytest
import yaml

from digitalmodel.modules.orcaflex.format_converter.format_detector import (
    FormatType,
    detect_format,
)


class TestFormatDetector:
    """Test format auto-detection."""

    def test_detect_modular_directory(self, a01_modular_dir: Path):
        """Detect modular format from directory with master.yml + includes/."""
        assert detect_format(a01_modular_dir) == FormatType.MODULAR

    def test_detect_modular_master_file(self, a01_master_file: Path):
        """Detect modular format from master.yml file."""
        assert detect_format(a01_master_file) == FormatType.MODULAR

    def test_detect_single_file(self, a01_single_file: Path):
        """Detect single format from raw OrcaFlex YAML."""
        assert detect_format(a01_single_file) == FormatType.SINGLE

    def test_detect_spec_file(self, spec_file: Path):
        """Detect spec format from spec.yml."""
        assert detect_format(spec_file) == FormatType.SPEC

    def test_detect_single_from_dict(self, tmp_path: Path):
        """Detect single format from file with OrcaFlex sections."""
        data = {'General': {'UnitsSystem': 'SI'}, 'Environment': {'WaterDepth': 100}}
        f = tmp_path / 'model.yml'
        with open(f, 'w') as fh:
            yaml.dump(data, fh)
        assert detect_format(f) == FormatType.SINGLE

    def test_detect_spec_from_dict(self, tmp_path: Path):
        """Detect spec format from file with metadata + pipeline."""
        data = {
            'metadata': {'name': 'test'},
            'pipeline': {'name': 'pipe'},
            'environment': {'water': {'depth': 100}},
        }
        f = tmp_path / 'spec.yml'
        with open(f, 'w') as fh:
            yaml.dump(data, fh)
        assert detect_format(f) == FormatType.SPEC

    def test_detect_modular_from_list(self, tmp_path: Path):
        """Detect modular format from master.yml with includefile list."""
        data = [
            {'includefile': 'includes/01_general.yml'},
            {'includefile': 'includes/03_environment.yml'},
        ]
        f = tmp_path / 'master.yml'
        with open(f, 'w') as fh:
            yaml.dump(data, fh)
        assert detect_format(f) == FormatType.MODULAR

    def test_detect_modular_directory_structure(self, tmp_path: Path):
        """Detect modular format from directory with master.yml + includes/."""
        (tmp_path / 'master.yml').write_text('- includefile: includes/01_general.yml\n')
        (tmp_path / 'includes').mkdir()
        (tmp_path / 'includes' / '01_general.yml').write_text('General: {}\n')
        assert detect_format(tmp_path) == FormatType.MODULAR

    def test_error_on_nonexistent_path(self, tmp_path: Path):
        with pytest.raises(ValueError, match="does not exist"):
            detect_format(tmp_path / 'nonexistent.yml')

    def test_error_on_empty_file(self, tmp_path: Path):
        f = tmp_path / 'empty.yml'
        f.write_text('')
        with pytest.raises(ValueError, match="empty or invalid"):
            detect_format(f)

    def test_error_on_unknown_dict_format(self, tmp_path: Path):
        data = {'foo': 'bar', 'baz': 123}
        f = tmp_path / 'unknown.yml'
        with open(f, 'w') as fh:
            yaml.dump(data, fh)
        with pytest.raises(ValueError, match="Cannot determine format"):
            detect_format(f)

    def test_error_on_directory_without_master(self, tmp_path: Path):
        (tmp_path / 'includes').mkdir()
        with pytest.raises(ValueError, match="does not contain master.yml"):
            detect_format(tmp_path)

    def test_format_type_values(self):
        """Verify enum string values."""
        assert FormatType.SPEC == "spec"
        assert FormatType.MODULAR == "modular"
        assert FormatType.SINGLE == "single"
