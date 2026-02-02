"""Tests for yaml_utils module."""

from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.yaml_utils import (
    OrcaFlexDumper,
    orcaflex_dump,
    orcaflex_load,
)


class TestOrcaFlexDumper:
    """Test OrcaFlexDumper YAML representations."""

    def test_none_represented_as_tilde(self):
        data = {'key': None}
        result = yaml.dump(data, Dumper=OrcaFlexDumper)
        assert 'key: ~' in result

    def test_true_represented_as_yes(self):
        data = {'enabled': True}
        result = yaml.dump(data, Dumper=OrcaFlexDumper)
        assert 'enabled: Yes' in result

    def test_false_represented_as_no(self):
        data = {'enabled': False}
        result = yaml.dump(data, Dumper=OrcaFlexDumper)
        assert 'enabled: No' in result

    def test_strings_preserved(self):
        data = {'name': 'Test Model'}
        result = yaml.dump(data, Dumper=OrcaFlexDumper)
        assert 'name: Test Model' in result

    def test_numbers_preserved(self):
        data = {'depth': 100.5}
        result = yaml.dump(data, Dumper=OrcaFlexDumper)
        assert 'depth: 100.5' in result


class TestOrcaFlexDump:
    """Test orcaflex_dump convenience function."""

    def test_creates_file(self, tmp_path: Path):
        data = {'General': {'UnitsSystem': 'SI'}}
        out = tmp_path / 'test.yml'
        orcaflex_dump(data, out)
        assert out.exists()
        loaded = yaml.safe_load(out.read_text())
        assert loaded == data

    def test_creates_parent_directories(self, tmp_path: Path):
        data = {'General': {'UnitsSystem': 'SI'}}
        out = tmp_path / 'sub' / 'dir' / 'test.yml'
        orcaflex_dump(data, out)
        assert out.exists()

    def test_with_header(self, tmp_path: Path):
        data = {'General': {'UnitsSystem': 'SI'}}
        header = '%YAML 1.1\n# Type: Model'
        out = tmp_path / 'test.yml'
        orcaflex_dump(data, out, header=header)
        text = out.read_text()
        assert text.startswith('%YAML 1.1\n')
        assert '# Type: Model' in text
        assert '---\n' in text

    def test_header_with_separator(self, tmp_path: Path):
        data = {'key': 'value'}
        header = '%YAML 1.1\n---'
        out = tmp_path / 'test.yml'
        orcaflex_dump(data, out, header=header)
        text = out.read_text()
        # Should not have double ---
        assert text.count('---') == 1

    def test_none_values_as_tilde(self, tmp_path: Path):
        data = {'LogStartTime': None, 'StartTime': None}
        out = tmp_path / 'test.yml'
        orcaflex_dump(data, out)
        text = out.read_text()
        assert 'LogStartTime: ~' in text

    def test_bool_values_as_yes_no(self, tmp_path: Path):
        data = {'WholeSystemStaticsEnabled': True, 'UseVariableTimeStep': False}
        out = tmp_path / 'test.yml'
        orcaflex_dump(data, out)
        text = out.read_text()
        assert 'WholeSystemStaticsEnabled: Yes' in text
        assert 'UseVariableTimeStep: No' in text

    def test_sort_keys_false(self, tmp_path: Path):
        data = {'Zebra': 1, 'Apple': 2, 'Mango': 3}
        out = tmp_path / 'test.yml'
        orcaflex_dump(data, out)
        text = out.read_text()
        lines = [l for l in text.strip().split('\n') if ':' in l]
        assert lines[0].startswith('Zebra')
        assert lines[1].startswith('Apple')
        assert lines[2].startswith('Mango')


class TestOrcaFlexLoad:
    """Test orcaflex_load convenience function."""

    def test_load_simple_file(self, tmp_path: Path):
        content = "General:\n  UnitsSystem: SI\n"
        f = tmp_path / 'test.yml'
        f.write_text(content)
        data, headers = orcaflex_load(f)
        assert data == {'General': {'UnitsSystem': 'SI'}}
        assert headers == []

    def test_load_with_yaml_header(self, tmp_path: Path):
        content = "%YAML 1.1\n# Type: Model\n---\nGeneral:\n  UnitsSystem: SI\n"
        f = tmp_path / 'test.yml'
        f.write_text(content)
        data, headers = orcaflex_load(f)
        assert data == {'General': {'UnitsSystem': 'SI'}}
        assert '%YAML 1.1' in headers
        assert '# Type: Model' in headers
        assert '---' in headers

    def test_load_preserves_header_comments(self, tmp_path: Path):
        content = (
            "%YAML 1.1\n"
            "# Program: OrcaFlex 11.5e\n"
            "# File: test.yml\n"
            "---\n"
            "General:\n"
            "  UnitsSystem: SI\n"
        )
        f = tmp_path / 'test.yml'
        f.write_text(content)
        data, headers = orcaflex_load(f)
        assert len(headers) == 4
        assert '# Program: OrcaFlex 11.5e' in headers

    def test_load_empty_file(self, tmp_path: Path):
        f = tmp_path / 'empty.yml'
        f.write_text('')
        data, headers = orcaflex_load(f)
        assert data == {}

    def test_load_real_a01_file(self, a01_single_file: Path):
        """Test loading a real OrcaFlex file."""
        data, headers = orcaflex_load(a01_single_file)
        assert isinstance(data, dict)
        assert 'General' in data
        assert 'Environment' in data
        assert any('%YAML' in h for h in headers)

    def test_roundtrip_dump_load(self, tmp_path: Path):
        """Verify dump -> load round-trip preserves data."""
        original = {
            'General': {
                'UnitsSystem': 'SI',
                'LogStartTime': None,
                'WholeSystemStaticsEnabled': True,
                'StageDuration': [7, 35],
            }
        }
        f = tmp_path / 'roundtrip.yml'
        orcaflex_dump(original, f, header='%YAML 1.1\n# Test\n---')
        loaded, headers = orcaflex_load(f)
        assert loaded['General']['UnitsSystem'] == 'SI'
        assert loaded['General']['LogStartTime'] is None
        # Note: Yes/No are loaded back as booleans by safe_load
        assert loaded['General']['WholeSystemStaticsEnabled'] is True
        assert loaded['General']['StageDuration'] == [7, 35]
