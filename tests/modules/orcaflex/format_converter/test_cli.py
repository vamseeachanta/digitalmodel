"""Tests for the CLI module."""

import shutil
from pathlib import Path

import pytest

from digitalmodel.modules.orcaflex.format_converter.cli import create_parser, main


class TestCLIDetect:
    """Test detect subcommand."""

    def test_detect_single_file(self, a01_single_file: Path):
        """Detect single format from CLI."""
        result = main(["detect", str(a01_single_file)])
        assert result == 0

    def test_detect_modular_dir(self, a01_modular_dir: Path):
        """Detect modular format from CLI."""
        result = main(["detect", str(a01_modular_dir)])
        assert result == 0

    def test_detect_spec_file(self, spec_file: Path):
        """Detect spec format from CLI."""
        result = main(["detect", str(spec_file)])
        assert result == 0

    def test_detect_nonexistent(self, tmp_path: Path):
        """Detect returns error for nonexistent file."""
        result = main(["detect", str(tmp_path / "nope.yml")])
        assert result == 1


class TestCLISingle2Modular:
    """Test single2modular subcommand."""

    def test_convert(self, a01_single_file: Path, tmp_path: Path):
        """Convert single to modular via CLI."""
        output = tmp_path / "modular_out"
        result = main(["single2modular", str(a01_single_file), "-o", str(output)])
        assert result == 0
        assert (output / "master.yml").exists()

    def test_no_output_uses_default(self, a01_single_file: Path, tmp_path: Path):
        """Without -o, uses default output directory."""
        src = tmp_path / "test.yml"
        shutil.copy(a01_single_file, src)
        result = main(["single2modular", str(src)])
        assert result == 0
        expected_dir = tmp_path / "test"
        assert expected_dir.exists()


class TestCLIModular2Single:
    """Test modular2single subcommand."""

    def test_convert(self, a01_master_file: Path, tmp_path: Path):
        """Convert modular to single via CLI."""
        output = tmp_path / "single.yml"
        result = main(["modular2single", str(a01_master_file), "-o", str(output)])
        assert result == 0
        assert output.exists()


class TestCLISpec2Modular:
    """Test spec2modular subcommand."""

    def test_convert(self, spec_file: Path, tmp_path: Path):
        """Convert spec to modular via CLI."""
        output = tmp_path / "spec_modular"
        result = main(["spec2modular", str(spec_file), "-o", str(output)])
        assert result == 0
        assert (output / "master.yml").exists()


class TestCLISingle2Spec:
    """Test single2spec subcommand."""

    def test_convert(self, a01_single_file: Path, tmp_path: Path):
        """Convert single to spec via CLI."""
        output = tmp_path / "extracted_spec.yml"
        result = main(["single2spec", str(a01_single_file), "-o", str(output)])
        assert result == 0
        assert output.exists()


class TestCLIAuto:
    """Test auto subcommand."""

    def test_auto_single_to_modular(self, a01_single_file: Path, tmp_path: Path):
        """Auto-detect and convert single to modular."""
        output = tmp_path / "auto_modular"
        result = main(
            ["auto", str(a01_single_file), "--to", "modular", "-o", str(output)]
        )
        assert result == 0

    def test_auto_modular_to_single(self, a01_master_file: Path, tmp_path: Path):
        """Auto-detect and convert modular to single."""
        output = tmp_path / "auto_single.yml"
        result = main(
            ["auto", str(a01_master_file), "--to", "single", "-o", str(output)]
        )
        assert result == 0


class TestCLIParser:
    """Test parser construction."""

    def test_parser_creates_subcommands(self):
        """Parser has all expected subcommands."""
        parser = create_parser()
        args = parser.parse_args(["detect", "/some/path"])
        assert args.command == "detect"

    def test_no_command_returns_1(self):
        """No subcommand returns exit code 1."""
        result = main([])
        assert result == 1
