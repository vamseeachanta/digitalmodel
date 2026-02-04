"""Tests for CampaignGenerator and CLI campaign subcommand."""
from __future__ import annotations

import argparse
import sys
from pathlib import Path
from unittest.mock import patch

import pytest
import yaml


def _make_campaign_data() -> dict:
    """Create minimal valid campaign data for testing."""
    return {
        "base": {
            "metadata": {
                "name": "test_pipe",
                "description": "Test pipeline",
                "structure": "pipeline",
                "operation": "installation/floating",
                "project": "test",
            },
            "environment": {
                "water": {"depth": 10, "density": 1.025},
                "seabed": {
                    "slope": 0.5,
                    "stiffness": {"normal": 100, "shear": 10},
                },
                "waves": {
                    "type": "jonswap",
                    "height": 1.5,
                    "period": 6,
                    "direction": 180,
                },
                "current": {"speed": 0.5, "direction": 270},
                "wind": {"speed": 5, "direction": 270},
            },
            "pipeline": {
                "name": "Test Line",
                "material": "X65",
                "dimensions": {"outer_diameter": 0.5, "wall_thickness": 0.02},
                "coatings": {
                    "corrosion": {"thickness": 0.004, "density": 0.95},
                    "weight": [
                        {"name": "CWC", "thickness": 0.1, "density": 3.0},
                    ],
                },
                "segments": [
                    {"type": "X65+CWC", "length": 5000, "segment_length": 10},
                ],
            },
            "equipment": {
                "tugs": {
                    "count": 3,
                    "spacing": 500,
                    "first_position": [500, -20, 0],
                    "properties": {"mass": 30, "volume": 100},
                },
                "buoyancy_modules": {
                    "spacing": 4,
                    "properties": {"volume": 4.91},
                },
            },
            "simulation": {"time_step": 0.1, "stages": [8, 16]},
        },
        "campaign": {
            "water_depths": [10, 20],
            "environments": [
                {
                    "name": "calm",
                    "waves": {
                        "type": "airy",
                        "height": 0.5,
                        "period": 5,
                        "direction": 180,
                    },
                    "current": {"speed": 0.3, "direction": 270},
                    "wind": {"speed": 3, "direction": 270},
                },
                {
                    "name": "storm",
                    "waves": {
                        "type": "jonswap",
                        "height": 3.0,
                        "period": 8,
                        "direction": 180,
                    },
                    "current": {"speed": 1.0, "direction": 270},
                    "wind": {"speed": 10, "direction": 270},
                },
            ],
        },
        "output_naming": "{base_name}_wd{water_depth}m_{environment}",
    }


def _write_campaign_yaml(path: Path) -> Path:
    """Write a minimal valid campaign YAML for testing."""
    campaign_file = path / "campaign.yml"
    campaign_file.write_text(yaml.dump(_make_campaign_data(), default_flow_style=False))
    return campaign_file


class TestCampaignGeneratorLoadsValidYaml:
    """Test 1: CampaignGenerator loads and parses a valid YAML file."""

    def test_campaign_generator_loads_valid_yaml(self, tmp_path):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignGenerator,
            CampaignSpec,
        )

        campaign_file = _write_campaign_yaml(tmp_path)
        gen = CampaignGenerator(campaign_file)

        assert isinstance(gen.spec, CampaignSpec)
        assert gen.spec.base.metadata.name == "test_pipe"
        assert len(gen.spec.campaign.water_depths) == 2
        assert len(gen.spec.campaign.environments) == 2


class TestCampaignGeneratorPreview:
    """Test 2: Preview returns correct number of combinations."""

    def test_campaign_generator_preview(self, tmp_path):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignGenerator,
        )

        campaign_file = _write_campaign_yaml(tmp_path)
        gen = CampaignGenerator(campaign_file)

        combos = gen.preview()

        # 2 water_depths x 2 environments = 4 combinations
        assert len(combos) == 4
        assert all("water_depth" in c for c in combos)
        assert all("environment" in c for c in combos)


class TestCampaignGeneratorValidate:
    """Test 3: Validate catches duplicate environment names."""

    def test_campaign_generator_validate_duplicate_names(self, tmp_path):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignGenerator,
        )

        data = _make_campaign_data()
        # Make both environments have the same name
        data["campaign"]["environments"][1]["name"] = "calm"
        campaign_file = tmp_path / "campaign_dup.yml"
        campaign_file.write_text(yaml.dump(data, default_flow_style=False))

        gen = CampaignGenerator(campaign_file)
        warnings = gen.validate()

        assert any("Duplicate environment names" in w for w in warnings)


class TestCampaignGeneratorGenerate:
    """Tests 4-7: Generate creates directories and returns correct results."""

    def test_campaign_generator_generate_creates_directories(self, tmp_path):
        """Test 4: Generate creates one directory per combination."""
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignGenerator,
        )

        campaign_file = _write_campaign_yaml(tmp_path)
        gen = CampaignGenerator(campaign_file)
        output_dir = tmp_path / "output"

        result = gen.generate(output_dir=output_dir)

        # 2 depths x 2 environments = 4 runs
        assert result.run_count == 4
        assert len(result.run_dirs) == 4
        for run_dir in result.run_dirs:
            assert run_dir.exists()
            assert run_dir.is_dir()

    def test_campaign_generator_generate_force_overwrites(self, tmp_path):
        """Test 5: With force=True, existing dirs are overwritten."""
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignGenerator,
        )

        campaign_file = _write_campaign_yaml(tmp_path)
        gen = CampaignGenerator(campaign_file)
        output_dir = tmp_path / "output"

        # First generation
        result1 = gen.generate(output_dir=output_dir)
        assert result1.run_count == 4

        # Second generation without force: existing dirs skipped
        result2 = gen.generate(output_dir=output_dir, force=False)
        assert result2.skipped_count == 4
        assert result2.run_count == 0

        # Third generation with force: all regenerated
        result3 = gen.generate(output_dir=output_dir, force=True)
        assert result3.run_count == 4
        assert result3.skipped_count == 0

    def test_campaign_generator_generate_resume_skips_existing(self, tmp_path):
        """Test 6: With resume=True, skips dirs with master.yml."""
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignGenerator,
        )

        campaign_file = _write_campaign_yaml(tmp_path)
        gen = CampaignGenerator(campaign_file)
        output_dir = tmp_path / "output"

        # First generate all
        result1 = gen.generate(output_dir=output_dir)
        assert result1.run_count == 4

        # Resume should skip all (master.yml exists in each)
        result2 = gen.generate(output_dir=output_dir, resume=True)
        assert result2.skipped_count == 4
        assert result2.run_count == 0

    def test_campaign_generator_generate_returns_result(self, tmp_path):
        """Test 7: CampaignResult has correct counts and summary."""
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignGenerator,
            CampaignResult,
        )

        campaign_file = _write_campaign_yaml(tmp_path)
        gen = CampaignGenerator(campaign_file)
        output_dir = tmp_path / "output"

        result = gen.generate(output_dir=output_dir)

        assert isinstance(result, CampaignResult)
        assert result.run_count == 4
        assert result.skipped_count == 0
        assert result.errors == []
        assert "water_depth" in result.matrix_summary
        assert "environment" in result.matrix_summary
        assert sorted(result.matrix_summary["water_depth"]) == [10.0, 20.0]
        assert sorted(result.matrix_summary["environment"]) == ["calm", "storm"]


class TestCampaignGeneratorMaxRuns:
    """Test 8: max_runs limits generation count."""

    def test_campaign_generator_generate_max_runs(self, tmp_path):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignGenerator,
        )

        data = _make_campaign_data()
        data["max_runs"] = 2
        campaign_file = tmp_path / "campaign_max.yml"
        campaign_file.write_text(yaml.dump(data, default_flow_style=False))

        gen = CampaignGenerator(campaign_file)
        output_dir = tmp_path / "output"

        result = gen.generate(output_dir=output_dir)

        # max_runs=2, so only 2 of the 4 combinations should be generated
        assert result.run_count == 2
        assert len(result.run_dirs) == 2


class TestCLICampaignPreview:
    """Test 9: CLI preview shows matrix without generating."""

    def test_cli_campaign_preview(self, tmp_path, capsys):
        from digitalmodel.solvers.orcaflex.modular_generator.cli import (
            cmd_campaign,
            create_parser,
        )

        campaign_file = _write_campaign_yaml(tmp_path)
        parser = create_parser()
        args = parser.parse_args([
            "campaign", str(campaign_file), "--preview",
        ])

        exit_code = cmd_campaign(args)
        captured = capsys.readouterr()

        assert exit_code == 0
        assert "test_pipe" in captured.out
        assert "2 depths" in captured.out
        assert "2 environments" in captured.out
        assert "4 runs" in captured.out


class TestCLICampaignGenerate:
    """Test 10: CLI generate creates output directories."""

    def test_cli_campaign_generate(self, tmp_path, capsys):
        from digitalmodel.solvers.orcaflex.modular_generator.cli import (
            cmd_campaign,
            create_parser,
        )

        campaign_file = _write_campaign_yaml(tmp_path)
        output_dir = tmp_path / "cli_output"
        parser = create_parser()
        args = parser.parse_args([
            "campaign", str(campaign_file), "--output", str(output_dir),
        ])

        exit_code = cmd_campaign(args)
        captured = capsys.readouterr()

        assert exit_code == 0
        assert "Generated: 4" in captured.out
        assert output_dir.exists()

    def test_cli_campaign_missing_file(self, tmp_path, capsys):
        from digitalmodel.solvers.orcaflex.modular_generator.cli import (
            cmd_campaign,
            create_parser,
        )

        parser = create_parser()
        args = parser.parse_args([
            "campaign", str(tmp_path / "nonexistent.yml"), "--preview",
        ])

        exit_code = cmd_campaign(args)

        assert exit_code == 1

    def test_cli_campaign_no_output_no_preview(self, tmp_path, capsys):
        from digitalmodel.solvers.orcaflex.modular_generator.cli import (
            cmd_campaign,
            create_parser,
        )

        campaign_file = _write_campaign_yaml(tmp_path)
        parser = create_parser()
        args = parser.parse_args(["campaign", str(campaign_file)])

        exit_code = cmd_campaign(args)

        assert exit_code == 1
