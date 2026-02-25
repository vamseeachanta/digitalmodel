"""Integration tests for campaign generation end-to-end workflows.

Tests cover floating and S-lay campaign generation, backward compatibility
with existing spec files, and consistency between preview and generation.
"""
from __future__ import annotations

from pathlib import Path

import pytest
import yaml


# ------------------------------------------------------------------
# Paths to existing spec files used in backward-compatibility tests
# ------------------------------------------------------------------
FLOATING_SPEC = Path(
    "D:/workspace-hub/digitalmodel/docs/domains/orcaflex/pipeline"
    "/installation/floating/30in_pipeline/spec.yml"
)
SLAY_SPEC = Path(
    "D:/workspace-hub/digitalmodel/docs/domains/orcaflex/pipeline"
    "/installation/s-lay/SB-SA/spec.yml"
)


# ------------------------------------------------------------------
# Helper: write a minimal floating campaign YAML
# ------------------------------------------------------------------
def _write_floating_campaign(path: Path) -> Path:
    """Write a minimal floating campaign YAML and return its path."""
    data = {
        "base": {
            "metadata": {
                "name": "floating_test",
                "description": "Integration test floating",
                "structure": "pipeline",
                "operation": "installation/floating",
                "project": "test",
            },
            "environment": {
                "water": {"depth": 8, "density": 1.025},
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
                "current": {
                    "speed": 1.0,
                    "direction": 270,
                    "profile": [[0, 0.46], [6, 0.39], [15, 0.33]],
                },
                "wind": {"speed": 5, "direction": 270},
            },
            "pipeline": {
                "name": "Test Line",
                "material": "X65",
                "dimensions": {
                    "outer_diameter": 0.762,
                    "wall_thickness": 0.0254,
                },
                "coatings": {
                    "corrosion": {"thickness": 0.004, "density": 0.95},
                    "weight": [
                        {
                            "name": "CWC120",
                            "thickness": 0.12,
                            "density": 2.978,
                        },
                    ],
                },
                "segments": [
                    {
                        "type": "X65+CWC120",
                        "length": 5000,
                        "segment_length": 5,
                    },
                ],
            },
            "equipment": {
                "tugs": {
                    "count": 3,
                    "spacing": 800,
                    "first_position": [700, -20, 0],
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
            "water_depths": [8, 15],
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
            ],
        },
        "output_naming": "{base_name}_wd{water_depth}m_{environment}",
    }

    campaign_file = path / "floating_campaign.yml"
    campaign_file.write_text(yaml.dump(data, default_flow_style=False))
    return campaign_file


# ------------------------------------------------------------------
# Helper: write a minimal S-lay campaign YAML
# ------------------------------------------------------------------
def _write_slay_campaign(path: Path) -> Path:
    """Write a minimal S-lay campaign YAML and return its path."""
    data = {
        "base": {
            "metadata": {
                "name": "slay_test",
                "description": "Integration test S-lay",
                "structure": "pipeline",
                "operation": "installation/s-lay",
                "project": "test",
            },
            "environment": {
                "water": {"depth": 20, "density": 1.025},
                "seabed": {
                    "slope": 0,
                    "stiffness": {"normal": 100, "shear": 10},
                },
                "waves": {
                    "type": "airy",
                    "height": 0,
                    "period": 8,
                    "direction": 0,
                },
                "current": {
                    "speed": 1.0,
                    "direction": 0,
                    "profile": [[0, 1.0]],
                },
                "wind": {"speed": 0, "direction": 0},
            },
            "pipeline": {
                "name": "SB-SA Pipeline",
                "material": "X60",
                "dimensions": {
                    "outer_diameter": 0.2731,
                    "wall_thickness": 0.0143,
                },
                "coatings": {
                    "corrosion": {"thickness": 0.005, "density": 1.3},
                    "weight": [
                        {
                            "name": "CWC40",
                            "thickness": 0.04,
                            "density": 3.04,
                        },
                    ],
                },
                "segments": [
                    {
                        "type": "X60+coating+CWC40",
                        "length": 1000,
                        "segment_length": 2.0,
                    },
                ],
            },
            "equipment": {
                "vessel": {
                    "name": "Eclipse",
                    "properties": {
                        "loa": 130,
                        "beam": 28,
                        "depth": 9.5,
                        "draft": 4.0,
                        "gmt": 14.7,
                        "cog": [68.6, 0.0, 9.6],
                        "gyration_radii": [10.3, 37.8, 37.8],
                    },
                    "mooring": {"drums": 8, "winch_capacity": 735.75},
                    "position": [0, 0, 0],
                },
                "stinger": {
                    "radius": 150,
                    "sections": [{"length": 40, "bend_radius": 150}],
                    "rollers": [],
                    "origin_position": [-30, 0, 7],
                    "origin_orientation": [180, 0, 0],
                },
                "tensioner": {
                    "capacity_kn": 490,
                    "tension_value": 585.4,
                },
            },
            "simulation": {"time_step": 0.1, "stages": [8, 200]},
        },
        "campaign": {
            "water_depths": [20, 30],
            "tensions": [500, 600],
        },
        "output_naming": "{base_name}_wd{water_depth}m_t{tension}kN",
    }

    campaign_file = path / "slay_campaign.yml"
    campaign_file.write_text(yaml.dump(data, default_flow_style=False))
    return campaign_file


# ==================================================================
# Tests
# ==================================================================


class TestFloatingCampaignGeneratesAllRuns:
    """Floating campaign produces correct number of run directories."""

    def test_floating_campaign_generates_all_runs(self, tmp_path):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignGenerator,
        )

        campaign_file = _write_floating_campaign(tmp_path)
        gen = CampaignGenerator(campaign_file)
        output_dir = tmp_path / "output"

        result = gen.generate(output_dir=output_dir)

        # 2 water_depths x 1 environment = 2 runs
        assert result.run_count == 2
        assert len(result.run_dirs) == 2
        assert result.errors == []

        for run_dir in result.run_dirs:
            assert run_dir.exists(), f"Run dir missing: {run_dir}"
            includes_dir = run_dir / "includes"
            assert includes_dir.exists(), f"includes/ missing in {run_dir}"
            master_file = run_dir / "master.yml"
            assert master_file.exists(), f"master.yml missing in {run_dir}"


class TestFloatingCampaignWaterDepthInEnvironment:
    """Water depth in generated environment YAML matches campaign value."""

    def test_floating_campaign_water_depth_in_environment(self, tmp_path):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignGenerator,
        )

        campaign_file = _write_floating_campaign(tmp_path)
        gen = CampaignGenerator(campaign_file)
        output_dir = tmp_path / "output"

        result = gen.generate(output_dir=output_dir)
        assert result.run_count == 2

        expected_depths = {8.0, 15.0}
        found_depths = set()

        for run_dir in result.run_dirs:
            env_file = run_dir / "includes" / "03_environment.yml"
            assert env_file.exists(), f"03_environment.yml missing in {run_dir}"

            env_data = yaml.safe_load(env_file.read_text())
            depth = env_data["Environment"]["SeabedOriginDepth"]
            found_depths.add(float(depth))

        assert found_depths == expected_depths


class TestFloatingCampaignCurrentProfileScaled:
    """Current profile depths scale proportionally with water depth.

    Uses a depth-only campaign (no environment overrides) so the base
    current profile is preserved across all runs and only scaled by the
    water-depth ratio.
    """

    @staticmethod
    def _write_depth_only_campaign(path: Path) -> Path:
        """Campaign that varies only water depths (no env override)."""
        data = {
            "base": {
                "metadata": {
                    "name": "profile_scale_test",
                    "description": "Current profile scaling test",
                    "structure": "pipeline",
                    "operation": "installation/floating",
                    "project": "test",
                },
                "environment": {
                    "water": {"depth": 8, "density": 1.025},
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
                    "current": {
                        "speed": 1.0,
                        "direction": 270,
                        "profile": [[0, 0.46], [6, 0.39], [15, 0.33]],
                    },
                    "wind": {"speed": 5, "direction": 270},
                },
                "pipeline": {
                    "name": "Test Line",
                    "material": "X65",
                    "dimensions": {
                        "outer_diameter": 0.762,
                        "wall_thickness": 0.0254,
                    },
                    "coatings": {
                        "corrosion": {"thickness": 0.004, "density": 0.95},
                        "weight": [
                            {
                                "name": "CWC120",
                                "thickness": 0.12,
                                "density": 2.978,
                            },
                        ],
                    },
                    "segments": [
                        {
                            "type": "X65+CWC120",
                            "length": 5000,
                            "segment_length": 5,
                        },
                    ],
                },
                "equipment": {
                    "tugs": {
                        "count": 3,
                        "spacing": 800,
                        "first_position": [700, -20, 0],
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
                "water_depths": [8, 15],
            },
            "output_naming": "{base_name}_wd{water_depth}m",
        }
        campaign_file = path / "depth_only_campaign.yml"
        campaign_file.write_text(yaml.dump(data, default_flow_style=False))
        return campaign_file

    def test_floating_campaign_current_profile_scaled(self, tmp_path):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignGenerator,
        )

        campaign_file = self._write_depth_only_campaign(tmp_path)
        gen = CampaignGenerator(campaign_file)
        output_dir = tmp_path / "output"

        result = gen.generate(output_dir=output_dir)
        assert result.run_count == 2

        # Verify scaling via the campaign spec directly for precision.
        # Base depth is 8m, profile depths are [0, 6, 15].
        # For water_depth=15: scale = 15/8 = 1.875
        #   new profile depths = [0, 11.25, 28.125]
        # For water_depth=8: scale = 1.0 (unchanged)
        for name, spec in gen.spec.generate_run_specs():
            wd = spec.environment.water.depth
            profile = spec.environment.current.profile

            assert len(profile) == 3, (
                f"Expected 3-point profile for wd={wd}, got {len(profile)}"
            )

            if wd == 8.0:
                # Unchanged: scale=1.0
                assert profile[0][0] == pytest.approx(0.0)
                assert profile[1][0] == pytest.approx(6.0)
                assert profile[2][0] == pytest.approx(15.0)
            elif wd == 15.0:
                # Scaled: scale=15/8=1.875
                scale = 15.0 / 8.0
                assert profile[0][0] == pytest.approx(0.0 * scale)
                assert profile[1][0] == pytest.approx(6.0 * scale)
                assert profile[2][0] == pytest.approx(15.0 * scale)

                # Speed factors remain unchanged
                assert profile[0][1] == pytest.approx(0.46)
                assert profile[1][1] == pytest.approx(0.39)
                assert profile[2][1] == pytest.approx(0.33)


class TestSlayCampaignGeneratesCorrectly:
    """S-lay campaign with tension variations produces valid output."""

    def test_slay_campaign_generates_correctly(self, tmp_path):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignGenerator,
        )

        campaign_file = _write_slay_campaign(tmp_path)
        gen = CampaignGenerator(campaign_file)
        output_dir = tmp_path / "output"

        result = gen.generate(output_dir=output_dir)

        # 2 water_depths x 2 tensions = 4 runs
        assert result.run_count == 4
        assert len(result.run_dirs) == 4
        assert result.errors == []

        for run_dir in result.run_dirs:
            assert run_dir.exists()
            master_file = run_dir / "master.yml"
            assert master_file.exists()
            includes_dir = run_dir / "includes"
            assert includes_dir.exists()

            # S-lay models should have vessel-related sections
            master_content = master_file.read_text()
            assert "includefile:" in master_content

            # Verify at least one include file exists
            include_files = list(includes_dir.glob("*.yml"))
            assert len(include_files) > 0


class TestBackwardCompatSingleRunFloating:
    """Existing floating spec generates correct output via ModularModelGenerator."""

    @pytest.mark.skipif(
        not FLOATING_SPEC.exists(),
        reason=f"Floating spec not found: {FLOATING_SPEC}",
    )
    def test_backward_compat_single_run_floating(self, tmp_path):
        from digitalmodel.solvers.orcaflex.modular_generator import (
            ModularModelGenerator,
        )

        gen = ModularModelGenerator(FLOATING_SPEC)
        output_dir = tmp_path / "floating_output"
        gen.generate(output_dir)

        # Verify master.yml exists and has expected structure
        master_file = output_dir / "master.yml"
        assert master_file.exists()
        master_content = master_file.read_text()
        assert "%YAML 1.1" in master_content
        assert "includefile: includes/" in master_content

        # Verify includes directory has generated files
        includes_dir = output_dir / "includes"
        assert includes_dir.exists()
        include_files = list(includes_dir.glob("*.yml"))
        assert len(include_files) > 0

        # Verify environment file reflects spec water depth
        env_file = includes_dir / "03_environment.yml"
        assert env_file.exists()
        env_data = yaml.safe_load(env_file.read_text())
        assert env_data["Environment"]["SeabedOriginDepth"] == 8

        # Verify parameters file
        params_file = output_dir / "inputs" / "parameters.yml"
        assert params_file.exists()
        params = yaml.safe_load(params_file.read_text())
        assert params["water_depth"] == 8


class TestBackwardCompatSingleRunSlay:
    """Existing S-lay spec generates correct output via ModularModelGenerator."""

    @pytest.mark.skipif(
        not SLAY_SPEC.exists(),
        reason=f"S-lay spec not found: {SLAY_SPEC}",
    )
    def test_backward_compat_single_run_slay(self, tmp_path):
        from digitalmodel.solvers.orcaflex.modular_generator import (
            ModularModelGenerator,
        )

        gen = ModularModelGenerator(SLAY_SPEC)
        output_dir = tmp_path / "slay_output"
        gen.generate(output_dir)

        # Verify master.yml exists and has expected structure
        master_file = output_dir / "master.yml"
        assert master_file.exists()
        master_content = master_file.read_text()
        assert "%YAML 1.1" in master_content
        assert "includefile: includes/" in master_content

        # Verify includes directory has generated files
        includes_dir = output_dir / "includes"
        assert includes_dir.exists()
        include_files = list(includes_dir.glob("*.yml"))
        assert len(include_files) > 0

        # Verify environment file reflects spec water depth
        env_file = includes_dir / "03_environment.yml"
        assert env_file.exists()
        env_data = yaml.safe_load(env_file.read_text())
        assert env_data["Environment"]["SeabedOriginDepth"] == 20

        # Verify parameters file
        params_file = output_dir / "inputs" / "parameters.yml"
        assert params_file.exists()
        params = yaml.safe_load(params_file.read_text())
        assert params["water_depth"] == 20


class TestCampaignPreviewMatchesGenerationCount:
    """Preview returns same count as actual generation."""

    def test_campaign_preview_matches_generation_count(self, tmp_path):
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignGenerator,
        )

        campaign_file = _write_floating_campaign(tmp_path)
        gen = CampaignGenerator(campaign_file)

        preview_combos = gen.preview()
        preview_count = len(preview_combos)

        output_dir = tmp_path / "output"
        result = gen.generate(output_dir=output_dir)

        assert result.run_count == preview_count
