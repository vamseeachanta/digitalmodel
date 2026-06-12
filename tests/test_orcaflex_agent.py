"""
ABOUTME: Test suite for OrcaFlex Agent file generators
ABOUTME: Tests base file and environmental file generation with actual templates
"""

import pytest
from pathlib import Path
import tempfile
import shutil
from digitalmodel.agents.orcaflex.generators.base_files import BaseFileGenerator
from digitalmodel.agents.orcaflex.generators.env_files import EnvFileGenerator


class TestBaseFileGenerator:
    """Test base file generation"""

    @pytest.fixture
    def temp_output_dir(self):
        """Create temporary output directory"""
        temp_dir = tempfile.mkdtemp()
        yield Path(temp_dir)
        shutil.rmtree(temp_dir)

    @pytest.fixture
    def generator(self):
        """Create base file generator instance"""
        return BaseFileGenerator(
            project_name="test_project",
            vessel_type="crowley650_atb",
            water_depth=39.0,
            num_mooring_lines=6,
            verbose=True,
        )

    def test_generator_initialization(self, generator):
        """Test generator initializes correctly"""
        assert generator.project_name == "test_project"
        assert generator.vessel_type == "crowley650_atb"
        assert generator.water_depth == 39.0
        assert generator.num_mooring_lines == 6

    def test_generate_general_file(self, generator, temp_output_dir):
        """Test 01_general.yml generation"""
        output_file = generator.generate_general(temp_output_dir)

        assert output_file.exists()
        assert output_file.name == "01_general.yml"

        content = output_file.read_text()
        assert "General:" in content
        assert "StageDuration:" in content
        assert "DynamicsSolutionMethod:" in content

    def test_generate_environment_file(self, generator, temp_output_dir):
        """Test 02_environment.yml generation"""
        output_file = generator.generate_environment(temp_output_dir)

        assert output_file.exists()
        assert output_file.name == "02_environment.yml"

        content = output_file.read_text()
        assert "Environment:" in content
        assert "WaterDepth: 39.0" in content

    def test_generate_vessel_type_files(self, generator, temp_output_dir):
        """Test vessel type wrapper and data generation"""
        wrapper, data = generator.generate_vessel_type(temp_output_dir)

        assert wrapper.exists()
        assert wrapper.name == "04_vessel.yml"
        assert data.exists()
        assert data.name == "_04_vessel_data.yml"

        wrapper_content = wrapper.read_text()
        data_content = data.read_text()
        assert "VesselTypes:" in wrapper_content
        assert "includefile: _04_vessel_data.yml" in wrapper_content
        assert "crowley650_atb:" in data_content
        assert "MomentOfInertiaTensorX, MomentOfInertiaTensorY" in data_content

    def test_generate_vessel_instance_files(self, generator, temp_output_dir):
        """Test vessel instance wrapper and data generation"""
        wrapper, data = generator.generate_vessel_instance(temp_output_dir)

        assert wrapper.exists()
        assert wrapper.name == "05_vessel_inst.yml"
        assert data.exists()
        assert data.name == "_05_vessel_inst_data.yml"

        assert "Vessels:" in wrapper.read_text()
        data_content = data.read_text()
        assert "Vessel1:" in data_content
        assert "VesselType: crowley650_atb" in data_content

    def test_generate_line_type_and_line_files(self, generator, temp_output_dir):
        """Test line type plus line wrapper/data generation"""
        line_types = generator.generate_line_types(temp_output_dir)
        wrapper, data = generator.generate_lines(temp_output_dir)

        assert line_types.exists()
        assert line_types.name == "06_line_types.yml"
        assert wrapper.exists()
        assert wrapper.name == "07_lines.yml"
        assert data.exists()
        assert data.name == "_07_lines_data.yml"

        assert "LineTypes:" in line_types.read_text()
        assert "Lines:" in wrapper.read_text()
        data_content = data.read_text()
        assert "Mooring1" in data_content
        assert "Mooring6" in data_content

    def test_generate_all_files(self, generator, temp_output_dir):
        """Test generating all base files"""
        files = generator.generate_all(temp_output_dir)

        assert len(files) == 13

        expected_files = [
            "01_general.yml",
            "02_environment.yml",
            "04_vessel.yml",
            "_04_vessel_data.yml",
            "05_vessel_inst.yml",
            "_05_vessel_inst_data.yml",
            "06_line_types.yml",
            "07_lines.yml",
            "_07_lines_data.yml",
            "08_buoys.yml",
            "_08_buoys_data.yml",
            "09_groups.yml",
            "test_project_base.yml",
        ]

        for expected in expected_files:
            assert any(f.name == expected for f in files), f"Missing file: {expected}"


class TestEnvFileGenerator:
    """Test environmental file generation"""

    @pytest.fixture
    def temp_output_dir(self):
        """Create temporary output directory"""
        temp_dir = tempfile.mkdtemp()
        yield Path(temp_dir)
        shutil.rmtree(temp_dir)

    @pytest.fixture
    def generator(self):
        """Create environmental file generator instance"""
        return EnvFileGenerator(conditions_profile="baltic_sea", verbose=True)

    def test_generator_initialization(self, generator):
        """Test generator initializes correctly"""
        assert generator.conditions_profile == "baltic_sea"
        assert "1yr" in generator.conditions["baltic_sea"]
        assert "10yr" in generator.conditions["baltic_sea"]
        assert "100yr" in generator.conditions["baltic_sea"]

    def test_generate_wave_file(self, generator, temp_output_dir):
        """Test wave file generation"""
        output_file = generator.generate_wave_file(temp_output_dir, "10yr", 0)

        assert output_file.exists()
        assert output_file.name == "wave_10yr_000deg.yml"

        content = output_file.read_text()
        assert "WaveType: JONSWAP" in content
        assert "WaveDirection: 0" in content
        assert "WaveHs:" in content

    def test_generate_wind_file(self, generator, temp_output_dir):
        """Test wind file generation"""
        output_file = generator.generate_wind_file(temp_output_dir, "10yr", 30)

        assert output_file.exists()
        assert output_file.name == "wind_10yr_030deg.yml"

        content = output_file.read_text()
        assert "WindType: Constant" in content
        assert "WindDirection: 30" in content

    def test_generate_current_file(self, generator, temp_output_dir):
        """Test current file generation"""
        output_file = generator.generate_current_file(temp_output_dir, "10yr", 60)

        assert output_file.exists()
        assert output_file.name == "current_10yr_060deg.yml"

        content = output_file.read_text()
        assert "CurrentMethod: Interpolated" in content
        assert "RefCurrentDirection: 60" in content

    def test_generate_composite_env_file(self, generator, temp_output_dir):
        """Test composite environmental file generation"""
        output_file = generator.generate_composite_env_file(temp_output_dir, "10yr", 0)

        assert output_file.exists()
        assert output_file.name == "env_10yr_000deg.yml"

        content = output_file.read_text()
        assert "includefile: wave_10yr_000deg.yml" in content
        assert "includefile: wind_10yr_000deg.yml" in content
        assert "includefile: current_10yr_000deg.yml" in content

    def test_generate_all_files(self, generator, temp_output_dir):
        """Test generating all environmental files"""
        return_periods = ["1yr", "10yr"]
        headings = [0, 30, 60]

        files = generator.generate_all(temp_output_dir, return_periods, headings)

        # Should generate 4 files per combination (wave, wind, current, composite)
        # 2 return periods * 3 headings * 4 files = 24 files
        assert len(files) == 24

        # Check specific files exist
        assert any(f.name == "wave_1yr_000deg.yml" for f in files)
        assert any(f.name == "wind_10yr_030deg.yml" for f in files)
        assert any(f.name == "env_10yr_060deg.yml" for f in files)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
