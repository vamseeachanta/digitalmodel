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
            project_name='test_project',
            vessel_type='crowley650_atb',
            water_depth=39.0,
            num_mooring_lines=6,
            verbose=True
        )

    def test_generator_initialization(self, generator):
        """Test generator initializes correctly"""
        assert generator.project_name == 'test_project'
        assert generator.vessel_type == 'crowley650_atb'
        assert generator.water_depth == 39.0
        assert generator.num_mooring_lines == 6

    def test_generate_general_file(self, generator, temp_output_dir):
        """Test 01_general.yml generation"""
        output_file = generator.generate_general(temp_output_dir)

        assert output_file.exists()
        assert output_file.name == '01_general.yml'

        content = output_file.read_text()
        assert 'General:' in content
        assert 'StageDuration:' in content
        assert 'WaveType: JONSWAP' in content

    def test_generate_var_data_file(self, generator, temp_output_dir):
        """Test 02_var_data.yml generation"""
        output_file = generator.generate_var_data(temp_output_dir)

        assert output_file.exists()
        assert output_file.name == '02_var_data.yml'

        content = output_file.read_text()
        assert 'WaterDepth: 39.0' in content
        assert 'NumMooringLines: 6' in content

    def test_generate_vessel_file(self, generator, temp_output_dir):
        """Test 04_vessel_*.yml generation"""
        output_file = generator.generate_vessel(temp_output_dir)

        assert output_file.exists()
        assert output_file.name == '04_vessel_crowley650_atb.yml'

        content = output_file.read_text()
        assert 'Vessel:' in content
        assert 'Name: Vessel1' in content
        assert 'MomentOfInertiaTensor:' in content

    def test_generate_lines_file(self, generator, temp_output_dir):
        """Test 05_lines.yml generation"""
        output_file = generator.generate_lines(temp_output_dir)

        assert output_file.exists()
        assert output_file.name == '05_lines.yml'

        content = output_file.read_text()
        assert 'LineType:' in content
        assert 'Line:' in content
        assert 'Mooring1' in content
        assert 'Mooring6' in content

    def test_generate_all_files(self, generator, temp_output_dir):
        """Test generating all base files"""
        files = generator.generate_all(temp_output_dir)

        # Should generate 9 files
        assert len(files) == 9

        # Check all expected files exist
        expected_files = [
            '01_general.yml',
            '02_var_data.yml',
            '03_environment.yml',
            '04_vessel_crowley650_atb.yml',
            '05_lines.yml',
            '06_buoys.yml',
            '08_groups.yml',
            '_90_calculated_positions.yml',
            'test_project_base.yml'
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
        return EnvFileGenerator(
            conditions_profile='baltic_sea',
            verbose=True
        )

    def test_generator_initialization(self, generator):
        """Test generator initializes correctly"""
        assert generator.conditions_profile == 'baltic_sea'
        assert '1yr' in generator.conditions['baltic_sea']
        assert '10yr' in generator.conditions['baltic_sea']
        assert '100yr' in generator.conditions['baltic_sea']

    def test_generate_wave_file(self, generator, temp_output_dir):
        """Test wave file generation"""
        output_file = generator.generate_wave_file(temp_output_dir, '10yr', 0)

        assert output_file.exists()
        assert output_file.name == 'wave_10yr_000deg.yml'

        content = output_file.read_text()
        assert 'WaveType: JONSWAP' in content
        assert 'WaveDirection: 0' in content
        assert 'WaveHs:' in content

    def test_generate_wind_file(self, generator, temp_output_dir):
        """Test wind file generation"""
        output_file = generator.generate_wind_file(temp_output_dir, '10yr', 30)

        assert output_file.exists()
        assert output_file.name == 'wind_10yr_030deg.yml'

        content = output_file.read_text()
        assert 'WindType: Constant' in content
        assert 'WindDirection: 30' in content

    def test_generate_current_file(self, generator, temp_output_dir):
        """Test current file generation"""
        output_file = generator.generate_current_file(temp_output_dir, '10yr', 60)

        assert output_file.exists()
        assert output_file.name == 'current_10yr_060deg.yml'

        content = output_file.read_text()
        assert 'CurrentMethod: Interpolated' in content
        assert 'RefCurrentDirection: 60' in content

    def test_generate_composite_env_file(self, generator, temp_output_dir):
        """Test composite environmental file generation"""
        output_file = generator.generate_composite_env_file(temp_output_dir, '10yr', 0)

        assert output_file.exists()
        assert output_file.name == 'env_10yr_000deg.yml'

        content = output_file.read_text()
        assert 'includefile: wave_10yr_000deg.yml' in content
        assert 'includefile: wind_10yr_000deg.yml' in content
        assert 'includefile: current_10yr_000deg.yml' in content

    def test_generate_all_files(self, generator, temp_output_dir):
        """Test generating all environmental files"""
        return_periods = ['1yr', '10yr']
        headings = [0, 30, 60]

        files = generator.generate_all(temp_output_dir, return_periods, headings)

        # Should generate 4 files per combination (wave, wind, current, composite)
        # 2 return periods * 3 headings * 4 files = 24 files
        assert len(files) == 24

        # Check specific files exist
        assert any(f.name == 'wave_1yr_000deg.yml' for f in files)
        assert any(f.name == 'wind_10yr_030deg.yml' for f in files)
        assert any(f.name == 'env_10yr_060deg.yml' for f in files)


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
