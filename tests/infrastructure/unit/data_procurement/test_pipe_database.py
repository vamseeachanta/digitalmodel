"""
Unit tests for Pipe Specification Database Client
===================================================

Tests:
- Pipe database coverage
- Finding pipes by diameter, schedule, grade
- Finding pipes by pressure rating
- Property calculations with coatings and contents
- OrcaFlex export
"""

import pytest
import yaml
from digitalmodel.data_procurement.riser.database_clients import PipeSpecificationClient


class TestPipeSpecificationClient:
    """Test suite for PipeSpecificationClient."""

    @pytest.fixture
    def client(self):
        """Create client instance."""
        return PipeSpecificationClient()

    def test_pipe_database_coverage(self, client):
        """Test that database contains expected number of pipes."""
        # Should have pipes for multiple diameters, schedules, and grades
        # 14 standard diameters × ~8 schedules × 6 grades ≈ 500+ pipes
        assert len(client.pipe_database) > 500
        assert len(client.pipe_database) < 1000  # Sanity check

    def test_find_by_diameter(self, client):
        """Test finding pipes by diameter."""
        # Test finding all 10" pipes
        pipes = client.find_by_diameter(10)
        assert len(pipes) > 0

        # All should be 10" diameter
        for pipe in pipes:
            assert pipe['nominal_diameter'] == 10

    def test_find_by_diameter_and_schedule(self, client):
        """Test finding pipes by diameter and schedule."""
        pipes = client.find_by_diameter(10, schedule='SCH 80')
        assert len(pipes) >= 6  # Multiple grades available

        # All should be 10" SCH 80
        for pipe in pipes:
            assert pipe['nominal_diameter'] == 10
            assert pipe['schedule'] == 'SCH 80'

    def test_find_by_diameter_schedule_grade(self, client):
        """Test finding specific pipe."""
        pipes = client.find_by_diameter(10, schedule='SCH 80', grade='X52')
        assert len(pipes) == 1

        pipe = pipes[0]
        assert pipe['nominal_diameter'] == 10
        assert pipe['schedule'] == 'SCH 80'
        assert pipe['grade'] == 'X52'

    def test_pipe_properties(self, client):
        """Test pipe property calculations."""
        pipes = client.find_by_diameter(10, schedule='SCH 80', grade='X52')
        pipe = pipes[0]

        # Check required properties exist
        assert 'outer_diameter_mm' in pipe
        assert 'inner_diameter_mm' in pipe
        assert 'wall_thickness_mm' in pipe
        assert 'mass_per_meter_kg' in pipe
        assert 'axial_stiffness_ea_n' in pipe
        assert 'bending_stiffness_ei_nm2' in pipe
        assert 'torsional_stiffness_gj_nm2' in pipe

        # Check values are reasonable
        assert pipe['outer_diameter_mm'] > 0
        assert pipe['inner_diameter_mm'] > 0
        assert pipe['wall_thickness_mm'] > 0
        assert pipe['mass_per_meter_kg'] > 0
        assert pipe['axial_stiffness_ea_n'] > 0
        assert pipe['bending_stiffness_ei_nm2'] > 0

    def test_pressure_rating(self, client):
        """Test finding pipes by pressure rating."""
        # Find pipes for 10 MPa internal pressure
        pipes = client.find_by_pressure_rating(
            internal_pressure=10.0,
            diameter=10,
            safety_factor=1.5
        )

        assert len(pipes) > 0

        # All pipes should meet pressure requirement
        required_pressure = 10.0 * 1.5
        for pipe in pipes:
            assert pipe['burst_pressure_mpa'] >= required_pressure

        # Pipes should be sorted by wall thickness
        for i in range(len(pipes) - 1):
            assert pipes[i]['wall_thickness_mm'] <= pipes[i+1]['wall_thickness_mm']

    def test_calculate_properties_bare_pipe(self, client):
        """Test property calculation for bare pipe."""
        pipe = client.find_by_diameter(10, schedule='SCH 80', grade='X52')[0]

        # Calculate properties without coatings
        props = client.calculate_properties(pipe)

        # Dry mass should equal base pipe mass (no coatings)
        assert props['mass_per_meter_dry_kg'] == pytest.approx(pipe['mass_per_meter_kg'], rel=0.01)

        # Wet mass should equal dry (no contents)
        assert props['mass_per_meter_wet_kg'] == pytest.approx(props['mass_per_meter_dry_kg'], rel=0.01)

        # Submerged mass should be less (buoyancy)
        assert props['mass_per_meter_submerged_kg'] < props['mass_per_meter_wet_kg']

    def test_calculate_properties_with_coatings(self, client):
        """Test property calculation with coatings."""
        pipe = client.find_by_diameter(10, schedule='SCH 80', grade='X52')[0]

        # Add coatings
        props = client.calculate_properties(
            pipe=pipe,
            coatings=[
                {'type': '3LPE', 'thickness': 3.2, 'density': 940},
                {'type': 'insulation', 'thickness': 50, 'density': 500}
            ]
        )

        # Dry mass should be greater than bare pipe
        assert props['mass_per_meter_dry_kg'] > pipe['mass_per_meter_kg']

        # OD with coatings should be larger
        assert props['outer_diameter_with_coatings_mm'] > pipe['outer_diameter_mm']

        # Should have coating information
        assert len(props['coatings']) == 2

    def test_calculate_properties_with_contents(self, client):
        """Test property calculation with contents."""
        pipe = client.find_by_diameter(10, schedule='SCH 80', grade='X52')[0]

        # Add oil contents
        props = client.calculate_properties(
            pipe=pipe,
            contents_density=850  # kg/m³ (oil)
        )

        # Wet mass should be greater than dry
        assert props['mass_per_meter_wet_kg'] > props['mass_per_meter_dry_kg']

        # Contents mass should be positive
        assert props['contents_mass_per_meter_kg'] > 0

    def test_orcaflex_export(self, client):
        """Test OrcaFlex YAML export."""
        pipe = client.find_by_diameter(10, schedule='SCH 80', grade='X52')[0]
        props = client.calculate_properties(pipe)

        yaml_output = client.to_orcaflex_line_type(props)

        # Should be valid YAML
        data = yaml.safe_load(yaml_output)

        # Check structure
        assert 'LineType' in data
        line_type = data['LineType']

        # Check required fields
        assert 'Name' in line_type
        assert 'Category' in line_type
        assert 'OD' in line_type
        assert 'ID' in line_type
        assert 'Mass per unit length' in line_type
        assert 'EA' in line_type
        assert 'EI' in line_type
        assert 'GJ' in line_type

    def test_orcaflex_export_with_coatings(self, client):
        """Test OrcaFlex export with coatings."""
        pipe = client.find_by_diameter(10, schedule='SCH 80', grade='X52')[0]
        props = client.calculate_properties(
            pipe=pipe,
            coatings=[
                {'type': '3LPE', 'thickness': 3.2, 'density': 940},
                {'type': 'insulation', 'thickness': 50, 'density': 500}
            ]
        )

        yaml_output = client.to_orcaflex_line_type(props, name="Test_Riser")

        data = yaml.safe_load(yaml_output)
        line_type = data['LineType']

        # Name should be custom
        assert line_type['Name'] == "Test_Riser"

        # OD should include coatings
        od_m = props['outer_diameter_with_coatings_mm'] / 1000
        assert line_type['OD'] == pytest.approx(od_m, rel=0.01)

    def test_all_standard_diameters(self, client):
        """Test that all standard diameters have pipes."""
        standard_diameters = client.STANDARD_DIAMETERS

        for diameter in standard_diameters:
            pipes = client.find_by_diameter(diameter)
            assert len(pipes) > 0, f"No pipes found for {diameter}\" diameter"

    def test_all_grades(self, client):
        """Test that all API 5L grades are available."""
        grades = list(client.API_5L_GRADES.keys())

        for grade in grades:
            pipes = client.find_by_diameter(10, grade=grade)
            assert len(pipes) > 0, f"No pipes found for grade {grade}"

    def test_schedule_mapping(self, client):
        """Test that schedule mapping is complete."""
        # Should have wall thickness data for common schedules
        assert 10 in client.SCHEDULE_WALL_THICKNESS['SCH 40']
        assert 10 in client.SCHEDULE_WALL_THICKNESS['SCH 80']
        assert 10 in client.SCHEDULE_WALL_THICKNESS['STD']
        assert 10 in client.SCHEDULE_WALL_THICKNESS['XS']

    def test_indexing(self, client):
        """Test that indexes are built correctly."""
        # Diameter index should exist
        assert hasattr(client, 'diameter_index')
        assert 10 in client.diameter_index

        # Diameter-schedule index should exist
        assert hasattr(client, 'diameter_schedule_index')
        assert (10, 'SCH 80') in client.diameter_schedule_index

    def test_material_properties(self, client):
        """Test material property consistency."""
        pipe = client.find_by_diameter(10, schedule='SCH 80', grade='X52')[0]

        # Steel properties should be consistent
        assert pipe['youngs_modulus_gpa'] == 207
        assert pipe['poissons_ratio'] == 0.30
        assert pipe['density_kg_m3'] == 7850

        # Grade properties should match
        assert pipe['yield_strength_mpa'] == 359  # X52
        assert pipe['tensile_strength_mpa'] == 455


class TestRiserClient:
    """Test suite for RiserClient."""

    @pytest.fixture
    def client(self):
        """Create RiserClient instance."""
        from digitalmodel.data_procurement.riser import RiserClient
        return RiserClient()

    def test_get_pipe_specification(self, client):
        """Test getting pipe specification through RiserClient."""
        pipe = client.get_pipe_specification(diameter=10, schedule='SCH 80', grade='X52')

        assert pipe['nominal_diameter'] == 10
        assert pipe['schedule'] == 'SCH 80'
        assert pipe['grade'] == 'X52'

    def test_get_pipe_specification_not_found(self, client):
        """Test error handling for pipe not found."""
        with pytest.raises(ValueError, match="No pipe found"):
            client.get_pipe_specification(diameter=999, schedule='SCH 80', grade='X52')

    def test_calculate_properties(self, client):
        """Test property calculation through RiserClient."""
        pipe = client.get_pipe_specification(10, 'SCH 80', 'X52')

        props = client.calculate_properties(
            pipe=pipe,
            coatings=[
                {'type': '3LPE', 'thickness': 3.2, 'density': 940}
            ],
            contents_density=850
        )

        assert 'mass_per_meter_dry_kg' in props
        assert 'mass_per_meter_wet_kg' in props
        assert 'mass_per_meter_submerged_kg' in props

    def test_to_orcaflex_line_type(self, client):
        """Test OrcaFlex export through RiserClient."""
        pipe = client.get_pipe_specification(10, 'SCH 80', 'X52')
        props = client.calculate_properties(pipe)

        yaml_output = client.to_orcaflex_line_type(props, name="Test_Riser")

        # Should be valid YAML
        data = yaml.safe_load(yaml_output)
        assert 'LineType' in data
        assert data['LineType']['Name'] == "Test_Riser"
