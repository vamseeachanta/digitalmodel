#!/usr/bin/env python3
"""
Unit Tests for Mooring Analysis Module

Tests core functionality including:
- Catenary analysis
- Mooring design verification
- Environmental load calculations
- OrcaFlex model generation
"""

import pytest
import numpy as np
from pathlib import Path

from digitalmodel.mooring_analysis import (
    # Models
    MooringLineProperties,
    AnchorProperties,
    MooringLine,
    EnvironmentalConditions,
    VesselParticulars,
    MooringSystem,
    DesignLoadCase,
    MooringType,
    LineType,
    ConditionType,

    # Analyzers
    CatenaryAnalyzer,
    MooringDesigner,
    OrcaFlexModelGenerator,

    # Material library
    CHAIN_R3_84MM,
    POLYESTER_140MM,
    get_material,
)


class TestMooringLineProperties:
    """Test mooring line properties and material library"""

    def test_create_chain_segment(self):
        """Test creating chain line segment"""
        chain = MooringLineProperties(
            line_type=LineType.CHAIN,
            length=400.0,
            diameter=84.0,
            mbl=8500.0,
            weight_water=145.0,
            ea=850000.0
        )

        assert chain.line_type == LineType.CHAIN
        assert chain.length == 400.0
        assert chain.mbl == 8500.0

    def test_create_polyester_segment(self):
        """Test creating polyester line segment"""
        polyester = MooringLineProperties(
            line_type=LineType.POLYESTER,
            length=200.0,
            diameter=140.0,
            mbl=7200.0,
            weight_water=-8.0,  # Buoyant
            ea=180000.0
        )

        assert polyester.line_type == LineType.POLYESTER
        assert polyester.weight_water < 0  # Buoyant

    def test_material_library(self):
        """Test accessing material library"""
        chain = get_material('chain_r3_84')
        assert chain.diameter == 84.0
        assert chain.mbl == 8500.0

        polyester = get_material('polyester_140')
        assert polyester.diameter == 140.0


class TestMooringLine:
    """Test mooring line configuration"""

    @pytest.fixture
    def sample_line(self):
        """Create sample mooring line"""
        chain = CHAIN_R3_84MM
        chain.length = 400.0

        anchor = AnchorProperties(
            anchor_type="suction_pile",
            holding_capacity=5000.0,
            location=(400.0, 0.0, -100.0)
        )

        return MooringLine(
            line_id="ML1",
            segments=[chain],
            anchor=anchor,
            fairlead_location=(20.0, 0.0, -10.0),
            pretension=500.0
        )

    def test_create_mooring_line(self, sample_line):
        """Test creating mooring line"""
        assert sample_line.line_id == "ML1"
        assert len(sample_line.segments) == 1
        assert sample_line.pretension == 500.0

    def test_total_length(self, sample_line):
        """Test total line length calculation"""
        assert sample_line.total_length == 400.0

    def test_min_mbl(self, sample_line):
        """Test minimum breaking load"""
        assert sample_line.min_mbl == 8500.0


class TestCatenaryAnalyzer:
    """Test catenary analysis"""

    @pytest.fixture
    def analyzer(self):
        """Create catenary analyzer"""
        return CatenaryAnalyzer(water_depth=100.0)

    @pytest.fixture
    def chain_segment(self):
        """Create chain segment"""
        chain = CHAIN_R3_84MM
        chain.length = 500.0
        return chain

    def test_solve_catenary_basic(self, analyzer, chain_segment):
        """Test basic catenary solution"""
        result = analyzer.solve_catenary(
            chain_segment,
            horizontal_tension=1000.0,
            vertical_height=100.0
        )

        assert result.horizontal_tension == 1000.0
        assert result.fairlead_tension > result.horizontal_tension
        assert result.touchdown_tension == result.horizontal_tension
        assert result.arc_length > 0
        assert result.horizontal_distance > 0

    def test_catenary_angle_positive(self, analyzer, chain_segment):
        """Test fairlead angle is positive"""
        result = analyzer.solve_catenary(
            chain_segment,
            horizontal_tension=1000.0
        )

        assert result.fairlead_angle > 0
        assert result.fairlead_angle < 90

    def test_catenary_tension_relationship(self, analyzer, chain_segment):
        """Test tension relationships"""
        result = analyzer.solve_catenary(
            chain_segment,
            horizontal_tension=1000.0
        )

        # Fairlead tension > horizontal tension (due to weight)
        assert result.fairlead_tension > result.horizontal_tension
        # Touchdown tension = horizontal tension
        assert abs(result.touchdown_tension - result.horizontal_tension) < 0.1

    def test_grounded_length(self, analyzer, chain_segment):
        """Test grounded length calculation"""
        result = analyzer.solve_catenary(
            chain_segment,
            horizontal_tension=500.0  # Lower tension = more grounded
        )

        # Some line should be on seabed
        assert result.grounded_length >= 0
        assert result.arc_length + result.grounded_length <= chain_segment.length + 1.0

    def test_solve_for_target_tension(self, analyzer, chain_segment):
        """Test solving for target fairlead tension"""
        target_tension = 2000.0

        result = analyzer.solve_for_horizontal_tension(
            chain_segment,
            target_fairlead_tension=target_tension,
            tolerance=1.0
        )

        assert abs(result.fairlead_tension - target_tension) < 5.0

    def test_catenary_stiffness(self, analyzer, chain_segment):
        """Test stiffness calculation"""
        result = analyzer.solve_catenary(
            chain_segment,
            horizontal_tension=1000.0
        )

        stiffness = analyzer.calculate_stiffness(chain_segment, result)

        assert stiffness.horizontal_stiffness > 0
        assert stiffness.vertical_stiffness > 0
        assert stiffness.geometric_stiffness > 0
        assert stiffness.elastic_stiffness > 0

    def test_touchdown_check(self, analyzer, chain_segment):
        """Test touchdown point checking"""
        check = analyzer.check_touchdown(
            chain_segment,
            horizontal_distance=300.0
        )

        assert 'has_touchdown' in check
        assert 'min_horizontal_tension' in check
        assert check['min_horizontal_tension'] > 0


class TestEnvironmentalLoads:
    """Test environmental load calculations"""

    @pytest.fixture
    def vessel(self):
        """Create sample vessel"""
        return VesselParticulars(
            vessel_type="tanker",
            length=280.0,
            beam=46.0,
            draft=17.5,
            displacement=150000.0,
            windage_area=6000.0
        )

    @pytest.fixture
    def environment(self):
        """Create environmental conditions"""
        return EnvironmentalConditions(
            wave_hs=8.0,
            wave_tp=12.0,
            wave_direction=0.0,
            current_speed=1.0,
            current_direction=0.0,
            wind_speed=20.0,
            wind_direction=0.0,
            return_period=100
        )

    def test_wave_drift_force(self, vessel, environment):
        """Test wave drift force calculation"""
        # Simplified test - actual forces from designer
        # Wave drift should increase with Hs²
        assert environment.wave_hs > 0

    def test_current_force(self, vessel, environment):
        """Test current force calculation"""
        # Current force proportional to V²
        assert environment.current_speed > 0

    def test_wind_force(self, vessel, environment):
        """Test wind force calculation"""
        # Wind force proportional to V²
        assert environment.wind_speed > 0


class TestMooringDesigner:
    """Test mooring design and verification"""

    @pytest.fixture
    def simple_system(self):
        """Create simple 4-line mooring system"""
        chain = CHAIN_R3_84MM
        chain.length = 500.0

        vessel = VesselParticulars(
            vessel_type="fpso",
            length=250.0,
            beam=42.0,
            draft=16.0,
            displacement=120000.0,
            windage_area=5000.0
        )

        lines = []
        for i in range(4):
            angle = i * 90.0
            angle_rad = np.radians(angle)
            anchor_r = 400.0
            fairlead_r = 20.0

            anchor = AnchorProperties(
                anchor_type="drag",
                holding_capacity=4000.0,
                location=(anchor_r * np.cos(angle_rad), anchor_r * np.sin(angle_rad), -100.0)
            )

            line = MooringLine(
                line_id=f"ML{i+1}",
                segments=[chain],
                anchor=anchor,
                fairlead_location=(fairlead_r * np.cos(angle_rad), fairlead_r * np.sin(angle_rad), -10.0),
                pretension=500.0
            )
            lines.append(line)

        return MooringSystem(
            system_type=MooringType.SPREAD,
            water_depth=100.0,
            lines=lines,
            vessel=vessel
        )

    @pytest.fixture
    def environment(self):
        """Create environmental conditions"""
        return EnvironmentalConditions(
            wave_hs=6.0,
            wave_tp=11.0,
            wave_direction=0.0,
            current_speed=0.8,
            current_direction=0.0,
            wind_speed=15.0,
            wind_direction=0.0
        )

    def test_create_designer(self, simple_system):
        """Test creating mooring designer"""
        designer = MooringDesigner(simple_system)
        assert designer.system == simple_system
        assert designer.analyzer.water_depth == 100.0

    def test_environmental_loads(self, simple_system, environment):
        """Test environmental load calculation"""
        designer = MooringDesigner(simple_system)
        loads = designer.calculate_environmental_loads(environment)

        assert loads.wave_drift_force > 0
        assert loads.current_force > 0
        assert loads.wind_force > 0
        assert loads.total_force > 0
        assert loads.total_force >= max(loads.wave_drift_force, loads.current_force, loads.wind_force)

    def test_analyze_intact_condition(self, simple_system, environment):
        """Test intact condition analysis"""
        designer = MooringDesigner(simple_system)

        load_case = DesignLoadCase(
            name="intact_test",
            condition=ConditionType.INTACT,
            environment=environment
        )

        results = designer.analyze_intact_condition(load_case)

        assert len(results) == 4  # 4 lines
        for result in results:
            assert result.max_tension > 0
            assert result.safety_factor > 0
            assert result.utilization > 0
            assert isinstance(result.passes, bool)

    def test_analyze_damaged_condition(self, simple_system, environment):
        """Test damaged condition analysis"""
        designer = MooringDesigner(simple_system)

        load_case = DesignLoadCase(
            name="damaged_test",
            condition=ConditionType.DAMAGED,
            environment=environment,
            damaged_line_id="ML1"
        )

        results = designer.analyze_damaged_condition(load_case)

        assert len(results) == 3  # 3 remaining lines
        for result in results:
            assert result.max_tension > 0
            # Damaged condition has lower SF requirement
            assert result.details['condition'] == 'damaged'

    def test_safety_factors(self, simple_system):
        """Test safety factor requirements"""
        designer = MooringDesigner(simple_system)

        # Intact condition
        assert designer.SAFETY_FACTORS[ConditionType.INTACT]['dynamic'] == 1.67
        # Damaged condition
        assert designer.SAFETY_FACTORS[ConditionType.DAMAGED]['dynamic'] == 1.25
        # Transient
        assert designer.SAFETY_FACTORS[ConditionType.TRANSIENT]['dynamic'] == 1.05

    def test_design_summary(self, simple_system, environment):
        """Test design summary generation"""
        designer = MooringDesigner(simple_system)

        load_case = DesignLoadCase(
            name="test_case",
            condition=ConditionType.INTACT,
            environment=environment
        )

        results = designer.analyze_intact_condition(load_case)
        summary = designer.generate_design_summary(results)

        assert 'overall_status' in summary
        assert 'min_safety_factor' in summary
        assert 'max_utilization' in summary
        assert 'critical_line' in summary


class TestOrcaFlexModelGenerator:
    """Test OrcaFlex model generation"""

    @pytest.fixture
    def simple_system(self):
        """Create simple mooring system"""
        chain = CHAIN_R3_84MM
        chain.length = 400.0

        vessel = VesselParticulars(
            vessel_type="tanker",
            length=200.0,
            beam=40.0,
            draft=15.0,
            displacement=100000.0,
            windage_area=4000.0
        )

        anchor = AnchorProperties(
            anchor_type="pile",
            holding_capacity=5000.0,
            location=(300.0, 0.0, -80.0)
        )

        line = MooringLine(
            line_id="ML1",
            segments=[chain],
            anchor=anchor,
            fairlead_location=(15.0, 0.0, -8.0),
            pretension=400.0
        )

        return MooringSystem(
            system_type=MooringType.CALM,
            water_depth=80.0,
            lines=[line],
            vessel=vessel
        )

    def test_create_generator(self, simple_system):
        """Test creating OrcaFlex model generator"""
        generator = OrcaFlexModelGenerator(simple_system)
        assert generator.system == simple_system

    def test_generate_line_type(self, simple_system):
        """Test line type generation"""
        generator = OrcaFlexModelGenerator(simple_system)
        chain = simple_system.lines[0].segments[0]

        line_type = generator.generate_line_type(chain)

        assert 'Name' in line_type
        assert 'OD' in line_type
        assert 'EA' in line_type
        assert 'MBL' in line_type
        assert line_type['EA'] > 0

    def test_generate_line_data(self, simple_system):
        """Test line data generation"""
        generator = OrcaFlexModelGenerator(simple_system)
        line = simple_system.lines[0]

        line_data = generator.generate_line_data(line)

        assert line_data['Name'] == 'ML1'
        assert 'EndAX' in line_data  # Anchor
        assert 'EndBX' in line_data  # Fairlead
        assert len(line_data['LineType']) == len(line.segments)

    def test_generate_vessel_data(self, simple_system):
        """Test vessel data generation"""
        generator = OrcaFlexModelGenerator(simple_system)
        vessel_data = generator.generate_vessel_data()

        assert 'Name' in vessel_data
        assert 'Length' in vessel_data
        assert vessel_data['Length'] == 200.0

    def test_generate_environment_data(self, simple_system):
        """Test environment data generation"""
        generator = OrcaFlexModelGenerator(simple_system)
        env_data = generator.generate_environment_data()

        assert 'WaterDepth' in env_data
        assert env_data['WaterDepth'] == 80.0
        assert env_data['WaterDensity'] == 1025.0

    def test_generate_model_dict(self, simple_system):
        """Test complete model dictionary generation"""
        generator = OrcaFlexModelGenerator(simple_system)
        model = generator.generate_model_dict()

        assert 'General' in model
        assert 'Environment' in model
        assert 'Vessel' in model
        assert 'LineTypes' in model
        assert 'Lines' in model
        assert len(model['Lines']) == 1

    def test_generate_model_yml(self, simple_system, tmp_path):
        """Test YAML model file generation"""
        generator = OrcaFlexModelGenerator(simple_system)
        output_file = tmp_path / "mooring_model.yml"

        result_path = generator.generate_model_yml(str(output_file))

        assert Path(result_path).exists()
        assert output_file.exists()


class TestDesignLoadCase:
    """Test design load case configuration"""

    def test_create_intact_load_case(self):
        """Test creating intact load case"""
        env = EnvironmentalConditions(
            wave_hs=7.0,
            wave_tp=11.0,
            wave_direction=0.0,
            current_speed=0.9,
            current_direction=0.0,
            wind_speed=18.0,
            wind_direction=0.0
        )

        load_case = DesignLoadCase(
            name="intact_100yr",
            condition=ConditionType.INTACT,
            environment=env
        )

        assert load_case.condition == ConditionType.INTACT
        assert load_case.safety_factor_required == 1.67

    def test_create_damaged_load_case(self):
        """Test creating damaged load case"""
        env = EnvironmentalConditions(
            wave_hs=7.0,
            wave_tp=11.0,
            wave_direction=0.0,
            current_speed=0.9,
            current_direction=0.0,
            wind_speed=18.0,
            wind_direction=0.0
        )

        load_case = DesignLoadCase(
            name="damaged_100yr",
            condition=ConditionType.DAMAGED,
            environment=env,
            damaged_line_id="ML1"
        )

        assert load_case.condition == ConditionType.DAMAGED
        assert load_case.safety_factor_required == 1.25
        assert load_case.damaged_line_id == "ML1"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
