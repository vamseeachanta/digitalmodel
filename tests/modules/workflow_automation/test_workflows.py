#!/usr/bin/env python3
"""
ABOUTME: Unit tests for pre-built engineering analysis workflows including
riser analysis, mooring design, and structural workflows.
"""

import pytest
from digitalmodel.modules.workflow_automation.workflows import (
    CompleteRiserAnalysisWorkflow,
    MooringSystemDesignWorkflow,
    PlatformStructuralWorkflow,
    VesselResponseWorkflow,
)


class TestCompleteRiserAnalysisWorkflow:
    """Test complete riser analysis workflow"""

    def test_create_workflow(self):
        """Test creating complete riser analysis workflow"""
        workflow = CompleteRiserAnalysisWorkflow.create(
            diameter=0.508,
            thickness=0.025,
            length=1500,
            water_depth=1000,
            offset=500,
            current_speed=1.0,
        )

        assert workflow.name == "Complete Riser Analysis"
        assert "riser analysis" in workflow.description.lower()
        assert len(workflow.tasks) == 4  # catenary, freq, viv, fatigue

    def test_task_dependencies(self):
        """Test task dependency structure"""
        workflow = CompleteRiserAnalysisWorkflow.create(
            diameter=0.508,
            thickness=0.025,
            length=1500,
            water_depth=1000,
            offset=500,
            current_speed=1.0,
        )

        # Get tasks
        catenary_task = workflow.get_task('catenary')
        freq_task = workflow.get_task('natural_freq')
        viv_task = workflow.get_task('viv_screen')
        fatigue_task = workflow.get_task('fatigue')

        # Verify dependencies
        assert catenary_task is not None
        assert len(catenary_task.depends_on) == 0  # No dependencies

        assert freq_task is not None
        assert 'catenary' in freq_task.depends_on

        assert viv_task is not None
        assert 'natural_freq' in viv_task.depends_on

        assert fatigue_task is not None
        assert 'viv_screen' in fatigue_task.depends_on
        assert fatigue_task.required is False  # Optional task

    def test_task_inputs(self):
        """Test task input configuration"""
        workflow = CompleteRiserAnalysisWorkflow.create(
            diameter=0.508,
            thickness=0.025,
            length=1500,
            water_depth=1000,
            offset=500,
            current_speed=1.0,
            material='x65',
            internal_fluid='oil',
        )

        catenary_task = workflow.get_task('catenary')

        assert catenary_task.inputs['diameter'] == 0.508
        assert catenary_task.inputs['thickness'] == 0.025
        assert catenary_task.inputs['length'] == 1500
        assert catenary_task.inputs['water_depth'] == 1000
        assert catenary_task.inputs['offset'] == 500
        assert catenary_task.inputs['material'] == 'x65'
        assert catenary_task.inputs['internal_fluid'] == 'oil'

    def test_task_outputs(self):
        """Test task output mapping"""
        workflow = CompleteRiserAnalysisWorkflow.create(
            diameter=0.508,
            thickness=0.025,
            length=1500,
            water_depth=1000,
            offset=500,
            current_speed=1.0,
        )

        catenary_task = workflow.get_task('catenary')
        viv_task = workflow.get_task('viv_screen')
        fatigue_task = workflow.get_task('fatigue')

        # Check output mappings
        assert catenary_task.outputs['top_tension'] == 'riser_top_tension'
        assert viv_task.outputs['viv_status'] == 'viv_risk_status'
        assert fatigue_task.outputs['fatigue_life'] == 'riser_fatigue_life_years'

    def test_reference_resolution(self):
        """Test input reference resolution"""
        workflow = CompleteRiserAnalysisWorkflow.create(
            diameter=0.508,
            thickness=0.025,
            length=1500,
            water_depth=1000,
            offset=500,
            current_speed=1.0,
        )

        viv_task = workflow.get_task('viv_screen')

        # Check that VIV task references natural frequency from previous task
        assert viv_task.inputs['natural_frequency'] == '$riser_natural_frequency'


class TestMooringSystemDesignWorkflow:
    """Test mooring system design workflow"""

    def test_create_workflow(self):
        """Test creating mooring system design workflow"""
        workflow = MooringSystemDesignWorkflow.create(
            vessel_type='fpso',
            water_depth=1500,
            line_length=2000,
            line_diameter=0.120,
            wind_speed=25.0,
            current_speed=1.5,
        )

        assert workflow.name == "Mooring System Design"
        assert "mooring" in workflow.description.lower()
        assert len(workflow.tasks) == 3  # env_loads, catenary, safety

    def test_task_dependencies(self):
        """Test task dependency structure"""
        workflow = MooringSystemDesignWorkflow.create(
            vessel_type='fpso',
            water_depth=1500,
            line_length=2000,
        )

        env_loads_task = workflow.get_task('env_loads')
        catenary_task = workflow.get_task('catenary_mooring')
        safety_task = workflow.get_task('safety_check')

        # Verify dependencies
        assert len(env_loads_task.depends_on) == 0
        assert 'env_loads' in catenary_task.depends_on
        assert 'catenary_mooring' in safety_task.depends_on

    def test_environmental_loading_inputs(self):
        """Test environmental loading inputs"""
        workflow = MooringSystemDesignWorkflow.create(
            vessel_type='semisubmersible',
            water_depth=1500,
            line_length=2000,
            wind_speed=30.0,
            current_speed=2.0,
            wind_direction=45.0,
            current_direction=90.0,
        )

        env_task = workflow.get_task('env_loads')

        assert env_task.inputs['vessel_type'] == 'semisubmersible'
        assert env_task.inputs['wind_speed'] == 30.0
        assert env_task.inputs['current_speed'] == 2.0
        assert env_task.inputs['wind_direction'] == 45.0
        assert env_task.inputs['current_direction'] == 90.0

    def test_load_reference_propagation(self):
        """Test load reference propagation through workflow"""
        workflow = MooringSystemDesignWorkflow.create(
            vessel_type='fpso',
            water_depth=1500,
            line_length=2000,
        )

        catenary_task = workflow.get_task('catenary_mooring')
        safety_task = workflow.get_task('safety_check')

        # Catenary should reference horizontal load from env task
        assert catenary_task.inputs['horizontal_load'] == '$mooring_horizontal_load'

        # Safety should reference line tension from catenary task
        assert safety_task.inputs['line_tension'] == '$mooring_line_tension'


class TestPlatformStructuralWorkflow:
    """Test platform structural analysis workflow"""

    def test_create_workflow(self):
        """Test creating structural workflow"""
        workflow = PlatformStructuralWorkflow.create(
            load_type='combined',
            plate_length=3.0,
            plate_width=2.0,
            plate_thickness=0.025,
            material='s355',
            applied_stress=200.0,
        )

        assert workflow.name == "Platform Structural Analysis"
        assert "structural" in workflow.description.lower()
        assert len(workflow.tasks) == 3  # stress, buckling, capacity

    def test_task_dependencies(self):
        """Test task dependency chain"""
        workflow = PlatformStructuralWorkflow.create(
            load_type='combined',
            plate_length=3.0,
            plate_width=2.0,
            plate_thickness=0.025,
        )

        stress_task = workflow.get_task('stress_calc')
        buckling_task = workflow.get_task('buckling')
        capacity_task = workflow.get_task('capacity')

        # Verify linear dependency chain
        assert len(stress_task.depends_on) == 0
        assert 'stress_calc' in buckling_task.depends_on
        assert 'buckling' in capacity_task.depends_on

    def test_stress_calculation_inputs(self):
        """Test stress calculation inputs"""
        workflow = PlatformStructuralWorkflow.create(
            load_type='combined',
            plate_length=3.0,
            plate_width=2.0,
            plate_thickness=0.025,
            material='s355',
            applied_stress=250.0,
        )

        stress_task = workflow.get_task('stress_calc')

        # Check that stresses are derived from applied stress
        assert stress_task.inputs['sigma_x'] == 250.0
        assert stress_task.inputs['sigma_y'] == 250.0 * 0.5
        assert stress_task.inputs['tau_xy'] == 250.0 * 0.3
        assert stress_task.inputs['material'] == 's355'

    def test_result_propagation(self):
        """Test result propagation through analysis chain"""
        workflow = PlatformStructuralWorkflow.create(
            load_type='combined',
            plate_length=3.0,
            plate_width=2.0,
            plate_thickness=0.025,
        )

        buckling_task = workflow.get_task('buckling')
        capacity_task = workflow.get_task('capacity')

        # Buckling should use von mises stress from stress calc
        assert buckling_task.inputs['applied_stress'] == '$plate_von_mises_stress'

        # Capacity should use results from stress and buckling
        assert capacity_task.inputs['stress_utilization'] == '$plate_stress_utilization'
        assert capacity_task.inputs['buckling_status'] == '$plate_buckling_status'


class TestVesselResponseWorkflow:
    """Test vessel response analysis workflow"""

    def test_create_workflow(self):
        """Test creating vessel response workflow"""
        workflow = VesselResponseWorkflow.create(
            hs=3.5,
            tp=10.0,
            vessel_type='fpso',
            wave_direction=45.0,
            spectrum_type='jonswap',
        )

        assert workflow.name == "Vessel Response Analysis"
        assert "vessel" in workflow.description.lower()
        assert len(workflow.tasks) == 2  # wave_spectrum, response_calc

    def test_task_dependencies(self):
        """Test task dependency structure"""
        workflow = VesselResponseWorkflow.create(
            hs=3.5,
            tp=10.0,
        )

        spectrum_task = workflow.get_task('wave_spectrum')
        response_task = workflow.get_task('response_calc')

        # Response depends on spectrum
        assert len(spectrum_task.depends_on) == 0
        assert 'wave_spectrum' in response_task.depends_on

    def test_wave_spectrum_inputs(self):
        """Test wave spectrum generation inputs"""
        workflow = VesselResponseWorkflow.create(
            hs=4.0,
            tp=12.0,
            spectrum_type='bretschneider',
        )

        spectrum_task = workflow.get_task('wave_spectrum')

        assert spectrum_task.inputs['hs'] == 4.0
        assert spectrum_task.inputs['tp'] == 12.0
        assert spectrum_task.inputs['spectrum_type'] == 'bretschneider'

    def test_response_calculation_inputs(self):
        """Test vessel response calculation inputs"""
        workflow = VesselResponseWorkflow.create(
            hs=3.5,
            tp=10.0,
            vessel_type='semisubmersible',
            wave_direction=90.0,
        )

        response_task = workflow.get_task('response_calc')

        assert response_task.inputs['vessel_type'] == 'semisubmersible'
        assert response_task.inputs['wave_direction'] == 90.0
        assert response_task.inputs['wave_spectrum'] == '$wave_spectrum_data'


class TestWorkflowDependencyOrdering:
    """Test dependency ordering for all workflows"""

    def test_riser_workflow_order(self):
        """Test riser workflow execution order"""
        workflow = CompleteRiserAnalysisWorkflow.create(
            diameter=0.508,
            thickness=0.025,
            length=1500,
            water_depth=1000,
            offset=500,
            current_speed=1.0,
        )

        order = workflow.get_dependency_order()

        # Should have 4 sequential groups (no parallel tasks)
        assert len(order) == 4
        assert order[0] == ['catenary']
        assert order[1] == ['natural_freq']
        assert order[2] == ['viv_screen']
        assert order[3] == ['fatigue']

    def test_mooring_workflow_order(self):
        """Test mooring workflow execution order"""
        workflow = MooringSystemDesignWorkflow.create(
            vessel_type='fpso',
            water_depth=1500,
            line_length=2000,
        )

        order = workflow.get_dependency_order()

        # Should have 3 sequential groups
        assert len(order) == 3
        assert order[0] == ['env_loads']
        assert order[1] == ['catenary_mooring']
        assert order[2] == ['safety_check']

    def test_structural_workflow_order(self):
        """Test structural workflow execution order"""
        workflow = PlatformStructuralWorkflow.create(
            load_type='combined',
            plate_length=3.0,
            plate_width=2.0,
            plate_thickness=0.025,
        )

        order = workflow.get_dependency_order()

        # Should have 3 sequential groups
        assert len(order) == 3
        assert order[0] == ['stress_calc']
        assert order[1] == ['buckling']
        assert order[2] == ['capacity']
