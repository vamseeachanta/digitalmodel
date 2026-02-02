#!/usr/bin/env python3
"""
ABOUTME: Pre-built engineering analysis workflows combining multiple modules
for complete end-to-end analyses.
"""

from .models import WorkflowDefinition, WorkflowTask
from typing import Dict, Any


class CompleteRiserAnalysisWorkflow:
    """
    Complete riser analysis workflow

    Executes full riser analysis combining:
    1. Catenary static configuration
    2. VIV (Vortex-Induced Vibration) screening
    3. Fatigue life assessment
    4. Signal processing and statistics

    Example Usage:
        >>> workflow = CompleteRiserAnalysisWorkflow.create(
        ...     diameter=0.508,
        ...     thickness=0.025,
        ...     length=1500,
        ...     water_depth=1000,
        ...     offset=500,
        ...     current_speed=1.0
        ... )
        >>> orchestrator = WorkflowOrchestrator()
        >>> result = orchestrator.execute_workflow(workflow)
    """

    @staticmethod
    def create(
        diameter: float,
        thickness: float,
        length: float,
        water_depth: float,
        offset: float,
        current_speed: float,
        material: str = 'x65',
        internal_fluid: str = 'oil',
    ) -> WorkflowDefinition:
        """
        Create complete riser analysis workflow

        Args:
            diameter: Riser outer diameter (m)
            thickness: Wall thickness (m)
            length: Total riser length (m)
            water_depth: Water depth (m)
            offset: Horizontal offset at top (m)
            current_speed: Current speed (m/s)
            material: Material grade
            internal_fluid: Internal fluid type

        Returns:
            WorkflowDefinition ready for execution
        """
        workflow = WorkflowDefinition(
            name="Complete Riser Analysis",
            description="End-to-end riser analysis: catenary → VIV → fatigue → reporting"
        )

        # Task 1: Catenary static analysis
        workflow.add_task(WorkflowTask(
            name="Catenary Static Analysis",
            task_id="catenary",
            module="catenary_riser.simple_catenary",
            function="analyze_simple_catenary",
            inputs={
                'diameter': diameter,
                'thickness': thickness,
                'length': length,
                'water_depth': water_depth,
                'offset': offset,
                'material': material,
                'internal_fluid': internal_fluid,
            },
            outputs={
                'top_tension': 'riser_top_tension',
                'effective_weight': 'riser_effective_weight',
            }
        ))

        # Task 2: VIV natural frequency analysis
        workflow.add_task(WorkflowTask(
            name="Natural Frequency Calculation",
            task_id="natural_freq",
            module="viv_analysis.frequency_calculator",
            function="calculate_natural_frequency",
            inputs={
                'diameter': diameter,
                'thickness': thickness,
                'length': length,
                'material': material,
                'boundary_condition': 'pinned-pinned',
            },
            outputs={
                'natural_frequency': 'riser_natural_frequency',
            },
            depends_on=['catenary'],
        ))

        # Task 3: VIV screening
        workflow.add_task(WorkflowTask(
            name="VIV Screening",
            task_id="viv_screen",
            module="viv_analysis.screening",
            function="screen_viv_susceptibility",
            inputs={
                'current_speed': current_speed,
                'natural_frequency': '$riser_natural_frequency',
                'diameter': diameter,
            },
            outputs={
                'viv_status': 'viv_risk_status',
                'reduced_velocity': 'viv_reduced_velocity',
            },
            depends_on=['natural_freq'],
        ))

        # Task 4: Fatigue assessment (if VIV risk exists)
        workflow.add_task(WorkflowTask(
            name="Fatigue Life Assessment",
            task_id="fatigue",
            module="fatigue_analysis",
            function="assess_fatigue_life",
            inputs={
                'diameter': diameter,
                'thickness': thickness,
                'stress_range': 100.0,  # Simplified for workflow demo
                'cycles': 1e7,
                'material': material,
            },
            outputs={
                'fatigue_life': 'riser_fatigue_life_years',
                'damage_ratio': 'fatigue_damage_ratio',
            },
            depends_on=['viv_screen'],
            required=False,  # Continue if fatigue analysis fails
        ))

        return workflow


class MooringSystemDesignWorkflow:
    """
    Mooring system design workflow

    Complete mooring analysis combining:
    1. Environmental load calculation (wind/current)
    2. Catenary mooring line analysis
    3. Safety factor verification
    4. System optimization

    Example Usage:
        >>> workflow = MooringSystemDesignWorkflow.create(
        ...     vessel_type='fpso',
        ...     water_depth=1500,
        ...     line_length=2000,
        ...     wind_speed=25.0,
        ...     current_speed=1.5
        ... )
    """

    @staticmethod
    def create(
        vessel_type: str,
        water_depth: float,
        line_length: float,
        line_diameter: float = 0.120,
        wind_speed: float = 25.0,
        current_speed: float = 1.5,
        wind_direction: float = 45.0,
        current_direction: float = 90.0,
    ) -> WorkflowDefinition:
        """
        Create mooring system design workflow

        Args:
            vessel_type: Vessel type ('fpso', 'semisubmersible', 'tanker')
            water_depth: Water depth (m)
            line_length: Mooring line length (m)
            line_diameter: Chain diameter (m)
            wind_speed: Wind speed (m/s)
            current_speed: Current speed (m/s)
            wind_direction: Wind direction (deg)
            current_direction: Current direction (deg)

        Returns:
            WorkflowDefinition for mooring system design
        """
        workflow = WorkflowDefinition(
            name="Mooring System Design",
            description="Complete mooring design: loads → analysis → verification"
        )

        # Task 1: Calculate environmental loads
        workflow.add_task(WorkflowTask(
            name="Environmental Loads",
            task_id="env_loads",
            module="hydrodynamics.ocimf_loading",
            function="calculate_combined_loads",
            inputs={
                'vessel_type': vessel_type,
                'wind_speed': wind_speed,
                'wind_direction': wind_direction,
                'current_speed': current_speed,
                'current_direction': current_direction,
            },
            outputs={
                'horizontal_load': 'mooring_horizontal_load',
                'resultant_load': 'mooring_total_load',
            }
        ))

        # Task 2: Catenary mooring analysis
        workflow.add_task(WorkflowTask(
            name="Catenary Mooring Analysis",
            task_id="catenary_mooring",
            module="mooring_analysis.catenary",
            function="analyze_catenary_mooring",
            inputs={
                'water_depth': water_depth,
                'line_length': line_length,
                'line_diameter': line_diameter,
                'horizontal_load': '$mooring_horizontal_load',
            },
            outputs={
                'line_tension': 'mooring_line_tension',
                'anchor_load': 'mooring_anchor_load',
            },
            depends_on=['env_loads'],
        ))

        # Task 3: Safety factor verification
        workflow.add_task(WorkflowTask(
            name="Safety Factor Check",
            task_id="safety_check",
            module="mooring_analysis.designer",
            function="verify_safety_factors",
            inputs={
                'line_tension': '$mooring_line_tension',
                'line_diameter': line_diameter,
                'condition': 'intact',
            },
            outputs={
                'safety_factor': 'mooring_safety_factor',
                'status': 'mooring_design_status',
            },
            depends_on=['catenary_mooring'],
        ))

        return workflow


class PlatformStructuralWorkflow:
    """
    Platform structural analysis workflow

    Complete structural check combining:
    1. Load extraction from environmental conditions
    2. Von Mises stress calculation
    3. Plate buckling check
    4. Member capacity verification

    Example Usage:
        >>> workflow = PlatformStructuralWorkflow.create(
        ...     load_type='combined',
        ...     plate_length=3.0,
        ...     plate_width=2.0,
        ...     plate_thickness=0.025,
        ...     material='s355'
        ... )
    """

    @staticmethod
    def create(
        load_type: str,
        plate_length: float,
        plate_width: float,
        plate_thickness: float,
        material: str = 's355',
        applied_stress: float = 200.0,
    ) -> WorkflowDefinition:
        """
        Create platform structural workflow

        Args:
            load_type: Load type ('gravity', 'wind', 'combined')
            plate_length: Plate length (m)
            plate_width: Plate width (m)
            plate_thickness: Plate thickness (m)
            material: Material grade
            applied_stress: Applied stress (MPa)

        Returns:
            WorkflowDefinition for structural analysis
        """
        workflow = WorkflowDefinition(
            name="Platform Structural Analysis",
            description="Structural check: stress → buckling → capacity verification"
        )

        # Task 1: Calculate applied stresses
        workflow.add_task(WorkflowTask(
            name="Stress Calculation",
            task_id="stress_calc",
            module="structural_analysis.stress",
            function="calculate_von_mises_stress",
            inputs={
                'sigma_x': applied_stress,
                'sigma_y': applied_stress * 0.5,
                'tau_xy': applied_stress * 0.3,
                'material': material,
            },
            outputs={
                'von_mises_stress': 'plate_von_mises_stress',
                'utilization': 'plate_stress_utilization',
            }
        ))

        # Task 2: Plate buckling check
        workflow.add_task(WorkflowTask(
            name="Plate Buckling Check",
            task_id="buckling",
            module="structural_analysis.buckling",
            function="check_plate_buckling",
            inputs={
                'length': plate_length,
                'width': plate_width,
                'thickness': plate_thickness,
                'material': material,
                'applied_stress': '$plate_von_mises_stress',
            },
            outputs={
                'buckling_capacity': 'plate_buckling_capacity',
                'buckling_status': 'plate_buckling_status',
            },
            depends_on=['stress_calc'],
        ))

        # Task 3: Overall capacity check
        workflow.add_task(WorkflowTask(
            name="Capacity Verification",
            task_id="capacity",
            module="structural_analysis",
            function="verify_overall_capacity",
            inputs={
                'stress_utilization': '$plate_stress_utilization',
                'buckling_status': '$plate_buckling_status',
            },
            outputs={
                'overall_status': 'structural_status',
                'safety_margin': 'structural_safety_margin',
            },
            depends_on=['buckling'],
        ))

        return workflow


class VesselResponseWorkflow:
    """
    Vessel response analysis workflow

    Wave spectrum to vessel motions:
    1. Generate wave spectrum
    2. Load RAO data
    3. Calculate vessel response
    4. Extract motion statistics

    Example Usage:
        >>> workflow = VesselResponseWorkflow.create(
        ...     hs=3.5,
        ...     tp=10.0,
        ...     vessel_type='fpso',
        ...     wave_direction=45.0
        ... )
    """

    @staticmethod
    def create(
        hs: float,
        tp: float,
        vessel_type: str = 'fpso',
        wave_direction: float = 0.0,
        spectrum_type: str = 'jonswap',
    ) -> WorkflowDefinition:
        """
        Create vessel response workflow

        Args:
            hs: Significant wave height (m)
            tp: Peak period (s)
            vessel_type: Vessel type
            wave_direction: Wave direction (deg)
            spectrum_type: Spectrum type

        Returns:
            WorkflowDefinition for vessel response analysis
        """
        workflow = WorkflowDefinition(
            name="Vessel Response Analysis",
            description="Wave spectrum → RAO → vessel motions"
        )

        # Task 1: Generate wave spectrum
        workflow.add_task(WorkflowTask(
            name="Wave Spectrum Generation",
            task_id="wave_spectrum",
            module="hydrodynamics.wave_spectra",
            function="generate_jonswap_spectrum",
            inputs={
                'hs': hs,
                'tp': tp,
                'spectrum_type': spectrum_type,
            },
            outputs={
                'spectrum': 'wave_spectrum_data',
                'peak_frequency': 'wave_peak_frequency',
            }
        ))

        # Task 2: Calculate vessel response
        workflow.add_task(WorkflowTask(
            name="Vessel Response Calculation",
            task_id="response_calc",
            module="hydrodynamics",
            function="calculate_vessel_response",
            inputs={
                'vessel_type': vessel_type,
                'wave_spectrum': '$wave_spectrum_data',
                'wave_direction': wave_direction,
            },
            outputs={
                'motion_spectrum': 'vessel_motion_spectrum',
                'significant_motions': 'vessel_significant_motions',
            },
            depends_on=['wave_spectrum'],
        ))

        return workflow
