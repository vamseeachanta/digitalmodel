"""Tests for data models in comprehensive mooring analysis."""

import pytest
import pandas as pd
import numpy as np
from pathlib import Path
from datetime import datetime

from digitalmodel.modules.orcaflex.mooring_analysis.comprehensive_analysis.models import (
    PretensionLineData,
    PretensionData,
    PretensionMetrics,
    ConvergenceStatus,
    StiffnessLineData,
    StiffnessData,
    StiffnessMatrix,
    StiffnessMetrics,
    FenderData,
    FenderForceData,
    FenderMetrics,
    AnalysisResults,
    GroupStatistics,
    GroupComparison,
    ContextInfo,
    IndividualSummary,
    GroupSummary,
    OverallSummary,
    ComprehensiveResults
)


class TestPretensionModels:
    """Test suite for pretension data models."""
    
    def test_pretension_line_data_creation(self):
        """Test creation of PretensionLineData."""
        line_data = PretensionLineData(
            object_name="Line01",
            target_tension=80.0,
            current_tension=85.0,
            line_length=100.0,
            calculated_length=101.0,
            new_line_length=102.0,
            tension_diff_percent=6.25,
            converged=False,
            end_forces=(10.0, 20.0, 30.0),
            line_EA=47700.0,
            iterations=5
        )
        
        assert line_data.object_name == "Line01"
        assert line_data.target_tension == 80.0
        assert line_data.tension_diff_percent == 6.25
        assert not line_data.converged
        assert line_data.end_forces == (10.0, 20.0, 30.0)
    
    def test_pretension_data_properties(self, sample_pretension_dataframe):
        """Test PretensionData properties and calculations."""
        lines = [
            PretensionLineData(
                object_name=f"Line{i:02d}",
                target_tension=80.0,
                current_tension=82.0 if i < 3 else 85.0,
                line_length=100.0,
                calculated_length=101.0,
                new_line_length=102.0,
                tension_diff_percent=2.5 if i < 3 else 6.25,
                converged=(i < 3),  # First 3 lines converged
                end_forces=(10.0, 20.0, 30.0)
            )
            for i in range(5)
        ]
        
        pretension_data = PretensionData(
            filename=Path("test.csv"),
            timestamp=datetime.now(),
            lines=lines,
            raw_dataframe=sample_pretension_dataframe
        )
        
        assert pretension_data.num_lines == 5
        assert pretension_data.num_converged == 3
        assert pretension_data.convergence_rate == 60.0  # 3/5 * 100
    
    def test_pretension_metrics_initialization(self):
        """Test PretensionMetrics initialization."""
        metrics = PretensionMetrics(
            mean_convergence=0.95,
            std_convergence=0.02,
            max_deviation=0.10,
            min_deviation=0.01,
            converged_lines=["Line01", "Line02"],
            problem_lines=["Line03"],
            total_lines=3,
            tension_distribution={'mean': 82.5, 'std': 5.2},
            convergence_percentage=66.67,
            average_tension=82.5,
            tension_range=(75.0, 90.0),
            recommendations=["Adjust Line03 pretension"]
        )
        
        assert metrics.mean_convergence == 0.95
        assert len(metrics.converged_lines) == 2
        assert len(metrics.problem_lines) == 1
        assert metrics.convergence_percentage == 66.67
    
    def test_convergence_status(self):
        """Test ConvergenceStatus model."""
        status = ConvergenceStatus(
            overall_converged=False,
            convergence_quality="acceptable",
            critical_lines=["Line03", "Line04"],
            adjustment_needed={"Line03": 5.0, "Line04": -3.0},
            estimated_iterations={"Line03": 3, "Line04": 2},
            confidence_level=85.0
        )
        
        assert not status.overall_converged
        assert status.convergence_quality == "acceptable"
        assert len(status.critical_lines) == 2
        assert status.adjustment_needed["Line03"] == 5.0


class TestStiffnessModels:
    """Test suite for stiffness data models."""
    
    def test_stiffness_line_data_creation(self):
        """Test creation of StiffnessLineData."""
        line_data = StiffnessLineData(
            object_name="Line01",
            k_axial=1500.0,
            k_x=250.0,
            k_y=180.0,
            k_z=320.0,
            k_xy=12.5,
            k_xz=8.3,
            k_yz=6.2,
            direction_cosines=(0.707, 0.707, 0.0),
            forces=(60.0, -140.0, 28.0)
        )
        
        assert line_data.object_name == "Line01"
        assert line_data.k_axial == 1500.0
        assert line_data.direction_cosines == (0.707, 0.707, 0.0)
    
    def test_stiffness_matrix_properties(self):
        """Test StiffnessMatrix properties."""
        matrix = np.array([
            [1000, 50, 30, 0, 0, 0],
            [50, 800, 40, 0, 0, 0],
            [30, 40, 1200, 0, 0, 0],
            [0, 0, 0, 500, 20, 10],
            [0, 0, 0, 20, 400, 15],
            [0, 0, 0, 10, 15, 600]
        ])
        
        eigenvalues = np.linalg.eigvals(matrix)
        
        stiffness_matrix = StiffnessMatrix(
            matrix=matrix,
            translational=matrix[:3, :3],
            rotational=matrix[3:, 3:],
            eigenvalues=eigenvalues,
            condition_number=np.linalg.cond(matrix)
        )
        
        assert stiffness_matrix.matrix.shape == (6, 6)
        assert stiffness_matrix.is_positive_definite  # All eigenvalues > 0
        assert stiffness_matrix.principal_stiffnesses is not None
    
    def test_stiffness_metrics(self):
        """Test StiffnessMetrics model."""
        matrix = np.eye(6) * 1000
        stiffness_matrix = StiffnessMatrix(
            matrix=matrix,
            translational=matrix[:3, :3]
        )
        
        metrics = StiffnessMetrics(
            stiffness_matrix=stiffness_matrix,
            dominant_direction=(1.0, 0.0, 0.0),
            stiffness_ratios={'x/y': 1.0, 'x/z': 1.0, 'y/z': 1.0},
            anisotropy_factor=1.0,
            critical_lines=["Line01", "Line04"],
            natural_periods={'surge': 120.0, 'sway': 120.0, 'heave': 80.0}
        )
        
        assert metrics.anisotropy_factor == 1.0
        assert metrics.natural_periods['surge'] == 120.0


class TestFenderModels:
    """Test suite for fender force data models."""
    
    def test_fender_data_creation(self):
        """Test creation of FenderData."""
        fender = FenderData(
            fender_id="F1",
            location=(10.0, 5.0, 0.0),
            forces=[2000.0, 2200.0, 2500.0, 2100.0],
            max_force=2500.0,
            mean_force=2200.0,
            contact_events=4,
            contact_duration=45.5,
            compression=0.85
        )
        
        assert fender.fender_id == "F1"
        assert fender.max_force == 2500.0
        assert fender.contact_duration == 45.5
    
    def test_fender_force_data_properties(self):
        """Test FenderForceData properties."""
        fenders = [
            FenderData(
                fender_id=f"F{i}",
                location=(10.0*i, 5.0, 0.0),
                forces=[],
                max_force=2500.0 - i*100,
                mean_force=2000.0 - i*50,
                contact_events=5-i,
                contact_duration=50.0 - i*5
            )
            for i in range(1, 5)
        ]
        
        fender_data = FenderForceData(
            filename=Path("test.csv"),
            timestamp=datetime.now(),
            fenders=fenders,
            raw_dataframe=pd.DataFrame()
        )
        
        assert fender_data.num_fenders == 4
        assert fender_data.total_max_force == 9000.0  # Sum of max forces (2400+2300+2200+2100)
    
    def test_fender_metrics(self):
        """Test FenderMetrics model."""
        metrics = FenderMetrics(
            utilization_rates={'F1': 50.0, 'F2': 46.0, 'F3': 36.0, 'F4': 0.0},
            max_forces={'F1': 2500.0, 'F2': 2300.0, 'F3': 1800.0, 'F4': 0.0},
            mean_forces={'F1': 1200.0, 'F2': 1100.0, 'F3': 850.0, 'F4': 0.0},
            critical_fenders=['F1'],
            load_sharing={'F1': 38.5, 'F2': 35.4, 'F3': 26.1, 'F4': 0.0},
            contact_percentages={'F1': 45.5, 'F2': 42.3, 'F3': 35.2, 'F4': 0.0},
            force_distribution_stats={'mean': 1037.5, 'std': 523.4},
            overloaded_fenders=[],
            design_margin={'F1': 50.0, 'F2': 54.0, 'F3': 64.0, 'F4': 100.0}
        )
        
        assert metrics.utilization_rates['F1'] == 50.0
        assert len(metrics.critical_fenders) == 1
        assert metrics.design_margin['F4'] == 100.0


class TestComprehensiveModels:
    """Test suite for comprehensive analysis models."""
    
    def test_context_info(self):
        """Test ContextInfo model."""
        context = ContextInfo(
            vessel_type="LNGC",
            water_depth=150.0,
            environment="operational",
            loading_condition="ballast",
            return_period=100,
            analysis_type="static",
            mooring_configuration="spread",
            custom_attributes={'wind_speed': 25.0},
            confidence_score=0.95
        )
        
        assert context.vessel_type == "LNGC"
        assert context.water_depth == 150.0
        assert context.confidence_score == 0.95
    
    def test_analysis_results(self):
        """Test AnalysisResults model."""
        context = ContextInfo(vessel_type="LNGC")
        
        results = AnalysisResults(
            run_id="run_001",
            filename=Path("test.csv"),
            context=context,
            pretension_metrics=None,
            stiffness_metrics=None,
            fender_metrics=None
        )
        
        assert results.run_id == "run_001"
        assert not results.has_all_analyses
    
    def test_comprehensive_results_success_rate(self):
        """Test ComprehensiveResults success rate calculation."""
        from digitalmodel.modules.orcaflex.mooring_analysis.comprehensive_analysis.config import AnalysisConfig
        
        config = AnalysisConfig()
        context = ContextInfo()
        
        # Create mix of complete and incomplete results
        individual_results = {}
        for i in range(5):
            results = AnalysisResults(
                run_id=f"run_{i:03d}",
                filename=Path(f"test_{i}.csv"),
                context=context,
                pretension_metrics=PretensionMetrics(
                    mean_convergence=0.95,
                    std_convergence=0.02,
                    max_deviation=0.1,
                    min_deviation=0.01,
                    converged_lines=[],
                    problem_lines=[],
                    total_lines=10,
                    tension_distribution={},
                    convergence_percentage=90.0,
                    average_tension=80.0,
                    tension_range=(70.0, 90.0)
                ) if i < 3 else None
            )
            individual_results[f"run_{i:03d}"] = results
        
        comprehensive = ComprehensiveResults(
            config=config,
            individual_results=individual_results
        )
        
        assert comprehensive.success_rate == 0.0  # None have all three analyses
    
    def test_group_statistics(self):
        """Test GroupStatistics model."""
        stats = GroupStatistics(
            group_name="ballast_operational",
            num_runs=10,
            metrics={
                'convergence': {'mean': 0.95, 'std': 0.02, 'min': 0.90, 'max': 0.99, 'median': 0.95},
                'max_tension': {'mean': 85.0, 'std': 5.0, 'min': 75.0, 'max': 95.0, 'median': 85.0}
            },
            best_run="run_003",
            worst_run="run_007",
            trends=["Improving convergence over time"],
            outliers=["run_009"]
        )
        
        assert stats.group_name == "ballast_operational"
        assert stats.num_runs == 10
        assert stats.metrics['convergence']['mean'] == 0.95


class TestModelValidation:
    """Test validation methods for data models."""
    
    def test_pretension_data_validation(self):
        """Test validation of pretension data."""
        # This will be implemented when we add validation methods
        pass
    
    def test_stiffness_data_validation(self):
        """Test validation of stiffness data."""
        # This will be implemented when we add validation methods
        pass
    
    def test_fender_data_validation(self):
        """Test validation of fender data."""
        # This will be implemented when we add validation methods
        pass