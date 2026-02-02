#!/usr/bin/env python3
"""
Test Suite for Reference Seastate Processor

Tests the complete reference seastate scaling fatigue analysis pipeline
including configuration handling, data loading, and fatigue calculations.
"""

import pytest
import numpy as np
import pandas as pd
from pathlib import Path
import tempfile
import json
from unittest.mock import Mock, patch, MagicMock

from digitalmodel.fatigue_analysis import (
    ReferenceSeaStateProcessor,
    Configuration,
    FatigueCondition,
    FatigueResult,
    TensionToStressConverter
)


class TestTensionToStressConverter:
    """Test suite for tension to stress conversion"""
    
    def test_default_linear_conversion(self):
        """Test default linear tension-stress relationship"""
        converter = TensionToStressConverter()
        
        # Default should be Stress = Tension / 4
        assert converter.convert_to_stress(0) == 0
        assert converter.convert_to_stress(100) == 25
        assert converter.convert_to_stress(2000) == 500
    
    def test_interpolation(self):
        """Test interpolation between lookup table values"""
        converter = TensionToStressConverter()
        
        # Test intermediate values
        stress_500 = converter.convert_to_stress(500)
        assert 0 < stress_500 < 500
        assert abs(stress_500 - 125) < 1  # Should be ~125 for linear
    
    def test_array_conversion(self):
        """Test converting array of tensions"""
        converter = TensionToStressConverter()
        
        tensions = np.array([0, 100, 500, 1000, 2000])
        stresses = converter.convert_array(tensions)
        
        assert len(stresses) == len(tensions)
        assert stresses[0] == 0
        assert stresses[-1] == 500
        assert np.all(stresses >= 0)
    
    def test_custom_lookup_table(self, tmp_path):
        """Test loading custom lookup table"""
        # Create custom lookup table
        lookup_file = tmp_path / "lookup.csv"
        df = pd.DataFrame({
            'Tension Range (kN)': [0, 500, 1000, 1500, 2000],
            'Stress Range (Mpa)': [0, 100, 250, 400, 500]
        })
        df.to_csv(lookup_file, index=False)
        
        converter = TensionToStressConverter(str(lookup_file))
        
        # Test conversion with custom table
        assert converter.convert_to_stress(0) == 0
        assert converter.convert_to_stress(500) == 100
        assert converter.convert_to_stress(1000) == 250
        assert converter.convert_to_stress(2000) == 500


class TestConfiguration:
    """Test suite for Configuration dataclass"""
    
    def test_configuration_creation(self):
        """Test creating configuration"""
        config = Configuration(
            name="test_config",
            weight=25.0,
            description="Test configuration"
        )
        
        assert config.name == "test_config"
        assert config.weight == 25.0
        assert config.description == "Test configuration"


class TestFatigueCondition:
    """Test suite for FatigueCondition dataclass"""
    
    def test_fatigue_condition_creation(self):
        """Test creating fatigue condition"""
        condition = FatigueCondition(
            id=1,
            wind_speed=10.0,
            wind_dir=45.0,
            hs=2.5,
            tp=8.0,
            wave_dir=90.0,
            occurrence=5.5
        )
        
        assert condition.id == 1
        assert condition.wind_speed == 10.0
        assert condition.wind_dir == 45.0
        assert condition.hs == 2.5
        assert condition.tp == 8.0
        assert condition.wave_dir == 90.0
        assert condition.occurrence == 5.5


class TestReferenceSeaStateProcessor:
    """Test suite for main processor"""
    
    def test_initialization(self, tmp_path):
        """Test processor initialization"""
        processor = ReferenceSeaStateProcessor(
            data_path=str(tmp_path),
            output_path=str(tmp_path / "output"),
            scf=1.2,
            design_life_years=25.0
        )
        
        assert processor.data_path == tmp_path
        assert processor.base_wind_speed == 10.0
        assert processor.base_hs == 0.5
        assert processor.fatigue_calculator.scf == 1.2
        assert processor.fatigue_calculator.design_life_years == 25.0
    
    def test_default_configurations(self, tmp_path):
        """Test default vessel configurations"""
        processor = ReferenceSeaStateProcessor(data_path=str(tmp_path))
        
        # Should have 4 default configurations
        assert len(processor.configurations) == 4
        assert 'fsts_l015' in processor.configurations
        assert 'fsts_l095' in processor.configurations
        assert 'fsts_l015_125km3_l100_pb' in processor.configurations
        assert 'fsts_l095_125km3_l000_pb' in processor.configurations
        
        # Check weights sum to 100%
        total_weight = sum(c.weight for c in processor.configurations.values())
        assert abs(total_weight - 100.0) < 0.01
    
    def test_create_sample_conditions(self, tmp_path):
        """Test creating sample fatigue conditions"""
        processor = ReferenceSeaStateProcessor(data_path=str(tmp_path))
        conditions = processor._create_sample_conditions()
        
        assert len(conditions) == 3
        assert all(isinstance(c, FatigueCondition) for c in conditions)
        assert conditions[0].id == 1
        assert conditions[-1].id == 3
    
    def test_load_fatigue_conditions_csv(self, tmp_path):
        """Test loading fatigue conditions from CSV"""
        # Create test CSV
        conditions_file = tmp_path / "conditions.csv"
        df = pd.DataFrame({
            'Row': [1, 2],
            'Wind Speed (m/s)': [5.0, 15.0],
            'Wind Dir (°)': [0, 180],
            'Hs (m)': [0.5, 2.5],
            'Tp (s)': [4.0, 12.0],
            'Wave Dir (°)': [45, 225],
            'Occurrence (%)': [10.0, 5.0]
        })
        df.to_csv(conditions_file, index=False)
        
        processor = ReferenceSeaStateProcessor(data_path=str(tmp_path))
        conditions = processor.load_fatigue_conditions(str(conditions_file))
        
        assert len(conditions) == 2
        assert conditions[0].wind_speed == 5.0
        assert conditions[1].wind_speed == 15.0
        assert conditions[0].occurrence == 10.0
    
    def test_select_reference_seastate(self, tmp_path):
        """Test selecting closest reference seastate"""
        # Create mock directory structure
        config_path = tmp_path / "test_config"
        config_path.mkdir()
        
        # Create reference directories
        (config_path / "wind_000deg").mkdir()
        (config_path / "wind_090deg").mkdir()
        (config_path / "wind_180deg").mkdir()
        (config_path / "wave_000deg_Hs050cm_Tp270cs").mkdir()
        (config_path / "wave_090deg_Hs050cm_Tp347cs").mkdir()
        
        processor = ReferenceSeaStateProcessor(data_path=str(tmp_path))
        
        condition = FatigueCondition(
            id=1,
            wind_speed=10,
            wind_dir=45,  # Closest to 90
            hs=1.0,
            tp=3.0,  # Closest to 3.47
            wave_dir=45,  # Closest to 90
            occurrence=5.0
        )
        
        wind_ref, wave_ref = processor.select_reference_seastate("test_config", condition)
        
        assert "090deg" in wind_ref  # Closest to 45 degrees
        assert "090deg" in wave_ref
        assert "347cs" in wave_ref  # Closest Tp
    
    def test_find_closest_direction(self, tmp_path):
        """Test finding closest direction reference"""
        processor = ReferenceSeaStateProcessor(data_path=str(tmp_path))
        
        refs = ["wind_000deg", "wind_090deg", "wind_180deg", "wind_270deg"]
        
        # Test various target directions
        assert processor._find_closest_direction(refs, 0) == "wind_000deg"
        assert processor._find_closest_direction(refs, 45) == "wind_090deg"
        assert processor._find_closest_direction(refs, 135) == "wind_180deg"
        assert processor._find_closest_direction(refs, 315) == "wind_270deg"
        assert processor._find_closest_direction(refs, 359) == "wind_000deg"  # Wrap around
    
    def test_calculate_overall_fatigue_life(self, tmp_path):
        """Test overall fatigue life calculation"""
        processor = ReferenceSeaStateProcessor(data_path=str(tmp_path))
        
        config_summaries = [
            {
                'config_name': 'config1',
                'weight_pct': 50.0,
                'min_fatigue_life': 20.0
            },
            {
                'config_name': 'config2',
                'weight_pct': 50.0,
                'min_fatigue_life': 30.0
            }
        ]
        
        overall_life = processor.calculate_overall_fatigue_life(config_summaries)
        
        # Weighted average: 1/(0.5/20 + 0.5/30) = 24
        assert abs(overall_life - 24.0) < 0.1
    
    def test_save_results(self, tmp_path):
        """Test saving analysis results"""
        processor = ReferenceSeaStateProcessor(
            data_path=str(tmp_path),
            output_path=str(tmp_path / "output")
        )
        
        # Add mock results
        processor.results = [
            FatigueResult(
                config="test_config",
                strut=1,
                condition_id=1,
                tension_ranges=100,
                max_tension=1000,
                min_tension=500,
                mean_tension=750,
                max_stress_range=50,
                damage=0.001,
                weighted_damage=0.0001,
                annual_damage=0.01,
                fatigue_life_years=100,
                occurrence_pct=10.0
            )
        ]
        
        processor.summary = {
            'timestamp': '2025-01-20T12:00:00',
            'configurations': [],
            'overall_fatigue_life': 25.0
        }
        
        processor.save_results()
        
        # Check files created
        output_path = tmp_path / "output"
        assert (output_path / "fatigue_analysis_results.csv").exists()
        assert (output_path / "analysis_summary.json").exists()
        assert (output_path / "fatigue_analysis_report.md").exists()
        
        # Verify CSV content
        df = pd.read_csv(output_path / "fatigue_analysis_results.csv")
        assert len(df) == 1
        assert df.iloc[0]['config'] == "test_config"
        
        # Verify JSON content
        with open(output_path / "analysis_summary.json") as f:
            summary = json.load(f)
        assert summary['overall_fatigue_life'] == 25.0


class TestProcessingIntegration:
    """Integration tests for complete processing pipeline"""
    
    @pytest.fixture
    def mock_data_structure(self, tmp_path):
        """Create mock data directory structure"""
        # Create configuration directories
        configs = ['fsts_l015', 'fsts_l095']
        
        for config in configs:
            config_path = tmp_path / config
            config_path.mkdir()
            
            # Create reference directories
            refs = [
                'wind_000deg',
                'wind_090deg',
                'wave_000deg_Hs050cm_Tp270cs',
                'wave_090deg_Hs050cm_Tp270cs'
            ]
            
            for ref in refs:
                ref_path = config_path / ref
                ref_path.mkdir()
                
                # Create strut files
                for strut in [1, 2]:
                    strut_file = ref_path / f"Strut{strut}.csv"
                    
                    # Create mock CSV data
                    t = np.linspace(0, 100, 1000)
                    if 'wind' in ref:
                        tension = 100 + 20*np.sin(2*np.pi*t/10)
                    else:
                        tension = 50 + 10*np.sin(2*np.pi*t/5)
                    
                    df = pd.DataFrame({
                        'Time (s)': t,
                        'FST Vessel End Effective Tension (kN)': tension
                    })
                    df.to_csv(strut_file, index=False)
        
        return tmp_path
    
    def test_load_strut_data(self, mock_data_structure):
        """Test loading strut data from CSV"""
        processor = ReferenceSeaStateProcessor(data_path=str(mock_data_structure))
        
        time_data, tension_data = processor.load_strut_data(
            'fsts_l015', 'wind_000deg', 1
        )
        
        assert len(time_data) == 1000
        assert len(tension_data) == 1000
        assert np.mean(tension_data) > 90  # Should be around 100
    
    def test_process_fatigue_condition(self, mock_data_structure):
        """Test processing single fatigue condition"""
        processor = ReferenceSeaStateProcessor(
            data_path=str(mock_data_structure),
            sample_timesteps=100  # Use fewer timesteps for speed
        )
        
        condition = FatigueCondition(
            id=1,
            wind_speed=20.0,  # 2x base wind
            wind_dir=0,
            hs=1.0,  # 2x base wave
            tp=2.7,
            wave_dir=0,
            occurrence=10.0
        )
        
        result = processor.process_fatigue_condition('fsts_l015', condition, 1)
        
        assert result is not None
        assert result.config == 'fsts_l015'
        assert result.strut == 1
        assert result.condition_id == 1
        assert result.tension_ranges > 0
        assert result.max_tension > result.min_tension
        assert result.annual_damage > 0
        assert result.fatigue_life_years > 0
    
    def test_process_configuration(self, mock_data_structure):
        """Test processing entire configuration"""
        processor = ReferenceSeaStateProcessor(
            data_path=str(mock_data_structure),
            sample_timesteps=100
        )
        
        conditions = [
            FatigueCondition(1, 10, 0, 0.5, 2.7, 0, 50.0),
            FatigueCondition(2, 15, 90, 1.0, 2.7, 90, 50.0)
        ]
        
        summary = processor.process_configuration('fsts_l015', conditions, [1, 2])
        
        assert summary['config_name'] == 'fsts_l015'
        assert 'critical_strut' in summary
        assert 'min_fatigue_life' in summary
        assert 'strut_lives' in summary
        assert len(summary['strut_lives']) == 2
    
    def test_full_analysis_pipeline(self, mock_data_structure):
        """Test complete analysis pipeline"""
        processor = ReferenceSeaStateProcessor(
            data_path=str(mock_data_structure),
            output_path=str(mock_data_structure / "output"),
            sample_timesteps=100
        )
        
        # Run limited analysis
        success = processor.run_analysis(
            conditions=[
                FatigueCondition(1, 10, 0, 0.5, 2.7, 0, 100.0)
            ],
            strut_nums=[1],
            config_names=['fsts_l015']
        )
        
        assert success
        assert len(processor.results) > 0
        assert processor.summary['total_results'] > 0
        
        # Check output files
        output_path = mock_data_structure / "output"
        assert (output_path / "fatigue_analysis_results.csv").exists()
        assert (output_path / "analysis_summary.json").exists()


class TestErrorHandling:
    """Test error handling and edge cases"""
    
    def test_missing_data_path(self, tmp_path):
        """Test handling of missing data path"""
        processor = ReferenceSeaStateProcessor(
            data_path=str(tmp_path / "nonexistent")
        )
        
        # Should handle gracefully
        time_data, tension_data = processor.load_strut_data(
            'config', 'ref', 1
        )
        
        assert len(time_data) == 0
        assert len(tension_data) == 0
    
    def test_empty_reference_selection(self, tmp_path):
        """Test reference selection with no references"""
        processor = ReferenceSeaStateProcessor(data_path=str(tmp_path))
        
        condition = FatigueCondition(1, 10, 0, 0.5, 2.7, 0, 10)
        wind_ref, wave_ref = processor.select_reference_seastate(
            'nonexistent', condition
        )
        
        assert wind_ref == ""
        assert wave_ref == ""
    
    def test_invalid_csv_format(self, tmp_path):
        """Test handling of invalid CSV format"""
        # Create config directory
        config_path = tmp_path / "test_config"
        config_path.mkdir()
        ref_path = config_path / "wind_000deg"
        ref_path.mkdir()
        
        # Create invalid CSV
        strut_file = ref_path / "Strut1.csv"
        strut_file.write_text("Invalid,CSV,Content\n1,2,3")
        
        processor = ReferenceSeaStateProcessor(data_path=str(tmp_path))
        time_data, tension_data = processor.load_strut_data(
            'test_config', 'wind_000deg', 1
        )
        
        # Should return empty arrays on error
        assert len(time_data) == 0
        assert len(tension_data) == 0


if __name__ == "__main__":
    pytest.main([__file__, "-v"])