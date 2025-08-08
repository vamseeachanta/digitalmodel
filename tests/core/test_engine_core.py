# Standard library imports
import os
import sys
from pathlib import Path
from unittest.mock import patch, MagicMock, mock_open

# Third party imports
import pytest

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from digitalmodel import engine


class TestEngineCore:
    """Test suite for digitalmodel.engine core functionality."""

    @patch('digitalmodel.engine.wwyaml')
    @patch('digitalmodel.engine.app_manager')
    def test_engine_with_valid_input_file(self, mock_app_manager, mock_wwyaml):
        """Test engine function with valid input file."""
        # Mock setup
        mock_app_manager.validate_arguments_run_methods.return_value = ("test.yml", {})
        mock_wwyaml.ymlInput.return_value = {
            "basename": "test",
            "Analysis": {"type": "test"}
        }
        
        # Mock all the module classes to avoid import issues
        with patch.multiple(
            'digitalmodel.engine',
            Aqwa=MagicMock(),
            CathodicProtection=MagicMock(),
            DNVRPH103_hydrodynamics_circular=MagicMock(),
            DNVRPH103_hydrodynamics_rectangular=MagicMock(),
            FatigueAnalysis=MagicMock(),
            ShipDesign=MagicMock(),
            Mooring=MagicMock(),
            OrcaFlex=MagicMock(),
            OrcaflexFileManagement=MagicMock(),
            OrcInstallation=MagicMock(),
            OrcModalAnalysis=MagicMock(),
            UmbilicalAnalysis=MagicMock(),
            PipeCapacity=MagicMock(),
            Pipeline=MagicMock(),
            RAOAnalysis=MagicMock(),
            TimeSeriesAnalysis=MagicMock(),
            Transformation=MagicMock(),
            vertical_riser=MagicMock(),
            VIVAnalysis=MagicMock()
        ):
            # Test the engine function
            result = engine.engine("test.yml")
            
            # Assertions
            assert isinstance(result, dict)
            mock_app_manager.validate_arguments_run_methods.assert_called_once_with("test.yml")
            mock_wwyaml.ymlInput.assert_called_once_with("test.yml", updateYml=None)

    @patch('digitalmodel.engine.wwyaml')
    @patch('digitalmodel.engine.app_manager')
    def test_engine_with_cfg_parameter(self, mock_app_manager, mock_wwyaml):
        """Test engine function with cfg parameter provided."""
        test_cfg = {
            "basename": "test",
            "Analysis": {"type": "test"}
        }
        
        # Mock all the module classes
        with patch.multiple(
            'digitalmodel.engine',
            Aqwa=MagicMock(),
            CathodicProtection=MagicMock(),
            DNVRPH103_hydrodynamics_circular=MagicMock(),
            DNVRPH103_hydrodynamics_rectangular=MagicMock(),
            FatigueAnalysis=MagicMock(),
            ShipDesign=MagicMock(),
            Mooring=MagicMock(),
            OrcaFlex=MagicMock(),
            OrcaflexFileManagement=MagicMock(),
            OrcInstallation=MagicMock(),
            OrcModalAnalysis=MagicMock(),
            UmbilicalAnalysis=MagicMock(),
            PipeCapacity=MagicMock(),
            Pipeline=MagicMock(),
            RAOAnalysis=MagicMock(),
            TimeSeriesAnalysis=MagicMock(),
            Transformation=MagicMock(),
            vertical_riser=MagicMock(),
            VIVAnalysis=MagicMock()
        ):
            # Test with cfg provided
            result = engine.engine(cfg=test_cfg)
            
            # Assertions
            assert isinstance(result, dict)
            # Should not call validation or ymlInput when cfg is provided
            mock_app_manager.validate_arguments_run_methods.assert_not_called()
            mock_wwyaml.ymlInput.assert_not_called()

    @patch('digitalmodel.engine.wwyaml')
    @patch('digitalmodel.engine.app_manager')
    def test_engine_error_handling_none_cfg(self, mock_app_manager, mock_wwyaml):
        """Test engine function error handling when cfg is None."""
        # Mock setup to return None
        mock_app_manager.validate_arguments_run_methods.return_value = ("test.yml", {})
        mock_wwyaml.ymlInput.return_value = None
        
        # Test that ValueError is raised
        with pytest.raises(ValueError, match="cfg is None"):
            engine.engine("test.yml")

    def test_engine_constants(self):
        """Test engine module constants."""
        assert hasattr(engine, 'library_name')
        assert engine.library_name == "digitalmodel"
        
        assert hasattr(engine, 'wwyaml')
        assert hasattr(engine, 'app_manager')

    @patch('digitalmodel.engine.wwyaml')
    @patch('digitalmodel.engine.app_manager')
    def test_engine_config_flag_parameter(self, mock_app_manager, mock_wwyaml):
        """Test engine function with config_flag parameter."""
        test_cfg = {"basename": "test", "Analysis": {"type": "test"}}
        mock_wwyaml.ymlInput.return_value = test_cfg
        mock_app_manager.validate_arguments_run_methods.return_value = ("test.yml", {})
        
        with patch.multiple(
            'digitalmodel.engine',
            Aqwa=MagicMock(),
            CathodicProtection=MagicMock(),
            DNVRPH103_hydrodynamics_circular=MagicMock(),
            DNVRPH103_hydrodynamics_rectangular=MagicMock(),
            FatigueAnalysis=MagicMock(),
            ShipDesign=MagicMock(),
            Mooring=MagicMock(),
            OrcaFlex=MagicMock(),
            OrcaflexFileManagement=MagicMock(),
            OrcInstallation=MagicMock(),
            OrcModalAnalysis=MagicMock(),
            UmbilicalAnalysis=MagicMock(),
            PipeCapacity=MagicMock(),
            Pipeline=MagicMock(),
            RAOAnalysis=MagicMock(),
            TimeSeriesAnalysis=MagicMock(),
            Transformation=MagicMock(),
            vertical_riser=MagicMock(),
            VIVAnalysis=MagicMock()
        ):
            # Test with config_flag=True
            result1 = engine.engine("test.yml", config_flag=True)
            assert isinstance(result1, dict)
            
            # Test with config_flag=False  
            result2 = engine.engine("test.yml", config_flag=False)
            assert isinstance(result2, dict)

    def test_engine_import_structure(self):
        """Test that engine module imports are properly structured."""
        # Test that required classes are imported
        required_classes = [
            'Aqwa', 'CathodicProtection', 'DNVRPH103_hydrodynamics_circular',
            'DNVRPH103_hydrodynamics_rectangular', 'FatigueAnalysis', 'ShipDesign',
            'Mooring', 'OrcaFlex', 'OrcaflexFileManagement', 'OrcInstallation',
            'OrcModalAnalysis', 'UmbilicalAnalysis', 'PipeCapacity', 'Pipeline',
            'RAOAnalysis', 'TimeSeriesAnalysis', 'Transformation', 'vertical_riser',
            'VIVAnalysis'
        ]
        
        import digitalmodel.engine as engine_module
        
        for class_name in required_classes:
            assert hasattr(engine_module, class_name), f"Missing import: {class_name}"

    @patch('digitalmodel.engine.logger')
    def test_engine_logging_integration(self, mock_logger):
        """Test that engine integrates with logging properly."""
        # Test that logger is imported and available
        assert hasattr(engine, 'logger')

    def test_engine_function_signature(self):
        """Test engine function has correct signature."""
        import inspect
        
        sig = inspect.signature(engine.engine)
        params = list(sig.parameters.keys())
        
        expected_params = ['inputfile', 'cfg', 'config_flag']
        assert params == expected_params
        
        # Check parameter defaults
        assert sig.parameters['inputfile'].default is None
        assert sig.parameters['cfg'].default is None
        assert sig.parameters['config_flag'].default is True

    def test_engine_return_type(self):
        """Test that engine function returns dict type."""
        import inspect
        
        sig = inspect.signature(engine.engine)
        return_annotation = sig.return_annotation
        
        # Should return dict type
        assert return_annotation == dict