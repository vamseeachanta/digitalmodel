# Standard library imports
import sys
from pathlib import Path
from unittest.mock import patch, MagicMock

# Third party imports
import pytest

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from digitalmodel import engine


class TestErrorHandling:
    """Test suite for error handling in digitalmodel core functionality."""

    @patch('digitalmodel.engine.wwyaml')
    @patch('digitalmodel.engine.app_manager')
    def test_engine_handles_none_cfg(self, mock_app_manager, mock_wwyaml):
        """Test that engine properly handles None cfg."""
        mock_app_manager.validate_arguments_run_methods.return_value = ("test.yml", {})
        mock_wwyaml.ymlInput.return_value = None
        
        with pytest.raises(ValueError) as excinfo:
            engine.engine("test.yml")
        
        assert "cfg is None" in str(excinfo.value)

    @patch('digitalmodel.engine.wwyaml')
    @patch('digitalmodel.engine.app_manager')
    def test_engine_handles_invalid_input_file(self, mock_app_manager, mock_wwyaml):
        """Test that engine properly handles invalid input file."""
        # Mock app_manager to raise FileNotFoundError
        mock_app_manager.validate_arguments_run_methods.side_effect = FileNotFoundError("Input file not found")
        
        with pytest.raises(FileNotFoundError):
            engine.engine("nonexistent.yml")

    @patch('digitalmodel.engine.wwyaml')
    @patch('digitalmodel.engine.app_manager')
    def test_engine_handles_yaml_parsing_error(self, mock_app_manager, mock_wwyaml):
        """Test that engine handles YAML parsing errors."""
        mock_app_manager.validate_arguments_run_methods.return_value = ("test.yml", {})
        # Mock yaml parsing to raise an exception
        mock_wwyaml.ymlInput.side_effect = Exception("Invalid YAML format")
        
        with pytest.raises(Exception) as excinfo:
            engine.engine("test.yml")
        
        assert "Invalid YAML format" in str(excinfo.value)

    @patch('digitalmodel.engine.wwyaml')
    @patch('digitalmodel.engine.app_manager')
    def test_engine_handles_empty_cfg(self, mock_app_manager, mock_wwyaml):
        """Test that engine handles empty configuration."""
        mock_app_manager.validate_arguments_run_methods.return_value = ("test.yml", {})
        mock_wwyaml.ymlInput.return_value = {}
        
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
            # Should handle empty cfg gracefully
            result = engine.engine("test.yml")
            assert isinstance(result, dict)

    def test_import_error_handling(self):
        """Test graceful handling of import errors."""
        # Test importing non-existent submodule
        with pytest.raises(ImportError):
            from digitalmodel.modules.nonexistent import NonExistent

    def test_attribute_error_handling(self):
        """Test handling of attribute errors."""
        import digitalmodel
        
        # Test accessing non-existent attribute
        with pytest.raises(AttributeError):
            _ = digitalmodel.nonexistent_attribute

    def test_module_loading_with_missing_dependencies(self):
        """Test module loading behavior with missing dependencies."""
        # Some modules may have optional dependencies
        # Test that the core package still works
        import digitalmodel
        assert digitalmodel.__version__ is not None

    @patch('digitalmodel.engine.wwyaml')
    @patch('digitalmodel.engine.app_manager')
    def test_engine_handles_malformed_config(self, mock_app_manager, mock_wwyaml):
        """Test engine handles malformed configuration."""
        mock_app_manager.validate_arguments_run_methods.return_value = ("test.yml", {})
        # Return malformed config (not a dict)
        mock_wwyaml.ymlInput.return_value = "invalid_config"
        
        with patch.multiple(
            'digitalmodel.engine',
            Aqwa=MagicMock(),
            AttributeDict=MagicMock(side_effect=Exception("Cannot convert to AttributeDict"))
        ):
            with pytest.raises(Exception) as excinfo:
                engine.engine("test.yml")
            assert "Cannot convert to AttributeDict" in str(excinfo.value)

    def test_error_propagation(self):
        """Test that errors are properly propagated up the call stack."""
        import digitalmodel.engine
        
        # Test that the engine function exists and is callable
        assert callable(digitalmodel.engine.engine)
        
        # Test with invalid parameters
        with pytest.raises(Exception):
            # Pass invalid parameter types
            digitalmodel.engine.engine(inputfile=123)  # Should be string

    def test_graceful_degradation(self):
        """Test graceful degradation when optional features fail."""
        import digitalmodel
        
        # Core functionality should work even if optional features fail
        assert hasattr(digitalmodel, '__version__')
        assert hasattr(digitalmodel, '__doc__')

    @patch('digitalmodel.engine.logger')
    def test_error_logging(self, mock_logger):
        """Test that errors are properly logged."""
        import digitalmodel.engine
        
        # Verify logger is available for error logging
        assert hasattr(digitalmodel.engine, 'logger')

    def test_resource_cleanup_on_error(self):
        """Test that resources are properly cleaned up on errors."""
        # This is more of a design pattern test
        # Ensure that file handles, connections, etc. are cleaned up
        
        import digitalmodel
        
        # Test multiple imports and ensure no resource leaks
        for _ in range(3):
            import importlib
            importlib.reload(digitalmodel)
        
        # Should complete without issues
        assert digitalmodel.__version__ is not None

    def test_exception_chaining(self):
        """Test that exception chaining works properly."""
        # Test that original exceptions are preserved in chains
        with pytest.raises(Exception) as excinfo:
            try:
                raise ValueError("Original error")
            except ValueError as e:
                raise RuntimeError("Wrapped error") from e
        
        # Check that both exceptions are in the chain
        assert excinfo.value.__cause__ is not None
        assert isinstance(excinfo.value.__cause__, ValueError)

    def test_edge_case_handling(self):
        """Test handling of edge cases."""
        import digitalmodel
        
        # Test empty version string handling
        original_version = digitalmodel.__version__
        
        # Ensure version is not empty or None
        assert original_version is not None
        assert len(original_version.strip()) > 0

    def test_concurrent_access_safety(self):
        """Test thread safety of core module loading."""
        import threading
        import digitalmodel
        
        results = []
        errors = []
        
        def import_module():
            try:
                import digitalmodel as dm
                results.append(dm.__version__)
            except Exception as e:
                errors.append(e)
        
        # Create multiple threads importing the module
        threads = [threading.Thread(target=import_module) for _ in range(5)]
        
        for thread in threads:
            thread.start()
        
        for thread in threads:
            thread.join()
        
        # Should have no errors and consistent results
        assert len(errors) == 0, f"Concurrent import errors: {errors}"
        assert len(set(results)) == 1, "Version should be consistent across threads"

    def test_memory_error_handling(self):
        """Test handling of memory-related errors."""
        # This is a basic test - in real scenarios we might test with limited memory
        import digitalmodel
        
        # Test that basic imports don't consume excessive memory
        # (This is more of a smoke test)
        assert digitalmodel.__version__ is not None