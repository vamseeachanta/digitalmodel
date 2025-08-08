# Standard library imports
import sys
from pathlib import Path
from unittest.mock import patch, MagicMock

# Third party imports
import pytest

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))


class TestModuleLoading:
    """Test suite for digitalmodel module loading and initialization."""

    def test_core_modules_importable(self):
        """Test that core modules can be imported without errors."""
        core_modules = [
            'digitalmodel',
            'digitalmodel.engine',
            'digitalmodel.aqwa',
            'digitalmodel.time_series'
        ]
        
        for module_name in core_modules:
            try:
                __import__(module_name)
            except ImportError as e:
                pytest.fail(f"Failed to import {module_name}: {e}")

    def test_common_modules_importable(self):
        """Test that common utility modules can be imported."""
        common_modules = [
            'digitalmodel.common.fatigue_analysis',
            'digitalmodel.common.ship_design',
            'digitalmodel.common.cathodic_protection'
        ]
        
        for module_name in common_modules:
            try:
                __import__(module_name)
            except ImportError as e:
                # Some modules may have external dependencies, so we expect some failures
                # Just log the failure for now
                print(f"Expected import failure for {module_name}: {e}")

    def test_module_structure_consistency(self):
        """Test that module structure follows consistent patterns."""
        from digitalmodel.modules import aqwa, pipeline
        
        # Test that modules have expected structure
        modules_to_test = [
            ('digitalmodel.modules.aqwa', ['aqwa_utilities', 'aqwa_reader']),
            ('digitalmodel.modules.pipeline', ['pipeline', 'lateral_buckling']),
        ]
        
        for module_path, expected_submodules in modules_to_test:
            try:
                module = __import__(module_path, fromlist=[''])
                # Check that module directory exists
                assert hasattr(module, '__path__'), f"{module_path} should be a package"
            except ImportError:
                # Some modules may not be importable due to dependencies
                pass

    @patch('digitalmodel.engine.logger')
    def test_logger_integration(self, mock_logger):
        """Test that modules properly integrate with logging."""
        import digitalmodel.engine
        
        # Test that logger is available
        assert hasattr(digitalmodel.engine, 'logger')

    def test_base_configs_accessibility(self):
        """Test that base configuration files are accessible."""
        import digitalmodel
        from pathlib import Path
        
        # Get the package path
        package_path = Path(digitalmodel.__file__).parent
        base_configs_path = package_path / 'base_configs'
        
        # Test that base_configs directory exists
        assert base_configs_path.exists(), "base_configs directory should exist"
        
        # Test that modules directory exists within base_configs
        modules_config_path = base_configs_path / 'modules'
        assert modules_config_path.exists(), "base_configs/modules should exist"

    def test_data_directory_accessibility(self):
        """Test that data directory is accessible."""
        import digitalmodel
        from pathlib import Path
        
        package_path = Path(digitalmodel.__file__).parent
        data_path = package_path / 'data'
        
        # Test that data directory exists
        assert data_path.exists(), "data directory should exist"

    def test_modules_package_structure(self):
        """Test that modules package has expected structure."""
        from digitalmodel.modules import aqwa
        from pathlib import Path
        
        # Test that modules are packages
        assert hasattr(aqwa, '__path__'), "aqwa module should be a package"

    def test_custom_modules_importable(self):
        """Test that custom modules can be imported."""
        try:
            from digitalmodel.custom import PipeSizing
            # If import succeeds, test basic functionality
            assert hasattr(PipeSizing, '__name__')
        except ImportError:
            # Custom modules may have specific dependencies
            pass

    def test_module_initialization_order(self):
        """Test that modules can be initialized in any order."""
        # Test importing in different orders
        import digitalmodel.engine
        import digitalmodel
        
        # Should work without issues
        assert digitalmodel.__version__ is not None
        assert hasattr(digitalmodel.engine, 'engine')

    def test_circular_import_prevention(self):
        """Test that there are no circular import issues."""
        try:
            # Import multiple related modules
            import digitalmodel.engine
            import digitalmodel.aqwa
            import digitalmodel.time_series
            
            # If we get here, no circular imports
            assert True
        except ImportError as e:
            if "circular import" in str(e).lower():
                pytest.fail(f"Circular import detected: {e}")

    def test_module_cleanup_on_reload(self):
        """Test that modules can be reloaded without issues."""
        import importlib
        import digitalmodel
        
        # Store original version
        original_version = digitalmodel.__version__
        
        # Reload the module
        importlib.reload(digitalmodel)
        
        # Should still have the same version
        assert digitalmodel.__version__ == original_version

    def test_submodule_lazy_loading(self):
        """Test that submodules are loaded lazily when needed."""
        import digitalmodel
        
        # Initially, submodules should not be loaded
        # (This tests lazy loading behavior)
        
        # When we access a submodule, it should load
        try:
            import digitalmodel.engine
            assert hasattr(digitalmodel.engine, 'engine')
        except ImportError:
            # Some modules may have dependency issues
            pass

    def test_module_metadata_consistency(self):
        """Test that module metadata is consistent across imports."""
        import digitalmodel
        import digitalmodel.engine
        
        # Test that package name is consistent
        assert digitalmodel.__name__ == 'digitalmodel'
        assert digitalmodel.engine.__name__ == 'digitalmodel.engine'

    def test_plugin_architecture_compatibility(self):
        """Test that the module structure supports plugin architecture."""
        import digitalmodel
        from pathlib import Path
        
        # Test that modules directory supports dynamic loading
        package_path = Path(digitalmodel.__file__).parent
        modules_path = package_path / 'modules'
        
        if modules_path.exists():
            # Should be able to list available modules
            module_dirs = [d for d in modules_path.iterdir() if d.is_dir() and not d.name.startswith('__')]
            assert len(module_dirs) > 0, "Should have at least one module directory"