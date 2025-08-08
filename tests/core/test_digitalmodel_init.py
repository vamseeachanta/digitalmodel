# Standard library imports
import sys
from pathlib import Path

# Third party imports
import pytest

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

import digitalmodel


class TestDigitalModelInit:
    """Test suite for digitalmodel package initialization."""

    def test_version_attribute_exists(self):
        """Test that __version__ attribute exists."""
        assert hasattr(digitalmodel, '__version__')

    def test_version_format(self):
        """Test that version follows semantic versioning format."""
        version = digitalmodel.__version__
        assert isinstance(version, str)
        
        # Check basic format (x.y.z)
        parts = version.split('.')
        assert len(parts) >= 2, f"Version {version} should have at least major.minor format"
        
        # Check that parts are numeric
        for i, part in enumerate(parts[:2]):  # Check first two parts
            assert part.isdigit(), f"Version part {i} '{part}' should be numeric"

    def test_version_not_empty(self):
        """Test that version is not empty."""
        version = digitalmodel.__version__
        assert version.strip() != ""

    def test_package_docstring_exists(self):
        """Test that package has a docstring."""
        assert digitalmodel.__doc__ is not None
        assert isinstance(digitalmodel.__doc__, str)
        assert len(digitalmodel.__doc__.strip()) > 0

    def test_package_name_attribute(self):
        """Test that package name can be determined."""
        assert digitalmodel.__name__ == 'digitalmodel'

    def test_package_file_location(self):
        """Test that package file location is correct."""
        expected_path_suffix = "digitalmodel/__init__.py"
        actual_path = digitalmodel.__file__
        assert expected_path_suffix in actual_path.replace("\\", "/")

    def test_import_structure(self):
        """Test basic import structure works."""
        # Test that we can import the package
        import digitalmodel
        
        # Test that basic attributes exist
        required_attrs = ['__version__', '__doc__', '__name__', '__file__']
        for attr in required_attrs:
            assert hasattr(digitalmodel, attr), f"Missing required attribute: {attr}"

    def test_version_consistency(self):
        """Test version consistency across multiple imports."""
        import digitalmodel as dm1
        import digitalmodel as dm2
        
        assert dm1.__version__ == dm2.__version__

    def test_package_is_importable(self):
        """Test that package imports without errors."""
        try:
            import digitalmodel
        except ImportError as e:
            pytest.fail(f"Failed to import digitalmodel: {e}")

    def test_module_level_constants(self):
        """Test module-level constants are properly defined."""
        # Test version is properly formatted
        version = digitalmodel.__version__
        
        # Should be string
        assert isinstance(version, str)
        
        # Should not contain whitespace at edges
        assert version == version.strip()
        
        # Should not be placeholder values
        assert version not in ["", "0.0.0", "unknown", "dev"]