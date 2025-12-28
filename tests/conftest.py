"""Pytest configuration for marine engineering tests."""
import sys
from pathlib import Path
from unittest.mock import MagicMock

# Add src/ to Python path
repo_root = Path(__file__).parent.parent
src_path = repo_root / "src"
if str(src_path) not in sys.path:
    sys.path.insert(0, str(src_path))

assetutilities_path = (repo_root.parent / "assetutilities" / "src").resolve()
if assetutilities_path.exists() and str(assetutilities_path) not in sys.path:
    sys.path.insert(0, str(assetutilities_path))

# Add aceengineercode to path for 'common' module imports
aceengineercode_path = (repo_root.parent / "aceengineercode").resolve()
if aceengineercode_path.exists() and str(aceengineercode_path) not in sys.path:
    sys.path.insert(0, str(aceengineercode_path))


# Mock catenary_riser_summary to prevent module-level code execution during test patching
# The module has module-level code that runs on import and requires config files
def _create_mock_catenary_riser_summary():
    """Create a mock module for catenary_riser_summary."""
    mock_module = MagicMock()
    mock_module.plt = MagicMock()
    mock_module.logging = MagicMock()
    mock_module.CompareTools = MagicMock()
    mock_module.application_configuration = MagicMock(return_value={
        "default": {"config": {"overwrite": {"output": False}}},
        "Analysis": {"file_name": "test", "result_folder": "/tmp/"},
        "ymlFiles": [],
        "plot": {"settings": None}
    })
    return mock_module


# Pre-register the mock module before tests attempt to import via @patch decorators
# This prevents the actual module-level code from running
_mock_catenary_riser_summary = _create_mock_catenary_riser_summary()

# Import the real digitalmodel package first to populate sys.modules properly
# Then only mock the specific module that has problematic module-level code
try:
    import digitalmodel
    import digitalmodel.modules
    import digitalmodel.modules.catenary
except ImportError:
    pass  # Package not installed, tests will fail with clear import errors

# Only register the mock for the specific problematic module
if 'digitalmodel.modules.catenary.catenary_riser_summary' not in sys.modules:
    sys.modules['digitalmodel.modules.catenary.catenary_riser_summary'] = _mock_catenary_riser_summary
