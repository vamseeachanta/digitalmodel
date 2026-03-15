"""Pytest configuration for marine engineering tests."""
import sys
from pathlib import Path
from unittest.mock import MagicMock

# Tests that fail at collection due to missing modules, deleted files, or
# platform-specific imports. Excluded so the rest of the suite can run.
_tests_dir = Path(__file__).parent
collect_ignore = [
    # Missing local modules (standalone scripts, not real tests)
    str(_tests_dir / "marine_ops/artificial_lift/dynacard/test_vision_benchmark.py"),
    str(_tests_dir / "solvers/orcaflex/examples_integration/test_converter.py"),
    str(_tests_dir / "solvers/orcaflex/examples_integration/test_single_download.py"),
    str(_tests_dir / "structural/fatigue_analysis/test_reference_seastate_scaling.py"),
    str(_tests_dir / "visualization/design_tools/pilot_program/test_case_1_separator.py"),
    # Missing src modules (not yet implemented)
    str(_tests_dir / "solvers/orcaflex/test_orcaflex_unit.py"),
    str(_tests_dir / "structural/fatigue_apps/test_load_scaling.py"),
    str(_tests_dir / "subsea/pipeline/test_on_bottom_stability.py"),
    str(_tests_dir / "test_plate_capacity.py"),
    # Deleted orcaflex-dashboard service files
    str(_tests_dir / "visualization/test_anomaly_detection.py"),
    str(_tests_dir / "visualization/test_comparative_analysis.py"),
    str(_tests_dir / "visualization/test_component_classifier.py"),
    str(_tests_dir / "visualization/test_csv_parser.py"),
    str(_tests_dir / "visualization/test_data_validator.py"),
    str(_tests_dir / "visualization/test_loading_decoder.py"),
    str(_tests_dir / "visualization/test_sensitivity_analysis.py"),
    str(_tests_dir / "visualization/test_statistical_analysis.py"),
    # Platform-specific or missing optional deps
    str(_tests_dir / "workflows/orcawave/test_com_connection.py"),
    str(_tests_dir / "workflows/orcawave/test_end_to_end.py"),
    str(_tests_dir / "workflows/orcawave/test_integration.py"),
    str(_tests_dir / "workflows/standalone/markitdown/test_converter.py"),
    # Requires pytest-asyncio plugin (disabled due to hypothesis conflict)
    str(_tests_dir / "test_workflow_checkpoints.py"),
]

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
    import digitalmodel.catenary
except ImportError:
    pass  # Package not installed, tests will fail with clear import errors

# Only register the mock for the specific problematic module
if 'digitalmodel.subsea.catenary_riser.legacy.catenary_riser_summary' not in sys.modules:
    sys.modules['digitalmodel.subsea.catenary_riser.legacy.catenary_riser_summary'] = _mock_catenary_riser_summary
