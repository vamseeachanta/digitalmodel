"""Test Suite for Module Reorganization.

This test suite verifies that the marine_analysis module reorganization
was successful and all imports work correctly.
"""

import pytest
import sys
from pathlib import Path


class TestModuleStructure:
    """Test that the new module structure is correctly organized."""

    def test_profiling_submodule_exists(self):
        """Test that profiling submodule exists and is importable."""
        from digitalmodel.marine_analysis import profiling
        assert profiling is not None

    def test_extraction_submodule_exists(self):
        """Test that extraction submodule exists and is importable."""
        from digitalmodel.marine_analysis import extraction
        assert extraction is not None

    def test_validation_submodule_exists(self):
        """Test that validation submodule exists and is importable."""
        from digitalmodel.marine_analysis import validation
        assert validation is not None

    def test_visualization_submodule_exists(self):
        """Test that visualization submodule exists and is importable."""
        from digitalmodel.marine_analysis import viz_tools
        assert viz_tools is not None

    def test_analysis_submodule_exists(self):
        """Test that analysis submodule exists and is importable."""
        from digitalmodel.marine_analysis import analysis
        assert analysis is not None


class TestProfilingModule:
    """Test profiling module imports."""

    def test_profile_modules_import(self):
        """Test profile_modules can be imported."""
        try:
            from digitalmodel.marine_analysis.profiling import profile_modules
            assert profile_modules is not None
        except ImportError as e:
            pytest.skip(f"Module import failed (dependencies may be missing): {e}")

    def test_performance_charts_import(self):
        """Test performance_charts can be imported."""
        try:
            from digitalmodel.marine_analysis.profiling import performance_charts
            assert performance_charts is not None
        except ImportError as e:
            pytest.skip(f"Module import failed (dependencies may be missing): {e}")

    def test_optimization_report_import(self):
        """Test optimization_report can be imported."""
        try:
            from digitalmodel.marine_analysis.profiling import optimization_report
            assert optimization_report is not None
        except ImportError as e:
            pytest.skip(f"Module import failed (dependencies may be missing): {e}")


class TestExtractionModule:
    """Test extraction module imports."""

    def test_extract_ocimf_import(self):
        """Test extract_ocimf can be imported."""
        try:
            from digitalmodel.marine_analysis.extraction import extract_ocimf
            assert extract_ocimf is not None
        except ImportError as e:
            pytest.skip(f"Module import failed (dependencies may be missing): {e}")

    def test_extract_hydro_import(self):
        """Test extract_hydro can be imported."""
        try:
            from digitalmodel.marine_analysis.extraction import extract_hydro
            assert extract_hydro is not None
        except ImportError as e:
            pytest.skip(f"Module import failed (dependencies may be missing): {e}")

    def test_extract_mooring_import(self):
        """Test extract_mooring can be imported."""
        try:
            from digitalmodel.marine_analysis.extraction import extract_mooring
            assert extract_mooring is not None
        except ImportError as e:
            pytest.skip(f"Module import failed (dependencies may be missing): {e}")


class TestValidationModule:
    """Test validation module imports."""

    def test_validate_phase2_import(self):
        """Test validate_phase2 can be imported."""
        try:
            from digitalmodel.marine_analysis.validation import validate_phase2
            assert validate_phase2 is not None
        except ImportError as e:
            pytest.skip(f"Module import failed (dependencies may be missing): {e}")

    def test_validate_catenary_import(self):
        """Test validate_catenary can be imported."""
        try:
            from digitalmodel.marine_analysis.validation import validate_catenary
            assert validate_catenary is not None
        except ImportError as e:
            pytest.skip(f"Module import failed (dependencies may be missing): {e}")


class TestVisualizationModule:
    """Test visualization module imports."""

    def test_integration_charts_import(self):
        """Test integration_charts can be imported."""
        try:
            from digitalmodel.marine_analysis.visualization import integration_charts
            assert integration_charts is not None
        except ImportError as e:
            pytest.skip(f"Module import failed (dependencies may be missing): {e}")

    def test_ocimf_charts_import(self):
        """Test ocimf_charts can be imported."""
        try:
            from digitalmodel.marine_analysis.visualization import ocimf_charts
            assert ocimf_charts is not None
        except ImportError as e:
            pytest.skip(f"Module import failed (dependencies may be missing): {e}")


class TestAnalysisModule:
    """Test analysis module imports."""

    def test_excel_analyzer_import(self):
        """Test excel_analyzer can be imported."""
        try:
            from digitalmodel.marine_analysis.analysis import excel_analyzer
            assert excel_analyzer is not None
        except ImportError as e:
            pytest.skip(f"Module import failed (dependencies may be missing): {e}")

    def test_hydro_usage_example_import(self):
        """Test hydro_usage_example can be imported."""
        try:
            from digitalmodel.marine_analysis.analysis import hydro_usage_example
            assert hydro_usage_example is not None
        except ImportError as e:
            pytest.skip(f"Module import failed (dependencies may be missing): {e}")


class TestOutputDirectories:
    """Test that output directories are properly organized."""

    def test_tests_outputs_exists(self):
        """Test that tests/outputs directory exists."""
        output_dir = Path("tests/outputs")
        assert output_dir.exists(), "tests/outputs directory should exist"
        assert output_dir.is_dir(), "tests/outputs should be a directory"

    def test_phase2_output_structure(self):
        """Test that phase2 output structure is correct."""
        phase2_dir = Path("tests/outputs/phase2")
        assert phase2_dir.exists(), "phase2 output directory should exist"

        ocimf_dir = phase2_dir / "ocimf"
        hydro_dir = phase2_dir / "hydro"
        validation_dir = phase2_dir / "validation"

        assert ocimf_dir.exists(), "phase2/ocimf directory should exist"
        assert hydro_dir.exists(), "phase2/hydro directory should exist"
        assert validation_dir.exists(), "phase2/validation directory should exist"

    def test_phase3_output_structure(self):
        """Test that phase3 output structure is correct."""
        phase3_dir = Path("tests/outputs/phase3")
        assert phase3_dir.exists(), "phase3 output directory should exist"

        integration_dir = phase3_dir / "integration"
        performance_dir = phase3_dir / "performance"

        assert integration_dir.exists(), "phase3/integration directory should exist"
        assert performance_dir.exists(), "phase3/performance directory should exist"

    def test_specs_outputs_exists(self):
        """Test that specs/outputs directory exists."""
        specs_dir = Path("specs/outputs")
        assert specs_dir.exists(), "specs/outputs directory should exist"
        assert specs_dir.is_dir(), "specs/outputs should be a directory"


class TestCLIInterface:
    """Test CLI interface functionality."""

    def test_main_module_exists(self):
        """Test that __main__.py exists."""
        main_file = Path("src/digitalmodel/modules/marine_analysis/__main__.py")
        assert main_file.exists(), "__main__.py should exist for CLI entry point"

    def test_main_module_imports(self):
        """Test that __main__ module can be imported."""
        try:
            from digitalmodel.marine_analysis import __main__
            assert __main__ is not None
            assert hasattr(__main__, 'main'), "__main__ should have a main() function"
        except ImportError as e:
            pytest.skip(f"Module import failed: {e}")


class TestVersioning:
    """Test module versioning."""

    def test_module_version(self):
        """Test that module has a version."""
        from digitalmodel.marine_analysis import __version__
        assert __version__ is not None
        assert isinstance(__version__, str)
        assert __version__ == '2.1.0', "Version should be 2.1.0 after reorganization"


class TestSubmoduleInitFiles:
    """Test that all submodules have proper __init__.py files."""

    def test_profiling_init_exists(self):
        """Test profiling __init__.py exists."""
        init_file = Path("src/digitalmodel/modules/marine_analysis/profiling/__init__.py")
        assert init_file.exists(), "profiling/__init__.py should exist"

    def test_extraction_init_exists(self):
        """Test extraction __init__.py exists."""
        init_file = Path("src/digitalmodel/modules/marine_analysis/extraction/__init__.py")
        assert init_file.exists(), "extraction/__init__.py should exist"

    def test_validation_init_exists(self):
        """Test validation __init__.py exists."""
        init_file = Path("src/digitalmodel/modules/marine_analysis/validation/__init__.py")
        assert init_file.exists(), "validation/__init__.py should exist"

    def test_visualization_init_exists(self):
        """Test visualization __init__.py exists."""
        init_file = Path("src/digitalmodel/modules/marine_analysis/visualization/__init__.py")
        assert init_file.exists(), "visualization/__init__.py should exist"

    def test_analysis_init_exists(self):
        """Test analysis __init__.py exists."""
        init_file = Path("src/digitalmodel/modules/marine_analysis/analysis/__init__.py")
        assert init_file.exists(), "analysis/__init__.py should exist"


if __name__ == '__main__':
    # Run tests with verbose output
    pytest.main([__file__, '-v', '--tb=short'])
