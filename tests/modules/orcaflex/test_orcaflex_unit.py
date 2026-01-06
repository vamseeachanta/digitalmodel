#!/usr/bin/env python3
"""
ABOUTME: Unit tests for OrcaFlex integration module covering universal runner,
post-processing, and core functionality.
"""

import pytest
import tempfile
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock

from digitalmodel.modules.orcaflex import (
    __version__,
    get_version,
    check_availability,
    list_cli_commands,
    CLI_COMMANDS,
    UNIVERSAL_RUNNER_AVAILABLE,
    POST_PROCESSING_AVAILABLE,
    ORCAFLEX_AVAILABLE,
)


class TestModuleInfo:
    """Test module information and metadata"""

    def test_version_exists(self):
        """Test that version is defined"""
        assert __version__ is not None
        assert isinstance(__version__, str)
        assert len(__version__.split('.')) == 3  # Major.Minor.Patch

    def test_get_version(self):
        """Test get_version function"""
        version = get_version()
        assert version == __version__
        assert version == '1.0.0'

    def test_check_availability(self):
        """Test check_availability returns proper structure"""
        availability = check_availability()

        assert isinstance(availability, dict)
        assert 'universal_runner' in availability
        assert 'post_processing' in availability
        assert 'orcaflex_api' in availability
        assert 'core' in availability

        # All values should be boolean
        for key, value in availability.items():
            assert isinstance(value, bool), f"{key} should be boolean"

    def test_list_cli_commands(self):
        """Test CLI command listing"""
        commands = list_cli_commands()

        assert isinstance(commands, dict)
        assert 'orcaflex-universal' in commands
        assert 'run-to-sim' in commands
        assert len(commands) == 2

        # Should be a copy, not the original
        assert commands is not CLI_COMMANDS


class TestAvailabilityFlags:
    """Test availability flags for optional components"""

    def test_universal_runner_flag(self):
        """Test universal runner availability flag"""
        assert isinstance(UNIVERSAL_RUNNER_AVAILABLE, bool)

    def test_post_processing_flag(self):
        """Test post-processing availability flag"""
        assert isinstance(POST_PROCESSING_AVAILABLE, bool)

    def test_orcaflex_api_flag(self):
        """Test OrcaFlex API availability flag"""
        assert isinstance(ORCAFLEX_AVAILABLE, bool)

    def test_flags_consistency(self):
        """Test that flags are consistent with check_availability"""
        availability = check_availability()

        assert availability['universal_runner'] == UNIVERSAL_RUNNER_AVAILABLE
        assert availability['post_processing'] == POST_PROCESSING_AVAILABLE
        assert availability['orcaflex_api'] == ORCAFLEX_AVAILABLE


class TestImportStructure:
    """Test module import structure and exports"""

    def test_all_exports_defined(self):
        """Test that __all__ contains expected exports"""
        from digitalmodel.modules.orcaflex import __all__

        expected_exports = [
            '__version__',
            'UniversalOrcaFlexRunner',
            'StatusReporter',
            'UNIVERSAL_RUNNER_AVAILABLE',
            'OrcaFlexPostProcessor',
            'POST_PROCESSING_AVAILABLE',
            'run_models',
            'ORCAFLEX_AVAILABLE',
            'BaseAnalysisEngine',
            'ModelInterface',
            'AnalysisConfiguration',
            'CORE_AVAILABLE',
        ]

        for export in expected_exports:
            assert export in __all__, f"{export} should be in __all__"

    def test_cli_commands_dictionary(self):
        """Test CLI commands dictionary structure"""
        assert isinstance(CLI_COMMANDS, dict)
        assert all(isinstance(k, str) for k in CLI_COMMANDS.keys())
        assert all(isinstance(v, str) for v in CLI_COMMANDS.values())


@pytest.mark.skipif(not UNIVERSAL_RUNNER_AVAILABLE, reason="Universal runner not available")
class TestUniversalRunner:
    """Test universal runner functionality (when available)"""

    def test_universal_runner_import(self):
        """Test UniversalOrcaFlexRunner can be imported"""
        from digitalmodel.modules.orcaflex import UniversalOrcaFlexRunner
        assert UniversalOrcaFlexRunner is not None

    def test_status_reporter_import(self):
        """Test StatusReporter can be imported"""
        from digitalmodel.modules.orcaflex import StatusReporter
        assert StatusReporter is not None


@pytest.mark.skipif(not POST_PROCESSING_AVAILABLE, reason="Post-processing not available")
class TestPostProcessing:
    """Test post-processing functionality (when available)"""

    def test_post_processor_import(self):
        """Test OrcaFlexPostProcessor can be imported"""
        from digitalmodel.modules.orcaflex import OrcaFlexPostProcessor
        assert OrcaFlexPostProcessor is not None


class TestRunToSim:
    """Test run-to-sim functionality"""

    def test_run_models_import(self):
        """Test run_models function can be imported"""
        from digitalmodel.modules.orcaflex import run_models
        # May be None if import failed, but should exist in namespace
        assert 'run_models' in dir()


class TestModuleDocumentation:
    """Test module documentation"""

    def test_module_docstring(self):
        """Test module has docstring"""
        import digitalmodel.modules.orcaflex as orcaflex
        assert orcaflex.__doc__ is not None
        assert len(orcaflex.__doc__) > 50
        assert 'OrcaFlex' in orcaflex.__doc__

    def test_key_features_documented(self):
        """Test key features are mentioned in docstring"""
        import digitalmodel.modules.orcaflex as orcaflex
        docstring = orcaflex.__doc__.lower()

        assert 'model' in docstring or 'runner' in docstring
        assert 'post-processing' in docstring or 'processing' in docstring


class TestModuleIntegration:
    """Test module integration patterns"""

    def test_graceful_import_failure(self):
        """Test that module handles import failures gracefully"""
        # Module should import even if some components fail
        import digitalmodel.modules.orcaflex as orcaflex
        assert orcaflex is not None
        assert hasattr(orcaflex, '__version__')

    def test_availability_check_no_exceptions(self):
        """Test that availability check doesn't raise exceptions"""
        try:
            availability = check_availability()
            assert isinstance(availability, dict)
        except Exception as e:
            pytest.fail(f"check_availability raised exception: {e}")

    def test_list_commands_no_exceptions(self):
        """Test that list_cli_commands doesn't raise exceptions"""
        try:
            commands = list_cli_commands()
            assert isinstance(commands, dict)
        except Exception as e:
            pytest.fail(f"list_cli_commands raised exception: {e}")


class TestErrorHandling:
    """Test error handling and edge cases"""

    def test_check_availability_returns_dict(self):
        """Test check_availability always returns dict"""
        result = check_availability()
        assert isinstance(result, dict)
        assert len(result) > 0

    def test_list_cli_commands_returns_dict(self):
        """Test list_cli_commands always returns dict"""
        result = list_cli_commands()
        assert isinstance(result, dict)
        assert len(result) > 0

    def test_get_version_returns_string(self):
        """Test get_version always returns string"""
        result = get_version()
        assert isinstance(result, str)
        assert len(result) > 0


class TestConstants:
    """Test module constants"""

    def test_version_format(self):
        """Test version follows semantic versioning"""
        version_parts = __version__.split('.')
        assert len(version_parts) == 3

        # Should be integers
        try:
            major, minor, patch = map(int, version_parts)
            assert major >= 0
            assert minor >= 0
            assert patch >= 0
        except ValueError:
            pytest.fail(f"Version {__version__} is not valid semantic version")

    def test_cli_commands_not_empty(self):
        """Test CLI_COMMANDS is not empty"""
        assert len(CLI_COMMANDS) > 0
        assert 'orcaflex-universal' in CLI_COMMANDS


# Run tests if executed directly
if __name__ == '__main__':
    pytest.main([__file__, '-v'])
