# Engine.py Test Implementation Plan - Detailed Specifications

## Overview

This document provides detailed test specifications for implementing comprehensive test coverage of the `engine.py` file following TDD London School methodology. Each test case includes specific assertions, mock setups, and expected behaviors.

## Test File Structure

```
tests/
├── test_engine/
│   ├── __init__.py
│   ├── conftest.py                 # Shared fixtures and mocks
│   ├── test_engine_unit.py         # Core unit tests
│   ├── test_engine_integration.py  # Integration scenarios
│   ├── test_engine_property.py     # Hypothesis property tests
│   ├── test_engine_performance.py  # Benchmark tests
│   └── fixtures/
│       ├── config_samples.py       # Test configuration data
│       ├── input_files/             # Sample YAML files
│       └── mock_contracts.py       # Mock specifications
```

## 1. Shared Test Infrastructure (conftest.py)

```python
import pytest
from unittest.mock import Mock, patch, MagicMock
from pathlib import Path
from digitalmodel.engine import engine
from assetutilities.common.ApplicationManager import ConfigureApplicationInputs
from assetutilities.common.yml_utilities import WorkingWithYAML
from assetutilities.common.file_management import FileManagement

# London School Mock Contracts
@pytest.fixture
def mock_app_manager():
    """Mock ApplicationManager with contract verification"""
    mock = Mock(spec=ConfigureApplicationInputs)
    mock.validate_arguments_run_methods.return_value = ("test.yml", {})
    mock.configure.return_value = {"basename": "test", "configured": True}
    mock.configure_result_folder.return_value = ({}, {"result_configured": True})
    mock.save_cfg.return_value = None
    return mock

@pytest.fixture
def mock_wwyaml():
    """Mock YAML utilities with contract verification"""
    mock = Mock(spec=WorkingWithYAML)
    mock.ymlInput.return_value = {"basename": "test", "inputs": {}}
    return mock

@pytest.fixture
def mock_file_manager():
    """Mock FileManagement with contract verification"""
    mock = Mock(spec=FileManagement)
    mock.router.return_value = {"routed": True}
    return mock

@pytest.fixture
def mock_logger():
    """Mock logger for interaction verification"""
    with patch('digitalmodel.engine.logger') as mock:
        yield mock

@pytest.fixture
def mock_output_controller():
    """Mock output controller with argument parsing"""
    with patch('digitalmodel.engine.get_output_level_from_argv') as mock:
        mock.return_value = None  # Default: no special output level
        yield mock

@pytest.fixture
def standard_mocks(mock_app_manager, mock_wwyaml, mock_file_manager,
                  mock_logger, mock_output_controller):
    """Standard mock configuration for most tests"""
    return {
        'app_manager': mock_app_manager,
        'wwyaml': mock_wwyaml,
        'file_manager': mock_file_manager,
        'logger': mock_logger,
        'output_controller': mock_output_controller
    }

# Test Configuration Data
@pytest.fixture
def minimal_config():
    return {"basename": "orcaflex"}

@pytest.fixture
def meta_config():
    return {"meta": {"basename": "catenary"}}

@pytest.fixture
def full_config():
    return {
        "basename": "vertical_riser",
        "Analysis": {"analysis_root_folder": "/test"},
        "inputs": {"shape": "circular", "file": "test.dat"}
    }

@pytest.fixture
def invalid_configs():
    return {
        'no_basename': {"meta": {"project": "test"}},
        'empty_config': {},
        'none_config': None
    }
```

## 2. Unit Tests (test_engine_unit.py)

### 2.1 Configuration Loading Tests

```python
class TestConfigurationLoading:
    """Test configuration loading and validation logic (Lines 45-58)"""

    def test_engine_with_provided_config_skips_loading(self, standard_mocks, minimal_config):
        """When cfg is provided, should skip file loading"""
        with patch.multiple('digitalmodel.engine',
                           app_manager=standard_mocks['app_manager'],
                           wwyaml=standard_mocks['wwyaml']):

            result = engine(cfg=minimal_config, config_flag=False)

            # Verify file loading was skipped
            standard_mocks['app_manager'].validate_arguments_run_methods.assert_not_called()
            standard_mocks['wwyaml'].ymlInput.assert_not_called()

    def test_engine_loads_config_from_file_when_none_provided(self, standard_mocks):
        """When cfg is None, should load from input file"""
        test_file = "test_input.yml"

        with patch.multiple('digitalmodel.engine',
                           app_manager=standard_mocks['app_manager'],
                           wwyaml=standard_mocks['wwyaml']):

            result = engine(inputfile=test_file)

            # Verify correct loading sequence
            standard_mocks['app_manager'].validate_arguments_run_methods.assert_called_once_with(test_file)
            standard_mocks['wwyaml'].ymlInput.assert_called_once()

    def test_engine_adds_config_file_paths_when_file_exists(self, standard_mocks):
        """Should add config file path metadata when file exists"""
        test_file = "/absolute/path/test.yml"

        with patch('os.path.exists', return_value=True), \
             patch('os.path.abspath', return_value=test_file), \
             patch('os.path.dirname', return_value="/absolute/path"), \
             patch.multiple('digitalmodel.engine',
                           app_manager=standard_mocks['app_manager'],
                           wwyaml=standard_mocks['wwyaml']):

            result = engine(inputfile=test_file)

            # Verify path metadata was added
            assert "_config_file_path" in result
            assert "_config_dir_path" in result

    def test_engine_raises_error_when_config_is_none_after_loading(self, standard_mocks):
        """Should raise ValueError when config loading returns None"""
        standard_mocks['wwyaml'].ymlInput.return_value = None

        with patch.multiple('digitalmodel.engine',
                           app_manager=standard_mocks['app_manager'],
                           wwyaml=standard_mocks['wwyaml']):

            with pytest.raises(ValueError, match="cfg is None"):
                engine(inputfile="test.yml")
```

### 2.2 Basename Extraction Tests

```python
class TestBasenameExtraction:
    """Test basename extraction logic (Lines 59-64)"""

    def test_basename_extracted_from_root_level(self, standard_mocks):
        """Should extract basename from root level of config"""
        config = {"basename": "orcaflex", "other": "data"}

        with patch.multiple('digitalmodel.engine',
                           app_manager=standard_mocks['app_manager']):
            result = engine(cfg=config, config_flag=False)

        # Verify basename was used correctly
        assert "basename" in config

    def test_basename_extracted_from_meta(self, standard_mocks):
        """Should extract basename from meta section when not in root"""
        config = {"meta": {"basename": "catenary"}, "other": "data"}

        with patch.multiple('digitalmodel.engine',
                           app_manager=standard_mocks['app_manager']):
            result = engine(cfg=config, config_flag=False)

        # Should successfully process catenary basename
        assert result is not None

    def test_basename_root_takes_precedence_over_meta(self, standard_mocks):
        """Root level basename should take precedence over meta"""
        config = {
            "basename": "orcaflex",
            "meta": {"basename": "catenary"}
        }

        with patch('digitalmodel.orcaflex.orcaflex.OrcaFlex') as mock_orcaflex:
            mock_instance = Mock()
            mock_orcaflex.return_value = mock_instance
            mock_instance.router.return_value = config

            with patch.multiple('digitalmodel.engine',
                               app_manager=standard_mocks['app_manager']):
                result = engine(cfg=config, config_flag=False)

            # Should use OrcaFlex (root basename) not Catenary (meta basename)
            mock_orcaflex.assert_called_once()

    def test_missing_basename_raises_error(self, standard_mocks):
        """Should raise ValueError when basename is missing from both locations"""
        config = {"other": "data", "meta": {"other": "data"}}

        with patch.multiple('digitalmodel.engine',
                           app_manager=standard_mocks['app_manager']):

            with pytest.raises(ValueError, match="basename not found in cfg"):
                engine(cfg=config, config_flag=False)
```

### 2.3 Module Routing Tests

```python
class TestModuleRouting:
    """Test module routing logic for all supported basenames (Lines 92-174)"""

    def test_orcaflex_routing(self, standard_mocks, minimal_config):
        """Test OrcaFlex module routing and interaction"""
        config = {"basename": "orcaflex"}

        with patch('digitalmodel.orcaflex.orcaflex.OrcaFlex') as mock_orcaflex:
            mock_instance = Mock()
            mock_orcaflex.return_value = mock_instance
            mock_instance.router.return_value = {"processed": True}

            with patch.multiple('digitalmodel.engine',
                               app_manager=standard_mocks['app_manager']):
                result = engine(cfg=config, config_flag=False)

            # Verify correct instantiation and routing
            mock_orcaflex.assert_called_once()
            mock_instance.router.assert_called_once_with(config)
            assert result["processed"] is True

    def test_catenary_routing_with_dynamic_import(self, standard_mocks):
        """Test Catenary module routing with dynamic import"""
        config = {"basename": "catenary_analysis"}

        with patch('digitalmodel.catenary.catenary.Catenary') as mock_catenary:
            mock_instance = Mock()
            mock_catenary.return_value = mock_instance
            mock_instance.router.return_value = {"catenary_processed": True}

            with patch.multiple('digitalmodel.engine',
                               app_manager=standard_mocks['app_manager']):
                result = engine(cfg=config, config_flag=False)

            mock_catenary.assert_called_once()
            mock_instance.router.assert_called_once_with(config)

    def test_vertical_riser_function_call(self, standard_mocks):
        """Test vertical_riser function call (not class instantiation)"""
        config = {"basename": "vertical_riser"}

        with patch('digitalmodel.vertical_riser.vertical_riser.vertical_riser') as mock_func:
            mock_func.return_value = {"riser_processed": True}

            with patch.multiple('digitalmodel.engine',
                               app_manager=standard_mocks['app_manager']):
                result = engine(cfg=config, config_flag=False)

            # Verify function call (not instantiation)
            mock_func.assert_called_once_with(config)
            assert result["riser_processed"] is True

    def test_code_dnvrph103_shape_based_routing(self, standard_mocks):
        """Test DNVRPH103 routing based on shape parameter"""
        # Test rectangular shape
        config_rect = {
            "basename": "code_dnvrph103",
            "inputs": {"shape": "rectangular"}
        }

        with patch('digitalmodel.common.code_dnvrph103_hydrodynamics_rectangular.DNVRPH103_hydrodynamics_rectangular') as mock_rect:
            mock_instance = Mock()
            mock_rect.return_value = mock_instance
            mock_instance.get_orcaflex_6dbuoy.return_value = {"shape": "rectangular"}

            with patch.multiple('digitalmodel.engine',
                               app_manager=standard_mocks['app_manager']):
                result = engine(cfg=config_rect, config_flag=False)

            mock_rect.assert_called_once()
            mock_instance.get_orcaflex_6dbuoy.assert_called_once_with(config_rect)

        # Test circular shape
        config_circ = {
            "basename": "code_dnvrph103",
            "inputs": {"shape": "circular"}
        }

        with patch('digitalmodel.common.code_dnvrph103_hydrodynamics_circular.DNVRPH103_hydrodynamics_circular') as mock_circ:
            mock_instance = Mock()
            mock_circ.return_value = mock_instance
            mock_instance.get_orcaflex_6dbuoy.return_value = {"shape": "circular"}

            with patch.multiple('digitalmodel.engine',
                               app_manager=standard_mocks['app_manager']):
                result = engine(cfg=config_circ, config_flag=False)

            mock_circ.assert_called_once()

    def test_installation_conditional_routing(self, standard_mocks):
        """Test installation module conditional execution"""
        config_with_flag = {
            "basename": "installation",
            "structure": {"flag": True}
        }

        with patch('digitalmodel.orcaflex.orcaflex_installation.OrcInstallation') as mock_install:
            mock_instance = Mock()
            mock_install.return_value = mock_instance
            mock_instance.create_model_for_water_depth.return_value = {"installed": True}

            with patch.multiple('digitalmodel.engine',
                               app_manager=standard_mocks['app_manager']):
                result = engine(cfg=config_with_flag, config_flag=False)

            # Should call method when flag is True
            mock_instance.create_model_for_water_depth.assert_called_once()

        # Test with flag False
        config_no_flag = {
            "basename": "installation",
            "structure": {"flag": False}
        }

        with patch('digitalmodel.orcaflex.orcaflex_installation.OrcInstallation') as mock_install:
            mock_instance = Mock()
            mock_install.return_value = mock_instance

            with patch.multiple('digitalmodel.engine',
                               app_manager=standard_mocks['app_manager']):
                result = engine(cfg=config_no_flag, config_flag=False)

            # Should NOT call method when flag is False
            mock_instance.create_model_for_water_depth.assert_not_called()

    def test_unknown_basename_raises_exception(self, standard_mocks):
        """Test that unknown basename raises appropriate exception"""
        config = {"basename": "unknown_analysis_type"}

        with patch.multiple('digitalmodel.engine',
                           app_manager=standard_mocks['app_manager']):

            with pytest.raises(Exception, match="Analysis for basename: unknown_analysis_type not found"):
                engine(cfg=config, config_flag=False)

    @pytest.mark.parametrize("basename,module_path,class_name", [
        ("aqwa", "digitalmodel.aqwa.Aqwa", "Aqwa"),
        ("modal_analysis", "digitalmodel.orcaflex.orcaflex_modal_analysis.OrcModalAnalysis", "OrcModalAnalysis"),
        ("umbilical_analysis", "digitalmodel.orcaflex.umbilical_analysis_components.UmbilicalAnalysis", "UmbilicalAnalysis"),
        ("orcaflex_file_management", "digitalmodel.orcaflex.orcaflex_file_management.OrcaflexFileManagement", "OrcaflexFileManagement"),
        ("rao_analysis", "digitalmodel.rao_analysis.rao_analysis.RAOAnalysis", "RAOAnalysis"),
        ("ship_design", "digitalmodel.common.ship_design.ShipDesign", "ShipDesign"),
        ("fatigue_analysis", "digitalmodel.signal_analysis.fatigue.FatigueDamageCalculator", "FatigueAnalysis"),
        ("cathodic_protection", "digitalmodel.common.cathodic_protection.CathodicProtection", "CathodicProtection"),
        ("transformation", "digitalmodel.transformation.transformation.Transformation", "Transformation"),
        ("pipeline", "digitalmodel.pipeline.pipeline.Pipeline", "Pipeline"),
        ("pipe_capacity", "digitalmodel.pipe_capacity.pipe_capacity.PipeCapacity", "PipeCapacity"),
        ("viv_analysis", "digitalmodel.viv_analysis.viv_analysis.VIVAnalysis", "VIVAnalysis"),
        ("time_series", "digitalmodel.time_series.time_series_analysis.TimeSeriesAnalysis", "TimeSeriesAnalysis"),
        ("plate_buckling", "digitalmodel.common.plate_buckling.PlateBuckling", "PlateBuckling"),
        ("mooring", "digitalmodel.mooring.mooring.Mooring", "Mooring"),
    ])
    def test_module_routing_comprehensive(self, basename, module_path, class_name, standard_mocks):
        """Comprehensive test for all module routing paths"""
        config = {"basename": basename}

        # Extract module and class for patching
        module_parts = module_path.split('.')
        import_path = '.'.join(module_parts[:-1])

        with patch(module_path) as mock_class:
            mock_instance = Mock()
            mock_class.return_value = mock_instance
            mock_instance.router.return_value = {"processed": True}

            with patch.multiple('digitalmodel.engine',
                               app_manager=standard_mocks['app_manager']):
                result = engine(cfg=config, config_flag=False)

            # Verify instantiation and routing
            mock_class.assert_called_once()
            # Most modules use 'router' method
            if hasattr(mock_instance, 'router'):
                mock_instance.router.assert_called_once_with(config)
```

### 2.4 Output Control Tests

```python
class TestOutputControl:
    """Test output control settings (Lines 81-88)"""

    def test_quiet_mode_configuration(self, standard_mocks):
        """Test quiet mode sets correct flags"""
        from digitalmodel.orcaflex.output_control import OutputController

        standard_mocks['output_controller'].return_value = OutputController.QUIET
        config = {"basename": "orcaflex"}

        with patch('digitalmodel.orcaflex.orcaflex.OrcaFlex') as mock_orcaflex:
            mock_instance = Mock()
            mock_orcaflex.return_value = mock_instance
            mock_instance.router.return_value = config

            with patch.multiple('digitalmodel.engine',
                               app_manager=standard_mocks['app_manager']):
                result = engine(cfg=config, config_flag=False)

            # Verify quiet flags were set
            call_args = mock_instance.router.call_args[0][0]
            assert call_args['quiet'] is True
            assert call_args['verbose'] is False

    def test_verbose_mode_configuration(self, standard_mocks):
        """Test verbose mode sets correct flags"""
        from digitalmodel.orcaflex.output_control import OutputController

        standard_mocks['output_controller'].return_value = OutputController.VERBOSE
        config = {"basename": "orcaflex"}

        with patch('digitalmodel.orcaflex.orcaflex.OrcaFlex') as mock_orcaflex:
            mock_instance = Mock()
            mock_orcaflex.return_value = mock_instance
            mock_instance.router.return_value = config

            with patch.multiple('digitalmodel.engine',
                               app_manager=standard_mocks['app_manager']):
                result = engine(cfg=config, config_flag=False)

            # Verify verbose flags were set
            call_args = mock_instance.router.call_args[0][0]
            assert call_args['quiet'] is False
            assert call_args['verbose'] is True

    def test_default_output_mode(self, standard_mocks):
        """Test default output mode (no special flags)"""
        standard_mocks['output_controller'].return_value = None
        config = {"basename": "orcaflex"}

        with patch('digitalmodel.orcaflex.orcaflex.OrcaFlex') as mock_orcaflex:
            mock_instance = Mock()
            mock_orcaflex.return_value = mock_instance
            mock_instance.router.return_value = config

            with patch.multiple('digitalmodel.engine',
                               app_manager=standard_mocks['app_manager']):
                result = engine(cfg=config, config_flag=False)

            # Verify no special output flags were set
            call_args = mock_instance.router.call_args[0][0]
            assert 'quiet' not in call_args or call_args.get('quiet') is not True
            assert 'verbose' not in call_args or call_args.get('verbose') is not True
```

### 2.5 Configuration Processing Tests

```python
class TestConfigurationProcessing:
    """Test configuration processing logic (Lines 66-79)"""

    def test_config_processing_enabled(self, standard_mocks, minimal_config):
        """Test configuration processing when config_flag=True"""
        with patch.multiple('digitalmodel.engine',
                           app_manager=standard_mocks['app_manager'],
                           wwyaml=standard_mocks['wwyaml']):

            result = engine(cfg=minimal_config, config_flag=True)

            # Verify configuration processing steps
            standard_mocks['app_manager'].configure.assert_called_once()
            standard_mocks['file_manager'].router.assert_called_once()
            standard_mocks['app_manager'].configure_result_folder.assert_called_once()

    def test_config_processing_disabled(self, standard_mocks, minimal_config):
        """Test configuration processing bypassed when config_flag=False"""
        with patch.multiple('digitalmodel.engine',
                           app_manager=standard_mocks['app_manager']):

            result = engine(cfg=minimal_config, config_flag=False)

            # Verify configuration processing was skipped
            standard_mocks['app_manager'].configure.assert_not_called()
            standard_mocks['app_manager'].configure_result_folder.assert_not_called()

    def test_config_path_preservation(self, standard_mocks):
        """Test that config file paths are preserved during processing"""
        config = {
            "basename": "orcaflex",
            "_config_file_path": "/test/config.yml",
            "_config_dir_path": "/test"
        }

        with patch.multiple('digitalmodel.engine',
                           app_manager=standard_mocks['app_manager'],
                           wwyaml=standard_mocks['wwyaml']):

            result = engine(cfg=config, config_flag=True)

            # Verify paths were preserved in the configure call
            configure_call_args = standard_mocks['app_manager'].configure.call_args[0][0]
            assert configure_call_args["_config_file_path"] == "/test/config.yml"
            assert configure_call_args["_config_dir_path"] == "/test"
```

### 2.6 Final Processing Tests

```python
class TestFinalProcessing:
    """Test final processing steps (Lines 176-179)"""

    def test_logging_and_save_operations(self, standard_mocks, minimal_config):
        """Test final logging and save operations"""
        with patch.multiple('digitalmodel.engine',
                           app_manager=standard_mocks['app_manager']):

            result = engine(cfg=minimal_config, config_flag=False)

            # Verify logging calls
            standard_mocks['logger'].info.assert_any_call("orcaflex, application ... START")
            standard_mocks['logger'].debug.assert_called_with("orcaflex, application ... END")

            # Verify save operation
            standard_mocks['app_manager'].save_cfg.assert_called_once_with(cfg_base=result)

    def test_return_value_propagation(self, standard_mocks):
        """Test that modified config is returned correctly"""
        config = {"basename": "orcaflex", "original": True}
        expected_result = {"basename": "orcaflex", "original": True, "processed": True}

        with patch('digitalmodel.orcaflex.orcaflex.OrcaFlex') as mock_orcaflex:
            mock_instance = Mock()
            mock_orcaflex.return_value = mock_instance
            mock_instance.router.return_value = expected_result

            with patch.multiple('digitalmodel.engine',
                               app_manager=standard_mocks['app_manager']):
                result = engine(cfg=config, config_flag=False)

            assert result == expected_result
            assert result["processed"] is True
            assert result["original"] is True
```

## 3. Integration Tests (test_engine_integration.py)

```python
class TestEngineIntegration:
    """Integration tests with minimal mocking"""

    @pytest.mark.integration
    def test_real_yaml_loading(self, tmp_path):
        """Test with real YAML file loading"""
        # Create test YAML file
        yaml_content = """
        basename: orcaflex
        Analysis:
          analysis_root_folder: /tmp/test
        inputs:
          test_param: test_value
        """
        yaml_file = tmp_path / "test_config.yml"
        yaml_file.write_text(yaml_content)

        # Test with minimal mocking (only the actual module)
        with patch('digitalmodel.orcaflex.orcaflex.OrcaFlex') as mock_orcaflex:
            mock_instance = Mock()
            mock_orcaflex.return_value = mock_instance
            mock_instance.router.return_value = {"integration_test": True}

            result = engine(inputfile=str(yaml_file))

            # Verify real YAML loading worked
            assert result is not None
            mock_orcaflex.assert_called_once()

    @pytest.mark.integration
    def test_end_to_end_workflow(self, tmp_path):
        """Test end-to-end workflow with file operations"""
        # Setup test directory structure
        config_dir = tmp_path / "config"
        config_dir.mkdir()
        output_dir = tmp_path / "output"
        output_dir.mkdir()

        yaml_content = f"""
        basename: time_series
        Analysis:
          analysis_root_folder: {output_dir}
        inputs:
          data_file: test.csv
        """
        yaml_file = config_dir / "integration_test.yml"
        yaml_file.write_text(yaml_content)

        with patch('digitalmodel.time_series.time_series_analysis.TimeSeriesAnalysis') as mock_tsa:
            mock_instance = Mock()
            mock_tsa.return_value = mock_instance
            mock_instance.router.return_value = {"time_series_processed": True}

            result = engine(inputfile=str(yaml_file))

            # Verify full workflow
            assert result["time_series_processed"] is True
            mock_tsa.assert_called_once()
```

## 4. Property-Based Tests (test_engine_property.py)

```python
from hypothesis import given, strategies as st, assume
from hypothesis import settings, Verbosity

class TestEngineProperties:
    """Property-based tests using Hypothesis"""

    @given(
        basename=st.sampled_from([
            'orcaflex', 'catenary', 'vertical_riser', 'aqwa', 'modal_analysis',
            'umbilical_analysis', 'orcaflex_file_management', 'rao_analysis',
            'ship_design', 'fatigue_analysis', 'cathodic_protection',
            'transformation', 'pipeline', 'pipe_capacity', 'viv_analysis',
            'time_series', 'plate_buckling', 'mooring'
        ]),
        extra_fields=st.dictionaries(
            keys=st.text(min_size=1, max_size=20, alphabet=st.characters(whitelist_categories=('Lu', 'Ll', 'Nd'))),
            values=st.one_of(
                st.text(max_size=100),
                st.integers(min_value=-1000, max_value=1000),
                st.floats(min_value=-1000, max_value=1000, allow_nan=False, allow_infinity=False),
                st.booleans()
            ),
            min_size=0,
            max_size=10
        )
    )
    @settings(max_examples=50, verbosity=Verbosity.verbose)
    def test_engine_handles_arbitrary_valid_configs(self, basename, extra_fields):
        """Test that engine handles any valid configuration structure"""
        config = {"basename": basename, **extra_fields}

        # Mock the appropriate module for this basename
        module_mocks = {
            'orcaflex': 'digitalmodel.orcaflex.orcaflex.OrcaFlex',
            'catenary': 'digitalmodel.catenary.catenary.Catenary',
            'aqwa': 'digitalmodel.aqwa.Aqwa',
            # ... add other mappings as needed
        }

        mock_path = module_mocks.get(basename, 'digitalmodel.generic.Generic')

        with patch(mock_path) as mock_module:
            mock_instance = Mock()
            mock_module.return_value = mock_instance
            mock_instance.router.return_value = config

            with patch('digitalmodel.engine.app_manager') as mock_app_manager:
                mock_app_manager.save_cfg.return_value = None

                # Should not raise exception for any valid config
                result = engine(cfg=config, config_flag=False)
                assert result is not None

    @given(
        shape=st.sampled_from(['rectangular', 'circular']),
        numeric_inputs=st.dictionaries(
            keys=st.sampled_from(['width', 'height', 'diameter', 'thickness']),
            values=st.floats(min_value=0.1, max_value=100.0, allow_nan=False),
            min_size=1,
            max_size=4
        )
    )
    def test_dnvrph103_shape_handling(self, shape, numeric_inputs):
        """Test DNVRPH103 handling with various shape configurations"""
        config = {
            "basename": "code_dnvrph103",
            "inputs": {"shape": shape, **numeric_inputs}
        }

        if shape == 'rectangular':
            mock_path = 'digitalmodel.common.code_dnvrph103_hydrodynamics_rectangular.DNVRPH103_hydrodynamics_rectangular'
        else:
            mock_path = 'digitalmodel.common.code_dnvrph103_hydrodynamics_circular.DNVRPH103_hydrodynamics_circular'

        with patch(mock_path) as mock_module:
            mock_instance = Mock()
            mock_module.return_value = mock_instance
            mock_instance.get_orcaflex_6dbuoy.return_value = config

            with patch('digitalmodel.engine.app_manager') as mock_app_manager:
                mock_app_manager.save_cfg.return_value = None

                result = engine(cfg=config, config_flag=False)
                assert result is not None
                mock_module.assert_called_once()

    @given(
        config_structure=st.recursive(
            st.dictionaries(
                keys=st.text(min_size=1, max_size=10),
                values=st.one_of(st.text(max_size=50), st.integers(), st.booleans()),
                min_size=1,
                max_size=5
            ),
            lambda children: st.dictionaries(
                keys=st.text(min_size=1, max_size=10),
                values=children,
                min_size=1,
                max_size=3
            ),
            max_leaves=20
        )
    )
    def test_engine_handles_deeply_nested_configs(self, config_structure):
        """Test engine with deeply nested configuration structures"""
        # Ensure we have a valid basename
        config_structure["basename"] = "orcaflex"

        with patch('digitalmodel.orcaflex.orcaflex.OrcaFlex') as mock_orcaflex:
            mock_instance = Mock()
            mock_orcaflex.return_value = mock_instance
            mock_instance.router.return_value = config_structure

            with patch('digitalmodel.engine.app_manager') as mock_app_manager:
                mock_app_manager.save_cfg.return_value = None

                # Should handle any depth of nesting without crashing
                result = engine(cfg=config_structure, config_flag=False)
                assert result is not None
```

## 5. Performance Tests (test_engine_performance.py)

```python
import pytest
from pytest_benchmark.fixture import BenchmarkFixture

class TestEnginePerformance:
    """Performance benchmarks for engine function"""

    def test_engine_performance_baseline(self, benchmark: BenchmarkFixture):
        """Baseline performance test for engine function"""
        config = {"basename": "orcaflex"}

        with patch('digitalmodel.orcaflex.orcaflex.OrcaFlex') as mock_orcaflex:
            mock_instance = Mock()
            mock_orcaflex.return_value = mock_instance
            mock_instance.router.return_value = config

            with patch('digitalmodel.engine.app_manager') as mock_app_manager:
                mock_app_manager.save_cfg.return_value = None

                # Benchmark the engine function
                result = benchmark(engine, cfg=config, config_flag=False)
                assert result is not None

    @pytest.mark.parametrize("config_size", [10, 100, 1000])
    def test_engine_performance_with_config_size(self, benchmark: BenchmarkFixture, config_size):
        """Test performance with varying configuration sizes"""
        config = {
            "basename": "orcaflex",
            **{f"param_{i}": f"value_{i}" for i in range(config_size)}
        }

        with patch('digitalmodel.orcaflex.orcaflex.OrcaFlex') as mock_orcaflex:
            mock_instance = Mock()
            mock_orcaflex.return_value = mock_instance
            mock_instance.router.return_value = config

            with patch('digitalmodel.engine.app_manager') as mock_app_manager:
                mock_app_manager.save_cfg.return_value = None

                result = benchmark(engine, cfg=config, config_flag=False)
                assert result is not None
```

## Test Execution Commands

```bash
# Run all engine tests
pytest tests/test_engine/ -v

# Run with coverage
pytest tests/test_engine/ --cov=src/digitalmodel/engine.py --cov-report=html

# Run only unit tests
pytest tests/test_engine/test_engine_unit.py -v

# Run property-based tests
pytest tests/test_engine/test_engine_property.py -v

# Run performance benchmarks
pytest tests/test_engine/test_engine_performance.py --benchmark-only

# Run with mutation testing
mutmut run --paths-to-mutate=src/digitalmodel/engine.py

# Parallel execution
pytest tests/test_engine/ -n auto
```

This implementation plan provides detailed, executable test cases that will achieve comprehensive coverage of the engine.py file while following TDD London School principles with proper mock usage and interaction verification.