"""
Additional engine tests to achieve higher coverage.

This module provides targeted tests to cover the missing lines identified
in the coverage analysis, focusing on edge cases and conditional paths.
"""

import sys
import os
import tempfile
from pathlib import Path
from unittest.mock import patch, MagicMock
import yaml

# Mock all problematic imports
mock_modules = {
    'tabulate': MagicMock(),
    'assetutilities.common.ApplicationManager': MagicMock(),
    'assetutilities.common.data': MagicMock(),
    'assetutilities.common.file_management': MagicMock(),
    'assetutilities.common.update_deep': MagicMock(),
    'assetutilities.common.yml_utilities': MagicMock(),
    'digitalmodel.aqwa': MagicMock(),
    'digitalmodel.modules.aqwa.mes_files': MagicMock(),
    'digitalmodel.common.cathodic_protection': MagicMock(),
    'digitalmodel.common.code_dnvrph103_hydrodynamics_circular': MagicMock(),
    'digitalmodel.common.code_dnvrph103_hydrodynamics_rectangular': MagicMock(),
    'digitalmodel.modules.signal_analysis.fatigue': MagicMock(),
    'digitalmodel.common.ship_design': MagicMock(),
    'digitalmodel.modules.mooring.mooring': MagicMock(),
    'digitalmodel.modules.orcaflex.orcaflex': MagicMock(),
    'digitalmodel.modules.orcaflex.orcaflex_file_management': MagicMock(),
    'digitalmodel.modules.orcaflex.orcaflex_installation': MagicMock(),
    'digitalmodel.modules.orcaflex.orcaflex_modal_analysis': MagicMock(),
    'digitalmodel.modules.orcaflex.umbilical_analysis_components': MagicMock(),
    'digitalmodel.modules.pipe_capacity.pipe_capacity': MagicMock(),
    'digitalmodel.modules.pipeline.pipeline': MagicMock(),
    'digitalmodel.modules.rao_analysis.rao_analysis': MagicMock(),
    'digitalmodel.modules.time_series.time_series_analysis': MagicMock(),
    'digitalmodel.modules.transformation.transformation': MagicMock(),
    'digitalmodel.modules.vertical_riser.vertical_riser': MagicMock(),
    'digitalmodel.modules.viv_analysis.viv_analysis': MagicMock(),
    'digitalmodel.common.plate_buckling': MagicMock(),
    'loguru': MagicMock(),
    'digitalmodel.modules.orcaflex.output_control': MagicMock(),
}

for module_name, mock_obj in mock_modules.items():
    sys.modules[module_name] = mock_obj

# Mock AttributeDict
class MockAttributeDict(dict):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.__dict__ = self

sys.modules['assetutilities.common.update_deep'].AttributeDict = MockAttributeDict

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from digitalmodel.engine import engine


def test_engine_configuration_paths():
    """Test configuration-related paths and edge cases."""
    print("Testing configuration paths...")

    # Test config_flag=True path
    cfg = MockAttributeDict({"basename": "transformation"})

    with patch('digitalmodel.engine.app_manager') as mock_app_manager:
        with patch('digitalmodel.engine.FileManagement') as mock_fm:
            # Setup mocks for configuration path
            mock_app_manager.configure.return_value = cfg
            mock_app_manager.configure_result_folder.return_value = ({}, cfg)
            mock_app_manager.save_cfg = MagicMock()

            mock_fm_instance = MagicMock()
            mock_fm_instance.router.return_value = cfg
            mock_fm.return_value = mock_fm_instance

            with patch('digitalmodel.engine.logger'):
                result = engine(cfg=cfg, config_flag=True)

            # Verify configuration methods were called
            mock_app_manager.configure.assert_called_once()
            mock_app_manager.configure_result_folder.assert_called_once()

    print("✓ Configuration paths tested")


def test_yaml_input_path():
    """Test YAML input file processing path."""
    print("Testing YAML input processing...")

    # Create a temporary YAML file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.yml', delete=False) as f:
        yaml.dump({"basename": "transformation"}, f)
        temp_file = f.name

    try:
        with patch('digitalmodel.engine.app_manager') as mock_app_manager:
            with patch('digitalmodel.engine.wwyaml') as mock_wwyaml:
                # Setup validation and YAML loading
                mock_app_manager.validate_arguments_run_methods.return_value = (temp_file, {})
                mock_wwyaml.ymlInput.return_value = {"basename": "transformation"}

                with patch('digitalmodel.engine.logger'):
                    with patch('os.path.exists', return_value=True):
                        with patch('os.path.abspath', return_value=temp_file):
                            with patch('os.path.dirname', return_value=os.path.dirname(temp_file)):
                                result = engine(inputfile=temp_file, cfg=None)

                # Verify YAML processing was called
                mock_app_manager.validate_arguments_run_methods.assert_called_once()
                mock_wwyaml.ymlInput.assert_called_once()

    finally:
        os.unlink(temp_file)

    print("✓ YAML input processing tested")


def test_output_control_levels():
    """Test output control level settings."""
    print("Testing output control levels...")

    cfg = MockAttributeDict({"basename": "transformation"})

    # Test QUIET output level
    with patch('digitalmodel.engine.get_output_level_from_argv') as mock_output:
        from digitalmodel.modules.orcaflex.output_control import OutputController
        mock_output.return_value = OutputController.QUIET

        with patch('digitalmodel.engine.logger'):
            result = engine(cfg=cfg, config_flag=False)

        assert result.get('quiet') is True
        assert result.get('verbose') is False

    # Test VERBOSE output level
    with patch('digitalmodel.engine.get_output_level_from_argv') as mock_output:
        mock_output.return_value = OutputController.VERBOSE

        with patch('digitalmodel.engine.logger'):
            result = engine(cfg=cfg, config_flag=False)

        assert result.get('quiet') is False
        assert result.get('verbose') is True

    print("✓ Output control levels tested")


def test_basename_extraction_meta():
    """Test basename extraction from meta structure."""
    print("Testing meta basename extraction...")

    # Test basename in meta
    cfg_meta = MockAttributeDict({"meta": {"basename": "transformation"}})

    with patch('digitalmodel.engine.logger'):
        result = engine(cfg=cfg_meta, config_flag=False)

    assert result is not None
    print("✓ Meta basename extraction tested")


def test_config_path_preservation():
    """Test config file path preservation."""
    print("Testing config path preservation...")

    test_file = "/tmp/test_config.yml"
    test_dir = "/tmp"

    with patch('digitalmodel.engine.app_manager') as mock_app_manager:
        with patch('digitalmodel.engine.wwyaml') as mock_wwyaml:
            mock_app_manager.validate_arguments_run_methods.return_value = (test_file, {})
            mock_wwyaml.ymlInput.return_value = {"basename": "transformation"}

            with patch('os.path.exists', return_value=True):
                with patch('os.path.abspath', return_value=test_file):
                    with patch('os.path.dirname', return_value=test_dir):
                        with patch('digitalmodel.engine.logger'):
                            result = engine(inputfile=test_file, cfg=None)

            # Verify path tracking
            assert "_config_file_path" in result
            assert "_config_dir_path" in result

    print("✓ Config path preservation tested")


def test_rigging_dynamic_import():
    """Test rigging dynamic import path."""
    print("Testing rigging dynamic import...")

    cfg = MockAttributeDict({"basename": "rigging"})

    with patch('digitalmodel.engine.Rigging') as mock_rigging:
        mock_instance = MagicMock()
        mock_instance.get_rigging_groups.return_value = cfg
        mock_rigging.return_value = mock_instance

        with patch('digitalmodel.engine.logger'):
            result = engine(cfg=cfg, config_flag=False)

        mock_rigging.assert_called_once()
        mock_instance.get_rigging_groups.assert_called_once()

    print("✓ Rigging dynamic import tested")


def test_catenary_dynamic_import():
    """Test catenary dynamic import path."""
    print("Testing catenary dynamic import...")

    cfg = MockAttributeDict({"basename": "catenary_analysis"})

    with patch('digitalmodel.engine.Catenary') as mock_catenary:
        mock_instance = MagicMock()
        mock_instance.router.return_value = cfg
        mock_catenary.return_value = mock_instance

        with patch('digitalmodel.engine.logger'):
            result = engine(cfg=cfg, config_flag=False)

        mock_catenary.assert_called_once()
        mock_instance.router.assert_called_once()

    print("✓ Catenary dynamic import tested")


def test_app_manager_save_cfg():
    """Test app_manager.save_cfg call."""
    print("Testing app_manager.save_cfg...")

    cfg = MockAttributeDict({"basename": "transformation"})

    with patch('digitalmodel.engine.app_manager') as mock_app_manager:
        mock_app_manager.save_cfg = MagicMock()

        with patch('digitalmodel.engine.logger'):
            result = engine(cfg=cfg, config_flag=False)

        mock_app_manager.save_cfg.assert_called_once()

    print("✓ app_manager.save_cfg tested")


def test_installation_structure_flag_false():
    """Test installation with structure flag False."""
    print("Testing installation with structure flag False...")

    cfg = MockAttributeDict({
        "basename": "installation",
        "structure": {"flag": False}
    })

    with patch('digitalmodel.engine.OrcInstallation') as mock_install:
        mock_instance = MagicMock()
        mock_install.return_value = mock_instance

        with patch('digitalmodel.engine.logger'):
            result = engine(cfg=cfg, config_flag=False)

        # Should not call create_model_for_water_depth when flag is False
        mock_instance.create_model_for_water_depth.assert_not_called()

    print("✓ Installation structure flag False tested")


def test_orcaflex_variations():
    """Test all OrcaFlex basename variations."""
    print("Testing OrcaFlex variations...")

    orcaflex_basenames = ["orcaflex", "orcaflex_analysis", "orcaflex_post_process"]

    for basename in orcaflex_basenames:
        cfg = MockAttributeDict({"basename": basename})

        with patch('digitalmodel.engine.OrcaFlex') as mock_orcaflex:
            mock_instance = MagicMock()
            mock_instance.router.return_value = cfg
            mock_orcaflex.return_value = mock_instance

            with patch('digitalmodel.engine.logger'):
                result = engine(cfg=cfg, config_flag=False)

            mock_orcaflex.assert_called_once()

    print("✓ OrcaFlex variations tested")


def test_copy_and_paste_module():
    """Test copy_and_paste module routing."""
    print("Testing copy_and_paste module...")

    cfg = MockAttributeDict({"basename": "copy_and_paste"})

    with patch('digitalmodel.engine.CopyAndPasteFiles') as mock_cpf:
        mock_instance = MagicMock()
        mock_instance.iterate_all_cfgs.return_value = cfg
        mock_cpf.return_value = mock_instance

        with patch('digitalmodel.engine.logger'):
            result = engine(cfg=cfg, config_flag=False)

        mock_cpf.assert_called_once()
        mock_instance.iterate_all_cfgs.assert_called_once()

    print("✓ copy_and_paste module tested")


def run_all_additional_tests():
    """Run all additional tests for better coverage."""
    print("🧪 Running Additional Engine Tests for Better Coverage\n")

    test_functions = [
        test_engine_configuration_paths,
        test_yaml_input_path,
        test_output_control_levels,
        test_basename_extraction_meta,
        test_config_path_preservation,
        test_rigging_dynamic_import,
        test_catenary_dynamic_import,
        test_app_manager_save_cfg,
        test_installation_structure_flag_false,
        test_orcaflex_variations,
        test_copy_and_paste_module,
    ]

    passed = 0
    failed = 0

    for test_func in test_functions:
        try:
            test_func()
            passed += 1
        except Exception as e:
            print(f"❌ {test_func.__name__} failed: {e}")
            failed += 1

    print(f"\n📊 Additional Tests Summary:")
    print(f"Passed: {passed}")
    print(f"Failed: {failed}")
    print(f"Total: {len(test_functions)}")

    return passed, failed


if __name__ == "__main__":
    run_all_additional_tests()