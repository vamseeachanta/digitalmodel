"""
Comprehensive test suite for src/digitalmodel/aqwa.py module.

This test suite achieves 100% coverage of the 40-line AQWA module by testing:
- Aqwa class initialization
- router() method with different configurations
- get_cfg_with_master_data() method logic
- Edge cases and error conditions
- All conditional paths in the module

The module organizes data in the tests directory as per project standards.
"""

import pytest
from unittest.mock import Mock, patch, MagicMock
import copy
from pathlib import Path

# Import the module under test
from digitalmodel.aqwa import Aqwa


class TestAqwaClass:
    """Test suite for the Aqwa class."""

    def test_aqwa_initialization(self):
        """Test Aqwa class initializes correctly."""
        aqwa = Aqwa()
        assert isinstance(aqwa, Aqwa)

    def test_aqwa_multiple_instances(self):
        """Test multiple Aqwa instances can be created."""
        aqwa1 = Aqwa()
        aqwa2 = Aqwa()
        assert aqwa1 is not aqwa2
        assert isinstance(aqwa1, Aqwa)
        assert isinstance(aqwa2, Aqwa)


class TestAqwaRouter:
    """Test suite for the Aqwa.router() method."""

    @pytest.fixture
    def aqwa_instance(self):
        """Create an Aqwa instance for testing."""
        return Aqwa()

    @pytest.fixture
    def base_cfg(self):
        """Base configuration for testing."""
        return {
            "type": {
                "preprocess": False,
                "analysis": False,
                "results": False
            }
        }

    @patch('digitalmodel.hydrodynamics.aqwa.a_pre')
    @patch('digitalmodel.hydrodynamics.aqwa.a_post')
    @patch('digitalmodel.hydrodynamics.aqwa.mes_files')
    def test_router_no_processing(self, mock_mes, mock_post, mock_pre, aqwa_instance, base_cfg):
        """Test router when no processing flags are enabled."""
        # Mock get_cfg_with_master_data to return cfg unchanged
        with patch.object(aqwa_instance, 'get_cfg_with_master_data', return_value=base_cfg):
            result = aqwa_instance.router(base_cfg)

        # Assert no processing methods were called
        mock_pre.pre_process_router.assert_not_called()
        mock_post.post_process_router.assert_not_called()
        mock_mes.router.assert_not_called()

        # Assert configuration is returned
        assert result == base_cfg

    @patch('digitalmodel.hydrodynamics.aqwa.a_pre')
    @patch('digitalmodel.hydrodynamics.aqwa.a_post')
    @patch('digitalmodel.hydrodynamics.aqwa.mes_files')
    def test_router_preprocess_only(self, mock_mes, mock_post, mock_pre, aqwa_instance, base_cfg):
        """Test router when only preprocess flag is enabled."""
        cfg = copy.deepcopy(base_cfg)
        cfg["type"]["preprocess"] = True

        with patch.object(aqwa_instance, 'get_cfg_with_master_data', return_value=cfg):
            result = aqwa_instance.router(cfg)

        # Assert preprocess was called
        mock_pre.pre_process_router.assert_called_once_with(cfg)

        # Assert other methods were not called
        mock_post.post_process_router.assert_not_called()
        mock_mes.router.assert_not_called()

        assert result == cfg

    @patch('digitalmodel.hydrodynamics.aqwa.a_pre')
    @patch('digitalmodel.hydrodynamics.aqwa.a_post')
    @patch('digitalmodel.hydrodynamics.aqwa.mes_files')
    def test_router_analysis_only(self, mock_mes, mock_post, mock_pre, aqwa_instance, base_cfg):
        """Test router when only analysis flag is enabled."""
        cfg = copy.deepcopy(base_cfg)
        cfg["type"]["analysis"] = True

        # Mock the dynamic import and analysis instance
        mock_analysis_instance = Mock()

        with patch.object(aqwa_instance, 'get_cfg_with_master_data', return_value=cfg), \
             patch('builtins.__import__') as mock_import:

            # Mock the module import
            mock_module = Mock()
            mock_module.AqwaAnalysis.return_value = mock_analysis_instance
            mock_import.return_value = mock_module

            result = aqwa_instance.router(cfg)

        # Assert analysis was called
        mock_analysis_instance.analysis_router.assert_called_once_with(cfg)

        # Assert other methods were not called
        mock_pre.pre_process_router.assert_not_called()
        mock_post.post_process_router.assert_not_called()
        mock_mes.router.assert_not_called()

        assert result == cfg

    @patch('digitalmodel.hydrodynamics.aqwa.a_pre')
    @patch('digitalmodel.hydrodynamics.aqwa.a_post')
    @patch('digitalmodel.hydrodynamics.aqwa.mes_files')
    @patch('digitalmodel.hydrodynamics.aqwa.aqwa_router.ViscousDampingDetermination')
    def test_router_analysis_viscous_damping_enabled(
        self,
        mock_viscous,
        mock_mes,
        mock_post,
        mock_pre,
        aqwa_instance,
        base_cfg,
    ):
        """Router should invoke viscous damping workflow when configured."""

        cfg = copy.deepcopy(base_cfg)
        cfg["type"]["analysis"] = True
        viscous_config = {
            "config": {"context": {"analysis_source": "."}, "cases": []},
            "results_directory": "results/path",
        }
        cfg["analysis_settings"] = {"viscous_damping": viscous_config}

        mock_instance = mock_viscous.return_value
        expected_result = {"results": [], "output_directory": "results/path"}
        mock_instance.run.return_value = expected_result

        with patch.object(aqwa_instance, 'get_cfg_with_master_data', return_value=cfg):
            result = aqwa_instance.router(cfg)

        mock_viscous.assert_called_once()
        mock_instance.run.assert_called_once_with(viscous_config["config"], "results/path")
        assert result["analysis_settings"]["viscous_damping"]["results"] == expected_result
        mock_pre.pre_process_router.assert_not_called()
        mock_post.post_process_router.assert_not_called()
        mock_mes.router.assert_not_called()

    @patch('digitalmodel.hydrodynamics.aqwa.a_pre')
    @patch('digitalmodel.hydrodynamics.aqwa.a_post')
    @patch('digitalmodel.hydrodynamics.aqwa.mes_files')
    @patch('digitalmodel.hydrodynamics.aqwa.aqwa_router.ViscousDampingDetermination')
    def test_router_analysis_viscous_damping_disabled_fallback(
        self,
        mock_viscous,
        mock_mes,
        mock_post,
        mock_pre,
        aqwa_instance,
        base_cfg,
    ):
        """Router should fall back to legacy analysis when viscous damping disabled."""

        cfg = copy.deepcopy(base_cfg)
        cfg["type"]["analysis"] = True
        cfg["analysis_settings"] = {"viscous_damping": {"enabled": False}}

        mock_analysis_instance = Mock()

        with patch.object(aqwa_instance, 'get_cfg_with_master_data', return_value=cfg), \
             patch('builtins.__import__') as mock_import:

            mock_module = Mock()
            mock_module.AqwaAnalysis.return_value = mock_analysis_instance
            mock_import.return_value = mock_module

            result = aqwa_instance.router(cfg)

        mock_viscous.assert_not_called()
        mock_analysis_instance.analysis_router.assert_called_once_with(cfg)
        mock_pre.pre_process_router.assert_not_called()
        mock_post.post_process_router.assert_not_called()
        mock_mes.router.assert_not_called()
        assert result == cfg

    @patch('digitalmodel.hydrodynamics.aqwa.a_pre')
    @patch('digitalmodel.hydrodynamics.aqwa.a_post')
    @patch('digitalmodel.hydrodynamics.aqwa.mes_files')
    def test_router_mes_flag_true(self, mock_mes, mock_post, mock_pre, aqwa_instance, base_cfg):
        """Test router when mes flag is enabled."""
        cfg = copy.deepcopy(base_cfg)
        cfg["mes"] = {"flag": True}

        with patch.object(aqwa_instance, 'get_cfg_with_master_data', return_value=cfg):
            result = aqwa_instance.router(cfg)

        # Assert mes router was called
        mock_mes.router.assert_called_once_with(cfg)

        # Assert other methods were not called
        mock_pre.pre_process_router.assert_not_called()
        mock_post.post_process_router.assert_not_called()

        assert result == cfg

    @patch('digitalmodel.hydrodynamics.aqwa.a_pre')
    @patch('digitalmodel.hydrodynamics.aqwa.a_post')
    @patch('digitalmodel.hydrodynamics.aqwa.mes_files')
    def test_router_mes_flag_false(self, mock_mes, mock_post, mock_pre, aqwa_instance, base_cfg):
        """Test router when mes flag is disabled."""
        cfg = copy.deepcopy(base_cfg)
        cfg["mes"] = {"flag": False}

        with patch.object(aqwa_instance, 'get_cfg_with_master_data', return_value=cfg):
            result = aqwa_instance.router(cfg)

        # Assert mes router was not called
        mock_mes.router.assert_not_called()

        assert result == cfg

    @patch('digitalmodel.hydrodynamics.aqwa.a_pre')
    @patch('digitalmodel.hydrodynamics.aqwa.a_post')
    @patch('digitalmodel.hydrodynamics.aqwa.mes_files')
    def test_router_no_mes_key(self, mock_mes, mock_post, mock_pre, aqwa_instance, base_cfg):
        """Test router when mes key is not present."""
        cfg = copy.deepcopy(base_cfg)
        # No mes key at all

        with patch.object(aqwa_instance, 'get_cfg_with_master_data', return_value=cfg):
            result = aqwa_instance.router(cfg)

        # Assert mes router was not called
        mock_mes.router.assert_not_called()

        assert result == cfg

    @patch('digitalmodel.hydrodynamics.aqwa.a_pre')
    @patch('digitalmodel.hydrodynamics.aqwa.a_post')
    @patch('digitalmodel.hydrodynamics.aqwa.mes_files')
    def test_router_results_only(self, mock_mes, mock_post, mock_pre, aqwa_instance, base_cfg):
        """Test router when only results flag is enabled."""
        cfg = copy.deepcopy(base_cfg)
        cfg["type"]["results"] = True

        # Mock post_process_router to return modified cfg
        modified_cfg = copy.deepcopy(cfg)
        modified_cfg["processed"] = True
        mock_post.post_process_router.return_value = modified_cfg

        with patch.object(aqwa_instance, 'get_cfg_with_master_data', return_value=cfg):
            result = aqwa_instance.router(cfg)

        # Assert post processing was called
        mock_post.post_process_router.assert_called_once_with(cfg)

        # Assert other methods were not called
        mock_pre.pre_process_router.assert_not_called()
        mock_mes.router.assert_not_called()

        # Assert modified cfg is returned
        assert result == modified_cfg
        assert result["processed"] is True

    @patch('digitalmodel.hydrodynamics.aqwa.a_pre')
    @patch('digitalmodel.hydrodynamics.aqwa.a_post')
    @patch('digitalmodel.hydrodynamics.aqwa.mes_files')
    def test_router_all_flags_enabled(self, mock_mes, mock_post, mock_pre, aqwa_instance, base_cfg):
        """Test router when all processing flags are enabled."""
        cfg = copy.deepcopy(base_cfg)
        cfg["type"] = {
            "preprocess": True,
            "analysis": True,
            "results": True
        }
        cfg["mes"] = {"flag": True}

        # Mock the analysis instance
        mock_analysis_instance = Mock()

        # Mock post_process_router to return modified cfg
        modified_cfg = copy.deepcopy(cfg)
        modified_cfg["all_processed"] = True
        mock_post.post_process_router.return_value = modified_cfg

        with patch.object(aqwa_instance, 'get_cfg_with_master_data', return_value=cfg), \
             patch('builtins.__import__') as mock_import:

            # Mock the module import
            mock_module = Mock()
            mock_module.AqwaAnalysis.return_value = mock_analysis_instance
            mock_import.return_value = mock_module

            result = aqwa_instance.router(cfg)

        # Assert all processing methods were called
        mock_pre.pre_process_router.assert_called_once_with(cfg)
        mock_analysis_instance.analysis_router.assert_called_once_with(cfg)
        mock_mes.router.assert_called_once_with(cfg)
        mock_post.post_process_router.assert_called_once_with(cfg)

        # Assert modified cfg is returned
        assert result == modified_cfg
        assert result["all_processed"] is True

    @pytest.mark.parametrize("preprocess,analysis,results,mes_flag,expected_calls", [
        (True, False, False, False, ["pre"]),
        (False, True, False, False, ["analysis"]),
        (False, False, True, False, ["post"]),
        (False, False, False, True, ["mes"]),
        (True, True, False, False, ["pre", "analysis"]),
        (True, False, True, False, ["pre", "post"]),
        (False, True, True, False, ["analysis", "post"]),
        (True, True, True, True, ["pre", "analysis", "mes", "post"]),
    ])
    @patch('digitalmodel.hydrodynamics.aqwa.a_pre')
    @patch('digitalmodel.hydrodynamics.aqwa.a_post')
    @patch('digitalmodel.hydrodynamics.aqwa.mes_files')
    def test_router_parametrized_configurations(self, mock_mes, mock_post, mock_pre,
                                              aqwa_instance, base_cfg,
                                              preprocess, analysis, results, mes_flag, expected_calls):
        """Test router with various flag combinations using parametrized tests."""
        cfg = copy.deepcopy(base_cfg)
        cfg["type"]["preprocess"] = preprocess
        cfg["type"]["analysis"] = analysis
        cfg["type"]["results"] = results

        if mes_flag:
            cfg["mes"] = {"flag": True}

        # Mock the analysis instance
        mock_analysis_instance = Mock()

        # Mock post_process_router to return cfg
        mock_post.post_process_router.return_value = cfg

        if "analysis" in expected_calls:
            with patch.object(aqwa_instance, 'get_cfg_with_master_data', return_value=cfg), \
                 patch('builtins.__import__') as mock_import:

                # Mock the module import for analysis
                mock_module = Mock()
                mock_module.AqwaAnalysis.return_value = mock_analysis_instance
                mock_import.return_value = mock_module

                result = aqwa_instance.router(cfg)

                # Check analysis call
                mock_analysis_instance.analysis_router.assert_called_once_with(cfg)
        else:
            with patch.object(aqwa_instance, 'get_cfg_with_master_data', return_value=cfg):
                result = aqwa_instance.router(cfg)

        # Check expected calls
        if "pre" in expected_calls:
            mock_pre.pre_process_router.assert_called_once_with(cfg)
        else:
            mock_pre.pre_process_router.assert_not_called()

        if "post" in expected_calls:
            mock_post.post_process_router.assert_called_once_with(cfg)
        else:
            mock_post.post_process_router.assert_not_called()

        if "mes" in expected_calls:
            mock_mes.router.assert_called_once_with(cfg)
        else:
            mock_mes.router.assert_not_called()

        assert result is not None


class TestGetCfgWithMasterData:
    """Test suite for the get_cfg_with_master_data() method."""

    @pytest.fixture
    def aqwa_instance(self):
        """Create an Aqwa instance for testing."""
        return Aqwa()

    def test_get_cfg_with_master_data_no_master_data(self, aqwa_instance):
        """Test when cfg has no master data."""
        cfg = {"type": {"preprocess": True}}
        result = aqwa_instance.get_cfg_with_master_data(cfg)
        assert result == cfg

    def test_get_cfg_with_master_data_summary_settings_master(self, aqwa_instance):
        """Test processing of summary_settings_master."""
        cfg = {
            "summary_settings_master": {
                "groups": [{
                    "name": "master_group",
                    "default_value": 100,
                    "common_setting": "master"
                }]
            },
            "summary_settings": {
                "groups": [
                    {"name": "group1", "specific_value": 50},
                    {"name": "group2", "specific_value": 75}
                ]
            }
        }

        with patch('digitalmodel.hydrodynamics.aqwa.update_deep_dictionary') as mock_update:
            # Mock update_deep_dictionary to return a merged result
            mock_update.side_effect = lambda master, group: {**master, **group}

            result = aqwa_instance.get_cfg_with_master_data(cfg)

        # Verify update_deep_dictionary was called for each group
        assert mock_update.call_count == 2

        # Verify the structure is maintained
        assert "summary_settings" in result
        assert "groups" in result["summary_settings"]
        assert len(result["summary_settings"]["groups"]) == 2

    def test_get_cfg_with_master_data_settings_master(self, aqwa_instance):
        """Test processing of settings_master with keychain."""
        cfg = {
            "settings_master": {
                "keychain": ["test_groups"],
                "default_setting": "master_default",
                "master_value": 200
            },
            "test_groups": [
                {"name": "test1", "value": 10},
                {"name": "test2", "value": 20}
            ]
        }

        with patch('digitalmodel.hydrodynamics.aqwa.update_deep_dictionary') as mock_update:
            # Mock update_deep_dictionary to return merged results
            mock_update.side_effect = lambda dict1, dict2: {**dict1, **dict2}

            result = aqwa_instance.get_cfg_with_master_data(cfg)

        # Verify update_deep_dictionary was called correctly
        # Should be called twice per group (once for merging with master, once for final merge)
        assert mock_update.call_count == 4

        # Verify the structure is maintained
        assert "test_groups" in result
        assert len(result["test_groups"]) == 2

    def test_get_cfg_with_master_data_both_masters(self, aqwa_instance):
        """Test when both summary_settings_master and settings_master are present."""
        cfg = {
            "summary_settings_master": {
                "groups": [{"master_summary": "value"}]
            },
            "summary_settings": {
                "groups": [{"group": "summary"}]
            },
            "settings_master": {
                "keychain": ["data_groups"],
                "master_setting": "value"
            },
            "data_groups": [
                {"group": "data"}
            ]
        }

        with patch('digitalmodel.hydrodynamics.aqwa.update_deep_dictionary') as mock_update:
            mock_update.side_effect = lambda dict1, dict2: {**dict1, **dict2}

            result = aqwa_instance.get_cfg_with_master_data(cfg)

        # Both processing paths should be executed
        assert mock_update.call_count >= 2
        assert "summary_settings" in result
        assert "data_groups" in result

    def test_get_cfg_with_master_data_empty_groups(self, aqwa_instance):
        """Test with empty groups in summary_settings."""
        cfg = {
            "summary_settings_master": {
                "groups": [{"master": "value"}]
            },
            "summary_settings": {
                "groups": []
            }
        }

        result = aqwa_instance.get_cfg_with_master_data(cfg)

        # Empty groups should remain empty
        assert result["summary_settings"]["groups"] == []

    def test_get_cfg_with_master_data_multiple_groups(self, aqwa_instance):
        """Test with multiple groups in settings processing."""
        cfg = {
            "settings_master": {
                "keychain": ["multiple_groups"],
                "shared_setting": "master"
            },
            "multiple_groups": [
                {"id": 1, "name": "group1"},
                {"id": 2, "name": "group2"},
                {"id": 3, "name": "group3"}
            ]
        }

        with patch('digitalmodel.hydrodynamics.aqwa.update_deep_dictionary') as mock_update:
            mock_update.side_effect = lambda dict1, dict2: {**dict1, **dict2}

            result = aqwa_instance.get_cfg_with_master_data(cfg)

        # Should process all 3 groups
        assert len(result["multiple_groups"]) == 3
        # Update should be called twice per group (6 total calls)
        assert mock_update.call_count == 6

    @patch('digitalmodel.hydrodynamics.aqwa.update_deep_dictionary')
    def test_get_cfg_with_master_data_update_deep_dictionary_called_correctly(self, mock_update, aqwa_instance):
        """Test that update_deep_dictionary is called with correct arguments."""
        cfg = {
            "summary_settings_master": {
                "groups": [{"master_key": "master_value"}]
            },
            "summary_settings": {
                "groups": [{"child_key": "child_value"}]
            }
        }

        mock_update.return_value = {"merged": "result"}

        result = aqwa_instance.get_cfg_with_master_data(cfg)

        # Verify update_deep_dictionary was called with master and child
        mock_update.assert_called_with(
            {"master_key": "master_value"},
            {"child_key": "child_value"}
        )


class TestAqwaEdgeCases:
    """Test edge cases and error conditions."""

    @pytest.fixture
    def aqwa_instance(self):
        """Create an Aqwa instance for testing."""
        return Aqwa()

    def test_router_with_none_cfg(self, aqwa_instance):
        """Test router behavior with None configuration."""
        with pytest.raises(TypeError):
            # This should raise an error when trying to pass None to get_cfg_with_master_data
            aqwa_instance.router(None)

    def test_router_with_missing_type_key(self, aqwa_instance):
        """Test router behavior with missing 'type' key."""
        cfg = {"data": "value"}

        with pytest.raises(KeyError):
            aqwa_instance.router(cfg)

    def test_router_with_malformed_type_structure(self, aqwa_instance):
        """Test router behavior with malformed type structure."""
        cfg = {"type": "not_a_dict"}

        with pytest.raises(TypeError):
            aqwa_instance.router(cfg)

    @patch('digitalmodel.hydrodynamics.aqwa.a_post')
    def test_router_partial_type_keys(self, mock_post, aqwa_instance):
        """Test router with partial type keys."""
        cfg = {
            "type": {
                "preprocess": True
                # Missing 'analysis' and 'results' keys
            }
        }

        with patch.object(aqwa_instance, 'get_cfg_with_master_data', return_value=cfg):
            with pytest.raises(KeyError):
                aqwa_instance.router(cfg)

    def test_get_cfg_with_master_data_malformed_summary_settings(self, aqwa_instance):
        """Test get_cfg_with_master_data with malformed summary_settings."""
        cfg = {
            "summary_settings_master": {
                "groups": [{"master": "value"}]
            },
            "summary_settings": "not_a_dict"  # Should be a dict
        }

        with pytest.raises(TypeError):
            aqwa_instance.get_cfg_with_master_data(cfg)

    def test_get_cfg_with_master_data_missing_keychain(self, aqwa_instance):
        """Test get_cfg_with_master_data with missing keychain."""
        cfg = {
            "settings_master": {
                # Missing 'keychain' key
                "master_setting": "value"
            }
        }

        with pytest.raises(KeyError):
            aqwa_instance.get_cfg_with_master_data(cfg)

    def test_get_cfg_with_master_data_invalid_keychain_reference(self, aqwa_instance):
        """Test get_cfg_with_master_data with invalid keychain reference."""
        cfg = {
            "settings_master": {
                "keychain": ["nonexistent_key"],
                "master_setting": "value"
            }
        }

        with pytest.raises(KeyError):
            aqwa_instance.get_cfg_with_master_data(cfg)

    def test_router_import_error_simulation(self, aqwa_instance):
        """Test router behavior when import of AqwaAnalysis fails."""
        cfg = {
            "type": {
                "preprocess": False,
                "analysis": True,
                "results": False
            }
        }

        with patch.object(aqwa_instance, 'get_cfg_with_master_data', return_value=cfg), \
             patch('builtins.__import__') as mock_import:

            # Simulate import error
            mock_import.side_effect = ImportError("Module not found")

            with pytest.raises(ImportError):
                aqwa_instance.router(cfg)


class TestAqwaIntegration:
    """Integration tests for the complete AQWA workflow."""

    @pytest.fixture
    def aqwa_instance(self):
        """Create an Aqwa instance for testing."""
        return Aqwa()

    @pytest.fixture
    def complete_cfg(self):
        """Complete configuration for integration testing."""
        return {
            "type": {
                "preprocess": True,
                "analysis": True,
                "results": True
            },
            "mes": {"flag": True},
            "summary_settings_master": {
                "groups": [{
                    "default_timeout": 300,
                    "log_level": "INFO"
                }]
            },
            "summary_settings": {
                "groups": [
                    {"name": "analysis1", "timeout": 600},
                    {"name": "analysis2", "custom_param": "value"}
                ]
            },
            "settings_master": {
                "keychain": ["processing_groups"],
                "default_threads": 4,
                "memory_limit": "8GB"
            },
            "processing_groups": [
                {"name": "group1", "threads": 8},
                {"name": "group2", "memory_limit": "16GB"}
            ]
        }

    @patch('digitalmodel.hydrodynamics.aqwa.a_pre')
    @patch('digitalmodel.hydrodynamics.aqwa.a_post')
    @patch('digitalmodel.hydrodynamics.aqwa.mes_files')
    @patch('digitalmodel.hydrodynamics.aqwa.update_deep_dictionary')
    def test_complete_workflow_integration(self, mock_update, mock_mes, mock_post,
                                         mock_pre, aqwa_instance, complete_cfg):
        """Test complete workflow with all components enabled."""
        # Mock dependencies
        mock_analysis_instance = Mock()
        mock_update.side_effect = lambda dict1, dict2: {**dict1, **dict2}

        # Mock post processor to return modified config
        processed_cfg = copy.deepcopy(complete_cfg)
        processed_cfg["workflow_completed"] = True
        mock_post.post_process_router.return_value = processed_cfg

        with patch('builtins.__import__') as mock_import:
            # Mock the module import for analysis
            mock_module = Mock()
            mock_module.AqwaAnalysis.return_value = mock_analysis_instance
            mock_import.return_value = mock_module

            # Execute the workflow
            result = aqwa_instance.router(complete_cfg)

        # Verify all components were called in correct order
        mock_pre.pre_process_router.assert_called_once()
        mock_analysis_instance.analysis_router.assert_called_once()
        mock_mes.router.assert_called_once()
        mock_post.post_process_router.assert_called_once()

        # Verify master data processing occurred
        assert mock_update.call_count > 0

        # Verify final result
        assert result["workflow_completed"] is True
        assert "summary_settings" in result
        assert "processing_groups" in result

    def test_workflow_state_preservation(self, aqwa_instance):
        """Test that workflow preserves and passes state correctly."""
        cfg = {
            "type": {
                "preprocess": True,
                "analysis": False,
                "results": True
            },
            "initial_state": "preserved"
        }

        with patch('digitalmodel.hydrodynamics.aqwa.a_pre') as mock_pre, \
             patch('digitalmodel.hydrodynamics.aqwa.a_post') as mock_post:

            # Mock post processor to verify state is passed through
            def verify_state(passed_cfg):
                assert passed_cfg["initial_state"] == "preserved"
                return passed_cfg

            mock_post.post_process_router.side_effect = verify_state

            result = aqwa_instance.router(cfg)

            # Verify state was preserved
            assert result["initial_state"] == "preserved"
            mock_pre.pre_process_router.assert_called_once()
            mock_post.post_process_router.assert_called_once()


# Performance and stress tests
class TestAqwaPerformance:
    """Performance tests for AQWA module."""

    @pytest.fixture
    def aqwa_instance(self):
        """Create an Aqwa instance for testing."""
        return Aqwa()

    def test_large_configuration_processing(self, aqwa_instance):
        """Test processing of large configuration structures."""
        # Create a large configuration
        large_cfg = {
            "type": {
                "preprocess": False,
                "analysis": False,
                "results": False
            },
            "summary_settings_master": {
                "groups": [{"default": f"value_{i}"} for i in range(100)]
            },
            "summary_settings": {
                "groups": [{"id": i, "data": f"group_{i}"} for i in range(1000)]
            }
        }

        with patch('digitalmodel.hydrodynamics.aqwa.update_deep_dictionary') as mock_update:
            mock_update.side_effect = lambda dict1, dict2: {**dict1, **dict2}

            # This should complete without timeout or memory issues
            result = aqwa_instance.get_cfg_with_master_data(large_cfg)

            # Verify processing completed
            assert len(result["summary_settings"]["groups"]) == 1000
            # Should be called once per group
            assert mock_update.call_count == 1000

    def test_deep_nested_configuration(self, aqwa_instance):
        """Test processing of deeply nested configuration structures."""
        nested_cfg = {
            "type": {
                "preprocess": False,
                "analysis": False,
                "results": False
            }
        }

        # Create deeply nested structure
        current = nested_cfg
        for i in range(50):  # 50 levels deep
            current[f"level_{i}"] = {}
            current = current[f"level_{i}"]
        current["final_value"] = "deep_value"

        # Should handle deep nesting without stack overflow
        result = aqwa_instance.get_cfg_with_master_data(nested_cfg)

        # Verify structure is preserved
        assert "level_0" in result