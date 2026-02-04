# ABOUTME: Tests for the design code strategy registry and dispatch mechanism
# ABOUTME: Verifies registration, protocol compliance, and backward compatibility

from digitalmodel.structural.analysis.wall_thickness import DesignCode
from digitalmodel.structural.analysis.wall_thickness_codes import CODE_REGISTRY
from digitalmodel.structural.analysis.wall_thickness_codes.base import CodeStrategy


class TestCodeRegistry:
    def test_registry_contains_dnv(self):
        assert DesignCode.DNV_ST_F101 in CODE_REGISTRY

    def test_registry_contains_api(self):
        assert DesignCode.API_RP_1111 in CODE_REGISTRY

    def test_dnv_strategy_has_required_attributes(self):
        cls = CODE_REGISTRY[DesignCode.DNV_ST_F101]
        instance = cls()
        assert hasattr(instance, 'code_name')
        assert hasattr(instance, 'check_names')
        assert hasattr(instance, 'run_checks')
        assert hasattr(instance, 'compute_plastic_moment')
        assert hasattr(instance, 'compute_plastic_tension')

    def test_api_strategy_has_required_attributes(self):
        cls = CODE_REGISTRY[DesignCode.API_RP_1111]
        instance = cls()
        assert hasattr(instance, 'code_name')
        assert hasattr(instance, 'check_names')
        assert hasattr(instance, 'run_checks')

    def test_dnv_check_names_match_expected(self):
        cls = CODE_REGISTRY[DesignCode.DNV_ST_F101]
        assert set(cls.check_names) == {
            "pressure_containment", "collapse", "propagation_buckling", "combined_loading"
        }

    def test_api_check_names_match_expected(self):
        cls = CODE_REGISTRY[DesignCode.API_RP_1111]
        assert set(cls.check_names) == {"burst", "collapse", "propagation"}
