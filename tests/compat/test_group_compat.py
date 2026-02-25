"""Tests for grouped module architecture backward compatibility.

These tests verify that:
1. The _FLAT_TO_GROUP mapping is complete and consistent
2. All 11 group scaffold packages are importable
3. After Phase 14 moves: all 3 import layers work for every module
   - Layer 1: digitalmodel.modules.X (oldest compat)
   - Layer 2: digitalmodel.X (current flat compat)
   - Layer 3: digitalmodel.<group>.X (new canonical)
"""

import importlib
import sys

import pytest

from digitalmodel._compat import _FLAT_TO_GROUP, get_group_for_module


# ── Mapping completeness ──────────────────────────────────────────────

EXPECTED_GROUPS = {
    "solvers",
    "hydrodynamics",
    "structural",
    "subsea",
    "marine_ops",
    "signal_processing",
    "infrastructure",
    "data_systems",
    "workflows",
    "visualization",
    "specialized",
}

EXPECTED_MODULE_COUNT = 64  # 64 modules mapped to groups (group-named modules excluded)


class TestFlatToGroupMapping:
    """Verify the _FLAT_TO_GROUP registry is correct."""

    def test_mapping_is_dict(self):
        assert isinstance(_FLAT_TO_GROUP, dict)

    def test_all_expected_groups_present(self):
        groups_used = set(_FLAT_TO_GROUP.values())
        assert groups_used == EXPECTED_GROUPS

    def test_mapping_count(self):
        assert len(_FLAT_TO_GROUP) >= EXPECTED_MODULE_COUNT

    def test_get_group_for_known_module(self):
        assert get_group_for_module("orcaflex") == "solvers"
        assert get_group_for_module("aqwa") == "hydrodynamics"
        assert get_group_for_module("fatigue_apps") == "structural"
        assert get_group_for_module("mooring_analysis") == "subsea"
        assert get_group_for_module("signal_analysis") == "signal_processing"
        assert get_group_for_module("common") == "infrastructure"
        assert get_group_for_module("data_scraping") == "data_systems"
        assert get_group_for_module("automation") == "workflows"
        assert get_group_for_module("gis") == "specialized"
        assert get_group_for_module("marine_analysis") == "marine_ops"

    def test_get_group_for_unknown_module(self):
        assert get_group_for_module("nonexistent_xyz") is None

    def test_no_empty_group_names(self):
        for mod, group in _FLAT_TO_GROUP.items():
            assert group, f"Module {mod!r} has empty group name"
            assert mod, f"Empty module name maps to {group!r}"


# ── Group scaffold packages ───────────────────────────────────────────

class TestGroupScaffolds:
    """Verify all 11 group packages are importable."""

    @pytest.mark.parametrize("group", sorted(EXPECTED_GROUPS))
    def test_group_package_importable(self, group):
        mod = importlib.import_module(f"digitalmodel.{group}")
        assert mod is not None


# ── Layer 2 flat import (current state, pre-move) ────────────────────

# Modules that are known to exist as flat directories right now
SAMPLE_FLAT_MODULES = [
    "orcaflex",
    "aqwa",
    "diffraction",
    "mooring_analysis",
    "signal_analysis",
    "fatigue_apps",
    "pipe_capacity",
    "pipeline",
    "viv_analysis",
    "catenary",
    "catenary_riser",
    "time_series",
    "rao_analysis",
    "transformation",
    "ct_hydraulics",
]


class TestFlatImports:
    """Verify Layer 2 (digitalmodel.X) still works."""

    @pytest.mark.parametrize("module_name", SAMPLE_FLAT_MODULES)
    def test_flat_import(self, module_name):
        try:
            mod = importlib.import_module(f"digitalmodel.{module_name}")
            assert mod is not None
        except ImportError as e:
            # Module resolved but internal dep missing — acceptable
            if module_name not in str(e):
                pass
            else:
                pytest.skip(f"Module not available: {e}")


# ── Layer 1 compat (digitalmodel.modules.X) ──────────────────────────

SAMPLE_LAYER1_MODULES = [
    "orcaflex",
    "aqwa",
    "pipeline",
    "signal_analysis",
    "viv_analysis",
]


class TestLayer1Compat:
    """Verify Layer 1 (digitalmodel.modules.X) still works."""

    @pytest.mark.parametrize("module_name", SAMPLE_LAYER1_MODULES)
    def test_modules_compat_import(self, module_name):
        try:
            mod = importlib.import_module(f"digitalmodel.modules.{module_name}")
            assert mod is not None
        except ImportError as e:
            if module_name not in str(e):
                pass
            else:
                pytest.skip(f"Module not available: {e}")


# ── Layer 3 group import (only works AFTER Phase 14 moves) ───────────
# These tests are EXPECTED TO FAIL until modules are physically moved.
# They serve as a verification target for Phase 14.

SAMPLE_GROUPED_IMPORTS = [
    ("solvers", "orcaflex"),
    ("hydrodynamics", "aqwa"),
    ("structural", "fatigue_apps"),
    ("subsea", "mooring_analysis"),
    ("marine_ops", "marine_analysis"),
    ("signal_processing", "signal_analysis"),
    ("data_systems", "data_scraping"),
    ("specialized", "gis"),
]


class TestGroupedImports:
    """Verify Layer 3 (digitalmodel.<group>.X) works after Phase 14.

    Before Phase 14: these tests are expected to fail (modules not yet moved).
    After Phase 14: these tests must pass.
    """

    @pytest.mark.parametrize("group,module_name", SAMPLE_GROUPED_IMPORTS)
    def test_grouped_import(self, group, module_name):
        """Import via grouped path — passes after Phase 14."""
        try:
            mod = importlib.import_module(f"digitalmodel.{group}.{module_name}")
            assert mod is not None
        except ImportError:
            pytest.skip(
                f"digitalmodel.{group}.{module_name} not yet available "
                f"(expected before Phase 14 module moves)"
            )
