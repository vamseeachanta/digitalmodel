"""Fracture entry-point control flow; no engineering solver is imported.

Issue 2598 requires an explicit warning or status for a disabled check:
https://github.com/vamseeachanta/digitalmodel/issues/2598
The component double tests dispatch only, not fracture-mechanics calculations.
"""

import sys
import warnings
from copy import deepcopy
from types import ModuleType
from unittest.mock import MagicMock, call

import pytest

from digitalmodel.asset_integrity.fracture_mechanics import fracture_mechanics


@pytest.fixture
def components(monkeypatch):
    """Replace the component import before either branch can load dependencies."""
    name = "digitalmodel.asset_integrity.common.fracture_mechanics_components"
    module = ModuleType(name)
    factory = MagicMock()
    module.FractureMechanicsComponents = factory
    monkeypatch.setitem(sys.modules, name, module)
    return factory


def test_disabled_check_warns_without_initializing_components(components):
    """A disabled check gives NOT_EVALUATED and returns the original config."""
    cfg = {"default": {"Analysis": {"fracture_mechanics": {"flaw_limits": False}}}}
    original = deepcopy(cfg)

    with pytest.warns(RuntimeWarning, match="NOT_EVALUATED.*flaw_limits.*disabled"):
        result = fracture_mechanics(cfg)

    assert result is cfg
    assert cfg["default"] == original["default"]
    status = cfg["Result"]["assessment_status"]["flaw_limits"]
    assert status["status"] == "NOT_EVALUATED"
    assert status["reason"] == "flaw_limits is disabled"
    components.assert_not_called()


def test_disabled_check_does_not_import_components(monkeypatch):
    """Disabled analysis needs no optional calculation or plotting dependencies."""
    name = "digitalmodel.asset_integrity.common.fracture_mechanics_components"
    monkeypatch.setitem(sys.modules, name, None)
    cfg = {"default": {"Analysis": {"fracture_mechanics": {"flaw_limits": False}}}}

    with pytest.warns(RuntimeWarning, match="NOT_EVALUATED"):
        assert fracture_mechanics(cfg) is cfg


def test_enabled_check_preserves_component_order(components):
    """The enabled branch retains the existing six operations and cfg identity."""
    cfg = {"default": {"Analysis": {"fracture_mechanics": {"flaw_limits": True}}}}

    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        result = fracture_mechanics(cfg)

    assert caught == []
    assert result is cfg
    components.assert_called_once_with(cfg)
    assert components.return_value.mock_calls == [
        call.evaluate_FAD(),
        call.critical_flaw_limits(),
        call.get_flaw_growth_for_fatigue_loading(),
        call.get_initial_allowable_flaw_for_life(),
        call.save_result_tables(),
        call.create_visualizations(),
    ]


def test_enabled_check_does_not_hide_component_failure(components):
    """A failed evaluation cannot return the configuration as a completed check."""
    cfg = {"default": {"Analysis": {"fracture_mechanics": {"flaw_limits": True}}}}
    components.return_value.evaluate_FAD.side_effect = RuntimeError("evaluation failed")

    with pytest.raises(RuntimeError, match="evaluation failed"):
        fracture_mechanics(cfg)

    components.return_value.save_result_tables.assert_not_called()
