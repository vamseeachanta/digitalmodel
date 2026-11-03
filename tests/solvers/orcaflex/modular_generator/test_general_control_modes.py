"""Offline control-selection regressions; no solver import or execution.

Source: Orcina, OrcaFlex online help, accessed 2026-10-19 (unversioned),
'General data: Dynamics' and 'General data: Implicit integration',
'Use variable time step' and 'Time step / maximum time step':
https://www.orcina.com/webhelp/OrcaFlex/Content/html/Generaldata,Dynamics.htm
https://www.orcina.com/webhelp/OrcaFlex/Content/html/Generaldata,Implicitintegration.htm

Expected key sets follow the documented mode selection. The dyadic timestep
inputs below are arbitrary round-trip probes, not engineering limits:
constant = 1/8 s; maximum = 3/4 s. Expected values are the supplied inputs
without arithmetic, conversion or replacement. No accuracy claim is made.
"""

from copy import deepcopy

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema import GenericModel

from .test_generic_builder import _make_builder


@pytest.mark.parametrize("mode", [
    True, "Yes", False, "No", pytest.param(None, id="absent-mode"),
])
@pytest.mark.parametrize("method", [
    "Implicit time domain", "Explicit time domain", "Frequency domain",
    pytest.param(None, id="absent-method"),
])
def test_general_controls_follow_source_mode(method, mode):
    """Mode -> active control, with scrambled input order and no mutation."""
    properties = {
        "ImplicitVariableMaxTimeStep": 0.75,  # Probe: 3/4 s, see module basis.
        "ImplicitConstantTimeStep": 0.125,  # Probe: 1/8 s, see module basis.
        "StageDuration": [1],  # Arbitrary pass-through duration, 1 s.
    }
    if mode is not None:
        properties["ImplicitUseVariableTimeStep"] = mode
    if method is not None:
        properties["DynamicsSolutionMethod"] = method
    original = deepcopy(properties)
    model = GenericModel(general_properties=properties)
    builder, _ = _make_builder(model)
    general = builder.build()["General"]
    expected = {"StageDuration": [1]}
    if method is not None:
        expected["DynamicsSolutionMethod"] = method
    if method in (None, "Implicit time domain"):
        if mode is not None:
            expected["ImplicitUseVariableTimeStep"] = mode
        active = (
            "ImplicitVariableMaxTimeStep" if mode in (True, "Yes")
            else "ImplicitConstantTimeStep"
        )
        expected[active] = original[active]
        keys = list(general)
        controls = [key for key in (
            "DynamicsSolutionMethod", "ImplicitUseVariableTimeStep", active,
        ) if key in expected]
        assert [key for key in keys if key in controls] == controls
    assert general == expected
    assert model.general_properties == original


@pytest.mark.parametrize("mode", ["yes", "no", 1, 0, 1.0, 0.0, None, [], {}])
@pytest.mark.parametrize("method", [
    "Implicit time domain", "Explicit time domain", "Frequency domain",
    pytest.param(None, id="absent-method"),
])
def test_unrecognised_variable_mode_fails_visibly(tmp_path, method, mode):
    """Reject unsupported values rather than silently discarding a cap."""
    properties = {
        "ImplicitVariableMaxTimeStep": 0.75,  # Probe: 3/4 s.
        "ImplicitConstantTimeStep": 0.125,  # Probe: 1/8 s.
        "ImplicitUseVariableTimeStep": mode,
    }
    if method is not None:
        properties["DynamicsSolutionMethod"] = method
    original = deepcopy(properties)
    model = GenericModel(general_properties=properties)
    builder, _ = _make_builder(model)
    with pytest.raises(ValueError, match="ImplicitUseVariableTimeStep"):
        builder.build()
    with pytest.raises(ValueError, match="ImplicitUseVariableTimeStep"):
        ModularModelGenerator.from_spec(builder.spec).generate(tmp_path)
    assert model.general_properties == original
    assert list(tmp_path.iterdir()) == []


@pytest.mark.parametrize("mode", [True, "Yes"])
def test_generated_includes_preserve_active_maximum(tmp_path, mode):
    """The late generic include must preserve the cap in the saved YAML."""
    model = GenericModel(general_properties={
        "ImplicitVariableMaxTimeStep": 0.75,  # Probe: 3/4 s.
        "ImplicitConstantTimeStep": 0.125,  # Inactive probe: 1/8 s.
        "ImplicitUseVariableTimeStep": mode,
        "DynamicsSolutionMethod": "Implicit time domain",
    })
    builder, _ = _make_builder(model)
    ModularModelGenerator.from_spec(builder.spec).generate(tmp_path)
    general = yaml.safe_load(
        (tmp_path / "includes/20_generic_objects.yml").read_text()
    )["General"]
    assert general["ImplicitVariableMaxTimeStep"] == 0.75
    assert "ImplicitConstantTimeStep" not in general


@pytest.mark.parametrize("method", ["Explicit time domain", "Frequency domain"])
def test_generated_nonimplicit_override_omits_implicit_controls(tmp_path, method):
    """The late include cannot write implicit controls after changing method."""
    model = GenericModel(general_properties={
        "ImplicitConstantTimeStep": 0.125,  # Inactive probe: 1/8 s.
        "ImplicitVariableMaxTimeStep": 0.75,  # Inactive probe: 3/4 s.
        "ImplicitUseVariableTimeStep": "Yes",
        "DynamicsSolutionMethod": method,
    })
    builder, _ = _make_builder(model)
    ModularModelGenerator.from_spec(builder.spec).generate(tmp_path)
    general = yaml.safe_load(
        (tmp_path / "includes/20_generic_objects.yml").read_text()
    )["General"]
    assert general == {"DynamicsSolutionMethod": method}
