# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Wave setter order and mode regressions without a solver.

Sources: Orcina OrcaFlex public help, accessed 2026-10-19 (online edition):
* Text data files, Ordering issues:
  https://www.orcina.com/webhelp/OrcaFlex/Content/html/Textdatafiles.htm
* Environment: Data for JONSWAP and ISSC spectra, spectral parameters:
  https://www.orcina.com/webhelp/OrcaFlex/Content/html/Environment%2CDataforJONSWAPandISSCspectra.htm

The help requires active data and gamma before Tp for partially specified
JONSWAP. All numbers below are synthetic input sentinels, not engineering
constants or acceptance limits. Expected values follow the identity calculation
output height = input height, output period = input period, output gamma = input
gamma when editable. No spectral conversion or solver result is asserted.
"""

from copy import deepcopy
from itertools import permutations

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
from digitalmodel.solvers.orcaflex.modular_generator.builders.environment_builder import (
    EnvironmentBuilder,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec


# Synthetic sentinels; the independent identity calculation is documented above.
_HEIGHT = 2.0
_PERIOD = 8.0
_GAMMA = 2.0


def _spec(raw_trains, wave_type="jonswap"):
    return ProjectInputSpec.model_validate({
        "metadata": {
            "name": "wave ordering", "description": "synthetic ordering test",
            "structure": "generic", "operation": "in-place",
        },
        "environment": {
            # Synthetic positive depth and zero stiffness satisfy the schema;
            # they have no role in the ordering or identity assertions.
            "water": {"depth": 1},
            "seabed": {"stiffness": {"normal": 0, "shear": 0}},
            "waves": {
                "type": wave_type, "height": _HEIGHT,
                "period": _PERIOD, "gamma": _GAMMA,
            },
            "raw_properties": {"WaveTrains": raw_trains},
        },
        "generic": {},
    })


def _build(spec):
    return EnvironmentBuilder(spec, BuilderContext()).build()["Environment"]["WaveTrains"]


@pytest.mark.parametrize("extra_raw", [False, True], ids=["matched", "extra"])
@pytest.mark.parametrize("mode", ["Automatic", "Partially specified", "Fully specified"])
def test_jonswap_mode_controls_gamma_without_mutating_input(mode, extra_raw):
    """Automatic computes gamma; editable modes retain the input sentinel.

    Hand calculation: gamma = 2 for editable modes, absent for Automatic;
    Hs = 2 and Tz = 8 are unchanged input tokens, not a spectral calculation.
    This checks the existing overlay contract, including fully specified data;
    it does not establish native acceptance of Hs/Tz in fully specified mode.
    """
    raw = {
        "WaveTz": _PERIOD,
        "WaveGamma": _GAMMA,
        "WaveHs": _HEIGHT,
        "WaveJONSWAPParameters": mode,
        "WaveType": "JONSWAP",
        "Name": "raw wave",
    }
    spec = _spec([{}, raw] if extra_raw else [raw])
    before = spec.model_dump()
    trains = _build(spec)
    train = trains[-1]
    assert train["WaveJONSWAPParameters"] == mode
    assert train["WaveHs"] == _HEIGHT
    assert train["WaveTz"] == _PERIOD
    assert "WaveTp" not in train
    if mode == "Automatic":
        assert "WaveGamma" not in train
    else:
        assert train["WaveGamma"] == _GAMMA
        keys = list(train)
        assert keys.index("WaveType") < keys.index("WaveJONSWAPParameters")
        assert keys.index("WaveJONSWAPParameters") < keys.index("WaveGamma")
        assert keys.index("WaveGamma") < keys.index("WaveTz")
    assert spec.model_dump() == before
    assert _build(spec) == trains
    assert list(EnvironmentBuilder._order_wave_train(train)) == list(train)


def test_all_raw_key_permutations_preserve_peak_period_and_values():
    """Independently derive precedence: type < mode < gamma < Tp.

    Five distinct keys have 5! = 120 input orders. For every order, output
    Tp = input Tp = 8 and gamma = input gamma = 2; Hs = typed height = 2.
    Orcina's spectral-parameter note supplies precedence, not a numeric formula.
    """
    raw = {
        "WaveTp": _PERIOD,
        "WaveGamma": _GAMMA,
        "WaveJONSWAPParameters": "Partially specified",
        "WaveType": "JONSWAP",
        "Name": "raw wave",
    }
    for items in permutations(raw.items()):
        inverted = dict(items)
        before = deepcopy(inverted)
        trains = _build(_spec([inverted, inverted]))
        for train in trains:
            keys = list(train)
            assert keys.index("Name") < keys.index("WaveType")
            assert keys.index("WaveType") < keys.index("WaveJONSWAPParameters")
            assert keys.index("WaveJONSWAPParameters") < keys.index("WaveGamma")
            assert keys.index("WaveGamma") < keys.index("WaveTp")
            assert train["WaveTp"] == _PERIOD
            assert train["WaveGamma"] == _GAMMA
            assert "WaveTz" not in train
        assert trains[0]["WaveHs"] == _HEIGHT
        assert inverted == before


@pytest.mark.parametrize("wave_type,expected", [("airy", "Airy"), ("jonswap", "JONSWAP")])
def test_typed_wave_type_overrides_raw_selector_before_common_properties(wave_type, expected):
    """A type edit changes the selector value and retains common data order.

    The raw mapping contains only properties shared by both wave types. Dormant
    properties carried across a change of wave theory are outside this test.
    """
    raw = {
        "WaveDirection": 0,  # Synthetic direction sentinel, copied unchanged.
        "WaveType": "JONSWAP" if wave_type == "airy" else "Airy",
        "Name": "raw wave",
    }
    spec = _spec([raw], wave_type)
    before = spec.model_dump()
    train, = _build(spec)
    assert train["WaveType"] == expected
    keys = list(train)
    assert all(keys.index("WaveType") < keys.index(key)
               for key in train if key.startswith("Wave") and key != "WaveType")
    assert spec.model_dump() == before


def test_selector_edit_rebuilds_from_unmodified_raw_gamma():
    """Dropping dormant output gamma must not erase it from the source input."""
    spec = _spec([{
        "WaveGamma": _GAMMA,
        "WaveJONSWAPParameters": "Automatic",
        "WaveType": "JONSWAP",
    }])
    automatic, = _build(spec)
    assert "WaveGamma" not in automatic
    raw = spec.environment.raw_properties["WaveTrains"][0]
    raw["WaveJONSWAPParameters"] = "Partially specified"
    partial, = _build(spec)
    assert partial["WaveGamma"] == _GAMMA
    assert list(partial).index("WaveJONSWAPParameters") < list(partial).index("WaveGamma")


def test_generated_yaml_omits_automatic_gamma_and_retains_selector_order(tmp_path):
    """Check the serialized include, rather than only the builder dictionary."""
    spec = _spec([{
        "WaveGamma": _GAMMA,
        "WaveTz": _PERIOD,
        "WaveJONSWAPParameters": "Automatic",
        "WaveType": "JONSWAP",
    }])
    ModularModelGenerator.from_spec(spec).generate(tmp_path)
    environment_file = tmp_path / "includes" / "03_environment.yml"
    data = yaml.safe_load(environment_file.read_text())
    train, = data["Environment"]["WaveTrains"]
    assert "WaveGamma" not in train
    assert train["WaveTz"] == _PERIOD
    keys = list(train)
    assert keys.index("WaveType") < keys.index("WaveJONSWAPParameters")
    assert keys.index("WaveJONSWAPParameters") < keys.index("WaveTz")
