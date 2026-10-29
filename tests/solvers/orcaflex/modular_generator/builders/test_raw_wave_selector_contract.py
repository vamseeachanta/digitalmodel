# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Acceptance coverage for raw wave selector ordering (issue #2513).

Sources: Orcina public help, online edition accessed 2026-10-22:
* Text data files, Ordering issues:
  https://www.orcina.com/webhelp/OrcaFlex/Content/html/Textdatafiles.htm
* Environment: Data for JONSWAP and ISSC spectra, Setting the data:
  https://www.orcina.com/webhelp/OrcaFlex/Content/html/Environment%2CDataforJONSWAPandISSCspectra.htm

All numeric values are synthetic input tokens. Independent hand calculation:
output height = typed height = 2; output period = typed period = 8; extra raw
height = 4 and period = 9 are unchanged. There is no spectral conversion.
Precedence is categorical: WaveType before every Wave property, then JONSWAP
mode before gamma, and gamma before Tp. Zero inverted pairs is exact acceptance
of this ordering relation, not a numerical engineering tolerance.
"""

from copy import deepcopy
from pathlib import Path
import runpy

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
from digitalmodel.solvers.orcaflex.modular_generator.builders.environment_builder import (
    EnvironmentBuilder,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec


def _spec(raw_trains, wave_type="airy"):
    return ProjectInputSpec.model_validate({
        "metadata": {
            "name": "raw selector contract", "description": "synthetic inputs",
            "structure": "generic", "operation": "in-place",
        },
        "environment": {
            # Schema-only positive depth and zero stiffness are synthetic.
            "water": {"depth": 1},
            "seabed": {"stiffness": {"normal": 0, "shear": 0}},
            "waves": {"type": wave_type, "height": 2, "period": 8, "gamma": 2},
            "raw_properties": {"WaveTrains": raw_trains},
        },
        "generic": {},
    })


def _trains(spec):
    return EnvironmentBuilder(spec, BuilderContext()).build()["Environment"]["WaveTrains"]


def _assert_selector_order(train):
    """Read in assignment order; a dependent property needs an active selector."""
    seen = set()
    for key in train:
        if key.startswith("Wave") and key != "WaveType":
            assert "WaveType" in seen, f"{key} assigned before WaveType"
        if train["WaveType"] == "JONSWAP":
            if key == "WaveGamma":
                assert "WaveJONSWAPParameters" in seen
                assert train["WaveJONSWAPParameters"] != "Automatic"
            if key == "WaveTp" and "WaveGamma" in train:
                assert "WaveGamma" in seen
        seen.add(key)


@pytest.mark.parametrize("wave_type,native_type,height,period", [
    ("airy", "Airy", "WaveHeight", "WavePeriod"),
    ("dean_stream", "Dean stream", "WaveHeight", "WavePeriod"),
    ("jonswap", "JONSWAP", "WaveHs", "WaveTp"),
    ("pierson_moskowitz", "Pierson-Moskowitz", "WaveHs", "WaveTp"),
])
def test_reversed_raw_keys_survive_generation_with_no_order_violations(
    tmp_path, wave_type, native_type, height, period,
):
    """Typed output is (2, 8); extra raw output is (4, 9), by identity.

    The raw entry puts both dependents before its selector. Name and origin
    must survive; relative order of the non-selector raw fields must survive.
    """
    raw = {
        height: 4, period: 9, "WaveOrigin": [2, 4],
        "WaveType": native_type, "Name": "matched",
    }
    extra = {**raw, "Name": "extra"}
    spec = _spec([raw, extra], wave_type)
    before = spec.model_dump()
    ModularModelGenerator.from_spec(spec).generate(tmp_path)
    path = tmp_path / "includes" / "03_environment.yml"
    matched, extra_output = yaml.safe_load(path.read_text())["Environment"]["WaveTrains"]
    for output in (matched, extra_output):
        _assert_selector_order(output)
        assert output["WaveOrigin"] == [2, 4]
        retained = [key for key in output if key in (height, period, "WaveOrigin")]
        assert retained == [height, period, "WaveOrigin"]
    assert matched[height] == 2
    assert matched[period] == 8
    assert extra_output[height] == 4
    assert extra_output[period] == 9
    assert [matched["Name"], extra_output["Name"]] == ["matched", "extra"]
    assert spec.model_dump() == before
    assert list(spec.environment.raw_properties["WaveTrains"][0]) == list(raw)


@pytest.mark.parametrize("extra_raw", [False, True], ids=["matched", "extra"])
def test_raw_mapping_edits_rebuild_selectors_and_keep_source_order(extra_raw):
    """An in-place edit must be reflected on the next build without stale state.

    Hand calculation: Automatic omits gamma; partially specified copies gamma
    = 2 and Tp = 8. Reversing input order changes neither value nor precedence.
    """
    raw = {
        "WaveTp": 8, "WaveGamma": 2, "WaveHs": 2,
        "WaveJONSWAPParameters": "Partially specified", "WaveType": "JONSWAP",
    }
    spec = _spec([{}, raw] if extra_raw else [raw], "jonswap")
    source = spec.environment.raw_properties["WaveTrains"][-1]
    first = _trains(spec)[-1]
    first_items = list(first.items())
    # Mutate the actual stored mapping, moving the selector behind dependents.
    reversed_items = list(reversed(list(source.items())))
    source.clear()
    source.update(reversed_items)
    source["WaveType"] = source.pop("WaveType")
    source["WaveJONSWAPParameters"] = "Automatic"
    # Automatic takes Tz, so the input edit changes the period key too.
    source["WaveTz"] = source.pop("WaveTp")
    before = deepcopy(list(source.items()))
    automatic = _trains(spec)[-1]
    _assert_selector_order(automatic)
    assert "WaveGamma" not in automatic
    assert automatic["WaveTz"] == 8
    assert list(source.items()) == before
    assert list(first.items()) == first_items
    source["WaveJONSWAPParameters"] = "Partially specified"
    source["WaveTp"] = source.pop("WaveTz")
    partial = _trains(spec)[-1]
    _assert_selector_order(partial)
    assert partial["WaveGamma"] == 2
    assert partial["WaveTp"] == 8
    assert list(_trains(spec)[-1].items()) == list(partial.items())


@pytest.mark.parametrize("initial,edited,native,height,period", [
    ("airy", "jonswap", "JONSWAP", "WaveHs", "WaveTz"),
    ("jonswap", "airy", "Airy", "WaveHeight", "WavePeriod"),
])
def test_typed_type_edit_reorders_common_raw_fields(initial, edited, native, height, period):
    """Typed edits are authoritative; shared raw values remain unchanged.

    This intentionally uses shared fields, not dormant data from another theory.
    Hand calculation: height = 2 and period = 8 after either selector edit.
    """
    spec = _spec([{"WaveDirection": 4, "WaveType": "Airy", "WaveTimeOrigin": 2}], initial)
    _trains(spec)
    spec.environment.waves.trains[0].type = edited
    before = spec.model_dump()
    train, = _trains(spec)
    _assert_selector_order(train)
    assert train["WaveType"] == native
    assert train[height] == 2
    assert train[period] == 8
    assert train["WaveTimeOrigin"] == 2
    assert spec.model_dump() == before


def test_worked_example_runs_and_matches_hand_derived_answer(capsys):
    """Two trains copy H = 2 and T = 8; WaveType precedes both dependents."""
    root = Path(__file__).resolve().parents[5]
    runpy.run_path(
        str(root / "docs/domains/orcaflex/examples/wave_selector_ordering.py"),
        run_name="__main__",
    )
    data = yaml.safe_load(capsys.readouterr().out)
    matched, extra = data["Environment"]["WaveTrains"]
    for train in (matched, extra):
        assert list(train) == ["Name", "WaveType", "WaveHeight", "WavePeriod", "WaveDirection"]
        assert train["WaveHeight"] == 2
        assert train["WavePeriod"] == 8
        _assert_selector_order(train)
