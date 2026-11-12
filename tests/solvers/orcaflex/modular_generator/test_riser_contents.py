"""Riser contents pressure reaches the emitted line; non-zero flow rate is refused (#3983).

``RiserContents.pressure`` and ``flow_rate`` were validated and then dropped:
only ``ContentsDensity`` was emitted, so a pressurised riser was generated at
zero internal pressure.  The native data names are those of the public example
"A01 Lazy wave riser" (``ContentsPressure``, ``ContentsFlowRate``).
"""

from __future__ import annotations

import copy
from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
from digitalmodel.solvers.orcaflex.modular_generator.builders.riser_lines_builder import (
    RiserLinesBuilder,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

REPO = Path(__file__).resolve().parents[4]
TEMPLATE = REPO / "docs/domains/orcaflex/library/templates/riser_catenary/spec.yml"
NATIVE = REPO / (
    "docs/domains/orcaflex/examples/modular/A01/A01 Lazy wave riser/includes/07_lines.yml"
)


def _line(pressure: float, flow_rate: float = 0.0) -> dict:
    data = yaml.safe_load(TEMPLATE.read_text(encoding="utf-8"))
    data = copy.deepcopy(data)
    data["riser"]["lines"][0]["contents"] = {
        "density": 0.8, "pressure": pressure, "flow_rate": flow_rate,
    }
    spec = ProjectInputSpec(**data)
    return RiserLinesBuilder(spec, BuilderContext()).build()["Lines"][0]


def test_contents_pressure_is_emitted_in_kpa():
    line = _line(pressure=15000.0)
    assert line["ContentsDensity"] == 0.8
    assert line["ContentsPressure"] == 15000.0


def test_zero_pressure_is_emitted_explicitly():
    assert _line(pressure=0.0)["ContentsPressure"] == 0.0


def test_contents_data_names_match_native_example():
    native = yaml.safe_load(NATIVE.read_text(encoding="utf-8"))["Lines"][0]
    line = _line(pressure=100.0)
    contents_keys = {k for k in line if k.startswith("Contents")}
    assert contents_keys <= set(native)


def test_nonzero_flow_rate_fails_closed():
    """The schema states m/s; the OrcaFlex flow-rate unit is not established
    offline, so a non-zero value is refused rather than emitted or dropped."""
    with pytest.raises(ValueError, match="flow_rate"):
        _line(pressure=100.0, flow_rate=1.5)


def test_pressure_reference_height_is_not_emitted():
    """The reference height stays at the OrcaFlex default; the generator does
    not invent one. Which height the default denotes needs a licensed load."""
    line = _line(pressure=15000.0)
    assert "ContentsPressureRefZ" not in line


def test_zero_flow_rate_is_accepted_and_not_emitted():
    line = _line(pressure=100.0, flow_rate=0.0)
    assert "ContentsFlowRate" not in line
