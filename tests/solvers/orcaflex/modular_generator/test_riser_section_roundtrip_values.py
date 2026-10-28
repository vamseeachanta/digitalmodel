# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Synthetic serialization checks for the riser data models.

Unit source: src/digitalmodel/solvers/orcaflex/modular_generator/schema/riser.py,
RiserSection, EndConnection, RiserContents, BuoyancyZone and ClumpAttachments
field descriptions: lengths/positions in m, angles in degrees, end stiffness in
kN.m/rad, contents density in te/m3, pressure in kPa and flow rate in m/s.

All numbers below are authored synthetic test inputs, not design criteria or
standard-derived coefficients. Independent hand calculation of the section
lengths: 12.50 m + 27.25 m = 39.75 m; 39.75 m + 40.75 m = 80.50 m.
Quarter-metre inputs are exactly representable as binary floats, so equality
checks require no rounding allowance. Segment targets, connection coordinates,
rotations and optional values are distinct synthetic sentinels for field loss,
unit scaling or end swaps. These checks do not qualify any component.
"""

import json
from math import fsum

import pytest

from digitalmodel.solvers.orcaflex.modular_generator.schema.riser import (
    BuoyancyZone,
    ClumpAttachments,
    EndConnection,
    RiserContents,
    RiserLine,
    RiserSection,
)


@pytest.fixture(params=["mapping", "json"])
def serialization(request):
    """Exercise both existing model serialization interfaces."""
    return request.param


@pytest.fixture(params=[False, True], ids=["optional-absent", "optional-present"])
def authored_values(request):
    """Supply an explicit independent record of every authored field."""
    return {
        "name": "synthetic-line",
        "configuration": "free_hanging",
        "end_a": {
            "type": "buoy",
            "name": "synthetic-support-a",
            "position": [1.25, -2.5, 3.75],
            "azimuth": 37.5,
            "declination": 112.5,
            "gamma": -12.5,
            "bending_stiffness": 42.5,
        },
        "end_b": {
            "type": "fixed",
            "name": "synthetic-support-b" if request.param else None,
            "position": [-6.25, 7.5, -80.0],
            "azimuth": 217.5,
            "declination": 67.5,
            "gamma": 22.5,
            "bending_stiffness": 85.0,
        },
        "sections": [
            {"line_type": "synthetic-upper", "length": 12.5, "segment_length": 0.5},
            {"line_type": "synthetic-middle", "length": 27.25, "segment_length": 1.25},
            {"line_type": "synthetic-lower", "length": 40.75, "segment_length": 2.5},
        ],
        "contents": {"density": 0.75, "pressure": 125.0, "flow_rate": 1.5},
        "buoyancy_zone": {
            "start_arc_length": 12.5,
            "end_arc_length": 39.75,
            "line_type": "synthetic-middle",
        } if request.param else None,
        "clump_attachments": {
            "clump_type": "synthetic-clump",
            "start_arc_length": 15.0,
            "end_arc_length": 35.0,
            "spacing": 5.0,
        } if request.param else None,
        "lay_azimuth": 157.5,
        "statics_method": "Catenary",
        "statics_step2": "Full statics" if request.param else None,
        "include_torsion": request.param,
    }


@pytest.fixture
def authored_line(authored_values):
    """Construct section and connection models directly, without builders."""
    values = authored_values
    return RiserLine(
        name=values["name"],
        configuration=values["configuration"],
        end_a=EndConnection(**values["end_a"]),
        end_b=EndConnection(**values["end_b"]),
        sections=[RiserSection(**section) for section in values["sections"]],
        contents=RiserContents(**values["contents"]),
        buoyancy_zone=(
            BuoyancyZone(**values["buoyancy_zone"])
            if values["buoyancy_zone"] is not None else None
        ),
        clump_attachments=(
            ClumpAttachments(**values["clump_attachments"])
            if values["clump_attachments"] is not None else None
        ),
        lay_azimuth=values["lay_azimuth"],
        statics_method=values["statics_method"],
        statics_step2=values["statics_step2"],
        include_torsion=values["include_torsion"],
    )


def _round_trip(model, serialization):
    """Return the serialized record and the restored model."""
    if serialization == "json":
        payload = model.model_dump_json()
        return json.loads(payload), type(model).model_validate_json(payload)
    payload = model.model_dump()
    return payload, type(model).model_validate(payload)


def test_riser_sections_round_trip_physical_fields(
    authored_line, authored_values, serialization,
):
    """Preserve every authored value, including section order and metre units."""
    payload, restored = _round_trip(authored_line, serialization)

    assert payload == authored_values
    assert restored.model_dump(mode="json") == authored_values
    for section, expected in zip(restored.sections, authored_values["sections"], strict=True):
        section_payload, restored_section = _round_trip(section, serialization)
        assert section_payload == expected
        assert restored_section.line_type == expected["line_type"]
        assert restored_section.length == expected["length"]
        assert restored_section.segment_length == expected["segment_length"]


def test_riser_section_lengths_match_independent_sum(authored_line, serialization):
    """Check the hand-calculated 80.50 m total before and after serialization."""
    _, restored = _round_trip(authored_line, serialization)

    assert fsum(section.length for section in authored_line.sections) == 80.5
    assert fsum(section.length for section in restored.sections) == 80.5
    assert authored_line.get_total_length() == 80.5
    assert restored.get_total_length() == 80.5


def test_riser_connection_serialization_retains_end_identity(
    authored_line, authored_values, serialization,
):
    """Retain each end's type, name, signed position, rotation and stiffness."""
    payload, restored = _round_trip(authored_line, serialization)

    for end in ("end_a", "end_b"):
        expected = authored_values[end]
        assert payload[end] == expected
        connection = getattr(restored, end)
        connection_payload, restored_connection = _round_trip(connection, serialization)
        assert connection_payload == expected
        assert restored_connection.model_dump(mode="json") == expected
        assert connection.type.value == expected["type"]
        assert connection.name == expected["name"]
        assert connection.position == expected["position"]
