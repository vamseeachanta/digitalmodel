"""Seabed friction is a schema input, not a hard-coded value (#3989).

Riser line types emitted SeabedLateralFrictionCoefficient 0.5 and mooring
line types 1.0 (chain), 0.5 (wire) and 0.3 (polyester) with no way to change
them. Optional fields now carry the value; the defaults reproduce the former
output so existing models are unchanged.
"""

from __future__ import annotations

import pytest

from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
from digitalmodel.solvers.orcaflex.modular_generator.builders.riser_linetype_builder import (
    RiserLineTypeBuilder,
)
from digitalmodel.solvers.orcaflex.modular_generator.routers.mooring_router import MooringRouter
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
from digitalmodel.solvers.orcaflex.modular_generator.schema.mooring import (
    MooringSegment,
    MooringSegmentType,
)


def _riser_spec(**line_type_extra) -> ProjectInputSpec:
    line_type = {
        "name": "lt", "outer_diameter": 0.3, "inner_diameter": 0.2,
        "mass_per_length": 0.1, "bending_stiffness": 100, "axial_stiffness": 5e5,
        **line_type_extra,
    }
    return ProjectInputSpec(**{
        "metadata": {"name": "r", "description": "r", "structure": "riser",
                     "operation": "production", "project": "T"},
        "environment": {"water": {"depth": 100}, "seabed": {"stiffness": {"normal": 100, "shear": 100}}},
        "riser": {
            "vessel": {"name": "V"},
            "line_types": [line_type],
            "lines": [{
                "name": "L",
                "end_a": {"type": "vessel", "name": "V", "position": [0, 0, -5]},
                "end_b": {"type": "anchor", "position": [100, 0, 0]},
                "sections": [{"line_type": "lt", "length": 150, "segment_length": 5}],
            }],
        },
    })


def _riser_line_type(**extra) -> dict:
    spec = _riser_spec(**extra)
    return RiserLineTypeBuilder(spec, BuilderContext()).build()["LineTypes"][0]


def test_riser_friction_default_unchanged():
    lt = _riser_line_type()
    assert lt["SeabedLateralFrictionCoefficient"] == 0.5
    assert lt["SeabedAxialFrictionCoefficient"] is None


def test_riser_friction_from_schema():
    lt = _riser_line_type(seabed_lateral_friction=0.8, seabed_axial_friction=0.4)
    assert lt["SeabedLateralFrictionCoefficient"] == 0.8
    assert lt["SeabedAxialFrictionCoefficient"] == 0.4


def test_riser_drag_table_name_and_number_share_layout():
    assert _riser_line_type(drag_coefficient="Generic Drag")["Cd"] == ["Generic Drag", None, 0.008]
    assert _riser_line_type(drag_coefficient=1.1)["Cd"] == [1.1, None, 0.008]


@pytest.mark.parametrize(
    ("seg_type", "extra", "default"),
    [
        (MooringSegmentType.CHAIN, {"grade": "R3"}, 1.0),
        (MooringSegmentType.WIRE_ROPE, {}, 0.5),
        (MooringSegmentType.POLYESTER, {}, 0.3),
    ],
)
def test_mooring_friction_default_and_override(seg_type, extra, default):
    seg = MooringSegment(type=seg_type, diameter=0.08, length=100, **extra)
    lt = MooringRouter()._segment_to_linetype(seg)
    assert lt["properties"]["SeabedLateralFrictionCoefficient"] == default
    seg = MooringSegment(type=seg_type, diameter=0.08, length=100,
                         seabed_lateral_friction=0.7, **extra)
    lt = MooringRouter()._segment_to_linetype(seg)
    assert lt["properties"]["SeabedLateralFrictionCoefficient"] == 0.7
