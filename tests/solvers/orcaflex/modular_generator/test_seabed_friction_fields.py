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


def test_mooring_friction_override_is_not_merged_by_line_type_name():
    """Two legs of the same chain with different friction keep both values.

    Line types are de-duplicated by name; an override must not be dropped
    because another leg already defined a line type of the same size and grade.
    """
    from digitalmodel.solvers.orcaflex.modular_generator.schema.mooring import MooringSystem

    def leg(name, **seg_extra):
        return {
            "name": name,
            "segments": [{"type": "chain", "diameter": 0.076, "length": 900,
                          "grade": "R3", **seg_extra}],
            "anchor": {"type": "anchor", "position": [800, 0, -200]},
            "fairlead": {"type": "fairlead", "position": [40, 0, -10], "vessel": "V"},
        }

    out = MooringRouter().route(MooringSystem(lines=[
        leg("Default"), leg("Override", seabed_lateral_friction=0.7), leg("Again"),
    ]))
    friction = {lt["name"]: lt["properties"]["SeabedLateralFrictionCoefficient"]
                for lt in out["line_types"]}
    assert len(friction) == 2
    for line in out["lines"]:
        (row,) = line["properties"]["LineType, Length, NumberOfSegments"]
        expected = 0.7 if line["name"] == "Override" else 1.0
        assert friction[row[0]] == expected


@pytest.mark.parametrize("order", ["forward", "reversed"])
def test_mooring_nearby_friction_overrides_keep_distinct_line_types(order):
    """Overrides that differ below six significant digits are not merged.

    The authored values 0.7000001 and 0.7000002 are the expected outputs;
    each leg must reference the line type carrying its own value, whichever
    leg is routed first.
    """
    from digitalmodel.solvers.orcaflex.modular_generator.schema.mooring import MooringSystem

    def leg(name, mu):
        return {
            "name": name,
            "segments": [{"type": "chain", "diameter": 0.076, "length": 900,
                          "grade": "R3", "seabed_lateral_friction": mu}],
            "anchor": {"type": "anchor", "position": [800, 0, -200]},
            "fairlead": {"type": "fairlead", "position": [40, 0, -10], "vessel": "V"},
        }

    authored = {"A": 0.7000001, "B": 0.7000002}
    names = list(authored) if order == "forward" else list(reversed(authored))
    out = MooringRouter().route(MooringSystem(lines=[leg(n, authored[n]) for n in names]))
    friction = {lt["name"]: lt["properties"]["SeabedLateralFrictionCoefficient"]
                for lt in out["line_types"]}
    assert len(friction) == 2
    for line in out["lines"]:
        (row,) = line["properties"]["LineType, Length, NumberOfSegments"]
        assert friction[row[0]] == authored[line["name"]]


# Riser-plus-generic fallback route (the supported conversion path). The
# fixture and YAML helper are shared with the conversion contract tests.
from tests.solvers.orcaflex.test_spec_upgrader_v2 import (  # noqa: E402
    generated_mapping,
    single_riser,  # noqa: F401  (pytest fixture)
)
from digitalmodel.solvers.orcaflex.spec_upgrader import (  # noqa: E402
    DetectedModelType,
    upgrade_spec,
)

_LATERAL = "SeabedLateralFrictionCoefficient"
_AXIAL = "SeabedAxialFrictionCoefficient"


def _fallback_line_type(spec, tmp_path, name):
    objects = generated_mapping(spec, tmp_path / name)["includes/20_generic_objects.yml"]
    (line_type,) = objects["LineTypes"]
    return line_type


def test_fallback_route_emits_authored_typed_friction(single_riser, tmp_path):
    """Typed friction edits reach YAML on the riser-plus-generic route.

    The fallback carries 0.5/0.2; the authored typed values 0.8/0.4 are the
    expected output.
    """
    single_riser["generic"]["line_types"][0]["properties"].update({_LATERAL: 0.5, _AXIAL: 0.2})
    upgraded = upgrade_spec(single_riser, DetectedModelType.RISER)
    upgraded["riser"]["line_types"][0].update(
        seabed_lateral_friction=0.8, seabed_axial_friction=0.4)
    line_type = _fallback_line_type(upgraded, tmp_path, "authored")
    assert (line_type[_LATERAL], line_type[_AXIAL]) == (0.8, 0.4)


def test_fallback_route_unset_friction_keeps_fallback_and_adds_no_default(single_riser, tmp_path):
    """Unset typed friction leaves native values alone and injects no default."""
    upgraded = upgrade_spec(single_riser, DetectedModelType.RISER)
    typed = upgraded["riser"]["line_types"][0]
    assert "seabed_lateral_friction" not in typed
    assert "seabed_axial_friction" not in typed
    line_type = _fallback_line_type(upgraded, tmp_path, "absent")
    assert _LATERAL not in line_type and _AXIAL not in line_type

    upgraded["generic"]["line_types"][0]["properties"].update({_LATERAL: 0.9, _AXIAL: 0.3})
    line_type = _fallback_line_type(upgraded, tmp_path, "native")
    assert (line_type[_LATERAL], line_type[_AXIAL]) == (0.9, 0.3)


def test_fallback_route_round_trip_preserves_native_friction(single_riser, tmp_path):
    """Upgrading a native riser with friction 0.9/null leaves the YAML unchanged."""
    single_riser["generic"]["line_types"][0]["properties"].update({_LATERAL: 0.9, _AXIAL: None})
    before = generated_mapping(single_riser, tmp_path / "before")
    upgraded = upgrade_spec(single_riser, DetectedModelType.RISER)
    assert upgraded["riser"]["line_types"][0]["seabed_lateral_friction"] == 0.9
    assert generated_mapping(upgraded, tmp_path / "after") == before
