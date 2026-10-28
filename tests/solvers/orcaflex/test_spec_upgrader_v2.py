# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline conversion contracts using authored synthetic riser input.

All physical numbers below are synthetic input, not design recommendations.
Expected values are copied from that authored input; the length check is the
independent hand sum of the two authored section lengths.
"""

from copy import deepcopy

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec, Riser
from digitalmodel.solvers.orcaflex.spec_upgrader import (
    DetectedModelType,
    detect_model_type,
    upgrade_spec,
)

CONNECTIONS = (
    "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, "
    "ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, "
    "ConnectionzRelativeTo"
)
SECTIONS = "LineType, Length, TargetSegmentLength, NumberOfSegments"


@pytest.fixture
def single_riser():
    """An authored two-section riser with deliberate pass-through properties."""
    return {
        "metadata": {
            "name": "synthetic_riser",
            "description": "Synthetic conversion contract",
            "structure": "riser",
            "operation": "in-place",
        },
        "environment": {"water": {"depth": 200}, "seabed": {"stiffness": {}}},
        "generic": {
            "vessel_types": [{"name": "SupportType", "length": 80}],
            "vessels": [{
                "name": "Support",
                "vessel_type": "SupportType",
                "initial_position": [1, 2, 3],
                "properties": {"PrimaryMotion": "None", "InitialHeading": 25},
            }],
            "line_types": [{
                "name": "Flexible Riser",
                "category": "General",
                "outer_diameter": 0.3,
                "inner_diameter": 0.2,
                "mass_per_length": 0.1,
                "bending_stiffness": 120,
                "axial_stiffness": 500000,
                "properties": {"GJ": 15, "Cd": 1.1, "UnmappedTable": [[2, 4]]},
            }],
            "lines": [{
                "name": "Synthetic Riser",
                "line_type_refs": ["Flexible Riser"],
                "properties": {
                    CONNECTIONS: [
                        ["Support", 4, 5, -10, 15, 80, 7, None, "End A"],
                        ["Anchored", 150, 8, 0.2, 190, 95, -3, 2, "End B"],
                    ],
                    SECTIONS: [
                        ["Flexible Riser", 120, 2, 60],
                        ["Flexible Riser", 45, 1.5, 30],
                    ],
                    "ContentsDensity": 0.8,
                    "UnmappedSetting": {"enabled": True, "values": [3, None]},
                },
            }],
            "general_properties": {"StaticsMaxIterations": 500},
            "groups": {"data": {"Structure": {"Synthetic Riser": "Model"}}},
        },
        "unmapped_root": {"annotation": ["retain", "verbatim"]},
    }


def generated_mapping(spec, output):
    """Compare every generated YAML document, including master and parameters."""
    generator = ModularModelGenerator.from_spec(ProjectInputSpec(**spec))
    generator.generate(output)
    return {
        path.relative_to(output).as_posix(): yaml.safe_load(path.read_text(encoding="utf-8"))
        for path in output.rglob("*.yml")
    }


def test_upgrade_single_riser_preserves_sections_and_connections(single_riser):
    assert detect_model_type(single_riser).model_type == DetectedModelType.RISER
    upgraded = upgrade_spec(single_riser, DetectedModelType.RISER)
    riser = Riser(**upgraded["riser"])
    line = riser.lines[0]
    authored = single_riser["generic"]["lines"][0]["properties"]
    assert line.name == single_riser["generic"]["lines"][0]["name"]
    for section, row in zip(line.sections, authored[SECTIONS], strict=True):
        assert [section.line_type, section.length, section.segment_length] == row[:3]
    assert line.get_total_length() == 120 + 45  # independent authored length sum
    for end, row in zip((line.end_a, line.end_b), authored[CONNECTIONS], strict=True):
        assert end.position == row[1:4]
        assert [end.azimuth, end.declination, end.gamma] == row[4:7]
    assert line.end_a.type.value == "vessel"
    assert line.end_a.name == "Support"
    assert line.end_b.type.value == "anchor"
    line_type = riser.line_types[0]
    for field in ("outer_diameter", "inner_diameter", "mass_per_length",
                  "bending_stiffness", "axial_stiffness"):
        assert getattr(line_type, field) == single_riser["generic"]["line_types"][0][field]
    assert line_type.torsional_stiffness == single_riser["generic"]["line_types"][0]["properties"]["GJ"]
    assert riser.vessel.position == single_riser["generic"]["vessels"][0]["initial_position"]


def test_upgrade_riser_preserves_unmapped_properties(single_riser):
    upgraded = upgrade_spec(single_riser, DetectedModelType.RISER)
    fallback = upgraded["generic"]
    assert upgraded["unmapped_root"] == single_riser["unmapped_root"]
    for field in ("vessel_types", "general_properties", "groups"):
        assert fallback[field] == single_riser["generic"][field]
    assert fallback["lines"][0]["properties"]["UnmappedSetting"] == single_riser["generic"]["lines"][0]["properties"]["UnmappedSetting"]
    assert fallback["line_types"][0]["properties"]["UnmappedTable"] == single_riser["generic"]["line_types"][0]["properties"]["UnmappedTable"]
    # Scalar properties have moved to the typed section, not a shadow copy.
    assert "outer_diameter" not in fallback["line_types"][0]
    assert "GJ" not in fallback["line_types"][0]["properties"]


def test_upgrade_riser_leaves_input_unchanged(single_riser):
    original = deepcopy(single_riser)
    upgraded = upgrade_spec(single_riser, DetectedModelType.RISER)
    assert single_riser == original
    upgraded["generic"]["lines"][0]["properties"]["UnmappedSetting"]["values"].append("changed")
    upgraded["riser"]["lines"][0]["end_a"]["position"][0] = 99
    assert single_riser == original


def test_upgrade_riser_yaml_round_trip(single_riser, tmp_path):
    before = generated_mapping(single_riser, tmp_path / "before")
    upgraded = upgrade_spec(single_riser, DetectedModelType.RISER)
    # Exercise a serialized public spec, not an in-memory-only adapter.
    upgraded = yaml.safe_load(yaml.safe_dump(upgraded))
    assert generated_mapping(upgraded, tmp_path / "after") == before


def test_validated_model_serialization_preserves_mapped_scalars(single_riser, tmp_path):
    upgraded = ProjectInputSpec(**upgrade_spec(single_riser, DetectedModelType.RISER))
    reloaded = yaml.safe_load(yaml.safe_dump(upgraded.model_dump(mode="json")))
    objects = generated_mapping(reloaded, tmp_path / "serialized")["includes/20_generic_objects.yml"]
    line_type = objects["LineTypes"][0]
    assert [line_type[key] for key in ("OD", "ID", "MassPerUnitLength", "EI", "EA")] == [
        0.3, 0.2, 0.1, 120, 500000,
    ]
    assert objects["Vessels"][0]["VesselType"] == "SupportType"
    assert objects["Vessels"][0]["InitialPosition"] == [1, 2, 3]


@pytest.mark.parametrize("alias_value", [None, 0.4])
def test_typed_edits_override_conflicting_fallback_aliases(single_riser, tmp_path, alias_value):
    upgraded = upgrade_spec(single_riser, DetectedModelType.RISER)
    upgraded["generic"]["line_types"][0]["outer_diameter"] = alias_value
    upgraded["generic"]["vessels"][0].update(vessel_type="OtherType", initial_position=[9, 9, 9])
    upgraded["riser"]["line_types"][0]["outer_diameter"] = 0.35
    upgraded["riser"]["vessel"]["position"] = [4, 5, 6]
    original = deepcopy(upgraded)
    objects = generated_mapping(upgraded, tmp_path / "conflict")["includes/20_generic_objects.yml"]
    assert objects["LineTypes"][0]["OD"] == 0.35
    assert objects["Vessels"][0]["VesselType"] == "SupportType"
    assert objects["Vessels"][0]["InitialPosition"] == [4, 5, 6]
    assert upgraded == original


@pytest.mark.parametrize("target", [DetectedModelType.MOORING, DetectedModelType.PIPELINE,
                                    DetectedModelType.GENERIC])
def test_other_upgrades_remain_unsupported(single_riser, target):
    with pytest.raises(NotImplementedError, match="planned for v2"):
        upgrade_spec(single_riser, target)


def test_typed_edits_reach_generated_yaml(single_riser, tmp_path):
    upgraded = upgrade_spec(single_riser, DetectedModelType.RISER)
    # Synthetic edit values; expected values are these authored assignments.
    line = upgraded["riser"]["lines"][0]
    line["sections"][0]["length"] = 125
    line["end_b"]["position"] = [160, 9, 0.3]
    upgraded["riser"]["line_types"][0]["outer_diameter"] = 0.35
    mapping = generated_mapping(upgraded, tmp_path / "edited")
    objects = mapping["includes/20_generic_objects.yml"]
    assert objects["Lines"][0][SECTIONS][0][1] == 125
    assert objects["Lines"][0][CONNECTIONS][1][1:4] == [160, 9, 0.3]
    assert objects["LineTypes"][0]["OD"] == 0.35


@pytest.mark.parametrize("field,value", [("azimuth", 45), ("declination", 60), ("gamma", 12)])
@pytest.mark.parametrize("end_name", ["end_a", "end_b"])
@pytest.mark.parametrize("layout", ["four_columns", "short_row"])
def test_unrepresentable_angular_edits_are_rejected(single_riser, tmp_path, field, value, end_name, layout):
    props = single_riser["generic"]["lines"][0]["properties"]
    rows = props.pop(CONNECTIONS)
    key = ", ".join(CONNECTIONS.split(", ")[:4]) if layout == "four_columns" else CONNECTIONS
    props[key] = [row[:4] for row in rows]
    upgraded = upgrade_spec(single_riser, DetectedModelType.RISER)
    upgraded["riser"]["lines"][0][end_name][field] = value
    original = deepcopy(upgraded)
    with pytest.raises(ValueError, match="Cannot render riser connection angle"):
        generated_mapping(upgraded, tmp_path / "unrepresentable")
    assert upgraded == original


@pytest.mark.parametrize("native", ["ConnectionAzimuth", "ConnectionDeclination", "ConnectionGamma"])
@pytest.mark.parametrize("stage", ["upgrade", "render"])
def test_standalone_connection_angles_are_rejected(single_riser, tmp_path, native, stage):
    spec = single_riser if stage == "upgrade" else upgrade_spec(single_riser, DetectedModelType.RISER)
    props = spec["generic"]["lines"][0]["properties"]
    rows = props.pop(CONNECTIONS)
    props[", ".join(CONNECTIONS.split(", ")[:4])] = [row[:4] for row in rows]
    props[native] = [row[CONNECTIONS.split(", ").index(native)] for row in rows]
    original = deepcopy(spec)
    with pytest.raises(ValueError, match="Standalone riser connection angles are not supported"):
        if stage == "upgrade":
            upgrade_spec(spec, DetectedModelType.RISER)
        else:
            generated_mapping(spec, tmp_path / "standalone")
    assert spec == original


def test_unedited_four_column_connections_round_trip(single_riser, tmp_path):
    props = single_riser["generic"]["lines"][0]["properties"]
    rows = props.pop(CONNECTIONS)
    props[", ".join(CONNECTIONS.split(", ")[:4])] = [row[:4] for row in rows]
    upgraded = upgrade_spec(single_riser, DetectedModelType.RISER)
    assert generated_mapping(upgraded, tmp_path / "after") == generated_mapping(single_riser, tmp_path / "before")


@pytest.mark.parametrize("field,column,value", [("azimuth", 4, 45), ("declination", 5, 60), ("gamma", 6, 12)])
@pytest.mark.parametrize("end_name,row", [("end_a", 0), ("end_b", 1)])
def test_representable_angular_edits_reach_generated_yaml(single_riser, tmp_path, field, column, value, end_name, row):
    upgraded = upgrade_spec(single_riser, DetectedModelType.RISER)
    upgraded["riser"]["lines"][0][end_name][field] = value
    objects = generated_mapping(upgraded, tmp_path / "angle")["includes/20_generic_objects.yml"]
    assert objects["Lines"][0][CONNECTIONS][row][column] == value


def test_reordered_table_columns_extract_by_header(single_riser):
    props = single_riser["generic"]["lines"][0]["properties"]
    for key in (CONNECTIONS, SECTIONS):
        rows = props.pop(key)
        props[", ".join(reversed(key.split(", ")))] = [list(reversed(row)) for row in rows]
    upgraded = upgrade_spec(single_riser, DetectedModelType.RISER)
    line = upgraded["riser"]["lines"][0]
    assert [section["length"] for section in line["sections"]] == [120, 45]
    assert line["end_a"]["name"] == "Support"
    assert line["end_b"]["position"] == [150, 8, 0.2]


@pytest.mark.parametrize("connection", ["Fixed", "Free"])
def test_non_object_end_connections(single_riser, connection, tmp_path):
    single_riser["generic"]["lines"][0]["properties"][CONNECTIONS][1][0] = connection
    upgraded = upgrade_spec(single_riser, DetectedModelType.RISER)
    assert upgraded["riser"]["lines"][0]["end_b"]["type"] == connection.lower()
    assert generated_mapping(upgraded, tmp_path / "after") == generated_mapping(single_riser, tmp_path / "before")


@pytest.mark.parametrize("defect", ["multiple_lines", "multiple_vessels", "missing_sections",
                                  "missing_connections", "unknown_connection", "missing_dimension"])
def test_unsupported_riser_shapes_fail_without_mutation(single_riser, defect):
    generic = single_riser["generic"]
    props = generic["lines"][0]["properties"]
    if defect == "multiple_lines":
        generic["lines"].append(deepcopy(generic["lines"][0]))
    elif defect == "multiple_vessels":
        generic["vessels"].append({"name": "Other"})
    elif defect == "missing_sections":
        del props[SECTIONS]
    elif defect == "missing_connections":
        del props[CONNECTIONS]
    elif defect == "unknown_connection":
        props[CONNECTIONS][1][0] = "Unresolved"
    else:
        del generic["line_types"][0]["outer_diameter"]
    original = deepcopy(single_riser)
    with pytest.raises(ValueError):
        upgrade_spec(single_riser, DetectedModelType.RISER)
    assert single_riser == original


def test_native_scalar_properties_round_trip(single_riser, tmp_path):
    obj = single_riser["generic"]["line_types"][0]
    # Native spellings are the existing generic schema's public field mapping.
    native_fields = {
        "outer_diameter": "OD", "inner_diameter": "ID",
        "mass_per_length": "MassPerUnitLength", "bending_stiffness": "EI",
        "axial_stiffness": "EA",
    }
    for field, native in native_fields.items():
        obj["properties"][native] = obj.pop(field)
    upgraded = upgrade_spec(single_riser, DetectedModelType.RISER)
    assert upgraded["riser"]["line_types"][0]["outer_diameter"] == obj["properties"]["OD"]
    assert generated_mapping(upgraded, tmp_path / "after") == generated_mapping(single_riser, tmp_path / "before")


def test_typed_generic_fields_take_precedence(single_riser, tmp_path):
    obj = single_riser["generic"]["line_types"][0]
    # A deliberately conflicting synthetic value exercises existing precedence.
    obj["properties"]["OD"] = 0.4
    upgraded = upgrade_spec(single_riser, DetectedModelType.RISER)
    assert upgraded["riser"]["line_types"][0]["outer_diameter"] == obj["outer_diameter"]
    assert generated_mapping(upgraded, tmp_path / "after") == generated_mapping(single_riser, tmp_path / "before")


@pytest.mark.parametrize("defect", ["duplicate_table", "short_row", "unknown_line_type",
                                  "non_scalar_stiffness", "missing_vessel"])
def test_invalid_riser_data_is_rejected(single_riser, defect):
    generic = single_riser["generic"]
    props = generic["lines"][0]["properties"]
    if defect == "duplicate_table":
        props["LineType, Length, TargetSegmentLength"] = [row[:3] for row in props[SECTIONS]]
    elif defect == "short_row":
        props[SECTIONS][0] = ["Flexible Riser"]
    elif defect == "unknown_line_type":
        props[SECTIONS][0][0] = "Unresolved"
    elif defect == "non_scalar_stiffness":
        generic["line_types"][0]["bending_stiffness"] = [120, None]
    else:
        del generic["vessels"]
    original = deepcopy(single_riser)
    with pytest.raises(ValueError):
        upgrade_spec(single_riser, DetectedModelType.RISER)
    assert single_riser == original


def test_unused_line_types_remain_in_fallback(single_riser, tmp_path):
    unused = {"name": "Unused", "category": "Chain", "properties": {"ChainBarDiameter": 0.05}}
    single_riser["generic"]["line_types"].append(unused)
    upgraded = upgrade_spec(single_riser, DetectedModelType.RISER)
    assert [obj["name"] for obj in upgraded["riser"]["line_types"]] == ["Flexible Riser"]
    assert upgraded["generic"]["line_types"][1] == unused
    assert generated_mapping(upgraded, tmp_path / "after") == generated_mapping(single_riser, tmp_path / "before")


def test_multiple_referenced_line_types_preserve_order(single_riser, tmp_path):
    other = deepcopy(single_riser["generic"]["line_types"][0])
    other["name"] = "Other Riser Type"
    other["outer_diameter"] = 0.4  # authored synthetic second diameter
    single_riser["generic"]["line_types"].append(other)
    rows = single_riser["generic"]["lines"][0]["properties"][SECTIONS]
    rows[1][0] = other["name"]
    upgraded = upgrade_spec(single_riser, DetectedModelType.RISER)
    assert [s["line_type"] for s in upgraded["riser"]["lines"][0]["sections"]] == [row[0] for row in rows]
    assert generated_mapping(upgraded, tmp_path / "after") == generated_mapping(single_riser, tmp_path / "before")
